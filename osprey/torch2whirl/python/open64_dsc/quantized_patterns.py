"""Exact source pack/unpack joins; no tensor execution or native allocation."""
import operator


def u4_patterns(nodes):
    import torch
    replacements, skipped = {}, set()

    def function(node, target, arity):
        return (isinstance(node, torch.fx.Node) and node.op == "call_function" and
                node.target is target and len(node.args) == arity and not node.kwargs)

    def only(node, users):
        return set(node.users) == set(users)

    def half(node, start):
        if not function(node, operator.getitem, 2):
            return None
        source, index = node.args
        if (type(index) is tuple and len(index) == 2 and index[0] is Ellipsis and
                type(index[1]) is slice and index[1].start == start and
                index[1].stop is None and index[1].step == 2):
            return source
        return None

    for node in nodes:
        if function(node, torch.bitwise_or, 2):
            low, shifted = node.args
            if not function(shifted, torch.bitwise_left_shift, 2) or shifted.args[1] != 4:
                continue
            high = shifted.args[0]
            source = half(low, 0)
            if (source is not None and half(high, 1) is source and
                    only(low, [node]) and only(high, [shifted]) and only(shifted, [node])):
                replacements[node] = ("common.pack_u4", source)
                skipped.update((low, high, shifted))
        if (node.op == "call_method" and node.target == "flatten" and
                len(node.args) == 2 and node.args[1] == -2 and not node.kwargs):
            stack = node.args[0]
            if not (isinstance(stack, torch.fx.Node) and stack.op == "call_function" and
                    stack.target is torch.stack and len(stack.args) == 1 and
                    stack.kwargs == {"dim": -1}):
                continue
            pair = stack.args[0]
            if not isinstance(pair, (tuple, list)) or len(pair) != 2:
                continue
            low, high = pair
            if (function(low, torch.bitwise_and, 2) and low.args[1] == 15 and
                    function(high, torch.bitwise_right_shift, 2) and high.args[1] == 4 and
                    low.args[0] is high.args[0] and only(low, [stack]) and only(high, [stack]) and
                    only(stack, [node])):
                replacements[node] = ("common.unpack_u4", low.args[0])
                skipped.update((low, high, stack))
    return replacements, skipped
