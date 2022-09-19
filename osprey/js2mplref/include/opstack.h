/// Copyright [year] <Copyright Owner>
#ifndef JS2MPL_INCLUDE_OPSTACK_H_
#define JS2MPL_INCLUDE_OPSTACK_H_
#include <assert.h>
#include <vector>
#include "mapleir/include/mirbuilder.h"
#include "util.h"

#include "jsvalue.h"

namespace maple {
class OpInfo {
  BaseNode *bn_;
  uint32_t tag_;
  explicit OpInfo(BaseNode *bn, uint32_t tag) : bn_(bn), tag_(tag) {}
};

class OpStack {
 private:
  uint32_t current_depth_;
  uint32_t max_depth_;
  std::vector<OpInfo> stack_;

 public:
  BaseNode *rval;
  bool flag_has_rval;
  explicit OpStack(unsigned max_depth) : current_depth_(0), flag_has_rval(false), max_depth_(max_depth) {}

  bool CheckDepth(unsigned expected) {
    return current_depth_ == expected;
  }

  void Push(BaseNode *bn) {}

  void Push(void *node) {
    stack_.push_back(node);
    current_depth_++;
    if (js2mplDebug >= 2) {
      PrintIndentation(js2mplDebugIndent);
      std::cout << "------stack depth increased to " << current_depth_ << "------- " << node << std::endl;
    }
    assert(current_depth_ <= max_depth_);
  }

  void *Pop() {
    assert(current_depth_ >= 1);
    current_depth_--;
    void *last = stack_[stack_.size() - 1];
    if (js2mplDebug >= 2) {
      PrintIndentation(js2mplDebugIndent);
      std::cout << "------stack depth decreased to " << current_depth_ << "------- " << last << std::endl;
    }
    stack_.pop_back();
    return last;
  }

  void *Top() {
    assert(current_depth_ >= 1);
    return stack_[stack_.size() - 1];
  }

  void *GetOpAt(uint32_t n) {
    std::vector<void *> temp_stack;
    for (uint32_t i = 0; i < n; i++) {
      temp_stack.push_back(Pop());
    }
    void *bn = Pop();
    Push(bn);
    for (uint32_t i = 0; i < n; i++) {
      Push(temp_stack[temp_stack.size() - 1]);
      temp_stack.pop_back();
    }
    return bn;
  }

  OpInfo Pop() {}
};
}  // namespace maple
#endif  // JS2MPL_INCLUDE_OPERANDSTACK_H_
