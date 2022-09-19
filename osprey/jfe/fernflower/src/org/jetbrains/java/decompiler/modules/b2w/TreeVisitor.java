/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package org.jetbrains.java.decompiler.modules.b2w;

import org.jetbrains.java.decompiler.code.CodeConstants;
import org.jetbrains.java.decompiler.main.ClassesProcessor;
import org.jetbrains.java.decompiler.main.DecompilerContext;
import org.jetbrains.java.decompiler.main.extern.IFernflowerPreferences;
import org.jetbrains.java.decompiler.main.rels.ClassWrapper;
import org.jetbrains.java.decompiler.modules.b2w.handler.JavaToWhirlConst;
import org.jetbrains.java.decompiler.modules.b2w.handler.SymbolHandler;
import org.jetbrains.java.decompiler.modules.b2w.handler.WhirlConstants;
import org.jetbrains.java.decompiler.modules.bgen.BGenDriver;
import org.jetbrains.java.decompiler.modules.decompiler.exps.*;
import org.jetbrains.java.decompiler.modules.decompiler.stats.BasicBlockStatement;
import org.jetbrains.java.decompiler.modules.decompiler.stats.RootStatement;
import org.jetbrains.java.decompiler.modules.decompiler.stats.Statement;
import org.jetbrains.java.decompiler.struct.StructClass;
import org.jetbrains.java.decompiler.struct.StructField;
import org.jetbrains.java.decompiler.struct.StructMember;
import org.jetbrains.java.decompiler.struct.StructMethod;
import org.jetbrains.java.decompiler.struct.gen.MethodDescriptor;
import org.jetbrains.java.decompiler.util.InterpreterUtil;

import java.util.LinkedList;
import java.util.List;
import java.util.Map;

public class TreeVisitor {
  private final Map<String, ClassesProcessor.ClassNode> mapRootClasses;
  long currentEntry = 0;
  LinkedList<Long> blockStack = new LinkedList<>();

  public TreeVisitor(final Map<String, ClassesProcessor.ClassNode> mapRootClasses) {
    this.mapRootClasses = mapRootClasses;
  }

  // ============================== Visit Struct ========================================
  public void visit(StructMember struct) {
    if(struct == null) {
      return;
    }
    if(struct instanceof StructClass) {
      visitStructClass((StructClass) struct);
    } else if(struct instanceof StructMethod) {
      visitStructMethod((StructMethod) struct);
    } else if(struct instanceof StructField) {
      visitStructField((StructField) struct);
    } else {
      assert false: "Not support struct : " + struct.getClass().getName();
    }
  }

  public void visitStructClass(StructClass sc) {
    ClassesProcessor.ClassNode node = mapRootClasses.get(sc.qualifiedName);
    ClassesProcessor.ClassNode outerNode = (ClassesProcessor.ClassNode) DecompilerContext.getProperty(DecompilerContext.CURRENT_CLASS_NODE);
    DecompilerContext.setProperty(DecompilerContext.CURRENT_CLASS_NODE, node);
    int flags = node.type == ClassesProcessor.ClassNode.CLASS_ROOT ? sc.getAccessFlags() : node.access;
    boolean isDeprecated = sc.hasAttribute("Deprecated");
    boolean isSynthetic = (flags & CodeConstants.ACC_SYNTHETIC) != 0 || sc.hasAttribute("Synthetic");
    boolean isEnum = DecompilerContext.getOption(IFernflowerPreferences.DECOMPILE_ENUM) && (flags & CodeConstants.ACC_ENUM) != 0;
    boolean isInterface = (flags & CodeConstants.ACC_INTERFACE) != 0;
    boolean isAnnotation = (flags & CodeConstants.ACC_ANNOTATION) != 0;

    try {
      ClassWrapper wrapper = node.getWrapper();
      // visit super class

      // visit interface
      if (!isAnnotation) {
        int[] interfaces = sc.getInterfaces();
        if (interfaces.length > 0) {
          for (int i = 0; i < interfaces.length; i++) {

          }
        }
      }

      for (StructField fd : sc.getFields()) {
        boolean hide = fd.isSynthetic() && DecompilerContext.getOption(IFernflowerPreferences.REMOVE_SYNTHETIC) ||
          wrapper.getHiddenMembers().contains(InterpreterUtil.makeUniqueKey(fd.getName(), fd.getDescriptor()));
        if (hide) continue;
        visit(fd);
      }
      for (StructMethod mt : sc.getMethods()) {
        visit(mt);
      }
    } finally {
      DecompilerContext.setProperty(DecompilerContext.CURRENT_CLASS_NODE, outerNode);
    }
  }

  private void visitStructField(StructField sf) {
    ClassWrapper wrapper =
      ((ClassesProcessor.ClassNode) DecompilerContext.getProperty(DecompilerContext.CURRENT_CLASS_NODE)).getWrapper();
    Exprent initializer;
    if (sf.hasModifier(CodeConstants.ACC_STATIC)) {
      initializer = wrapper.getStaticFieldInitializers().getWithKey(
        InterpreterUtil.makeUniqueKey(sf.getName(), sf.getDescriptor())
      );
      long symbolIdx = SymbolHandler.getSymbol(sf);
    } else {
      assert false: "Need to add to member field, name : " + sf.getName();
    }
  }

  private void visitStructMethod(StructMethod sm) {
    long pool  = BGenDriver.jniPoolInitialize();
    long out   = BGenDriver.jniWnMapTabCreate(pool);
    long scope = BGenDriver.jniNewScope(pool);
    long symbolIdx = SymbolHandler.getSymbol(sm);

    currentEntry = BGenDriver.startFunction(symbolIdx, MethodDescriptor.parseDescriptor(sm.getDescriptor()).params.length, 2);
    blockStack.push(BGenDriver.wnGetBodyFromEntry(currentEntry));
    ClassWrapper wrapper =
      ((ClassesProcessor.ClassNode) DecompilerContext.getProperty(DecompilerContext.CURRENT_CLASS_NODE)).getWrapper();
    RootStatement root = wrapper.getMethodWrapper(sm.getName(), sm.getDescriptor()).root;
    visit(root);
    BGenDriver.finishFunction(0);
    currentEntry = 0;
    blockStack.pop();
  }

  // ============================== Visit Stmt ========================================
  private void visit(Statement stmt) {
    if(stmt == null) {
      return;
    }
    if(stmt instanceof RootStatement) {
      visitRootStmt((RootStatement) stmt);
    } else if(stmt instanceof BasicBlockStatement) {
      visitBasicBlockStmt((BasicBlockStatement) stmt);
    } else {
      assert false: "Not support stmt : " + stmt.getClass().getName();
    }
  }

  private void visitRootStmt(RootStatement rs) {
    visit(rs.getVarDefinitions());
    visit(rs.getFirst());
  }

  private void visitBasicBlockStmt(BasicBlockStatement bbs) {
    assert blockStack.size() > 0: "Block stack size is 0.";
    blockStack.push(BGenDriver.jniWnCreateBlock());
//    visit(bbs.getVarDefinitions());
    if(bbs.getExprents() != null && !bbs.getExprents().isEmpty()) {
      for(Exprent lst: bbs.getExprents()) {
        long wnId = visit(lst);

        BGenDriver.jniInsertBlockLast(blockStack.peek(), wnId);
      }
    }
    long currentBlock = blockStack.pop();
    BGenDriver.jniInsertBlockLast(blockStack.peek(), currentBlock);
  }

  // ============================== Visit Expr ========================================
  private void visit(List<Exprent> lst) {
    if(lst == null || lst.isEmpty()) {
      return;
    }
    for(Exprent expr: lst) {
      visit(expr);
    }
  }

  private long visit(Exprent expr) {
    if(expr == null) {
      return 0;
    }
    if(expr instanceof VarExprent) {
      return visitVarExprent((VarExprent) expr);
    } else if(expr instanceof FieldExprent) {
      return visitFieldExprent((FieldExprent) expr);
    } else if(expr instanceof AssignmentExprent) {
      return visitAssignmentExprent((AssignmentExprent) expr);
    } else if(expr instanceof ConstExprent) {
      return visitConstExprent((ConstExprent) expr);
    } else if(expr instanceof FunctionExprent) {
      return visitFunctionExprent((FunctionExprent) expr);
    }
    else {
      assert false: "Not support expr : " + expr.getClass().getName();
    }
    return 0;
  }

  private long visitVarExprent(VarExprent ve) {
    return SymbolHandler.getSymbol(ve);
  }

  private long visitFieldExprent(FieldExprent fe) {
    return SymbolHandler.getSymbol(fe);
  }

  private long visitAssignmentExprent(AssignmentExprent ae) {
    Exprent left = ae.getLeft();
    Exprent right = ae.getRight();
    assert isVarExprent(left): "AssignmentExprent left node is not var exprent, left : " + left.getClass().getName();
    long leftSymIdx = visit(ae.getLeft());
    long leftSymTypeIdx = BGenDriver.getTyFromST(leftSymIdx);
    long rightWN = 0;
    if (right instanceof VarExprent ||
        right instanceof FieldExprent)
    {
      long rightSymIdx = visit(right);
      long rightSymTypeIdx = BGenDriver.getTyFromST(rightSymIdx);
      rightWN = BGenDriver.jniLDID(JavaToWhirlConst.getMType(right.getExprType().type),
                             0, rightSymIdx, rightSymTypeIdx);
    }
    else
    {
      rightWN = visit(right);
    }
    // create binary operator
    if(ae.getCondType() != AssignmentExprent.CONDITION_NONE) {
      // ldid left var
      long ldidWN = BGenDriver.jniLDID(JavaToWhirlConst.getMType(ae.getLeft().type), 0, leftSymIdx, leftSymTypeIdx);
      // create binary operator
      rightWN = BGenDriver.jniWNCreateBinary(
        JavaToWhirlConst.getOperator(ae.getCondType()), JavaToWhirlConst.getMType(left.type), ldidWN, rightWN);
    }
    int leftMType = 0;
    if(left instanceof VarExprent) {
      leftMType = JavaToWhirlConst.getMType(((VarExprent) left).getVarType().type);
    } else if(left instanceof FieldExprent) {
      leftMType = JavaToWhirlConst.getMType(((FieldExprent) left).getExprType().type);
    } else {
      assert false: "Not var exprent.";
    }
    // stid to left
    return BGenDriver.jniSTID(leftMType, 0, leftSymIdx, leftSymTypeIdx, rightWN);
  }

  private static boolean isVarExprent(Exprent expr) {
    return expr instanceof VarExprent || expr instanceof FieldExprent;
  }

  private long visitConstExprent(ConstExprent ce) {
    Object value = ce.getValue();
    int mtype = JavaToWhirlConst.getMType(ce.getConstType().type);
    switch (ce.getConstType().type) {
      case CodeConstants.TYPE_BOOLEAN:
        return BGenDriver.jniWNIntConst(mtype, (Integer)value);
      case CodeConstants.TYPE_CHAR:
        assert false: "Unicode char handle later.";
        break;
      case CodeConstants.TYPE_BYTE:
      case CodeConstants.TYPE_BYTECHAR:
      case CodeConstants.TYPE_SHORT:
      case CodeConstants.TYPE_SHORTCHAR:
      case CodeConstants.TYPE_INT:
        return BGenDriver.jniWNIntConst(WhirlConstants.Mtype.MTYPE_I4.toInt(), (Integer)value);
      case CodeConstants.TYPE_LONG:
        return BGenDriver.jniWNIntConst(WhirlConstants.Mtype.MTYPE_I8.toInt(), (Long)value);
      case CodeConstants.TYPE_FLOAT:
      case CodeConstants.TYPE_DOUBLE:
      case CodeConstants.TYPE_NULL:
      case CodeConstants.TYPE_OBJECT:
        assert false: "handle later.";
        break;
    }
    return 0;
  }

  private long genLDForSymbol(Exprent expr) {
    if(expr instanceof VarExprent ||
       expr instanceof FieldExprent) {
        long symIdx = visit(expr);
        long symTypeIdx = BGenDriver.getTyFromST(symIdx);
        long ldwn = BGenDriver.jniLDID(JavaToWhirlConst.getMType(expr.getExprType().type), 0, symIdx, symTypeIdx);
        return ldwn;
    }
    else {
      return visit(expr);
    }

  }
  private long visitFunctionExprent(FunctionExprent fe) {
    switch(fe.getFuncType()) {
      case (FunctionExprent.FUNCTION_ADD):
        Exprent left = fe.getLstOperands().get(0);
        Exprent right = fe.getLstOperands().get(1);
        long leftWN = genLDForSymbol(left);
        long rightWN = genLDForSymbol(right);
        long retWN = BGenDriver.jniAdd(fe.getExprType().type, leftWN,  rightWN);
        return retWN;
      default:
        assert false: "Not supported function type";
        break;
    }
    return 0;
  }

  private static StructField getStructField(FieldExprent field) {
    StructField fd = null;
    ClassesProcessor.ClassNode node = ((ClassesProcessor.ClassNode) DecompilerContext.getProperty(DecompilerContext.CURRENT_CLASS_NODE));
    if (node != null) {
      fd = node.classStruct.getField(field.getName(), field.getDescriptor().descriptorString);
    }
    return fd;
  }
}
