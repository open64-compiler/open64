/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import jas.Pair;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import soot.*;
import soot.dava.Dava;
import soot.dava.DavaBody;
import soot.dava.DavaPrinter;
import soot.jimple.*;
import soot.jimple.internal.*;
import soot.tagkit.AbstractHost;
import soot.tagkit.Host;
import soot.tagkit.SourceFileTag;
import soot.tagkit.Tag;
import soot.util.Chain;
import soot.util.HashChain;
import soot.util.MultiMap;
import soot.util.PhaseDumper;
import static org.assertj.core.api.Assertions.*;

import java.io.*;
import java.util.*;
import java.util.regex.Matcher;

import static io.xcalibyte.InternalConstants.*;
import static io.xcalibyte.WhirlConstants.*;

/*
    Traverse Jimple tree and generate WHIRL
    see: RectangularArrayFinder.java and TightestQualifiersTagger.java that in soot project for example
 */
class B2WGenerator extends SceneTransformer {

  private static Logger logger = LogManager.getLogger(B2WGenerator.class);

  public enum ECode {
    SUCCESS(0),
    FILE_NOT_FOUND(1),
    FILE_CANNOT_CLOSE(2),
    RUNTIME_EXCEPTION(4),
    INVALID_ARGS(5);

    private final int errCode;

    ECode(int code) {
      errCode = code;
    }

    public int toInt() {
      return errCode;
    }
  }

  /**
   *  Return-Object
   *  Data Structure for return-value
   *  Use to capture the following kinds of data (or a tuple of them):
   *  1. Category (cate)
   *  2. Symbol that is recovered from a LD/ST expr
   *  3. node that maps to a Whirl Node (WN)
   **/
  private static class R {
    enum RCate {
      NONE,
      SYM,
      NODE
    }

    RCate cate;
    long value;
    List<Long> before;
    List<Long> after;

    private R(RCate cate, long value, List<Long> before, List<Long> after) {
      this.cate = cate;
      this.value = value;
      this.before = before;
      this.after = after;
    }

    private R(RCate cate, long value) {
      this(cate, value, new ArrayList<Long>(0), new ArrayList<Long>(0));
    }

    private R(R r, long value) {
      this(r.cate, value, r.before, r.after);
    }

    /***
     * Creating a Return-Object
     * @param value to be returned.
     * @return
     */
    static R newSym(long value) {
      return new R(RCate.SYM, value);
    }

    static R newNode(long value) {
      return new R(RCate.NODE, value);
    }

    static R newNode(R r, long value) {
      assertThat(r.isNode()).as("Not node, cate : " + r.cate).isTrue();
      return new R(r, value);
    }

    static R newNone() {
      return new R(RCate.NONE, 0);
    }

    long sym() {
      assertThat(isSymbol()).as("Not symbol, cate : " + cate).isTrue();
      return value;
    }

    long node() {
      assertThat(isNode()).as("Not node, cate : " + cate).isTrue();
      return value;
    }

    void updateNode(long value) {
      this.value = value;
    }

    List<Long> getAllNode() {
      assertThat(isNode()).as("Not node, cate : " + cate).isTrue();
      List<Long> nodes = new ArrayList<>(before);
      nodes.add(value);
      nodes.addAll(after);
      return nodes;
    }

    void insertToBefore(long value) {
      assertThat(isNode()).as("Not node, cate : " + cate).isTrue();
      before.add(0, value);
    }

    void addToBefore(long value) {
      assertThat(isNode()).as("Not node, cate : " + cate).isTrue();
      before.add(value);
    }

    void addToAfter(long value) {
      assertThat(isNode()).as("Not node, cate : " + cate).isTrue();
      after.add(value);
    }

    boolean isSymbol() {
      return cate == RCate.SYM;
    }

    boolean isNode() {
      return cate == RCate.NODE;
    }

    boolean isNone() {
      return cate == RCate.NONE;
    }

    boolean isBeforeEmpty() {
      return before.isEmpty();
    }

    boolean isAfterEmpty() {
      return after.isEmpty();
    }

    boolean isExtraEmpty() {
      return before.isEmpty() && after.isEmpty();
    }
  }

  public B2WGenerator(List<String> src, boolean skip) {
    super();
    skipSrcNotFound = skip;
    srcDirectory = src;
  }

  /***
   * Data Structure to represent a try-catch pair
   ***/
  class TrapGroup {
    private Unit beginUnit;               // the start of try
    private Unit endUnit;                 // the end of try
    private ArrayList<Unit> handleUnits;  // handlers of try
    private ArrayList<RefType> excetpionTypes;  // exception types
    private Unit parentHandler;           // parent handler
    TrapGroup(Unit begin, Unit end, ArrayList<Unit> handles, ArrayList<RefType> ehTypes, Unit parnHandler) {
      beginUnit = begin;
      endUnit = end;
      handleUnits = handles;
      excetpionTypes = ehTypes;
      parentHandler = parnHandler;
    }
  }

  // dynamic inovke need to generate new class
  private static List<SootClass> handleClassList = new ArrayList<>();
  private LinkedList<Long> blockStack = new LinkedList<>();
  // save current method body stmt to wn
  private Map<Unit, Long> unitWNMap = new HashMap<>();
  // save label and target stmt
  private List<Pair<Long, Unit>> labelUnitList = new ArrayList<>();
  // invoke dynamic can't get current class info, create lambda class need to know current class
  private SootClass currentClass = null;
  // gcj need to add init class symbol stmt by our self, the following conditions need to add init class symbol stmt
  // 1. static method need to init it's own class symbol
  // 2. every method need to init class symbol that method body referenced it's static member field
  private Set<SootClass> initClassSet = new HashSet<>();
  private static Stack<Long> pendingWn = new Stack<>();
  List<Pair<Long, Unit>> regionStmtList = new ArrayList<>();

  // Trap Group List
  private List<TrapGroup> trapGpList = null;

  // Trap Tree ???? @Jason
  private Map<TrapGroup, TrapGroup> trapParent = new HashMap<>();

  // set with data return by buuldTryCatch and used by region around stmts (INITO);
  private Map<TrapGroup, Long> trapTryIdxMap = new HashMap<>();

  // stores the current source line number
  private int currSrcPos = 0;

  // Whether to skip all sources not found.
  private boolean skipSrcNotFound;

  /**
   *  Source Directory for finding source files
   */
  private final List<String> srcDirectory;

  // ======================================== Visit Host ========================================
  private void visit(AbstractHost host) {
    if (host == null) {
      return;
    }
    if (host instanceof SootClass) {
      logger.info("  Visit Class: [{}]", host.toString());
      visitClass((SootClass) host);
    } else if (host instanceof SootMethod) {
      logger.debug("  Visit Method: [{}]", host.toString());
      visitMethod((SootMethod) host);
    } else if (host instanceof SootField) {
      visitField((SootField) host);
    } else if (host instanceof Body) {
      visitBody((Body) host);
    }
  }

  private void visitClass(SootClass host) {
    // if create vtable file, just need to generate vtable information
    if (!B2WFrontEnd.i().isVTable()) {
      currentClass = host;
      TypeHandler.getType(host, true);
      Chain<SootField> fields = host.getFields();
      for (SootField field : fields) {
        visit(field);
      }
      List<SootMethod> methods = host.getMethods();
      for (SootMethod method : methods) {
        visit(method);
      }
    }
    SymbolInitializer.initClassSymbol(host);
  }

  private void visitField(SootField host) {
    // only handle static field, non-static field contained in class type
    // static field need to be in global symbol in WHIRL
    if (host.isStatic()) {
      SymbolHandler.getSymbol(host);
      SymbolInitializer.initStaticSymbol(host);
    }
  }

  static int getLineNumber(Host host) {
    int lineNum = host.getJavaSourceStartLineNumber();
    return lineNum == -1 ? 0 : lineNum;
  }

  /**
   * Soot non-static method body stmt layout
   *   this pointer
   *   arg1
   *   arg2
   *   ...
   *   stmt1
   *   ...
   * @param host
   */
  private void visitMethod(SootMethod host) {
    currSrcPos = getLineNumber(host);
    long symbolIdx = SymbolHandler.getSymbol(host);
    // abstract needn't to do anything
    if(host.isAbstract()) {
      return;
    }
    // bridge method, needn't to do anything
    // TODO: need to do more research, about bridge method
    // application/jfe/extendj/generics/bridge_method_01p-Test
    // need to generate method, if not, will cause link error
    /*
    if(Modifier.isSynthetic(host.getModifiers()) && Modifier.isVolatile(host.getModifiers())) {
      return;
    }
    */
    // native method just generate symbol
    if(host.isNative()) {
      NativeMethodCreator.createCallToNative(host);
    }

    long pool = BGenDriver.jniPoolInitialize();
    BGenDriver.jniMapTabCreate(pool);
    BGenDriver.jniNewScope(pool);
    int parCount = host.getParameterCount();
    int parIndex = 0;
    int parOffset = 0;
    // if method is non static, first parameter must be ThisRef
    if(!host.isStatic()) {
      parCount += 1;
      parOffset = 1;
    } else {
      // static method need to init it's declare class
      initClassSet.add(host.getDeclaringClass());
    }
    long[] parSymArr = new long[parCount];
    long currentEntry = BGenDriver.startFunction(symbolIdx, parCount, 2, currSrcPos);
    blockStack.push(BGenDriver.getBodyFromEntry(currentEntry));
    Body body = host.retrieveActiveBody();
    assertThat(body != null).as("Function body is null, function : " + host).isTrue();
    PatchingChain<Unit> units = body.getUnits();
    Iterator<Unit> it = units.iterator();
    // get this pointer for args
    if(!host.isStatic()) {
      Unit unit = it.next();
      assertThat(unit instanceof JIdentityStmt).as(
        "Non static method body, first stmt is not JIdentifyStmt.").isTrue();
      JIdentityStmt stmt = (JIdentityStmt) unit;
      assertThat(stmt.getRightOp() instanceof ThisRef).as(
        "Non static method body, first stmt must be ThisRef.").isTrue();
      long parSymIdx = visit(stmt.getLeftOp()).sym();
      BGenDriver.setMiscFlags(parSymIdx, WhirlConstants.FlagKind.FLAG_SET_STFLAG_EXT.toInt(),
        STFlagsExt.ST_IS_THIS_PTR.toInt());
      parSymArr[parIndex++] = parSymIdx;
    }
    // get other parameters for args
    for (; it.hasNext() && parIndex < parCount; parIndex++) {
      Unit unit = it.next();
      assertThat(unit instanceof JIdentityStmt ).as("First n stmt must be parameter.").isTrue();
      JIdentityStmt stmt = (JIdentityStmt) unit;
      long parSymIdx = visit(stmt.getLeftOp()).sym();
      assertThat(stmt.getRightOp() instanceof ParameterRef).as( "Must be function parameter.").isTrue();
      assertThat((parIndex - parOffset) == ((ParameterRef) stmt.getRightOp()).getIndex()).as(
        "Parameter order is not correct.").isTrue();
      parSymArr[parIndex] = parSymIdx;
    }
    assertThat(parIndex == parCount).as("Parameter count and parameter stmt not equal.").isTrue();
    BGenDriver.bindFunctionParams(parSymArr, symbolIdx, currentEntry);
    visit(body);
    BGenDriver.finishFunction(currSrcPos);
    blockStack.pop();
    if (host.isMain()) {
      BGenDriver.setPuIsMainPu(symbolIdx);
      BGenDriver.setPuNoInline(symbolIdx);
      BGenDriver.clearPuIsInlie(symbolIdx);
    }
  }

  // EH global variable, only use for one method
  HashChain<TrapGroup> trapGroups;
  HashMap<Unit, Integer> unitIdxMap;
  boolean needWrapByEHRegion;

  private void finishBody(Body host) {
    currSrcPos = getLineNumber(host);
    long currentBlock = blockStack.pop();

    for(SootClass clazz : initClassSet) {
      long initWN = getJvInitClass(clazz).node();
      BGenDriver.jniSetLineNum(initWN, currSrcPos);
      BGenDriver.jniInsertBlockFirst(currentBlock, initWN);
    }
    // insert label before stmt
    for (Pair<Long, Unit> entry : labelUnitList) {
      long labelWN = entry.getO1();
      Unit unit = entry.getO2();
      long targetWN = unitWNMap.getOrDefault(unit, 0L);
      assertThat(targetWN).as( "Target stmt wn not found: " + unit.toString()).isNotEqualTo(0L);
      BGenDriver.jniSetLineNum(labelWN, getLineNumber(unit));
      long parentBlock = BGenDriver.jniGetParentBlock(targetWN);
      BGenDriver.jniInsertBlockBefore(parentBlock, labelWN, targetWN);
      BGenDriver.jniSetLineNum(labelWN, getLineNumber(unit));
    }

    // generate Exception Handling code,
    // where it's okay to use labelUnitMap to initialize INITO with
    // Data Flow: INITO -> REGION -> STMT -> (TrapGroup) -> UNIT -> LABEL
    if(B2WFrontEnd.i().isGenEH()) {
      visitTraps(host);
    }

    // clear all map
    unitWNMap.clear();
    labelUnitList.clear();
    initClassSet.clear();
    regionStmtList.clear();
    SymbolHandler.finishMethodLocals();
    BGenDriver.jniInsertBlockLast(blockStack.peek(), currentBlock);
  }

  private long createEHRegion(long stmtWN) {
    long bodyWN = BGenDriver.jniCreateBlock();
    BGenDriver.jniInsertBlockLast(bodyWN, stmtWN);
    long line = BGenDriver.jniGetLineNum(stmtWN);
    BGenDriver.jniSetLineNum(bodyWN, BGenDriver.jniGetLineNum((stmtWN)));
    long pragmaWN = BGenDriver.jniCreateBlock();
    long exitWN = BGenDriver.jniCreateBlock();
    return BGenDriver.jniBuildEHRegion(bodyWN, exitWN, pragmaWN, 0);
  }

  private void visitBody(Body host) {
    ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_VISIT_BODY);

    assertThat(blockStack.size()).as( "Block stack size is 0.").isGreaterThan(0);
    long block = BGenDriver.jniCreateBlock();
    blockStack.push(block);
    BGenDriver.jniSetLineNum(block, getLineNumber(host));

    Chain<Unit> units = host.getUnits();
    List<SootClass> ehThrowsList = host.getMethod().getExceptions();
    long[] ehThrowsSym = new long[ehThrowsList.size()];
    for (int i = 0; i < ehThrowsList.size(); i++) {
      Local typeSym = Jimple.v().newLocal(ehThrowsList.get(i).getName(), ehThrowsList.get(i).getType());
      ehThrowsSym[i] = SymbolHandler.getSymbol(typeSym);
    }
    trapGroups = new HashChain<>();
    unitIdxMap = new HashMap<>();
    trapGpList = null;
    BGenDriver.jniInitEHSym(ehThrowsSym);
    if(B2WFrontEnd.i().isGenEH()) {
      if(host.getTraps().size() > 0) {
        buildTrapGroups(host);
        buildUnitSequence(units);
        buildTrapGroupTree(new ArrayList<>(trapGroups));
      }
    }

    for (Unit unit : units) {
      R r;
      if (unit instanceof JIdentityStmt) {
        r = visitJIdentityStmt((JIdentityStmt) unit);
      } else {
        r = visit(unit);
      }
      if (r.isNode()) {
        for (long n : r.getAllNode()) {
          BGenDriver.jniInsertBlockLast(blockStack.peek(), n);
        }
      }
    }

    finishBody(host);
    ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_VISIT_BODY);
  }

  private HashMap<Unit, Integer> unitSequence = null;
  private List<Unit> unitList = new ArrayList();
  private void buildUnitSequence(Chain<Unit> units) {
    unitList.addAll(units);
    unitSequence = new HashMap<>();
    int i = 0;
    for(Unit un : unitList){
      assertThat(unitSequence.containsKey(un)).isFalse();
      unitSequence.put(un, i);
      i++;
    }
  }

  private Boolean isSameTry(Trap trap1, Trap trap2) {
    return trap1.getBeginUnit() == trap2.getBeginUnit() &&
       trap1.getEndUnit() == trap2.getEndUnit();
  }

  /**
   * Check if the trap1 is the parent of trap2
   * @param trap1  the assumed parent
   * @param trap2  the assumed child
   * @param unitIdxMap
   * @return
   */
  private Boolean isParentTry(Trap trap1, Trap trap2, HashMap<Unit,Integer> unitIdxMap) {
    Unit beginUnit1 = trap1.getBeginUnit();
    Unit endUnit1 = trap1.getEndUnit();
    Unit beginUnit2 = trap2.getBeginUnit();
    Unit endUnit2 = trap2.getEndUnit();

    assertThat(unitIdxMap.containsKey(beginUnit1)).as( "no unit found in unitIdxMap").isTrue();
    assertThat(unitIdxMap.containsKey(beginUnit2)).as( "no unit found in unitIdxMap").isTrue();
    assertThat(unitIdxMap.containsKey(endUnit1)).as("no unit found in unitIdxMap").isTrue();
    assertThat(unitIdxMap.containsKey(endUnit2)).as("no unit found in unitIdxMap").isTrue();

    int beginIdx1 = unitIdxMap.get(beginUnit1);
    int endIdx1 = unitIdxMap.get(endUnit1);
    int beginIdx2 = unitIdxMap.get(beginUnit2);
    int endIdx2 = unitIdxMap.get(endUnit2);
    if (beginIdx1 == beginIdx2 && endIdx1 == endIdx2) {
      return false;
    }
    return beginIdx2 >= beginIdx1 && endIdx2 <= endIdx1;
  }

  private void buildTrapGroups(Body host) {
    Chain<Trap> traps = host.getTraps();
    HashMap<Unit, Unit> tryRange = new HashMap<>();

    Chain<Unit> units = host.getUnits();
    int unitIdx = 0;
    for (Unit unit : units) {
      unitIdxMap.put(unit, unitIdx);
      unitIdx++;
    }

    /**
     * Building up the EH-Type table that contains
     * all the exceptions needed in the trap-group
     */
    for (Trap trap : traps) {
      Unit beginUnit = trap.getBeginUnit();
      Unit endUnit = trap.getEndUnit();
      if (tryRange.containsKey(beginUnit) && tryRange.get(beginUnit) == endUnit) {
        continue;
      }
      Unit handleUnit = trap.getHandlerUnit();
      ArrayList<Unit> handleUnits = new ArrayList<Unit>();
      ArrayList<RefType> ehTypes  = new ArrayList<RefType>();
      // TODO
      Unit parentHandlerUnit = null;
      handleUnits.add(handleUnit);
      assertThat(trap.getException().hasRefType()).as("exception don't have reference type").isTrue();

      ehTypes.add(trap.getException().getType());
      Trap nextTrap = traps.getSuccOf(trap);
      if (nextTrap != null) {
        Iterator<Trap> trapIter = traps.iterator(nextTrap, traps.getLast());
        while (trapIter.hasNext()) {
          Trap trapValue = trapIter.next();
          // Multiple trapValue with same try
          if (isSameTry(trapValue, trap)) {
            Unit nextHandleUnit = trapValue.getHandlerUnit();
            handleUnits.add(nextHandleUnit);
            assertThat(trapValue.getException().hasRefType()).as("exception don't have reference type").isTrue();
            ehTypes.add(trapValue.getException().getType());
          } else if (isParentTry(trapValue, trap, unitIdxMap) &&
            parentHandlerUnit == null) {
            parentHandlerUnit = trapValue.getHandlerUnit();
          }
        }
      }
      TrapGroup trapGroup = new TrapGroup(beginUnit, endUnit, handleUnits, ehTypes, parentHandlerUnit);
      trapGroups.addLast(trapGroup);
      tryRange.put(beginUnit, endUnit);
    }
  }

  /**
   * Is the Unit(Stmt) inside a trapGroup?
   * @param trapGp
   * @param u unit
   * @return
   */
  private boolean isInTrap(TrapGroup trapGp, Unit u) {
    long beginIdx = unitIdxMap.get(trapGp.beginUnit);
    long endIdx = unitIdxMap.get(trapGp.endUnit);
    long uIdx = unitIdxMap.get(u);
    return beginIdx <= uIdx && uIdx < endIdx;
  }

  /**
   * Build up a trap-group tree (Topo-sorted)
   * Save to trapGpList
   * @param newList
   */
  private void buildTrapGroupTree(List<TrapGroup> newList) {
    // Efficiency of O(n^2)
    int totalSize = newList.size();
    newList.sort((x, y) ->
    { return (unitSequence.get(x.endUnit)  < unitSequence.get(y.endUnit) ? -1 :
             (unitSequence.get(x.endUnit)  > unitSequence.get(y.endUnit) ? 1 :
              (
                unitSequence.get(x.beginUnit) < unitSequence.get(y.beginUnit) ? -1 :
                unitSequence.get(x.beginUnit) > unitSequence.get(y.beginUnit) ? 1 : 0
              )
             ));
    });
    for(int i = totalSize - 1 ; i >= 0; i--){
      TrapGroup tg = newList.get(i);
      int beginOfst = unitSequence.get(tg.beginUnit);
      boolean found = false;
      for (int j = i + 1; j < totalSize; j++) {
        if (unitSequence.get(newList.get(j).beginUnit) <= beginOfst) {
          trapParent.put(tg, newList.get(j));
          found = true;
          break;
        }
      }
      if(!found){
        trapParent.put(tg, null);
      }
    }
    this.trapGpList = newList;
    this.trapGpList.sort((x,y) ->
    { return (unitSequence.get(x.beginUnit)  < unitSequence.get(y.beginUnit) ? -1 :
            (unitSequence.get(x.beginUnit)  > unitSequence.get(y.beginUnit) ? 1 :
              (
                unitSequence.get(x.endUnit) < unitSequence.get(y.endUnit) ? 1 :
                unitSequence.get(x.endUnit) > unitSequence.get(y.endUnit) ? -1 : 0
              )
            ));
    });
  }

  private void visitTraps(Body host) {
    ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_GEN_EH);
    int trapSize = host.getTraps().size();
    Map<TrapGroup, Pair<long[], long[]>> trapHandlerMap = new HashMap<>();
    long tryIdx = 0;
    // Step 1: Generate trapGroups, put multiple catch with same try block into one group
    //         Mark the parent handler
    // Step 2: generate EH for each try
    if(trapSize > 0) {
      BGenDriver.jniBuildEHBegin();

      for (TrapGroup trapGp : trapGroups) {
        Unit beginUnit = trapGp.beginUnit;
        Unit endUnit = trapGp.endUnit;
        Unit parentUnit = trapGp.parentHandler;
        ArrayList<Unit> handlerUnits = trapGp.handleUnits;
        ArrayList<RefType> ehTypes = trapGp.excetpionTypes;

        assertThat(unitWNMap.containsKey(beginUnit)).as("cannot find begin wn").isTrue();
        assertThat(unitWNMap.containsKey(endUnit)).as("cannot find end wn").isTrue();
        long beginWn = unitWNMap.get(beginUnit);
        long endWn = unitWNMap.get(endUnit);
        long parentHandlerWN = 0;
        if (parentUnit != null) {
          assertThat(unitWNMap.containsKey(parentUnit)).as("cannot find parent handler wn").isTrue();
          parentHandlerWN = unitWNMap.get(parentUnit);
        }

        assertThat(handlerUnits.size() > 0).as("should have handlers").isTrue();
        long handlerWn[] = new long[handlerUnits.size()];
        long handlerSt[] = new long[handlerUnits.size()];
        List<Long> handlerNoThrowableList = new ArrayList<>();
        int idx = 0;
        for (Unit handlerUnit : handlerUnits) {
          assertThat(unitWNMap.containsKey(handlerUnit)).as("cannot find handler wn").isTrue();
          assertThat(handlerUnit instanceof JIdentityStmt).as("handler unit should be JIdentityStmt").isTrue();
          assertThat(((JIdentityStmt) handlerUnit).getRightOp() instanceof JCaughtExceptionRef).as(
            "handler unit should be caught exception").isTrue();
          handlerWn[idx] = unitWNMap.get(handlerUnit);
          RefType ehType = ehTypes.get(idx);
          long stIdx = SymbolHandler.getEHTypeSymbol(ehType);
          if (!ehType.getSootClass().getName().equals("java.lang.Throwable")) {
            handlerNoThrowableList.add(stIdx);
          } else {
            handlerNoThrowableList.add(0L);
          }
          handlerSt[idx] = stIdx;
          idx++;
        }
        trapHandlerMap.put(trapGp, new Pair<>(handlerWn, handlerNoThrowableList.stream().mapToLong(x -> x.longValue()).toArray()));
        tryIdx = BGenDriver.jniBuildTryCatch(beginWn, endWn, parentHandlerWN, handlerWn, handlerSt);
        trapTryIdxMap.put(trapGp, tryIdx);
      }

    }
    if(B2WFrontEnd.i().isGenFullEH()) {
      for (Pair<Long, Unit> entry : regionStmtList) {
        long regionWN = entry.getO1();
        Unit stmtUnit = entry.getO2();
        long regionHandlerLabel = 0;
        long[] exceptionSymArr = new long[0];
        int size = (trapGpList == null) ? 0 : trapGpList.size();
        int lastTrapGpIndex = -1;
        for (int i = 0; i < size; i++) {
          TrapGroup trapGp = trapGpList.get(i);
          if (isInTrap(trapGp, stmtUnit))
            lastTrapGpIndex = i;
        }
        List<Long> exceptionSymList = new ArrayList<>();
        if(size > 0 && lastTrapGpIndex >= 0)
        {
          assertThat(lastTrapGpIndex).as("minimal trap group covering the stmt {} must exist!", stmtUnit).isGreaterThanOrEqualTo(0);
          TrapGroup current = trapGpList.get(lastTrapGpIndex);
          int trialNums = 0;
          while(current != null) {
            Pair<long[], long[]> handlerPair = trapHandlerMap.get(current);
            if(regionHandlerLabel == 0) {
              tryIdx = trapTryIdxMap.get(current);
              regionHandlerLabel = BGenDriver.getTryComparator(tryIdx);
            }
            for (long v : handlerPair.getO2()) {
              if(!exceptionSymList.contains(v)){
                exceptionSymList.add(v);
              }
            }
            // Finding all symbols for trapGroup which contains the statement
            if (trapParent.get(current) != null){
              current = trapParent.get(current);
            } else {
              break;
            }
            assertThat(trialNums ++).as("Parent is recursive and running infinitely").isLessThan(9999);
          }
          assertThat(regionHandlerLabel == 0 || exceptionSymList.size() > 0).as("Either no TrapGroup or having at least one catchable exception in that TrapGroup").isTrue();
          exceptionSymArr = exceptionSymList.stream().mapToLong(Long::longValue).toArray();
        }
        long initoSymIdx = SymbolHandler.getEHHandlerSymbol(exceptionSymArr.length);
        BGenDriver.jniSetupRegionInito(regionWN, regionHandlerLabel, initoSymIdx, exceptionSymArr);
        // TODO: make symbol a array
      }
    }
    if(trapSize > 0) {
      BGenDriver.jniBuildEHEnd();
    }
    ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_GEN_EH);
  }

  // ======================================== Visit Unit ========================================
  private R visit(Unit unit) {
    R r = null;
    if (unit instanceof Stmt) {
      r = visit((Stmt) unit);
    } else {
      assertThat(false).as("Don't support unit: " + unit.toString()).isTrue();
    }
    return r;
  }

  /***
   * Visit any statement should be done in this process
   * @param stmt
   * @return
   */
  private R visit(Stmt stmt) {
    R r = null;
    currSrcPos = getLineNumber(stmt);

    if (stmt instanceof JAssignStmt) {
      r = visitJAssignStmt((JAssignStmt) stmt);
    } else if (stmt instanceof JIfStmt) {
      r = visitJIfStmt((JIfStmt) stmt);
    } else if (stmt instanceof JGotoStmt) {
      r = visitJGotoStmt((JGotoStmt) stmt);
    } else if (stmt instanceof JReturnVoidStmt) {
      r = visitJReturnVoidStmt();
    } else if (stmt instanceof JReturnStmt) {
      r = visitJReturnStmt((JReturnStmt) stmt);
    } else if (stmt instanceof JInvokeStmt) {
      r = visitJInvokeStmt((JInvokeStmt) stmt);
    } else if (stmt instanceof JLookupSwitchStmt) {
      r = visitJLookupSwitchStmt((JLookupSwitchStmt) stmt);
    } else if (stmt instanceof JTableSwitchStmt) {
      r = visitJTableSwitchStmt((JTableSwitchStmt)(stmt));
    } else if (stmt instanceof JThrowStmt) {
      r = visitJThrowStmt((JThrowStmt)stmt);
    } else if (stmt instanceof JEnterMonitorStmt || stmt instanceof JExitMonitorStmt) {
      r = visitMonitorStmt((MonitorStmt)(stmt));
    } else if (stmt instanceof JNopStmt) {
      r = visitJNopStmt();
    } else if (stmt instanceof JNativeStmt) {
      r = visitJNativeStmt((JNativeStmt) stmt);
    } else {
      assertThat(false).as("Not support unit: " + stmt.toString()).isTrue();
    }
    // soot may create some statement that do not have line number info
    // we can not distinguish them for now, comment below assert to let it pass
    // assertThat(stmt.getJavaSourceStartLineNumber() > 0).as( "Can't find line number.").isTrue();
    boolean first = true;
    for (Long node : r.getAllNode()) {
      BGenDriver.jniSetLineNum(node, currSrcPos);
      if (first) {
        unitWNMap.put(stmt, node);
        first = false;
      }
    }
    if (needWrapByEHRegion && B2WFrontEnd.i().isGenFullEH()) {
      long regionWN = createEHRegion(r.value);
      unitWNMap.put(stmt, regionWN);
      r.updateNode(regionWN);
      regionStmtList.add(new Pair(regionWN, stmt));
      needWrapByEHRegion = false;
    }
    while (!pendingWn.empty()) {
      BGenDriver.jniSetLineNum(pendingWn.pop(), currSrcPos);
    }
    return r;
  }

  private R visitJIdentityStmt(JIdentityStmt stmt) {
    assertThat(isSymbolValue(stmt.getLeftOp())).as(
      "JIdentifyStmt left op is not a var symbol, left: " + stmt.getLeftOp().toString()).isTrue();
    // if right op is var symbol, needn't return anything
    visit(stmt.getLeftOp());
    Value rightOp = stmt.getRightOp();
    assertThat(
      rightOp instanceof ThisRef || rightOp instanceof ParameterRef || rightOp instanceof CaughtExceptionRef
    ).as("JIdentityStmt right op is not ThisRef ParameterRef or CaughtExceptionRef, right op : " +
      rightOp.toString()).isTrue();
    if(rightOp instanceof JCaughtExceptionRef) {
      Value left = stmt.getLeftOp();
      R leftSym = visit(left);
      assertThat(leftSym.isSymbol()).as("caught left should be a symbol").isTrue();
      Type leftType = left.getType();
      long leftTypeIdx = TypeHandler.getType(leftType);
      long leftSymOffset = 0;
      long rightSym = BGenDriver.jniGetExecPtr();
      long rightType = BGenDriver.getTyFromST(rightSym);
      int rightMtype = BGenDriver.getMTypeFromTypeIdx(rightType);
      long ldExecPtr = BGenDriver.jniLDID(rightMtype, 0, rightSym, rightType);

      // the execPtr offset is equal to word length
      long execPtrOffset = -1 * BCRConfig.getWordBytes();
      long ty_idx = BGenDriver.jniMakePointerType(BGenDriver.getPrimitiveType(Mtype.MTYPE_V.toInt()));
      long ildExecPtr = BGenDriver.jniILoad(BCRConfig.getPtrType(), execPtrOffset, ty_idx,ldExecPtr, 0);

      long storeWn = BGenDriver.jniSTID(BGenDriver.getMTypeFromTypeIdx(leftTypeIdx),leftSymOffset, leftSym.sym(), leftTypeIdx, ildExecPtr);
      BGenDriver.jniSetLineNum(storeWn, getLineNumber(stmt));
      unitWNMap.put(stmt, storeWn);
      return R.newNode(storeWn);
    }
    return R.newNone();
  }

  private R visitJAssignStmt(JAssignStmt stmt) {
    Value left = stmt.getLeftOp();
    Value right = stmt.getRightOp();
    R rightR = visitRightValue(right);
    R leftR = visit(left);

    assertThat(leftR.isExtraEmpty()).as("Left expr can't return multi node.").isTrue();
    // need to generate ISTORE for array type like
    //   I4INTCONST 1
    //     XXXX
    //   U4ARRAY
    // ISTORE
    // TODO: a[i] = 0; need to do more things
    if (isSymbolValue(left)) {
      long leftSymIdx = leftR.sym();
      long leftSymOffset = 0;
      long leftSymTypeIdx = BGenDriver.getTyFromST(leftSymIdx);
      if (left instanceof JInstanceFieldRef) {
        JInstanceFieldRef fieldRef = (JInstanceFieldRef) left;
        // FIXME: field member type should use pointer type? need to re-fix null type
        if (fieldRef.getBase().getType() instanceof NullType) {
          leftSymTypeIdx = TypeHandler.getType(((JInstanceFieldRef) left).getField().getDeclaringClass());
          leftSymTypeIdx = BGenDriver.jniMakePointerType(leftSymTypeIdx);
        }
        // field reference IStore need real field type as descriptor type
        // for ex:  field float a;  need type float F4
        int pointeeMtype = JavaToWhirlConst.getMType(left.getType());
        long ldidWN = BGenDriver.jniLDID(JavaToWhirlConst.getMType(fieldRef.getBase().getType()),
          0, leftSymIdx, leftSymTypeIdx);
        return R.newNode(rightR, BGenDriver.jniIStore(pointeeMtype, leftSymOffset,
          leftSymTypeIdx, ldidWN, rightR.node(), TypeHandler.getFieldIdx(fieldRef.getFieldRef())));
      } else {
        int rType = BGenDriver.getMTypeFromTypeIdx(leftSymTypeIdx);
        return R.newNode(rightR, BGenDriver.jniSTID(rType, leftSymOffset, leftSymIdx,
          leftSymTypeIdx, rightR.node()));
      }
    } else if (left instanceof JArrayRef) {
      JArrayRef arrRef = (JArrayRef) left;
      int desc = JavaToWhirlConst.getMType(arrRef.getType());
      long leftTypeIdx = BGenDriver.getPrimitiveType(desc);
      return R.newNode(rightR, BGenDriver.jniIStore(desc, 0,
        BGenDriver.jniMakePointerType(leftTypeIdx), leftR.node(), rightR.node(), 0));
    } else {
      assertThat(false).as("Not support assign left value, left : " + left).isTrue();
      return R.newNone();
    }
  }

  private R visitJIfStmt(JIfStmt stmt) {
    long condWN = visitRightValue(stmt.getCondition()).node();
    long labelWN = BGenDriver.jniLabel();
    labelUnitList.add(new Pair(labelWN, stmt.getTarget()));
    return R.newNode(BGenDriver.jniCreateTrueBr(BGenDriver.getLabelNum(labelWN), condWN));
  }

  private R visitJGotoStmt(JGotoStmt stmt) {
    long labelWN = BGenDriver.jniLabel();
    labelUnitList.add(new Pair(labelWN, stmt.getTarget()));
    return R.newNode(BGenDriver.jniCreateGoto(labelWN, currSrcPos));
  }

  private R visitJReturnVoidStmt() {
    return R.newNode(BGenDriver.jniCreateReturn());
  }

  private R visitJReturnStmt(JReturnStmt stmt) {
    long node = visitRightValue(stmt.getOp()).node();
    return R.newNode(BGenDriver.jniCreateReturnVal(node));
  }

  private R visitJInvokeStmt(JInvokeStmt stmt) {
    // the invoke Expr generated is expression when have return value
    // need to store the ret value to a symbol to generate a stmt
    // otherwise, if just return the expression, there would be
    // error when set line number(as line can only be set on stmt)
    R node = visit(stmt.getInvokeExpr());
    if(node.isNode()) {
      Type retType = stmt.getInvokeExpr().getMethodRef().returnType();
      if(!(retType instanceof VoidType)) {
        int retMType = JavaToWhirlConst.getMType(retType);
        long pregSt = BGenDriver.createPregSymbol(retMType);
        long resPreg = BGenDriver.createPreg(retMType, "temp_comma_preg");
        long pregStType = BGenDriver.getTyFromST(pregSt);
        R ret = R.newNode(node, BGenDriver.jniSTID(retMType, resPreg, pregSt, pregStType, node.node()));
        return ret;
      }
    }
    return node;
  }

  private boolean isSymbolValue(Value value) {
    return value instanceof Local || value instanceof FieldRef;
  }


  private R createCaseGoto(int index, Unit target) {
    long caseLabelWN = BGenDriver.jniLabel();
    long caseLabelNumber = BGenDriver.getLabelNum(caseLabelWN);
    long caseWN = BGenDriver.jniCreateCasegoto(index, caseLabelNumber);
    labelUnitList.add(new Pair(caseLabelWN, target));
    return R.newNode(caseWN);
  }

  private R visitJTableSwitchStmt(JTableSwitchStmt stmt) {
    int lowIndex = stmt.getLowIndex();
    int highIndex = stmt.getHighIndex();
    long caseBlock = BGenDriver.jniCreateBlock();
    // In this for-loop, we cannot use "<=" since 'i' would wrap around.
    // The case for "i == highIndex" is handled separately after the loop.
    for(int i = lowIndex; i < highIndex; i++) {
      R caseWN = createCaseGoto(i , stmt.getTarget(i - lowIndex));
      BGenDriver.jniInsertBlockLast(caseBlock, caseWN.node());
      BGenDriver.jniSetLineNum(caseWN.node(), currSrcPos);
    }
    R highCaseWN = createCaseGoto(highIndex, stmt.getTarget(highIndex - lowIndex));
    BGenDriver.jniInsertBlockLast(caseBlock, highCaseWN.node());
    BGenDriver.jniSetLineNum(highCaseWN.node(), currSrcPos);

    long defaultLabelWN = BGenDriver.jniLabel();
    long defaultLabelNumber = BGenDriver.getLabelNum(defaultLabelWN);
    long defaultWN = BGenDriver.jniCreateGoto(defaultLabelWN, currSrcPos);
    labelUnitList.add(new Pair(defaultLabelWN, stmt.getDefaultTarget()));

    Value key = stmt.getKey();
    long keyWN = visitRightValue(key).node();

    return R.newNode(
      BGenDriver.jniCreateSwitch(highIndex - lowIndex + 1, keyWN, caseBlock, defaultWN, defaultLabelNumber));
  }

  private R visitJLookupSwitchStmt(JLookupSwitchStmt stmt) {
    Value key = stmt.getKey();
    long keyWN = visitRightValue(key).node();
    long caseBlock = BGenDriver.jniCreateBlock();
    BGenDriver.jniSetLineNum(caseBlock, currSrcPos);
    for (int i = 0; i < stmt.getLookupValues().size(); i++) {
      R caseWN = createCaseGoto(stmt.getLookupValues().get(i).value, stmt.getTarget(i));
      BGenDriver.jniInsertBlockLast(caseBlock, caseWN.node());
      BGenDriver.jniSetLineNum(caseWN.node(), currSrcPos);
    }
    long defaultLabelWN = BGenDriver.jniLabel();
    long defaultLabelNumber = BGenDriver.getLabelNum(defaultLabelWN);
    long defaultWN = BGenDriver.jniCreateGoto(defaultLabelWN, currSrcPos);
    BGenDriver.jniSetLineNum(defaultWN, currSrcPos);
    labelUnitList.add(new Pair(defaultLabelWN, stmt.getDefaultTarget()));
    return R.newNode(
      BGenDriver.jniCreateSwitch(stmt.getLookupValues().size(), keyWN, caseBlock, defaultWN, defaultLabelNumber));
  }

  // ======================================== Visit Value ========================================
  private R visit(Value value) {
    R r = null;
    if (value instanceof JimpleLocal) {
      r = visitJimpleLocal((JimpleLocal) value);
    } else if (value instanceof StaticFieldRef) {
      r = visitStaticFieldRef((StaticFieldRef) value);
    } else if (value instanceof JInstanceFieldRef) {
      r = visitJInstanceFieldRef((JInstanceFieldRef) value);
    } else if (value instanceof IntConstant) {
      r = visitIntConstant((IntConstant) value);
    } else if (value instanceof LongConstant) {
      r = visitLongConstant((LongConstant) value);
    } else if (value instanceof FloatConstant) {
      r = visitFloatConst((FloatConstant) value);
    } else if (value instanceof DoubleConstant) {
      r = visitDoubleConst((DoubleConstant) value);
    } else if (value instanceof StringConstant) {
      r = visitStringConst((StringConstant) value);
    } else if (value instanceof BinopExpr) {
      r = visitBinopExpr((BinopExpr) value);
    } else if (value instanceof JCastExpr) {
      r = visitCastExpr((JCastExpr) value);
    } else if (value instanceof AbstractInvokeExpr) {
      r = visitAbstractInvokeExpr((AbstractInvokeExpr) value);
    } else if (value instanceof JNewArrayExpr) {
      r = visitJNewArrayExpr((JNewArrayExpr) value);
    } else if (value instanceof JArrayRef) {
      r = visitJArrayRef((JArrayRef) value);
    } else if (value instanceof JNewMultiArrayExpr) {
      r = visitJNewMultiArrayExpr((JNewMultiArrayExpr) value);
    } else if (value instanceof JNegExpr) {
      r = visitJNegExpr((JNegExpr) value);
    } else if (value instanceof JLengthExpr) {
      r = visitJLengthExpr((JLengthExpr) value);
    } else if (value instanceof NullConstant) {
      r = visitNullConstant();
    } else if (value instanceof ClassConstant) {
      r = visitClassConstant((ClassConstant) value);
    } else if (value instanceof JInstanceOfExpr) {
      r = visitJInstanceOfExpr((JInstanceOfExpr) value);
    } else if (value instanceof JNewExpr) {
      r = visitJNewExpr((JNewExpr) value);
    } else if (value instanceof JNativeExpr) {
      r = visitJNativeExpr((JNativeExpr) value);
    } else {
      assertThat(false).as("Not support value: " + value.toString()).isTrue();
    }
    return r;
  }

  // if value is right value, wrap it as ldid
  private R visitRightValue(Value value) {
    // if value is symbol, wn is symbol index
    // otherwise it is Whirl node pointer
    R r = visit(value);
    if(r.isSymbol()) {
      long symIdx = r.sym();
      long rightSymOffset = 0;

      if(value instanceof JInstanceFieldRef) {
        JInstanceFieldRef fieldRef = (JInstanceFieldRef) value;
        long tyIdx = BGenDriver.getTyFromST(symIdx);
        long ldidWN = BGenDriver.jniLDID(JavaToWhirlConst.getMType(fieldRef.getBase().getType()),
          0, symIdx, tyIdx);

        Type valueBaseType =((JInstanceFieldRef)value).getBase().getType();
        long fieldIdx = 0;
        if(valueBaseType instanceof RefType) {
          tyIdx = BGenDriver.getPointeeTyFromTY(tyIdx);
          fieldIdx = TypeHandler.getFieldIdx(fieldRef.getFieldRef());
        } else {
          // for nullType, as there is no class info, set the fieldIdx to 0
          assertThat(valueBaseType instanceof NullType).as(
            "Assume a RefType/NullType base for JInstanceFieldRef but not " + valueBaseType.toString()).isTrue();
        }
        return R.newNode(BGenDriver.jniILoad(JavaToWhirlConst.getMType(value.getType()), 0,
          tyIdx, ldidWN, fieldIdx));
      } else {
        long rightSymTypeIdx = BGenDriver.getTyFromST(symIdx);
        r = R.newNode(BGenDriver.jniLDID(JavaToWhirlConst.getMType(value.getType()),
          rightSymOffset, symIdx, rightSymTypeIdx));
      }
    } else if(value instanceof JArrayRef) {
      assertThat(r.isNode()).as("right value of Array Ref should be address wn").isTrue();
      long wn = r.node();
      Type rType = value.getType();
      Type baseType = ((JArrayRef)value).getBase().getType();
      Type descType = null;
      if(baseType instanceof ArrayType) {
        descType = ((ArrayType) baseType).baseType;
      } else if(baseType instanceof NullType) {
        descType = baseType;
      } else {
        assertThat(false).as("arryRef base should be array Type or null Type but not " + baseType.toString()).isTrue();
      }
      int mType = JavaToWhirlConst.getMType(rType);
      long typeIdx = TypeHandler.getType(descType);
      long iload = BGenDriver.jniILoad(mType, 0, typeIdx, wn, 0);
      r = R.newNode(r, iload);
    }
    return r;
  }

  private R visitJimpleLocal(JimpleLocal value) {
    return R.newSym(SymbolHandler.getSymbol(value));
  }

  private R getJvInitClass(SootClass clazz){
//    needWrapByEHRegion = true;
    long classsym = SymbolHandler.getClassInstanceSymbol(clazz);
    assertThat(classsym).isGreaterThan(0);
    int  ptrType = BCRConfig.getPtrType();
    long ldclasssym = BGenDriver.jniLDA(ptrType, 0, classsym);
    int  vtype = Mtype.MTYPE_V.toInt();
    int[] rMType = {ptrType};
    long[] kids = {ldclasssym};
    long[] kidsType = {TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR)};

    long jvInitNode = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      vtype, -1,
      Intrinsic.INTRN_INIT_CLASS.toInt(),
      rMType, kidsType, kids);

    requestLineNum(jvInitNode);
    return R.newNode(jvInitNode);
  }

  private R visitStaticFieldRef(StaticFieldRef value) {
    initClassSet.add(value.getFieldRef().declaringClass());
    return R.newSym(SymbolHandler.getSymbol(value));
  }

  private R visitJInstanceFieldRef(JInstanceFieldRef value) {
    Value base = value.getBase();
    assertThat(base instanceof JimpleLocal).as(
      "JInstanceFieldRef base is not JimpleLocal, base : " + base.toString()).isTrue();
    return R.newSym(SymbolHandler.getSymbol((JimpleLocal) base));
  }

  private R visitIntConstant(IntConstant value) {
    return R.newNode(BGenDriver.jniIntConst(JavaToWhirlConst.getMType(value.getType()), value.value));
  }

  private R visitLongConstant(LongConstant value) {
    return R.newNode(BGenDriver.jniIntConst(JavaToWhirlConst.getMType(value.getType()), value.value));
  }

  private R visitFloatConst(FloatConstant value) {
    return R.newNode(BGenDriver.jniFloatConst(JavaToWhirlConst.getMType(value.getType()), (double) value.value));
  }

  private R visitDoubleConst(DoubleConstant value) {
    return R.newNode(BGenDriver.jniFloatConst(JavaToWhirlConst.getMType(value.getType()), value.value));
  }

  private R visitStringConst(StringConstant value) {
    Type type = value.getType();
    assertThat(type instanceof RefType ).as("classConstant not reference type").isTrue();
    long stringSym = SymbolInitializer.initStringConst(value.value);
    int pos = SymbolHandler.getClassInfo(currentClass).addConstant(
      SymbolHandler.JvConstant.getConstVal(JvConstantType.JV_CONSTANT_String, stringSym));
    long symIdx = SymbolHandler.getInternal(currentClass, SymbolCategory.GCJ_CONSTANTS_DATA_BASE);
    long typeIdx = TypeHandler.getType(value.getType());
    return R.newNode(BGenDriver.jniLDID(
            JavaToWhirlConst.getMType(type), pos * BCRConfig.ALIGN, symIdx, typeIdx));
  }

  private R visitNullConstant() {
    // generate type U8 and value 0 for null constant
    return R.newNode(BGenDriver.jniIntConst(BCRConfig.getPtrType(), 0));
  }

  private R visitClassConstant(ClassConstant value) {
    assertThat(value.toSootType() instanceof RefType ||
            (value.toSootType() instanceof ArrayType))
            .as("[Class constant has to be a class or array!], given: " + value).isTrue();
    Type type = value.toSootType();
    String mangledSign = MangleTool.mangleForClassConstant(type);
    long stringSym = SymbolInitializer.initStringConst(mangledSign);
    int pos = SymbolHandler.getClassInfo(currentClass).addConstant(
      SymbolHandler.JvConstant.getConstVal(JvConstantType.JV_CONSTANT_String, stringSym));
    long symIdx = SymbolHandler.getInternal(currentClass, SymbolCategory.GCJ_CONSTANTS_DATA_BASE);
    long typeIdx = TypeHandler.getType(value.getType());
    return R.newNode(BGenDriver.jniLDID(
            JavaToWhirlConst.getMType(type), pos * BCRConfig.ALIGN, symIdx, typeIdx));
  }

  private R visitJInstanceOfExpr(JInstanceOfExpr value) {
    Value leftValue = value.getOp();
    Type checkType = value.getCheckType();
    assertThat(checkType instanceof RefType || checkType instanceof ArrayType).as("check GCJType should be reference type").isTrue();
    if(!(leftValue.getType() instanceof NullType)) {
      assertThat(isSymbolValue(leftValue)).as("instanceof left value should be a local or static field").isTrue();
    }
    // 1. Class <-> Class (::class$ == ::class$)
    R leftR = visit(leftValue);
    long ldidThisWN;
    int ptrType = BCRConfig.getPtrType();
    if(! (leftValue instanceof NullConstant)) {
      long leftSym = leftR.sym();
      ldidThisWN = BGenDriver.jniLDID(ptrType, 0, leftSym, BGenDriver.getTyFromST(leftSym));
    } else {
      // for NullConstant, pass the load const wn to intrinsic, let backend to analyze
      ldidThisWN = leftR.node();
    }
    long checkClassSym = 0;
    if(checkType instanceof ArrayType) {
      checkClassSym = SymbolHandler.getArrayInstanceSymbol((ArrayType)checkType);
    } else if(checkType instanceof RefType) {
      checkClassSym = SymbolHandler.getClassInstanceSymbol(((RefType) checkType).getSootClass());
    }
    long ldidCheckClassWN = BGenDriver.jniLDA(ptrType, 0,  checkClassSym);
    int rtype = Mtype.MTYPE_I4.toInt();
    int[]  paramRtype = {ptrType, ptrType};
    long[] kids = {ldidThisWN, ldidCheckClassWN};
    long[] kidsType = {TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR), TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR)};;
    long intrnCall = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_OP.toInt(),
      rtype, BGenDriver.getPrimitiveType(rtype),
      Intrinsic.INTRN_INSTANCEOF.toInt(),
      paramRtype, kidsType, kids);
    return R.newNode(intrnCall);
  }

  private R visitBinopExpr(BinopExpr value) {
    // remder expr, 1 % 2; 1.0 % 3.0
    if(value instanceof JRemExpr) {
      return visitJRemExpr((JRemExpr) value);
    }
    long op1WN = visitRightValue(value.getOp1()).node();
    long op2WN = visitRightValue(value.getOp2()).node();
    // AbstractJimpleIntBinopExpr direct subclasses : JCmpExpr, JCmpgExpr, JCmplExpr, JEqExpr, JGeExpr, JGtExpr, JLeExpr, JLtExpr, JNeExpr
    if (value instanceof AbstractJimpleIntBinopExpr) {
      if(value instanceof JCmpExpr ||
         value instanceof JCmpgExpr ||
         value instanceof JCmplExpr) {
        return visitCmpExpr(value, op1WN, op2WN);
      }
      else {
        return R.newNode(BGenDriver.jniRelational(
          JavaToWhirlConst.getOperator(value.getSymbol()),
          JavaToWhirlConst.getMType(value.getType()), op1WN, op2WN
        ));
      }
    }
    // AbstractJimpleFloatBinopExpr direct subclass : JAddExpr, JDivExpr, JMulExpr, JRemExpr, JSubExpr
    // AbstractJimpleIntLongBinopExpr direct subclass : JAndExpr, JOrExpr, JShlExpr, JShrExpr, JUshrExpr, JXorExpr
    else if (value instanceof AbstractJimpleFloatBinopExpr || value instanceof AbstractJimpleIntLongBinopExpr) {
      if (value instanceof JDivExpr) {
        needWrapByEHRegion = true;
      }
      return R.newNode(BGenDriver.jniCreateBinary(
        JavaToWhirlConst.getOperator(value.getSymbol()),
        JavaToWhirlConst.getMType(value.getType()), op1WN, op2WN
      ));
    } else {
      assertThat(false).as("Not support binary operator: " + value.toString()).isTrue();
    }
    return R.newNone();
  }

  // generate intrinsic op for long/float/dobule compare
  // JCmpExpr for long compare
  // JCmpgExpr/JCmpleExpr for float/double compare
  private R visitCmpExpr(BinopExpr value, long op1WN, long op2WN) {
    Value op1 = value.getOp1();
    Value op2 = value.getOp2();
    assertThat((isSymbolValue(op1) || op1 instanceof Constant)).as("opnd1 should be symbol or const").isTrue();
    assertThat((isSymbolValue(op2) || op2 instanceof Constant)).as("opnd2 should be symbol or const").isTrue();

    Type op1Type = op1.getType();
    Type op2Type = op2.getType();
    int[] paramRType = {JavaToWhirlConst.getMType(op1Type), JavaToWhirlConst.getMType(op2Type)};
    long[] ops = {op1WN, op2WN};
    long[] opsType = {BGenDriver.getPrimitiveType(paramRType[0]), BGenDriver.getPrimitiveType(paramRType[1])};
    int rtype = Mtype.MTYPE_I4.toInt();
    int intrnOP = -1;

    if(value instanceof JCmpExpr) {
      intrnOP = Intrinsic.INTRN_LCMP.toInt();
    } else if (value instanceof JCmpgExpr) {
      if(value.getOp1().getType() instanceof FloatType) {
        intrnOP = Intrinsic.INTRN_FCMPG.toInt();
      } else {
        intrnOP = Intrinsic.INTRN_DCMPG.toInt();
      }
    } else if( value instanceof JCmplExpr) {
      if(value.getOp1().getType() instanceof FloatType) {
      intrnOP = Intrinsic.INTRN_FCMPL.toInt();
      } else {
      intrnOP = Intrinsic.INTRN_DCMPL.toInt();
      }
    }
    return R.newNode(BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_OP.toInt(),
      rtype, BGenDriver.getPrimitiveType(rtype),
      intrnOP,
      paramRType, opsType, ops));
  }

  // mod expr need to handle specifically
  // if operator is not integer, convert to double and call fmod method
  private R visitJRemExpr(JRemExpr value) {
    Value op1 = value.getOp1();
    Value op2 = value.getOp2();
    long op1WN = visitRightValue(op1).node();
    long op2WN = visitRightValue(op2).node();

    // gcj use fmod to mod float and double value
    // double fmod(double, double)
    // we convert float mod to double mod, and convert return value to float
    if (value.getType() instanceof DoubleType || value.getType() instanceof FloatType) {
      long op1CastWN = op1WN, op2CastWN = op2WN;
      Mtype paramMtype = Mtype.MTYPE_F8; // is this correct ?
      if(!(op1.getType() instanceof DoubleType)) {
        op1CastWN = visitTypeConvert(op1WN, Mtype.valueOf(BGenDriver.getRtypeFromWN(op1WN)), paramMtype);
      }
      if(!(op2.getType() instanceof DoubleType)) {
        op2CastWN = visitTypeConvert(op2WN, Mtype.valueOf(BGenDriver.getRtypeFromWN(op2WN)), paramMtype);
      }
      long[] ops = {op1CastWN, op2CastWN};
      int[] paramRtype = {paramMtype.toInt(), paramMtype.toInt()};
      long[] opsType = {BGenDriver.getPrimitiveType(paramRtype[0]), BGenDriver.getPrimitiveType(paramRtype[1])};
      int rMtype = Mtype.MTYPE_F8.toInt();
      long rType = BGenDriver.getPrimitiveType(rMtype);
      if(value.getType() instanceof FloatType) {
        return R.newNode(visitTypeConvert(BGenDriver.createIntrinsic(
          Operator.OPR_INTRINSIC_OP.toInt(), rMtype, rType,
          Intrinsic.INTRN_FMOD.toInt(), paramRtype, opsType, ops), Mtype.MTYPE_F8, Mtype.MTYPE_F4));
      } else {
        return R.newNode(BGenDriver.createIntrinsic(
          Operator.OPR_INTRINSIC_OP.toInt(), rMtype, rType,
          Intrinsic.INTRN_FMOD.toInt(), paramRtype, opsType, ops
        ));
      }
    } else {
      return R.newNode(BGenDriver.jniCreateBinary(
        JavaToWhirlConst.getOperator(value.getSymbol()),
        JavaToWhirlConst.getMType(value.getType()), op1WN, op2WN
      ));
    }
  }

  private long iloadFromCDTable(SootClass clazz, int index) {
    long constDataSymIdx = SymbolHandler.getInternal(clazz, SymbolCategory.GCJ_CONSTANTS_DATA_BASE);
    long ldaWN = BGenDriver.jniLDA(BCRConfig.getPtrType(), 0, constDataSymIdx);
    // dim size NEED REFILL;
    long[] dims  = {
      BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(), SymbolHandler.getClassInfo(currentClass).getConstantSize()),
      BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(), index)
    };
    long arrWN   = BGenDriver.jniArray(ldaWN, dims, TypeHandler.getInternal(GCJType.V_PTR));
    return BGenDriver.jniILoad(BCRConfig.getPtrType(), 0,
      TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR), arrWN, 0);
  }

  private R visitCastExpr(JCastExpr value) {
    Type castToType = value.getCastType();
    Value fromValue = value.getOp();
    R fromWnIdx = visitRightValue(fromValue);
    Type fromType = fromValue.getType();
    if (fromType instanceof PrimType && castToType instanceof PrimType) {
      return R.newNode(visitTypeConvert(fromWnIdx.node(),
              Mtype.valueOf(JavaToWhirlConst.getMType(fromType)),
              Mtype.valueOf(JavaToWhirlConst.getMType(castToType))));
    } else if(fromValue instanceof NullConstant) {
      // CVT from nullConstant to object type, no special CVT needed
      return fromWnIdx;
    } else if(fromType instanceof RefLikeType && castToType instanceof RefLikeType) {
      needWrapByEHRegion = true;
      // Object Reference type are same size(U8), no CVT needed
      assertThat(isSymbolValue(fromValue) || fromValue instanceof Constant).
        as("CastExpr from value is not symbol or constant, from value : " + fromValue).isTrue();
      long leftKid = 0;
      if (castToType instanceof RefType) {
        // FIXME: maybe check over able-to-cast needed!
        SootClass castToClass = ((RefType) castToType).getSootClass();
        leftKid = BGenDriver.jniLDA(BCRConfig.getPtrType(), 0, SymbolHandler.getClassInstanceSymbol(castToClass));
      } else if (castToType instanceof ArrayType) {
        String signature = MangleTool.mangleForSignature(castToType);
        long stringSym   = SymbolInitializer.initStringConst(signature);
        int arrIndex  = SymbolHandler.getClassInfo(currentClass).addConstant(
          SymbolHandler.JvConstant.getConstVal(JvConstantType.JV_CONSTANT_String, stringSym));
        leftKid = iloadFromCDTable(currentClass, arrIndex);
      } else {
        assertThat(false).as("Not support cast to Type, type : " + castToType).isTrue();
      }
      long rightWN = fromWnIdx.node();
      int[]  paramRtype = {BCRConfig.getPtrType(), BCRConfig.getPtrType()};
      long[] kids = {leftKid, rightWN};
      long[] kidsType = {
        TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR),
        TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR)
      };

      long callWN = BGenDriver.createIntrinsic(
        Operator.OPR_INTRINSIC_CALL.toInt(),
        BCRConfig.getPtrType(),
        TypeHandler.getType(castToType),
        Intrinsic.INTRN_CHECK_CAST.toInt(),
        paramRtype,
        kidsType,
        kids);
      return R.newNode(callWN);
    } else {
      assertThat(false).as("not supported cvt from: " + fromType.toString()
        + "to type: " + castToType.toString()).isTrue();
    }
    return R.newNone();
  }

  private R visitJNegExpr(JNegExpr value) {
    Type valueType = value.getType();
    int mtype = JavaToWhirlConst.getMType(valueType);
    Value rsym = value.getOp();
    if(isSymbolValue(rsym)) {
      long symbolIdx = visit(rsym).sym();
      if (valueType instanceof PrimType) {
        long offset = 0;
        // if mtype if not the type of st, may need conversion
        int originType = BGenDriver.getMTypeFromTypeIdx(BGenDriver.getTyFromST(symbolIdx));
        // check if origin & mtype are both valid for negation
        assertThat(isFloatingType(Mtype.valueOf(originType)) || isIntegral(Mtype.valueOf(originType)))
                .as("negation: originType should be numeric(not float/int present)").isTrue();
        assertThat(isFloatingType(Mtype.valueOf(mtype)) || isIntegral(Mtype.valueOf(mtype)))
                .as("negation: targetType should be numeric(not float/int present)").isTrue();
        long ldidwn = BGenDriver.jniLDID(originType, offset, symbolIdx, BGenDriver.getTyFromST(symbolIdx));
        long rwn = visitTypeConvert(ldidwn, Mtype.valueOf(originType), Mtype.valueOf(mtype));
        return R.newNode(BGenDriver.jniCreateUnary(JavaToWhirlConst.getOperator("neg"), mtype, rwn));
      } else {
        assertThat(false).as("not supported type").isTrue();
      }
    } else if(rsym instanceof NumericConstant) {
      long ldconst = visit(rsym).node();
      return R.newNode(BGenDriver.jniCreateUnary(JavaToWhirlConst.getOperator("neg"), mtype, ldconst));
    } else {
      assertThat(false).as("right side should be a symbol or constant").isTrue();
    }
    return R.newNone();
  }

  /**
   * ILOAD <field_id:4>
   */
  private R visitJLengthExpr(JLengthExpr value) {
    Value arrValue = value.getOp();
    Type  arrType  = arrValue.getType();
    assertThat(arrType instanceof ArrayType || arrType instanceof NullType).
      as("JLengthExpr expression type:[" + arrValue.getType().toString() +"]should be refType").isTrue();
    if(arrType instanceof NullType) {
      return R.newNode(BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(), 0));
    } else {
      long stIdx = visit(arrValue).sym();
      long tyIdx = BGenDriver.getTyFromST(stIdx);
      long ldid = BGenDriver.jniLDID(BCRConfig.getPtrType(), 0, stIdx, tyIdx);
      return R.newNode(BGenDriver.jniILoad(Mtype.MTYPE_I4.toInt(), 0,
        BGenDriver.getPointeeTyFromTY(tyIdx), ldid, 4));
    }
  }

  private boolean isPolymorphic(SootMethodRef methodRef) {
    String[] polymorphicClass = {"java.lang.invoke.MethodHandle", "java.lang.invoke.VarHandle"};
    try{
      boolean find = false;
      String clazzName = methodRef.declaringClass().getName();
      for (int i = 0; i < polymorphicClass.length; i++) {
        if (clazzName.equals(polymorphicClass[i])) {
          find = true;
        }
      }
      if (!find) {
        return false;
      }
      if(methodRef.parameterTypes().size() == 1 && methodRef.parameterTypes().get(0) instanceof ArrayType
        && ((ArrayType) methodRef.parameterTypes().get(0)).getElementType() instanceof RefType
        && ((RefType) ((ArrayType) methodRef.parameterTypes().get(0)).getElementType()).getClassName().equals("java.lang.Object")){
        return true;
      }
    } catch (Exception e) {
      e.printStackTrace();
    }
    return false;
  }

  private AbstractInvokeExpr optimizeInvokeExpr(AbstractInvokeExpr invoke) {
    if (invoke instanceof JVirtualInvokeExpr) {
      JVirtualInvokeExpr virtualInvoke = (JVirtualInvokeExpr) invoke;
      try {
        SootMethod method = virtualInvoke.getMethod();
        if (method.isFinal() || method.isPrivate()) {
          return new JSpecialInvokeExpr((JimpleLocal) virtualInvoke.getBase(), method.makeRef(), virtualInvoke.getArgs());
        }
        // Polymorphic function
        if (isPolymorphic(virtualInvoke.getMethodRef())) {
          SootMethodRef methodRef = invoke.getMethodRef();
          List<SootMethod> methods = methodRef.declaringClass().getMethods();
          for (SootMethod m : methods) {
            if ((m.getModifiers() & AccFlags.ACC_VARARGS) != 0 && m.getName().equals(methodRef.name())) {
              return new JSpecialInvokeExpr((JimpleLocal) virtualInvoke.getBase(), m.makeRef(), virtualInvoke.getArgs());
            }
          }
          assertThat(false).isTrue().as("Can't find polymorphic method def, method ref : " + invoke.getMethodRef());
        }
      } catch (ResolutionFailedException e) {
        // Bailing
        return invoke;
      }
    }
    return invoke;
  }

  /**
   * Virtual Invoke:
   * first kid is this pointer
   *   1 ~ n-1 kid is original parameter
   *   n kid is vtable offset load
   *   for example:
   *       LDID this
   *     PARM
   *       ...
   *     PARM
   *         LDID this
   *       ILOAD vptr field
   *     ILOAD method offset
   *   ICALL
   *
   * Static Invoke: direct call
   *
   * Special Invoke: for invoke constructor
   *
   * Interface Invoke:
   * first kid is this pointer, 1 ~ n -1 kid is original parameter n kid is _Jv_LookupInterfaceMethodIdx call
   *   for example:
   *      LDID this
   *    PARM
   *      ...
   *    PARM
   *              LDID this
   *            ILOAD super object field
   *          ILOAD class sym field
   *        PARM
   *          LDA interface class sym
   *        PARM
   *          INTCONST method table offset
   *        PARM
   *      CALL _Jv_LookupInterfaceMethodIdx
   *      LDID return preg
   *     COMMA
   *   ICALL
   *
   * @param value
   * @return
   */
  private R visitAbstractInvokeExpr(AbstractInvokeExpr value) {
    value = optimizeInvokeExpr(value);
    // Change to REF
    SootMethodRef methodRef = value.getMethodRef();
    needWrapByEHRegion = true;
    long symbolIdx = 0;
    long callWN = 0;
    int kidIndex = 0;
    if(value instanceof AbstractInstanceInvokeExpr) {
      symbolIdx = SymbolHandler.getSymbol(methodRef);
      if(value instanceof SpecialInvokeExpr) {
        // generate direct call for special invoke
        callWN = BGenDriver.jniCall(
          JavaToWhirlConst.getMType(methodRef.returnType()),
          Mtype.MTYPE_V.toInt(), methodRef.parameterTypes().size() + 1, symbolIdx
        );
      } else {
        callWN = BGenDriver.funcICall(
          JavaToWhirlConst.getMType(methodRef.returnType()), Mtype.MTYPE_V.toInt(),
          methodRef.parameterTypes().size() + 2, TypeHandler.getType(methodRef)
        );
      }
      AbstractInstanceInvokeExpr vInvoke = (AbstractInstanceInvokeExpr) value;
      assertThat(vInvoke.getBase() instanceof JimpleLocal).as(
        "Invoke base is not JimpleLocal, base : " + vInvoke.getBase().toString()).isTrue();
      long thisSymIdx = SymbolHandler.getSymbol((JimpleLocal) vInvoke.getBase());
      long ldidThisWN = BGenDriver.jniLDID(BCRConfig.getPtrType(), 0,
        thisSymIdx, BGenDriver.getTyFromST(thisSymIdx));
      long parmLdidThisWN = BGenDriver.jniParm(
        BCRConfig.getPtrType(), ldidThisWN,
        BGenDriver.getTyFromST(thisSymIdx), Flag.WN_PARM_BY_VALUE.toInt()
      );
      BGenDriver.jniSetWNKid(callWN, kidIndex++, parmLdidThisWN);
    } else if(value instanceof JStaticInvokeExpr) {
      symbolIdx = SymbolHandler.getSymbol(methodRef);
      callWN = BGenDriver.jniCall(
        JavaToWhirlConst.getMType(methodRef.returnType()),
        Mtype.MTYPE_V.toInt(), methodRef.parameterTypes().size(), symbolIdx
      );
    } else if(value instanceof JDynamicInvokeExpr) {
      SootClass lambdaClass = LambdaClassCreator.createLambdaClass((JDynamicInvokeExpr) value, currentClass);
      TypeHandler.getType(lambdaClass, true);
      handleClassList.add(lambdaClass);
      SootMethod getLambdaMethod = lambdaClass.getMethodByName(LambdaClassCreator.getLambdaMethodName);
      symbolIdx = SymbolHandler.getSymbol(getLambdaMethod);
      callWN = BGenDriver.jniCall(
        JavaToWhirlConst.getMType(getLambdaMethod.getReturnType()),
        Mtype.MTYPE_V.toInt(), methodRef.parameterTypes().size(), symbolIdx
      );
    } else {
      assertThat(false).as("Not support invoke, expr : " + value.toString()).isTrue();
    }
    for (Value arg: value.getArgs()) {
      long argWN = visitRightValue(arg).node();
      long parmKidWN = argWN;
      int parmMType = JavaToWhirlConst.getMType(arg.getType());
      // WHIRL PARM can't receive BOOL I1 I2 type, so should convert I1 and I2 type to I4
      // boolean type case : application/jfe/extendj/run/conditional_01-Test
      if(arg.getType() instanceof BooleanType || arg.getType() instanceof ByteType || arg.getType() instanceof CharType || arg.getType() instanceof ShortType) {
        parmKidWN = BGenDriver.typeConversion(argWN, (byte) Mtype.MTYPE_I4.toInt());
        parmMType = Mtype.MTYPE_I4.toInt();
      }
      // primitive type use by value, RefType is same as pointer, also use by value
      int flag = Flag.WN_PARM_BY_VALUE.toInt();
      long parmWN = BGenDriver.jniParm(
        parmMType, parmKidWN, TypeHandler.getType(arg.getType()), flag
      );
      BGenDriver.jniSetWNKid(callWN, kidIndex++, parmWN);
    }
    requestLineNum(callWN);
    // TODO: private method should do more things
    if(value instanceof JVirtualInvokeExpr) {
      long extraParaWN = prepareExtraParaForVirtualInvoke((JVirtualInvokeExpr) value);
      BGenDriver.jniSetWNKid(callWN, kidIndex++, extraParaWN);
      BGenDriver.jniSetCallFlag(callWN, CallFlags.WN_CALL_IS_VIRTUAL.toInt());
    } else if(value instanceof JInterfaceInvokeExpr) {
      long extraParaWN = prepareExtraParaForInterfaceInvoke((JInterfaceInvokeExpr) value);
      BGenDriver.jniSetWNKid(callWN, kidIndex++, extraParaWN);
      BGenDriver.jniSetCallFlag(callWN, CallFlags.WN_CALL_IS_INTERFACE.toInt());
    }
    if (methodRef.returnType() instanceof VoidType) {
      return R.newNode(callWN);
    } else {
      Type retType = methodRef.returnType();
      long blockWN = BGenDriver.jniCreateBlock();
      BGenDriver.jniSetLineNum(callWN, currSrcPos);
      BGenDriver.jniInsertBlockLast(blockWN, callWN);
      long regIdx = BGenDriver.getReturnValPreg();
      long ldidWN = BGenDriver.jniLDID(
        JavaToWhirlConst.getMType(retType), -1, regIdx, TypeHandler.getType(retType));
      requestLineNum(blockWN);
      return R.newNode(BGenDriver.jniComma(
        BGenDriver.getRtypeFromWN(callWN), Mtype.MTYPE_V.toInt(), blockWN, ldidWN));
    }
  }

  private long prepareExtraParaForVirtualInvoke(JVirtualInvokeExpr expr) {
    assertThat(expr.getBase() instanceof JimpleLocal).as(
      "Invoke base is not JimpleLocal, base : " + expr.getBase().toString()).isTrue();
    SootMethodRef methodRef = expr.getMethodRef();
    if (methodRef.tryResolve() == null) {
      assertThat(false).as("Methods (including polymorphic signatures) should be optimized and " +
              "signatures should be optimized, yet given" + methodRef.getSubSignature()).isTrue();
    }
    if(methodRef.resolve().isPrivate()) { // !!! might be resolving failure !!!
      // LDA method symbol
      return BGenDriver.jniLDA(BCRConfig.getPtrType(), 0, SymbolHandler.getSymbol(methodRef));
    } else {
      /*
            LDID this
           ILOAD vtable field
          ILOAD method filed
      */
      long pointerThisSymIdx = SymbolHandler.getSymbol((JimpleLocal) expr.getBase());
      long ldidThisWN = BGenDriver.jniLDID(BCRConfig.getPtrType(), 0,
        pointerThisSymIdx, BGenDriver.getTyFromST(pointerThisSymIdx));
      SootMethod method = methodRef.resolve();
      long gcjCompatibleResult;
      if(expr.getBase().getType() instanceof NullType) {
        // for null.fun()
        // there is no vtable for null object, use
        //  LDID this
        // ILOAD  as the fake vtable
        gcjCompatibleResult = BGenDriver.jniILoad(BCRConfig.getPtrType(),
          SymbolHandler.getClassInfo(methodRef.declaringClass()).getVTableOffset(method),
          TypeHandler.getType(methodRef), ldidThisWN, 0);
      } else {
        long iloadVTableWN = BGenDriver.jniILoad(BCRConfig.getPtrType(), 0,
          BGenDriver.getPointeeTyFromTY(BGenDriver.getTyFromST(pointerThisSymIdx)), ldidThisWN, 1);
        int vtableOffset = SymbolHandler.getClassInfo(methodRef.declaringClass()).getVTableOffset(method);
        long methodPtr = BGenDriver.jniMakePointerType(TypeHandler.getType(methodRef));
        gcjCompatibleResult = BGenDriver.jniILoad(BCRConfig.getPtrType(), vtableOffset * BCRConfig.getWordBytes(),
          methodPtr, iloadVTableWN, 0);
      }
      if (B2WFrontEnd.i().isUseDevirtIndicator()) {
        return prepareDevirtIndicatorWN(gcjCompatibleResult, method);
      } else {
        // For GCJ Compatibility
        return gcjCompatibleResult;
      }
    }
  }

  private long prepareExtraParaForInterfaceInvoke(JInterfaceInvokeExpr expr) {
    SootClass ownerClass = expr.getMethod().getDeclaringClass();
    assertThat(expr.getBase() instanceof JimpleLocal).as(
      "Invoke base is not JimpleLocal, base : " + expr.getBase().toString()).isTrue();
    int ptrMtype= BCRConfig.getPtrType();
    long pointerThisSymIdx = SymbolHandler.getSymbol((JimpleLocal) expr.getBase());
    /*
            LDID this
           ILOAD super object field
          ILOAD this class symbol
         PARM
     */
    long ldidThisWN = BGenDriver.jniLDID(ptrMtype, 0,
      pointerThisSymIdx, BGenDriver.getTyFromST(pointerThisSymIdx));
    long iloadObjFieldWN = BGenDriver.jniILoad(
      ptrMtype, 0,
      TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT), ldidThisWN, 1
    );
    long iloadVTableFieldWN = BGenDriver.jniILoad(
      ptrMtype, 0,
      TypeHandler.getInternal(GCJType.GCJ_VTABLE), iloadObjFieldWN, 1
    );

    // LDA interface class symbol
    long clazzSymLdaWN = BGenDriver.jniLDA(
      ptrMtype, 0, SymbolHandler.getClassInstanceSymbol(ownerClass)
    );


    // INTCONST method table offset
    SymbolHandler.ClassInfo clazzInfo = SymbolHandler.getClassInfo(ownerClass);
    long methodTableOffsetConstWN = BGenDriver.jniIntConst(
      Mtype.MTYPE_I4.toInt(), clazzInfo.getMethodTableOffset(expr.getMethod())
    );
    int offMtype = Mtype.MTYPE_I4.toInt();

    int paramRtype[] = {ptrMtype, ptrMtype, offMtype} ;
    long paramWN[] = {iloadVTableFieldWN, clazzSymLdaWN, methodTableOffsetConstWN };
    long paramType[] = {
      TypeHandler.getInternal(GCJType.GCJ_VTABLE_PTR),
      TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR),
      BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt())
    };
    long lookupCallWN = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      ptrMtype,
      BGenDriver.getPrimitiveType(ptrMtype),
      Intrinsic.INTRN_LOOKUP_IF.toInt(),
      paramRtype,
      paramType,
      paramWN
      );

    if (B2WFrontEnd.i().isUseDevirtIndicator()) {
      return prepareDevirtIndicatorWN(lookupCallWN, expr.getMethod());
    } else {
      // For GCJ Compatibility
      // FIXME: needWrapByEHRegion = true;
      return lookupCallWN;
    }
  }


  /***
   * Prepare the devirt indicator call, used only for devirt purposes,
   * should be removed in whirl to assembly
   *      U8U8LDID this
   *     U8U8ILOAD super       /  ...
   *    U8U8ILOAD class_symbol / U8INTRINSIC_CALL LOOKUP_IF
   *   U8PARM (Original Value)
   *    U8LDA Function Name Constant
   *   U8PARM (Function Name Indicator)
   *  U8INTRINSIC_CALL
   * @param gcjCompatibleResult the original GCJ conforming LDID/ILOAD WN.
   * @param method Method
   */
  private long prepareDevirtIndicatorWN(long gcjCompatibleResult, SootMethod method) {
    String mangledName = MangleTool.mangle(method);
    long mangledFunctionNameConstant = SymbolInitializer.initStringConst(mangledName);
    int PTR_MTYPE = BCRConfig.getPtrType();
    // The returned pointer is the called function
    long returnTypeIdx = TypeHandler.getType(method);
    int returnMType = PTR_MTYPE;
    int paramRtype[] = {returnMType, PTR_MTYPE};
    long paramType[] = {returnTypeIdx,
                        TypeHandler.getInternal(GCJType.V_PTR)};
    long ldaStringConstant = BGenDriver.jniLDA(PTR_MTYPE, 0, mangledFunctionNameConstant);
    long paramWN[] = {gcjCompatibleResult, ldaStringConstant};
    long devirtIndicatorCallWN = BGenDriver.createIntrinsic(
            Operator.OPR_INTRINSIC_OP.toInt(),
            returnMType,
            returnTypeIdx,
            Intrinsic.INTRN_LOOKUP_VIRT_FUNC.toInt(),
            paramRtype,
            paramType,
            paramWN
    );
    return devirtIndicatorCallWN;
  }

  static void requestLineNum(long ... node) {
    for(long one : node) {
      pendingWn.push(one);
    }
  }

  private R visitJNewExpr(JNewExpr value) {
    needWrapByEHRegion = true;
    Type type = value.getType();
    assertThat(type instanceof RefType).as( "The type of new expression must be RefType").isTrue();
    SootClass sootClass = ((RefType) type).getSootClass();
    long classSymIdx = SymbolHandler.getClassInstanceSymbol(sootClass);
    long lda = BGenDriver.jniLDA(BCRConfig.getPtrType(), 0, classSymIdx);
    int[] kidsMtype = {BCRConfig.getPtrType()};
    long[] kids = {lda};
    long[] kidsType = {TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR)};
    long callStmt = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      BCRConfig.getPtrType(),
      TypeHandler.getType(type),
      Intrinsic.INTRN_ALLOC_OBJ.toInt(),
      kidsMtype, kidsType, kids
    );
    return R.newNode(callStmt);
  }

  private R visitJNewArrayExpr(JNewArrayExpr value) {
    Type type = value.getType();
    Value sizeVal = value.getSizeBox().getValue();
    R sizeRet = visitRightValue(sizeVal);
    long sizeWn;
    // convert size to U4 type
    sizeWn = visitTypeConvert(sizeRet.node(), sizeVal.getType(), Mtype.MTYPE_U4);
    return newPrimArray(sizeWn, type);
  }

  private long visitTypeConvert(long node, Mtype originType, Mtype targetType) {
    if(originType == targetType)
      return node;

    /*** Widening & Narrowing ***/
    if (originType == Mtype.MTYPE_I1 && targetType == Mtype.MTYPE_U2)
      return BGenDriver.typeConversion(BGenDriver.typeConversion(node, (byte) Mtype.MTYPE_I4.toInt()), (byte) Mtype.MTYPE_U2.toInt());

    /*** Narrowing ***/
    if (originType == Mtype.MTYPE_F8 && targetType == Mtype.MTYPE_F4) {
      return BGenDriver.typeConversion(node, (byte) targetType.toInt());
    }

    if (isSignedIntegral(originType) && isNarrowingSignedIntegral(originType, targetType))
      return BGenDriver.typeConversion(node, (byte) targetType.toInt());

    if (originType == Mtype.MTYPE_U2 && (targetType == Mtype.MTYPE_I1 || targetType == Mtype.MTYPE_I2)){
      byte firstTarget = (byte) ((targetType == Mtype.MTYPE_I1) ? Mtype.MTYPE_U1.toInt() : Mtype.MTYPE_U2.toInt());
      byte secondTarget = (byte) targetType.toInt();
      return BGenDriver.typeConversion(BGenDriver.typeConversion(node, firstTarget), secondTarget);
    }

    // Narrow.1 float -> Integrals (incl. char)
    if(isFloatingType(originType) && isIntegral(targetType)) {
      Mtype firstType = targetType == Mtype.MTYPE_I8 ? Mtype.MTYPE_I8 : Mtype.MTYPE_I4; // FIXME: Mtype.MTYPE_I4
      long firstUpperSelect = visitFloatToIntegralCast(node, originType, firstType);
      return visitTypeConvert(firstUpperSelect, firstType, targetType);
    }

    /***  Widening  ***/
    if(isSignedIntegral(originType) || originType == Mtype.MTYPE_U2)
      return BGenDriver.typeConversion(node, (byte) targetType.toInt());
    if(originType == Mtype.MTYPE_F4 && targetType == Mtype.MTYPE_F8)
      return BGenDriver.typeConversion(node, (byte) targetType.toInt());
    assertThat(false).as("Cannot convert " + originType + " to " + targetType).isTrue();
    return BGenDriver.typeConversion(node, (byte) targetType.toInt());
  }

  private long visitFloatToIntegralCast(long node, Mtype originType, Mtype firstType) {
    assertThat(originType == Mtype.MTYPE_F8 || originType == Mtype.MTYPE_F4).as("origin type should be F4 or F8").isTrue();
    long longMaxVal = 0, longMinVal = 0, longMaxRetVal = 0, longMinRetVal = 0;
    switch (firstType){
      case MTYPE_I4:
        longMaxRetVal = BGenDriver.jniIntConst(firstType.toInt(), Integer.MAX_VALUE);
        longMinRetVal = BGenDriver.jniIntConst(firstType.toInt(), Integer.MIN_VALUE);
        longMaxVal = BGenDriver.jniFloatConst(originType.toInt(), Integer.MAX_VALUE);
        longMinVal = BGenDriver.jniFloatConst(originType.toInt(), Integer.MIN_VALUE);
        break;
      case MTYPE_I2:
        longMaxRetVal = BGenDriver.jniIntConst(firstType.toInt(), Short.MAX_VALUE);
        longMinRetVal = BGenDriver.jniIntConst(firstType.toInt(), Short.MIN_VALUE);
        longMaxVal = BGenDriver.jniFloatConst(originType.toInt(), Short.MAX_VALUE);
        longMinVal = BGenDriver.jniFloatConst(originType.toInt(), Short.MIN_VALUE);
        break;
      case MTYPE_I1:
        longMaxRetVal = BGenDriver.jniIntConst(firstType.toInt(), Byte.MAX_VALUE);
        longMinRetVal = BGenDriver.jniIntConst(firstType.toInt(), Byte.MIN_VALUE);
        longMaxVal = BGenDriver.jniFloatConst(originType.toInt(), Byte.MAX_VALUE);
        longMinVal = BGenDriver.jniFloatConst(originType.toInt(), Byte.MIN_VALUE);
        break;
      case MTYPE_U2:
        longMaxRetVal = BGenDriver.jniIntConst(firstType.toInt(), Character.MAX_VALUE);
        longMinRetVal = BGenDriver.jniIntConst(firstType.toInt(), Character.MIN_VALUE);
        longMaxVal = BGenDriver.jniFloatConst(originType.toInt(), Character.MAX_VALUE);
        longMinVal = BGenDriver.jniFloatConst(originType.toInt(), Character.MIN_VALUE);
        break;
      case MTYPE_I8:
        longMaxRetVal = BGenDriver.jniIntConst(firstType.toInt(), Long.MAX_VALUE);
        longMinRetVal = BGenDriver.jniIntConst(firstType.toInt(), Long.MIN_VALUE);
        longMaxVal = BGenDriver.jniFloatConst(originType.toInt(), Long.MAX_VALUE);
        longMinVal = BGenDriver.jniFloatConst(originType.toInt(), Long.MIN_VALUE);
        break;
      default:
        assertThat(false).as("target mtype must be i1-i8 or u2").isTrue();
        return 0;
    }
    int[] opsMtypes = new int[]{originType.toInt(), originType.toInt()};
    long[] ops = new long[]{node, longMaxVal};
    long[] opsType = {BGenDriver.getPrimitiveType(opsMtypes[0]), BGenDriver.getPrimitiveType(opsMtypes[1])};
    int rMtype = Mtype.MTYPE_I4.toInt();
    long rType = BGenDriver.getPrimitiveType(rMtype);
    long firstUpperCheck  = BGenDriver.createIntrinsic(Operator.OPR_INTRINSIC_OP.toInt(),
            rMtype, rType, Intrinsic.INTRN_FCMPL.toInt(), opsMtypes, opsType, ops);
    ops = new long[] {node, longMinVal};
    long firstBottomCheck = BGenDriver.createIntrinsic(Operator.OPR_INTRINSIC_OP.toInt(),
            rMtype, rType, Intrinsic.INTRN_FCMPG.toInt(), opsMtypes, opsType, ops);

    long directResult = BGenDriver.typeConversion(node, (byte) firstType.toInt());
    long bottomCheckGreaterThanOne = BGenDriver.jniRelational(Operator.OPR_GE.toInt(),
            Mtype.MTYPE_I4.toInt(), firstBottomCheck, BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(),1));
    long upperCheckLessThanOne = BGenDriver.jniRelational(Operator.OPR_LT.toInt(),
            Mtype.MTYPE_I4.toInt(), firstUpperCheck, BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(),1));

    //long block = BGenDriver.jniCreateBlock();
    //long pregSym = BGenDriver.createPregSymbol(originType.toInt());
    //long resPreg = BGenDriver.createPreg(originType.toInt(), "temp_comma_preg");
    //long pregStType = BGenDriver.getTyFromST(pregSym);
    //long stid = BGenDriver.jniSTID(originType.toInt(), resPreg, pregSym, pregStType, BGenDriver.jniFloatConst(originType.toInt(), Double.NaN));
    //long ldid = BGenDriver.jniLDID(originType.toInt(), resPreg, pregSym, pregStType);
    //BGenDriver.jniInsertBlockLast(block, stid);

    //long comma = BGenDriver.jniComma(originType.toInt(), Mtype.MTYPE_V.toInt(), block, ldid);
    long checkNan = BGenDriver.jniRelational(Operator.OPR_EQ.toInt(), originType.toInt(), node, node); //BGenDriver.jniFloatConst(originType.toInt(), Double.NaN)); //comma

    long firstBottomSelect = BGenDriver.jniCreateTernary(Operator.OPR_CSELECT.toInt(), firstType.toInt(), bottomCheckGreaterThanOne, directResult, longMinRetVal);
    long inRangeResult = BGenDriver.jniCreateTernary(Operator.OPR_CSELECT.toInt(), firstType.toInt(), upperCheckLessThanOne, firstBottomSelect, longMaxRetVal);
    long nonNaNResult = BGenDriver.jniCreateTernary(Operator.OPR_CSELECT.toInt(), firstType.toInt(), checkNan, inRangeResult, BGenDriver.jniIntConst(firstType.toInt(), 0));
    return nonNaNResult;
  }

  private boolean isNarrowingSignedIntegral(Mtype originType, Mtype targetType) {
    return originType.toInt() > targetType.toInt();
  }

  private boolean isIntegral(Mtype targetType) {
    return targetType == Mtype.MTYPE_U2 || isSignedIntegral(targetType);
  }

  private static boolean isSignedIntegral(Mtype origin) {
    return origin == Mtype.MTYPE_I1 || origin == Mtype.MTYPE_I4 || origin == Mtype.MTYPE_I2 || origin == Mtype.MTYPE_I8;
  }

  private boolean isFloatingType(Mtype originType) {
    return originType == Mtype.MTYPE_F4 || Mtype.MTYPE_F8 == originType;
  }

  private long visitTypeConvert(long node, Type originalType, Mtype targetType) {
    return visitTypeConvert(node, Mtype.valueOf(JavaToWhirlConst.getMType(originalType)), targetType);
  }

  /**
   * MultiArray (1. int[8][], 2. int[8][4] 3.int[][])
   * @param value
   * @return
   */
  private R visitJNewMultiArrayExpr(JNewMultiArrayExpr value) {
    assertThat(value.getType() instanceof ArrayType).as("newMultiArray type should be ArrayType, type : " +
      value.getType()).isTrue();
    needWrapByEHRegion = true;
    ArrayType arrType = (ArrayType) value.getType();
    List<Value> arrSize = value.getSizes();
    /*
              U8LDA 0 CD_THIS_CLASS
              I4INTCONST dim
              I4INTCONST index
            U8ARRAY
          U8U8IOAD 0
        U8STID
     */
    String signature = MangleTool.mangleSign(arrType.baseType, arrSize.size());
    long stringSym   = SymbolInitializer.initStringConst(signature);
    int arrIndex  = SymbolHandler.getClassInfo(currentClass).addConstant(
      SymbolHandler.JvConstant.getConstVal(JvConstantType.JV_CONSTANT_String, stringSym));
    long iloadWN = iloadFromCDTable(currentClass, arrIndex);
    long tmpVarTypeIdx = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
    long tmpVarSymIdx = SymbolHandler.getTempSymbol(tmpVarTypeIdx);
    long stidWN = BGenDriver.jniSTID(BCRConfig.getPtrType(), 0, tmpVarSymIdx, tmpVarTypeIdx, iloadWN);
    /*
          BLOCK
              U8U8LDID 0 class_ptr_var
            U8PARM
              I4INTCONST dims
            I4PARM
              I4INTCONST _1st_dim_length
            I4PARM
              I4INTCONST _2ed_dim_length
            I4PARM
            ...
          U8CALL _Jv_NewMultiArray
          END_BLOCK
          U8U8LDID .preg_return_val
        U8COMMA
     */
    //long methodSymIdx = SymbolHandler.getJvMethodSym(JvMethod._Jv_NewMultiArray);
    int[] kidsMtype = new int[arrSize.size() + 2];
    long[] kids = new long[arrSize.size() + 2];
    long[] kidsType = new long[arrSize.size() + 2];
    int kidIdx = 0;
    long I4Type = BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt());
    long ldidWN = BGenDriver.jniLDID(BCRConfig.getPtrType(), 0, tmpVarSymIdx, tmpVarTypeIdx);
    kids[kidIdx] = ldidWN;
    kidsMtype[kidIdx] = BCRConfig.getPtrType();
    kidsType[kidIdx]  = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
    kidIdx++;
    kids[kidIdx] = BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(), arrSize.size());
    kidsMtype[kidIdx] = Mtype.MTYPE_I4.toInt();
    kidsType[kidIdx]  = I4Type;
    kidIdx++;
    for (Value sizeVal : arrSize) {
      assertThat(sizeVal instanceof JimpleLocal || sizeVal instanceof IntConstant).
        as("Array size is not var or int const, size : " + sizeVal).isTrue();
      R sizeWN = visitRightValue(sizeVal);
      kids[kidIdx] = sizeWN.node();
      kidsMtype[kidIdx] = Mtype.MTYPE_I4.toInt();
      kidsType[kidIdx]  = I4Type;
      kidIdx++;
    }
    long intrnCall = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      BCRConfig.getPtrType(),
      TypeHandler.getType(arrType),
      Intrinsic.INTRN_NEW_MULTI_ARR.toInt(),
      kidsMtype,
      kidsType,
      kids);
    R r = R.newNode(intrnCall);
    r.insertToBefore(stidWN);
    return r;
  }

  // below are complex expression
  // new prim array, return a comma
  private R newPrimArray(long sizeWn, Type type) {
    assertThat(type instanceof ArrayType).as("newMultiArray type should be ArrayType but not" + type.toString()).isTrue();
    needWrapByEHRegion = true;
    Type elem_ty = ((ArrayType)type).baseType;

    long[] parms;
    int[] paramMType;
    long[] paramType;
    long I4Type = BGenDriver.getPrimitiveType(Mtype.MTYPE_I4.toInt());
    long classPtr = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
    int ptrType = BCRConfig.getPtrType();
    long classSym = 0;
    int iopc = 0;
    if (elem_ty instanceof RefType) {
      classSym = SymbolHandler.getClassInstanceSymbol(((RefType) elem_ty).getSootClass());
      long ldaWn = BGenDriver.jniLDA(ptrType, (long) 0, classSym);
      long objptr = TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT);
      paramMType = new int[3];
      parms = new long[3];
      paramType = new long [3];
      paramMType[0] = BGenDriver.getRtypeFromWN(sizeWn);
      paramMType[1] = ptrType;
      paramMType[2] = ptrType;
      parms[0] = sizeWn;
      parms[1] = ldaWn;
      parms[2] = BGenDriver.jniIntConst(ptrType, 0);
      paramType[0] = I4Type;
      paramType[1] = classPtr;
      paramType[2] = classPtr;
      iopc = Intrinsic.INTRN_NEW_OBJ_ARR.toInt();
    } else if (elem_ty instanceof PrimType) {
      classSym = SymbolHandler.getPrimitiveClassSymbol(elem_ty);
      long ldaWn = BGenDriver.jniLDA(ptrType, (long) 0, classSym);
      paramMType = new int[2];
      parms = new long[2];
      paramType = new long[2];
      paramMType[0] = ptrType;
      paramMType[1] = BGenDriver.getRtypeFromWN(sizeWn);
      parms[0] = ldaWn;
      parms[1] = sizeWn;
      paramType[0] = classPtr;
      paramType[1] = I4Type;
      iopc = Intrinsic.INTRN_NEW_PRIM_ARR.toInt();
    } else {
      parms = new long[0];
      paramMType = new int[0];
      paramType = new long[0];
      assertThat(false).as("Array of " + elem_ty + " is not supported.").isTrue();
    }
    long arrPtrType = TypeHandler.getType(type);
    long newPrimArrayCallWn = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      ptrType, arrPtrType, iopc, paramMType, paramType, parms);

    BGenDriver.setArbUbd(arrPtrType, sizeWn);
    return R.newNode(newPrimArrayCallWn);
  }

  // TODO: need to generate more exception error, reference to open64java
  private R visitJArrayRef(JArrayRef value) {
    // base
    // what if array of array
    JimpleLocal local = (JimpleLocal) value.getBaseBox().getValue();
    long arraySymIdx = visit(local).sym();
    Type type = value.getBase().getType();
    assertThat(type instanceof ArrayType).as("Array sym should have array type!").isTrue();
    int offsetToData = 12;
    R index = visitRightValue(value.getIndex());
    // kids TODO: may need to set correct array size. Just set it zero as in openjava, WHY I4?
    long[] dims = {BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(), (long) 0), index.node()};
    long elemType = 0;
    assertThat(type instanceof ArrayType).as("type should be array for arrayRef").isTrue();
    if (((ArrayType)type).getElementType() instanceof RefType) {
      elemType = BGenDriver.getPrimitiveType(BCRConfig.getPtrType());
    } else if(((ArrayType) type).getElementType() instanceof PrimType) {
      elemType = TypeHandler.getTypeForArrayUse(TypeHandler.getBottomType((ArrayType) (type)));
    } else if(((ArrayType) type).getElementType() instanceof ArrayType) {
      // TODO: is there a need for a seperate temporary var ?
      //long symIdx = SymbolHandler.getTempSymbol(BGenDriver.getPrimitiveType(BCRConfig.getPtrType()));
      //retVal = R.newNode(BGenDriver.jniSTID(Mtype.MTYPE_U8.toInt(), 0, symIdx,
      //        BGenDriver.getPrimitiveType(BCRConfig.getPtrType()), BGenDriver.jniArray(arrAddr, dims, elemType)));
      // R recurse = visitArraySym(symIdx, value, ((ArrayType) type).getElementType());
      //retVal.values.addAll(recurse.values);
      elemType = BGenDriver.getPrimitiveType(BCRConfig.getPtrType());
    } else {
      assertThat(elemType).as("not supporting type of " + ((ArrayType) type).getElementType()).isNotZero();
    }
    if (BGenDriver.getTySize(elemType) >= BCRConfig.getWordBytes() && offsetToData % BCRConfig.getWordBytes() != 0) { //alignment is 16 (alignment)
      offsetToData = offsetToData + BCRConfig.getWordBytes() - (offsetToData % BCRConfig.getWordBytes());
    }
    long arrBase = BGenDriver.jniLDID(BCRConfig.getPtrType(), (long) 0, arraySymIdx, BGenDriver.getTyFromST(arraySymIdx));
    long offset  = BGenDriver.jniIntConst(Mtype.MTYPE_I4.toInt(), offsetToData);
    long arrAddr = BGenDriver.jniCreateBinary(Operator.OPR_ADD.toInt(), BCRConfig.getPtrType(), arrBase, offset);
    return R.newNode(BGenDriver.jniArray(arrAddr, dims, elemType));
  }

  private R visitMonitorStmt(MonitorStmt monitorStmt) {
    //FIXME: Maybe need to be wrapped in an EH region
    Value monitorValue = monitorStmt.getOp();
    assertThat(isSymbolValue(monitorValue) ||
      monitorValue instanceof StringConstant ||
      monitorValue instanceof ClassConstant ||
      monitorValue instanceof NullConstant).as(
      "monitor value should be a symbol or string constant").isTrue();

    int rtype = Mtype.MTYPE_V.toInt();
    int intrnOp = 0;
    if(monitorStmt instanceof JEnterMonitorStmt) {
      intrnOp = Intrinsic.INTRN_ENTER_MONITOR.toInt();
    } else if(monitorStmt instanceof JExitMonitorStmt) {
      intrnOp = Intrinsic.INTRN_EXIT_MONITOR.toInt();
    } else {
      assertThat(false).as("should be enter monitor or exit monitor Stmt").isTrue();
    }

    int opType = JavaToWhirlConst.getMType(monitorValue.getType());
    int[] opsMtypes = {opType};
    long[] ops = {visitRightValue(monitorValue).node()};
    long[] opsType = {TypeHandler.getInternal(GCJType.JAVA_LANG_OBJECT_PTR)};
    long intrnCall = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      rtype, -1,
      intrnOp,
      opsMtypes,
      opsType,
      ops);
    return R.newNode(intrnCall);
  }

  private R visitJNopStmt() {
    // JNopStmt does not contain any code, but generated by soot for marking
    // generate a dummy label for nop stmt, as the stmt may be in EH border,
    // need a WN for it
    return R.newNode(BGenDriver.jniLabel());
  }

  private R visitJThrowStmt(JThrowStmt throwStmt) {
    needWrapByEHRegion = true;
    Value throwValue = throwStmt.getOp();
    assertThat(isSymbolValue(throwValue) || throwValue instanceof NullConstant).as(
      "throw value should be a symbol or null").isTrue();
    int opType = JavaToWhirlConst.getMType(throwValue.getType());
    int[] opsMtypes = {opType};
    long[] ops = {visitRightValue(throwValue).node()};
    long[] opsType = { TypeHandler.getType(throwValue.getType()) };
    /*
    long intrnCall = BGenDriver.createIntrinsic(
      Operator.OPR_INTRINSIC_CALL.toInt(),
      Mtype.MTYPE_V.toInt(),
      Intrinsic.INTRN_THROW.toInt(),
      opsMtypes,
      opsType,
      ops);
    return R.newNode(intrnCall);
    */

    // For Throw, generate call instead of intrnsic call as be do not need modeling it
    return R.newNode(BGenDriver.createCallStmt(Mtype.MTYPE_V.toInt(), Mtype.MTYPE_V.toInt(),
      SymbolHandler.getJvMethodSym(JvMethod._Jv_Throw), opsMtypes, opsType, ops, currSrcPos));
  }


  private R visitJNativeStmt(JNativeStmt nativeStmt) {
    Value nativeExpr = nativeStmt.getNativeExpr();
    return (visit(nativeExpr));
  }

  private R visitJNativeExpr(JNativeExpr nativeExpr) {
    String nativeName = nativeExpr.getName();
    int ptrMtype = BCRConfig.getPtrType();
    if(nativeName.equals(JvMethod._Jv_GetJNIEnvNewFrame.name())) {
      long retType = TypeHandler.getInternal(GCJType.V_PTR);
      // only one parameter for jclass
      long kidsTypes[] = new long [1];
      int  kidsMType[] = new int [1];
      long kids[] = new long [1];
      kidsTypes[0] = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
      kidsMType[0] = BGenDriver.getMTypeFromTypeIdx(kidsTypes[0]);
      long classSymbol = SymbolHandler.getClassInstanceSymbol(nativeExpr.getMethod().getDeclaringClass());
      kids[0] = BGenDriver.jniLDID(kidsMType[0], 0, classSymbol, kidsTypes[0]);
      return R.newNode(BGenDriver.createIntrinsic(Operator.OPR_INTRINSIC_CALL.toInt(), ptrMtype, retType, Intrinsic.INTRN_GET_JNIENV.toInt(),
         kidsMType, kidsTypes, kids));
    } else if(nativeName.equals(JvMethod._Jv_LookupJNIMethod.name())) {
      SootMethod sm = nativeExpr.getMethod();
      Value methodRet = nativeExpr.getAuxArg(0);
      Value methodNum  = nativeExpr.getAuxArg(1);
      Type classType = sm.getDeclaringClass().getType();
      assertThat(methodNum instanceof IntConstant).as("methodNum should be IntConst but not " +
        methodNum.getType().toString()).isTrue();
      String mangledName = MangleTool.mangleJNIMethod((RefType)classType, ((IntConstant)methodNum).value);
      Type retType = methodRet.getType();
      List<Type> paramType = new ArrayList<>();
      long retTypeIdx = TypeHandler.getType(methodRet.getType());
      long funSym = SymbolHandler.getNativeSymbol(mangledName, retType, paramType);
      long callNative = BGenDriver.jniCall(
        BGenDriver.getMTypeFromTypeIdx(retTypeIdx),
        Mtype.MTYPE_V.toInt(),
        nativeExpr.argCnt(), // extra env and object param
        funSym
      );

      // generate params
      for (int argIdx = 0; argIdx < nativeExpr.argCnt(); argIdx++) {
        if(argIdx == 1 && sm.isStatic()) {
          // the second param is class symbol, if static load the parameter from class symbol var
          // if dynamic load from this pointer
          long classSymbol = SymbolHandler.getClassInstanceSymbol(nativeExpr.getMethod().getDeclaringClass());
          long classTyIdx = TypeHandler.getInternal(GCJType.JAVA_LANG_CLASS_PTR);
          int  classMtype = BGenDriver.getMTypeFromTypeIdx(classTyIdx);
          long ldClassSt =  BGenDriver.jniLDID(classMtype, 0, classSymbol, classTyIdx);
          long paramWn = BGenDriver.jniParm(classMtype, ldClassSt, classTyIdx, Flag.WN_PARM_BY_VALUE.toInt());
          BGenDriver.jniSetWNKid(callNative, argIdx, paramWn);
        } else {
          Value arg = nativeExpr.getArg(argIdx);
          long argType = TypeHandler.getType(arg.getType());
          int argMType = BGenDriver.getMTypeFromTypeIdx(argType);
          long argSym = visit(arg).value;
          long ldArg = BGenDriver.jniLDID(argMType, 0, argSym, argType);
          long parmKidWN = ldArg;
          if (arg.getType() instanceof BooleanType ||
            arg.getType() instanceof ByteType ||
            arg.getType() instanceof CharType ||
            arg.getType() instanceof ShortType) {
            parmKidWN = BGenDriver.typeConversion(ldArg, (byte) Mtype.MTYPE_I4.toInt());
            argMType = Mtype.MTYPE_I4.toInt();
          }
          long paramWn = BGenDriver.jniParm(argMType, parmKidWN, argType, Flag.WN_PARM_BY_VALUE.toInt());
          BGenDriver.jniSetWNKid(callNative, argIdx, paramWn);
        }
      }

      // generate return value
      if(retType instanceof VoidType) {
        return R.newNode(callNative);
      } else {
        long blockWN = BGenDriver.jniCreateBlock();
        BGenDriver.jniSetLineNum(callNative, currSrcPos);
        BGenDriver.jniInsertBlockLast(blockWN, callNative);
        long regIdx = BGenDriver.getReturnValPreg();
        long ldidWN = BGenDriver.jniLDID(
          JavaToWhirlConst.getMType(retType), -1, regIdx, TypeHandler.getType(retType));
        requestLineNum(blockWN);
        return R.newNode(BGenDriver.jniComma(
          BGenDriver.getRtypeFromWN(callNative), Mtype.MTYPE_V.toInt(), blockWN, ldidWN));

      }
    } else {
      /* TODO: for other native calls
      int argCnt = nativeExpr.argCnt();
      long kidsTypes[] = new long [argCnt];
      int  kidsMType[] = new int [argCnt];
      long kids[] = new long [argCnt];
      for(int argIdx = 0; argIdx < argCnt; argIdx++) {
        Value arg = nativeExpr.getArg(argIdx);
        kidsTypes[argIdx] = TypeHandler.getType(arg.getType());
        kidsMType[argIdx] = BGenDriver.getMTypeFromTypeIdx(kidsTypes[argIdx]);
        long kidSym = visit(arg).value;
        kids[argIdx] = BGenDriver.jniLDID(kidsMType[argIdx], 0, kidSym, kidsTypes[argIdx]);
      }
      */
    }
    assertThat(false).as("unsupported native method " + nativeName).isTrue();
    return null;
  }

  private void dumpClassBody(SootClass cls, String baseName) {
    for (SootMethod method : cls.getMethods()) {
      if(skipMethod(method)) {
        continue;
      }
      method.retrieveActiveBody();
      if (method.hasActiveBody()) {
        Body body = method.getActiveBody();
        PhaseDumper.v().dumpBody(body, baseName);
      }
    }
  }

  // Is this is a valid call
  private boolean validateCalls(Body body) {
    for (Unit u : body.getUnits()) {
      Stmt s = (Stmt) u;
      if (s.containsInvokeExpr()) {
        InvokeExpr ie = s.getInvokeExpr();
        if (ie.getMethodRef().tryResolve() == null) {
          return false;
        }
      }
      if (s.containsFieldRef()) {
        FieldRef fr = s.getFieldRef();
        try {
          fr.getFieldRef().resolve();
        } catch (SootResolver.SootClassNotFoundException e) {
          logger.warn("SOOT FIX FOR B2W [SootField]: force resolve class Failed");
          return false;
        }
      }
    }
    return true;
  }

  private boolean verifyClass(SootClass cls) {
    try {
      for (SootMethod method : cls.getMethods()) {
        if (skipMethod(method)) {
          continue;
        }
        method.retrieveActiveBody();
        if (method.hasActiveBody()) {
          Body body = method.getActiveBody();
          if (validateCalls(body) == false) {
            return false;
          }
        }
      }
    } catch (RuntimeException e) {
      return false;
    }
    return true;
  }

  private boolean skipMethod(SootMethod method) {
    if (method.isAbstract() || method.isNative() || (
      Modifier.isSynthetic(method.getModifiers()) && Modifier.isVolatile(method.getModifiers()))) {
      return true;
    }
    if (!B2WFrontEnd.i().shouldVisitMethod(method.getName(), method.getDeclaringClass().toString())) {
      return true;
    }
    return false;
  }

  private String getDecompileFileFor(SootClass c) {
    char separatorChar = '/';
    StringBuffer b = new StringBuffer();
    b.append("decompile");
    b.append(separatorChar);
    String fixedPackageName = c.getJavaPackageName();
    if (!fixedPackageName.equals("")) {
      b.append(fixedPackageName.replace('.', separatorChar));
      b.append(separatorChar);
    }
    // SourceLocator.ensureDirectoryExists(new File(b.toString()));
    b.append(c.getShortJavaStyleName());
    b.append(".java");
    return b.toString();
  }


  private boolean generateDavaOutput(SootClass c)  {
    HashMap<SootMethod, Body> oldJimpleBodyMap= new HashMap<>();
    try {
      for (SootMethod m : c.getMethods()) {
        if (m.hasActiveBody()) {
          Body body = m.getActiveBody();
          if (body != null) {
            oldJimpleBodyMap.put(m, m.getActiveBody());
            m.setActiveBody(Dava.v().newBody(m.getActiveBody()));
          }
        }
      }

      OutputStream streamOut = null;
      PrintWriter writerOut = null;

      String fileName = getDecompileFileFor(c);
      new File(fileName).getParentFile().mkdirs();
      try {
        streamOut = new FileOutputStream(fileName);
      } catch (FileNotFoundException e) {
        e.printStackTrace();
        B2WFrontEnd.i().exit(ECode.FILE_NOT_FOUND);
      }
      writerOut = new PrintWriter(new OutputStreamWriter(streamOut));

      DavaPrinter.v().printTo(c, writerOut);
      try {
        writerOut.flush();
        streamOut.close();
        writerOut.close();
      } catch (IOException closeEx) {
        closeEx.printStackTrace();
        B2WFrontEnd.i().exit(ECode.FILE_CANNOT_CLOSE);
      }

      // Restore from davaBody to JimpleBody, and propagate the dava line number to JimpleBody
      for (SootMethod m : c.getMethods()) {
        if (m.hasActiveBody()) {
          Body body = m.getActiveBody();
          if (body != null) {
            assertThat(body instanceof DavaBody).as("soot Method body should be daveBody").isTrue();
            Body oldBody = oldJimpleBodyMap.get(m);
            assertThat(oldBody != null).as("old Jimple body should exists").isTrue();

            DavaBody davaBody = (DavaBody) body;
            for (Unit unit : oldBody.getUnits()) {
              Unit newUnit = davaBody.getNewUnit(unit);
              assertThat(newUnit != null).as("newUnit should not be null, oldUnit: " + unit.toString()).isTrue();
              unit.addTag(newUnit.getTag("JimpleLineNumberTag"));
            }
            m.setActiveBody(oldBody);
          }
        }
      }
      return true;
    } catch (Exception e) {
      for (SootMethod method :oldJimpleBodyMap.keySet())
      {
        Body body = oldJimpleBodyMap.get(method);
        assertThat(body != null && body instanceof JimpleBody).as("old body should exists and should be jimple body").isTrue();
        method.setActiveBody(oldJimpleBodyMap.get(method));
      }
      // skip the exception, continue generate
      return false;
    }
  }

  private String getLogHeader(int index) {
    return "[B2W " + index + "/" + handleClassList.size() + "] ";
  }

  // ==========================================================================================================
  @Override
  protected void internalTransform(String phaseName, Map options) {
    ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_SOOT);

    Scene s = Scene.v();
    Chain<SootClass> clazzList = s.getApplicationClasses();
    List<SootClass> visitedClassList = new ArrayList<>();
    handleClassList.addAll(clazzList);

    // reorder class order to unify generate whirl sequence
    Collections.sort(handleClassList, new Comparator<SootClass>() {
      public int compare(SootClass c1, SootClass c2) {
        return c1.getName().compareTo(c2.getName());
      }
    });

    ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_GEN_WHIRL);

    for (int i = 0; i < handleClassList.size(); i++) {
      SootClass clazz = handleClassList.get(i);
      // In library generation mode, use the filter
      if (isSkipClassForLibraryGen(clazz)) {
        logger.debug(getLogHeader(i) + "Skip clazz for library, class : " + clazz.toString());
        continue;
      }
      Tag sourceFile = clazz.getTag("SourceFileTag");
      // Get the plain source name
      String sourceName = (sourceFile != null) ?
        String.valueOf(((SourceFileTag) sourceFile).getSourceFile()) :
        clazz.getJavaStyleName() + ".java";

      // Source Code File Name Resolving
      if(sourceName != null && !B2WFrontEnd.i().isVTable()) {
        String realSource = locateRealSourceFile(clazz.getPackageName(), sourceName);
        BGenDriver.jniSetCurrentSrcFile(realSource, B2WFrontEnd.i().getFileSystemFlag().val);
      }
      if (B2WFrontEnd.i().isSkip(i, clazz.toString())) {
        logger.debug(getLogHeader(i) + "Skip class:" + clazz.toString());
        continue;
      }

      ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_VERIFY);
      if(B2WFrontEnd.i().isSkipSootError() && verifyClass(clazz) != true) {
        logger.debug(getLogHeader(i) + "Skip failed class:" + clazz.toString());
        continue;
      }
      ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_VERIFY);

      logger.debug(getLogHeader(i) + "Analyze class:" + clazz.toString());

      if (B2WFrontEnd.i().isDumpEnabled() && !B2WFrontEnd.i().isSkipSootError()) {
        ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_DUMP_BEFORE);
        dumpClassBody(clazz, "wjtp:b2w.in");
        ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_DUMP_BEFORE);
      }

      ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PAHSE_DAVA);

      if(B2WFrontEnd.i().isUseDavaLine()) {
        if(!generateDavaOutput(clazz)) {
          logger.warn("  Dava Generate fail, use class line number:" + clazz.toString());
        }
      }

      ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PAHSE_DAVA);
      try {
        visit(clazz);
      } catch (Exception e) {
        e.printStackTrace();
        System.err.println("Try run with -skip_e=" + i + " to skip the error class");
        B2WFrontEnd.i().exit(ECode.RUNTIME_EXCEPTION);
      }
      visitedClassList.add(clazz);
    }
    handleClassList.clear();

    ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_GEN_WHIRL);

    // Invoke library related
    LibraryGenerationHelper.dump(visitedClassList);

    if (B2WFrontEnd.i().isNoSootOutput()) {
      BGenDriver.bgenFileClose();
      BGenDriver.bgenFileFinish();
      B2WFrontEnd.i().finishUp();
      ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_ALL);
      ResourceMonitor.dumpAllUsage();
      B2WFrontEnd.i().exit(ECode.SUCCESS);
    }
  }

  /***
   * Filter through the classes of need,
   * only generate those desired classes
   * @param clazz
   * @return
   */
  boolean isSelectedClass(SootClass clazz) {
    final List<String> filter = B2WFrontEnd.i().getLibraryFilter();
    final boolean blackList = B2WFrontEnd.i().isLibraryBlackListMode();
    boolean selected = blackList;
    if (filter == null)
      return selected;
    for (String oneCriteria : filter) {
      // Matching the class name with filter
      if (clazz.getName().startsWith(oneCriteria)) {
        selected = !blackList;
        break;
      }
    }
    logger.debug("Class filter {}, visit?: {}", clazz.getName(), selected);
    return selected;
  }

  /***
   * Use the default filter to filter through cases.
   * @param clazz
   * @return true if the class should be skipped, false otherwise
   */
  boolean isSkipClassForLibraryGen(SootClass clazz) {
    boolean skipped = B2WFrontEnd.i().isLibGenOnly() && (!isSelectedClass(clazz) || clazz.isPhantom());
    if (skipped) {
      logger.debug("Skipping class by filter for libgen: {}", clazz.getName());
    }
    return skipped;
  }

  /**
   * Retrieve the real source file paths
   * When failed, return not-found/sourceName or in fileinfo.json mode, return packageName + clazzName;
   * TODO: there is still room for using the source list info from Maven/Gradle for this.
   *
   * @param packageName
   * @param sourceName in a.b.c.d.java
   * @return resolved path in absolute path
   */
  private String locateRealSourceFile(String packageName, String sourceName) {
    // If we only have the fileInfo.json available, we search in it.
    if (B2WFrontEnd.i().useFileInfo()) {
      // locate source name
      String fullName = sourceName;
      if (packageName != null && packageName.length() > 0) {
        String separator = B2WFrontEnd.i().getFileInfoSeparator();
        fullName = packageName.replaceAll("\\.", Matcher.quoteReplacement(separator)) + separator + sourceName;
      }
      MultiMap<String, String> srcNameToPath = B2WFrontEnd.i().getSourceCodeFilePaths();
      // First filter through by sourceName
      if (srcNameToPath.containsKey(sourceName)) {
        // Then filter through the packageName so that we don't add the wrong one.
        for (String onePath: srcNameToPath.get(sourceName)) {
          if (onePath.endsWith(fullName)) {
            logger.debug("Class {} :: {} mapped to file {}", packageName, sourceName, onePath);
            return onePath;
          }
        }
      }
      return fullName;
    }

    // If we are running locally, we search in local directories.
    if (srcDirectory != null && !srcDirectory.isEmpty()) {
      for (String oneDir : srcDirectory) {
        // Add package name to the prefix, as JFE is search locally, we use local separators
        String fullSourceName = packageName.replaceAll("\\.", File.separator) +
                File.separator + sourceName;
        File possible = new File(oneDir, sourceName);
        if (possible.exists()) {
          B2WFrontEnd.i().appendSourceFileToList(possible.getAbsolutePath());
          return possible.getAbsolutePath();
        }
      }
    }
    if (!skipSrcNotFound) {
      srcDirectory.forEach( x -> {System.out.println("Verifying showed '" + sourceName + "' is not found in dir '"+x+"'"); });
      assertThat(false).as("Cannot locate certain real source file for case scanned, use -skip-no-source if this should happen!!!").isTrue();
      return new File("not-found", sourceName).getAbsolutePath();
    } else {
      return new File("not-found", sourceName).getAbsolutePath();
    }
  }
}

