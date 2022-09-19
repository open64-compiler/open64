/*
 * Copyright (C) 2021 Xcalibyte (Shenzhen) Limited.
 */

package io.xcalibyte;

import com.github.rvesse.airline.SingleCommand;
import com.github.rvesse.airline.annotations.Arguments;
import com.github.rvesse.airline.annotations.Command;
import com.github.rvesse.airline.annotations.Option;
import com.github.rvesse.airline.annotations.restrictions.*;
import com.github.rvesse.airline.builder.ParserBuilder;
import com.github.rvesse.airline.help.Help;
import com.github.rvesse.airline.model.CommandMetadata;
import com.github.rvesse.airline.model.ParserMetadata;
import com.github.rvesse.airline.parser.options.AbstractNameValueOptionParser;
import com.github.rvesse.airline.parser.options.ClassicGetOptParser;
import com.github.rvesse.airline.types.numerics.bases.Hexadecimal;
import org.json.JSONArray;
import org.json.JSONObject;
import org.json.JSONTokener;
import org.objectweb.asm.ClassReader;
import soot.Main;
import soot.PackManager;
import soot.Transform;
import soot.util.HashMultiMap;
import soot.util.MultiMap;

// We are not using Slf4j for avoid conflict with soot's dependency
import org.apache.logging.log4j.Level;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.logging.log4j.core.LoggerContext;
import org.apache.logging.log4j.core.config.Configuration;
import org.apache.logging.log4j.core.config.Configurator;
import org.apache.logging.log4j.core.config.LoggerConfig;

import javax.inject.Inject;

import static org.assertj.core.api.Assertions.*;

import java.io.*;
import java.io.File;
import java.util.*;

class SepratorParser<T> extends AbstractNameValueOptionParser<T> {
  SepratorParser(char sep) {
    super(sep);
  }
}

@Command(name = "macbcr", description = "Mastiff Analyzer Component Byte Code Reader")
public class B2WFrontEnd {
  static final int initialSkipIdx = -1;
  static final int initialSkipMax = 0x7FFFFFFF;
  private static final Logger logger = LogManager.getLogger(B2WFrontEnd.class);

  private static B2WFrontEnd instance;

  // This is a map using Soot's implementation, a map that's like Map<Object, List<Object>>
  // Used for storing all source code files
  private MultiMap<String, String> sourceCodeFilePaths = new HashMultiMap<>();

  private String fileInfoSeparator = File.separator;

  // The file system flag is by-default Unix
  private InternalConstants.FileSystemFlag fileSystemFlag = InternalConstants.FileSystemFlag.FILE_SYSTEM_UNIX;

  @Inject
  private CommandMetadata metadata;

  @Option(name = "-h", title = "-h", arity = 0, description = "Show help message.")
  @Once
  private boolean help;

  @Option(name = "-fC", title = "-fC", arity = 1, description = "Input file path.")
  @Once
  @Path(mustExist = true, writable = false)
  private String inputFilePath;

  @Option(name = "-fD", title = "-fD", arity = 1, description = "Input directory path.")
  @Path(mustExist = true, writable = false)
  private List<File> classFileDirectory;

  @Option(name = "-srcdir", title = "-srcdir", arity = 1, description = "Source directory path.")
  @Path(mustExist = true, writable = false)
  private List<String> srcDirectories;

  @Option(name = "-skip-no-source", title = "true|false", arity = 1, description =
          "Skip source locating process if could not found, should not be used since out WebUI cannot take in files that cannot be located")
  @Once
  private boolean skipNoSource = true;


  @Option(name = "-fB", title = "-fB", arity = 1, description = "Output file name.")
  @Required
  @Once
  private String outputFileName;

  @Option(name = "-fbootclasspath", title = "-fbootclasspath", description = "bootstrap class path.")
  @Once
  @Path(mustExist = true, kind = PathKind.FILE, writable = false)
  private String bootClassPath = null;

  @Option(name = "-android-jars", title = "-android-jars", description = "android platform jar location")
  @Once
  @Path(mustExist = false, kind = PathKind.FILE, writable = false)
  private String androidJar = null;

  @Option(name = "-" + "g", title = "-g", arity = 0, description = "Convert args to soot and only run soot.")
  @Once
  private boolean directInvokeSoot = false;

  @Option(name = "-cp", title = "-cp", arity = 1, description = "Class path.")
  @Path(mustExist = true, writable = false)
  private List<String> inputClassPaths = null;

  @Option(name = "-srcPathOutput", title = "-srcPathList", arity = 1, description = "Output location for source_file.json.")
  @Once
  private String srcPathOutput;

  @Option(name = "-srcPathInput", title = "-srcPathInput", arity = 1, description = "path to the fileinfo.json.")
  @Once
  @Path(mustExist = true, readable = true)
  private String srcPathInput;

  // Externally appending list of source file names.
  private List<String> srcPathList = new LinkedList<>();

  // Opened file from above output location
  private FileWriter srcPathOutputFileWriter;

  @Option(name = "-jd", title = "-jd", arity = 1, description = "Jar directory, will find all jars.")
  @Path(mustExist = true, writable = false)
  private List<String> inputJarDirectory = null;

  @Option(name = "-ocob", title = "true|false", arity = 1, description = "Generate one B file for one class.")
  @Once
  private boolean genOneBForOneClass = false;

  @Option(name = "-monitor", title = "true|false", arity = 1, description = "monitor time/memory resource.")
  @Once
  private boolean monitorEnabled = false;

  @Option(name = "-m", title = "-m", arity = 1, description = "ABI bit length, 32 or 64.")
  @Once
  private int abiBitLength = 64;

  @Option(name = "-gcj", title = "-gcj", arity = 1, description = "Generate WHIRL for gcj.")
  @Once
  private boolean useGCJ = false;

  @Option(name = "-devirtIndicator", title = "-dI", arity = 1, description = "Generate Devirt Indicator Call with ICALL.")
  @Once
  private boolean devirtIndicator = true;

  @Option(name = "-throwNoReturn", title = "-tnr", arity = 1, description = "Mark Throw function as no-return.")
  @Once
  private boolean throwNoReturn = true;

  @Option(name = "-trace", title = "true|false", arity = 1, description = "Enable the trace or not")
  @Once
  private boolean trace = false;

  @Option(name = "-traceLevel", title = "Integer Value", arity = 1, description = "Set trace level")
  @Once
  private int traceLevel = 0;

  @Option(name = "-skip_e", title = "0|1|2...", arity = 1, description = "skip gen B for class index at given input index")
  private List<Integer> skipEqual = null;

  @Option(name = "-skip_l", title = "0|1|2...", arity = 1, description = "skip gen B for class index less than given input index")
  @Once
  private int skipLess = initialSkipIdx;

  @Option(name = "-skip_g", title = "0|1|2...", arity = 1, description = "skip gen B for class index larger than given input index")
  @Once
  private int skipGreater = initialSkipMax;

  @Option(name = "-d", title = "-d", arity = 0, description = "dump intermediate soot jimple")
  @Once
  private boolean dumpEnabled = false;

  @Option(name = "-v", title = "-v", arity = 0, description = "print verbose for B2W front end")
  @Once
  private boolean verbose = false;

  @Option(name = "-logLevel", title = "ALL|DEBUG|INFO|WARN|ERROR|FATAL", arity = 1, description = "logging level, this will override verbose (-v)")
  @Once
  private String logLevel = null;


  @Option(name = "-genEH", title = "0|1|2", arity = 1, description = "Generate EH Level\n  0: no EH\n  1: partitial EH\n  2: Full EH(Default)")
  @Once
  private int genEH = 2;

  @Option(name = "-noSootOutput", title = "true|false", arity = 1, description = "Only gen Whirl file, do not emit soot output")
  @Once
  private boolean noSootOutput = true;

  @Option(name = "-useDavaLine", title = "true|false", arity = 1, description = "generate line number using dava decompiled source line")
  @Once
  private boolean useDavaLine = false;


  @Option(name = "-useBytecodeMapping", title = "true|false", arity = 1, description = "generate line number using dava decompiled source line")
  @Once
  private boolean useBytecodeMapping = false;


  @Option(name = "-sootOutputDir", title = "ARG ", arity = 1, description = "soot option, set soot output dir")
  private String sootOutputDir = null;

  @Option(name = "-allow-phantom-refs", title = "true|false", arity = 1, description = "soot option, allow phantom reference")
  @Once
  private boolean allowPhantomRef = false;


  @Option(name = "-skip-soot-error", title = "true|false", arity = 1, description = "ignore soot errors, and verify jimple " +
    "by traverse calls, skip generate the method if failed, default on for apk build")
  @Once
  private boolean skipSootError = false;

  @Option(name = "-include", title = "-include ARG ", arity = 1, description = "soot option, add ARG as extra application class")
  private List<String> includeClasses = null;

  @Option(name = "-only-include", title = "-only-include ARG ", arity = 1, description = "soot option, only set ARG as application class")
  private List<String> onlyIncludeClasses = null;

  @Option(name = "-exclude", title = "exclude ARG ", arity = 1, description = "soot option, exclude ARG from application class")
  private List<String> excludeClasses = null;


  @Option(name = "-RBC", title = "true|false", arity = 1, description = "current file is rbc")
  @Once
  private boolean isRBC = false;

  @Option(name = "-VTABLE", title = "true|false", arity = 1, description = "current file is vtable file")
  @Once
  private boolean isVTable = false;

  @Option(name = "-OPT", title = "true|false", arity = 1, description = "optimize jimple body before convert")
  @Once
  private boolean optimize = true;

  @Option(name = "-libGen", title = "true|false", arity = 1, description = "generate V-Table for library classes")
  @Once
  private boolean libraryVTable = false;

  @Option(name = "-libGenOnly", title = "true|false", arity = 1, description = "generate V-Table for only libraries, no code generation ")
  @Once
  private boolean libGenOnly = false;

  @Option(name = "-libNoRuntime", title = "true|false", arity = 1, description = "generate V-Table while skipping JDK Runtime")
  @Once
  private boolean libraryNoJavaRuntime = false;

  @Option(name = "-libFilter", title = "-libFilter ARG ", arity = 1, description = "filter of class names for visiting")
  private List<String> clazzFilters;

  @Option(name = "-libFilterMethod", title = "-libFilterMethod ARG ", arity = 1, description = "filter for method names for visiting")
  private List<String> methodFilters;

  @Option(name = "-libFilterBlackList", title = "true|false", arity = 1, description = "filtering classes in a black list mode, used with -libFilter")
  private boolean libraryBlackListMode = true;

  @Option(name = "-libFilterMethodBlackList", title = "true|false", arity = 1, description = "filtering methods in a black list mode, used with -libFilterMethod")
  private boolean methodFilterBlackListMode = true;

  @Option(name = "-libSubsideFile", title = "-libSubsideFile", arity = 1, description = "subsidiary data for WHIRL vtable info")
  @Once
  @Path(mustExist = false, kind = PathKind.FILE, writable = true)
  private String subsideFile = null;

  @Option(name = "-dumpMethodName", title = "-dumpMethodName", arity = 1, description = "dump each method's signature and mangled name")
  @Once
  private boolean dumpMethodName = false;

  @Option(name = "-reflectionSubside", title = "-reflectionSubside", arity = 1, description = "reflection subsidiary data to be stored in the IR, for debugging purpose")
  @Once
  private String reflectionSubsideData;

  @Option(name = "-anno", title = "-anno", arity = 1, description = "whether to enable dumping annotation to WHIRL")
  @Once
  private boolean enableAnnotationData = true;

  @Arguments
  private List<String> otherArgs;

  boolean isDumpEnabled() {
    return dumpEnabled;
  }

  boolean isSkipSootError() {
    return skipSootError;
  }

  boolean isSkip(int index, String clazzName) {
    if(index < skipLess || index > skipGreater) {
      return true;
    }
    if(skipEqual != null && skipEqual.contains(index)) {
      return true;
    }
    // Use library filter to filter the classes
    final List<String> filter = getLibraryFilter();
    final boolean blackList = isLibraryBlackListMode();
    boolean shouldVisitThisMethod = blackList;
    if (filter == null)
      return !shouldVisitThisMethod;
    for (String oneCriteria : filter) {
      // Matching the class name with filter
      if (clazzName.startsWith(oneCriteria)) {
        shouldVisitThisMethod = !blackList;
        break;
      }
    }
    logger.debug("Class filter, clazz name {}, visit?: {}", clazzName, shouldVisitThisMethod);
    return !shouldVisitThisMethod;
  }

  boolean isVerbose() {
    return verbose;
  }

  void verbose(String string) {
    logger.trace(string);
  }

  boolean isGenEH()     { return genEH > 0; }
  boolean isGenPartEH() { return genEH == 1; }
  boolean isGenFullEH() { return genEH == 2; }

  boolean isNoSootOutput() {
    return noSootOutput;
  }

  boolean isUseDavaLine() { return useDavaLine; }

  boolean isVTable() {
    return isVTable;
  }

  boolean Optimize() {
    return optimize;
  }

  private void findJar(File dir, List<String> jarPathList) {
    for (File f: dir.listFiles()) {
      if (f.isDirectory()) {
        findJar(f, jarPathList);
      } else if (f.getName().endsWith(".jar")) {
        jarPathList.add(f.getAbsolutePath());
      }
    }
  }

  private String[] convertToSootArgs() {
    assertThat((classFileDirectory != null && !classFileDirectory.isEmpty()) ||
            ((inputFilePath != null) &&
           (inputFilePath.endsWith(".class") ||
            inputFilePath.endsWith(".jar") ||
            inputFilePath.endsWith(".apk") ||
            inputFilePath.endsWith(".war") ||
            inputFilePath.endsWith(".java")))).as(
            "Input file is not class/jar/dir, file path : " + inputFilePath).isTrue();
    File outputFile = new File(outputFileName);
    assertThat(!(outputFile.exists() && outputFile.isDirectory())).as(
            "Output file should not be a directory, given: " + outputFileName).isTrue();
    if (outputFile.exists()) {
      outputFile.delete();
    }
    String clazzName = "";
    String suffix = "";
    boolean isDirectoryMode = false;
    StringBuilder clazzPath = new StringBuilder();

    ArrayList<String> sootArgsList = new ArrayList<>();
    if (classFileDirectory != null && !classFileDirectory.isEmpty()) {
      for (File cfd : classFileDirectory) {
        sootArgsList.add("-process-dir");
        sootArgsList.add(cfd.getAbsolutePath());
        isDirectoryMode = true;
      }
    } else {
      assertThat(inputFilePath != null)
              .as("-fC, or -fD must at least be given one. use -help to display usage info")
              .isTrue();
      File inputFile = new File(inputFilePath);
      String inputFileAbsPath = inputFile.getAbsolutePath();
      if (inputFilePath.endsWith(".class")) {
        try {
          InputStream is = new FileInputStream(inputFilePath);
          ClassReader cr = new ClassReader(is);
          clazzName = cr.getClassName();
          is.close();
        } catch (Exception e) {
          logger.error("Read .class input file exception, class file : " + inputClassPaths);
          e.printStackTrace();
          System.exit(1);
        }
        suffix = clazzName + ".class";
        assertThat(inputFile.getAbsolutePath().endsWith(suffix)).as(
          "Class file abs path not match class qualifier name, class file abs path : "
          + inputFileAbsPath + ", class qualifier name : " + suffix).isTrue();
        clazzPath.append(inputFileAbsPath.substring(0, inputFileAbsPath.length() - suffix.length()));
      } else if (inputFilePath.endsWith(".jar") || inputFilePath.endsWith(".war")) {
        suffix = inputFilePath;
        sootArgsList.add("-process-dir");
        sootArgsList.add(inputFileAbsPath);
      } else if (inputFilePath.endsWith(".apk")) {
        suffix = inputFilePath;
        sootArgsList.add("-src-prec");
        sootArgsList.add("apk");
        sootArgsList.add("-process-multiple-dex");
        sootArgsList.add("-process-dir");
        sootArgsList.add(inputFileAbsPath);

        assertThat(androidJar != null).as("need specify android platform jar location for apk build").isTrue();
        sootArgsList.add("-android-jars");
        sootArgsList.add(androidJar);

        sootArgsList.add("-ignore-resolving-levels");

        skipSootError = true;

        // below are used for debug usage
        // sootArgsList.add("-ire");
        // sootArgsList.add("-debug-resolver");
        //sootArgsList.add("-allow-phantom-refs");
      } else {
        assertThat(false).as("inputfile should be class or jar file").isTrue();
      }
    }

    if(bootClassPath != null) {
      File bootClass = new File(bootClassPath);
      clazzPath.append(":").append(bootClass.getAbsolutePath());
    }
    if(inputClassPaths != null) {
      for(String path : inputClassPaths) {
        clazzPath.append(":").append(path);
      }
    }
    if (inputJarDirectory != null) {
      List<String> jarPathList = new ArrayList<>();
      for (String path: inputJarDirectory) {
        findJar(new File(path), jarPathList);
      }
      for (String jarPath: jarPathList) {
        clazzPath.append(":").append(jarPath);
      }
    }
    // set class path
    sootArgsList.add("-cp");
    sootArgsList.add(clazzPath.toString());

    // if not set bootclasspath,  use system JAVA_HOME
    if(bootClassPath == null) {
      sootArgsList.add("-pp");
    }

    if(allowPhantomRef) {
      sootArgsList.add("-allow-phantom-refs");
    }

    if(includeClasses != null) {
      for (String includeClass :includeClasses) {
        sootArgsList.add("-include");
        sootArgsList.add(includeClass);
      }
    }

    if(excludeClasses != null) {
      for (String excludeClass :excludeClasses) {
        sootArgsList.add("-exclude");
        sootArgsList.add(excludeClass);
      }
    }

    if(onlyIncludeClasses != null) {
      for (String onlyIncludeClass :onlyIncludeClasses) {
        sootArgsList.add("-only-include");
        sootArgsList.add(onlyIncludeClass);
      }
    }

    if(sootOutputDir != null) {
      sootArgsList.add("-d");
      sootArgsList.add(sootOutputDir);
    }

    if(useDavaLine) {
      noSootOutput = true;
      sootArgsList.add("-use-dava-line");
    }

    if(useBytecodeMapping) {
      sootArgsList.add("-use-bytecode-mapping");
    }

    if(verbose) {
      sootArgsList.add("-show-start-end");
    }

    if(trace) {
      sootArgsList.add("-debug-b2w");
    }

    if(skipSootError) {
      sootArgsList.add("-skip-soot-error");
    }

    // set output file format
    sootArgsList.add("-f");
    sootArgsList.add("J");

    // input Attribute Option
    // keep the line number info
    sootArgsList.add("-keep-line-number");
    sootArgsList.add("-keep-offset");

    // use whole-program mode, this means that we find references for each class transitively,
    // A(application Class) -> B (library Class) -> C (library Class)
    sootArgsList.add("-w");

    // disable cg
    sootArgsList.add("-p");
    sootArgsList.add("cg");
    sootArgsList.add("enabled:false");
    // disable wjop
    sootArgsList.add("-p");
    sootArgsList.add("wjop");
    sootArgsList.add("enabled:false");
    // disable wjap
    sootArgsList.add("-p");
    sootArgsList.add("wjap");
    sootArgsList.add("enabled:false");
    // disable jtp
    sootArgsList.add("-p");
    sootArgsList.add("jtp");
    sootArgsList.add("enabled:false");

    // disable jop
    sootArgsList.add("-p");
    sootArgsList.add("jop");
    sootArgsList.add("enabled:false");
    // disable jap
    sootArgsList.add("-p");
    sootArgsList.add("jap");
    sootArgsList.add("enabled:false");
    // disable bb
    sootArgsList.add("-p");
    sootArgsList.add("bb");
    sootArgsList.add("enabled:false");
    // disable tag
    sootArgsList.add("-p");
    sootArgsList.add("tag");
    sootArgsList.add("enabled:false");

    if (libGenOnly) {
      // This disables all jimple builder phase to not construct function body
      // Only applies when we do not want any of the code, but only the V-Tables
      sootArgsList.add("-allow-phantom-elms");
      sootArgsList.add("-library-only-mode");
    }

    // disable jb phase
    sootArgsList.add("-p");
    sootArgsList.add("jb.dtr");
    sootArgsList.add("enabled:false");

    sootArgsList.add("-p");
    sootArgsList.add("jb.ese");
    sootArgsList.add("enabled:false");

    sootArgsList.add("-p");
    sootArgsList.add("jb.a");
    sootArgsList.add("enabled:false");

    sootArgsList.add("-p");
    sootArgsList.add("jb.ule");
    sootArgsList.add("enabled:false");

    if(!Optimize()) {
      sootArgsList.add("-p");
      sootArgsList.add("jb.cp");
      sootArgsList.add("enabled:false");

      sootArgsList.add("-p");
      sootArgsList.add("jb.cp");
      sootArgsList.add("only-stack-locals:false");

      sootArgsList.add("-p");
      sootArgsList.add("jb.dae");
      sootArgsList.add("enabled:false");
    }
    sootArgsList.add("-p");
    sootArgsList.add("jb.cp-ule");
    sootArgsList.add("enabled:false");

    // ignore null pointer dereference, soot will convert
    // null.fun() -> throw null exception
    // we don't want this check, let be check this
    sootArgsList.add("-p");
    sootArgsList.add("jb.tr");
    sootArgsList.add("ignore-nullpointer-dereferences:true");

    // can't disable this phase, test case : ExceptionTest5a.java
    /*
    sootArgsList.add("-p");
    sootArgsList.add("jb.uce");
    sootArgsList.add("enabled:false");
    */

    // can't disable this phase, will cause stack trace
    /*
    sootArgsList.add("-p");
    sootArgsList.add("jb.ls");
    sootArgsList.add("enabled:false");
    */

    sootArgsList.add("-p");
    sootArgsList.add("jb");
    sootArgsList.add("use-original-names:true");

    if (!libraryNoJavaRuntime) {
      // do not exclude java libraries when analyze rt.jar
      sootArgsList.add("-include-all");
    }

    if (libGenOnly) {
      // This enforces all classes to be loaded only to signature level, not body level
      // This was used for fixing issues related to zentao-id
      sootArgsList.add("-x");
      sootArgsList.add("*");
    }

    // force resolver all referenced class, and do not parse body for excluded class
    // sootArgsList.add("-full-resolver");
    // can't turn on this option, will ignore some error, but we need to make sure soot is ok
    // sootArgsList.add("-no-bodies-for-excluded");

    if(!isDirectoryMode && inputFilePath.endsWith(".class")) {
      sootArgsList.add(clazzName.replace('/', '.'));
    }

    String[] sootArgs = new String[sootArgsList.size()];
    return sootArgsList.toArray(sootArgs);
  }

  void run() {
    if(help || otherArgs != null) {
      int exitCode = B2WGenerator.ECode.SUCCESS.toInt();
      if(otherArgs != null) {
        System.err.println("Invalid arg:" + otherArgs);
        exitCode = B2WGenerator.ECode.INVALID_ARGS.toInt();
      }
      try {
        Help.help(this.metadata);
      } catch (Exception e) {
        e.printStackTrace();
      }
      System.exit(exitCode);
    }

    initLoggers();

    ResourceMonitor.initialize(monitorEnabled);
    ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_ALL);
    BCRConfig.init(abiBitLength, useGCJ);

    // Open fileinfo.json for searching
    if (srcPathInput != null) {
      try {
        readFileInfo(srcPathInput);
      } catch (IOException e) {
        e.printStackTrace();
        System.exit(B2WGenerator.ECode.INVALID_ARGS.toInt());
      }
    } else if (srcPathOutput != null){
      // Open output src location file
      File srcPathOutputFile = new File(srcPathOutput).getAbsoluteFile();
      if (srcPathOutputFile.getParentFile() != null && !srcPathOutputFile.getParentFile().exists()) {
        boolean mkDirResult = srcPathOutputFile.getParentFile().mkdirs();
        assertThat(mkDirResult).as("Cannot make directory for storing the source_file under : " + srcPathOutput).isTrue();
      }

      // Remove old file.
      if (srcPathOutputFile.exists()){
        boolean deleteResult = srcPathOutputFile.delete();
        assertThat(deleteResult).as("Cannot delete old source file json").isTrue();
      }
      assertThat(!srcPathOutputFile.exists()).as("Cannot remove pre-existing source_file json file").isTrue();

      // Verify writable
      assertThat(srcPathOutputFile.getParentFile().canWrite()).as("Cannot write to the folder of source file json").isTrue();
      try {
        srcPathOutputFileWriter = new FileWriter(srcPathOutputFile);
      } catch (IOException e) {
        // If open file failed, continue regardless the issue, yet leaving warning.
        e.printStackTrace();
        srcPathOutputFileWriter = null;
      }
    }

    String[] sootArgs = convertToSootArgs();
    logger.debug("Soot args : " + String.join(" ", sootArgs));
    {
      System.loadLibrary("macbcb");
      // prepare job finished, start now, we can use trace
    }
    logger.debug("Finished loading library macbcb");
    if(directInvokeSoot) {
      Main.main(sootArgs);
      return;
    }
    if(!genOneBForOneClass) {
      BGenDriver.bgenInit(trace, traceLevel, abiBitLength);
      BGenDriver.bgenInitOpenIrFile(outputFileName);
      if(isRBC) {
        BGenDriver.setFileRbc();
      }
      if(isVTable) {
        assertThat(!isRBC).as("isVTable is ture, isRBC can't be true.").isTrue();
        BGenDriver.setFileVTable();
      }
      // First time must be called.
      //
      if (classFileDirectory != null && !classFileDirectory.isEmpty()) {
        inputFilePath = new File("in.dir.scan").getAbsolutePath();
      }
      else{
        String realSourcePath = new File(inputFilePath).getAbsolutePath();
        assertThat(realSourcePath.charAt(0) == '/' ||
                realSourcePath.length() > 2 && realSourcePath.charAt(1) == ':')
                .as("The source path resolved should be absolute, yet given = " + realSourcePath).isTrue();
      }
      BGenDriver.bgenInitSetDST(inputFilePath);
    }
    B2WGenerator gen = new B2WGenerator(srcDirectories, skipNoSource);
    PackManager.v().getPack("wjtp").add(
      new Transform("wjtp.b2w", gen)
    );
    ResourceMonitor.start(ResourceMonitor.MonitorPhase.MONITOR_PHASE_SOOT);

    Main.main(sootArgs);

    this.finishUp();

    if(!genOneBForOneClass) {
      BGenDriver.bgenFileClose();
      BGenDriver.bgenFileFinish();
    }

    ResourceMonitor.end(ResourceMonitor.MonitorPhase.MONITOR_PHASE_ALL);
    ResourceMonitor.dumpAllUsage();
  }

  /***
   * Initialize loggers with logLevel
   */
  private void initLoggers() {
    Level levelToSet = Level.WARN;
    if (verbose)
      levelToSet = Level.ALL;
    if (logLevel != null) {
      switch (logLevel) {
        case "ALL":
          levelToSet = Level.ALL;
          break;
        case "DEBUG":
          levelToSet = Level.DEBUG;
          break;
        case "INFO":
          levelToSet = Level.INFO;
          break;
        case "WARN":
          levelToSet = Level.WARN;
          break;
        case "ERROR":
          levelToSet = Level.ERROR;
          break;
        case "FATAL":
          levelToSet = Level.FATAL;
          break;
        default:
          // fall through expected
      }
    }
    LoggerContext ctx = (LoggerContext) LogManager.getContext(false);
    Configuration config = ctx.getConfiguration();
    LoggerConfig loggerConfig = config.getLoggerConfig(LogManager.ROOT_LOGGER_NAME);
    loggerConfig.setLevel(levelToSet);
    ctx.updateLoggers();  // This causes all Loggers to refetch information from their LoggerConfig.
    // org.apache.logging.log4j.core.config.Configurator;
    Configurator.setLevel("io.xc5", levelToSet);
    // You can also set the root logger:
    Configurator.setRootLevel(levelToSet);
    logger.debug("Log level set to : {}", levelToSet);
  }

  void appendSourceFileToList(String sourceFileAbsoluteName) {
    if (sourceFileAbsoluteName!= null)
      this.srcPathList.add(sourceFileAbsoluteName);
  }

  public String getFileInfoSeparator() {
    return fileInfoSeparator;
  }

  /***
    Reading the fileinfo.json to a multimap, with file basenames -> file absolute path.
    Doing this instead of reading all file names to a list is to improve the performance.
   */
  private void readFileInfo(String srcPathInput) throws IOException {
    assertThat(new File(srcPathInput)).as("Fileinfo.json does not exist : " + srcPathInput).exists();
    FileInputStream srcFileInputStream = new FileInputStream(new File(srcPathInput));
    JSONTokener jsonTokener = new JSONTokener(srcFileInputStream);
    JSONObject jsonObject = new JSONObject(jsonTokener);
    /**
      A jsonObject here is like:
      {
      "numberOfFilesWithoutPermission": "0",
      "osType": "linux",
      "sourceType": "volume_upload",
      "numberOfFiles": "100",
      "gitlabProjectId": "",
      "gitUrl": "",
      "sourceCodeFileId": "00000000-0000-0000-0000-000000000000",
      "files": [
        {
          "noOfLines": "0",
          "checksum": "0",
          "filePath": "/home/xc5/xc5/testcase/java-gzip-maven",
          "fileName": "java-gzip-maven",
          "version": "0",
          "relativePath": "/",
          "depth": "0",
          "parentPath": null,
          "fileSize": "0",
          "type": "DIRECTORY",
          "fileId": "1"
        },
        {
          "noOfLines": "10",
          "checksum": "123456789",
          "filePath": "/home/xc5/xc5/testcase/java-gzip-maven/a.java",
          "fileName": "a.java",
          "version": "1567850052856000000",
          "relativePath": "/a.java",
          "depth": "1",
          "parentPath": "/",
          "fileSize": "100",
          "type": "FILE",
          "fileId": "1"
        },
        ... many more ...
      ]

     */
    if (jsonObject.has("files")) {
      JSONArray files = jsonObject.getJSONArray("files");
      for (Object entry : files) {
        if (entry instanceof JSONObject) {
          if ("FILE".equals(((JSONObject) entry).getString("type"))) {
            // We'd extract the filePath and fileName from the fileinfo.json
            String path = ((JSONObject) entry).getString("filePath");
            String fileName = ((JSONObject) entry).getString("fileName");

            // We don't want duplicate entries in it.
            if (!sourceCodeFilePaths.contains(fileName, path))
              sourceCodeFilePaths.put(fileName, path);
          }
        }
      }
    }
    // We need this info to match the file names for unix-like or windows paths.
    if (jsonObject.has("osType") && jsonObject.getString("osType").equals("win")) {
      fileSystemFlag = InternalConstants.FileSystemFlag.FILE_SYSTEM_WINDOWS;
      fileInfoSeparator = "\\";
    } else {
      fileSystemFlag = InternalConstants.FileSystemFlag.FILE_SYSTEM_UNIX;
      fileInfoSeparator = "/";
    }
    srcFileInputStream.close();
  }

  /***
   * We'd use fileinfo.json when available.
   * @return true if it's applicable
   */
  public boolean useFileInfo() {
    return srcPathInput != null;
  }

  /***
   * Return the map containing source-code file paths
   * Using this map will reduce the complexity in traversing the source code list
   * @return
   */
  public MultiMap<String, String> getSourceCodeFilePaths() {
    return sourceCodeFilePaths;
  }

  void finishUp() {
    // Writing the source files list to Json format
    if (srcPathOutputFileWriter != null) {
      JSONArray sourceFileNames = new JSONArray();
      srcPathList.forEach(x -> sourceFileNames.put(x));
      try {
        srcPathOutputFileWriter.write(sourceFileNames.toString());
        srcPathOutputFileWriter.close();
      } catch (IOException e) {
        e.printStackTrace();
        assertThat(false).as("Cannot write to the source_files json").isTrue();
      }
    }
  }

  void exit(B2WGenerator.ECode errcode) {
    if(errcode != B2WGenerator.ECode.SUCCESS) {
      // clean up output itermidate files for fail exit
      File outputFile = new File(outputFileName);
      if (outputFile.exists()) {
        outputFile.delete();
      }
    }
    System.exit(errcode.toInt());
  }

  static synchronized B2WFrontEnd i() {
    if (instance == null) {
      assertThat(false).as("Not set B2WFrontEnd instance.").isTrue();
    }
    return instance;
  }

  static void parse(String[] args) {
    ParserMetadata<B2WFrontEnd> parserMetaData = new ParserBuilder<B2WFrontEnd>()
      .withOptionParser(new SepratorParser<>(',')).withOptionParser(new SepratorParser<>('='))
      .withOptionParser(new ClassicGetOptParser<>()).build();
    SingleCommand<B2WFrontEnd> parser = SingleCommand.singleCommand(B2WFrontEnd.class, parserMetaData);
    instance = parser.parse(args);
  }

  public static void main(String[] args) throws IOException {
    B2WFrontEnd.parse(args);
    B2WFrontEnd.i().run();
  }

  public boolean isUseDevirtIndicator() {
    return devirtIndicator;
  }

  public boolean isThrowNoReturn() {
    return throwNoReturn;
  }

  public boolean isGcj() {
    return useGCJ;
  }

  public boolean isLibraryVTable() {
    return libraryVTable;
  }

  public boolean isLibGenOnly() {
    return libGenOnly;
  }

  public boolean isLibraryNoJavaRuntime() {
    return libraryNoJavaRuntime;
  }

  public boolean isLibraryBlackListMode() {
    return libraryBlackListMode;
  }

  public boolean isMethodFilterBlackListMode() {
    return methodFilterBlackListMode;
  }

  public List<String> getLibraryFilter() {
    return clazzFilters;
  }

  public void setVTable(boolean b) {
    isVTable = b;
  }

  public boolean needToDumpMethodName() { return dumpMethodName; }

  public String getLibrarySubsidiaryOutputFileName() {
    if (subsideFile != null)
      return subsideFile;
    else
      return outputFileName + ".vtable";
  }

  public boolean shouldVisitMethod(String methodName, String clazzName) {
    final List<String> filter = methodFilters;
    final boolean blackList = isMethodFilterBlackListMode();
    boolean selected = blackList;
    if (filter == null)
      return selected;
    for (String oneCriteria : filter) {
      // Matching the class name with filter
      if (methodName.startsWith(oneCriteria)) {
        selected = !blackList;
        break;
      }
    }
    logger.debug("Method filter {}, visit?: {}", methodName, selected);
    return selected;
  }

  public String getReflectionSubsideData() {
    return reflectionSubsideData;
  }

  public void setReflectionSubsideData(String reflectionSubsideData) {
    this.reflectionSubsideData = reflectionSubsideData;
  }

  public boolean isEnableAnnotationData() {
    return enableAnnotationData;
  }

  public void setEnableAnnotationData(boolean enableAnnotationData) {
    this.enableAnnotationData = enableAnnotationData;
  }

  public InternalConstants.FileSystemFlag getFileSystemFlag() {
    return fileSystemFlag;
  }
}
