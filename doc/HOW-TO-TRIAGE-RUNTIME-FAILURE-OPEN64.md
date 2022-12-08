# Summary

Triaging is a scientific process by sequence of experiment. Establish the baseline first and then add more condition on top. Once the new condition is proven working, it becomes the new baseline, and the experiment continues till it fails. The last addition to the experiment is the culprit for next level detail triaging. It is design goal to make Open64 optimizations debug-able by developer. The side effect is the parallel development by different optimization team. We are describing how such method can be used for triaging.

# Optimization Levels

There are four optimization level at procedure level. IPA works across function boundary. Therefore, the IPA can be used with all optimization level. The natural sequence of building a new target with Open64, we always make -O0 work first, then -O1, -O2 , -O3, and apply -ipa as the last step. Due to this design and development steps for optimization levels, we can also make use of the same method for bug triaging. The following table shows the relationship among optimization level used to triage optimization to component level:

| Baseline works | Compare set does not work | Error likely located in |
| -------------- | ------------------------- | ----------------------- |
| -O0            | -O1                       | CG                      |
| -O1            | -O2                       | WOPT                    |
| -O2            | -O3                       | LNO                     |
| -O*            | -O* -ipa                  | IPA                     |

Using optimization level usually is the first level of triaging. Once the likely culprit is identified, the triaging process will go to the next level.

# Determine IT’S a REGRESSION

Controlling the WHIRL version has been central to Open64 since it is created. As long as the version of WHIRL remains the same, it enables backend components, including BE, CG, WOPT, LNO and IPA to be mix-and-match and still runs correctly. Typically, we stash away at least two versions, STABLE and LATEST, of sharable library of each component. Developers in each back end team can use either of these two set of libraries as the baseline and redirect the compiler to use the to-be-tested component by command line option “-*path” to specify the path of this component. For example, “-wpath=/…/…/…” for wopt build directory.



Let’s say, -O1 works and -O2 fails. The reasonable next step is to find out if such failure is a regression in WOPT since the “STABLE” compiler. One could use the “STABLE” version of such component using the path option to verify if this component has regression against the “STABLE” version of compiler. 

 

The following table shows what option to use for each of the backend components:

| Backend component | Sharable library name | Path option |
| ----------------- | --------------------- | ----------- |
| CG                | cg.so ->              |             |
| WOPT              | wopt.so ->            | -wpath      |
| LNO               | lno.so->              |             |
| IPA               | Ipo.so->              |             |

# Identify bad Files 

It’s safe to assume applications are composed of many files in its build procedure/script. Identifying the optimization bug, assuming -O0 build passed, it is important to identify the file or files are incorrectly compiled due to bad optimization. To speed up the triaging process, we can do two things:

1. Save the all baseline build, such as -O1 build, .o files and current triaging option, such as -O2 build. This will result in saving of recompiling all files in each build procedure.

2. Modify build process to be link only

3. Perform test run by mix-and-match baseline vs. compare set objects by binary search to reduce the number of run required.

# Identify bad Procedure by Binary Search

Many components offer such capability. The compiler will show the procedure number right after the procedure name when the option “-show” is used. We can find out the number of procedures in the file by noting down the last procedure get compiled. We could then use the option “-OPT:skip_before=#” and “-OPT:skip_after=#” to skip optimizations while compiling the file. Once the procedure is identified, we can continue using these two flags to control optimization only happens to the specific procedure that we are debugging.

# Identify the BAD Optimizations in backend components

When a specific component has been identified as the culprit of a runtime failure, the very next thing to do is to identify what specific optimization has caused the failure. As we are all aware, there are often dependencies among phases of optimizations. Therefore, it is sensible to disable optimizations performed in the specific component in reverse order. It means, disable optimizations starting from the last optimization step and backward. (We do not have the option list readily available at present. The work will start from scanning the driver of each backend components as the following):

| Component | Component driver source | Option definition location |
| --------- | ----------------------- | -------------------------- |
| BE        |                         |                            |
| CG        |                         |                            |
| WOPT      | be/opt/opt_main.cxx     | common/com/config_wopt.h   |
| LNO       |                         |                            |
| IPA       |                         |                            |

# Identify specific instance of optimization

Using SSAPRE as an example, the PRE driver can skip after a number of expressions have been processed. This option is not visible at command line today. However, it can be easily added back.

# Filing a bug report for optimization related runtime errors

A good bug report should include the following information:

1. The build environment which contains the following

   * The required source files

   * The build scripts

   * The compile scripts

   * The link scripts

2. The component that causes the failure, which should be stated by compile option

3.  The file and the procedure that introduce error by the optimization stated in #2

4. The last STABLE version that does not have the error, if it is a regression

5. The specific optimization that introduces the error

6. The summary line of the bug report must state what app, at what optimization level as stated in #2, and state the last working version if it is a regression.

# Automate the triage process

It should be a good tool to build to automate the triaging process as described above. Let’s try to articulate the set of requirements to build such tool.

 
