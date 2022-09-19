 echo "--------------------------------------------------"
echo "01110101010111010101011101010101110555555555555555"
echo "01110101010111010101011101010101110555555555555555"
echo "01110     101110     1011101           55555555555"
echo "0111010     11      1010      01010111010101055555"
echo "011101010          0101    10101111110111115555555"
echo "011101010        010101     1010101010110101155555"
echo "01110101    0      0101     0101010111010101555555"
echo "011101      0111    010       0101011   5555555555"
echo "01110      101110      10              55555555555"
echo "01110101010111010101011101010101110101010155555555"
echo "01101010111010101011100101011101010101110555555555"
echo "01110111010101111111110101011111010001010555555555"
echo "--------------------------------------------------"

REALPATH="realpath"
which realpath && FERNDIR=$(dirname $($REALPATH $0)) || FERNDIR=$(pwd)
cd $FERNDIR   

echo "=========================================="
echo " Running gradle classes"
echo "=========================================="

#gradle classes 2>&1 | grep FAILED
gradle classes 2>&1
if [[ $? -eq 1 ]]
then
    echo "=========================================="
    echo "[ERROR] Gradle Running failed ! "
    echo "[ERROR] please make sure gradle is runnable"
    echo "[ERROR] try run"
    echo "******   gradle classes   *******"
    echo "[ERROR] and then try to run $0 again      "
    echo "=========================================="
    exit 2 
else
    echo "checking whether gradle classes  ...  ok "    
fi    

if [ -d ./build/classes/main ]
then 
    cd build/classes/main
    VSM=""
else
    cd build/classes/java/main
    VSM="../"
fi    
echo "Now at : "$(pwd)
# Javah is only for Java 8
javah -version 1>/dev/null 2>&1
if [ $? -eq 0 ]
then
    echo "checking whether javah is present ... yes"
    echo "==========================================="
    echo "   Running JAVAH "
    echo "==========================================="
    echo "javah org.jetbrains.java.decompiler.modules.bgen.BGenDriver"
    javah org.jetbrains.java.decompiler.modules.bgen.BGenDriver
    if [ $? -eq 0 ]
    then 
	  echo " JavaH failed somehow, ignoring! " 
    else
	echo " javah generation .... ok"
    fi
elif [ "A$(javac -version | grep 10.0)" != "A" ]
then     
    echo "checking whether javah is present ... javac -h"
    echo "==========================================="
    echo "   Running JAVAC -h "
    echo "==========================================="
    javac -h . $FERNDIR/src/org/jetbrains/java/decompiler/modules/bgen/BGenDriver.java || echo " JavaH failed somehow, quitting! " && exit 3
    exit 4
else
    echo "checking whether javah is present ... no  (in PATH)"
    echo " ----------- Please Install Javah (JDK 8) ------------"
    exit 2
fi

echo "==========================================="
echo "   Running COPY to libb2w  "
echo "==========================================="
pwd
cp ./org*.h ../$VSM../../../libb2w/
echo "==========================================="
echo "    Entering outter libb2w dir "
cd ../$VSM../../../libb2w
echo "  Now, at : "$(pwd)
echo "==========================================="
echo "    Locating Origin, now files"
FILLLEA=$(echo org_jetbrains_java_decompiler_modules_bgen_BGenDriver.h)
FILLLEB=$(echo ./libb2w/org_jetbrains_java_decompiler_modules_bgen_BGenDriver.h)
if [ -e $FILLLEA ]
then
   echo "File A : $FILLLEA"
fi
if [ -e $FILLLEB ]
then   
   echo "File B : $FILLLEB"
fi
echo " at "$(pwd)
echo "==========================================="
echo "    Calculating Diff ... "
diff $FILLLEA $FILLLEB > diff.log
echo "    ----------------------------           "
DIFFRESULT=$(cat diff.log)
wc diff.log
echo ""
echo $DIFFRESULT
echo $(echo $DIFFRESULT | wc)
echo "==========================================="
echo "   Moving the org.h to inner libb2w "
echo "==========================================="
mv ./org*.h ./libb2w/
echo "==========================================="
echo "   Using git to compare with the last commit"
git diff $FILLLEB > git.diff.log
echo "   -----"
cat org_jni.log
echo "==========================================="
echo "   Moved to libb2w/libb2w !"
echo "  Please check the org_jni.log inside libb2w to see the difference"
echo "  "
echo "==========================================="
