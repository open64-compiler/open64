#!/bin/bash

JFEDIR=$(dirname $(realpath $0))/../..
echo $JFEDIR

$JFEDIR/B2WFrontEnd/gradlew build
if [[ $? -eq 0 ]]
then
    echo "gradlew build finished with success"
else
    echo "gradlew build failed with : $?"
fi


JFEJAR=$(echo $JFEDIR/B2WFrontEnd/build/libs/b2wfrontend.jar)
if test -f $JFEJAR
then  
    echo "-- found JFEJAR: $JFEJAR "
else
    echo "-- cannot locate JFE($JFEDIR), "
    echo "-- cannot locate jar at $JFEJAR "
    exit 2
fi

TCFILE=$JFEDIR/B2WFrontEnd/sanity/OneVar.class
TBFILE=$JFEDIR/B2WFrontEnd/sanity/test.B

if test -f $TCFILE
then
    echo "-- found OneVar.class at $TCFILE"
else
    echo "-- cannot locate TCFILE at $TCFILE, please check"
    exit 2
fi

if test -f $TBFILE
then
   echo "-- removing old $TBFILE"
   rm $TBFILE
fi


PATH_RT_DIR=$(dirname $PATH_RT_DIR)
CLPATH=$PATH_RT_DIR/rt.jar
if test -f $CLPATH
then
    echo "-- found rt.jar at $CLPATH"
else
    echo "-- cannot locate rt.jar at $CLPATH"
    echo "Please set PATH_RT_DIR using "
    echo " export PATH_RT_DIR='/path/to/rt.jar' "
    exit 2
fi


echo "-- Running --"
echo ""$(echo java -ea -Djava.library.path=$JFEDIR/libb2w/cmake-build-debug/libb2w  -jar $JFEJAR -fC,$TCFILE -fB,$TBFILE) -fbootclasspath=$CLPATH
java -Djava.library.path=$JFEDIR/libb2w/cmake-build-debug/libb2w -jar $JFEJAR -fC,$TCFILE -fB,$TBFILE  -fbootclasspath=$CLPATH > $JFEDIR/B2WFrontEnd/sanity/javarun.out
echo "--------"
echo "-******-"

if [[ $? -eq 0 ]]
then
    echo "-- java -jar runnning success."
else
    echo "-******-"
    echo "-------------------------------------------------------------"
    echo "-- java -jar running failed!"
    echo "-------------------------------------------------------------"
    echo "Please rerun "
    echo "-------------------------------"
    echo $(echo java -ea -Djava.library.path=$JFEDIR/libb2w/cmake-build-debug/libb2w  -jar $JFEJAR -fC,$TCFILE -fB,$TBFILE) -fbootclasspath=$CLPATH
    java -Djava.library.path=$JFEDIR/libb2w/cmake-build-debug/libb2w -jar $JFEJAR -fC,$TCFILE -fB,$TBFILE  -fbootclasspath=$CLPATH
    echo "-------------------------------------------------------------"
    exit 2
fi

echo "--------------------------------"
echo "-- all sanity check passed.   --"
echo "-- continue to commit & push  --"
echo "--------------------------------"
   
