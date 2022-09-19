#!/bin/bash
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

SCRIPT_DIR=$(dirname $(realpath $0))
PRE_DIR=$(pwd)
cd ${SCRIPT_DIR}
./gradlew classes 2>&1
if [[ $? -eq 1 ]]
then
    echo "gradle build failed, run: cd $(SCRIPT_DIR) && ./gradlew classes"
    exit 2 
fi    

if [ -d ./build/classes/java/main ]
then 
    cd build/classes/java/main
else
    echo "class folder not exists, folder : build/classes/java/main"
    exit 1
fi

# Javah is only for Java 8
JAVA_VERSION=$(javah -version|egrep -o "[0-9]\.[0-9]{1,2}")
if [ "${JAVA_VERSION}" == "1.8" ]
then
    javah io.xcalibyte.BGenDriver
    if [ $? != 0 ]
    then 
	    echo "javah failed, run : javah io.xcalibyte.BGenDriver"
        exit 1
    fi
else
    echo "should use java8, java version : ${JAVA_VERSION}"
    exit 2
fi

rm -f ${SCRIPT_DIR}/../libb2w/libb2w/io_xcalibyte_BGenDriver.h
cp ./io_xcalibyte_BGenDriver.h ${SCRIPT_DIR}/../libb2w/libb2w/io_xcalibyte_BGenDriver.h
if [ $? == 0 ]
then
    echo
    echo "============================"
    echo "Generate jni header file ok."
    echo "============================"
else
    echo "mv fialed."
    exit 1
fi