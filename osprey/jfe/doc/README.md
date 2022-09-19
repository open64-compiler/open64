### 1. build jfe

build and install mastiff, jfe will be built and installed

install dependency

``` shell
	sudo apt install git flex bison openjdk-8-jdk gradle cmake gcc-5 gcc-5-multilib g++-5 g++-5-multilib
```

build and install

``` shell
	# user name : xc5
	cd /home/xc5
	git clone --recursive http://git.xc5.io/git/xc5-sz/mastiff.git
	mkdir mastiff-build mastiff-install
	cd mastiff-build
	/home/xc5/mastiff/configure --host=x86_64-linux-gnu --target=x86_64-linux-gnu --with-build-optimize=DEBUG --disable-multilib --prefix=/home/xc5/mastiff-install
	make
	make install
```

#### 2. run jfe

run jfe with xvsa
the input can be a class file or a jar package

``` shell
	# test case located at /home/xc5/test/Test.java
	cd /home/xc5/test
	javac Test.java
	/home/xc5/mastiff-install/bin/xvsa -show -keep -c Test.class
	# build android apk
	xvsa -kp -Wf,-android-jars=/../android_platforms -Wf,-noSootOutput=true -Wf,-v -Wb,-OPT:alias=nofield_sensitive -Wf,-skip-soot-error=true -Wf,-sootOutputDir=/tmp/dTM -noxfa test.apk
```

### 3. use jfe to generate executable file

jfe can be used to generate executable, if your code can be compiled by gcj6; gcj6 follow the 6th edition of java language specification

``` shell
	cd /home/xc5
	git clone http://git.xc5.io/git/xc5-sz/cti_testware_libs.git
	export GCC6_HOME=/home/xc5/cti_testware_libs/libgcj
	export XVSA_HOME=/home/xc5/mastiff-install
	cd /home/xc5/test
	javac Test.java
	# make sure there is the main method in Test.class
	/home/xc5/cti_testware_libs/libgcj/bin/openjava.py --main=Test Test.class
	./Test.out
```

### 4. Use JFE to generate a v-table full list for runtime.jar

- Following command works for JDK-8u212

```bash
export XVSA_HOME="/path/to/install"
java -Xms2048m -Xmx5000m -Djava.library.path=${XVSA_HOME}/lib/1.0 -ea -jar ${XVSA_HOME}/lib/1.0/macbcr.jar -noSootOutput=true -VTABLE=true -cp=/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/jsse.jar -cp=/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/charsets.jar -fC,/usr/lib/jvm/java-8-openjdk-amd64/jre/lib/rt.jar -fB,rt.B
```
