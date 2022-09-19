# Java Rule Based Checker

Java RBC(Rule based checker) is a part of XVSA that targets Java.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.


### Prerequisites

Basically, you will need XVSA and gradle, Java Development Kit 8.

Running the following command-line on Ubuntu 16.04.

```
sudo apt install gradle openjdk-8-jdk
```


For developers of XVSA, it is recommended to also inlcude:

```bash
sudo gcc-5.0 gcj-5.0 
```

### Running The Checker

A step by step series of instructions of installing XVSA is given elsewhere, depending on which version you are installing

Say that you have `lib`, `bin`, folder ready in `${XVSA_HOME}`

```bash
export XVSA_HOME="/path/to/xvsa_install_folder"
```

And then, start cloning this project in java_vul_rule folder with : 

if you have just a simple testcase, i.e. (sample.class) ready (compiled from sample.java)

```bash
git clone http://git.xc5.io/git/xc5-sz/java_vul_rule 
./java_vul_rule/scripts/run.sh sample.class
```

Results are available in 

```bash
./java_vul_rule/autogen
```

This will perform a test on sample.class (with rules in java_vul_rule)


## 2. Runner Synopsis


Rules are currently based on CERT-Java and are organized according to 

## Running the tests

*Using the runner* 

```bash
/path/to/java_vul_rule/scripts/run.sh [class1.class] [class2.class] [class3.class] 
... 
[-skip] [-keep] [-no-gradle] 
```

#### -skip 

this is used for skipping failed compilation of class files (for both testcases and rules)

#### -keep

keeping the results (basically all `.o`, `.s`, `.I`, `.B`, `.v`, `.t`) files  
note that this option will ***NOT*** hinder for the clean up when compiling inside each testcase folder  

> which means, with `-keep` enabled

i.e. there is a case in `/path/a/d.class`  
there is the scripts in `/path/b/java_vul_rule/scripts`
+ the `.B`/`.I`/`.o`/`.v`/`.t` files will be cleaned in `/path/a/`
+ but no files will be cleaned  in `/path/e/java_vul_rule`

#### -no-gradle

Because the runner will first try to build the rules with gradle,   
You may try to use -no-gradle to disable this process.

### Break down into end to end tests

Explain what these tests test and why

```
Give an example
```

### And coding style tests

Explain what these tests test and why

```
Give an example
```

## Deployment

Add additional notes about how to deploy this on a live system

You should start using by 

## Built With

* [Gradle](http://gradle.org/) - Dependency Management 
* [Openjdk-8](https://openjdk.java.net/) - Java Runtime Library we are targeting
* [XVSA](https://git.xc5.io/git/xc5-sz/mastiff) - XVSA (XVSA as installed) with `lib`, `bin`, `include` ready

## Contributing

Please make sure that gradle test is running just fine before pushing to master branch.

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Versioning

We use [SemVer](http://semver.org/) for versioning. For the versions available, see the [tags on this repository](https://github.com/your/project/tags). 

## Authors

* **Many Authors** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

See also the list of [contributors](https://github.com/your/project/contributors) who participated in this project.

## License

This project is licensed under the Xcalibyte License - see the [LICENSE.md](LICENSE.md) file for details

## Acknowledgments

* Java Front End Team Members @ SZ & SH
