#!/bin/bash

######################################################
#Script for testing the regression test and send email
######################################################
TEST_HOME=`pwd`
TEST_SCRIPT=$TEST_HOME/bin
CONF_DIR=$TEST_HOME/conf
TEST_LOG=$TEST_HOME/log/`date +%F`
MAIL_SENDER=${USER}@${HOSTNAME}
curdate="`date +%Y-%m-%d`"

gen_report() {
cat << _EOF_
From: ${MAIL_SENDER}
To:   $2
Date: `date +"%a, %d %b %Y %H:%M:%S %z"`
X-Mailer: Osprey Test
Subject: Regression Test Result `date +%F`
Content-Type: text/html

<html>
<head>
	<title>Test Result for `date +%F`</title>
</head>
<body>
_EOF_

cat << _EOF_
The Configuration for this test:<br>
Test Machine: `hostname`<br>
Test Account: `whoami`<br>
_EOF_


if [ -e $TEST_LOG/testrun.log ]; then
cat << _EOF_
        </pre>
        <h3>Test summary</h3>
_EOF_
        grep "# of*" $TEST_LOG/testrun.log > $TEST_LOG/tmp.log
        cat $TEST_LOG/tmp.log
fi

cat << _EOF_
        </pre>
        <h5>Test Case:</h5>
_EOF_
if [ -e $TEST_LOG/single_source_cases.log ]; then
	cat $TEST_LOG/single_source_cases.log
fi

if [ -e $TEST_LOG/cern.log ]; then
        cat $TEST_LOG/cern.log
fi

cat << _EOF_
        </pre>
        <h5> Build Status:</h5>
_EOF_
if [ -e $TEST_LOG/buildstatus.log ]; then
       cat $TEST_LOG/buildstatus.log
       cat << _EOF_
       <br>
_EOF_
fi

if [ -d $TEST_LOG ]; then
        cat << _EOF_
        <P><A HREF=http://london/$curdate/>$curdate regression test log link</A></P>
        <pre>
_EOF_
fi

cat << _EOF_
  </body>
  </html>
_EOF_

}

runtest -pr 
sed "/do_cmd_succeed/d" testrun.log > $TEST_LOG/testrun.log
#cd /data/home01/test/fpga-test
#sh exe.sh
cd $TEST_HOME
gen_report $mail > $TEST_LOG/testsummary-$curdate 
