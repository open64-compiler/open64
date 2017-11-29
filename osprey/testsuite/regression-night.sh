#!/bin/bash

######################################################
#Script for testing the regression test and send email
######################################################
TEST_HOME=`pwd`
curdate="`date +%Y-%m-%d`"
TEST_LOG=$TEST_HOME/log/`date +%F`
HOME="/data/home01/test"

gen_report() {
cat << _EOF_
From: ${MAIL_SENDER}
To:   $2
Date: `date +"%a, %d %b %Y %H:%M:%S %z"`
X-Mailer: Osprey Test
Subject: Profiling result `date +%F`
Content-Type: text/html

<html>
<head>
        <title>Test Result for `date +%F`</title>
</head>
<body>
_EOF_

if [ -e $TEST_LOG/mp3-pro.log ]; then
cat << _EOF_
  </pre>
  <h3>mp3 summary</h3>
_EOF_
  cat $TEST_LOG/mp3-pro.log
fi

cat << _EOF_
  </body>
  </html>
_EOF_
}


sh $TEST_HOME/regression.sh
for mail in `cat $TEST_HOME/conf/mails.conf`
do
    cat $TEST_LOG/testsummary-$curdate | /usr/lib/sendmail $mail
done
