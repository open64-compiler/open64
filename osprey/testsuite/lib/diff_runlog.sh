#!/bin/bash
if [ -f $1 -a -f $2 ]
then
  file $1 | awk -F: '{print $2}' | grep "ASCII" > /dev/null
  if [ $? -eq 0 ]
  then
    grep -v -e SimpLight\
            -e Machine\
            -e Warn\
            -e "---"\
            -e syscall\
            -e PC\
            -e Loading\
            -e Clear\
            -e SP\
            $1 > $1.1
    grep -v -e SimpLight\
            -e Machine\
            -e Warn\
            -e "---"\
            -e syscall\
            -e PC\
            -e Loading\
            -e Clear\
            -e SP\
            $2 > $1.2
  else
    cat $1 > $1.1
    cat $2 > $1.2
  fi
  diff $1.1 $1.2 > /dev/null
else
  exit 1
fi
