#!/bin/sh

# check the intrn_entry.def to find out possible duplicated intrn IDs
#  1. Pre-Process the def file, using -DTARG_IA64, -DTARG_X8664, -DTARG_SL, -DTARG_NVISA respectively
#  2. Trim all empty lines
#  3. Run sort and uniq -c, check if there are duplicated intrn IDs

TARGS="TARG_IA64 TARG_X8664 TARG_SL TARG_NVISA"

INTRN_FILE=intrn_entry.def

check_intro_entry() {
  # $1 is the target
  TMP_FILE=/tmp/check_intrn_dup_$1.txt
  # Preprocess, Trim emtry lines, Sort, Uniq -c, Check Dup lines
  gcc -E -x c -D$1 -DNEED_INTRN_ID $INTRN_FILE | sed -e '/^\ *$/d' -e '/^#/d' | sort | uniq -c | grep -v '^\ *1' >$TMP_FILE

  if [ $? -ne -0 ] ; then
    echo "Check INTRN ID for $1 ... PASS"
    rm -f $TMP_FILE
    return 0
  fi
  echo "Check INTRN ID for $1 ... FAILED:" | tee $TMP_FILE.tmp
  cat $TMP_FILE | awk -F' ' '{ print ">> " $2 " appears " $1 " times!" }' | tee -a $TMP_FILE.tmp
  mv -f $TMP_FILE.tmp $TMP_FILE
  echo "The duplicated IDs are saved in $TMP_FILE"
  return 1
} 

for TARG in $TARGS; do
  check_intro_entry $TARG
done

