#!/bin/sh

#  Copyright (C) 2019-2022 Xcalibyte (Shenzhen) Limited.
#
#  Licensed under the Apache License, Version 2.0 (the "License");
#  you may not use this file except in compliance with the License.
#  You may obtain a copy of the License at
#
#    http://www.apache.org/licenses/LICENSE-2.0
#
#  Unless required by applicable law or agreed to in writing, software
#  distributed under the License is distributed on an "AS IS" BASIS,
#  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#  See the License for the specific language governing permissions and
#  limitations under the License.

action="$1"
intrn="$2"

verbose=0

usage() {
  echo "Usage: $0 <action> <intrn_prefix>"
  echo "  <action>: gen, list, install or clean"
  echo ""
  echo "  gen: generate files to declare c++ intrinsics"
  echo "    make sure <intrn_prefix>.cxx and <intrn_prefix>.filter exists in current directory"
  echo ""
  echo "  list: generate files to declare c++ intrinics and list all template functions/class into a file"
  echo "    make sure <intrn_prefix>.cxx exists in current directory"
  echo ""
  echo "  install: copy files with given <intrn_prefix> to right location in xvsa source tree"
  echo "    a prompt will be asked if file already exists in same location"
}

if [ "$action" = "clean" ]; then
  rm -f intrn_entry_cxx*.def
  rm -f c2w_intrn_cxx*.inc
  rm -f rbc_intrn_cxx*.inc
  rm -f verbose_*.list
  rm -f xvsa.log
  exit
fi

if [ "$action" != "gen" -a "$action" != "list" -a "$action" != "install" ]; then
  echo "Error: wrong action \"$action\""
  usage
  exit
fi

if [ -z "$intrn" ]; then
  echo "Error: <intrn_prefix> is not specified"
  usage
  exit
fi

if [ "$action" = "list" -a ! -f ${intrn}.filter ]; then
  echo "" > ${intrn}.filter
fi

if [ ! -f "${intrn}.cxx" -o ! -f "${intrn}.filter" ]; then
  echo "Error: not find ${intrn}.cxx or ${intrn}.filter"
  usage
  exit
fi

gen_cxx_intrn() {
  xvsa -sw -kp -clang -c -noinline -Wf,-mllvm,-verbose=${verbose},-mllvm,-gen-cpp-intrn=1,-mllvm,-gen-cpp-intrn-prefix=${intrn},-mllvm,-gen-cpp-intrn-filter=@${intrn}.filter ${intrn}.cxx > xvsa.log 2>&1
  ret=$?
  if [ $ret -ne 0 ]; then
    echo "Genreate c++ intrinsic for $intrn failed. Please check xvsa.log."
  fi
  rm -f ${intrn}.o
}

check_directory() {
  dir=$1
  if [ ! -d "$dir" ]; then
    echo "Not find ${dir}, make sure $0 is run from current working directory."
    exit
  fi
}

check_source() {
  src=$1
  if [ ! -f "$src" ]; then
    echo "Not find ${src}, install failed"
    exit
  fi
}

check_dest() {
  dst=$1
  if [ -f "$dst" ]; then
    fn=`basename $dst`
    dir=`dirname $dst`
    echo -n "File \`$fn' already exists in \`$dir'. Overwrite it(y/n)? "
    read ans
    if [ "$ans" = 'Y' -o "$ans" = 'y' ]; then
      return;
    fi
    echo "abort installation."
    exit
  fi
}

install_file() {
  cp -v $1 $2
  [ $? -ne 0 ] && echo "copy $1 failed. Please check if $2 is writable"
}

update_file() {
  grep $1 $2 >/dev/null
  [ $? -eq 0 ] && echo "File $1 has been included by $2." && return
  sed -i -e "s/\/\* DO NOT EDIT THIS MARK \*\//#include \"$1\"\n\/* DO NOT EDIT THIS MARK *\//g" $2
  echo "Updated $2 to include $1. Please double check."
}

install_cxx_intrn() {
  check_directory ../../../common/com
  check_directory ../../../clang2whirl
  check_directory ../cxx_intent
  check_source    ../../../common/com/intrn_entry.def
  check_source    ../../../clang2whirl/c2w_cxx_intrn.cxx
  check_source    ../cxx_intent/rbc_cxx_intrn.cxx

  check_source intrn_entry_cxx_${intrn}.def
  check_source c2w_intrn_cxx_${intrn}.inc
  check_source rbc_intrn_cxx_${intrn}.inc
  check_dest   ../../../common/com/intrn_entry_cxx_${intrn}.def
  check_dest   ../../../clang2whirl/c2w_intrn_cxx_${intrn}.inc
  check_dest   ../cxx_intent/rbc_intrn_cxx_${intrn}.inc

  install_file intrn_entry_cxx_${intrn}.def ../../../common/com/intrn_entry_cxx_${intrn}.def
  update_file  intrn_entry_cxx_${intrn}.def ../../../common/com/intrn_entry_cxx.def
  install_file c2w_intrn_cxx_${intrn}.inc ../../../clang2whirl/c2w_intrn_cxx_${intrn}.inc
  update_file  c2w_intrn_cxx_${intrn}.inc ../../../clang2whirl/c2w_cxx_intrn.cxx
  install_file rbc_intrn_cxx_${intrn}.inc ../cxx_intent/rbc_intrn_cxx_${intrn}.inc
  update_file  rbc_intrn_cxx_${intrn}.inc ../cxx_intent/rbc_cxx_intrn.cxx
}

case "$action" in
"gen" )
  gen_cxx_intrn
  ;;
"install" )
  install_cxx_intrn
  ;;
"list" )
  verbose=1
  gen_cxx_intrn
  ;;
*)
  ;;
esac

