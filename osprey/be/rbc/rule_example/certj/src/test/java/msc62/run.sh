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

javac -g *.java
XVSA_HOME=`which xvsa`
BIN_PATH=`dirname $XVSA_HOME`
LIB_PATH=$BIN_PATH/../lib/1.0/
RT_OBJ=$1
if [ -z $1 ]
then
  echo "please specify rt.o location"
  echo "$0 <rt.o>"
  exit
fi
xvsa -VSA:certj=1  -noinline *.class   $1  $LIB_PATH/librulex.a
