#!/bin/bash

# set -x

HASH=$1
TITLE=$2

REPO=/home/kyukhin/gcc/git/gcc
if [ -n "$3" ]; then REPO=$3; fi

TPATH=$PWD
if [ -n "$4" ]; then TPATH=$4; fi

STATUS=$TPATH/$$-$TITLE.status
TPATH=$TPATH/$$-$TITLE

echo "kyukhin ($$-$TITLE): Setting up..." |tee $STATUS
echo "kyukhin ($$-$TITLE): Working dir: $TPATH" |tee -a $STATUS
mkdir $TPATH || exit -1
mkdir $TPATH/src || exit -1
mkdir $TPATH/bld || exit -1
cp ~/bin/Makefile $TPATH/bld/Makefile ||exit -1

pushd $TPATH/src > /dev/null || exit -1
git clone $REPO gcc >> $STATUS || exit -1
cd gcc
git checkout $HASH >> $STATUS || exit -1

echo "kyukhin ($$-$TITLE): Bootstrapping..." |tee -a $STATUS
cd $TPATH/bld || exit -1

GCC_SOURCE=$TPATH/src/gcc time make config rr >> $STATUS.detail 2>&1
S=$?

echo "kyukhin ($$-$TITLE): STATUS is $S" |tee -a $STATUS

if [ $S -ne 0 ]
then
    echo "**** ($$-$TITLE): BOOTSTRAP FAILED. STOP STOP STOP. FAILED ****" |tee -a $STATUS
    popd
    exit $1
fi

echo "kyukhin ($$-$TITLE): Testing..." |tee -a $STATUS
cd $TPATH/bld || exit -1

GCC_SOURCE=$TPATH/src/gcc time make c >> $STATUS.detail 2>&1
S=$?

echo "kyukhin ($$-$TITLE): STATUS is $S" |tee -a $STATUS

if [ $S -ne 0 ]
then
  echo "**** ($$-$TITLE): TESTING FAILED. STOP STOP STOP. FAILED ****" |tee -a $STATUS
else
#  rm -rf $TPATH
#  rm $STATUS.detail
  echo "**** ($$-$TITLE): OK! ****" |tee -a $STATUS
  echo " Test results" |tee -a $STATUS
  tail -n16 $STATUS.detail |head -n7 |tee -a $STATUS
fi

popd > /dev/null
