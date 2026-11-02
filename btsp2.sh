#!/bin/bash

# set -x

trap 'set -x; trap - SIGTERM SIGINT; kill 0' SIGINT SIGTERM
# kill $job_list ;

HASH=$1
TITLE=$2
REPO=$3
HERE=$(cd "$(dirname "$0")" && pwd)

RH=$HASH~1
RT=$TITLE-ref
$HERE/btsp.sh $RH $RT "$REPO" $PWD &
Rpid=$!
RP=$PWD/$Rpid-$RT

EH=$HASH
ET=$TITLE-exp
$HERE/btsp.sh $EH $ET "$REPO" $PWD &
Epid=$!
EP=$PWD/$Epid-$ET

echo $RH $RT $RP
echo $EH $ET $EP

echo "Waiting for ref(pid:$Rpid) and exp(pid:$Epid) to finish..."
job_list=`jobs -p`
wait

$RP/src/gcc/contrib/compare_tests $RP/bld $EP/bld
