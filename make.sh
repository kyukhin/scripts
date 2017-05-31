#!/bin/bash

no_quark=0
no_objdump=0
filter_prefetch=0
no_compile=0
no_link=0

scripts_p=~/scripts
bin_p=~/bin

nucom_path=~/nucleus/bin/linux.i686
nucom_scripts_path=~/nucleus/scripts/linker

nucom_file=nucom
file=$1
shift
insmn=
trace_regs=
no_opaque=-nq
com_asm=-ca

while [ "$1" != "" ]; do
    case $1 in
        -nq | --no-quark )        no_quark=1
                                  ;;
        -nd | --no-objdump )      no_objdump=1
                                  ;;
        --filter-prefetch )       filter_prefetch=1
                                  ;;
        -nc | --no-compile )      no_compile=1
                                  ;;
        -nl | --no-link )         no_link=1
                                  ;;
        --use-nucomd )            nucom_file=nucomd
                                  ;;
        -ni | --no-insmn )        insmn=-ni
                                  ;;
        --trace-regs )            trace_regs="-sra -srb"
                                  ;;
        --opaque )                no_opaque=
                                  ;;
        -nca | --no-comment-asm ) com_asm=
                                  ;;
        -h | --help )             echo "-nq | --no-quark"
                                  echo "-nd | --no-objdump"
                                  echo "--filter-prefecth"
                                  echo "-nc | --no-compile"
                                  echo "-nl | --no-link"
                                  echo "--use-nucomd"
                                  echo "-ni | --no-insmn"
                                  echo "--trace-regs"
                                  echo "--opaque"
                                  echo "-nca | --no-comment-asm"
                                  echo "-h | --help"
                                  exit
                                  ;;
        * )                       echo warning: unknown option $1
                                  ;;
    esac
    shift
done

if [ ${no_objdump} -ne "1" ]
then
  if [ -f ${file}.dump ] 
  then
    echo rm ${file}.dump
         rm ${file}.dump
  fi

  echo objdump -d ${file} 
       objdump -d ${file} > ${file}.dump
fi


if [ ${no_quark} -ne 1 ]
then
  if [ -f ${file}.xml.patched.xml ] 
  then
    echo rm ${file}.xml.patched.xml
         rm ${file}.xml.patched.xml
  fi

  if [ -f ${file}.xml ] 
  then
    echo rm ${file}.xml
         rm ${file}.xml
  fi

  echo ${bin_p}/quark ${file} -xml0
       ${bin_p}/quark ${file} -xml0

  echo mv result.xml ${file}.xml
       mv result.xml ${file}.xml

  echo ${scripts_p}/patch_xml.pl ${file}.xml ${file}.dump
       ${scripts_p}/patch_xml.pl ${file}.xml ${file}.dump
fi

if [ ${no_compile} -ne 1 ]
then
  if [ -f ${file}.S ] 
  then
    echo rm ${file}.S
         rm ${file}.S
  fi

  echo "${nucom_path}/${nucom_file} -p ${file}.xml.patched.xml ./${file} ${com_asm} ${no_opaque} ${insmn} ${trace_regs} >/dev/null "
       ${nucom_path}/${nucom_file} -p ${file}.xml.patched.xml ./${file} ${com_asm} ${no_opaque} ${insmn} ${trace_regs} > /dev/null
fi

if [ ${filter_prefetch} -eq 1 ]
then
  echo sed s/prefetchnta.*$// ${file}.S > ${file}.S.2
       sed s/prefetchnta.*$// ${file}.S > ${file}.S.2

  echo mv ${file}.S.2 ${file}.S
       mv ${file}.S.2 ${file}.S
fi

if [ ${no_link} -ne 1 ]
then
  if [ -f ${file}.out ] 
  then
    echo rm ${file}.out
         rm ${file}.out
  fi

  echo gcc ${file}.S -o ${file}.out -g -L ${nucom_path} -lnu_s -Wl,-rpath ${nucom_path} -g -Wl,-T ${nucom_scripts_path}/elf32_i386.x;
       gcc ${file}.S -o ${file}.out -g -L ${nucom_path} -lnu_s -Wl,-rpath ${nucom_path} -g -Wl,-T ${nucom_scripts_path}/elf32_i386.x 2>log;
fi
