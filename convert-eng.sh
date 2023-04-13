#!/bin/bash

# Let's make some parsing
# Option strings
SHORT=hta:s:o:iq:f:yg
LONG=help,test,astream:,substream:,output:,info,quality:,file:,yes,guess

# Read the options
OPTS=$(getopt --options $SHORT --longoptions $LONG --name "$0" -- "$@")

if [ $? != 0 ] ; then echo "Failed to parse options...exiting." >&2 ; exit 1 ; fi

eval set -- "$OPTS"

# Defaults
INFO=false
TEST=false
OUTPUT=$PWD
TWO_PASS=false
VBITRATE=1M
FILE=
EXEC=true
OVERRIDE=-n
GUESS=false

# extract options and their arguments into variables.
while true ; do
  case "$1" in
    -t | --test )
	TEST=true
	shift
	;;
    -a | --astream )
	ASTREAM=$2
	shift 2
	;;
    -f | --file )
	FILE=$2
	EXEC=false
	shift 2
	;;
    -g | --guess )
	GUESS=true
	shift
	;;
    -o | --output )
	OUTPUT=$2
	shift 2
	;;
    -s | --substream )
	SSTREAM=$2
	shift 2
	;;
    -q | --quality )
	TWO_PASS=true
	VBITRATE=$2
	shift 2
	;;
    -i | --info )
	INFO=true
	shift
	;;
    -h |--help )
	# TODO
	echo "Usage:"
	echo "-t|--test Only run for first file"
	shift
	;;
    -y | --yes )
	OVERRIDE=-y
	shift
	;;
    -- )
	shift
	break
	;;
    *)
      echo "Internal error!"
      exit 1
      ;;
  esac
done

# Dump config vars
echo "TEST=$TEST"
echo "ASTREAM=$ASTREAM"
echo "SSTREAM=$SSTREAM"
echo "OUTPUT=$OUTPUT"
echo "INFO=$INFO"
echo "TWO_PASS=$TWO_PASS"
echo "VBITRATE=$VBITRATE"
echo "FILE=$FILE"
echo "EXEC=$EXEC"
echo "OVERRIDE=$OVERRIDE"
echo "GUESS=$GUESS"

# Subtitles in separate file (so called filter): -vf subtitles="${i%.}.srt"
# ... in the same file:                          -vf subtitles="$i"
# ... ................ second stream:            -vf subtitles="$i":si=1
# Use video, usually 0-channel, usually:         -vcodec libx264 -map 0:0
# Select audio, do not convert:                  -acodec copy -map 0:3
# Select input:                                  -i $i
#
# KNOWN PROBLEM WITH SUBTITLES:
# It cannot open files with ' or , in the name. To fix, just rename the file.
# This can be observed as converted file of zero size.
#
#
# H264, use 2-pass encoding:
#   ffmpeg -y -i $i -c:v libx264 -b:v $VBITRATE -pass 1 -an -f null /dev/null
#   ffmpeg -y -i $i -c:v libx264 -b:v $VBITRATE -pass 2 ...

# Bash tweaks:
# - walk through all dirs recursively selecting videos (e.g. across seasons)
#   for i in **/*.mkv ; do echo "$i" ; done
#
# - Now, replace file extention (for subtitles or for output):
#   for i in **/*.mkv ; do echo "$i" "${i%.*}.srt" "${i%.*}.mp4" ; done
#
# Remove special chars from filename recursively
#   for i in **/*.mkv ; do mv -v $i $(echo "$i" | tr , _); done
#   for i in **/*.mkv ; do mv -v $i $(echo "$i" | tr ' _); done

for i in ./**/*.mkv; do
    if [[ ! -z $FILE && $i =~ $FILE ]] ; then
	echo $i
	EXEC=true
    fi

    if [ "$EXEC" = true ] ; then
	if [ "$GUESS" = true ] ; then
	    SSTREAM=`ffprobe -show_entries stream=index,codec_type:stream_tags=language -of json -i "$i" -v panic -select_streams s |jq '[.streams[].tags.language=="eng"]|index(true)'`

	    if [ "$SSTREAM" = "null" ] ; then
		echo "error: Cannot find subtitles."
		exit 1
	    fi
	    echo "Detected eng. subs SSTREAM=$SSTREAM"

	    ASTREAM=0:`ffprobe -show_entries stream=index,codec_type:stream_tags=language -of json -i "$i" -v panic -select_streams a |jq '[.streams[] | select(.tags.language=="eng").index][0]'`

	    if [ "$ASTREAM" = "0:null" ] ; then
		echo "error: Cannot find audio track."
		exit 1
	    fi
	    echo "Detected eng. audio ASTREAM=$ASTREAM"
	fi

	if [ "$INFO" = true ] ; then
	    ffprobe -i "$i"
	else
	    f=$(basename -- "$i")
	    set -x
	    if [ "$TWO_PASS" = true ] ; then
		ffmpeg $OVERRIDE -i "$i" -c:v libx264 -b:v $VBITRATE -maxrate $VBITRATE -map 0:0 -pass 1 -an -f null /dev/null
		ffmpeg $OVERRIDE -i "$i" -c:v libx264 -b:v $VBITRATE -maxrate $VBITRATE -map 0:0 -pass 2 -vf subtitles="$i:si=$SSTREAM" -acodec copy -map $ASTREAM "$OUTPUT/${f%.*}.mp4"
	    else
		ffmpeg $OVERRIDE -i "$i" -c:v libx264 -preset slower -crf 17 -map 0:0 -vf subtitles="$i:si=$SSTREAM" -acodec copy -map $ASTREAM "$OUTPUT/${f%.*}.mp4"
	    fi
	    set +x
	fi
    fi

    if [[ ! -z $FILE && $i =~ $FILE ]] ; then
	EXEC=false
	exit 0
    fi

    if [ "$TEST" = true ] ; then
	exit 0
    fi
done
