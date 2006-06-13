#!/bin/bash
# prepare.sh: 
# moves experiment files to the appropriate Public directory 
# so experimenters can access them
# 
# $LastChangedDate$

EXPERIMENT=StopTrack07
PUBLIC_DIR=~/Public/Experiments/$EXPERIMENT

GENERATOR_IN=generator2.m
TRACK_IN=track.m
GENERATOR_OUT=${EXPERIMENT}Gen.m
TRACK_OUT=${EXPERIMENT}.m

PATH_FILES="*.mat"

if [ ! -d $PUBLIC_DIR ]; then
    mkdir -p $PUBLIC_DIR
fi

if [ -x $GENERATOR_IN ]; then
    echo "File $GENERATOR_IN not found"
    exit -1
fi
if [ -x $TRACK_IN ]; then
    echo "File $TRACK_IN not found"
    exit -1
fi

if [ $GENERATOR_IN -nt $PUBLIC_DIR/$GENERATOR_OUT ]; then
    echo cp -a $GENERATOR_IN $PUBLIC_DIR/$GENERATOR_OUT
    cp -a $GENERATOR_IN $PUBLIC_DIR/$GENERATOR_OUT
fi

if [ $TRACK_IN -nt $PUBLIC_DIR/$TRACK_OUT ]; then
    echo cp -a $TRACK_IN $PUBLIC_DIR/$TRACK_OUT
    cp -a $TRACK_IN $PUBLIC_DIR/$TRACK_OUT
fi

for f in $(/bin/ls $PATH_FILES); do
    if [ $f -nt $PUBLIC_DIR/$f ]; then
	echo cp -a $f $PUBLIC_DIR/
	cp -a $f $PUBLIC_DIR/
    fi
done
