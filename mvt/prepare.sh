#!/bin/bash
# prepare.sh: 
# moves experiment files to the appropriate directory so the RAs can access them
# 
# $Id: prepare.sh,v 1.4 2004/06/01 19:47:32 fencsik Exp $

PUBLIC_DIR=~/Public/Experiments/StopTrack3

GENERATOR_IN=generator.m
TRACK_IN=track.m
TRAIN_IN=train.m
GENERATOR_OUT=StopTrack3Gen.m
TRACK_OUT=StopTrack3.m
TRAIN_OUT=StopTrack3Train.m

PATH_FILES="*.mat"

if [ ! -d $PUBLIC_DIR ]; then
    mkdir $PUBLIC_DIR
fi

if [ -x $GENERATOR_IN ]; then
    echo "File $GENERATOR_IN not found"
    exit -1
fi
if [ -x $TRACK_IN ]; then
    echo "File $TRACK_IN not found"
    exit -1
fi
if [ -x $TRAIN_IN ]; then
    echo "File $TRAIN_IN not found"
    exit -1
fi

if [ -f $GENERATOR_IN ]; then
    echo cp -a $GENERATOR_IN $PUBLIC_DIR/$GENERATOR_OUT
    cp -a $GENERATOR_IN $PUBLIC_DIR/$GENERATOR_OUT
fi

if [ -f $TRACK_IN ]; then
    echo cp -a $TRACK_IN $PUBLIC_DIR/$TRACK_OUT
    cp -a $TRACK_IN $PUBLIC_DIR/$TRACK_OUT
fi

if [ -f $TRAIN_IN ]; then
    echo cp -a $TRAIN_IN $PUBLIC_DIR/$TRAIN_OUT
    cp -a $TRAIN_IN $PUBLIC_DIR/$TRAIN_OUT
fi

if [ ! -z $1 ]; then
    echo mv $PATH_FILES $PUBLIC_DIR/
    mv $PATH_FILES $PUBLIC_DIR/
fi
