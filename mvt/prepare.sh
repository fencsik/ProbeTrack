#!/bin/bash
# prepare.sh: 
# moves experiment files to the appropriate Public directory 
# so experimenters can access them
# 
# $Id: prepare.sh,v 1.9 2004/08/20 13:50:31 fencsik Exp $

PUBLIC_DIR=~/Public/Experiments/Tracking/ShiftTrack/ShiftTrack6

GENERATOR_IN=generator.m
TRACK_IN=track.m
TRAIN_IN=train.m
GENERATOR_OUT=ShiftTrack6Gen.m
TRACK_OUT=ShiftTrack6.m
TRAIN_OUT=ShiftTrack6Train.m

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
if [ -x $TRAIN_IN ]; then
    echo "File $TRAIN_IN not found"
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

if [ $TRAIN_IN -nt $PUBLIC_DIR/$TRAIN_OUT ]; then
    echo cp -a $TRAIN_IN $PUBLIC_DIR/$TRAIN_OUT
    cp -a $TRAIN_IN $PUBLIC_DIR/$TRAIN_OUT
fi

if [ ! -z $1 ]; then
    echo mv $PATH_FILES $PUBLIC_DIR/
    mv $PATH_FILES $PUBLIC_DIR/
fi
