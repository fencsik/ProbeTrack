#!/bin/bash
# prepare.sh: 
# moves experiment files to the appropriate directory so the RAs can access them
# 
# $Id: prepare.sh,v 1.1 2004/05/10 20:46:00 fencsik Exp $

PUBLIC_DIR=~/Public/Experiments/ShiftTrack4

GENERATOR_IN=generator.m
TRACK_IN=track.m
TRAIN_IN=train.m
GENERATOR_OUT=ShiftTrack4Gen.m
TRACK_OUT=ShiftTrack4.m
TRAIN_OUT=mvt.m

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

cp -a $GENERATOR_IN $PUBLIC_DIR/$GENERATOR_OUT
cp -a $TRACK_IN $PUBLIC_DIR/$TRACK_OUT
cp -a $TRAIN_IN $PUBLIC_DIR/$TRAIN_OUT
mv $PATH_FILES $PUBLIC_DIR/
