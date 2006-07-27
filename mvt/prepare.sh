#!/bin/bash
# prepare.sh: 
# moves experiment files to the appropriate Public directory 
# so experimenters can access them
# 
# $LastChangedDate$

EXPERIMENT=GetMoving01
PUBLIC_DIR=~/Public/Experiments/$EXPERIMENT
FILES="track.m generator2.m *.mat"

# make sure public directory exists
if [ ! -d $PUBLIC_DIR ]; then
    mkdir -p $PUBLIC_DIR
fi

for f in $(/bin/ls $FILES); do
    if [ -x $f ]; then
	echo "File $f not found"
    else
	if [ $f -nt $PUBLIC_DIR/$f ]; then
	    echo cp -a $f $PUBLIC_DIR/$f
	    cp -a $f $PUBLIC_DIR/$f
	fi
    fi
done
