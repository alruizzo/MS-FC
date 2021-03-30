#!/bin/bash

#ALRR, October 23, 2019, BASH script to prepare DR images for permutation analyses with FSL (e.g., 2nd level analyses)

SEARCHFOLDER=/Users/adriana/Downloads/MS/DRstage2 #specify where DR2 results are (subjects 4D with all components)
TEMPFOLDER=/Users/adriana/Downloads/MS/Temp

mkdir -p $TEMPFOLDER #temporal folder

cd $SEARCHFOLDER #make sure we are in the right folder

## Create participant-specific folders (using the number in the filename) in case they don't exist (and you have only lists)
for i in ${SEARCHFOLDER}/*
do
  CURRDIR=$(echo $i | awk -F"_" '{print $3}' | sed -e s/[^0-9]//g)
  echo $CURRDIR
  mkdir $TEMPFOLDER/$CURRDIR
  mv $i $TEMPFOLDER/$CURRDIR
done

mv $TEMPFOLDER/* $SEARCHFOLDER

rm $TEMPFOLDER

echo "done with data organization"

## Perform fslsplit in each image to separate the components for each subject
for i in ${SEARCHFOLDER}/*
do
  echo $i
  SUBJECT=$(basename "${i}")
  cd $i
    fslsplit $i/dr* $i/${SUBJECT}_ -t
done

echo "done with fslsplit"
