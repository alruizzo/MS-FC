#!/bin/bash

# A.L.R.R.
# March 30, 2021
# Script to copy files to different folders

# Specify source directory
BASEPATH=/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/DRstage2

# Specify independent component that we want to copy
icnumber='0006'

# Loop to copy and paste the specific IC files into different (participant)
# folders
for folder in ${BASEPATH}/*
do
	foldername=`basename $folder`
	cp $folder/${foldername}_$icnumber.nii.gz ${BASEPATH}_RRMS/${foldername}
done

echo "done"