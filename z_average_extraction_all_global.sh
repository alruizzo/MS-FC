#!/bin/bash

# A.L.R.R., November 8, 2019
# Modified on April 6, 2021

# Script to extract the average functional connectivity of specific...
# ...resting-state networks

# Specify where DR2 results are (subjects 4D with all components)
DRFOLDER=/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/DRstage2
OUTPUTFOLDER=/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/Randomise/globalFC
MELODICFOLDER=/Volumes/adrizzo/CELSIUS/MS/ICA30/Individual_ICs

echo "creating the output folder if it doesn't exist..."
mkdir -p $OUTPUTFOLDER

# Obtain average FC for the global network (for MR with sleep)
echo "working on extracting the mean..."
for i in ${DRFOLDER}/*
do
  SUBJECT=$(basename "${i}")
  echo $SUBJECT >> $OUTPUTFOLDER/00_global_FC_participants.txt
  cd $i
    # Obtain the mean within the thresholded MELODIC component (vs. all values above 0: "-l 0")
    fslstats $i/${SUBJECT}_0004.nii.gz -k $MELODICFOLDER/thr_vol0004.nii.gz -M >> $OUTPUTFOLDER/01_global_FC_IC5_RFPN.txt
    fslstats $i/${SUBJECT}_0005.nii.gz -k $MELODICFOLDER/thr_vol0005.nii.gz -M >> $OUTPUTFOLDER/02_global_FC_IC6_PDMN.txt
    fslstats $i/${SUBJECT}_0006.nii.gz -k $MELODICFOLDER/thr_vol0006.nii.gz -M >> $OUTPUTFOLDER/03_global_FC_IC7_SMN2.txt
    fslstats $i/${SUBJECT}_0007.nii.gz -k $MELODICFOLDER/thr_vol0007.nii.gz -M >> $OUTPUTFOLDER/04_global_FC_IC8_LFPN.txt
    fslstats $i/${SUBJECT}_0010.nii.gz -k $MELODICFOLDER/thr_vol0010.nii.gz -M >> $OUTPUTFOLDER/05_global_FC_IC11_ADMN.txt
    fslstats $i/${SUBJECT}_0011.nii.gz -k $MELODICFOLDER/thr_vol0011.nii.gz -M >> $OUTPUTFOLDER/06_global_FC_IC12_SMN1.txt
    fslstats $i/${SUBJECT}_0023.nii.gz -k $MELODICFOLDER/thr_vol0023.nii.gz -M >> $OUTPUTFOLDER/07_global_FC_IC24_BG.txt
done

# If I wanted to use names for files as column headers,...
# ...it could be something like, but I guess I won't need them...
# ...because it'll be a txt file:

for file in ${OUTPUTFOLDER}/*
do
  FILENAME=$(echo $file | awk -F"global_FC_" '{print $2}' | cut -f 1 -d '.')
  (echo $FILENAME && cat $file) > $OUTPUTFOLDER/${FILENAME}1.txt && mv $OUTPUTFOLDER/${FILENAME}1.txt $file
done

# Now put all texts files into one single as columns
cd $OUTPUTFOLDER
paste -d "  " ??_global_FC*.txt > Z_average_FC_global.txt
echo "done"
