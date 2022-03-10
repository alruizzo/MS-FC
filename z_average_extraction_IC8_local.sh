#!/bin/bash

# A.L.R.R., March 10, 2022

# Script to extract the average functional connectivity of...
# ...significant clusters in IC8 (LFPN). This was requested...
# during the review process, given that there were significant...
# results in this network for both fatigue and sleep quality

# To create the binarized cluster mask see
# "cluster_info_extraction.txt" and "cluster_values_extraction.txt"

# Specify where DR2 results are (subjects 4D with all components)
DRFOLDER=/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/DRstage2
OUTPUTFOLDER=/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/Randomise/localFC
MASKFOLDER=/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS/HigherLevel/TotalScore
CLUSTERMASKSLEEP="IC8_LFPN_104_cluster_mask_tstat2.nii.gz"
CLUSTERMASKFATIGUE="IC8_LFPN_104_cluster_mask_tstat1.nii.gz"

echo "creating the output folder if it doesn't exist..."
mkdir -p $OUTPUTFOLDER

# Obtain average FC for the significant clusters
echo "working on extracting the mean..."
for i in ${DRFOLDER}/*
do
  SUBJECT=$(basename "${i}")
  echo $SUBJECT >> $OUTPUTFOLDER/00_local_FC_participants.txt
  cd $i
    # Obtain the mean within the thresholded MELODIC component (vs. all values above 0: "-l 0")
    fslstats $i/${SUBJECT}_0007.nii.gz -k $MASKFOLDER/$CLUSTERMASKFATIGUE -M >> $OUTPUTFOLDER/01_local_FC_IC8_LFPN_fatigue.txt
    fslstats $i/${SUBJECT}_0007.nii.gz -k $MASKFOLDER/$CLUSTERMASKSLEEP -M >> $OUTPUTFOLDER/02_local_FC_IC8_LFPN_sleep.txt
done

# If I wanted to use names for files as column headers,...
# ...it could be something like, but I guess I won't need them...
# ...because it'll be a txt file:

for file in ${OUTPUTFOLDER}/*
do
  FILENAME=$(echo $file | awk -F"local_FC_" '{print $2}' | cut -f 1 -d '.')
  (echo $FILENAME && cat $file) > $OUTPUTFOLDER/${FILENAME}1.txt && mv $OUTPUTFOLDER/${FILENAME}1.txt $file
done

# Now put all texts files into one single as columns
cd $OUTPUTFOLDER
paste -d "  " ??_local_FC*.txt > Z_average_FC_local_IC8.txt

echo "Done. Script `basename $0` finished on $(date)"
