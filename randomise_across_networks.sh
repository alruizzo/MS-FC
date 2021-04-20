#!/bin/bash

# A.L.R.R., March 4th, 2021
# BASH script to loop over networks and run randomise on them
# (e.g., 2nd level analyses)

# Define paths
BASEPATH="/Users/lmuresearchfellowship/Documents/Adriana/LMU_Psychology/Projects/MS"
INPUTFOLDER="$BASEPATH/Randomise/input_RRMSonly"
OUTPUTFOLDER="$BASEPATH/HigherLevel/RRMSonly_Efficiency"
DESIGNFILE="$BASEPATH/design106_RRMSonly_effic.mat"
CONTRASTFILE="$BASEPATH/design106.con"

# FSL's randomise command line
# randomise -i <4D_input_data> -o <output_rootname> -d design.mat -t
# design.con -m <mask_image> -n 500 -D -T

# Create the output folder if it doesn't exist
mkdir -p $OUTPUTFOLDER

# Make sure we are in the right folder
cd $INPUTFOLDER

# Loop through inputs and run randomise
for i in ${INPUTFOLDER}/*
do
  # remove the .nii.gz from the network name
  network=$(basename "${i}" | awk -F'[.]' '{print $1}')
  echo $network
  mask=$(echo "$network" | awk -F'[_]' '{print $1 "_" $2}')
  echo $mask
  randomise -i $i -o $OUTPUTFOLDER/$network -d $DESIGNFILE -t $CONTRASTFILE -m $BASEPATH/NetworksSelected/${mask}_thr_vol00??.nii.gz -n 5000 -D -T
done

echo "finished $0 on $(date)"
