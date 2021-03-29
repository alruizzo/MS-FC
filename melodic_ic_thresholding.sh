#!/bin/bash

# A.L.R.R., September 25, 2019
# Adjusted on March 29, 2021 (as it had been done unthresholded previously)
# This script fslsplit the melodic output, and calculates
# the mean and SD for each one. The threshold was set following
# Allen et al. (2011) (as done for the RSN templates)

MELODICFOLDER=/Volumes/adrizzo/CELSIUS/MS/ICA30/
ICFOLDER=/Volumes/adrizzo/CELSIUS/MS/ICA30/Individual_ICs

# Create output dir if it doesn't exist
mkdir -p $ICFOLDER

# fslsplit the melodic output
cd $ICFOLDER
if [ ! -f vol0000.nii.gz ]
  then
  fslsplit $MELODICFOLDER/melodic_IC.nii.gz -t
fi

# Do fslstats (mean and SD) and fslmaths (thr) for each IC
for ic in ${ICFOLDER}/*
do
  icname=`basename $ic | sed 's/.nii.gz//'`
  mean_ic=`fslstats $ic -m`
  sd_ic=`fslstats $ic -s`
  thr_value=`echo "$mean_ic + ($sd_ic * 4)" | bc`
  echo "threshold of $icname is: $thr_value" >> $MELODICFOLDER/thr_values.txt
  echo "calculating fslmaths for $icname..."
  fslmaths $ic -thr $thr_value $ICFOLDER/thr_${icname}
done

# Merge all the thresholded ICs into one single 4D file
fslmerge -t $MELODICFOLDER/thres_melodic_IC.nii.gz $ICFOLDER/thr_*

echo "done"
