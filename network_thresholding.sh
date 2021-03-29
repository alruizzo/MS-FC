#!/bin/bash

# A.L.R.R., September 25, 2019
# Adjusted on March 29, 2021 (as it had been done unthresholded previously)
# This script goes through (already) fslsplit Allen et al.
# templates, compute the mean and SD for each one

NETWORKFOLDER=/Volumes/adrizzo/CELSIUS/templates/Allen_trendscenter/Individual_networks
OUTPUTFOLDER=/Volumes/adrizzo/CELSIUS/templates/Allen_trendscenter/

# Do fslstats and fslmaths for each network template
for network in ${NETWORKFOLDER}/*
do
  networkname=`basename $network | sed 's/.nii.gz//'`
  mean_net=`fslstats $network -m`
  sd_net=`fslstats $network -s`
  thr_value_net=`echo "$mean_net + ($sd_net * 4)" | bc`
  echo "threshold of $networkname is: $thr_value_net" >> $OUTPUTFOLDER/thr_values.txt
  echo "calculating fslmaths for $networkname..."
  fslmaths $network -thr $thr_value_net $NETWORKFOLDER/thr_${networkname}
done

fslmerge -t $OUTPUTFOLDER/RSN_HC_thresholded_tmaps_61_73_61.nii.gz $NETWORKFOLDER/thr_*

echo "done"
