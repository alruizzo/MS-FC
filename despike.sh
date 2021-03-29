#!/bin/bash

SOURCEPATH=/mnt/DATA/MS/DPARSF/FunImgARWglobalCFSD/

for SUBJECT in ${SOURCEPATH}*
do
	SUBJECTNAME=`basename $SUBJECT`
	echo $SUBJECTNAME
	cd ${SOURCEPATH}/${SUBJECTNAME}
	3dDespike -prefix ${SUBJECTNAME}_ARWglobalCFSD_despiked.nii.gz Detrend_4DVolume.nii >> /mnt/DATA/MS/DPARSF/Despike_globalC_log.txt
done
echo "DONE"
