#!/bin/bash

OUTPUTPATH=/mnt/DATA/MS/MELODIC/Despike/GSR/
SOURCEPATH=/mnt/DATA/MS/DPARSF/FunImgARWglobalCFSD/

for SUBJECT in ${SOURCEPATH}*
do
	SUBJECTNAME=`basename $SUBJECT`
	echo $SUBJECTNAME
	cd ${SOURCEPATH}/${SUBJECTNAME}
	echo $SUBJECT/${SUBJECTNAME}_ARWglobalCFSD_despiked.nii >> ${OUTPUTPATH}/participants_list_MELODIC_global.txt
	#echo $SUBJECT/Detrend_4DVolume.nii >> ${OUTPUTPATH}/participants_list_MELODIC_nodespiked.txt
done
