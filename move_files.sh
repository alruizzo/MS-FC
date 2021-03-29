#!/bin/bash

SOURCEPATH=/mnt/DATA/MS/DPARSF/FunRaw/ #path where data will be copied from
ENDPATH=/mnt/DATA/MS/DPARSF/FunImg/ #path where data will be pasted to

if [ ! -d $ENDPATH ]; then
  mkdir $ENDPATH
fi

# for SUBJECT in ${SOURCEPATH}*
# do
#   mv "$SUBJECT" "${SUBJECT//_t1w/}"
# done

# for SUBJECT in ${SOURCEPATH}*
# do
#   SUBJECTNAME=$(basename "$SUBJECT" .nii)
# 	echo $SUBJECTNAME
#   mkdir ${ENDPATH}/$SUBJECTNAME
#   mv $SUBJECT ${ENDPATH}/$SUBJECTNAME
# done
#
# mv $ENDPATH $SOURCEPATH

for SUBJECT in ${SOURCEPATH}*
do
  SUBJECTNAME=` basename $SUBJECT `
	echo $SUBJECTNAME
  mkdir ${ENDPATH}/$SUBJECTNAME
  cd $SUBJECT/slomoco4/
  mv ${SUBJECTNAME}.slicemocoxy_afni.slomoco_pestica.nii ${ENDPATH}/$SUBJECTNAME
done
