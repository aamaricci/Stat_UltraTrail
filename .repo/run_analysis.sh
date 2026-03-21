#!/bin/bash
shopt -s expand_aliases
source ~/.aliasrc
source ~/.functionsrc
ARGUMENTS=${@}
PATH_TO_ME=${0}
SCRIPT=${0##*/}
LOGFILE=log.${SCRIPT%.sh}
>$LOGFILE
exec >  >(tee -a $LOGFILE)
exec 2> >(tee -a $LOGFILE >&2)
echo "Executing: $SCRIPT from $PATH_TO_ME"

rm -vf PDF_*

while read RACE;do
    echo $RACE
    analyze_time_series ifile=input_${RACE}.in ofile=out_${RACE}.dat
done<list.race
