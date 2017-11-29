#!/bin/sh

#C='collman'
#E='collman14v2'
#F='collman_collman14v2'
#B='108 108 5'
#L='home/xyzLocations.csv'
#O='home/testing.csv'
#o='home/synTest'
#con='home/config.ini'
#
#getCube=true
#genSynaptograms=true

. ./home/config.conf

if $getCube
then
  echo "Downloading cubes"
  python3 getCubes.py -C $COLL -E $EXP -F $FM -B $BUFF -L /home/$LOC -O /home/$OUT --con /home/$CON
  echo "Done"
fi

if $genSynaptograms
then
  echo "Generating synaptograms"
  Rscript Synaptograms.R -f /home/$SYIN -o /home/$SYOUT
  echo "Done"
fi


