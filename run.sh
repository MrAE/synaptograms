#!/bin/sh

C='collman'
E='collman14v2'
F='collman_collman14v2'
B='108 108 5'
L='home/xyzLocations.csv'
O='home/testing.csv'
o='home/synTest'
con='home/config.ini'

getCube=true
genSynaptograms=true


if $getCube
then
  echo "Downloading cubes"
  python3 getCubes.py -C $C -E $E -F $F -B $B -L $L -O $O --con $con
  echo "Done"
fi

if $genSynaptograms
then
  echo "Generating synaptograms"
  Rscript Synaptograms.R -f $O.h5 -o $o
  echo "Done"
fi


