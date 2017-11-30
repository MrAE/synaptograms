
# Synaptograms

This docker image uses neurodata's BOSS to download regions
for plotting in synaptograms.  You will need a boss
configuration file named `neurodata.conf` that looks like this

```
[Default]
protocol = https
host = api.boss.neurodata.io
token = your_token_here
```

Your token can be obtained [here.](https://api.boss.neurodata.io/v1/mgmt/token)

You will also need a file named `config.conf` structured as follows with
your specific information filled in.  Below is an example using the
collman14 dataset. 

```
#### For getting synapse cubes

## Boolean for running get cube code
getCube=true

## collection id
COLL='collman'

## experiment id
EXP='collman14v2'

## coordinate frame id
FM='collman_collman14v2'

## buffer size in x y z
BUFF='108 108 5'      

## file of locations in x,y,z without header
LOC='xyzTesting.csv' 

## output filename 
OUT='testing.csv'

## boss configuration filename with token
CON='neurodata.conf'


#### For plotting synaptograms

## Boolean for running synaptogram code after cubes have downloaded
genSynaptograms=true

## cubes file
SYIN='testing.csv.h5'

## output file prefix
SYOUT='synTest'

## parameters file
PARAM='params.csv'
```


# params.csv

The parameters file should be in csv format with a header that matches
the feature names. 
The means and standard deviations are used to scale each feature F0
value before plotting. 

```
|"names"| bIIItubulin|   ...|VGluT1|EM10K|
|-------|------------|:----:|------------|
|"colors"|"blue"|...|"green"|"white"|
|"means"| 2120026.746139|...|1396392.87548263|0|
|"sds"| 848444.631131951|...|524801.872574875|NA|
```

# Usage

`docker run -v <working directory with config files>:/home/ -t neurodata/synaptograms`
