
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
## For getting synapse cubes

getCube=true
COLL='collman'
EXP='collman14v2'
FM='collman_collman14v2'
BUFF='108 108 5' # in x y z
LOC='xyzLocations.csv' # in x,y,z
OUT='testing.csv'
CON='neurodata.conf'

## For plotting synaptograms

genSynaptograms=true
SYIN='testing.csv.h5'
SYOUT='synTest'
MEAN=""
SD=""
```
