# Commencing 
FROM ubuntu:16.04
MAINTAINER Jesse Patsolic <studiojlp@gmail.com>

#### Add user
#RUN useradd -ms /bin/bash meda 
#RUN chown meda:meda /home/meda & addgroup meda staff

RUN apt-get update && apt-get install -y openssh-server \ 
    git libcurl4-openssl-dev libxml2-dev \
    libssl-dev libssh2-1-dev vim wget tmux

####Install and compile R
RUN apt-get update && apt-get -y install r-base \
        r-base-dev

#### Install package

RUN R -e "install.packages(c('ggplot2', 'foreach',\ 
         'gplots', 'reshape2',\ 
         'gridExtra', 'optparse',\ 
         'data.table', 'png', 'viridis', 'raster'),\
          repos = 'http://cran.rstudio.com/',\
          dependencies = TRUE)"

RUN R -e "source('https://bioconductor.org/biocLite.R'); biocLite('rhdf5')"

RUN apt-get -y install python3 python3-pip

RUN pip3 install --upgrade pip

RUN pip3 install --upgrade jupyter  \
  numpy \
  scipy \
  pandas \
  pillow \
  intern

RUN pip3 install --upgrade requests  \
  h5py \
  matplotlib \
  morton-py
  
RUN apt-get install -y python3-tk

COPY ./run.sh  /run.sh
COPY ./toolbox.py  /toolbox.py
COPY ./getCubes.py /getCubes.py
COPY ./Synaptograms.R /Synaptograms.R

RUN chmod +x /toolbox.py & \
    chmod +x /getCubes.py & \
    chmod +x /run.sh & \
    chmod +x /Synaptograms.R


ENTRYPOINT ["/run.sh"]

