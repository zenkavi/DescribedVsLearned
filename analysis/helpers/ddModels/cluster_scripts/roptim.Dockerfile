FROM rocker/tidyverse:4

RUN install2.r --error --skipinstalled --ncpus -1 remotes here foreach

RUN apt-get update && apt-get install -y libmagick++-dev

RUN installGithub.r jbryer/visualMLE

RUN install2.r optparse

RUN install2.r doParallel
