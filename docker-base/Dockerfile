# check available R versions here: <https://hub.docker.com/r/rocker/shiny/tags>
#FROM rocker/shiny:3.6.3
FROM rocker/shiny:4.1.2
# ---------------------------------------------
LABEL org.opencontainers.image.authors="bernard.silenou@helmholtz-hzi.de"

ENV RENV_PATHS_ROOT=/opt/local/renv
ENV RENV_PATHS_CACHE=/opt/local/renv/cache

FROM rocker/shiny:4.1.2

# system libraries of general use
## install debian packages
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libpq-dev \
    libssl-dev \
    && install2.r --error \
    --deps TRUE \
    # tidyverse \
    # dplyr \
    # devtools \
    formatR \
    # remotes \
    # selectr \
    caTools \
  && rm -rf /tmp/downloaded_packages

## update system libraries
RUN apt-get update && \
    apt-get upgrade -y && \
    apt-get clean
	
# Install missing debian/ubuntu packages
RUN apt-get update \
 && apt-get install -y --no-install-recommends \
       libudunits2-dev \
       libgdal-dev \
       libsodium-dev \
 && apt-get -y autoremove \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*		

RUN echo ${RENV_PATHS_CACHE} 
RUN mkdir -p ${RENV_PATHS_CACHE} && chmod a+rx ${RENV_PATHS_CACHE} 

RUN mkdir -p /var/log/shiny-server \
  && chown -R shiny.shiny /var/log/shiny-server \
  && chmod 664 /var/log/shiny-server

## renv.lock file
COPY renv.lock ./renv.lock 
#COPY sormas-stats-shinyapp/sormas-stats-app/renv.lock ./renv.lock

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")' 
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

