# check available R versions here: <https://hub.docker.com/r/rocker/shiny/tags>
FROM rocker/shiny:4.3.0
# ---------------------------------------------
LABEL org.opencontainers.image.authors="bernard.silenou@helmholtz-hzi.de"

ENV RENV_PATHS_ROOT=/opt/local/renv
ENV RENV_PATHS_CACHE=/opt/local/renv/cache

#code with default packahes from https://hub.docker.com/r/rocker/shiny-verse/dockerfile
RUN apt-get update -qq && apt-get upgrade -y \
  && apt-get -y --no-install-recommends install \
    libxml2-dev \
    libpq-dev \
    libssl-dev \
    libudunits2-dev \
    libgdal-dev \
    libsodium-dev \
  && apt-get -y autoremove \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*		

  
RUN install2.r --error \
    --deps TRUE \
    tidyverse \
    formatR \
    caTools \
  && rm -rf /tmp/downloaded_packages 

RUN mkdir -p ${RENV_PATHS_CACHE} && chmod a+rx ${RENV_PATHS_CACHE} 

## renv.lock file
COPY renv.lock ./renv.lock 

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")' 
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

