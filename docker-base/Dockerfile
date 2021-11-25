# check available R versions here: <https://hub.docker.com/r/rocker/shiny/tags>
FROM rocker/shiny:3.6.3
# ---------------------------------------------
LABEL org.opencontainers.image.authors="bernard.silenou@helmholtz-hzi.de"
#code with default packahes from https://hub.docker.com/r/rocker/shiny-verse/dockerfile
RUN apt-get --allow-releaseinfo-change update -qq && apt-get upgrade -y && apt-get -y --no-install-recommends install \
  libmysqlclient-dev \
  default-libmysqlclient-dev \
  libxml2-dev \
    libcairo2-dev \
    libsqlite3-dev \
    libmariadbd-dev \
	libmariadbclient-dev \
    libpq-dev \
    libssh2-1-dev \
    unixodbc-dev \
    libcurl4-openssl-dev \
    libssl-dev \
    libv8-dev \
    libudunits2-dev \
	libmysqlclient-dev \
    libgdal-dev \
    perl \ 
    libcompress-raw-zlib-perl \
    libsodium-dev \
  && apt-get -y autoremove \
  && apt-get clean \
  && rm -rf /var/lib/apt/lists/*		
  
RUN install2.r --error \
    --deps TRUE \
    tidyverse \
    dplyr \
    devtools \
    formatR \
    remotes \
    selectr \
    caTools \
  && rm -rf /tmp/downloaded_packages 


## renv.lock file
COPY renv.lock ./renv.lock 

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")'
RUN Rscript -e 'renv::restore()'