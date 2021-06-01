# check available R versions here: <https://hub.docker.com/r/rocker/shiny/tags>
#FROM rocker/shiny:3.6.3
FROM rocker/shiny:latest
# ---------------------------------------------
LABEL org.opencontainers.image.authors="bernard.silenou@helmholtz-hzi.de"
#code with default packahes from https://hub.docker.com/r/rocker/shiny-verse/dockerfile
RUN apt-get update -qq && apt-get -y --no-install-recommends install \
  libxml2-dev \
  libcairo2-dev \
  libsqlite3-dev \
  libmariadbd-dev \
  libmariadbclient-dev \
  libpq-dev \
  libssl-dev \
  libcurl4-openssl-dev \
  libssh2-1-dev \
  unixodbc-dev \
  xtail \
  libv8-dev \
  libudunits2-dev \
  libgdal-dev \
  perl \
  libcompress-raw-zlib-perl \
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
  
# Install required R packages for shiny
COPY requirements.R /root/requirements.R
RUN Rscript /root/requirements.R
#RUN R -e "library(gdata); gdata::installXLSXsupport()"

# Copy the app
COPY sormas-stats-shinyapp /srv/shiny-server/
RUN chmod -R 755 /srv/shiny-server/

# Expose port 
# (We can map it to standard HTTP port lateron when building the container!)
EXPOSE 3838

# run the shiny app on container start
CMD ["/usr/bin/shiny-server.sh"]
