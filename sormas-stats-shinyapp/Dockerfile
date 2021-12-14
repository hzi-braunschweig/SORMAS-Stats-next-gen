# Base image https://hub.docker.com/u/rocker/
# FROM rocker/shiny:latest 
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

# copy necessary files
## renv.lock file
COPY /sormas-stats-app/renv.lock ./renv.lock 
## app folder
COPY /sormas-stats-app ./app

RUN mkdir -p /var/log/shiny-server \
  && chown -R shiny.shiny /var/log/shiny-server \
  && chmod 664 /var/log/shiny-server

# install renv & restore packages
RUN Rscript -e 'install.packages("renv")' 
RUN Rscript -e 'renv::consent(provided = TRUE)'
RUN Rscript -e 'renv::restore()'

# expose port
EXPOSE 3838

# run app on container start
CMD ["R", "-e", "shiny::runApp('/app', host = '0.0.0.0', port = 3839)"]
