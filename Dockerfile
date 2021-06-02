# check available R versions here: <https://hub.docker.com/r/rocker/shiny/tags>
#FROM rocker/shiny:3.6.3
FROM hzibraunschweig/sormas-stats-base:latest
# ---------------------------------------------
LABEL org.opencontainers.image.authors="bernard.silenou@helmholtz-hzi.de"


# Copy the app
RUN mkdir /srv/shiny-server/sormas-stats-shinyapp
COPY sormas-stats-shinyapp /srv/shiny-server/sormas-stats-shinyapp
RUN chmod -R 755 /srv/shiny-server/sormas-stats-shinyapp

# Expose port 
# (We can map it to standard HTTP port lateron when building the container!)
EXPOSE 3838

# run the shiny app on container start
CMD ["/usr/bin/shiny-server"]
