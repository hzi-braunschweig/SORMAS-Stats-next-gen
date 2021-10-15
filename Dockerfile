# check available R versions here: <https://hub.docker.com/r/rocker/shiny/tags>
#FROM rocker/shiny:3.6.3
FROM hzibraunschweig/sormas-stats-base:latest
# ---------------------------------------------
LABEL org.opencontainers.image.authors="bernard.silenou@helmholtz-hzi.de"

ENV DB_USER="sormas_user"
ENV DB_PASS="password"
ENV DB_HOST="127.0.0.1"
ENV DB_PORT="5432"
ENV DB_NAME="sormas"

# Copy the app
RUN mkdir /srv/shiny-server/sormas-stats-shinyapp \
  && mkdir -p /home/shiny/log \
  && chown -R shiny.shiny /home/shiny
COPY sormas-stats-shinyapp/sormas-stats-app /srv/shiny-server/sormas-stats-shinyapp
RUN chmod -R 755 /srv/shiny-server/sormas-stats-shinyapp
COPY run_app.sh /usr/sbin/run_app.sh
RUN chmod 700 /usr/sbin/run_app.sh


# Expose port 
# (We can map it to standard HTTP port lateron when building the container!)
EXPOSE 3838

# run the shiny app on container start
CMD ["/usr/sbin/run_app.sh"]
