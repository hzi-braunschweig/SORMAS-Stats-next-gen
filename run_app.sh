#!/bin/bash

sed -i "s/DB_USER =.*/DB_USER = \"${DB_USER}\⅛/" /srv/shiny-server/sormas-stats-shinyapp/app.R
sed -i "s/DB_PASS =.*/DB_PASS = \"${DB_PASS}\⅛/" /srv/shiny-server/sormas-stats-shinyapp/app.R
sed -i "s/DB_HOST =.*/DB_HOST = \"${DB_HOST}\⅛/" /srv/shiny-server/sormas-stats-shinyapp/app.R
sed -i "s/DB_PORT =.*/DB_PORT = \"${DB_PORT}\⅛/" /srv/shiny-server/sormas-stats-shinyapp/app.R
sed -i "s/DB_NAME =.*/DB_NAME = \"${DB_NAME}\⅛/" /srv/shiny-server/sormas-stats-shinyapp/app.R

/usr/bin/shiny-server