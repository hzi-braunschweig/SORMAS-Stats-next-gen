#!/bin/bash

sed -i "s/DB_USER =.*/DB_USER = ${DB_USER}/" /app/sormas-stats-app/global.R
sed -i "s/DB_PASS =.*/DB_PASS = ${DB_PASS}/" /app/sormas-stats-app/global.R
sed -i "s/DB_HOST =.*/DB_HOST = ${DB_HOST}/" /app/sormas-stats-app/global.R
sed -i "s/DB_PORT =.*/DB_PORT = ${DB_PORT}/" /app/sormas-stats-app/global.R
sed -i "s/DB_NAME =.*/DB_NAME = ${DB_NAME}/" /app/sormas-stats-app/global.R

#/usr/local/bin/R -e "shiny::runApp('/app', host = '0.0.0.0', port = 3839)
/usr/bin/shiny-server