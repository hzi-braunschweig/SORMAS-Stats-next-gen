# SORMAS-Stats-next-gen
SORMAS-Stats contain functions to analyze and visualize surveillance data collected by SORMAS.

## sormas-stats-shinyapp folder
- `sormas-stats-shinyapp`
    - `app.R`
    - `utils/`
    - `demo-data/`

## Run docker container

### Prerequisites

(1) You need to install a container engine (e.g. Docker) 

### Run container

(1) Pull the latests version from hub.docker.com

```
docker pull hzibraunschweig/sormas-stats:latest
```

(2) Run the image

```
docker run hzibraunschweig/sormas-stats:latest
```

### Environment parameters

After starting the container, the application tries to connect to a SORMAS database. 
The container uses these environment parameters for configuration:

 | Parameter | default | accepted values |
 |-----------|---------|-----------------|
 | SHINY_LOG_STDERR| | 0 or 1 to log output of the app to stderr (needed to display via docker-logs)|
 | DB_USER | "sormas_user" | username for sormas databse |
 | DB_PASS | "password" | password of database user |
 | DB_HOST | 127.0.0.1 | IP-Adress of SORMAS database to connect to |
 | DB_PORT | 5432 | SORMAS database port |
 | DB_NAME | "sormas" | name of sormas database |

### Docker compose
You can use a docker-compose file to start the application: 

```
version: "3"
services: 
  sormas-stats:
    ports: 
      - "0.0.0.0:3838:3838"
    image: {{ stats.image }}      
    environment: 
      - SHINY_LOG_STDERR=1
      - DB_USER="sormas_user"
      - DB_PASS="{{ sormas.postgres.password }}"
      - DB_HOST="{{ ansible_default_ipv4.address }}"
      - DB_PORT="5432"
      - DB_NAME="sormas"
    volumes: 
      - {{ stats.path}}/shiny-server.conf:/etc/shiny-server/shiny-server.conf
    restart: {{ stats.restart }}
```

## Run locally
(1) Set the R PATH to

```r
setwd("SORMAS-Stats/shinyapp")
```

(2) Run app using the command `shiny::runApp()`


