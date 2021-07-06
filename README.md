# SORMAS-Stats-next-gen
SORMAS-Stats contain functions to analyze and visualize surveillance data collected by SORMAS.

## Run docker container

### Prerequisites

(1) You need to install a container engine (e.g. Docker) 

### Run container

(1) Pull the latests version from hub.docker.com

```
docker pull hzibraunschweig/sormas-stats:latest
```
I you like to pull an image built from development branch, you need to pull the tag 'developemnt'. 

(2) Run the image

```
docker run -p 0.0.0.0:3838:3838 hzibraunschweig/sormas-stats:latest
```
The app will listen on port 3838. You can map this port to any port on the server. 

```
docker run -p 0.0.0.0:3838:3838 -v /local_path_to_app/shiny-server.conf:/etc/shiny-server/shiny-server.conf hzibraunschweig/sormas-stats:latest
```
If you like to provide a custom shiny-server.conf you can map this as a volume. Example shiny-server.conf:
 
 ```
 # Instruct Shiny Server to run applications as the user "shiny"
run_as shiny;

# Define a server that listens on port 3838
server {
  listen 3838 0.0.0.0;

  # Define a location at the base URL
  location / {

    # Host the directory of Shiny Apps stored in this directory
    site_dir /srv/shiny-server;

    # Log all Shiny output to files in this directory
    log_dir /var/log/shiny-server;

    # When a user visits the base URL rather than a particular application,
    # an index of the applications available in this directory will be shown.
    directory_index on;
  }
}
 
 ```

Please refer to the documentation of R shiny [https://docs.rstudio.com/shiny-server/#default-configuration](https://docs.rstudio.com/shiny-server/#default-configuration)

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
    image: hzibraunschweig/sormas-stats:latest      
    environment: 
      - SHINY_LOG_STDERR=1
      - DB_USER="sormas_user"
      - DB_PASS="{{ sormas.postgres.password }}"
      - DB_HOST="{{ ansible_default_ipv4.address }}"
      - DB_PORT="5432"
      - DB_NAME="sormas"
    volumes: 
      - /path_to_shiny_app/shiny-server.conf:/etc/shiny-server/shiny-server.conf
    restart: always
```

## Run locally
(1) Set the R PATH to

```r
setwd("SORMAS-Stats/shinyapp")
```

(2) Run app using the command `shiny::runApp()`


