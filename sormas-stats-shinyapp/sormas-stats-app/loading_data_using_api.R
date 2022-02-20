

# loading data through API
library(httr)
library(jsonlite)
#base <- " HTTPS://api.intrinio.com/"
base_url = "http://127.0.0.1:6290/__docs__/#/"
event_url = "default/post_Events/"


my_url = base::paste0(base_url, event_url)

my_url_results = httr::GET(my_url, delay=1000)

my_url_results_text = content(my_url_results, as = "text")
my_url_results_json <- jsonlite::fromJSON(my_url_results_text, flatten = TRUE)

events_df <- as.data.frame(my_url_results_json)


###
user_url = "default/get_Users"
my_url_user = base::paste0(base_url, user_url)

my_url_results_user = httr::GET(my_url_user)

my_url_results_user_text = content(my_url_results_user, as = "text")
my_url_results_json <- jsonlite::fromJSON(my_url_results_text, flatten = TRUE)

events_df <- as.data.frame(my_url_results_json)
