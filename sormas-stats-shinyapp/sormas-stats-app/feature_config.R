# This file is to configure the data to export based on the features activated
# To deactivate a feature, set the parameter to "f" in feature_configuration.txt file

#loading feature configuration file
feature_config_table = utils::read.table(file.path("./data", "feature_configuration.txt"),header=TRUE,sep = ",",comment.char="#",strip.white=TRUE)
# assign each feature_name to its enabled value and store it in the global environment
# this is because the logic to determine which ui tab panel to activate depends on the feature_name
list2env(setNames(as.list(feature_config_table$enabled),feature_config_table$feature_name), .GlobalEnv)
#selecting non-active features 
nonactive_features_vector = feature_config_table %>% dplyr::filter(enabled =="f") %>% dplyr::pull(var=feature_name)
# loading data configuration table in order to determine the data to load for active features only
# the loading_data_config_table table map each feature to its back-end data to be exported from sormas
loading_data_config_table = utils::read.table(file.path("./data", "loading_data_configuration.txt"),header=TRUE,sep=",",comment.char="#",strip.white = TRUE)
# keeping only rows in loading_data_config_table that are not in non-active_features_vector
loading_data_config_vector = base::unique(loading_data_config_table[!(loading_data_config_table$feature_name %in% nonactive_features_vector),]$required_data_name)
# loading_data_config_vector contains the name of all dataframe mapping to onifgurable reatures.
# Only features whose associated dataframe name exist in loading_data_config_vector would be exprted from sormas.
