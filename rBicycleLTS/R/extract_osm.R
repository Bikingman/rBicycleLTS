# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'
library(dplyr)
library(sf)
library(yaml)

extract_highways <- function(path, list_of_highways=NA){
  if (is.na(list_of_highways)){
    list_of_highways = as.vector(read.csv('.//data//default_values//default_roads.csv', header=T, sep=',')$Value)
  }
  osmextract::oe_vectortranslate(path)
  df = sf::st_read('.//data//district-of-columbia-latest.gpkg')
  print(head(df))
  print(names(df))
  df <- df %>% filter(df$highway %in% list_of_highways)
  return(df)
}

path <- './/data//washington_dc//washington_dc_ways.shp'
yaml_centerline_path <- './/config//config_centerline.yaml'
yaml_lane_path <- './/config//config_forward_backwards.yaml'
yaml_config_type <- './/config//config_data.yaml'



config_centerline <- read_yaml(yaml_centerline_path)
config_lanes <- read_yaml(yaml_lane_path)
config_types <- read_yaml(yaml_config_type)

column_exists <- function(table, column){
  if (names(table) %in% column){
    return(TRUE)
  } else {
    return(FALSE)
  }
}

create_key_value <- function(keys, values){
  if (length(keys) == length(values)){
    dict <- list()
    for ( index in length(keys)){
      list[[keys[index]]] == values[index]
    }
    return(list)
  } else {
    return(stop('key, value arguments are not the same length'))
  }
}

df <- sf::read_sf(path)

View(head(df, 10000))

config_centerline$segment

for (i in 1:length(config_centerline$segment)){
  name <- config_centerline$segment[[i]]$name
  if ( !any(names(df) %in% name) ) {
    df[[name]] <- NA
  }
}



get_class <- function(column){
  return(config_types[[column]]$datatype)
}

get_default_class <- function(column){
  return(config_types[[column]]$default_datatype)
}

for (i in 1:length(config_centerline$segment)){

  name <- config_centerline$segment[[i]]$name
  class <- get_class(names(config_centerline$segment[i]))

  # check to make sure a user hasn't defined list of class types
  if (length(class) > 1){
    print(class)
    stop('Problem with your config_data.yaml file. Attribute should only have one datatype.')
  }
  class(df[[name]]) <- class
}




