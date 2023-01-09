# Hello, world!
#
# This is an example function named "hello"
# which prints "Hello, world!".
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           "Cmd + Shift + B"
#   Check Package:             "Cmd + Shift + E"
#   Test Package:              "Cmd + Shift + T"

library(dplyr)
library(sf)
library(yaml)
library(fuzzyjoin)

extract_highways <- function(path, list_of_highways=NA){
  if (is.na(list_of_highways)){
    list_of_highways = as.vector(read.csv(".//data//default_values//default_roads.csv", header=T, sep=",")$Value)
  }
  osmextract::oe_vectortranslate(path)
  df = sf::st_read(".//data//district-of-columbia-latest.gpkg")
  df <- df %>% filter(df$highway %in% list_of_highways)
  return(df)
}

column_exists <- function(table, column) {
  if (names(table) %in% column) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

create_key_value <- function(keys, values) {
  if (length(keys) == length(values)) {
    dict <- list()
    for (index in length(keys)) {
      dict[[keys[index]]] == values[index]
    }
    return(dict)
  } else {
    return(stop("key, value arguments are not the same length"))
  }
}

get_class <- function(column){
  path <- paste0(getwd(), "/rBicycleLTS/config/config_data.yaml")
  return(read_yaml(path)[[column]]$datatype)
}

get_default_class <- function(column) {
  path <- paste0(getwd(), "/rBicycleLTS/config/config_data.yaml")
  return(read_yaml(path)[[column]]$default_datatype)
}

add_default_columns <- function(df, config) {
  for (i in seq_len(length(config$segment))) {
    name <- config$segment[[i]]$name
    if (!any(names(df) %in% name)) {
      df[[name]] <- NA
    }
  }
  return(df)
}

assign_classes <- function(df, config) {
  for (i in seq_len(length(config$segment))) {
    name <- config$segment[[i]]$name
    # check to make sure a user hasn't defined list of class types
    if (!(class(df[[name]]) %in% get_class(names(config$segment[i])))) {
      if (length(get_default_class(names(config$segment[i]))) > 1) {
        stop("Problem with your config_data.yaml file.
              Attribute should only have one datatype.")
      } else {
        class(df[[name]]) <- get_default_class(names(config$segment[i]))
      }
    }
  }
  return(df)
}

define_assumptions <- function(df, config) {
  for (i in seq_len(length(config$segment) - 1)) {
    name <- config$segment[[i]]$name
    if ("assumptions" %in% names(config$segment[[i]])) {
        for (j in seq_len(length(config$segment[[i]]$assumptions))) {
            val <- config$segment[[i]]$assumptions[j][[1]]$val
            if (!(j == length(config$segment[[i]]$assumptions))) {
                condition <- config$segment[[i]]$assumptions[j][[1]]$where
                df[[name]] <- ifelse(is.na(df[[name]]) & 
                                     eval(parse(text = condition)), 
                                     val, 
                                     df[[name]]
                                     )
            } else {
                df[[name]] <- ifelse(is.na(df[[name]]), val, df[[name]])
            }}}}
    return(df)
  }

convert_title <- function(df){
    for (i in seq_len(length(names(df)))){
        name <- names(df)[i]
        if (class(df[[name]])[1] == "character") {
            yeses <- c("yes", "YES", "YEs", "YeS", "yES")
            df[[name]] <- ifelse(df[[name]] %in% yeses, "Yes", df[[name]])
            df[[name]] <- ifelse(df[[name]] %in% c("no", "nO", "NO"),
                                 "No", df[[name]])
        }
    }
    return(df)
}

detect <- function(x, y){ 
  mapply(function(x, y) any(x == y), strsplit(x, ', '), strsplit(y, ', '))
}

fix_speed <- function(roads, maxspeedlimit_col) {
    roads[[maxspeedlimit_col]] <- 5*(roads[[maxspeedlimit_col]]%/%5 + as.logical(roads[[maxspeedlimit_col]]%%5))
    return(roads)
}

perform_lanes_check <- function(df, lanes, centerline){
    if( any(df[[lanes]] < 2 & df[[centerline]] == 'Yes')){
        print('There are segments with ceterlines and only one lane of traffic')
    } else {
        print('Checked for: Centerline Presents and Only One Lane of Traffic')
        print('No Issues Found')
    }
}

get_names <- function(config){
    collection <- c()

    return(collection)
}

score_blts_or <- function(df, config, id, area_type='urban', rural_urban_col=NA, geometry='geometry') {

    for (i in seq_len(length(config$segment))){
        assign(config$segment[i]$name <- config$segment[i]$name)
    }

    for (i in seq_len(length(config$segment$bike_infra$vals))){
        assign(names(config$segment$bike_infra$vals[i]) <- config$segment$bike_infra$vals[i])
    }
    
    blts_no_bike_lane <- scores_sub_urb %>% filter(bike_lane == 'No')
    blts_bike_lane <- scores_sub_urb %>% filter(bike_lane == 'Yes')

    df1 <- fuzzy_left_join(df, blts_no_bike_lane,
            by = c( fclass = "func_class",
                    maxspeed = "lower_speed",
                    maxspeed = "upper_speed",
                    lanes = "lower_lanes",
                    lanes = "upper_lanes",
                    centerline = "centerline",
                    aadt = "lower_adt",
                    aadt = "upper_adt"
            ),
            match_fun = list( detect, `>=`, `<=`,  `>=`, `<=`,  `==`, `>=`, `<=` )
            )

    df1$blts_score <- ifelse(
      df[[bike_infra]] %in% c(protected_bl, shared_use_path), 1, df1$blts_score 
      ) 
    df1$blts_score <- ifelse(
      df[[fclass]] %in% c('motorway', 'motorway_link'), 4, df1$blts_score 
      ) 
 

path <- paste0(getwd(), "/rBicycleLTS/data//washington_dc/washington_dc_ways.shp")
yaml_centerline_path <- paste0(getwd(), "/rBicycleLTS/config/config_centerline.yaml")
yaml_lane_path <- paste0(getwd(), "/rBicycleLTS/config/config_forward_backwards.yaml")
yaml_config_type <- paste0(getwd(), "/rBicycleLTS/config/config_data.yaml")
csv_scores_rural <- paste0(getwd(), "/rBicycleLTS/data/scores/rural_bike_lts.csv")
csv_scores_sub_urb <- paste0(getwd(), "/rBicycleLTS/data/scores/sub_urban_lts.csv")

config <- read_yaml(yaml_centerline_path)
config_lanes <- read_yaml(yaml_lane_path)
config_types <- read_yaml(yaml_config_type)
scores_rural <- read.csv(csv_scores_rural, sep=',', header=T)
scores_sub_urb <- read.csv(csv_scores_sub_urb, sep=',', header=T)

roads <- sf::read_sf(path)
roads1 <- add_default_columns(roads, config)
roads2 <- assign_classes(roads1, config)
roads3 <- define_assumptions(roads2, config)
#roads2$lanes <- ifelse(roads2$lanes < 2 & roads2$centerline == 'Yes', 2, roads2$lanes)
roads4 <- convert_title(roads3)
roads5 <- fix_speed(roads4, 'maxspeed')
roads <- score_blts_or(roads5, scores_sub_urb, 'id')

st_write(roads, 'test21.shp', 'test21', delete_dsn=T)
