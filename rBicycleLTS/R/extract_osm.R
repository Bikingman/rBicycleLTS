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

extract_highways <- function(path, list_of_highways=NA){
  if (is.na(list_of_highways)){
    list_of_highways = as.vector(read.csv(".//data//default_values//default_roads.csv", header=T, sep=",")$Value)
  }
  osmextract::oe_vectortranslate(path)
  df = sf::st_read(".//data//district-of-columbia-latest.gpkg")
  print(head(df))
  print(names(df))
  df <- df %>% filter(df$highway %in% list_of_highways)
  return(df)
}

path <- paste0(getwd(), "/rBicycleLTS/data//washington_dc/washington_dc_ways.shp")
yaml_centerline_path <- paste0(getwd(), "/rBicycleLTS/config/config_centerline.yaml")
yaml_lane_path <- paste0(getwd(), "/rBicycleLTS/config/config_forward_backwards.yaml")
yaml_config_type <- paste0(getwd(), "/rBicycleLTS/config/config_data.yaml")



config <- read_yaml(yaml_centerline_path)
config_lanes <- read_yaml(yaml_lane_path)
config_types <- read_yaml(yaml_config_type)

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

roads <- sf::read_sf(path)

View(head(df, 10000))

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

roads1 <- add_default_columns(roads, config)

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

roads1 <- assign_classes(roads1, config)
str(roads1)

define_assumptions <- function(df, config) {
  for (i in seq_len(length(config$segment) - 1)) {
    name <- config$segment[[i]]$name
    for (i in seq_len(length(config$segment[[i]]$name))) {
      if ("assumptions" %in% names(config$segment[[i]])) {
        for (j in seq_len(length(config$segment[[i]]$assumptions) - 1)) {
          condition <- config$segment[[i]]$assumptions[j][[1]]$where
          val <- config$segment[[i]]$assumptions[j][[1]]$val
            browser()
          df <- df %>% mutate(!!as.name(name), ifelse(is.na(!!as.name(name)) & parse(text = condition), val, !!as.name(name)))


df %>% mutate({{ name }}, ifelse(is.na({{ name }}), {{ val }}, {{ name }}))

        }
      }
      }
    }
    return(df)
  }


test2 <- define_assumptions(roads1, config)
View(head(test2, 100))
assign_blts <- function(roads, config) {

  # first create new columns where missing
  roads <- add_default_columns(roads, config)
  roads <- assign_classes(roads, config)
  roads <- 1

}