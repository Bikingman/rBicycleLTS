# Author: Daniel Patterson
# Company: Cambridge Systematics
# Date: 2023.
# Description: Collection of functions to score bicycle level of traffic stress on road segments. 
# Source of BLTS Scores: Analysis Procedures Manual Version 2, Chapter 14
# https://www.oregon.gov/odot/planning/pages/apm.aspx
# Assumptions based on information available from People for Bikes, Bicycle Network Analysis 
# https://github.com/tooledesign/pybna/blob/master/pybna/config.yaml

library(dplyr)
library(sf)
library(yaml)
library(fuzzyjoin)

# gets class of each dataset from yaml
get_class <- function(column){
  return(config[[column]]$datatype)
}

get_default_class <- function(column) {
  return(config[[column]]$default_datatype)
}

add_default_columns <- function(df, config) {
  for (i in seq_len(length(config$segment))) {
    name <- config$segment[[i]]$name
    if (!any(names(df) %in% paste0(name, '_src'))){
      df[[paste0(name, '_src')]]
    }
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
    df[[paste0(name, '_src')]] <- ifelse(is.na(df[[name]]),  'assumed', 'source_data')
    if ("assumptions" %in% names(config$segment[[i]])) {
      for (j in seq_len(length(config$segment[[i]]$assumptions))) {
        val <- config$segment[[i]]$assumptions[j][[1]]$val
        if (!(j == length(config$segment[[i]]$assumptions))) {
          condition <- config$segment[[i]]$assumptions[j][[1]]$where
          df[[name]] <- ifelse(is.na(df[[name]]) & eval(parse(text = condition)),  val, df[[name]])
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
      df[[name]] <- ifelse(df[[name]] %in% yeses, 
                            "Yes", 
                            df[[name]])
      df[[name]] <- ifelse(df[[name]] %in% c("no", "nO", "NO"),
                           "No", 
                           df[[name]])
    }
  }
  return(df)
}

detect <- function(x, y){ 
  mapply(function(x, y) any(x == y), strsplit(x, ', '), strsplit(y, ', '))
}

fix_speed <- function(roads, maxspeedlimit_col) {
  roads[[maxspeedlimit_col]] <- 5*(as.integer(roads[[maxspeedlimit_col]])%/%5 + as.logical(as.integer(roads[[maxspeedlimit_col]])%%5))
  return(roads)
}

score_blts <- function(df, config, scores ) {

  df <- add_default_columns(df, config)
  df <- assign_classes(df, config)
  df <- define_assumptions(df, config)
  df <- convert_title(df)
  df <- fix_speed(df, 'maxspeed')
  
  for (i in seq_len(length(config$segment))){
    assign(names(config$segment[i]), config$segment[[i]]$name)
  }
  
  bike_facs <- c()
  for (i in seq_len(length(config$segment$bike_infra$vals))){
    bike_facs <- append(bike_facs, config$segment$bike_infra$vals[[i]])
    assign(names(config$segment$bike_infra$vals)[[i]], config$segment$bike_infra$vals[[i]])
  }
  
  blts_no_bike_lane <- scores %>% filter(bike_lane == 'No')
  blts_bike_lane <- scores %>% filter(bike_lane == 'Yes')

  df1 <- fuzzy_left_join(df[!(df[[bike_infra]] %in%  bike_facs), ], blts_no_bike_lane,
                         by = c( fclass = "func_class",
                                 maxspeed = "lower_speed",
                                 maxspeed = "upper_speed",
                                 lanes = "lower_lanes",
                                 lanes = "upper_lanes",
                                 centerline = "centerline",
                                 aadt = "lower_adt",
                                 aadt = "upper_adt"
                         ),
                         match_fun = list( detect, `>=`, `<=`,  `>=`, `<=`, `==`, `>=`, `<=` )
  )
  
  df2 <- fuzzy_left_join(df[df[[bike_infra]] %in%  bike_facs,], blts_bike_lane,
                         by = c(  
                           maxspeed = "lower_speed",
                           maxspeed = "upper_speed",
                           lanes = "lower_lanes",
                           lanes = "upper_lanes",
                           parking = "parking",
                           bike_l_width = "lower_bl_width",
                           bike_l_width = "upper_bl_width"
                         ),
                         match_fun = list( `>=`, `<=`,  `>=`, `<=`,  `==`, `>=`, `<=` )
  )
  
  df3 <- rbind(df1, df2)
  df3$blts_score <- ifelse(
    df3[[bike_infra]] %in% c(protected_bl, shared_use_path), 1, df3$blts_score 
  )
  return(df3)
}