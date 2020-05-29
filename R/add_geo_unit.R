#' Add geoid to survey data
#'
#' Perform spatial join between respondent's coordinates and the exisitng geographic data of the US
#'
#' @name add_geo_unit
#'
#' @param target_df The target dataframe with coordinates. Usually, the individual_repondent table in the YPCCC database.
#' @param input_geo_filepath Path to the .rda file of the geographic data
#' @param geotype Type of the geographic data. Possible values: "state", "county", "tract", "cd116", "sldl", "sldu", etc.
#' @param lat_long_columns Column names for longitude and latitude in the target_df. Default = c("long", "lat")
#' @param input_crs Coordinate system of the survey coordinates. Default = EPSG 4326
#' @param target_crs Coordinate system of the geograohic data. Default = EPSG 4269
#'
#' @return Tibble dataframe with a column for the geographic unit
#'
#' @import sf
#' @import dplyr
#' @import tibble
#'
#' @export

add_geo_unit <- function(target_df, input_geo_filepath, geotype, lat_long_columns = c("long", "lat"), input_crs = 4326, target_crs = 4269) {

  geounit <- readRDS(input_geo_filepath) # EPSG: 4269

  sf_data <- st_as_sf(target_df, coords = lat_long_columns,
                      crs = input_crs, agr = "constant", na.fail = FALSE, remove = FALSE)

  df.table_points <- st_transform(sf_data, target_crs)
  st_crs(df.table_points) <- st_crs(geounit)

  joined <- st_join(df.table_points, geounit)

  geodata <- joined %>%
    select(one_of(names(target_df), "GEOID")) %>%
    as_tibble()

  names(geodata)[names(geodata) == "GEOID"] <- geotype

  return(select(geodata, -geometry))

}
