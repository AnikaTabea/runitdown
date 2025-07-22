
#' Summarize GPX Run
#'
#' This function reads a .gpx file and calculates key run metrics like distance, duration, average pace and elevation gain.
#' It uses the 'sf' package to compute geospatial distances.
#'
#' @param gpx_file Path to the .gpx file.
#'
#' @return A named list with total distance (km), duration (hh:mm:ss), average pace (min/km) and elevation gain (m).
#' @export
#'
#' @examples
#' \dontrun{
#' summarize_run("example.gpx")
#' }
summarize_run <- function(gpx_file) {
  library(xml2)
  library(dplyr)
  library(lubridate)
  library(sf)

  # Read and parse GPX with namespace
  xml <- read_xml(gpx_file)
  ns <- c(d1 = "http://www.topografix.com/GPX/1/1")
  trkpts <- xml_find_all(xml, ".//d1:trkpt", ns = ns)

  if (length(trkpts) < 2) {
    stop("Not enough trackpoints found in the GPX file.")
  }

  lat <- as.numeric(xml_attr(trkpts, "lat"))
  lon <- as.numeric(xml_attr(trkpts, "lon"))
  ele_nodes <- xml_find_all(trkpts, ".//d1:ele", ns = ns)
  time_nodes <- xml_find_all(trkpts, ".//d1:time", ns = ns)

  if (length(lat) == 0 || length(lon) == 0 || length(ele_nodes) == 0 || length(time_nodes) == 0) {
    stop("Missing required data in GPX file (lat/lon/ele/time).")
  }

  ele <- as.numeric(xml_text(ele_nodes))
  time <- ymd_hms(xml_text(time_nodes), tz = "UTC")

  # Create tibble and filter invalid rows
  df <- tibble(lat, lon, ele, time) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(ele), !is.na(time))

  if (nrow(df) < 2) {
    stop("Not enough valid trackpoints after filtering.")
  }

  # Convert to sf and transform CRS
  sf_df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sf_proj <- st_transform(sf_df, crs = 32632)  # UTM Zone 32N (adjust if needed)

  # Compute distances between consecutive points
  coords <- st_geometry(sf_proj)
  dists <- st_distance(coords[-length(coords)], coords[-1], by_element = TRUE)
  total_dist_km <- sum(as.numeric(dists), na.rm = TRUE) / 1000

  # Duration
  duration_sec <- as.numeric(difftime(max(df$time), min(df$time), units = "secs"))
  duration_fmt <- seconds_to_period(duration_sec)

  # Format duration correctly
  duration_string <- sprintf(
    "%02d:%02d:%02d",
    as.integer(hour(duration_fmt)),
    as.integer(minute(duration_fmt)),
    as.integer(second(duration_fmt))
  )

  # Average pace (min/km)
  pace_min_per_km <- duration_sec / 60 / total_dist_km

  # Elevation gain
  ele_diff <- diff(df$ele)
  elevation_gain <- sum(ele_diff[ele_diff > 0], na.rm = TRUE)

  list(
    distance_km = round(total_dist_km, 2),
    duration = duration_string,
    average_pace = paste0(round(pace_min_per_km, 1), " min/km"),
    elevation_gain_m = round(elevation_gain)
  )
}
