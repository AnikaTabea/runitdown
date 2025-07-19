#' Plot Pace Profile from GPX
#'
#' This function reads a .gpx file and plots the pace (min/km) over time.
#'
#' @param gpx_file Path to the .gpx file.
#'
#' @return A ggplot object showing pace over time.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_pace_profile("example.gpx")
#' }
plot_pace_profile <- function(gpx_file) {
  library(xml2)
  library(dplyr)
  library(lubridate)
  library(sf)
  library(ggplot2)

  # Read and parse GPX
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

  ele <- as.numeric(xml_text(ele_nodes))
  time <- ymd_hms(xml_text(time_nodes), tz = "UTC")

  df <- tibble(lat, lon, ele, time) %>%
    filter(!is.na(lat), !is.na(lon), !is.na(time)) %>%
    arrange(time)

  # Convert to sf and transform to projected CRS
  sf_df <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326, remove = FALSE)
  sf_proj <- st_transform(sf_df, crs = 32632)

  # Calculate distance between points
  coords <- st_geometry(sf_proj)
  dists <- st_distance(coords[-length(coords)], coords[-1], by_element = TRUE)
  dist_vec <- as.numeric(dists)

  # Build pace data
  df <- df[-1, ]  # drop first point (no previous point to compare)
  df <- df %>%
    mutate(
      delta_dist_km = dist_vec / 1000,
      elapsed_time_sec = as.numeric(difftime(time, min(time), units = "secs")),
      cum_dist_km = cumsum(delta_dist_km),
      pace_min_per_km = (elapsed_time_sec / 60) / cum_dist_km
    )

  # Plot
  ggplot(df, aes(x = elapsed_time_sec / 60, y = pace_min_per_km)) +
    geom_line(color = "steelblue", linewidth = 1) +
    labs(
      title = "Pace Profile",
      x = "Elapsed Time (min)",
      y = "Pace (min/km)"
    ) +
    theme_minimal()
}
