#' Plot Running Route from GPX
#'
#' This function reads a .gpx file and plots the running route using the 'tmap' package and OpenStreetMap tiles.
#'
#' @param gpx_file
#'
#' @return A tmap map object displaying the running route.
#' @export
#'
#' @examples
#' \dontrun{
#' plot_route_map("example.gpx")
#' }
plot_route_map <- function(gpx_file) {
  library(xml2)
  library(dplyr)
  library(sf)
  library(tmap)

  # Parse GPX file with namespace
  xml <- read_xml(gpx_file)
  ns <- c(d1 = "http://www.topografix.com/GPX/1/1")
  trkpts <- xml_find_all(xml, ".//d1:trkpt", ns = ns)

  if (length(trkpts) < 2) {
    stop("Not enough trackpoints found in the GPX file.")
  }

  lat <- as.numeric(xml_attr(trkpts, "lat"))
  lon <- as.numeric(xml_attr(trkpts, "lon"))

  df <- tibble(lat, lon) %>%
    filter(!is.na(lat), !is.na(lon))

  if (nrow(df) < 2) {
    stop("Not enough valid coordinates after filtering.")
  }

  # Convert to sf LINESTRING
  sf_points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  route <- sf_points %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("LINESTRING")
tmap_mode("view") # view mode
  # Create map
  tm_shape(route) +
    tm_basemap("OpenStreetMap") +
    tm_lines(col = "red", lwd = 3) +
    tm_layout(title = "Running Route", frame = FALSE)
}
