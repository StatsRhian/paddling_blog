gpoly_to_sfpoly = function(gpoly){
  coords = googlePolylines::decode(gpoly)
  sfg = lapply(coords, function(x) sf::st_linestring(x = as.matrix(x[, c(2, 1)])))
  sfc = sf::st_sfc(sfg, crs = 4326)
  return(sfc)
}

strava_token = httr::config(token = rStrava::strava_oauth("rStrava",
                                            app_client_id = Sys.getenv("stravaID"),
                                            app_secret = Sys.getenv("stravaSecret"),
                                            app_scope="activity:read_all"))

my_acts <- rStrava::get_activity_list(strava_token) |>
  rStrava::compile_activities() |>
  tibble::as_tibble()

paddles <-
  my_acts |>
  dplyr::filter(type == "Kayaking") |>
  dplyr::filter(!is.na(map.summary_polyline) & map.summary_polyline != "") |>
  dplyr::mutate(geom = gpoly_to_sfpoly(map.summary_polyline)) |>
  dplyr::select(name, start_date, distance, geom) |>
  sf::st_as_sf() |>
  sf::st_set_crs(4326)

sf::st_write(paddles, "R/paddles.geojson", append = FALSE)


