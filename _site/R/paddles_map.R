library("rStrava")
library("dplyr")

stoken <- httr::config(token = strava_oauth("rStrava",
                                            app_client_id = Sys.getenv("stravaID"),
                                            app_secret = Sys.getenv("stravaSecret"),
                                            app_scope="activity:read_all"))

my_acts <- get_activity_list(stoken)

acts = 
my_acts %>%
  compile_activities()

gpoly_to_sfpoly = function(gpoly){
  coords <- googlePolylines::decode(gpoly)
  sfg <- lapply(coords, function(x) sf::st_linestring(x = as.matrix(x[, c(2, 1)])))
  sfc <- sf::st_sfc(sfg, crs = 4326)
  return(sfc)
}

paddles = 
  acts %>%
  filter(type == "Kayaking") %>%
  filter(!is.na(map.summary_polyline)) %>%
  mutate(geom = gpoly_to_sfpoly(map.summary_polyline)) %>%
  select(name, geom) %>%
  st_as_sf() %>%
  st_set_crs(4326)

st_write(paddles, "paddles.shp")
