#' Make a connection to the gis_course data base quickly.
#'
#' @param No parameters needed
#' @return returns the connection
#' @examples
#' conn<-connect()
#'

connect<-function(){

  library(RPostgreSQL)
  library(maptools)
  library(raster)
  conn <- dbConnect("PostgreSQL", host = "postgis",
  dbname = "gis_course" ,user = "gis_course", password = "gis_course123")
  return(conn)
}



#' Quckly form a map using geocoding to find a place
#'
#' @param place
#'
#' @return A mapview map
#'
#' @examples
#' qmap("Arne Dorset)
#'
qmap<-function(place="Bournemouth"){

  g<-tmaptools::geocode_OSM(place)
  mapview::mapview(g$bbox)
}

hansen_wms<-function(m){
  require(leaflet.extras)
  require(mapview)
  m@map %>% addFullscreenControl() %>% addMiniMap(position = "bottomleft",zoomLevelOffset = -4, toggleDisplay = TRUE) %>%
    addWMSTiles(group="Forest 2000",
                "http://r.bournemouth.ac.uk:8083/AG/wms",
                layers = "AG:forest2000",
                options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
    addWMSTiles(group="Forest loss",
                "http://r.bournemouth.ac.uk:8083/AG/wms",
                layers = "AG:hansen_loss",
                options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
    mapview:::mapViewLayersControl (names = c("Forest 2000","Forest loss")) -> mymap
  return(mymap)}



emap<-function(place="Bournemouth")
{
  require(mapedit)
  require(mapview)
  require(tmaptools)
  mapview(geocode_OSM(place)$bbox) %>% editMap() -> edits
  write_sf(edits$drawn,conn, "my_edits",overwrite=TRUE)
}

