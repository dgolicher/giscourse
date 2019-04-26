hello<-function()

connect<-function(){
  library(RPostgreSQL)
  library(maptools)
  library(raster)
  conn <- dbConnect("PostgreSQL", host = "postgis",
  dbname = "gis_course" ,user = "gis_course", password = "gis_course123")
  return(conn)
}

qmap<-function(place="Bournemouth"){
  g<-tmaptools::geocode_OSM(place)
  mapview::mapview(g$bbox)
}

emap<-function(place="Bournemouth")
{
  require(mapedit)
  require(mapview)
  require(tmaptools)
  mapview(geocode_OSM(place)$bbox) %>% editMap() -> edits
  write_sf(edits$drawn,conn, "my_edits",overwrite=TRUE)
}
