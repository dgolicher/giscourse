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
