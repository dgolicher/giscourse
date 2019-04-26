

connect<-function(){
  library(RPostgreSQL)
  library(maptools)
  library(raster)
  conn <- dbConnect("PostgreSQL", host = "postgis",
  dbname = "gis_course" ,user = "gis_course", password = "gis_course123")
  return(conn)
}
=======
>>>>>>> aa5a16e93f33f55659a8f6267236b34ede962f74
