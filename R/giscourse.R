
#'
#'
#' Make a connection to the gis_course data base quickly.
#'
#' @param No parameters needed
#' @return returns the connection
#' @examples
#' conn<-connect()
#'

connect<-function(db="gis_course"){

  library(RPostgreSQL)
  library(maptools)
  library(raster)
  conn <- dbConnect("PostgreSQL", host = "postgis",
                    dbname = db ,user = "gis_course", password = "gis_course123")
  return(conn)
}

sconnect<-function(pwd="docker", db="gis_course"){
  library(RPostgreSQL)
  library(maptools)
  library(raster)
  conn <- dbConnect("PostgreSQL", host = "postgis",
                    dbname = db ,user = "docker", password = pwd)
  return(conn)
}


make_db<-function(db="gis_course2")
{
  system(sprintf("PGPASSWORD=docker dropdb -U docker -h postgis %s",db))
  system(sprintf("PGPASSWORD=docker createdb -U docker -h postgis --template=template_postgis %s",db))

}


add_shp<-function(flnm= "Ancient_Woodlands_England.shp",pth="big_data/shapefiles",srid=27700,tabnm="",db="gis_course"){

  if(tabnm=="")tabnm<-gsub(".shp","",flnm)
  tabnm<-tolower(tabnm)
  sprintf("PGPASSWORD=docker shp2pgsql -s %s -d -I %s/%s  %s |
          PGPASSWORD=docker psql -h postgis -U docker -d %s",srid,pth,flnm,tabnm,db)
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

#' Add Hansen's deforestation maps to tropical areas in WMS format
#' This is for visualisation only
#' @param m
#'
#' @return
#' @export
#'
#' @examples qmap("Sumatra") %>% hansen_wms()
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



#' Quickly draw a geometry and save it in the data base
#' Only works interactively: Not run in markdown.
#' @param place Defaults to Bournemouth. Geocoded.
#' @param write Defaults to TRUE.Writes to the course data base
#' @param table Name of the table in the data base to write to
#'
#' @return A geometry
#' @export
#'
#' @examples
emap<-function(place="Bournemouth",write=TRUE,table="my_edits")
{
  if(interactive()){
    require(mapedit)
    require(mapview)
    require(tmaptools)
    mapview(geocode_OSM(place)$bbox) %>% editMap() -> edits
    if(write) write_sf(edits$drawn,conn, table,overwrite=TRUE)
    return(edits$drawn)
    }

}


#' Distance query
#' Finds all geometries for a query lying within a distance of a
#' place, found through Geocoding
#'
#' @param place
#' @param dist
#' @param query
#'
#' @return
#' @export
#'
#' @examples dquery() %>% qtm()
dquery<-function(place="Hengistbury head",dist=1000,query="select * from ph_v2_1"){
  require(tmaptools)
  require(sf)
  g<-geocode_OSM(place)
  pnt<-sprintf("select st_transform(st_setsrid(st_makepoint(%s,%s),4326),27700) geom",g$coords[1],g$coords[2])
  query<-sprintf("select s1.* from (%s) s1, (%s) s2
where st_dwithin(s2.geom, s1.geom, %s) ",query,pnt,dist)
  st_read(conn, query=query)
}

