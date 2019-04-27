
#' Quick connect
#'
#' Make a connection to the gis_course data base quickly.
#'
#' @param No parameters needed
#' @return Just returns the connection
#' @examples
#' conn<-connect()
#'

connect<-function(db="gis_course"){

  library(RPostgreSQL)
  library(rpostgis)
  library(maptools)
  library(raster)
  library(sf)
  conn <- dbConnect("PostgreSQL", host = "postgis",
                    dbname = db ,user = "gis_course", password = "gis_course123")
  return(conn)
}

#' Make super user connection
#'
#' Use with great care, as this gives permsissions to drop tables.
#'
#' @param db  Default db is gis_course
#'
#' @return
#' @export
#'
#' @examples conn<-sconnect()
sconnect<-function(db="gis_course"){
  library(RPostgreSQL)
  library(maptools)
  library(raster)
  conn <- dbConnect("PostgreSQL", host = "postgis",
                    dbname = db ,user = "docker", password = 'docker')
  return(conn)
}


#' Super user: Make a new data base
#'
#'
#' Use with great care. Will wipe out prexisting data base!
#' @param db
#'
#' @return
#' @export
#'
#' @examples make_db("gis_course2)
make_db<-function(db="gis_course3")
{
  system(sprintf("PGPASSWORD=docker dropdb -U docker -h postgis %s",db))
  system(sprintf("PGPASSWORD=docker createdb -U docker -h postgis --template=template_postgis %s",db))

}


#' Add shape files to data base
#'
#'
#' Note that by default the table name will
#' be the shapefile name without the extension.
#' Use your own table name, tabnm= "mytable" if you want to change this.
#'
#' @param flnm
#' @param pth
#' @param srid
#' @param tabnm
#' @param db
#'
#' @return
#' @export
#'
#' @examples
#'
add_shp<-function(flnm= "Ancient_Woodlands_England.shp",pth="big_data/shapefiles",srid=27700,tabnm="",db="gis_course"){

  if(tabnm=="")tabnm<-gsub(".shp","",flnm)
  tabnm<-tolower(tabnm)
  sprintf("PGPASSWORD=docker shp2pgsql -s %s -d -I %s/%s  %s |
          PGPASSWORD=docker psql -h postgis -U docker -d %s",srid,pth,flnm,tabnm,db)
}


#' Load all rasters to a table.
#'
#'
#' Takes a vector of file names and a table name
#' Pushes all the files into the same table on the db on the server
#'
#'
#' @param fls
#' @param tabnm
#' @param dbn
#' @param srid
#'
#' @return
#' @export
#'
#' @examples
merge_rasters<-function(fls=fls,tabnm="dsm2m",dbn=db,srid=27700){
  com<-sprintf("PGPASSWORD=docker raster2pgsql -d -s %s %s %s |
  PGPASSWORD=docker psql -h postgis -U docker -d %s",srid,fls[1],tabnm,dbn)
  system(com)
  for(i in 2:length(fls)){
    com<-sprintf("PGPASSWORD=docker raster2pgsql -a -s %s %s %s |
               PGPASSWORD=docker psql -h postgis -U docker -d %s",srid,fls[i],tabnm,dbn)
    system(com)}
}




#' Quick map.
#'
#' Quckly form a map using geocoding to any typed place name
#'
#' @param place
#'
#' @return A mapview map
#'
#' @examples
#' qmap("Arne Dorset)
#'

qmap<-function(place="Bournemouth"){
  require(leaflet.extras)
  g<-tmaptools::geocode_OSM(place)
  mp<-mapview::mapview(g$bbox, alpha.regions = 0,alpha=0)
  mp@map <-addFullscreenControl(mp@map)
  mp
}

#' Quick add leaflet extras
#'
#' @param mp A map
#'
#' @return
#' @export
#'
#' @examples qmap("Bournemouth") %>% extras()
extras<-function(mp){
  require(leaflet.extras)
  require(dplyr)
  mp@map %>% addSearchOSM(options = searchOptions(autoCollapse = TRUE, minLength = 2)) %>%
    addFullscreenControl() %>%
    addMiniMap(position = "bottomleft",tiles="OpenStreetMap.HOT",zoomLevelOffset = -5, toggleDisplay=TRUE) %>%
    addMeasure(primaryLengthUnit ='meters', secondaryLengthUnit='kilometers',primaryAreaUnit='hectares', secondaryAreaUnit='acres',position="topleft")

}

draw<-function(mp){
  require(leaflet.extras)
  require(dplyr)
  mp@map %>%
addDrawToolbar(
  targetGroup = "draw",
  editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions())
) %>%
  addLayersControl(
    overlayGroups = c("draw"), options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addStyleEditor()
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


#' Add priority habitat and SSSi WMS
#'
#' Should add the basic WMS layers from the goeserver
#'
#' @param m
#'
#' @return
#' @export
#'
#' @examples
uK_geo<-function(m){
  require(leaflet.extras)
  require(mapview)
m@map %>% addFullscreenControl() %>% addWMSTiles(group="Priority habitat",
                        "http://r.bournemouth.ac.uk:8083/gis_course/wms",
                        layers = "gis_course:ph_v2_1",
                        options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%
  addWMSTiles(group="SSSI",
              "http://r.bournemouth.ac.uk:8083/gis_course/wms",
              layers = "gis_course:sssi",
              options = WMSTileOptions(format = "image/png", transparent = TRUE)) %>%

  mapview:::mapViewLayersControl (names = c("Priority habitat","SSSI")) -> mymap
return(mymap)}


#' Quick  edit
#'
#' Quickly draw a geometry and save it in the data base
#' Only works interactively: Should not run by mistake in markdown.
#' Notice that is wrie is true the results are written to the connected
#' data base on conn to a table called my_edits.
#' This will overwrite any previous my_edits layer unless
#' a different name is given for the table.
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
    mapview(geocode_OSM(place)$bbox, alpha.regions = 0, alpha=0) %>% editMap() -> edits
    if(write) st_write(edits$drawn,conn, table,overwrite=TRUE)
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

#' Choose a type of priority habitat
#'
#' Give a place, a radius and some words for the phabitat
#'
#' @param place
#' @param phab
#' @param dist
#'
#' @return
#' @export
#'
#' @examples phab_choose %>% mapview()
phab_choose<-function(place="Bournemouth",phab="heath", dist=5000)
{
  quer <-sprintf("select * from ph_v2_1 where
    main_habit ilike '%s%s%s'",'%',phab,'%')
  dquery(place=place,dist=dist,query =quer)
}
