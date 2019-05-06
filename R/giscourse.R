
#' Quick type disconnect
#'
#' @param con
#'
#' @return
#' @export
#'
#' @examples disconnect(). Assuming con is called conn
disconnect<-function(con=conn){dbDisconnect(con)}

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
  library(mapview)
  library(leaflet.extras)
  library(tmap)
  library(tmaptools)
  require(dplyr)

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
  library(sf)
  library(mapview)
  library(tmap)
  library(tmaptools)
  require(dplyr)

  conn <- dbConnect("PostgreSQL", host = "172.16.49.31", port=25432,
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

uk_wms<-function(m){
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
    addWMSTiles(group="Land Cover 2015",
                "http://r.bournemouth.ac.uk:8083/gis_course/wms",
                layers = "gis_course:lcm2015gbvector",
                options = WMSTileOptions(format = "image/png", transparent = TRUE))%>%

    mapview:::mapViewLayersControl (names = c("Priority habitat","SSSI", "Land Cover 2015")) -> mymap
  return(mymap)}


#' Quick  edit
#'
#' Quickly draw a geometry and save it in the data base
#' Only works interactively: Should not run by mistake in markdown.
#' Notice that if write is true (default) the results are written to the connected
#' data base on conn to a table called emap
#' This will overwrite the previous emap layer. So if you want to save
#' permanently provide a different name for the table.
#' The table goes in the public schema. You may want to move it later to another schema.
#'
#' @param place Defaults to Bournemouth. Geocoded.
#' @param write Defaults to TRUE.Writes to the course data base
#' @param table Name of the table to write. Defaults to emap
#'
#' @return A geometry
#' @export
#'
#' @examples
emap<-function(place="Bournemouth",write=TRUE,table="emap")
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
  crs<-st_crs(st_read(conn,query = paste(query,"limit 1")))[[1]]
  g<-geocode_OSM(place)
  pnt<-sprintf("select st_transform(st_setsrid(st_makepoint(%s,%s),4326),%s) geom",g$coords[1],g$coords[2],crs)
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


#' Getting World Clim data
#'
#' Pulls out worldclim prec, tmin and tmax from the layers
#'
#'
#' @param place  Defaults to Bournemouth
#' @param res  Resolution defaults to 10
#'
#' @return Returns a data frame
#' @export
#'
#' @examples
getwclim<-function(place="Bournemouth",res=2.5) {
  require(raster)
  require(dismo)
  path<- "~/geoserver/data_dir/rasters/worldclim"
  prec<-getData(name = "worldclim", var = "prec", res = res,path=path)
  tmin<-getData(name = "worldclim", var = "tmin", res = res,path = path)
  tmax<-getData(name = "worldclim", var = "tmax", res = res,path=path)

  require(tmaptools)
  pnt<-as(geocode_OSM(place,as.sf=TRUE),"Spatial")
  tmins<-unlist(raster::extract(tmin,pnt)/10)
  tmaxs<-unlist(raster::extract(tmax,pnt)/10)
  precs<-raster::extract(prec,pnt)
  df<-data.frame(prec= as.vector(precs),tmax = as.vector(tmaxs),tmin=as.vector(tmins))
  df
}


#' Walter and Leith diagram
#'
#' Produces a simple WL diagram
#' Takes the df from getwlim
#' @param df
#'
#' @return
#' @export
#'
#' @examples  getwclim() %>% wldiag()
#'
#'
wldiag<-function(df){
  require(climatol)
  mat<-rbind(df$prec,df$tmax,df$tmin,df$tmin)
  diagwl(mat)
}
