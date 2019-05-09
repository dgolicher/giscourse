

#' Query within a distance from X and Y
#'
#' X is Lon, Y is Lat
#' Default query get's priority habitat
#'
#' @param x
#' @param y
#' @param dist
#' @param query
#'
#' @return
#' @export
#'
#' @examples Deprecated See mkpnt instead
#'
xyquery<-function (x=-1.95,y=50.667, dist = 1000, query = "select * from ph_v2_1")
{
  require(tmap)
  require(tmaptools)
  require(sf)

  pnt <- sprintf("select st_transform(st_setsrid(st_makepoint(%s,%s),4326),27700) geom",
                 x, y)
  query <- sprintf("select s1.* from (%s) s1, (%s) s2 where
                   st_dwithin(s2.geom, s1.geom, %s) ",
                   query, pnt, dist)
  st_read(conn, query = query)
}


#' Make a point
#'
#' This is a quick utility function that feeds into a workflow
#' It takes either a string or two numbers
#' If a string is provided the position is based on geocoding
#' If x and y are provided they are assumed to be lon lat, (or lat lon)
#' As this is just for the UK we can switch if they are typed in the wrong order
#' by testing if x > y-
#'
#' @param x If x is a string then the Geocoding is used to find the centre
#' @param y  If x and y are provided as numbers they are assumed to be lon and lat
#'
#'
#' @return Sf point in British National grid
#' @export
#'
#' @examples
#' mkpnt(x="Wareham, Dorset")
#' mkpnt(x=-1,y=52)
#'
#'
mkpnt<-function(x="Wareham, Dorset",y=0){
  if(class(x)=="character"){
    require(tmaptools)
    require(sf)
    g<-geocode_OSM(x)
    d<-data.frame(id=1,x=g$coords[1],y=g$coords[2])
  }

  if(class(x)=="numeric"){
    d<-data.frame(id=1,x=x,y=y)
    if(x>y){d<-data.frame(id=1,x=y,y=x)}
  }

st_as_sf(d,coords=c("x","y"),crs=4326) -> pnt
pnt<-st_transform(pnt,27700)
pnt

}

#' OS 5km grid square
#'
#' Uses same input rules as mkpnt as it feeds into it.
#' Provide either some text or lat long coordinates
#'
#' @param x
#' @param y
#'
#' @return
#' @export
#'
#' @examples
#' os5km("Wareham,Dorset")
#'
#'
os5km<-function(x="Wareham,Dorset",y=0){

  pnt<-mkpnt(x,y)
  write_sf(pnt,conn,"tmp",overwrite=TRUE)
  query<-"select o.* from os5km o, tmp t
  where st_intersects(o.geometry,t.geometry)"
  st_read(conn,query=query)
}


#' Make a site frame
#'
#' Same rules for x and y as in mkpnt
#' If a distance is provided then it is used as a buffer around the point
#' If a distance is not provided then the os5km grid square is used.
#'
#' @param x
#' @param y
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
mksite<-function(x="Arne,Dorset",y=0,dist=0,bx=FALSE){
  if(dist==0){
    site<- os5km(x,y)
  }
  if(dist>0){
    site<- st_buffer(mkpnt(x,y),dist)
  }
  write_sf(site,conn,"site",overwrite=TRUE)
  site
}



#' Gets the landcover for a site
#'
#' Uses the same rules for input as mksite.
#' Provide either a string or lat long coordinates.
#'
#'
#' @param x
#' @param y
#' @param dist
#'
#' @return Landcover clipped to the site
#' @export
#'
#' @examples
#'
#' landcover("Arne,Dorset"dist=1000)
#'
landcover<-function(x="Arne,Dorset",y=0,dist=0){
  site<-mksite(x,y,dist)
  query<- "select s.*, st_area(s.geom) area from
(select l.gid,bhab,
 st_intersection(l.geom,t.geometry) geom
 from
  lcm2015gbvector l,
  site t
  where st_intersects(l.geom,t.geometry)) s"
  lcover<-st_read(conn,query=query)
  dbSendQuery(conn, "drop table site")
  lcover
}

phabitat<-function(x="Arne,Dorset",y=0,dist=0){
  site<-mksite(x,y,dist)
  query<- "select s.*, st_area(s.geom) area from
(select p.gid,main_habit,
 st_intersection(p.geom,t.geometry) geom
 from
  ph_v2_1 p,
  site t
  where st_intersects(p.geom,t.geometry)) s"
  phabitat<-st_read(conn,query=query)
  dbSendQuery(conn, "drop table site")
  phabitat
}

#' Find sites of special scientific interest
#'
#' @param x
#' @param y
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
sssi<-function(x="Arne,Dorset",y=0,dist=0){
  site<-mksite(x,y,dist)
  query<- "select s.*, st_area(s.geom) area from
  (select p.gid,sssi_name,p.geom geom
  from
  sssi p,
  site t
  where st_intersects(p.geom,t.geometry)) s"
  sssi<-st_read(conn,query=query)
  dbSendQuery(conn, "drop table site")
  sssi
}

#' Find osm roads and paths around a site
#'
#' @param x
#' @param y
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
#'
osm<-function(x="Arne,Dorset",y=0,dist=0){
  site<-mksite(x,y,dist)
  query<- "select highway,st_intersection(p.geom,geometry) geom
  from
  (select highway,way geom from dorset_line where highway is not NULL) p,
  site t
  where st_intersects(p.geom,t.geometry)"
  osm<-st_read(conn,query=query)
  dbSendQuery(conn, "drop table site")
  osm
}


#' Get's DEM around point
#'
#' z is set to 12 by default (12 m)
#' Use value of 14 for highest resolution (3m)
#'
#' @param x
#' @param y
#' @param dist
#' @param z
#'
#'
#' @return
#' @export
#'
#' @examples
#'
#' dtm<-mkdem()
#'
mkdem<-function(x="Arne,Dorset",y=0,dist=0,z=12)
{
  require(elevatr)
  site<-mksite(x,y,dist)
  dem <- get_elev_raster(site, z = z,clip="bbox")
  dem
}


#' Direct beam insolation for day of the year
#'
#' Calculated using SAGA
#' Returns a raster pointing to a temporary file
#' Save locally if you need it to be permanent
#'
#' @param dtm
#' @param day
#'
#' @return
#' @export
#'
#' @examples
#'
#' dtm<-mkdem()
#' sol<-insol(dtm)
#'
insol<-function(dtm,day="13/10/19"){
  require(raster)
  infl<-tempfile(fileext = ".sgrd")
  outfl<-tempfile()
  writeRaster(dtm,infl,format="SAGA", overwrite=TRUE)
  com<-sprintf("saga_cmd ta_lighting 2 -GRD_DEM %s  -DAY '%s' -GRD_DIRECT    %s",infl,day,outfl)
  system (com)
  sol<-raster(paste(outfl,"sdat",sep="."))
  sol@crs<-dtm@crs
sol}


#' Title
#'
#' @param dtm
#'
#' @return raster layer in temporary directory
#' Save locally using writeRaster if needed
#' @export
#'
#' @examples
#'
#'
twi<-function(dtm){
  require(raster)
  infl<-tempfile(fileext = ".sgrd")
  outfl<-tempfile()
  writeRaster(dtm,infl,format="SAGA", overwrite=TRUE)
  com<-sprintf("saga_cmd ta_hydrology 15 -DEM %s  -TWI   %s",infl,outfl)
  system (com)
  twi<-raster(paste(outfl,"sdat",sep="."))
  twi@crs<-dtm@crs
  twi}



