

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
#' @examples
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
