
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

