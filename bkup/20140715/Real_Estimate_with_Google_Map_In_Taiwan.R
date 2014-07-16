rm(list=ls())
library(RgoogleMaps)
data  = data.frame(read.csv( 'D:\\統計研究\\20140712/A_lvr_land_A.csv',header=T))
#colnames(data)
#data[1:2,]
################ filter ###########################
draw.data = data[grep(pattern="文山區", x=data$鄉鎮市區 ),]
draw.data = draw.data[ !(draw.data$交易標的%in%"土地" | draw.data$交易標的%in%"車位"), ]  
draw.data = draw.data[which(draw.data$交易年月%in%10303),]
draw.data = draw.data[which(draw.data$都市土地使用分區%in%"住"),]
#draw.data = draw.data[which(draw.data$都市土地使用分區%in%"住"),]

lat = NULL
lon = NULL
col = NULL
data = NULL
location = NA
  for( i in 1:nrow(draw.data) ) { #i = 1  i=79
    cat(i)
    cat("\n")
    address = as.character(draw.data$土地區段位置或建物區門牌[i])
    location = getGeoCode(URLencode( iconv(address,to='UTF-8') ),verbose=1)
      while( is.na(location[1]) ) {
        location = getGeoCode(URLencode( iconv(address,to='UTF-8') ),verbose=1)
      }    
    lat = c(lat,location[1])
    lon = c(lon,location[2])
    col = c(col,"red")    
    data = rbind( data, c(i,address,location[1],location[2]) )
  }
address = "臺北市文山區"
location = getGeoCode(URLencode( iconv(address,to='UTF-8') ),verbose=1)
DefineMap <- GetMap(center=location, zoom=13, destfile = "Taipei_Address.png");
par(cex=1.5)
  if(0) {
PlotOnStaticMap(DefineMap, 
                destfile = "Taipei_Mark_Address.png", cex=1.5,pch=20,
                add=FALSE);


  PlotOnStaticMap(DefineMap, 
                  lat = lat,
                  lon = lon,
                  destfile = "Taipei_Mark_Address.png", cex=1.5,pch=20,
                  col=col, add=FALSE)
  }
lat.lon.map = data.frame( "latitude" = lat,
                          "longitude" = lon,
                          "dollars" = as.numeric(draw.data$單價每平方公尺)
                          )

tmp = LatLon2XY.centered(MyMap = DefineMap, 
                   lat = lat, 
                   lon = lon, 
                   zoom = 13)

lat.lon.map = data.frame( lat.lon.map , "x" = tmp$newX , "y" = tmp$newY )
lat.lon.map = na.exclude(lat.lon.map)

lat.lon.map.tmp = aggregate(lat.lon.map$dollars,list(lat.lon.map$x,lat.lon.map$y),mean)
colnames(lat.lon.map.tmp) = c( "x" , "y" , "dollars" ) 

#######################
library(kriging)
kriged <- kriging(x=lat.lon.map.tmp$x, 
                  y=lat.lon.map.tmp$y, 
                  response=lat.lon.map.tmp$dollars, 
                  pixels=300)
image.plot(kriged, 
           xlim = extendrange(lat.lon.map.tmp$x), 
           ylim = extendrange(lat.lon.map.tmp$y), 
           zlim=range(lat.lon.map.tmp$dollars)
           )
#######################

#key.entries = round(quantile(draw.data$單價每平方公尺, (1:6)/6),0)
#key.entries = round(exp( quantile( log( draw.data$單價每平方公尺 ) , (1:7)/7 ) ),0)
key.entries = c(50000,100000,150000,200000,250000)
bubbleMap(lat.lon.map, 
          coords = c("longitude","latitude"), 
          map=DefineMap,
          zcol="dollars", 
          key.entries = key.entries,
          max.radius = 100,
          do.sqrt = F);

















#return the google map url
MapBackground(lat=24.98478,
              lon=121.5621,
              zoom=13,
              destfile="Taipei_Address.png",
              PLOT=T)

tmp = LatLon2XY.centered(MyMap = DefineMap, 
                   lat = 24.98478, 
                   lon = 121.5621, 
                   zoom = 13)

points( tmp$newX , tmp$newY )


?bubbleMap


