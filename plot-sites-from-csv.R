#clear the workspace and load libraries
rm(list=ls())
spatial.packages <- c("maps", 
                      "maptools", 
                      "dplyr", 
                      "ggmap", 
                      "rgdal", 
                      "ggplot2", 
                      "RgoogleMaps", 
                      "raster")
lapply(spatial.packages, require, character.only = TRUE)

# load in site coordinates
sitedata <- read.csv("~/Documents/FENsites/triangles_basefile.csv")
sitedata <- sitedata[1:32,]
sitenames <- sitedata$Site

# function for plot export 
savePlot <- function(plot, filename) {
  jpeg(file=filename, 
       res=150, width=8, height=8, 
       units="in", quality=90)
  print(plot)
  dev.off()
}


# for loop to assign coordinates to sites, make and save plots
for (i in 1:length(sitedata$Site)){
  coords <- sitedata[i,5:32]
  data <- as.data.frame(matrix(c(coords), ncol=2, byrow=TRUE))
  rownames(data) <- c("parking", "center", 
                      "25ma", "25mb", "25mc", 
                      "125ma", "125mb", "125mc", 
                      "280ma", "280mb", "280mc", 
                      "625ma", "625mb", "625mc")
  colnames(data) <- c("lat", "long")
  newlong <- c()
  newlat <- c()
  for (x in 1:length(data$lat)) {
    newlong <- append(newlong, data$long[[x]][1])
    newlat <- append(newlat, data$lat[[x]][1])
  }
  data$long <- newlong
  data$lat <- newlat
  
  map <- get_map(c(lon=data$long[2], lat=data$lat[2]), 
                 zoom=16, maptype="hybrid", source="google") 
  
  ##### if you want to make more zoomed in maps, increase zoom in get_map
  ##### the maximum zoom is 20
  ##### you will get a warning message that some geom_points are missing
  ##### you can ignore that warning - it just means they're outside the zoom
  
  plot <- ggmap(map) + 
    geom_point(aes(x=long, y=lat),
               color="black", size=1,
               data=data)+
    geom_polygon(aes(x=long, y=lat),
                 color="blue", alpha=0,
                 data=data[12:14,])

  place <- as.character(sitedata[i,2])
  filename <- paste("~/Documents/FENsites/maps/", 
                    place,
                    ".jpg", sep="")
  savePlot(plot, filename)
}

