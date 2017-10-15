#### create new waypoints ####
triangles <- read.csv("~/Downloads/trianglesConvertforGPS.csv")

waypoints <- readLines("~/Downloads/xmlhead")

p1 <- "<wpt lat=\""
p2 <- "\" lon=\""
p3 <- "\"><ele></ele><time></time><name>"
p4 <- "</name><sym>"
symbol <- "Flag, Blue"
p5 <- "</sym></wpt>"

for (i in 1:length(triangles$Name)){
  name <- triangles$Name[i]
  lat <- as.character(triangles$Latitude[i])
  lon <- as.character(triangles$Longitude[i])
  point <- paste(p1, lat, p2, lon, p3, name, p4, symbol, p5, sep="")
  waypoints <- append(waypoints, point)
}

waypoints <- append(waypoints, "</gpx>")

writeLines(waypoints, "~/Downloads/allFENwaypoints.gpx")

#### extract data from waypoints to csv ####
rm(list=ls())
fileslist <- c("09-MAY-17", 
               "10-MAY-17", 
               "11-MAY-17", 
               "12-MAY-17",
               "13-MAY-17",
               "15-MAY-17",
               "16-MAY-17",
               "17-MAY-17",
               "18-MAY-17",
               "19-MAY-17",
               "20-MAY-17",
               "21-MAY-17")
filenames <- c()
for (i in 1:length(fileslist)) {
  name <- paste("~/Documents/Waypoints/Waypoints_",fileslist[i], ".gpx", sep="")
  filenames <- append(filenames, name)
}

splits <- c("<wpt lat=\"", "\" lon=\"", "\"><ele>", "</ele><time>", "</time><name>", "</name><sym>","</sym>")
point <- c("")
csvpoints <- as.data.frame(matrix(c(rep("",6)), ncol=6))
colnames(csvpoints) <- c("lat", "lon", "elev", "date", "name", "sym")

for (i in 1:length(filenames)){
  # read in the file and remove the first part
  file <- readLines(filenames[i]) 
  file <- strsplit(file, "</metadata>")
  data <- file[[1]][2]
  
  # split the data into multiple waypoints
  data <- strsplit(data, "</wpt>")
  data <- data[[1]]
  data <- data[1:(length(data)-1)]
  length(data)
  # take each individual waypoint and split it into components
  for (j in 1:length(data)) {
    point <- data[j] 
    for (k in 1:length(splits)){
      point <- gsub(splits[k], point, replacement="splithere ")
    }
    splitpoint <- strsplit(point, "splithere ")
    splitpoint <- splitpoint[[1]]
    lat <- as.numeric(splitpoint[2])
    lon <- as.numeric(splitpoint[3])
    elev <- as.numeric(splitpoint[4])
    date <- splitpoint[5]
    name <- splitpoint[6]
    sym <- splitpoint[7]
    
    # turn that point data into a data frame
    pointdata <- as.data.frame(matrix(c(lat, lon, elev, date, name, sym), ncol=6, byrow=TRUE))
    colnames(pointdata) <- c("lat", "lon", "elev", "date", "name", "sym")
    csvpoints <- rbind(csvpoints, pointdata)
  }
}

csvpoints <- csvpoints[-1,]
