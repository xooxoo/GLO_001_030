library(dplyr)
library(readr)


# Read our working nodes

nodes_178 <- read_csv('data/nodes_178.csv')

# functions to convert coordinates 
xy_to_lonlat <- function(x, y, angle=0){
  
  R = 6371.116
  a = pi * angle / 180
  sn = sin(a)
  cn = cos(a)
  pp = (y + x * sn / cn) / (cn+ sn * sn/cn) * -10
  yp = (x - pp * sn) / cn * 10
  
  pp = atan2(yp, pp)
  
  Lon = pp*180/pi
  Lat = 90 - 2 * 180/pi*asin(sqrt(x^2 * 100 + y^2 * 100) / 2 / R)
  return(list(Lon, Lat))
}

lonlat_to_xy <- function(lon, lat, angle=0){
  
  a = pi * angle / 180
  if (lon > 180) lon = lon - 360
  
  R = 6371.116
  
  a1 = ((90-lat)*pi/180)/2
  a2 = lon*pi/180
  
  x = (2*R*sin(a1)*sin(a2))/10
  y = -(2*R*sin(a1)*cos(a2))/10

  X = x*cos(a) + y*sin(a)
  Y = y*cos(a) - x*sin(a)

  return(c(X, Y))
}


# add lon and lat to nodes_178 and write it to existing file
nodes_178$Lon <- xy_to_lonlat(nodes_178$X, nodes_178$Y)[[1]]
nodes_178$Lat <- xy_to_lonlat(nodes_178$X, nodes_178$Y)[[2]]

# write_csv(nodes_178[,-1], 'data/nodes_178.csv')


