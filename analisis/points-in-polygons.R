# count points in polygons: https://gis.stackexchange.com/questions/110117/counts-the-number-of-points-in-a-polygon-in-r#110246
# read GeoJSON in R https://stackoverflow.com/questions/24183007/is-it-possible-to-read-geojson-or-topojson-file-in-r-to-draw-a-choropleth-map

#  Lodas libraries
library("raster")
library("sp")
library(rgdal)

# shapes
barrios <- readOGR("data/output/contornos/barrios-madrid.geojson")
proj4string(barrios)

#  points (insideairbnb_listings_madrid_151002, insideairbnb_listings_madrid_180117)
airbnb <- read.delim("data/original/insideairbnb_listings_madrid_151002.csv",sep = ",") 
# Only loads observations that have "completed" values for column "latitude" (a way to avoid NA values)
airbnb  <- airbnb[complete.cases(airbnb[ , c("latitude")]), ]

### Get long and lat from your data.frame. Make sure that the order is in lon/lat.
# source: https://stackoverflow.com/questions/29736577/how-to-convert-data-frame-to-spatial-coordinates#29736844
xy <- airbnb[,c("longitude","latitude")]
airbnbSp <- SpatialPointsDataFrame(coords = xy, data = airbnb, proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# serves to count points (idealista) in polygon (barrios)
# retrieve  the  geometry  indices  of Barrios at  the  locations  of A (idealista points).  
# More  in particular, an integer vector of length length(A) is returned, with NA values for locations in 
# A not matching with locations in B (e.g.  those points outside a set of polygons).
# (https://cran.r-project.org/web/packages/sp/vignettes/over.pdf) 
countBarrios <- over(airbnbSp, barrios)
table(countBarrios$NOMBRE)

# Adds calculated barrios to idealistaSp spatial poligon
airbnbSp$barrio <- countBarrios$NOMBRE
table(airbnbSp$barrio)

airbnb <- as.data.frame(airbnbSp) # convert spatial data to regular data frame
# removes duplicated columns with lat and long
drops <- c("latitude.1","longitude.1") 
airbnb <- airbnb[ , !(names(airbnb) %in% drops)]

# Saves listings with "barrio" in csv
write.csv(airbnb, file = "data/output/insideairbnb_listings_madrid_151002_con-barrio.csv", row.names = FALSE)

airbnb.barrios <- as.data.frame(table(airbnb$barrio))
colnames(airbnb.barrios) <-  c("barrio","n_listings_airbnb")

# Saves numbers of listings per "barrio" in csv
write.csv(airbnb.barrios, file = "data/output/insideairbnb_listings_madrid_151002_por-barrio.csv", row.names = FALSE)