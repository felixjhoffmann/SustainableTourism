#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Green Leader paper
# 2021-08-06
# Fig 3C: Europe map
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # Numerous data wrangling packages
library(data.table)   # Quick data loading
library(RColorBrewer) # Nice colours for plots

# Map packages
library("rgdal")      # Point in Polygon
library("rgeos")      # Simplified polygons
library("sp")
library("spatialEco")
library("tmap")       # Beautiful maps in R
library(classInt)     # Group data into distinct classes in map

#%#%#%#%#%#%#%#%#%
# Load data
#%#%#%#%#%#%#%#%#%

# Geocoded hotel data
df <- fread(paste(getwd(), "/Fig3C_data.csv", sep = ""))

#%#%#%
# Map data
#%#%#%

# EU NUTS2
# --------
# Data source:
# https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
eu <- readOGR(dsn = "Shapefiles/NUTS_RG_20M_2021_3857_LEVL_2.shp", layer = "NUTS_RG_20M_2021_3857_LEVL_2")

# EU NUTS0
# --------
# Data source:
# https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts
eu_0 <- readOGR(dsn = "Shapefiles/NUTS_RG_20M_2021_4326_LEVL_0.shp", layer = "NUTS_RG_20M_2021_4326_LEVL_0")

# Bosnia (Simplify the polygon before plotting)
# ------
# These data were extracted from the GADM database (www.gadm.org), version 2.5, July 2015.
bosnia0 <- readOGR(dsn = "Shapefiles/BIH_adm", layer = "BIH_adm0")
bosnia0a <-gSimplify(bosnia0,tol=0.01, topologyPreserve=TRUE)
bosnia0 <- SpatialPolygonsDataFrame(bosnia0a, data=bosnia0@data)

# Kosovo (Simplify the polygon before plotting)
# ------
# These files were extracted from GADM version 1.0, in March 2009.
kosovo0 <- readOGR(dsn = "Shapefiles/KO-_adm", layer = "KO__adm0")
kosovo0a <-gSimplify(kosovo0,tol=0.01, topologyPreserve=TRUE)
kosovo0 <- SpatialPolygonsDataFrame(kosovo0a, data=kosovo0@data)

#%#%#%#%
# Merge data with polygons
#%#%#%#%

# Select the coordinates columns
points <- df %>% select(lng, lat)
# Create a spatial POINTS data frame for the Points in Polygon matching
spdf <- SpatialPointsDataFrame(coords = points, data = points,
                               proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))

# Adjust the map projection
eu <- spTransform(eu, proj4string(spdf))

# Match points to polygons
pts.poly <- point.in.poly(spdf, eu)

# Extract the data from polygon file
df2 <- pts.poly@data
# Rename lng and lat before merging (to avoid error when applying cbind)
df2 <- df2 %>% rename(longitude2 = lng, latitutde2 = lat)

# Concatenate the data sets
df <- cbind(df,df2)
# Keep only the relevant columns
df <- df %>% dplyr::select(address, lng, lat, NUTS_ID, GreenLeader_Predictions)

# Aggregate data to NUTS regions: share of *estimated* Green Leader hotels per NUTS-2 region
df <- df %>% group_by(NUTS_ID) %>% summarise(GL = sum(GreenLeader_Predictions), count = n()) %>% mutate(GL_share = GL/count*100)

# Join aggregated data to polygons for plotting
eu <- merge(eu, df, by="NUTS_ID")

#%#%#%#%
# Prepare map and plot
#%#%#%#%

# Use appropriate map projection to plot the map
eu <-spTransform(eu, "+proj=laea +lat_0=50 +lon_0=11 +x_0=5000000 +y_0=3200000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# Find class breaks that distribute data roughly equally (but which are also rounded numbers) 
breaks_man <- classIntervals(eu[["GL_share"]], n = 6, style = "quantile")
breaks_man <- breaks_man$brks
breaks_man <- c(0,1,5,10,20,50,67)

# Define the colour palette for the plot
col_range <- c(brewer.pal(11,"RdBu")[6],brewer.pal(11,"RdBu")[7],brewer.pal(11,"RdBu")[8],
                  brewer.pal(11,"RdBu")[9],brewer.pal(11,"RdBu")[10],brewer.pal(11,"RdBu")[11])

colNA <- brewer.pal(9,"Greys")[3]

#%#%#%
# Plot the map
#%#%#%

# Zoom into the part of the map that is to be plotted
tm_shape(eu, bbox = c(4710000,4711000,1700000,5550000)) + 
  # Fill with the share of GL per NUTS-2 region
  tm_fill("GL_share", title = "Predicted from QDA model (1)     ", breaks = breaks_man, 
                             colorNA = colNA, palette =  col_range, textNA = "      no data", legend.reverse = T) + 
  # Include European countries missing in the shapefile
  tm_shape(bosnia0) + tm_fill(col = colNA) + tm_shape(kosovo0) + tm_fill(col = colNA)  + 
  # Add the borders of EU countries
  tm_shape(eu_0) + tm_borders()+
  # Layout details:
  tm_layout("Share of Green Leader hotels (%)                 ", asp = 1.1,
            legend.title.size = 1,
            legend.text.size = 1,
            frame = F,
            legend.position = c("left","top"),
            title.position = c("left","top"),
            legend.bg.alpha = 1, 
            title.color = "black", legend.text.color = "black", bg.color = "white", legend.bg.color = "white",
            title.size = 1,
            legend.frame = F,
            legend.format = list(
              text.separator = " - ",
              text.align = "right",
              text.less.than = 1,
              text.to.columns = F))

# END OF SCRIPT
#==============