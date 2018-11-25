library(raster)
library(ggplot2)
library(rgdal)
library(bfastSpatial)
library(maptools)
library(tidyr)

#======================= Processing =====================
setwd("E:/Uinta/LANDSAT/L5-8/New/L45")

dirout <- "E:/Uinta/LANDSAT/L5-8/New/L45/outdir"



mfiles <- list.files(getwd(), full.names = TRUE, pattern = ".gz")


# WARNNING!!!!
# 1) For Aquatic applications, make sure to change the QA numbers to avoid loosing the water pixels
# 2) These values might be different across platforms (Landsat 4-7 vs. Landsat 8)
# 3) Also notice that thermal bands have changd between L5-7 and L8

# For L5-7: Pixel_qa values : 66= clear, 68= water low cloud, 130= clear low cloud
# For L8 : 322=clear, 324=water, 322=low confidence cloud
# Also, band 10 and 11 are the new thermal bands in L8 (10 is for 10.60 - 11.19nm and 11 is for 11.50 - 12.51nm) while band6 was 10.40-12.50nm

for (fls in mfiles) {
  processLandsat(x=fls, 
                 vi=c("band6"), # for L5-7
                 #vi=c("band10", "band11"), # for L8
                 outdir = dirout,
                 delete=FALSE, 
                 mask="pixel_qa", 
                 keep = c(66, 68, 130), #for L5-7
                 #keep = c(322, 324, 322), #for L8
                 overwrite = TRUE, 
                 fileExt = "tif")
}

# =============== Extraction ================================
library(raster)
library(rgdal)
library(maptools) # For reading in the shapefiles
library(tidyr)

# File list
Blist <- list.files("E:/Uinta/LANDSAT/L5-8/New/L8/outdir/band10", full.names = TRUE, pattern = '.tif')

# Date Extractoin
#mdate <- substr(Blist, start = 54, stop=61) # for LS4-5
#mdate <- substr(Blist, start = 53, stop=60) # for LS7
mdate <- substr(Blist, start = 54, stop=61) # for L8
mdate <- as.Date(mdate, "%Y%m%d")

lake_obj <- readShapePoly("E:/Uinta/LANDSAT/polygones/Uinta_large_permanent_lakes.shp")
# Get the attribute table from the lake layer
Lk_info <- data.frame(lake_obj@data)
Lk_info$FID <- rownames(Lk_info)
#Stack
mstack <- stack(Blist)

#Extract Tip: This function removes the NA as well as NaN, Awesome
mtemp <- raster::extract(mstack, lake_obj, fun=mean, na.rm=TRUE, df=TRUE)
# Save the above raw file
write.csv(mtemp, "E:/Uinta/LANDSAT/tables/New/L8temp.csv")
# Preseve the Original Extract
temp5 <- mtemp
# Transposing the vectorial dataset!
temp5 <- data.frame(t(temp5))
#insert Lake names
#make sure that there is no duplicate in lake names, otherwise the "gather()" will make shitty data
colnames(temp5) <- Lk_info$GNIS_Name
#colnames(temp5) <- IDS
temp5 = temp5[-1,]
# Convert NaN to NA
temp5[temp5=="NaN"] <- NA
# Rescaling and rounding
temp5 <- round(temp5*0.1-273.15,2)
#add the date column
temp5$Date <- mdate
temp5$Month <- months(temp5$Date)
#Clean row names
row.names(temp5) <- c()
# Cast data
tot <- gather(temp5, LakeName, Temp, -Date, -Month)
# Merging/joining elevation in (we are merging tables here)
nm_elv <- data.frame(Lk_info$GNIS_Name, Lk_info$Elevation)
colnames(nm_elv) <- c("LakeName", "Elevation")
tot <- plyr::join(tot, nm_elv, by="LakeName", type="left", match="all")
# Save the csv
write.csv(tot, "E:/Uinta/LANDSAT/tables/New/L8tot.csv")
# ==================== Plotting ========================

tot$Month <- factor(tot$Month, levels=month.name)

ggplot(tot, aes(x=Date, y=Temp, color=Sites)) + geom_point() + facet_wrap(~ Month)

AugTemp <- totn[totn$Month == "August" & totn$Sites == "Site1",]

aug_lm <- lm(AugTemp$Temp ~ AugTemp$Date)

summary(aug_lm)

write.csv(mtemp, "E:/Uinta/LANDSAT/tables/L8tot.csv")

# ====================================

setwd("E:/WM-BIOL-300-PROJ/G5/outdir/ndvi")


mfile <- list.files(getwd(), full.names = TRUE, pattern = ".tif")


mstack <- stack(mfile)

mstack <- mstack*0.0001 # for NDVI or NDMI
#mstack <- mstack*0.1 - 273.2 # for Thermal Band plus converting to celcious


ndvi2_avg <- cellStats(mstack,mean)

ndvi2_avg <- as.data.frame(ndvi_avg)


#yrs <- substr(row.names(ndvi_avg), start = 18, stop = 21)
#mnt <- substr(row.names(ndvi_avg), start=22, stop = 23)
#dys <- substr(row.names(ndvi_avg), start=24, stop = 25)

mdate <- substr(row.names(ndvi_avg), start=11, stop = 18)

mdates <- as.Date(mdate, "%Y%m%d")

ndvi_avg$Date <- mdates
ndvi_avg$NDVI <- ndvi2_avg

ndvi_avg$ndvi_avg[ndvi_avg$ndvi_avg == "NaN"] <- NA
ndvi_avg$NDVI[ndvi_avg$NDVI=="NaN"] <- NA

colnames(ndvi_avg) <- c("NDMI", "DATE", "NDVI")

mdata <- ndvi_avg %>% gather(indices, value, -DATE)

#ndvi_avg_cln <- ndvi_avg[ndvi_avg$ndvi_avg < 1,]



ggplot(mdata, aes(y=value, x=DATE, color=indices)) + geom_line() 
