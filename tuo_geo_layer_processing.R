## formatting data for qgis
## jack tarricone
## december 5th, 2022

library(terra)

setwd("~/ch3_fusion/")

# usj
usj_v1 <-vect("./shapefiles/upper_san_joaquin.gpkg")
usj <-project(usj_v1, 'EPSG:32611') #utm z 11
plot(usj)

########## nlcd canopy cover 2016
cc_raw <-rast("./rasters/geo_layers/raw/NLCD_2016_Tree_Canopy_L48_20190831_8ILcwOA0bCSdi15EeQtJ.tiff")
cc_reproj <-project(cc_raw, 'EPSG:32611') # reproj
cc_usj <-crop(mask(cc_reproj, usj), ext(usj)) # mask usj
plot(cc_usj)
cc_usj
writeRaster(cc_usj, "~/rsf/rasters/cc_usj.tif")

# tuo
tuo_v1 <-vect("~/ch1_margulis/vectors/ca_basins/tuolumne_v2.gpkg")
tuo <-project(tuo_v1, 'EPSG:32611') #utm z 11
plot(tuo)

########## nlcd canopy cover 2016
cc_raw <-rast("./rasters/geo_layers/raw/NLCD_2016_Tree_Canopy_L48_20190831_8ILcwOA0bCSdi15EeQtJ.tiff")
cc_reproj <-project(cc_raw, 'EPSG:32611') # reproj
cc_tuo <-crop(mask(cc_reproj, tuo), ext(tuo)) # mask tuo
plot(cc_tuo)
cc_tuo
writeRaster(cc_tuo, "~/rsf/rasters/cc_tuo.tif")


# swe
depth_3m <-rast('./rasters/ASO_Tuolumne_2023Mar16-17_AllData_and_Reports/ASO_Tuolumne_2023Mar16-17_snowdepth_3m.tif')
plot(depth_3m)
depth_3m
# t <-ifel(is.na(swe_50m),-99,swe_50m)
# plot(t)

# conver to vect
as_vect <-as.polygons(depth_3m)

## aggregate polyongs up to just data extent
tuo_aso_shp <- aggregate(as_vect, dissolve = TRUE, fun = "mean", cores = 12)
plot(tuo_aso_shp)
writeVector(tuo_aso_shp, "./vectors/tuo_aso_3m_shp.gpkg")
