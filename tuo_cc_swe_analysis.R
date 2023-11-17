# nov 16
# jack tarricone

library(terra)

setwd("~/rsf")

# swe
swe_50m <-rast('./ASO_Tuolumne_2023Mar16-17_AllData_and_Reports/ASO_Tuolumne_2023Mar16-17_swe_50m.tif')
plot(swe_50m)
swe_50m

# agg test
swe_250m <-aggregate(swe_50m, fact=5, fun=mean)
swe_250m
plot(swe_250m)
writeRaster(swe_250m, "./rasters/swe_250m.tif")

# tuo shp
tuo_aso_shp <-vect('./vectors/tuo_aso_shp.gpkg')
plot(tuo_aso_shp)

# read in cc
cc_tuo_30_v1 <-rast('./rasters/cc_tuo.tif')

# crop
cc_tuo_30 <-crop(mask(cc_tuo_30_v1, tuo_aso_shp),ext(tuo_aso_shp))
plot(cc_tuo_30)
cc_tuo_30

# # read in snsr
# snsr_shp_v1 <-vect('./vectors/snsr_shp.gpkg')
# snsr <-project(snsr_shp_v1, crs(cc_tuo_30_v1))
# 
# # read in tuo
# tuo_v1 <-vect("./vectors/ca_basins/tuolumne_v2.gpkg")
# tuo <-project(tuo_v1, crs(snsr))
# plot(tuo)
# plot(snsr, add = T)

# mask cc
# tuo_snsr_ext <-ext(220000, 306603.16327276, 4165222.3694235, 4234629.3939246)
# cc_tuo_30 <-crop(mask(cc_tuo_30_v1, snsr),tuo_snsr_ext)
# plot(cc_tuo_30)
# plot(tuo, add = TRUE)
# plot(snsr, add = TRUE)
# writeRaster(cc_tuo_30[[1]], "./rasters/cc_tuo_30m.tif")

# create shape file
# crop, works
# tuo_snsr <-crop(snsr, tuo)
# plot(cc_tuo_30)
# plot(tuo_snsr, add = TRUE)
# writeVector(tuo_snsr, "./vectors/tuo_snsr.gpkg")

ext()

# create custom rast
r_50m <- rast(nrow = 1105, ncol = 1884,
          xmin = 212410.1, xmax = 306603.2, ymin = 4179351, ymax = 4234599)

r_50m
# height <-ymax(r_50m) -ymin(r_50m)
# length <-xmax(r_50m) -xmin(r_50m)
# h_pix <-height/50
# l_pix <-length/50

# calc funciton
calc_percents <-function(cc_rast){
  
  # pixel counts
  total <-as.integer(global(!(is.na(cc_rast)), "sum"))
  
  # forest bins
  nf <-as.integer(global(cc_rast == 0, "sum", na.rm=TRUE))
  sf <-as.integer(global(cc_rast > 0 & cc_rast <= 50, "sum", na.rm=TRUE))
  f50 <-as.integer(global(cc_rast > 50, "sum", na.rm=TRUE))
  
  # percents
  perc_nf <-round((nf/total)*100, 1)
  perc_sf <-round((sf/total)*100, 1)
  perc_f50 <-round((f50/total)*100, 1)
  
  results <-c(perc_nf,perc_sf,perc_f50)
  return(results)
}

# 30 m
perc_30 <-calc_percents(cc_tuo_30)
plot(cc_tuo_30)

# resample to 50m
## nearest neighbor
cc_m_50_n <-resample(cc_tuo_30, r_50m, method = 'near')
cc_m_50_n
plot(cc_m_50_n)
perc_n_50 <-calc_percents(cc_m_50_n)
# writeRaster(cc_m_50, "./rasters/cc_tuo_near_50m.tif")

# bilinear
cc_m_50_bi <-resample(cc_tuo_30, r_50m, method = 'bilinear')
perc_bi_50 <-calc_percents(cc_m_50_bi)
# writeRaster(cc_m_50_bi, "./rasters/cc_tuo_bi_50m.tif")

# 250 m mean
cc_m_250_bi <- aggregate(cc_m_50_bi, fact=5, fun=mean)
cc_m_250_n <- aggregate(cc_m_50_n, fact=5, fun=mean)
perc_bi_250 <-calc_percents(cc_m_250_bi)
perc_n_250 <-calc_percents(cc_m_250_n)
# writeRaster(cc_m_250_bi, "./rasters/cc_tuo_bi_250m.tif")
# writeRaster(cc_m_250_n, "./rasters/cc_tuo_near_250m.tif")


#### SWE data
