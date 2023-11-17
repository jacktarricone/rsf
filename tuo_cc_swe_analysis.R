# nov 16
# jack tarricone

library(terra)

setwd("~/rsf")

# read in shape files
# read in snsr
snsr_tuo <-vect("./vectors/tuo_snsr.gpkg")
plot(snsr_tuo)

# swe
swe_50m_raw <-rast('./rasters/ASO_Tuolumne_2023Mar16-17_AllData_and_Reports/ASO_Tuolumne_2023Mar16-17_swe_50m.tif')
swe_50m <-crop(mask(swe_50m_raw, snsr_tuo),ext(snsr_tuo))
plot(swe_50m)
swe_50m
# writeRaster(swe_50m, "./rasters/swe_50m.tif")

# agg test
swe_250m_mean <-aggregate(swe_50m, fact=5, fun=mean)
swe_250m_sd <-aggregate(swe_50m, fact=5, fun=sd)
swe_250m_mean
plot(swe_250m_mean)
plot(swe_250m_sd)
writeRaster(swe_250m_mean, "./rasters/swe_250m_mean.tif")
writeRaster(swe_250m_sd, "./rasters/swe_250m_sd.tif")

# read in cc
cc_tuo_30_v1 <-rast('./rasters/cc_tuo.tif')

# crop
cc_tuo_30 <-crop(mask(cc_tuo_30_v1, snsr_tuo),ext(snsr_tuo))
plot(cc_tuo_30)
cc_tuo_30


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
cc_m_50_n <-resample(cc_tuo_30, swe_50m, method = 'near')
cc_m_50_n
plot(cc_m_50_n)
perc_n_50 <-calc_percents(cc_m_50_n)
# writeRaster(cc_m_50_n, "./rasters/cc_tuo_near_50m.tif")

# bilinear
cc_m_50_bi <-resample(cc_tuo_30, swe_50m, method = 'bilinear')
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
