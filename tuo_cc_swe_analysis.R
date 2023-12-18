# nov 16
# jack tarricone

library(terra)
library(sf)

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
  nf <-as.integer(global(cc_rast <= 5, "sum", na.rm=TRUE))
  sf <-as.integer(global(cc_rast > 5 & cc_rast <= 50, "sum", na.rm=TRUE))
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



rcts <- aggregate( notna(r),  4,  fun=sum) 

# bilinear
cc_m_50_bi <-resample(cc_tuo_30, swe_50m, method = 'bilinear')
perc_bi_50 <-calc_percents(cc_m_50_bi)
# writeRaster(cc_m_50_bi, "./rasters/cc_tuo_bi_50m.tif")

hist(cc_m_250_bi, breaks = 100)
hist(cc_m_250_n, breaks = 100)

# mask all forest cover
plot(cc_m_250_bi)
cc_0_mask <-ifel(cc_m_50_bi > 10, 999, cc_m_50_bi)
mask_all_f <-mask(swe_50m, cc_0_mask, maskvalue = 999)
mask_all_f
plot(mask_all_f)
plot(swe_50m)
perc_pixels <-mask((aggregate( not.na(mask_all_f),  5,  fun=sum)/25)*100,snsr_tuo) 
perc_pixels
plot(perc_pixels)

# 250 m mean
cc_m_250_bi <- aggregate(cc_m_50_bi, fact=5, fun=mean)
cc_m_250_n <- aggregate(cc_m_50_n, fact=5, fun=mean)
perc_bi_250 <-calc_percents(cc_m_250_bi)
perc_n_250 <-calc_percents(cc_m_250_n)
# writeRaster(cc_m_250_bi, "./rasters/cc_tuo_bi_250m.tif")
# writeRaster(cc_m_250_n, "./rasters/cc_tuo_near_250m.tif")


#### SWE data masking
c_mask1 <-ifel(cc_m_50_bi > 40, 999, cc_m_50_bi)
c_mask <-ifel(c_mask1 == 999, NA, c_mask1)
writeRaster(c_mask, "./rasters/c_mask_40p.tif")
plot(c_mask)
swe50_masked <-mask(swe_50m, c_mask, maskvalues = NA)
writeRaster(swe50_masked, "./rasters/swe50_masked.tif")

#### SWE data masking
c_mask1 <-ifel(cc_m_250_bi > 40, 999, cc_m_250_bi)
c_mask <-ifel(c_mask1 == 999, NA, c_mask1)
writeRaster(c_mask, "./rasters/c_mask_40p.tif")
plot(c_mask)
swe50_masked <-mask(swe_50m, c_mask, maskvalues = NA)
writeRaster(swe50_masked, "./rasters/swe50_masked.tif")

swe250_masked <-mask(swe_250m_mean, c_mask, maskvalues = NA)
plot(swe50_masked)
plot(swe250_masked)


wi_grid <- st_make_grid(snsr_tuo, square = F, cellsize = c(250, 250))
class(wi_grid)
plot(wi_grid)
