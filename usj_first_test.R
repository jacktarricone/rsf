# nov 16
# jack tarricone

library(terra)

setwd("~/rsf")

# read in cc
cc_tuo_30_v2 <-rast('./rasters/cc_tuo.tif')

# read in snsr
snsr_shp_v1 <-vect('./vectors/snsr_shp.gpkg')
snsr <-project(snsr_shp_v1, crs(cc_tuo_30_v2))

# read in tuo
tuo_v1 <-vect("./vectors/ca_basins/tuolumne_v2.gpkg")
tuo <-project(tuo_v1, crs(snsr))
plot(tuo, add = T)
plot(snsr, add = T)

# mask cc
tuo_snsr_ext <-ext(220000, 306603.16327276, 4165222.3694235, 4234629.3939246)
cc_tuo_30 <-crop(mask(cc_tuo_30_v1, snsr),tuo_snsr_ext)
plot(cc_tuo_30)

# create shape file
## test plot
plot(tuo, add = TRUE)
plot(snsr, add = TRUE)

# crop, works
tuo_snsr <-crop(snsr, tuo)
plot(cc_tuo_30)
plot(tuo_snsr, add = TRUE)
plot(tuo_snsr)

ext(cc_)

# create custom rast
r_50m <- rast(nrow = 1388, ncol = 1732,
          xmin = 220000, xmax = 306603.16327276, ymin = 4165222.3694235, ymax = 4234629.3939246)

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

# resample to 50m
cc_m_50 <-resample(cc_tuo_30, r_50m, method = 'bilinear')
cc_m_50
plot(cc_m_50)
perc_50 <-calc_percents(cc_m_50)

# 250 m mean
cc_m_250 <- aggregate(cc_m_50, fact=5, fun=mean)
cc_m_250
plot(cc_m_250)
hist(cc_m_250)
perc_250 <-calc_percents(cc_m_250)

# mask > 50 %??
cc_mean_60_mask50 <-ifel(cc_mean_60 > 50, NA, cc_mean_60)
plot(cc_mean_60_mask50)
