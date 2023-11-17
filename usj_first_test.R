# nov 16
# jack tarricone

library(terra)

setwd("~/rsf")

# read in cc
cc_tuo_30_v1 <-rast('./rasters/cc_tuo.tif')

# read in snsr
snsr_shp_v1 <-vect('./vectors/snsr_shp.gpkg')
snsr <-project(snsr_shp_v1, crs(cc_tuo_30))
plot(snsr, add = TRUE)

# read in tuo
tuo_v1 <-vect("./vectors/ca_basins/tuo.gpkg")
tuo <-project(tuo_v1, crs(snsr))
plot(tuo, add = T)
plot(snsr, add = T)

# mask cc
cc_tuo_30 <-mask(cc_tuo_30_v1, snsr)
plot(cc_tuo_30)


# create custom rast
r_50m <- rast(ncol = 10, nrow = 10,
          xmin = 256940.5, xmax = 353065.7, ymin = 4096359, ymax = 4179140)

h <-ymax(r_50m) -ymin(r_50m)


# 60 m mean
cc_mean_60 <- aggregate(cc_tuo_30, fact=2, fun=mean)
cc_mean_60
plot(cc_mean_60)
hist(cc_mean_60)

# 60 m sd
cc_sd_60 <- aggregate(cc_tuo_30, fact=2, fun=sd)
cc_sd_60
plot(cc_sd_60)

# 240 m
cc_mean_240 <- aggregate(cc_tuo_30, fact=8, fun=mean)
cc_mean_240
plot(cc_mean_240)
hist(cc_mean_240)

# 240 sd
cc_sd_240 <- aggregate(cc_tuo_30, fact=8, fun=sd)
cc_sd_240
plot(cc_sd_240)


# mask > 50 %??
cc_mean_60_mask50 <-ifel(cc_mean_60 > 50, NA, cc_mean_60)
plot(cc_mean_60_mask50)
