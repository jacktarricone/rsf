# nov 16
# jack tarricone

library(terra)
library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)

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
swe_250m_mean <-aggregate(swe_50m, fact=5, fun=mean, na.rm = FALSE)
swe_250m_sd <-aggregate(swe_50m, fact=5, fun=sd, na.rm = FALSE)
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

# calc number and percentage of pixels
perc_pixels <-mask((aggregate( not.na(mask_all_f),  5,  fun=sum)/25)*100,snsr_tuo) 
perc_pixels
plot(perc_pixels)

# n
n_pixels <-mask(aggregate( not.na(mask_all_f),  5,  fun=sum),snsr_tuo) 
n_pixels
plot(n_pixels)
hist(n_pixels)
freq(n_pixels)


# masked 250 m
swe_250m_mean <-aggregate(swe_50m, fact=5, fun=mean, na.rm = FALSE)
swe_250m_sd <-aggregate(swe_50m, fact=5, fun=sd, na.rm = FALSE)
swe_250m_f90_mean <- aggregate(mask_all_f, fact=5, fun=mean, na.rm=TRUE)
swe_250m_f90_sd <- aggregate(mask_all_f, fact=5, fun=sd, na.rm=TRUE)


plot(swe_250m_f90_mean)
plot(swe_250m_mean)

plot(swe_250m_f90_sd)
plot(swe_250m_sd)


writeRaster(swe_250_f90_mean, "./rasters/swe_250m_f90_mean.tif", overwrite = T)
writeRaster(swe_250_f90_sd, "./rasters/swe_250m_f90_sd.tif", overwrite = T)
writeRaster(swe_250_f90_mean, "./rasters/swe_250m_f90_mean.tif", overwrite = T)


# plopt deviaions against eachother
swe_sd_f90_df <-as.data.frame(swe_250_f90_sd, xy = T)
swe_sd_df <-as.data.frame(swe_250m_sd, xy = T)
head(swe_sd_df)


joined_df <-full_join(sd_f90_df, swe_sd_f90_df, by = c('x','y'))
colnames(joined_df)[3:4] <-c("no_forest","forest")
head(joined_df)

# plot czo
p <-ggplot(joined_df, aes(x = forest, y = no_forest)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  # geom_smooth(method = "lm", se = FALSE) +
  # geom_errorbar(aes(y= insar_dswe, xmin=insitu_dswe-abs(insitu_error), xmax=insitu_dswe+abs(insitu_error)), 
  #              width=0.1, colour = 'black', alpha=0.4, size=.5) +
  geom_point() +  
  scale_y_continuous(limits = c(-10,10),breaks = c(seq(-10,10,2)),expand = (c(0,0))) +
  scale_x_continuous(limits = c(-10,10),breaks = c(seq(-10,10,2)),expand = (c(0,0))) +
  ylab(Delta~"SWE InSAR (cm)") + xlab(Delta~"SWE In Situ (cm)") +
  scale_color_manual(name = "InSAR Pair",
                     values = my_colors,
                     breaks = c('feb12_19', 'feb19_26', 'feb12_26'),
                     labels = c('Feb. 12-19', 'Feb. 19-26', 'Feb. 12-26'))+
  scale_fill_discrete(breaks=c('B', 'C', 'A'))  +
  theme_classic(15) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1), 
        legend.position = c(.80,.20))

print(p)


plot(swe_250m_mean)
plot(mask_all_f)

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
