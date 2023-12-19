# nov 16
# jack tarricone

library(terra)
library(sf)
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggpmisc)
library(Metrics)
library(glue)
library(viridis)

setwd("~/rsf")

# set custom plot theme
theme_classic <-function(base_size = 11, base_family = "",
                         base_line_size = base_size / 22,
                         base_rect_size = base_size / 22) {
  theme_bw(
    base_size = base_size,
    base_family = base_family,
    base_line_size = base_line_size,
    base_rect_size = base_rect_size
  ) %+replace%
    theme(
      # no background and no grid
      panel.border     = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # show axes
      # axis.line      = element_line(colour = "black", linewidth = rel(1)),
      
      # match legend key to panel.background
      legend.key       = element_blank(),
      
      # simple, black and white strips
      strip.background = element_rect(fill = "white", colour = "black", linewidth = rel(2)),
      # NB: size is 1 but clipped, it looks like the 0.5 of the axes
      
      complete = TRUE
    )
}

theme_set(theme_classic(16))

# read in shape files
# read in snsr
snsr_tuo <-vect("./vectors/tuo_snsr.gpkg")
plot(snsr_tuo)

# swe
swe_50m_raw <-rast('./rasters/ASO_Tuolumne_2023Mar16-17_AllData_and_Reports/ASO_Tuolumne_2023Mar16-17_swe_50m.tif')
swe_50m <-crop(mask(swe_50m_raw, snsr_tuo),ext(snsr_tuo))

# read in cc
cc_tuo_30_v1 <-rast('./rasters/cc_tuo.tif')
cc_tuo_30 <-crop(mask(cc_tuo_30_v1, snsr_tuo),ext(snsr_tuo))
cc_m_50_bi <-resample(cc_tuo_30, swe_50m, method = 'bilinear')
# writeRaster(cc_m_50_bi, "./rasters/cc_tuo_bi_50m.tif")


# mask all forest cover
cc_0_mask <-ifel(cc_m_50_bi > 10, 999, cc_m_50_bi)
mask_all_f <-mask(swe_50m, cc_0_mask, maskvalue = 999)

# calc number and percentage of pixels
perc_pixels <-terra::mask((terra::aggregate( not.na(mask_all_f),  5,  fun=sum)/25)*100,snsr_tuo) 
perc_pixels

# n
n_pixels <-mask(aggregate( not.na(mask_all_f),  5,  fun=sum),snsr_tuo) 
n_pixels
plot(n_pixels)


# masked 250 m
# agg test
swe_250m_mean <-aggregate(swe_50m, fact=5, fun=mean, na.rm = FALSE)
swe_250m_sd <-aggregate(swe_50m, fact=5, fun=sd, na.rm = FALSE)
swe_250m_f90_mean <- aggregate(mask_all_f, fact=5, fun=mean, na.rm=TRUE)
swe_250m_f90_sd <- aggregate(mask_all_f, fact=5, fun=sd, na.rm=TRUE)

# writeRaster(swe_250_f90_mean, "./rasters/swe_250m_f90_mean.tif", overwrite = T)
# writeRaster(swe_250_f90_sd, "./rasters/swe_250m_f90_sd.tif", overwrite = T)
# writeRaster(swe_250_f90_mean, "./rasters/swe_250m_f90_mean.tif", overwrite = T)


# plopt deviaions against eachother
swe_sd_f90_df <-as.data.frame(swe_250m_f90_sd, xy = T)
swe_sd_df <-as.data.frame(swe_250m_sd, xy = T)
perc_df <-as.data.frame(perc_pixels, xy = T)
head(swe_sd_df)


joined_df <-full_join(swe_sd_df, swe_sd_f90_df, by = c('x','y'))
colnames(joined_df)[3:4] <-c("no_mask","mask")
joined_df <-na.omit(joined_df)
head(joined_df)

rmse <-round(rmse(joined_df$no_mask, joined_df$mask), digits = 2)
mae <-round(mae(joined_df$no_mask, joined_df$mask), digits = 2)
cor <-round(cor(joined_df$no_mask, joined_df$mask, method = c("pearson")), digits=2) 


# plot czo
p <-ggplot(joined_df, aes(x = mask, y = no_mask)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(color = "firebrick", alpha = .1, shape = 4) +  
  scale_y_continuous(limits = c(0,2.5),expand = (c(0,0.01))) +
  scale_x_continuous(limits = c(0,2.5),expand = (c(0,0.01))) +
  ylab("No Mask SWE SD (m)") + xlab("Forest Mask SWE SD (m)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1)+
  geom_text(aes(.8, 2.1, label = paste("R = ", cor, "\n",
                                       "RMSE =", rmse," m \n",
                                       "MAE =", mae, " m \n")))

print(p)

# save
ggsave(p,
       file = "./plots/mask_vs_nomask_swesd.pdf",
       width = 5,
       height = 5,
       units = "in",
       dpi = 300)

system("open ./plots/mask_vs_nomask_swesd.pdf")


# plopt deviaions against eachother
swe_f90_df <-as.data.frame(swe_250m_f90_mean, xy = T)
swe_df <-as.data.frame(swe_250m_mean, xy = T)
head(swe_df)


joined_df2 <-full_join(swe_df, swe_f90_df, by = c('x','y'))
joined_df22 <-full_join(joined_df2, perc_df)
colnames(joined_df22)[3:5] <-c("no_mask","mask","perc_pix")
joined_df2 <-na.omit(joined_df22)
head(joined_df22)


rmse2 <-round(rmse(joined_df2$no_mask, joined_df2$mask), digits = 2)
mae2 <-round(mae(joined_df2$no_mask, joined_df2$mask), digits = 2)
cor2 <-round(cor(joined_df2$no_mask, joined_df2$mask, method = c("pearson")), digits=2) 

pix_scale <-viridis::viridis(9, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

??viridis

# plot czo
p2 <-ggplot(joined_df2, aes(y = no_mask, x = mask, color = perc_pix)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(alpha = .25, shape = 16) + 
  scale_color_viridis(option = "D", limits = c(0,100)) +
  scale_y_continuous(limits = c(0,3),expand = (c(0,0.01))) +
  scale_x_continuous(limits = c(0,3),expand = (c(0,0.01))) +
  ylab("No Mask SWE (m)") + xlab("Forest Mask SWE (m)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "top",
        legend.title=element_blank(),
        plot.margin = unit(c(0,0,0,0), "cm"),
        legend.box.spacing = unit(0, "pt"))+
  geom_text(aes(.8, 2.5, label = paste("R = ", cor2, "\n",
                                     "RMSE =", rmse2," m \n",
                                     "MAE =", mae2, " m \n")), color = "black")+
  guides(color = guide_colorbar(direction = "horizontal",
                               label.position = 'top',
                               barwidth = 17,
                               barheight = 1,
                               frame.colour = "black", 
                               ticks.colour = "black")) 


# save
ggsave("./plots/mask_vs_nomask_swe_v2.pdf",
       width = 4.5,
       height = 5,
       units = "in",
       dpi = 300)

system("open ./plots/mask_vs_nomask_swe_v2.pdf")



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
