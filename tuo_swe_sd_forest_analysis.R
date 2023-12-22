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
swe_50m_forest_mask <-mask(swe_50m, cc_0_mask, maskvalue = 999)
swe_50m_forest_mask
plot(swe_50m_forest_mask)

# calc number and percentage of pixels
perc_pixels_250m <-(terra::aggregate( not.na(swe_50m_forest_mask),  5,  fun=sum)/25)*100
perc_pixels_500m <-terra::mask((terra::aggregate( not.na(swe_50m_forest_mask),  10,  fun=sum)/100)*100,snsr_tuo) 
perc_pixels_1000m <-terra::mask((terra::aggregate( not.na(swe_50m_forest_mask),  20,  fun=sum)/400)*100,snsr_tuo) 
plot(perc_pixels_1000m)

# n
n_pixels_250m <-mask(aggregate( not.na(swe_50m_forest_mask),  5,  fun=sum),snsr_tuo)
n_pixels_500m <-mask(aggregate( not.na(swe_50m_forest_mask),  10,  fun=sum),snsr_tuo)
n_pixels_1000m <-mask(aggregate( not.na(swe_50m_forest_mask),  20,  fun=sum),snsr_tuo)

plot(n_pixels_250m)
plot(n_pixels_500m)
plot(n_pixels_1000m)
plot(swe_50m_forest_mask)

# masked for forest vs non forest
# 250m
swe_250m_mean <-aggregate(swe_50m, fact=5, fun=mean, na.rm = TRUE)
swe_250m_sd <-aggregate(swe_50m, fact=5, fun=sd, na.rm = TRUE)
swe_250m_f90_mean <- aggregate(swe_50m_forest_mask, fact=5, fun=mean, na.rm=TRUE)
swe_250m_f90_sd <- aggregate(swe_50m_forest_mask, fact=5, fun=sd, na.rm=TRUE)

diff_250m <-swe_250m_mean-swe_250m_f90_mean
plot(diff_250m)
hist(diff_250m, breaks = 100)

# 500m
swe_500m_mean <-aggregate(swe_50m, fact=10, fun=mean, na.rm = TRUE)
swe_500m_sd <-aggregate(swe_50m, fact=10, fun=sd, na.rm = TRUE)
swe_500m_f90_mean <-aggregate(swe_50m_forest_mask, fact=10, fun=mean, na.rm=TRUE)
swe_500m_f90_sd <-aggregate(swe_50m_forest_mask, fact=10, fun=sd, na.rm=TRUE)
plot(swe_500m_f90_mean)
plot(swe_500m_mean)

diff_500m <-swe_500m_mean-swe_500m_f90_mean
plot(diff_500m)
hist(diff_500m, breaks = 100)

# 1000m
test <-resample(swe_50m, swe_1000m_mean, method = "bilinear")
test1 <-resample(swe_50m_forest_mask, swe_1000m_mean, method = "bilinear")
plot(test)
plot(test1)
plot(swe_50m)
plot(swe_50m_forest_mask)

swe_1000m_mean <-aggregate(swe_50m, fact=20, fun=mean, na.rm = TRUE)
swe_1000m_sd <-aggregate(swe_50m, fact=20, fun=sd, na.rm = TRUE)
swe_1000m_f90_mean <-aggregate(swe_50m_forest_mask, fact=20, fun=mean, na.rm=TRUE)
swe_1000m_f90_sd <-aggregate(swe_50m_forest_mask, fact=20, fun=sd, na.rm=TRUE)
plot(swe_1000m_f90_mean)
plot(swe_1000m_mean)

diff_1000m <-swe_1000m_mean-swe_1000m_f90_mean
plot(diff_1000m)
hist(diff_1000m, breaks = 100)

plot(swe_1000m_sd)
plot(swe_1000m_f90_sd)
# writeRaster(swe_250_f90_mean, "./rasters/swe_250m_f90_mean.tif", overwrite = T)
# writeRaster(swe_250_f90_sd, "./rasters/swe_250m_f90_sd.tif", overwrite = T)
# writeRaster(swe_250_f90_mean, "./rasters/swe_250m_f90_mean.tif", overwrite = T)

##### 1000m
# plopt deviaions against eachother
swe1000_f90_df <-as.data.frame(swe_1000m_f90_mean, xy = T)
swe1000_df <-as.data.frame(swe_1000m_mean, xy = T)
perc_df <-as.data.frame(perc_pixels_1000m, xy = TRUE)

joined_df2 <-full_join(swe1000_df, swe1000_f90_df, by = c('x','y'))
joined_df22 <-full_join(joined_df2, perc_df)
colnames(joined_df22)[3:5] <-c("no_mask","mask","perc_pix")
joined_df22$perc_pix <-ifelse(joined_df22$perc_pix == 100, NA, joined_df22$perc_pix)
joined_df22 <-na.omit(joined_df22)
head(joined_df22)


rmse2 <-round(rmse(joined_df22$no_mask, joined_df22$mask), digits = 2)
mae2 <-round(mae(joined_df22$no_mask, joined_df22$mask), digits = 2)
cor2 <-round(cor(joined_df22$no_mask, joined_df22$mask, method = c("pearson")), digits=2) 
n <-length(joined_df22$no_mask)

pix_scale <-viridis::viridis(9, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

# plot czo
p2 <-ggplot(joined_df22, aes(y = no_mask, x = mask, color = perc_pix)) +
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
                                       "MAE =", mae2, " m \n",
                                       "n =", n)), color = "black")+
  guides(color = guide_colorbar(direction = "horizontal",
                                label.position = 'top',
                                barwidth = 17,
                                barheight = 1,
                                frame.colour = "black", 
                                ticks.colour = "black")) 


# save
ggsave(p2,
       file = "./plots/mask_vs_nomask_swe1000m_v2_TRUE.pdf",
       width = 4.7,
       height = 5,
       units = "in",
       dpi = 300)

system("open ./plots/mask_vs_nomask_swe1000m_v2_TRUE.pdf")



# plopt deviaions against eachother
swe_sd_f90_df <-as.data.frame(swe_250m_f90_sd, xy = T)
swe_sd_df <-as.data.frame(swe_250m_sd, xy = T)
perc_df <-as.data.frame(perc_pixels_250m, xy = T)
head(swe_sd_df)


joined_df <-full_join(swe_sd_df, swe_sd_f90_df, by = c('x','y'))
colnames(joined_df)[3:4] <-c("no_mask","mask")
joined_df1 <-full_join(joined_df, perc_df)
colnames(joined_df1)[3:5] <-c("no_mask","mask","perc_pix")
joined_df1$perc_pix <-ifelse(joined_df1$perc_pix == 100, NA, joined_df1$perc_pix)
joined_df1 <-na.omit(joined_df1)
head(joined_df1)


rmse <-round(rmse(joined_df1$no_mask, joined_df1$mask), digits = 2)
mae <-round(mae(joined_df1$no_mask, joined_df1$mask), digits = 2)
cor <-round(cor(joined_df1$no_mask, joined_df1$mask, method = c("pearson")), digits=2) 


# plot czo
p <-ggplot(joined_df1, aes(x = mask, y = no_mask, color = perc_pix)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(alpha = .25, shape = 16) + 
  scale_color_viridis(option = "A", limits = c(0,100)) +
  scale_y_continuous(limits = c(0,1),expand = (c(0,0.01))) +
  scale_x_continuous(limits = c(0,1),expand = (c(0,0.01))) +
  ylab("No Mask SWE SD (m)") + xlab("Forest Mask SWE SD (m)") +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1),
        aspect.ratio = 1,
        legend.position = "top",
        legend.title=element_blank(),
        plot.margin = unit(c(0.2,0.2,0.2,0.2), "cm"),
        legend.box.spacing = unit(0, "pt"))+
  geom_text(aes(.3, .8, label = paste("R = ", cor, "\n",
                                       "RMSE =", rmse," m \n",
                                       "MAE =", mae, " m \n")), color = "black")+
  guides(color = guide_colorbar(direction = "horizontal",
                                label.position = 'top',
                                barwidth = 17,
                                barheight = 1,
                                frame.colour = "black", 
                                ticks.colour = "black")) 

# save
ggsave(p,
       file = "./plots/mask_vs_nomask_swe_sd_v2.pdf",
       width = 4.7,
       height = 5,
       units = "in",
       dpi = 300)

system("open ./plots/mask_vs_nomask_swe_sd_v2.pdf")


# plopt deviaions against eachother
swe_250m_f90_df <-as.data.frame(swe_250m_f90_mean, xy = T)
swe_df <-as.data.frame(swe_250m_mean, xy = T)
perc_df_250 <-as.data.frame(perc_pixels_250m, xy = T)
head(swe_df)
head(perc_)

joined_df2 <-full_join(swe_df, swe_f90_df, by = c('x','y'))
colnames(joined_df2)[3:4] <-c("no_mask","mask")
joined_df22 <-full_join(joined_df2, perc_df_250, by = c('x','y'))
colnames(joined_df22)[3:5] <-c("no_mask","mask","perc_pix")
joined_df22$perc_pix <-ifelse(joined_df22$perc_pix == 100, NA, joined_df22$perc_pix)
joined_df22 <-na.omit(joined_df22)
head(joined_df22)


rmse2 <-round(rmse(joined_df22$no_mask, joined_df22$mask), digits = 2)
mae2 <-round(mae(joined_df22$no_mask, joined_df22$mask), digits = 2)
cor2 <-round(cor(joined_df22$no_mask, joined_df22$mask, method = c("pearson")), digits=2) 
n2 <-length(joined_df22$no_mask)

pix_scale <-viridis::viridis(9, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")

# plot czo
p2 <-ggplot(joined_df22, aes(y = no_mask, x = mask, color = perc_pix)) +
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
ggsave(p2,
       file = "./plots/mask_vs_nomask_swe_v2.pdf",
       width = 4.7,
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
