# Rename? To? 'Residences by no. of hawker centres within 1km range' 
# Load Na-maps-output.RData

# rm(list=ls()[!ls() %in% c("sg", "sg_residences_hc", "sg_residences_ee", "sg_residences_sm")])

# Hawker centres ------
sg_residences_hc_0 <- subset(sg_residences_hc, hc_within_1km==0)
sg_residences_hc_1km <- subset(sg_residences_hc, hc_within_1km!=0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_hc_0) +
  tm_dots("hc_within_1km", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_hc_1km) + 
  tm_dots(col="hc_within_1km", style="pretty", sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=0.7) + 
  tm_layout(frame=F, main.title=paste("Residences by number of hawker centres within 1km"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE)),
                labels = c("0", "1 to 2", "2 to 3", "3 to 4", "4 to 5"),
                title = "No. within 1km", border.lwd=0)

tmap_save(filename=paste("NA-2-hc-within-1km.png"), height=3.35, width=4.69)

sg_residences_hc_800m <- subset(sg_residences_hc, hc_within_800m!=0)
sg_residences_hc_800m_0 <- subset(sg_residences_hc, hc_within_800m==0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_hc_800m_0) +
  tm_dots("hc_within_800m", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_hc_800m) + 
  tm_dots(col="hc_within_800m", breaks=c(1, 2, 3, 4), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=1) + 
  tm_layout(frame=F, main.title=paste("Residences by number of hawker centres within 800m"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 3, plot = FALSE)),
                labels = c("0", "1 to 2", "2 to 3", "3 to 4"),
                title = "No. within 800m", border.lwd=0)

tmap_save(filename=paste("NA-2-hc-within-800m.png"), height=3.35, width=4.69)

sg_residences_hc_400m <- subset(sg_residences_hc, hc_within_400m!=0)
sg_residences_hc_400m_0 <- subset(sg_residences_hc, hc_within_400m==0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_hc_400m_0) +
  tm_dots("hc_within_400m", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_hc_400m) + 
  tm_dots(col="hc_within_400m", breaks=c(1, 2, 3), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=1) + 
  tm_layout(frame=F, main.title=paste("Residences by number of hawker centres within 400m"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 2, plot = FALSE)),
                labels = c("0", "1 to 2", "2 to 3"),
                title = "No. within 400m", border.lwd=0)

tmap_save(filename=paste("NA-2-hc-within-400m.png"), height=3.35, width=4.69)

# Supermarkets -------
sg_residences_sm_0 <- subset(sg_residences_sm, sm_within_1km==0)
sg_residences_sm_1km <- subset(sg_residences_sm, sm_within_1km!=0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_sm_0) +
  tm_dots("sm_within_1km", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_sm_1km) + 
  tm_dots(col="sm_within_1km", breaks=c(1, 3, 6, 9, 10), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=0.7) + 
  tm_layout(frame=F, main.title=paste("Residences by number of supermarkets within 1km"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 4, plot = FALSE)),
                labels = c("0", "1 to 3", "3 to 6", "6 to 9", "9 to 10"),
                title = "No. within 1km", border.lwd=0)

tmap_save(filename=paste("NA-2-sm-within-1km.png"), height=3.35, width=4.69)

sg_residences_sm_800m <- subset(sg_residences_sm, sm_within_800m!=0)
sg_residences_sm_800m_0 <- subset(sg_residences_sm, sm_within_800m==0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_sm_800m_0) +
  tm_dots("sm_within_800m", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_sm_800m) + 
  tm_dots(col="sm_within_800m", breaks=c(1, 3, 5, 7), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=1) + 
  tm_layout(frame=F, main.title=paste("Residences by number of supermarkets within 800m"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 3, plot = FALSE)),
                labels = c("0", "1 to 3", "3 to 5", "5 to 7"),
                title = "No. within 800m", border.lwd=0)

tmap_save(filename=paste("NA-2-sm-within-800m.png"), height=3.35, width=4.69)

sg_residences_sm_400m <- subset(sg_residences_sm, sm_within_400m!=0)
sg_residences_sm_400m_0 <- subset(sg_residences_sm, sm_within_400m==0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_sm_400m_0) +
  tm_dots("sm_within_400m", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_sm_400m) + 
  tm_dots(col="sm_within_400m", breaks=c(1, 2, 3, 4), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=1) + 
  tm_layout(frame=F, main.title=paste("Residences by number of supermarkets within 400m"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 3, plot = FALSE)),
                labels = c("0", "1 to 2", "2 to 3", "3 to 4"),
                title = "No. within 400m", border.lwd=0)

tmap_save(filename=paste("NA-2-sm-within-400m.png"), height=3.35, width=4.69)

# Eateries --------- 
sg_residences_ee_0 <- subset(sg_residences_ee, sm_within_1km==0)
sg_residences_ee_1km <- subset(sg_residences_ee, sm_within_1km!=0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_ee_0) +
  tm_dots("sm_within_1km", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_ee_1km) + 
  tm_dots(col="sm_within_1km", breaks=c(1,100, 200, 300, 400, 500, 1400), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=0.7) + 
  tm_layout(frame=F, main.title=paste("Residences by number of eateries within 1km"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 6, plot = FALSE)),
                labels = c("0", "1 to 100", "100 to 200", "200 to 300", "300 to 400", "400 to 500", "500 to 1400"),
                title = "No. within 1km", border.lwd=0)

tmap_save(filename=paste("NA-2-ee-within-1km.png"), height=3.35, width=4.69)

sg_residences_ee_800m <- subset(sg_residences_ee, sm_within_800m!=0)
sg_residences_ee_800m_0 <- subset(sg_residences_ee, sm_within_800m==0)
# breaks=c(1, 50, 150, 250, 350, 450, 1000)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_ee_800m_0) +
  tm_dots("sm_within_800m", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_ee_800m) + 
  tm_dots(col="sm_within_800m", breaks=c(1, 100, 200, 300, 400, 1000), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=1) + 
  tm_layout(frame=F, main.title=paste("Residences by number of eateries within 800m"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 5, plot = FALSE)),
                labels = c("0", "1 to 100", "100 to 200", "200 to 300", "300 to 400", "400 to 1000"),
                title = "No. within 800m", border.lwd=0)

tmap_save(filename=paste("NA-2-ee-within-800m.png"), height=3.35, width=4.69)

sg_residences_ee_400m <- subset(sg_residences_ee, sm_within_400m!=0)
sg_residences_ee_400m_0 <- subset(sg_residences_ee, sm_within_400m==0)

tm_shape(sg) + tm_borders(col="grey", alpha=0.7) +
  tm_shape(sg_residences_ee_400m_0) +
  tm_dots("sm_within_400m", legend.show=F, col="grey", border.lwd=NA, size=0.005, alpha=0.3) +
  tm_shape(sg_residences_ee_400m) + 
  tm_dots(col="sm_within_400m", breaks=c(1, 25, 50, 75, 100, 200, 350), sizes.legend=1.5, border.lwd=NA, 
          palette="YlOrRd", legend.show=F, size=0.025, alpha=1) + 
  tm_layout(frame=F, main.title=paste("Residences by number of eateries within 400m"), main.title.size=0.85, 
            legend.text.size=0.5, legend.title.size=0.75) +
  tm_add_legend(type = "fill", 
                col = c("grey75", tmaptools::get_brewer_pal("YlOrRd", 6, plot = FALSE)),
                labels = c("0", "1 to 25", "25 to 50", "50 to 75", "75 to 100", "100 to 200", "200 to 350"), 
                title = "No. within 400m", border.lwd=0)

tmap_save(filename=paste("NA-2-ee-within-400m.png"), height=3.35, width=4.69)

# 
# 
# # Maps ----------
# library(RColorBrewer)
# library(tmap)
# for (i in ranges){
#   # hawker centres
#   x<-tm_shape(sg) + tm_borders(col="grey", alpha=0.9) +
#     tm_shape(sg_residences_hc) + 
#     tm_dots(col=paste("hc_within_", i, sep=""), style="pretty", sizes.legend=1.5,border.lwd=NA, #legend.show=FALSE, 
#             breaks=c(0, 0, 1, 2, 3, 4, 5),
#             palette=c("grey", brewer.pal(5, "YlOrRd")), title="No. of hawker centres", size=0.025, alpha=0.3) + 
#     tm_layout(frame=F, main.title=paste("Residences by number of hawker centres within 
# ", i, " range", sep=""), main.title.size=1, legend.text.size=0.5, legend.title.size=0.75)
#   assign(paste("hc_", i , sep= ""), x) # + tm_add_legend(type = "fill", brewer.pal(5, "YlOrRd")) 
#   tmap_save(x, filename=paste("NA-1-hc-within-", i,".png", sep=""), height=3.35, width=4.69)
#   
#   # supermarkets
#   x<-tm_shape(sg) + tm_borders(col="grey", alpha=0.9) +
#     tm_shape(sg_residences_sm) + 
#     tm_dots(col=paste("sm_within_", i, sep=""), style="pretty", sizes.legend=1.5, border.lwd=NA, 
#             palette=c("grey", brewer.pal(5, "YlOrRd")), title="No. of supermarkets", size=0.025, alpha=0.3) + 
#     tm_layout(frame=F, main.title=paste("Residences by number of supermarkets within
# ", i, " range", sep=""), main.title.size=1, legend.text.size=0.5, legend.title.size=0.75)
#   assign(paste("sm_", i , sep= ""), x)
#   tmap_save(x, filename=paste("NA-1-sm-within-", i,".png", sep=""), height=3.35, width=4.69)
#   
#   # eating establishments
#   x<-tm_shape(sg) + tm_borders(col="grey", alpha=0.9) +
#     tm_shape(sg_residences_ee) + 
#     tm_dots(col=paste("sm_within_", i, sep=""), style="pretty", sizes.legend=1.5, border.lwd=NA, 
#             palette=c("grey", brewer.pal(5, "YlOrRd")),
#             title="No. of eating establishments", size=0.025, alpha=0.3) + 
#     tm_layout(frame=F, main.title=paste("Residences by number of eating establishments
# within ", i, " range", sep=""), main.title.size=1, 
#               legend.text.size=0.5, legend.title.size=0.75)
#   assign(paste("ee_", i , sep= ""), x)
#   tmap_save(x, filename=paste("NA-1-ee-within-", i,".png", sep=""), height=3.35, width=4.69)
# }
# 
# # y <- tmap_arrange(hc_1km, sm_1km, ee_1km, 
# #              hc_800m, sm_800m, ee_800m,
# #              hc_400m, sm_400m, ee_400m,
# #              ncol=3, nrow=3)
# # 
# # tmap_save(y, filename="NA-1-ALL.png", height=3.35, width=4.69)
