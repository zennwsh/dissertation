setwd("~/Documents/School/Dissertation/Workspace")
PA.table <- read.csv("Table Final by PA.csv")
PA.table$Planning.Area <- toupper(PA.table$Planning.Area)
PA.table$Afford <- as.numeric(PA.table$Afford)
sg.table <- merge(sg, PA.table, by.x="PLN_AREA_N", by.y="Planning.Area")

tm_shape(sg) + tm_fill(col="#f0f0f0") +
  tm_shape(sg.table) + tm_borders(col="grey", alpha=0.9) + 
  tm_fill("Afford", showNA=F, title="Average cost of meal ($)", palette="Reds", colorNA="Gray95") +
    tm_layout(frame=F,
            main.title="Affordability of food across planning areas in Singapore",
            main.title.size=1) +
  tm_compass(north = 0, type = NA, position="left") + tm_scale_bar(position="center")

tmap_save(filename="Affordability.png", width=7, height=5)
