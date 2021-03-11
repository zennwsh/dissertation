setwd("~/Documents/School/Dissertation/Workspace")

# Data Cleaning ----- 
# 0. Population by TOD (2015) ---- 
### Population by planning area (PA) ----
x<-read.csv("Datasets/respopagesextod2011to2020.csv")
x<-subset(x, Time==2015)
pop.by.PA<-aggregate(x$Pop, by=list(x$PA), FUN=sum)
colnames(pop.by.PA)<-c("Group.1", "x")

# 1a. Population by elderly (2015) ----
by.age<-aggregate(x$Pop, by=list(x$PA, x$AG), FUN=sum)
colnames(by.age) <- c("PA", "age", "pop")
by.age$age <- as.factor(by.age$age)
has_elderlies <- levels(by.age$age)[14:19]

by.elderly <- by.age
by.elderly$elderly <- ifelse(by.age$age %in% has_elderlies, 1, 0)

by.elderly <- subset(by.elderly, elderly==1)
by.elderly <- aggregate(by.elderly$pop, by=list(by.elderly$PA), FUN=sum)

by.elderly <- merge(by.elderly, pop.by.PA, by="Group.1")

by.elderly$prop <- by.elderly$x.x / by.elderly$x.y

colnames(by.elderly) <- c("PA", "pop", "PA.pop", "prop.elderly")
by.elderly <- by.elderly[c(1, 4)]
write.csv(by.elderly, "1-by-elderly.csv")

# 1b. PA by children --------
has_children <- levels(by.age$age)[1:3]
has_children <- as.data.frame(has_children)
has_children_1 <- levels(by.age$age)[10]
has_children <- rbind(has_children, has_children_1)
by.children <- by.age
has_children <- as.matrix(has_children)

by.children$children <- ifelse(by.age$age %in% has_children, 1, 0)

by.children <- subset(by.children, children==1)
by.children <- aggregate(by.children$pop, by=list(by.children$PA), FUN=sum)

by.children <- merge(by.children, pop.by.PA, by="Group.1")

by.children$prop <- by.children$x.x / by.children$x.y

colnames(by.children) <- c("PA", "pop", "PA.pop", "prop.children")
by.children <- by.children[c(1, 4)]
write.csv(by.children, "1-by-children.csv")

# 2. PA by dwelling type  ---------
by.dwelling<-aggregate(x$Pop, by=list(x$PA, x$TOD), FUN=sum)
colnames(by.dwelling) <- c("PA", "TOD", "pop")

by.dwelling <- cbind(by.dwelling, pop.by.PA)
by.dwelling = by.dwelling[,!(names(by.dwelling) %in% c("Group.1"))]

by.dwelling$prop <- by.dwelling$pop/by.dwelling$x
write.csv(by.dwelling, "by-dwelling.csv")

has_HDB <- by.dwelling$TOD %in% grep("HDB", by.dwelling$TOD, value=T)
by.dwelling.1 <- subset(by.dwelling, has_HDB)
by.dwelling.1 <- aggregate(by.dwelling.1$prop, by=list(by.dwelling.1$PA), FUN=sum)
colnames(by.dwelling.1) <- c("PA", "prop.in.HDB")

write.csv(by.dwelling.1, "1-by-dwelling.csv")

# 3. PA by sex  ------
by.sex<-aggregate(x$Pop, by=list(x$PA, x$Sex), FUN=sum)
colnames(by.sex) <- c("PA", "sex", "pop")

by.sex <- cbind(by.sex, pop.by.PA)
by.sex = by.sex[,!(names(by.sex) %in% c("Group.1"))]
colnames(by.sex) <- c("PA", "sex", "pop", "pop.by.PA")

by.sex$prop <- by.sex$pop/by.sex$pop.by.PA
by.sex <- subset(by.sex, sex=="Males")
by.sex <- by.sex[c(1, 5)]
colnames(by.sex) <- c("PA", "prop.males")
write.csv(by.sex, "1-by-sex.csv")

# 4. PA by household structure within HDB dwellers -----
by.structure <- read.csv("datasets/by-structure.csv")
by.structure$pop <- by.structure$value*1000
drop <- c("year", "level_2", "value")
by.structure = by.structure[,!(names(by.structure) %in% drop)]
colnames(by.structure) <- c("structure", "PA", "pop")

by.structure.PA <- subset(by.structure, structure=="Total")
by.structure <- subset(by.structure, structure!="Total")
by.structure <- merge(by.structure, by.structure.PA, by="PA")

by.structure$prop <- by.structure$pop.x/by.structure$pop.y
by.structure <- subset(by.structure, structure.x=="One Family Nucleus - Three Or More Generations")
by.structure.1 <- by.structure
by.structure <- by.structure[c(1,6)]
colnames(by.structure) <- c("PA", "prop.3gen")
write.csv(by.structure, "by-structure.csv")

# 5. PA by ethnicity ---------- 
by.ethnic <- read.csv("datasets/by-ethnic.csv")
by.ethnic<-by.ethnic[by.ethnic$level_2!="Females", ]
by.ethnic<-by.ethnic[by.ethnic$level_2!="Males", ]
by.ethnic<-aggregate(by.ethnic$value, by=list(by.ethnic$level_1, by.ethnic$level_3), FUN=sum)
by.ethnic<-by.ethnic[!grepl("Total", by.ethnic$Group.1),]
by.ethnic<-by.ethnic[by.ethnic$Group.2!="Total", ]

colnames(by.ethnic)<-c("ethnicity", "PA", "pop")

by.ethnic$PA <- gsub(paste0("- Total",collapse = "|"),"", by.ethnic$PA)

by.ethnic.PA.pop <- aggregate(by.ethnic$pop, by=list(by.ethnic$PA), FUN=sum)
by.ethnic <- merge(by.ethnic, by.ethnic.PA.pop, by.x="PA", by.y="Group.1")

by.ethnic$prop <- by.ethnic$pop/by.ethnic$x
write.csv(by.ethnic, "by-ethnic.csv")

by.ethnic.1 <- subset(by.ethnic, ethnicity=="Chinese")
by.ethnic.1 <- by.ethnic.1[c(1, 5)]
colnames(by.ethnic.1) <- c("PA", "prop.chinese")
write.csv(by.ethnic.1, "1-by-ethnic.csv")

# 6. PA by economic status ---------- 
by.econ <- read.csv("datasets/by-economic-status.csv")
by.econ$level_1<-as.factor(by.econ$level_1)
by.econ$level_1 <- gsub(paste0("Working- ",collapse = "|"),"", by.econ$level_1)

by.econ<-by.econ[by.econ$level_1!="Total", ]
by.econ$pop <- by.econ$value*1000
by.econ<-by.econ[by.econ$level_2!="Males", ]
by.econ<-by.econ[by.econ$level_2!="Females", ]
by.econ <- subset(by.econ, select=c("level_1", "level_4", "pop"))
colnames(by.econ) <- c("status", "PA", "pop")
by.econ.1 <- subset(by.econ, status=="Economically Active- Total" | status=="Economically Inactive")

by.econ.PA.pop <- aggregate(by.econ.1$pop, by=list(by.econ.1$PA), FUN=sum)
by.econ <- merge(by.econ, by.econ.PA.pop, by.x="PA", by.y="Group.1")

by.econ$prop <- by.econ$pop/by.econ$x
write.csv(by.econ, "by-econ.csv")

by.econ.1 <- subset(by.econ, status=="Economically Active- Working")
by.econ.1 <- aggregate(by.econ.1$prop, by=list(by.econ.1$PA), FUN=sum)
colnames(by.econ.1) <- c("PA", "prop.working")
write.csv(by.econ.1, "1-by-econ.csv")

# 7. PA by highest qualification attained -----
by.hqa <- read.csv("datasets/by-hqa.csv")
by.hqa <- subset(by.hqa, select=c("level_1", "level_3", "value"))
colnames(by.hqa) <- c("hqa", "PA", "value")
by.hqa<-by.hqa[by.hqa$hqa!="Total", ]
by.hqa$pop <- by.hqa$value*1000

by.hqa.PA.pop <- aggregate(by.hqa$pop, by=list(by.hqa$PA), FUN=sum)
by.hqa <- merge(by.hqa, by.hqa.PA.pop, by.x="PA", by.y="Group.1")

by.hqa$prop <- by.hqa$pop/by.hqa$x
write.csv(by.hqa, "by-hqa.csv")

by.hqa.1 <- subset(by.hqa, hqa=="University")
by.hqa.1 <- subset(by.hqa.1, select=c(1, 6))
colnames(by.hqa.1) <- c("PA", "prop.by.uni")
write.csv(by.hqa.1, "1-by-hqa.csv")

# 8. PA by language literacy -----
by.lang <- read.csv("datasets/by-language-lit.csv")
by.lang <- subset(by.lang, select=c("level_1", "level_3", "value"))
by.lang.total <- subset(by.lang, level_1=="Total")
by.lang<-by.lang[by.lang$level_1!="Total", ]
colnames(by.lang) <- c("lit", "PA", "value")

by.lang <- merge(by.lang, by.lang.total,
                 by.y = "level_3",
                 by.x = "PA")

by.lang$prop <- by.lang$value.x / by.lang$value.y

by.lang<- by.lang[c(-4)]

colnames(by.lang) <- c("PA", "lit", "pop", "PA.pop", "prop")

write.csv(by.lang, "by-lang-lit.csv")

by.lang.1 <- subset(by.lang, grepl(paste("English"), by.lang$lit, ignore.case=T))
by.lang.1 <- aggregate(by.lang.1$prop, by=list(by.lang.1$PA), FUN=sum)
colnames(by.lang.1) <- c("PA", "prop.english")

write.csv(by.lang.1, "1-by-lang-lit.csv")

# 9. PA by tenancy -----
by.tenancy <- read.csv("datasets/by-tenancy.csv")
by.tenancy <- subset(by.tenancy, select=c("level_1", "level_3", "value"))
by.tenancy.total <- subset(by.tenancy, level_1=="Total")
by.tenancy<-by.tenancy[by.tenancy$level_1!="Total", ]

colnames(by.tenancy) <- c("tenancy", "PA", "value")

by.tenancy <- merge(by.tenancy, by.tenancy.total,
                    by.x="PA",
                    by.y="level_3")

by.tenancy$prop <- by.tenancy$value.x / by.tenancy$value.y
write.csv(by.tenancy, "by-tenancy.csv")

by.tenancy.owner<-by.tenancy[by.tenancy$tenancy=="Owner", ]
by.tenancy.owner<-subset(by.tenancy.owner, select=c("PA", "prop"))
colnames(by.tenancy.owner) <- c("PA", "prop.owners")
by.nonowners <- as.data.frame(by.tenancy.owner$PA)
by.nonowners$prop.non.owners <- 1-by.tenancy.owner$prop.owners
colnames(by.nonowners) <- c("PA", "prop.nonowners")

write.csv(by.tenancy.owner, "1-by-tenancy-owner.csv")
write.csv(by.nonowners, "1-by-tenancy-non-owner.csv")

# 11. PA by religion SUPERSEDED --------------
by.religion <- read.csv("datasets/by-religion.csv")
by.religion <- subset(by.religion, select=c("level_1", "level_3", "value"))
colnames(by.religion) <- c("religion", "PA", "value")        
by.religion.total <- subset(by.religion, religion=="Total")
by.religion<-by.religion[by.religion$religion!="Total", ]

by.religion <- merge(by.religion, by.religion.total,
                     by.y="PA",
                     by.x="PA")

by.religion$prop <- by.religion$value.x / by.religion$value.y
colnames(by.religion) <- c("PA", "religion", "pop", "total", "PA.pop", "prop")
by.religion <- by.religion[c(-4)]

write.csv(by.religion, "by-religion.csv")

# 10. PA by poverty -----
by.income <- read.csv("datasets/by-income.csv")
by.income <- by.income[c(2,4,5)]
colnames(by.income) <- c("income", "PA", "value")
by.income.total <- subset(by.income, by.income$income=="Total")
by.income<-by.income[by.income$income!="Total", ]

by.income <- merge(by.income, by.income.total,
          by="PA")

by.income$prop <- by.income$value.x / by.income$value.y
by.income <- by.income[c(-4)]
colnames(by.income) <- c("PA", "income", "pop", "PA.pop", "prop")

write.csv(by.income, "by-income.csv")

by.income$income<-as.factor(by.income$income)
below_median <- levels(by.income$income)[c(1, 2, 6, 7, 8, 15)]
in_poverty <-  levels(by.income$income)[c(1, 15)]
in_wealth <- levels(by.income$income)[c(3, 4, 5, 11:14)]
by.income$in_poverty <- ifelse(by.income$income %in% in_poverty, 1, 0)

by.poverty <- subset(by.income, in_poverty==1)
by.poverty <- aggregate(by.poverty$prop, by=list(by.poverty$PA), FUN=sum)
colnames(by.poverty) <- c("PA", "prop.poverty")
write.csv(by.poverty, "by-poverty.csv")

by.income$in_wealth <- ifelse(by.income$income %in% in_wealth, 1, 0)
by.wealth <- subset(by.income, in_wealth==1)
by.wealth<- aggregate(by.wealth$prop, by=list(by.wealth$PA), FUN=sum)

colnames(by.wealth) <- c("PA", "prop.wealth")
write.csv(by.wealth, "by-wealth.csv")

# Overall dataset ----- 
dataset <- merge(by.econ.1, by.elderly, 
                 by="PA")

df <- list(by.ethnic.1, by.structure, by.lang.1, by.nonowners,
           by.children, by.hqa.1, by.poverty, by.wealth, by.dwelling.1)

for (i in df){
  dataset <- merge(dataset, i, 
                 by.x="PA",
                 by.y="PA")
}

write.csv(dataset, "1-dataset-MASTER.csv")

col_names <- c("Working", "Elderly", "Chinese",
               "Multigenerational", "English speakers", "Property non-owners", "Children",
               "University-educated", "Poor", "Wealthy", "Lives in HDB")

colnames(dataset)[2:12] <- col_names

x <- round(cor(dataset[,2:12]),2)
library(GGally)
ggcorr(dataset[,2:12], nbreaks = 6, 
       palette = "RdGy", label = TRUE, 
       label_size = 2, label_color = "white",
       label_round = 1.5, 
       size = 2,hjust=0.75, color="grey50",layout.exp = 2)

ggsave("SES-correlations.png", height=3, width=5)

# Singapore means ---------
sg_means <- setNames(data.frame(matrix(ncol = 11, nrow = 1)), col_names)
sg_means$Elderly <- sum(by.age$pop[by.age$age %in% has_elderlies]) / sum(by.age$pop)
sg_means$Children <- sum(by.age$pop[by.age$age %in% has_children]) / sum(by.age$pop)

by.dwelling$TOD <- as.factor(by.dwelling$TOD)
has_HDB <- levels(by.dwelling$TOD)[2:5]
sg_means$`Lives in HDB` <- sum(by.dwelling$pop[by.dwelling$TOD %in% has_HDB]) / sum(by.dwelling$pop)

sg_means$`Multigenerational` <- sum(by.structure.1$pop.x)/sum(by.structure.1$pop.y) 

sg_means$Chinese <- sum(by.ethnic$pop[by.ethnic$ethnicity=="Chinese"]) / sum(by.ethnic$pop)

sg_means$`University-educated` <- sum(by.hqa$pop[by.hqa$hqa=="University"]) / sum(by.hqa$pop)

by.econ <- subset(by.econ, status=="Economically Active- Working" | status=="Economically Inactive")
sg_means$`Working` <- sum(by.econ$pop[by.econ$status=="Economically Active- Working"]) / sum(by.econ$pop)

by.lang$lit<-as.factor(by.lang$lit)
by.lang <- subset(by.lang, lit!="Literate")
by.lang <- subset(by.lang, lit!="Not Literate")
by.lang$lit <- droplevels(by.lang$lit)
has_english <- levels(by.lang$lit)[c(2, 7:10)]
sg_means$`English speakers` <- sum(by.lang$pop[by.lang$lit %in% has_english]) / sum(by.lang$pop)

sg_means$`Wealthy` <- sum(by.income$pop[by.income$income %in% in_wealth]) / sum(by.income$pop)
sg_means$`Poor` <- sum(by.income$pop[by.income$income %in% in_poverty]) / sum(by.income$pop)

sg_means$`Property non-owners` <- 1 - (sum(by.tenancy$value.x[by.tenancy$tenancy=="Owner"]) / sum(by.tenancy$value.x))

write.table(round(sg_means, 3), file="SE_means.doc", sep=",")
write.csv(sg_means, "1-sg_means.csv")

# PA map ---------
sg <- readOGR("./Datasets/master-plan-2014-planning-area-boundary-no-sea-shp")
sg_PA <- sg
CBD.and.more <- c("Downtown Core", "Marina East", "Marina South",  "Museum", "Newton", "Orchard", "River Valley", "Rochor", "Singapore River", "Straits View",
                  "Changi Bay", "lim chu kang", "western water catchment", "tuas", "tengah", "seletar", "paya lebar",
                  "pioneer", "central water catchment", "north-eastern islands", "southern islands", "western islands")
CBD.and.more <- toupper(CBD.and.more)
remove <- (sg_PA$PLN_AREA_N %in% CBD.and.more)
sg_PA$PLN_AREA_N[remove]<- ""
writeOGR(sg_PA,layer="liveable-PAs", driver="ESRI Shapefile", dsn="./Datasets/SG-PAs-litreview")

tm_shape(sg_PA) + 
  tm_borders(col="grey", alpha=0.9) +
  tm_layout(frame=F, 
            main.title="Singapore's residential planning areas (2015)",
            main.title.size=1) + tm_text("PLN_AREA_N", col="grey40", size = 0.4) 
tmap_save(filename="SG_PAs.png", width=7, height=5)

# Population map to show 0 population PAs ---------
library(rgdal)
pop.by.PA$Group.1 <- toupper(pop.by.PA$Group.1)
pop.sg <- merge(sg, pop.by.PA,
                by.x="PLN_AREA_N",
                by.y="Group.1")

writeOGR(pop.sg,layer="POP-PAs", driver="ESRI Shapefile", dsn="./Datasets/SG-PAs-methodology")

# SE <- read.csv("1-dataset-MASTER.csv")
# SE_PAs <- as.vector(SE$PA)
# SE_PAs <- toupper(SE_PAs)

and.more <- c("Changi Bay", "lim chu kang", "western water catchment", "tuas", "tengah", "seletar", "paya lebar",
              "pioneer", "central water catchment", "north-eastern islands", "southern islands", "western islands")
and.more <- toupper(and.more)
remove <- (pop.sg$PLN_AREA_N %in% and.more)
pop.sg$PLN_AREA_N[remove]<- ""
pop.sg$PLN_AREA_N

library(tmap)
my_palette <- c("#ffffff", "#eaf5f0", "#aed9c4", "#7ba691", "#5c7c6d", "#3d5348") 
tm_shape(pop.sg) + tm_fill("x", style="fixed", colorNA="white",
                                  breaks=c(0, 1, 10000, 50000, 100000, 200000, 300000), showNA = F, 
                                  palette=c("#ffffff", brewer.pal(6,"Greens")), title="Population", alpha=0.6) + 
  tm_borders(col="grey", alpha=0.9) +
  tm_layout(frame=F, 
            main.title="Singapore population by planning area (2015)",
            main.title.size=1) + tm_text("PLN_AREA_N", col="grey40", size = 0.4) 
tmap_save(filename="Population_by_PA.png", width=7, height=5)

# Descriptive statistics ---------
descriptive <- setNames(as.data.frame(matrix(ncol=3, nrow=11)), c("min", "max", "range"))
row.names(descriptive) <- colnames(dataset)[2:12]

for (i in c(2:12)){
  descriptive[i-1,1] <- min(dataset[,i])
  descriptive[i-1,2] <- max(dataset[,i])
  descriptive[i-1,3] <- range(dataset[,i])[2] - range(dataset[,i])[1]
}
write.csv(round(descriptive,3), "descriptive.csv") 

# Correlation ------------
library(ggcorrplot)
corr <- round(cor(dataset[2:12]), 3)
p.val <- cor_pmat(dataset[2:12])
ggcorrplot(corr, p.mat = p.val, hc.order = TRUE, lab = T, 
           type = "lower", insig = "blank", 
           outline.col = "white",
           ggtheme = ggplot2::theme_classic,
           colors = c("#6D9EC1", "white", "#E46726"), lab_size=3)

ggsave("cor_plot.png", width=7, height=5)

