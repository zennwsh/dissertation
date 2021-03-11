setwd("~/Documents/School/Dissertation/Workspace")
final.table <- read.csv("Final_Cluster_Agg.csv")
national <- as.data.frame(final.table[6,])
final.table <- final.table[-6,]
final.table$Cluster.1 <- as.factor(final.table$Cluster.1)
final.table$Cluster.1 <- factor(final.table$Cluster.1, 
                                levels=c("Working Parents", "City Elites", "Comfortable Retirement", "Constrained Elderly", "Diverse Typicals"))

prettiest_colors_2 <- c("#F7A8B2", "#FEF8DD", "#D1EED3", "#cce6ff", "#D8C3E0")

# Availability ---------
library(ggplot2) 
library(ggthemes)
ggplot(data=final.table, aes(y=HC, x=Cluster.1))  + theme_classic() +
  geom_col(fill=prettiest_colors_2) + 
  geom_text(aes(label=HC), vjust=-0.5, size=3,color="Grey20") +
  labs(title="Availability of hawker centres") + 
  ylab("No. per 100,000 residents") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1)) +
  coord_cartesian(ylim = c(0, 1.10))

ggsave("final-availability-hc.png", height=4, width=5)

ggplot(data=final.table, aes(y=SM, x=Cluster.1))  + theme_classic() +
  geom_col(fill=prettiest_colors_2) + 
  geom_text(aes(label=SM), vjust=-0.5, size=3,color="Grey20") +
  labs(title="Availability of supermarkets") + 
  ylab("No. per 100,000 residents") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1)) +  coord_cartesian(ylim = c(0, 4.1)) 

ggsave("final-availability-SM.png", height=4, width=5)

ggplot(data=final.table, aes(y=EE, x=Cluster.1))  + theme_classic() +
  geom_col(fill=prettiest_colors_2) + 
  geom_text(aes(label=EE), vjust=-0.5, size=3,color="Grey20") +
  labs(title="Availability of eating establishments") + 
  ylab("No. per 100,000 residents") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1)) +   coord_cartesian(ylim = c(0, 170)) 

ggsave("final-availability-EE.png", height=4, width=5)

# Accessibility --------
access.table <- read.csv("Final_Cluster_Access.csv")
access.table <- access.table[1:15,]
access.table$Distance <-  as.factor(access.table$Distance)
access.table$Distance <- factor(access.table$Distance, levels=c("400m", "800m", "1km"))
access.table$Cluster.1 <- factor(access.table$Cluster.1, 
                                 levels=c("Working Parents", "City Elites", "Comfortable Retirement", "Constrained Elderly", "Diverse Typicals"))

ggplot(data=access.table, aes(y=HC, x=Cluster.1 ,fill=Distance))  + theme_classic() +
  geom_col(position=position_dodge(width=0.75)) + scale_fill_brewer(palette="Blues", direction=-1) +
  labs(title="Accessibility of hawker centres") + 
  geom_text(aes(label=HC), position=position_dodge(width=0.75), vjust=-1, hjust=0.6, size=1.5,color="Grey20") +
  ylab("No. of hawker centres accessible within buffer") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1))

ggsave("final-accessibility-hc.png", height=4, width=5.5)

ggplot(data=access.table, aes(y=SM, x=Cluster.1 ,fill=Distance))  + theme_classic() +
  geom_col(position=position_dodge(width=0.75)) + scale_fill_brewer(palette="Greens", direction=-1) +
  labs(title="Accessibility of supermarkets") + 
  geom_text(aes(label=SM), position=position_dodge(width=0.75), vjust=-1, hjust=0.6, size=1.5,color="Grey20") +
  ylab("No. of supermarkets accessible within buffer") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1))

ggsave("final-accessibility-SM.png", height=4, width=5)

ggplot(data=access.table, aes(y=EE, x=Cluster.1 ,fill=Distance))  + theme_classic() +
  geom_col(position=position_dodge(width=0.75)) + scale_fill_brewer(palette="Reds", direction=-1) +
  labs(title="Accessibility of eateries") + 
  geom_text(aes(label=EE), position=position_dodge(width=0.75), vjust=-1, hjust=0.6, size=1.5,color="Grey20") +
  ylab("No. of eateries accessible within buffer") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1))

ggsave("final-accessibility-EE.png", height=4, width=5)

 # Affordability ---------
ggplot(data=final.table, aes(y=Afford, x=Cluster.1))  + theme_classic() +
  geom_col(fill=prettiest_colors_2) +  scale_y_continuous(labels=scales::dollar) + 
  geom_text(aes(label=paste("$", Afford, sep="")), vjust=-0.5, size=2.5,color="Grey20") +
  labs(title="Affordability of food") + 
  ylab("Average price of a meal") +
  xlab("") + 
  theme(text = element_text(size=10), plot.title = element_text(hjust = 0.5), 
        axis.text.x = element_text(angle=40, hjust=1)) +
  coord_cartesian(ylim = c(11, 13))

ggsave("final-afford.png", height=5, width=4)
