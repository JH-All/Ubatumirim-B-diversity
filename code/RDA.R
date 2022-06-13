## packages of interest
packages = c("readxl", "tidyverse", "ggtext", "ggthemes", "vegan", "showtext",
             "ggrepel")

## install or load
package.check <- lapply(
  packages,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  }
)

## preparing data
env <- read_excel("environmental_data.xlsx")
data <- read_excel("abundance_data.xlsx", sheet="guilds")
abund <- read_excel("abundance_data.xlsx", sheet="abundance")

## transforming abundance data
dfhell <- decostand(abund[,3:97],method="hellinger")

## RDA
rda1 <-rda(dfhell ~ BT +BS + Phi, data=env)
score1 <-scores(rda1, scalling = 3)
score1

guilds = data.frame(data, score1$species)
envir = data.frame(rda1$CCA$biplot)

summary(rda1) #Proportion Explained RDA1 and  RDA2
anova(rda1, by="terms") #ANOVA for the RDA

## figure number 5
guilds <- guilds %>%
  mutate(should_be_labeled=ifelse(Species %in% c("Parbra", "Pelhar",
                                                 "Ctegra", "Peppar", "Steras",
                                                 "Cataga", "Eucarg", "Micfur",
                                                 "Isopar", "Haeaur"), TRUE, FALSE))
ggplot(guilds, aes(x=RDA1, y=RDA2))+
  geom_point(aes(color=Guilds),shape=16, size=3, alpha=0.6)+
  geom_segment(data=envir, aes(x=0, y=0, xend=RDA1*0.5, yend=RDA2*0.5), 
               alpha=0.8,arrow = arrow(length = unit(0.5, "cm")))+
  annotate("text", x =envir$RDA1*0.55, y  = envir$RDA2*0.55, 
           label = row.names(envir))+
  xlab("RDA1 (18.01%)")+
  ylab("RDA2 (0.057%)")+
  geom_label_repel(data=filter(guilds,should_be_labeled==TRUE), 
                   aes(label=Species, fill=should_be_labeled))+
  guides(fill="none")+
  theme_bw()
