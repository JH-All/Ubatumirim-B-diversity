## packages of interest
packages = c("readxl", "adespatial", "tidyverse", "ggtext", "writexl")

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
data <- read_excel("abundance_data.xlsx", sheet="abundance")
abund <- data[,3:97]


## calculating beta diversity
BDt <- beta.div(abund, method="hellinger",sqrt.D=FALSE,samp=TRUE,
                nperm=999,save.D=FALSE)

BDt$beta # total beta diversity
BDt$SCBD # species contribution to beta diversity
BDt$LCBD # local contribution to beta diversity
BDt$p.LCBD # p-value for LCBD values


# getting complete species name and their guilds
names <- read_excel("species_and_guilds.xlsx")

# creating a new data frame 
df1 <- as.data.frame(cbind(names, BDt$SCBD))
colnames(df1) <- c("species","guilds", "scbd")
str(df1)
df1$species <- as.factor(df1$species)
df1$guilds <- as.factor(df1$guilds)
df1$scbd <- as.numeric(df1$scbd)


## figure number 2
ggplot(df1, aes(x = reorder(species, -scbd), y = scbd, fill = guilds)) + 
  geom_bar(stat = "identity", alpha=0.6)+
  ylab("Species contribution to beta diversity") + 
  xlab(NULL)+
  coord_flip()+
  theme(legend.position="bottom")+ 
  guides(fill=guide_legend(title="Guilds"))+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.12))+ 
  theme(axis.text.y = element_text(face = "italic"))


## figure number 3
df2<- aggregate(df1$scbd, list(df1$guilds), sum)
colnames(df2) <- c("guilds","summed_scbd")

df2 %>%
  ggplot()+
  geom_col(aes(reorder(guilds,-summed_scbd),summed_scbd, fill=guilds),alpha=0.6) +
  ylab("Summed SCBD Values") + 
  xlab(NULL) + 
  theme_classic()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.65))+
  theme(axis.text.x=element_markdown(),
        legend.text=element_markdown(),
        legend.key.size=unit(10,"pt"),
        legend.position=c(0.85,0.85))+
  guides(fill=guide_legend(title="Guilds"))


## figure number 4
df3 <- aggregate(df1$scbd, list(df1$guilds), mean)
colnames(df3) <- c("guilds","mean_scbd")

df3 %>%
  ggplot()+
  geom_col(aes(reorder(guilds,-mean_scbd),mean_scbd, fill=guilds),alpha=0.6)+
  ylab("Mean SCBD Values") +
  xlab(NULL) + 
  theme_classic()+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.017))+
  theme(axis.text.x=element_markdown(),
        legend.text=element_markdown(),
        legend.key.size=unit(10,"pt"),
        legend.position=c(0.85,0.85))+
  guides(fill=guide_legend(title="Guilds"))

## multiple regression - tables 2 and 3

env <- read_excel("environmental_data.xlsx")
env$LCBD <- BDt$LCBD
reg <- lm(LCBD ~ BT + BS + Phi, data=env)
summary(reg)
my_anova <- anova(reg)
my_anova

## table number 4
data$Season <- as.factor(data$Season)
data$transect <- as.factor(data$transect)
data$LCBD <- BDt$LCBD
data$p.LCBD <- BDt$p.LCBD
View(data)
LCBD_table <- data[,c(1,2,98,99)]
