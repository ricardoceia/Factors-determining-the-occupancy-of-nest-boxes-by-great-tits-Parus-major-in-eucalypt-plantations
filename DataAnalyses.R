# Article: Factors determining the occupancy of nest-boxes by great tits (Parus major) in eucalypt plantations
# Analyses: Great tit occupancy analyses (binomial)
# Date:     2022-09-05

rm(list = ls()) # Clear environment
#### Setting working directory ####
setwd("C:/Nest-box selection in eucalypt stands/Data")
getwd()

#### Importing data ####
library(openxlsx)
nb <- read.xlsx("Dataset.xlsx",sheet=1,colNames=T) # Importing data
str(nb) # Checking the original column type
# and then adjusting columns type to numerical, categorical, or ordered
nb$year = factor(nb$year, levels = c("1", "2", "3"), ordered = TRUE)
nb$plot = as.factor(nb$plot)
nb$nestbox = as.factor(nb$nestbox)
nb$nestbox_ID = as.factor(nb$nestbox_ID)
nb$species = as.factor(nb$species)
nb$aspect.q = as.factor(nb$aspect.q)
nb$nestOpen.q = as.factor(nb$nestOpen.q)
nb$occupPrevYear = as.factor(nb$occupPrevYear)
nb$typeAdjHab = as.factor(nb$typeAdjHab)
str(nb) # Making sure all trait categories have changed as desired, and all cells still contain values. 

#### Test correlations ####
corr <- data.frame(nb[,c(9:27)]) # Convert data to correlation analysis
library(GGally)
ggcorr(corr,method = c("pairwise", "spearman"),
       nbreaks = 6,
       hjust = 0.8,
       label = TRUE,
       label_size = 3,
       color = "grey50")
cor.test(nb$nestTreeDiameter,nb$treeHgt)
cor.test(nb$shrubHgt, nb$shrubS)

#### GLM for year and plot ### 
summary(glm(GreatTit ~ year * plot, data=nb, family=binomial(link="logit")))
library(car)
Anova(glm(GreatTit ~ year * plot, data=nb, family=binomial(link="logit")), 
      test.statistic = "F")

#### GLMM for occupancy in the previous year ####
library(lme4)
summary(glmer(GreatTit ~ occupPrevYear + (1|year) + (1|plot), data=nb, family=binomial(link="logit"), nAGQ=1))

#### GLMMs for great tit occupancy ####
trans.cosine <- function(x){cos(pi*x/180)} #Cosine transformation for aspect and nestOpen
summary(global.model <- glmer(GreatTit ~ altitude + slope + trans.cosine(aspect) +
                           nestHgt + trans.cosine(nestOpen) +
                           EucCov + nonEucCov + shrubCov +
                           treeHgt + shrubHgt + 
                           distTrack + distWater +
                           distAdjHab + typeAdjHab + sizeAdjHab + 
                           (1|occupPrevYear) + (1|year) + (1|plot),
                         data=nb, family=binomial(link="logit"), nAGQ=1,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5))))
vif(global.model) # vif<2.5
library(arm)
stdz_global.model <- standardize(global.model, standardize.y = FALSE)
options(na.action=na.fail)
library(MuMIn)
dredge_global.model <- dredge(stdz_global.model)
top.models <- get.models(dredge_global.model,subset = delta<2)
avg.models <- model.avg(top.models)
summary(avg.models)

summary(null.model <- glmer(GreatTit ~ 1 + (1|occupPrevYear) + (1|year) + (1|plot),
                         data=nb, family=binomial(link="logit"), nAGQ=1,
                         control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))) # Null model
AICc(global.model,null.model) # Extracting AICc

#### Plot model coefficients ####
mA<-summary(avg.models) #pulling out model averages
df1<-as.data.frame(mA$coefmat.full) #selecting full model coefficient averages
CI <- as.data.frame(confint(avg.models, full=T)) # get confidence intervals for full model
df2 <- cbind(df1, "CI.min" = CI$'2.5 %', "CI.max" = CI$'97.5 %') #combined table
library(data.table)
setDT(df2, keep.rownames = "coefficient") #put rownames into column
names(df2) <- gsub(" ", "", names(df2)) # remove spaces from column headers
df2 <- df2[-c(1), ] 
level_order <- factor(df2$coefficient, level = c('z.sizeAdjHab','c.typeAdjHab',
                                                 'z.distAdjHab','z.distWater','z.distTrack',
                                                 'z.shrubS','z.shrubHgt','z.treeHgt',
                                                 'z.shrubCov','z.nonEucCov','z.EucCov',
                                                 'z.nestTreeDiameter','z.nestOpen','z.nestHgt',
                                                 'z.aspect','z.slope','z.altitude'))
library(ggplot2)
ggplot(data=df2, aes(x = level_order, y=Estimate))+
  scale_x_discrete(labels = c('* adjacent habitat size','adjacent habitat type','distance from adjacent habitat',
                              '* distance from watercourse','distance from track',
                              'understory height','canopy height','% understory cover',
                              '** % canopy cover by other trees','% canopy cover by eucalypts',
                              '* nest-box hanging height','altitude'))+
  geom_hline(yintercept=0, colour = "#696969",linetype="dashed", lwd=1.5)+ #add dashed line at zero
  geom_errorbar(aes(ymin=CI.min, ymax=CI.max), colour="grey",
                width=0, lwd=1.5) +
  coord_flip()+ # flipping x and y axes
  geom_point(size=4)+theme_classic(base_size = 22)+ 
  labs(x = "", y = "coefficient")

dev.off() # Clear plots
rm(list = ls()) # Clear environment
