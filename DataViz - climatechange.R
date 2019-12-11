#Author:  Benjamin Müller
#Course:   Data Vizualization 
#Exercise: Project
#Date of talk: 11.12.2019

#--------------------------

#Settings in advance
#Please change the working directory to the working directory you're working on.
setwd("C:/Users/deyno/Documents/R Workspace/DataVizProjekt - DatenKlima")
set.seed(666) #probably not necessary
rm(list = ls())
options(scipen = 999) #disabling scientific e+10 notification

#--------------------------


#install.packages

install.packages("ggplot2")
install.packages("eurostat")
install.packages("reshape2")
install.packages("ggrepel")
install.packages("dplyr")

#load.packages
library("ggplot2")
library("eurostat")
library("reshape2")
library("ggrepel")
library("dplyr")
library(tidyverse)
library(HSAUR3)
library(factoextra)


#--------------------------

#to do



#---------------------------

#--load and adjust data sets--

#unify data sets if possible
#(this might not be necessary)

#Datasets are loaded from the website, because the automated tool get_eurostat from eurostat package shortens the labels
#in a manner, where the labels are not completely clear anymore.

# data set 1
env_ac_ainah_r2 <- read.csv("./Data/Air emissions accounts by NACE Rev. 2 activity (env_ac_ainah_r2)/env_ac_ainah_r2_1_Data.csv")
str(env_ac_ainah_r2)

env_ac_ainah_r2$Value <- levels(env_ac_ainah_r2$Value)[env_ac_ainah_r2$Value]

env_ac_ainah_r2$Value <- gsub(",","",env_ac_ainah_r2$Value) 
     # 1000 are seperated by comma, e.g. 1,000,000 for a million
     # This has to be deleted for the as.numeric to work properly

env_ac_ainah_r2$Value <- as.double(env_ac_ainah_r2$Value) #NAs introduced by coercion!

sum(is.na(env_ac_ainah_r2$Value))
#There are 198 NAs here

#Simple imputation method: Replace NAs by 0

env_ac_ainah_r2$Value <- replace(env_ac_ainah_r2$Value, is.na(env_ac_ainah_r2$Value), 0)

#Check the sum of NAs and the resulting sum is zero.


#shorten the labels of the factors---

#look at the factor names of the account of greenhous gases
levels(env_ac_ainah_r2$NACE_R2) 

#too long descriptions shall be shortened, hopefully without changing the meaning
#Careful approch by now
levels(env_ac_ainah_r2$NACE_R2) [levels(env_ac_ainah_r2$NACE_R2) == "Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use"] <- "Activities of households es employers"

#Look at the names of the states

levels(env_ac_ainah_r2$GEO) [levels(env_ac_ainah_r2$GEO) == "Germany (until 1990 former territory of the FRG)"] <- "Germany"


#-----------------------------

# Question: What are the most important drivers of greenhouse gase emission in the European Union?

# First visualisation: A Barplot

#A barplot of the account of greenhous gases for the Total European Union and the year 2017
#the Barplot shall be ordered

env_ac_ainah_r2.total.2017 <- subset(env_ac_ainah_r2, (TIME == 2017) & (GEO == "European Union - 28 countries") & !(NACE_R2 == "Total - all NACE activities"))

#Make a barplot
plot.2017.total.account.barplot <- ggplot(env_ac_ainah_r2.total.2017, aes(x=reorder(NACE_R2,Value), y=Value, fill=NACE_R2))+
  geom_bar(stat = "identity", color = "brown") +
  coord_flip() +
  theme(legend.position = "none") +
  xlab("Account of Greenhous Gases") +
  ylab("Greenhous Gas equivalents in tons") +
  ggtitle ("Emission of Greenhous Gas equivalents in tons by account in the European Union in the year 2017")

#a description should be added
# all countries in the EU, in 2017
# What kind of value is it? CO2 equivalents in tons
plot.2017.total.account.barplot

#-----------------------------

#Question: How has the sccount of greenhouse gases in the European Union has changed from 2008 to 2017?

# Make a time series of the following accounts in one graphic
# - electricity, gas, steam and air conditioning supply
# - manufacturing
# - Transportation and storage
# - Agriculture, forestry and fishing


env_ac_ainah_r2.total.timeseries <- subset(env_ac_ainah_r2, (GEO == "European Union - 28 countries") & ((NACE_R2 == "Electricity, gas, steam and air conditioning supply") | (NACE_R2 == "Manufacturing") | (NACE_R2 == "Transportation and storage") | (NACE_R2 == "Agriculture, forestry and fishing")))                                   

#Change from long to wide format

env_ac_ainah_r2.total.timeseries.wide <- dcast(env_ac_ainah_r2.total.timeseries, TIME~NACE_R2, value.var="Value")

ggplot() +
  geom_line(data = env_ac_ainah_r2.total.timeseries.wide, aes(x = env_ac_ainah_r2.total.timeseries.wide [,1], y = env_ac_ainah_r2.total.timeseries.wide[,2]),color = "green", size = 2)+
  geom_line(data = env_ac_ainah_r2.total.timeseries.wide, aes(x = env_ac_ainah_r2.total.timeseries.wide [,1], y = env_ac_ainah_r2.total.timeseries.wide[,3]),color = "red", size = 2)+
  geom_line(data = env_ac_ainah_r2.total.timeseries.wide, aes(x = env_ac_ainah_r2.total.timeseries.wide [,1], y = env_ac_ainah_r2.total.timeseries.wide[,4]),color = "blue", size = 2)+
  geom_line(data = env_ac_ainah_r2.total.timeseries.wide, aes(x = env_ac_ainah_r2.total.timeseries.wide [,1], y = env_ac_ainah_r2.total.timeseries.wide[,5]),color = "orange", size = 2)+
  xlab("Time in Years") +
  ylab("Emission in tons") +
  ggtitle ("Time Series of Emission of Greenhous Gas equivalents in tons of different accounts") +
  labs(color = c("A", "B", "C", "D"))
  
  
  #Labels are missing. How to add them?
  
#----------------------------

# Question: How has the account of greenhouse gases in Germany has changed from 2008 to 2017?

# Make a time series of the following accounts in one graphic
# - electricity, gas, steam and air conditioning supply
# - manufacturing
# - Transportation and storage
# - Agriculture, forestry and fishing



#-----------------------------


# Question: How similar are different European countries in regard to their account of Greenhouse gas?

env_ac_ainah_r2.2017 <- subset(env_ac_ainah_r2, (TIME == 2017) & !(GEO == "European Union - 28 countries") & !(NACE_R2 == "Total - all NACE activities"))


# First try: Simply use a scatterplot of the biggest two drivers in the European Union in 2017:
# - electricity, gas, steam and air conditioning supply
# - manufacturing

#We can make our dataset then even smaller
env_ac_ainah_r2.2majoracc.2017 <- subset(env_ac_ainah_r2.2017, (NACE_R2 == "Electricity, gas, steam and air conditioning supply") | (NACE_R2 == "Manufacturing"))
# levels(env_ac_ainah_r2.2017$NACE_R2)

#Now the dataset has to change from long to wide data format with the reshape2 package

env_ac_ainah_r2.2majoracc.2017 <- dcast(env_ac_ainah_r2.2majoracc.2017, GEO~NACE_R2, value.var="Value")

#Remove the countries, where there was NA, but now a 0 was imputed.

#These countries are Iceland, Norway, Serbia, Switzerland and Turkey.

env_ac_ainah_r2.2majoracc.2017 <- subset(env_ac_ainah_r2.2majoracc.2017, !(env_ac_ainah_r2.2majoracc.2017[2] == 0))

str(env_ac_ainah_r2.2majoracc.2017)

#Make a scatterplot

plot.2majoracc.2017.scatter <- ggplot(env_ac_ainah_r2.2majoracc.2017, aes(x = env_ac_ainah_r2.2majoracc.2017[,2] , y = env_ac_ainah_r2.2majoracc.2017[,3])) +
  geom_point(size=1, shape=23) +
  geom_text(label=env_ac_ainah_r2.2majoracc.2017$GEO)
  xlab("Emission caused by Energy Supply in tons") +
  ylab("Emission caused by Manufacturing in tons") +
  ggtitle ("Emission of Greenhous Gas equivalents in tons by the 2 major accounts of different European Countries the year 2017")

plot.2majoracc.2017.scatter

#How do you make the names more visible? Use ggrepel
#Keep in mind that ggrepel moves the data points to ensure fewer overlap of the labels

plot.2majoracc.2017.scatter.repel <- ggplot(env_ac_ainah_r2.2majoracc.2017, aes(x = env_ac_ainah_r2.2majoracc.2017[,2] , y = env_ac_ainah_r2.2majoracc.2017[,3])) +
  geom_point(size=1, shape=23) +
  geom_text_repel(label=env_ac_ainah_r2.2majoracc.2017$GEO) +
  xlab("Emission caused by Energy Supply in tons") +
  ylab("Emission caused by Manufacturing in tons") +
  ggtitle ("Emission of Greenhous Gas equivalents in tons by the 2 major accounts of different European Countries the year 2017")

plot.2majoracc.2017.scatter.repel

#Still it is hard to spot the points for the countries.

#All in all it is a better idea to scale the greenhouse emissions of the countries for energy supply and
#manufacturing by country size. As a quantization population or GDP could be used. Here a scaling is done
#via population.

demo_gind <- read.csv("./Data/Population change - Demographic balance and crude rates at national level (demo_gind)/demo_gind_1_Data.csv")

demo_gind$Value <- gsub(",","",demo_gind$Value) 

demo_gind$Value <- as.double(demo_gind$Value) #NAs introduced by coercion!


demo_gind.2017 <- subset(demo_gind, (TIME == 2017))

#Rename one of the countries
levels(demo_gind.2017$GEO) [levels(demo_gind.2017$GEO) == "Germany (until 1990 former territory of the FRG)"] <- "Germany"


env_ac_ainah_r2.2majoracc.2017.pop <- inner_join(env_ac_ainah_r2.2majoracc.2017, demo_gind.2017)

#inner join seem to have worked. But it is not tidy and more important 5 values are missing.

#Add two new columns with energy ghg / population and manufacturing / population

env_ac_ainah_r2.2majoracc.2017.pop$GHG_by_energy_supply_per_capita <- env_ac_ainah_r2.2majoracc.2017.pop [,2] / env_ac_ainah_r2.2majoracc.2017.pop [,6]

env_ac_ainah_r2.2majoracc.2017.pop$GHG_by_manufacturing_per_capita <- env_ac_ainah_r2.2majoracc.2017.pop [,3] / env_ac_ainah_r2.2majoracc.2017.pop [,6]

#There should be a line with the slope

slope = env_ac_ainah_r2.total.2017$Value [env_ac_ainah_r2.total.2017$NACE_R2 == "Electricity, gas, steam and air conditioning supply"] /env_ac_ainah_r2.total.2017$Value [env_ac_ainah_r2.total.2017$NACE_R2 == "Manufacturing"]
df <- data.frame(x1 = 0, x2 = 2.6, y1 = 0, y2 = 2.6*slope)



#now plot

plot.2majoracc.2017.scatter.pop <- ggplot(env_ac_ainah_r2.2majoracc.2017, aes(x = env_ac_ainah_r2.2majoracc.2017.pop[,8] , y = env_ac_ainah_r2.2majoracc.2017.pop[,9])) +
  geom_point(size=2, shape=23) +
  geom_text_repel(label=env_ac_ainah_r2.2majoracc.2017.pop$GEO) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, colour = "segment"), data = df) +  
  xlab("Emission caused by Energy Supply in tons per capita") +
  ylab("Emission caused by Manufacturing in tons per capita") +
  ggtitle ("Emission of Greenhous Gas equivalents in tons per capita by the 2 major accounts of different European Countries the year 2017")

plot.2majoracc.2017.scatter.pop

#-------------------------------------------------------

#You could use this data to make a plot of the European map with the levels of emission by energy supply for example

#-------------------------------------------------------

#But this are just 2 accounts and there are other importants ones, e.g. agriculture.
#One could use the shiny package to compare countries with another and show the barplot
#of the accounts from above.

#--------------------------------------------------------

#Also you could try to reduce the dimensionality by principal component analysis.Then similar
#countries could be clustered on the principal component 1 and 2 plot


#Take the data from 2017 for all countries and accounts and change data from long to wide:

env_ac_ainah_r2.2017.wide <- dcast(env_ac_ainah_r2.2017, GEO~NACE_R2, value.var="Value")

env_ac_ainah_r2.2017.wide.pca <- prcomp(env_ac_ainah_r2.2017.wide[,c(2:22)], center = TRUE,scale. = TRUE)

summary(env_ac_ainah_r2.2017.wide.pca)

data.frame(env_ac_ainah_r2.2017.wide.pca$x)

#Make a skree plot
fviz_eig(env_ac_ainah_r2.2017.wide.pca)

#So the different accounts of emission can be summarized quite well.
#On the other hand this means, that a 2 dimnesional plot of the countries
#on the first two principal components might not be meaningful.
#It could make more sense to plot the countries on just on dimension.

#Nevertheless visualize the first two principle components, where the data points are depicted.

fviz_pca_ind(env_ac_ainah_r2.2017.wide.pca, repel = TRUE)

#It seems to be the case that a relabeling is not possible.

#Check by hand
#10: France
#11: Germany
#23: Poland
#33: UK

#The outliers are mostly big countries. 
#Scaling by population was omitted here, but it should not have greater effects.
#Other reasons, why these countries are outliers?


#That was quite the goal of the pca.

fviz_pca_var(env_ac_ainah_r2.2017.wide.pca)


#-----------------------------


#Discussion

#How were the shown plots?
#Have the plots delivered a lot of insight? Which plots haven't?
#What would you have plotted?



