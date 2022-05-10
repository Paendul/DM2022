setwd(dir="D:/DM") # SET PATH TO YOUR OWN GRAPHS DIRECTORY

library(Metrics)
library(ggplot2)
library(tidyverse)

data = read.csv(file="training_set_VU_DM.csv",header=TRUE,row.names=NULL)
nrow(data) #4.958.347

'''
test <- data[5]
t2 = na.omit(test)
t3<-subset(test, visitor_hist_starrating!="NULL") # 251.866
'''

plot(data[1],data[5])

# count number of searches
nb_srch = unique(data[1])
nrow(nb_srch) # 199.795

# (mean) number of propositions per search
clicks_per_srch <- table(data['srch_id'])
head(clicks_per_srch)
#cps <- t(clicks_per_srch)
plot(clicks_per_srch)
mean(clicks_per_srch) #24,81717
min(clicks_per_srch) # 5
max(clicks_per_srch) # 38
median(clicks_per_srch) # 29

# number of visits per country
nb_vis_loc <- select(data, c('srch_id', 'visitor_location_country_id'))
nb_vis_loc <- nb_vis_loc[!duplicated(nb_vis_loc$srch_id),]
nb_vis_locT <- table(nb_vis_loc['visitor_location_country_id'])
nb_vis_loc_Unique <- unique(nb_vis_loc[2])
nrow(nb_vis_loc_Unique) # 210 countries
nb_vis_locF = nb_vis_loc_Unique
nb_vis_locF[2] = nb_vis_locT
mean(nb_vis_locT) # 954,4048
min(nb_vis_locT) # 1
max(nb_vis_locT) # 114.186
median(nb_vis_locT) # 17
#######
nb_vis_ordered <- nb_vis_locF[order(nb_vis_locF$V2),]
plot(nb_vis_ordered)
head(nb_vis_ordered)
nb_vis_ordered
rownames(nb_vis_ordered) <- NULL


par(mfrow=c(1,3))
par(mfrow=c(1,1))
# star ratings
hist(data$prop_starrating)
# promotion
promo <- table(data['promotion_flag']) # 3889229 1069118 (21.57%)
brand <- table(data['prop_brand_bool']) # 1811287 3147060 (63.47%)
plot(data$price_usd)
pos <- table(data['position'])
plot(pos)

# number of clicks per search


######## dataset with only chosens ########
data_chosen <- data[data$booking_bool==1,]
hist(data_chosen$prop_starrating)
compl_promo <- table(data_chosen['promotion_flag']) # 96443 41947 (30.37%)
compl_brand <- table(data_chosen['prop_brand_bool']) # 46625 91765 (66.3%)
compl_pos <- table(data_chosen['position'])
plot(compl_pos)


######## dataset with only clicked on ########
data_clicked = data[data$click_bool==1,]
hist(data_clicked$prop_starrating)
clicked_promo <- table(data_clicked['promotion_flag']) # 157410 64469 (29.05%)
clicked_brand <- table(data_clicked['prop_brand_bool']) # 80625 141254 (63.6%)
clicked_pos <- table(data_clicked['position'])
plot(clicked_pos)

######## numeric variables ######## 
