library(RDS)
library(dplyr)
library(igraph)
library(ggplot2)
library(purrr)
library(stringr)
rm(list = ls())
options(scipen=999)

setwd("/projectnb/cbs/samalate/RDS/type1/results")

#type 1 sim results

#ttest randomize continuous
files_data <-list.files(pattern = "type1_0.*")

dat1 <- files_data %>% map(readRDS)

dat1 <- data.frame(Reduce(rbind, dat1))

dat1$rand.var = "continuous"

#ttest randomize categorical
files_data <-list.files(pattern = "type1_cat_0*")

dat2 <- files_data %>% map(readRDS)

dat2 <- data.frame(Reduce(rbind, dat2))

dat2$rand.var = "categorical"

type1_ttest <- rbind(dat1, dat2)

write.csv(type1_ttest, paste0("../type1_ttest_", Sys.Date(), ".csv"))


#pearson
files_data <-list.files(pattern = "type1_pearson*")

dat3 <- files_data %>% map(readRDS)

dat3 <- data.frame(Reduce(rbind, dat3))

dat3$rand.var = "pearson"


write.csv(dat3, paste0("../type1_pearson_", Sys.Date(), ".csv"))



#pois
files_data <-list.files(pattern = "type1_pois*")

dat4 <- files_data %>% map(readRDS)

dat4 <- data.frame(Reduce(rbind, dat4))

write.csv(dat4, paste0("../type1_pois_", Sys.Date(), ".csv"))


setwd("/projectnb/cbs/samalate/RDS/power/results")
#power sim results 

#power ttest continuous

files_data <-list.files(pattern = "power_cont_0.*") 

dat5 <- files_data[str_detect(files_data,"_1_")==F] %>% map(readRDS)

dat5 <- data.frame(Reduce(rbind, dat5))

write.csv(dat5, paste0("../power_cont_", Sys.Date(), ".csv"))


#power ttest cat

files_data <-list.files(pattern = "power_cat*")

dat6 <- files_data %>% map(readRDS)

dat6 <- data.frame(Reduce(rbind, dat6))

write.csv(dat6, paste0("../power_cat_", Sys.Date(), ".csv"))


#power pearson

files_data <-list.files(pattern = "power_pearson*")

dat7 <- files_data %>% map(readRDS)

dat7 <- data.frame(Reduce(rbind, dat7))

write.csv(dat7, paste0("../power_pearson_", Sys.Date(), ".csv"))

#power pois

files_data <-list.files(pattern = "power_cont_pois*")

dat8 <- files_data %>% map(readRDS)

dat8 <- data.frame(Reduce(rbind, dat8))

write.csv(dat8, paste0("../power_pois_", Sys.Date(), ".csv"))


