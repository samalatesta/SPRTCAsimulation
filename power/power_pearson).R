library(RDS)
library(dplyr)
library(igraph)
library(ggplot2)

rm(list = ls())
options(scipen=999)

setwd("/projectnb/cbs/samalate/RDS/type1")

####Batch Job####

#Finding the task number for the run
#iTask <- as.numeric(Sys.getenv("SGE_TASK_ID"))

#Get sim parameters from arguments 
args <- commandArgs(trailingOnly = TRUE)

#arg1 
r1 <- as.numeric(args[1])

#arg2 
r2 <- as.numeric(args[2])

#arg3 
n <- as.numeric(args[3])

ef <- as.numeric(args[4])

label <- paste0("r1=", r1,", r2=",r2)

generate.data.norm <- function(number.of.seeds, sample.size, r1, r2, ef){
  #create id variable
  id <- 1:sample.size
  #vector of 0s, length is number of seeds
  #rec.id of 0 indicates seed in final data set
  rec.id <- rep(0, number.of.seeds)
  c <- length(rec.id)
  initial <- 1
  finish <- number.of.seeds
  while(length(rec.id) <= sample.size){
    #sample with replacement, 1:3 recruits per node
    a <- sample(1:3, c, replace = T)
    #for each node assign recruiter  
    b <- sample(rep(initial:finish, a))
    #number of new recruits
    c <- length(b)
    #add new rec.ids to rec.id vector
    rec.id <- c(rec.id, b)
    #update initial to be start of new wave
    initial <- max(b)+1
    #update finish to be end of new wave
    finish <- max(b)+c
  }
  #store rec.id variable
  rec.id <- rec.id[1:sample.size]
  #assign degree to each node, random sample 1:30
  degree <- sample(1:30, sample.size, replace=T)
  #final data frame
  #edge list and degree
  dat <- data.frame(id, rec.id, degree)
  
  #coerce to RDS data frame
  dat <-as.rds.data.frame(df=dat,id="id",
                          recruiter.id="rec.id",
                          network.size="degree")
  
  dat$wave <- get.wave(dat)
  dat$seed.id <- get.seed.id(dat)
  dat <- as.data.frame(dat) 
  dat$id <- as.numeric(dat$id)
  dat$rec.id <- as.numeric(dat$rec.id)
  dat$seed.id <- as.numeric(dat$seed.id)
  
  order.table <- data.frame(dat) %>% dplyr::arrange(seed.id, wave)
  
  #####GENERATE DICHOTMOUS ATTRIBUTE#####
  
  
  
  
  #####GENERATE NUMERIC ATTRIBUTE#####
  order.table$num1 <- rep(NA,sample.size)
  for(i in 1:dim(order.table)[1]){
    #generate randomized values in first order markov process 
    if(order.table$rec.id[i] == 0){order.table$num1[i] <- rnorm(1)}
    else{
      order.table$rec.num1.id[i] <- order.table$rec.id[i]
      order.table$rec.num1[i] <- order.table$num1[order.table$id == order.table$rec.num1.id[i]]
      value <- rnorm(1)
      order.table$num1[i] <-  order.table$rec.num1[i]*r1 + value*sqrt(1-r1^2)
    }
  }
  
  
  #####GENERATE NUMERIC ATTRIBUTE#####
  order.table$num2 <- rep(NA,sample.size)
  for(i in 1:dim(order.table)[1]){
    #generate randomized values in first order markov process 
    if(order.table$rec.id[i] == 0){order.table$num2[i] <- rnorm(1)}
    else{
      order.table$rec.num2.id[i] <- order.table$rec.id[i]
      order.table$rec.num2[i] <- order.table$num2[order.table$id == order.table$rec.num2.id[i]]
      value <- rnorm(1)
      order.table$num2[i] <-  order.table$rec.num2[i]*r2 + value*sqrt(1-r2^2)
    }
  }
  
  order.table$num2 <- order.table$num1*ef + order.table$num2*sqrt(1-(ef)^2)
  ######PRODUCE RDS DATA FRAME#####
  dat <-as.rds.data.frame(df=order.table,id="id",
                          recruiter.id="rec.id",
                          network.size="degree",
                          population.size=15000)
  return(dat)
  
}

#net <- generate.data.norm(5, 500, .1, .9)

#function to perform randomization test randomizing continuous variable
#inputs
#iter: number of iterations to use
#character.var: character variable name as string
#numeric.car: continuous variable name as string
#net: tree data to be used for test
randomize.norm <-function(iter, num.var1, num.var2, net){
  #tree data to test
  dat <- net
  mean_num1 <- mean(dat$num1)
  sd_num1 <- sqrt(var(dat$num1))
  dat$num1_std <- (dat$num1 - mean_num1)/sd_num1
  mean_num2 <- mean(dat$num2)
  sd_num2 <- sqrt(var(dat$num2))
  dat$num2_std <- (dat$num2 - mean_num2)/sd_num2
  #run t-test on original tree data and save p-value and test statistic
  obs.pval <- cor.test(dat$num1_std, dat$num2_std)$p.value
  obs.res <- cor.test(dat$num1_std, dat$num2_std)$statistic
  mean_numeric.var <- mean(dat$num1)
  sd_numeric.var <- sqrt(var(dat$num1))
  #get wave and seed for each node
  dat$wave <- get.wave(net)
  dat$seed.id <- get.seed.id(net)
  
  #initialize empty vector to store results
  permuted.res <- rep(NA, iter)
  dat$rec.id2 <- dat$rec.id
  rec <- dat %>% dplyr::select(id, num1_std)
  rec$rec.id2 = rec$id
  dat2 <- dplyr::left_join(dat, rec, by = "rec.id2") %>% dplyr::filter(is.na(num1_std.y)==F)
  r = cor(dat2$num1_std.x, dat2$num1_std.y)
  
  #create new df that will store permuted results
  permute.table <- dat
  #initialize permuted variable
  permute.table$permute.num1 <- permute.table$num1_std
  #save values for seeds and set the rest to missing
  permute.table$permute.num1[permute.table$rec.id != 0] <- NA
  #create new df that is ordered by seed and wave so we permute in the correct order
  order.permute.table <- permute.table %>% dplyr::arrange(seed.id, wave)
  
  for(j in 1:iter){
    for(i in 1:dim(order.permute.table)[1]){
      #generate randomized values in first order markov process 
      if(order.permute.table$rec.id[i] != 0){
        rec.id <- order.permute.table$rec.id[i]
        rec.num1 <- order.permute.table$permute.num1[order.permute.table$id == rec.id]
        value <- rnorm(1)
        order.permute.table$permute.num1[i] <-  rec.num1*r + value*sqrt(1-r^2)
      }
    }
    permuted.res[j] <- cor.test(order.permute.table$permute.num1, order.permute.table$num2)$statistic
    
  }
  
  
  b <- sum(ifelse(abs(permuted.res) >= abs(obs.res), 1, 0))
  p.value <- b/length(permuted.res)
  p.value.mc <- (b+1)/(length(permuted.res)+1)
  return(list(obs.pval, obs.res,p.value.mc, permuted.res,r,  mean_numeric.var, sd_numeric.var))
}

#randomize.norm(500, "num1", "num2", net)

sim <- function(n, iter, number.of.seeds, sample.size, r1,r2, label, ef){
  #create empty vectors to save results
  random.stat <-rep(NA, n)
  random.pval <- rep(NA, n)
  obs.pval <- rep(NA, n)
  obs.stat <- rep(NA, n)
  label <- label
  mean_numeric <- rep(NA, n)
  sd_numeric <- rep(NA, n)
  
  for(i in 1:n){
    #generate tree data
    net <- generate.data.norm(number.of.seeds,sample.size, r1,r2, ef)
    #run test
    res <- randomize.norm(iter  , "num1", "num2", net)
    #save result
    suppressWarnings(random.stat[i] <- res[[4]])
    random.pval[i] <- res[[3]]
    obs.pval[i] <- res[[1]]
    obs.stat[i] <- res[[2]]
    mean_numeric[i] <- res[[6]]
    sd_numeric[i] <- res[[7]]
  }
  #return data frame of results
  return(data.frame(random.stat, random.pval, obs.pval,obs.stat, label, mean_numeric, sd_numeric, ef))
}



set.seed(30 + r1 + r2 + ef)

#simulations to run
case1 <- sim(n,1000, 5, 500, r1,r2,label, ef)

saveRDS(case1, paste0("/projectnb/cbs/samalate/RDS/power/results/power_pearson_",r1,"_", r2, "_", n, "_", ef, ".rds"))


