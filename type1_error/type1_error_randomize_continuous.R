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
A <- as.numeric(args[1])

#arg2 
B <- as.numeric(args[2])

#arg3 
r <- as.numeric(args[3])


n <- as.numeric(args[4])

label <- paste0("AA=", A,", BB=",B, ", r=",r )

#generate RDS data with two nodal variables (one continuous and one dichotomous)
#continuous variable is normally distributed, each recruiter can recruit up to 3 recruitees

generate.data.norm <- function(number.of.seeds, sample.size, AA, BB, r, ef){
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
  
  #save wave for each node
  dat$wave <- get.wave(dat)
  #save seed for each node
  dat$seed.id <- get.seed.id(dat)
  #convert to data frame
  dat <- as.data.frame(dat) 
  
  #make ids numeric
  dat$id <- as.numeric(dat$id)
  dat$rec.id <- as.numeric(dat$rec.id)
  dat$seed.id <- as.numeric(dat$seed.id)
  
  #sort data frame by seed and wave 
  order.table <- data.frame(dat) %>% dplyr::arrange(seed.id, wave)
  
  #####GENERATE DICHOTMOUS ATTRIBUTE#####
  
  #matrix of recruitment probabilities
  #AA and BB are same group probabilities
  myprob.char1 <- matrix(c(AA, 1-AA, 1-BB, BB),nrow=2)
  order.table$char1 <- rep(NA,sample.size)
  order.table$rec.char1.id <- rep(NA, sample.size)

  for(i in 1:sample.size){
    #generate randomized values in first order markov process 
    if(order.table$rec.id[i] == 0){order.table$char1[i] <- sample(1:2,1)}
    
    else{
      order.table$rec.char1.id[i] <- order.table$rec.id[i]
      order.table$rec.char1[i] <- order.table$char1[order.table$id == order.table$rec.id[i]]
      value <- rmultinom(1, 1, prob = myprob.char1[,order.table$rec.char1[i]])==1
      order.table$char1[i] <- which(value)
    }
  }
  
  order.table$char1 <- factor(order.table$char1, levels = c(1,2), labels = c("A", "B"))
  
  #####GENERATE NUMERIC ATTRIBUTE#####
  
  order.table$num1 <- rep(NA,sample.size)
  
  for(i in 1:dim(order.table)[1]){
    #generate randomized values in first order markov process 
    if(order.table$rec.id[i] == 0){order.table$num1[i] <- rnorm(1)}
    else{
      order.table$rec.num1.id[i] <- order.table$rec.id[i]
      order.table$rec.num1[i] <- order.table$num1[order.table$id == order.table$rec.num1.id[i]]
      value <- rnorm(1)
      order.table$num1[i] <-  order.table$rec.num1[i]*r + value*sqrt(1-r^2)
    }
  }
  
  #shift mean by EF
  #order.table$num1[order.table$char1=="A"] <- order.table$num1+ef
  #order.table$num1 <- ifelse(order.table$char1=="A", order.table$num1 + ef, order.table$num1)
  ######PRODUCE RDS DATA FRAME#####
  dat <-as.rds.data.frame(df=order.table,id="id",
                          recruiter.id="rec.id",
                          network.size="degree",
                          population.size=15000)
  return(dat)
  
}


#function to perform randomization test randomizing continuous variable
#inputs
#iter: number of iterations to use
#character.var: character variable name as string
#numeric.car: continuous variable name as string
#net: tree data to be used for test
randomize.norm <-function(iter, character.var, numeric.var, net){
  #tree data to test
  dat <- net
  #standardize data
  dat$numeric.var <- dat[,numeric.var]
  dat$character.var <- dat[,character.var]
  mean_numeric.var <- mean(dat$numeric.var)
  sd_numeric.var <- sqrt(var(dat$numeric.var))
  dat$numeric.var_std <- (dat$numeric.var - mean_numeric.var)/sd_numeric.var
  #run t-test on original tree data and save p-value and test statistic
  obs.pval <- t.test(dat$numeric.var_std  ~ dat$character.var, var.equal=T )$p.value
  obs.res <- t.test((dat$numeric.var_std ~ dat$character.var), var.equal=T)$statistic
  
  #get wave and seed for each node
  dat$wave <- get.wave(net)
  dat$seed.id <- get.seed.id(net)
  
  #initialize empty vector to store results
  permuted.res <- rep(NA, iter)
  dat$rec.id2 <- dat$rec.id
  rec <- dat %>% dplyr::select(id, numeric.var_std)
  rec$rec.id2 = rec$id
  dat2 <- dplyr::left_join(dat, rec, by = "rec.id2") %>% dplyr::filter(is.na(numeric.var_std.y)==F)
  r = cor(dat2$numeric.var_std.x, dat2$numeric.var_std.y)
  
  #create new df that will store permuted results
  permute.table <- dat
  #initialize permuted variable
  permute.table$permute.numeric.var <- permute.table$numeric.var_std
  #save values for seeds and set the rest to missing
  permute.table$permute.numeric.var[permute.table$rec.id != 0] <- NA
  #create new df that is ordered by seed and wave so we permute in the correct order
  order.permute.table <- permute.table %>% dplyr::arrange(seed.id, wave)
  
  for(j in 1:iter){
    for(i in 1:dim(order.permute.table)[1]){
      #generate randomized values in first order markov process 
      if(order.permute.table$rec.id[i] != 0){
        rec.id <- order.permute.table$rec.id[i]
        rec.numeric.var <- order.permute.table$permute.numeric.var[order.permute.table$id == rec.id]
        value <- rnorm(1)
        order.permute.table$permute.numeric.var[i] <-  rec.numeric.var*r + value*sqrt(1-r^2)
      }
    }
    permuted.res[j] <- t.test((order.permute.table$permute.numeric.var ~ order.permute.table$character.var), var.equal=T)$statistic
    
  }
  
  
  b <- sum(ifelse(abs(permuted.res) >= abs(obs.res), 1, 0))
  p.value <- b/length(permuted.res)
  p.value.mc <- (b+1)/(length(permuted.res)+1)
  
  return(list(obs.pval, obs.res,p.value.mc, permuted.res,r, mean_numeric.var, sd_numeric.var))
}

sim <- function(n, iter, number.of.seeds, sample.size, AA, BB, r, ef, label){
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
    net <- generate.data.norm(number.of.seeds,sample.size, AA, BB, r, ef)
    #run test
    res <- randomize.norm(iter  , "char1", "num1", net)
    #save result
    suppressWarnings(random.stat[i] <- res[[4]])
    random.pval[i] <- res[[3]]
    obs.pval[i] <- res[[1]]
    obs.stat[i] <- res[[2]]
    mean_numeric[i] <- res[[6]]
    sd_numeric[i] <- res[[7]]
  }
  #return data frame of results
  return(data.frame(random.stat, random.pval, obs.pval,obs.stat, label, mean_numeric, sd_numeric))
}

ef=0
set.seed(A + B + r)
#simulations to run
case1 <- sim(n,1000, 5, 500, A,B, r,ef,label)

saveRDS(case1, paste0("/projectnb/cbs/samalate/RDS/type1/results/type1_",A,"_", B, "_", r,"_", n, ".rds"))




