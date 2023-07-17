#This code is used for calculating the propagation data by the simulation results and the data can be used for plotting the propagations (Figure 2) in the software, such as ArcGIS.

#package preparation
library(readr)
library(readxl)
library(tidyverse)
library(reshape)

#Number of regions(countries)
RR = 4
#Number of sectors(original sectors)
NN = 3
#Total number of nodes
RRNN = RR*NN
#Numbers of time steps, half month in one step
UU = 1 
TT = 24

#Read the propagation record results from files
subfolder <- "Cascade Results"
setwd(paste0("./",subfolder))
for (i in 2:12) {
  path_bf <- paste0("con_con_", (i-1)*2, ".rds")#Note that two steps are equal to one month
  path_af <- paste0("con_con_", i*2, ".rds")
  assign(paste("con_bf_",i,sep = ""),read_rds(path_bf))
  assign(paste("con_af_",i,sep = ""),read_rds(path_af))
  }
#The records of propagation among countries in the first month
con_1 <- read_rds("con_con_2.rds")
length <- length(con_1)

#----------------Individual propagation matrix in each step---------------------
con_list <- list()

for (h in 2:12) {
  con_bf <- get(paste("con_bf_",h,sep=""))
  con_af <- get(paste("con_af_",h,sep=""))
  length <- length(con_af)
  C <- matrix(0, nrow = RR, ncol = RR)
  for (i in 1:RR) {
    for (j in 1:RR) {
      nn <- vector(mode = "numeric", length = length)
      for (k in 1:length) {
        nn[k] <- con_af[[k]][i,j] - con_bf[[k]][i,j]#Note that it is calculating the individual propagation frequency in each month, not the accumulative impacts
      }
      C[i,j] <- sum(nn)
    }
  }
  con_list[[h]] <- C
}

#The first month should be calculated separately.
con_1 <- read_rds("con_con_2.rds")
length <- length(con_1)
C <- matrix(0, nrow = RR, ncol = RR)
for (i in 1:RR) {
  for (j in 1:RR) {
    nn <- vector(mode = "numeric", length = length)
    for (k in 1:length) {
      nn[k] <- con_1[[k]][i,j]
    }
    C[i,j] <- sum(nn)
  }
}
con_list[[1]] <- C
#At this point we have a separate RR×RR propagation matrix for each month

#----------------Converse the data into the plot data(mydata)-------------------
mydata <- {}
for (i in 1:12) {
  C_dat <- data.frame(con_list[[i]])
  colnames(C_dat) <- 1:RR
  C_dat <- rownames_to_column(C_dat, var = "rowname")
  C_dat <- melt(C_dat, id.vars = "rowname", variable_name = "colname")
  colnames(C_dat) <- c("from", "to", "freq")
  C_dat$month <- i
  
  mydata <- rbind(mydata, C_dat)
}

mydata <- filter(mydata, freq > 0 & from!=to)
str(mydata)

#Write the plot data out.
filename <- "mydata_C.csv"
write.csv(mydata, filename, row.names = T)
filename <- "mydata_C.rds"
write_rds(mydata,filename)

#===============================================================================
#The code below is the same as the code above but the "country" is replaced by the "sector".

#Read files
for (i in 2:12) {
  path_bf <- paste0("sma_sma_", (i-1)*2, ".rds")
  path_af <- paste0("sma_sma_", i*2, ".rds")
  assign(paste("sma_bf_",i,sep = ""),read_rds(path_bf))
  assign(paste("sma_af_",i,sep = ""),read_rds(path_af))
}

sma_1 <- read_rds("sma_sma_2.rds")
length <- length(sma_1)

#----------------Individual propagation matrix in each step---------------------
sma_list <- list()

for (h in 2:12) {
  sma_bf <- get(paste("sma_bf_",h,sep=""))
  sma_af <- get(paste("sma_af_",h,sep=""))
  length <- length(sma_af)
  S <- matrix(0, nrow = NN, ncol = NN)
  for (i in 1:NN) {
    for (j in 1:NN) {
      nn <- vector(mode = "numeric", length = length)
      for (k in 1:length) {
        nn[k] <- sma_af[[k]][i,j] - sma_bf[[k]][i,j]#Note that it is calculating the individual propagation frequency in each month, not the accumulative impacts
      }
      S[i,j] <- sum(nn)
    }
  }
  sma_list[[h]] <- S
}

#The first month should be calculated separately.
sma_1 <- read_rds("sma_sma_2.rds")
length <- length(sma_1)
S <- matrix(0, nrow = NN, ncol = NN)
for (i in 1:NN) {
  for (j in 1:NN) {
    nn <- vector(mode = "numeric", length = length)
    for (k in 1:length) {
      nn[k] <- sma_1[[k]][i,j]
    }
    S[i,j] <- sum(nn)
  }
}
sma_list[[1]] <- S
#At this point we have a separate NN*NN propagation matrix for each month

#----------------Converse the data into the plot data(mydata2)------------------
mydata2 <- {}
for (i in 1:12) {
  S_dat <- data.frame(sma_list[[i]])
  colnames(S_dat) <- 1:NN
  S_dat <- rownames_to_column(S_dat, var = "rowname")
  S_dat <- melt(S_dat, id.vars = "rowname", variable_name = "colname")
  colnames(S_dat) <- c("from", "to", "freq")
  S_dat$month <- i
  
  mydata2 <- rbind(mydata2, S_dat)
}

mydata2 <- filter(mydata2, freq > 0 & from!=to)
str(mydata2)

#Write the plot data out.
filename <- "mydata_S.csv"
write.csv(mydata2, filename, row.names = T)
filename <- "mydata_S.rds"
write_rds(mydata2,filename)
