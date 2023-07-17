#package preparation
library(readxl)
library(Matrix)
library(data.table)
library(ggplot2)
library(Rcpp)
library(Cairo)
library(maptools)
library(rgeos)
library(ggmap)
library(scales)
library(RColorBrewer)
library(rgdal)
library(sf)
library(viridis)
library(ggsci)
library(wesanderson)
library(gridExtra)
library(pheatmap)
library(networkD3)
library(igraph)
library(R.matlab)
library(dplyr)
library(assertthat)
library(purrr)
library(ggraph)
library(tidygraph)
library(htmlwidgets)
library(parallel)
library(doParallel)
library(Rpdb)
library(microbenchmark)
library(leontief)
library(data.table)
library(tibble)
library(Rcpp)
sourceCpp("./test.cpp")

#Number of regions(countries)
RR = 4
#Number of sectors(original sectors)
NN = 3
#Total number of nodes
RRNN = RR*NN
#Numbers of time steps, half month in one step
UU = 1 
TT = 24
#Input-output matrix of output values, annual values
zz.s<-as.matrix(read.csv("./InputOutput_Z.csv",header = T,row.names=1))
#Input-output matrix of output values
ff.s<-as.matrix(read.csv("./InputOutput_f.csv",header = T,row.names=1))
#Theoretical output values of each node
xx.s<-as.matrix(read.csv("./InputOutput_X.csv",header = T))
va.s<-xx.s-(colSums(zz.s))
#Mean values for each step
zz.s<-zz.s/24
ff.s<-ff.s/24
xx.s<-xx.s/24
va.s<-va.s/24

#Sector index of IO matrix 
mat.index<-as.matrix(read.csv("./mat_index.csv",header = F))
no.input = apply(mat.index,2,max)

#Functions of calculating theoretical output by equilibrium
merge.mat = function(mat,mat.index)
{
  RRNN = nrow(mat)
  merge.mat.a = function(x,temp)
  {
    for(i in 1:RRNN)
    {
      temp[mat.index[i,x]] = temp[mat.index[i,x]] + mat[i,x]
    }
    return(temp)
  }
  
  cl=makeCluster(detectCores() - 3)
  registerDoParallel(cl)
  
  temp = rep(0,RRNN)
  
  result = foreach(x = 1:RRNN,.combine = cbind) %dopar%  merge.mat.a(x,temp)
  
  stopImplicitCluster()
  stopCluster(cl)
  return(result)
}

dis.mat = function(mat,mat.index,mat.ezz)
{
  RRNN = nrow(mat)
  dis.mat.a = function(x,temp)
  {
    for(i in 1:RRNN)
    {
      temp[i] = mat[mat.index[i,x],x]*mat.ezz[i,x]
    }
    return(temp)
  }
  
  cl=makeCluster(detectCores() - 3)
  registerDoParallel(cl)
  
  temp = rep(0,RRNN)
  
  result = foreach(x = 1:RRNN,.combine = cbind) %dopar%  dis.mat.a(x,temp)
  
  stopImplicitCluster()
  stopCluster(cl)
  return(result)
}

#Time, and pilot run the experiment
StartTime = Sys.time()

zz.s.c = merge.mat(zz.s,mat.index)#theoretical output by equilibrium
mat.stock = 2*zz.s.c#assumed stock, 2 steps(1 month) here
mat.stock.obj = mat.stock + zz.s.c#total stock
mat.order = cbind(zz.s,ff.s)#orders for each step
mat.ezc = sweep(zz.s.c,2,xx.s,FUN = "/")#coefficient a
mat.ezz = matrix(nrow = RRNN,ncol = RRNN)#coefficient b
mat.ezz.t = matrix(nrow = RRNN,ncol = RRNN)#coefficient b
zz.s.t<-1/zz.s

for(i in 1:RRNN)
{ 
  for(j in 1:RRNN)
  {
    mat.ezz.t[i,j] =eigenMapMatMult(zz.s.t[i,j], zz.s.c[mat.index[i,j],j])
  }
}
mat.ezz <-1/mat.ezz.t

#fraction of output reductions for the shocked sectors
VA_TT = array(1,dim = c(NN,RR,TT))
si<-as.matrix(read.csv("./si.csv"))
coff<-read.csv("./cof.csv")

for (i in 1:TT){
  si.temp1<-t(si[i,])
  si.temp2<-si.temp1[rep(seq_len(nrow(si.temp1)), each = NN), ]
  si.TT<-si.temp2
  si.TT<-(100-si.TT)/100
  si.TT[,"C1"]<-coff$C1
  si.TT[,"C2"]<-coff$C2
  VA_TT[,,i]<-si.TT
}
FF_TT = array(rep(ff.s,TT),dim = c(RRNN,RR,TT))

#Operate experiments in 24 steps
StartTime = Sys.time()
xx.TT = matrix(0,nrow = TT,ncol = RRNN)
for(t in 1:TT)##-----------calculate the matrix for 24 steps separately------------
{
  print(paste0("Step ",t," is in processing..."))
  
  va.t = matrix(VA_TT[,,t],nrow = 1)
  
  mat.order.sum = rowSums(mat.order)
  
  xx.t = matrix(0,nrow = 1,ncol = RRNN)
  xx.t.opt = matrix(0,nrow = 1,ncol = RRNN)
  for(i in 1:RRNN)
  {
    temp = mat.stock[1:no.input[i],i]/mat.ezc[1:no.input[i],i]; 
    #temp = subset(temp,as.logical(mat.key[1:no.input[i],i]))
    xx.t[i] = min(temp,va.t[i]*xx.s[i],mat.order.sum[i])##actual output
    xx.t.opt[i] = min(va.t[i]*xx.s[i],mat.order.sum[i])##maximum output
    #xx.t.opt[i] = va.t[i]*xx.s[i]
  }
  
  xx.t.dis = sweep(mat.order,1,mat.order.sum,FUN = "/")*t(xx.t[rep(1,RRNN+RR),])
  
  mat.stock.use = xx.t[rep(1,RRNN),]*mat.ezc  ##Used stock
  mat.stock.add = merge.mat(xx.t.dis[,1:RRNN],mat.index)   ##Increased stock
  
  mat.stock.use.opt = xx.t.opt[rep(1,RRNN),]*mat.ezc##Maximum potential inventory level that can be supported
  
  mat.stock = mat.stock - mat.stock.use + mat.stock.add  #Current stock
  
  mat.stock.gap = mat.stock.obj - mat.stock
  mat.stock.gap[which(mat.stock.gap<0)] = 0
  
  
  mat.order[,1:(RRNN)] = dis.mat(mat.stock.gap,mat.index,mat.ezz)
  mat.order.temp<- as.data.frame(mat.order[,1:(RRNN)])
  mat.order[,(RRNN+1):(RRNN+RR)] = FF_TT[,,t]  
  xx.TT[t,] = xx.t
}
#process end
EndTime <- Sys.time()
timerunning<-Sys.time()-StartTime
timerunning

#write result out
plot(rowSums(xx.TT))#Temporal changes of the sum of output
loss <- sum(xx.s) - rowSums(xx.TT)
loss#increasing economic losses in each month
for (i in 2:length(loss)) {
  loss[i] <- loss[i-1] + loss[i]
}
loss#accumulative economic losses by each month
write.csv(loss,"./loss_acc_steps.csv")
