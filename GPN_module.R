#----------------instruction of the experiment----------------------------------
#The experiment code should cover 141 countries and 65 sectors while the sample code only covers 4 suppositional countries and 3 suppositional sectors
#Each step corresponds to half a month
#The cascade failures should take place between two nodes dependent on each other (called related nodes)
#A node failure could induce the related node failures in the upstream or downstream
#Cascade to upstream means reduction of orders of the broken node
#Cascade to downstream means reduction of supply of the broken node

#Package preparation
library(readxl)
library(tidyverse)

#Number of regions(countries)
RR = 4
#Number of sectors(original sectors)
NN = 3
#Total number of nodes
RRNN = RR*NN
#Numbers of time steps, half month in one step
UU = 1 
TT = 24

#Some functions of calculating the country ID or sector ID according to the node number
#small():put a node id in, return a sector ID(1-NN)
small <- function(x){
  ssnn <- x %% NN
  if(ssnn == 0){ssnn = NN}
  return(ssnn)
}
#country():put a node id in, return a country ID(1-RR)
country <- function(x){
  ceiling(x/NN)
}

#A name list of the couple of country i and sector j, C[i]S[j]
name <- vector(mode = "character", length = RRNN)
for(i in 1:RR){
  for(j in 1:NN){
    number <- (i-1)*NN + j 
    name[number] <- paste0("C", i, "S", j, sep = "")#Country i Sector j
  }
}


#Define the dependency between nodes by direct consumption coefficient matrix (A)
A <- as.matrix(read.csv("./InputOutput_A.csv", header = T,row.names = 1))

#Define the initial failure probability.
#When propagating downstream, the sum of the columns of the direct consumption coefficient matrix A is calculated, representing the total amount of all sector products consumed by each regional sector per unit of output.
#The higher the total amount, the higher the dependence on the upstream industry, and the greater the probability of being affected in the downstream transmission chain.
fail_pro_d <- colSums(A)
fail_pro_d <- (fail_pro_d-min(fail_pro_d))/(max(fail_pro_d)-min(fail_pro_d))

#When propagating upstream, the sum of the rows of the direct consumption coefficient matrix A is calculated to represent the external dependence on the various regional sectors.
#The higher the total amount, the higher the external dependence on it, and the more serious the impact of upstream propagation.
fail_pro_u <- rowSums(A)
fail_pro_u <- (fail_pro_u-min(fail_pro_u))/(max(fail_pro_u)-min(fail_pro_u))

#----------------Cascade--------------------------------------------------------
#Simulate 1000 events.
#Note that each event is a time sequence that contains 24 sequential sub-events that together form an implementation.
#---------------------------------
n_events <- 100#Number of simulation events
Threshold <- 0.5#Assumed threshold to trigger losses by the shocks, which means that only the serious shocks on a node can induce a production loss and can further cascade to other nodes.
n_Iteration <- 10000#Upper limit of iterations

value.x <- read.csv("InputOutput_X.csv", header = T)#Annual total output | Total input (basis)
value.x <- t(value.x)
coff <- 0.25#Assumed coefficient of product loss because the sectors would usually not shut down completely. The coefficient means the extent of impacts on the output.
value.x <- value.x*coff
colnames(value.x) <- name

loss <- read.csv("loss_acc_steps.csv", header = T)#Output loss at each step based on ARIO model
loss <- loss$x
#The loss value in each step generated by the ARIO module is used as the concatenation constraint. 
#When the cumulative loss value of the cascade reaches or exceeds the constraint value, it indicates that the cascade terminates and jumps out of the iterative loop.

#Record variables for the cascade process are generated in batches.
for (i in UU:TT) {
  assign(paste0("lab_ord_", i), {})#The sequential events are recorded here.
  assign(paste0("sma_sma_", i), {})#The propagation among sectors (NN*NN) is recorded here.
  assign(paste0("con_con_", i), {})#The propagation among countries (RR*RR) is recorded here.
}


#--------------Cascade Starts---------------------------------------------------
StartTime = Sys.time()

#Note that we iterate 24 steps (j) for each event i, traversing all events.
for (i in 1:n_events){
  strength_d <- strength_u <- A #dependency strength matrix, which is changing with the iterations
  prob_d <- fail_pro_d#Initial downstream initial failure probability, dim:RRNN
  prob_u <- fail_pro_u 
  in_nodes <- which((name == "C1S1")|(name == "C2S1"))
  
  lab_ord   <- in_nodes#Both upstream and downstream propagation records
  
  count <- sum(value.x[,in_nodes]) 
  in_nodes.new <- in_nodes
  
  sma_sma <- matrix(0, nrow = NN, ncol = NN)
  con_con <- matrix(0, nrow = RR, ncol = RR)
  
  ##-------------starting iteration---------------------------------------------
  for (j in UU:TT){
    print(paste0("event", i, " step",j,"."))
    while (count < loss[j]) {
      in_nodes <- in_nodes.new
      for(nd in seq_along(in_nodes)){
        #strength=A, consumption coefficient table, "d" means downstream, so the column of the new node (for upstream consumption) =0
        #The same goes for "u", upstream
        strength_d[, in_nodes.new] <- 0#rows are propagated to downstream and it is ensured that the affected points are no longer selected
        strength_u[in_nodes.new, ] <- 0
        
        #Check whether all associated nodes are damaged, that is, both the consumption and output are 0
        if((sum(strength_d[in_nodes[nd],])+sum(strength_u[,in_nodes[nd]]))==0){
          next
        }
        
        #These two judgments are added to prevent errors with denominators of 0.
        if(sum(strength_d[in_nodes[nd],])==0){
          affnode_d <- 0
          conprob_d <- 0
        } else {
          affnode_d <- sample(RRNN, size = 1, prob = strength_d[in_nodes[nd], ])#Randomly extract downstream shocking points by rows
          #Accumulate cascading failure probability
          conprob_d <- strength_d[in_nodes[nd],affnode_d]/sum(strength_d[in_nodes[nd],]) + prob_d[affnode_d]
        }
        if(sum(strength_u[,in_nodes[nd]])==0){
          affnode_u <- 0
          conprob_u <- 0
        } else{
          affnode_u <- sample(RRNN, size = 1, prob = strength_u[, in_nodes[nd]])#Randomly extract upstream shocking points by columns
          conprob_u <- strength_u[affnode_u,in_nodes[nd]]/sum(strength_u[,in_nodes[nd]]) + prob_u[affnode_u]
        }
        if(conprob_d >= conprob_u){
          if(conprob_d >= Threshold){
            lab_ord   <- c(lab_ord, affnode_d)
            
            #Update record variables
            in_nodes.new <- lab_ord#The updated impacted node
            count <- count + value.x[, affnode_d]#The new accumulative loss value
            
            sma_sma[small(in_nodes[nd]), small(affnode_d)] <- sma_sma[small(in_nodes[nd]), small(affnode_d)] +1
            con_con[country(in_nodes[nd]), country(affnode_d)] <- con_con[country(in_nodes[nd]), country(affnode_d)] + 1
          } else {
            #Failed to propagate the impacts to downstream. So increase the posterior probability of this node
            prob_d[affnode_d] <- conprob_d
          }
        } else {
          #The same processes of propagating the impacts to upstream
          if(conprob_u >= Threshold) {
            lab_ord   <- c(lab_ord,   affnode_u)
            
            #Update record variables
            in_nodes.new <- lab_ord
            count <- count + value.x[, affnode_u]

            sma_sma[small(in_nodes[nd]), small(affnode_u)] <- sma_sma[small(in_nodes[nd]), small(affnode_u)] +1
            con_con[country(in_nodes[nd]), country(affnode_u)] <- con_con[country(in_nodes[nd]), country(affnode_u)] + 1
          } else {
            prob_u[affnode_u] <- conprob_u
          }
        }
        if(count>=loss[j]){break} #The accumulative loss value has exceeded the loss constraint so the cascading process in this step has finished.
      }
    }
    print(paste0("step ",j," loss is ", count))
    #Add the results to the variables in turn
    lab_ord_temp <- get(paste0("lab_ord_",j))
    lab_ord_temp <- append(lab_ord_temp, list(lab_ord))
    assign(paste0("lab_ord_",j),lab_ord_temp)
    
    sma_sma_temp <- get(paste0("sma_sma_",j))
    sma_sma_temp <- append(sma_sma_temp, list(sma_sma))
    assign(paste0("sma_sma_",j),sma_sma_temp)
    
    con_con_temp <- get(paste0("con_con_",j))
    con_con_temp <- append(con_con_temp, list(con_con))
    assign(paste0("con_con_",j),con_con_temp)
  }
}
#--------------A complete one-year experiment completed-------------------------
Sys.time()-StartTime

#Write the results out
subfolder <- "Cascade Results"
if (!dir.exists(subfolder)){
  dir.create(subfolder)
}
for (h in UU:TT) {
  lab_ord_temp <- get(paste0("lab_ord_", h))
  M <- matrix(0, nrow = n_events, ncol = RRNN)
  for (i in 1:n_events) {
    len <- RRNN - length(lab_ord_temp[[i]])
    M[i, ] <- c(lab_ord_temp[[i]], rep(0, times = len))
  }
  filename <- paste0(subfolder,"/lab_ord_", h, ".csv")
  write.csv(M, filename, row.names = F)
  
  filename <- paste0(subfolder,"/sma_sma_", h, ".rds")
  data <- get(paste0("sma_sma_", h))
  write_rds(data, filename)
  
  filename <- paste0(subfolder,"/con_con_", h, ".rds")
  data <- get(paste0("con_con_", h))
  write_rds(data, filename)
}

