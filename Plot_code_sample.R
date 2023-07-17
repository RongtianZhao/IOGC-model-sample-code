library(htmlwidgets)
library(webshot)
library(sankeyD3)
library(tidyverse)
library(ggalluvial)
library(reshape)
library(readxl)
library(dplyr)
library(scales)
library(ggsci)

RR <- 4
NN <- 3
RRNN <- RR*NN

#set folder, if the "Result_conversion.R" has been run, this part should be skipped.
# subfolder <- "Cascade Results"
# setwd(paste0("./",subfolder))

#First, read the cascading pass data (mydata.rds)
mydata <- read_rds("mydata_S.rds")

#Second, build the data frame of nodes
node_tb <- data.frame(matrix(nrow = NN,ncol = 2))
names(node_tb) <- c("name","id")
for (i in 1:NN){
  node_tb$name[i] <- paste0("S",i)
  node_tb$id[i] <- i
}

#Third, sankey plot
nodes <- rbind(node_tb,node_tb)
colorJS <- 'd3.scaleOrdinal(["#4E657D", "#FAD8C3", "#E0B88B",
              "#A28D70"]);'

#Aggregate annual data
mydata_new <- aggregate(data=mydata,freq~from+to,sum)
m_temp <- match(mydata_new$from,node_tb$id)-1
m_temp_2 <- match(mydata_new$to,node_tb$id)+NN-1
mydata_new$source <- m_temp
mydata_new$target <- m_temp_2

#plot test. If the propagation is too simple, there may be something abnormal
mydata_temp <- get(paste0("mydata_new"))
p <- sankeyNetwork(Links = mydata_temp, Nodes = nodes,
                   
                   Source = "source", Target = "target",
                   
                   Value = "freq", NodeID = "name",nodeWidth =18,units = 'cm',
                   
                   height=600,width=600,
                   
                   colourScale=colorJS,zoom = T,dragY = T,dragX = T,
                   
                   numberFormat=".0f",fontSize = 16,showNodeValues = F,
                   
                   LinkGroup = "from",NodeGroup = "id")
p

#----------------------------------------------------------------------
out_folder <- "./Output"
if (!dir.exists(out_folder)){
  dir.create(out_folder)
}

saveNetwork(p,"./Output/sankeyD3_S.html")
webshot("./Output/sankeyD3_S.html" , paste0("./Output/sankeyD3_Sector",".pdf"),cliprect = "viewport",zoom = 0.5,debug = F)

#The same processing of propagation among countries. If the nodes of countries are numerous, it is clearer to use a map to display the propagation using ggplot in R or ArcGIS software.
#First, read the cascading pass data (mydata.rds)
mydata <- read_rds("mydata_C.rds")

#Second, build the data frame of nodes
node_tb <- data.frame(matrix(nrow = RR,ncol = 2))
names(node_tb) <- c("name","id")
for (i in 1:RR){
  node_tb$name[i] <- paste0("C",i)
  node_tb$id[i] <- i
}

#Third, sankey plot
nodes <- rbind(node_tb,node_tb)

#Aggregate annual data
mydata_new <- aggregate(data=mydata,freq~from+to,sum)
m_temp <- match(mydata_new$from,node_tb$id)-1
m_temp_2 <- match(mydata_new$to,node_tb$id)+RR-1
mydata_new$source <- m_temp
mydata_new$target <- m_temp_2

#plot test.
mydata_temp <- get(paste0("mydata_new"))
p <- sankeyNetwork(Links = mydata_temp, Nodes = nodes,
                   
                   Source = "source", Target = "target",
                   
                   Value = "freq", NodeID = "name",nodeWidth =18,units = 'cm',
                   
                   height=600,width=600,
                   
                   colourScale=colorJS,zoom = T,dragY = T,dragX = T,
                   
                   numberFormat=".0f",fontSize = 16,showNodeValues = F,
                   
                   LinkGroup = "from",NodeGroup = "id")
p

saveNetwork(p,"./Output/sankeyD3_C.html")
webshot("./Output/sankeyD3_C.html" , paste0("./Output/sankeyD3_country",".pdf"),cliprect = "viewport",zoom = 0.5,debug = F)
