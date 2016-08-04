# PURPOSE -----------------------------------------------------------------

#  Dummy place holder to 00_getdata for deterioration model
#  This gets all the different initial states by building class, called the pdsp_data
#  Also prepares the transition matrix specification by reading another csv file
#  Aggregate transition matrix were developed by Bolun Wang and differ to the DfE


#AGGREGATE DATA FROM SQL QUERY
pdsp_data <- read.csv("built_initial_data.csv", header = TRUE)

#CREATE EMPTY values for transition matrix to overwrite later
#this longwinded approach facilitates user customisation later e.g. testing policies
nn <- 0
na <- 0
nb <- 0
nc <- 0
nd <- 0
ne <- 0
an <- 0
aa <- 0 
ab <- 0
ac <- 0
ad <- 0
ae <- 0
bn <- 0
ba <- 0
bb <- 0
bc <- 0
bd <- 0
be <- 0
cn <- 0
ca <- 0
cb <- 0
cc <- 0
cd <- 0
ce <- 0
dn <- 0
da <- 0
db <- 0
dc <- 0
dd <- 0
de <- 0
en <- 0
ea <- 0
eb <- 0
ec <- 0
ed <- 0
ee <- 0

tm_data <- read.csv("tm_bolun.csv", header = TRUE)  

