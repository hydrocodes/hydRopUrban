# Suggested code lines for running hydRopUrban functions
# Change the input and output path and files C:/.../
# Edit input values

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
output <- "C:/.../output.txt"
dt <- 0.05 
rational(data,dt)

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
output <- "C:/.../output.txt"
dt <- 0.05 
D <- 2
rationalm(data,D,dt)

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
output <- "C:/.../output.txt"
dt <- 0.05 
rationalu(data,dt)

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
a <- 5
b <- -0.6
caquots(data,a,b)

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
a <- 5
b <- -0.6
caquotp(data,a,b)

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
inflow <- data$Discharge
Qo <- 1.5 #m3/s
Vo <- 2.1 #m/s
So <- 0.01 #slope
To <- 2 #m
L <- 2500    #m
m <- 1.31 # coefficient
dt <- 0.05    #hr
init <- 0    #m3/s
output <- "D:/.../output2.txt"
mcunge(inflow, Qo, To, Vo, L, m, dt, init)
