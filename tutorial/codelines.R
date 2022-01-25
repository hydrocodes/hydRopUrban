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

library(hydRopUrban)
data <- read.table(file.choose(), header=T)
inflow <- data$Discharge
A <- 0.2023 #km2
w <- 2131.88 #kg
k <- 0.1811 #1/mm
dt <- 0.05    #hr
output <- "D:/.../output3.txt"
pollutant(inflow, A, w, k, dt)

library(hydRopUrban)
int30 <- c(7.62,12.7,33.02,10.16,7.62,5.08)
A <- 0.0121 #km2
K <- 0.27 #tonnes/acre or tons/ha
L <- 122 #m
S <- 0.001 #m/m
output <- "D:/.../output3.txt"
pollutantp(int30, A=0.0121, K=0.27, L=122, S=0.001, C=0.003, Pf=1)
