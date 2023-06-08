#' @title rational
#' @description Discharge time series by the rational, modified and universal method
#' @param crunoff A vector containing drainage units parameters: Runoff coefficient (c)
#' @param intensity A vector containing drainage units parameters: intensity in mm/hr (i)
#' @param area A vector containing drainage units parameters: area in km2 (A)
#' @param time.con A vector containing drainage units parameters: time of concentration in hr (Tc)
#' @param duration A numeric value: Event duration in hr
#' @param delta.time A numeric value: Time interval in hours
#' @param method A character: select method
#' @param path An option: path
#' @return Maximal discharges by drainage unit, hydrograph plots and output file with discharge series in m3/s
#' @examples rational(crunoff, intensity, area, time.con, duration, method, delta.time, path)
#' @export

rational <- function(crunoff = 0.95, intensity = 2.9, area = 0.57, time.con = 0.91, duration = 2,
                     method = 'standard', delta.time = 0.05, path = NA){
  Qp <- crunoff*intensity*area/3.6
  tc <- time.con
  dr <- duration
  dt <- delta.time
  if(method == 'standard'){
    qls <- list()
    for (h in 1:length(Qp)){
      qls[[h]] <- c(seq(0, Qp[h], dt*Qp[h]/tc[h]),
                    seq(Qp[h]-dt*Qp[h]/(tc[h]), 0, -dt*Qp[h]/(tc[h])))
    }
  }

  if(method == 'modified'){
    qls <- list()

    for (h in 1:length(Qp)){
      if (dr > tc[h]) {
        qls[[h]] <- c(seq(0, Qp[h], dt*Qp[h]/tc[h]),
                      rep(Qp[h], round((dr-tc[h]),1)/dt),
                      seq(Qp[h],0,-dt*Qp[h]/(1.5*tc[h])))
      } else{
        if (dr < tc[h]){
          qls[[h]] <- c(seq(0,Qp[h]*dr/tc[h],dt*Qp[h]/tc[h]),
                        seq((Qp[h]*dr/tc[h])-dt*Qp[h]/(1.5*tc[h]),0,-dt*Qp[h]/(1.5*tc[h])))
        }
        else {
          qls[[h]] <- c(seq(0,Qp[h],dt*Qp[h]/tc[h]),
                        seq(Qp[h]-dt*Qp[h]/(1.5*tc[h]),0,-dt*Qp[h]/(1.5*tc[h])))
        }
      }
    }
  }

  if(method == 'universal'){
    qls <- list()
    for (h in 1:length(Qp)){
      qls[[h]] <- c(seq(0,0.21*Qp[h],dt*0.21*Qp[h]/tc[h]),
                    seq(0.21*Qp[h]+dt*0.09*Qp[h]/tc[h],0.3*Qp[h],dt*0.09*Qp[h]/tc[h]),
                    seq(0.3*Qp[h]+dt*0.7*Qp[h]/tc[h],Qp[h],dt*0.7*Qp[h]/tc[h]),
                    seq(Qp[h]-dt*0.46*Qp[h]/tc[h],0.54*Qp[h],-dt*0.46*Qp[h]/tc[h]),
                    seq(0.54*Qp[h]-dt*0.15*Qp[h]/tc[h],0.39*Qp[h],-dt*0.15*Qp[h]/tc[h]),
                    seq(0.39*Qp[h]-dt*0.14*Qp[h]/tc[h],0.25*Qp[h],-dt*0.14*Qp[h]/tc[h]),
                    seq(0.25*Qp[h]-dt*0.07*Qp[h]/tc[h],0.18*Qp[h],-dt*0.07*Qp[h]/tc[h]),
                    seq(0.18*Qp[h]-dt*0.03*Qp[h]/tc[h],0.15*Qp[h],-dt*0.03*Qp[h]/tc[h]),
                    seq(0.15*Qp[h]-dt*0.01*Qp[h]/tc[h],0.14*Qp[h],-dt*0.01*Qp[h]/tc[h]),
                    seq(0.14*Qp[h]-dt*0.01*Qp[h]/tc[h],0.13*Qp[h],-dt*0.01*Qp[h]/tc[h]),
                    seq(0.13*Qp[h]-dt*0.13*Qp[h]/tc[h],0,-dt*0.13*Qp[h]/tc[h]))
    }
  }

  n_obs <- sapply(qls, length)
  mat <- t(sapply(qls, "[", i = seq_len(max(n_obs))))
  mat[is.na(mat)] <- 0

  hy_df <- data.frame(Hours = seq(from = as.POSIXct("2022-01-01 00:00:00 -05"),
                                  length.out = ncol(mat), by = paste0(dt*60, ' mins')),
                      Discharge = colSums(mat), t(mat))
  names(hy_df)[3:(2+length(Qp))] <- paste0('Discharge_s', 1:length(Qp))
  print(paste0("Qpeak = ", round(max(hy_df[,2]),2),
               " m3/s, Volume = ",round(sum(hy_df[,2])*dt*60*60,2)," m3"))
  plot(x = hy_df$Hours, y = hy_df$Discharge, type = 'l', lwd = 2, format = "%H:%M",
       ylab = 'Discharge [m3/s]', xlab = 'Time [hr]')
  coll <- rainbow(length(Qp))
  for (i in 1:length(Qp)){lines(x = hy_df$Hours,y = hy_df[,2+i],col = coll[i],type = 'l') }
  hy_df$Hours <- format(hy_df$Hours, format = "%H:%M:%S")
  hy_df[,-1] <- round(hy_df[,-1], 3)
  if(class(path) == 'character' & nchar(path) > 2){
    write.csv(hy_df, paste0(path, 'hydro_',method,'_out.csv'), row.names = F)
  }
  return(hy_df)
}

#' @title caquot
#' @description Maximal discharge by Caquot for serial and parallel drainage units
#' @param crunoff A vector containing drainage units parameters: Runoff coefficient (c)
#' @param slope A vector containing drainage units parameters: slope (S)
#' @param area A vector containing drainage units parameters: area in Ha (A)
#' @param longitud A vector containing drainage units parameters: travel length in m (L)
#' @param a A numeric value: Exponent from Montana IDF equation a*D^b
#' @param b A numeric value: Exponent from Montana IDF equation a*D^b
#' @param method A character: select method
#' @param path An option: path
#' @return Maximal discharge by drainage unit, total discharge and equivalent values
#' @examples caquot(crunoff, slope, area, longitud, a, b, method, path)
#' @export

caquot <- function(crunoff = 0.82, slope = 0.02, area = 200.21, longitud = 1300,
                   a = 5, b = -0.6, method = 'parallel', path = NA){
  k <- (0.5^b)*a/6.6
  u <- 1 + 0.287*b
  v <- -0.41*b
  w <- 0.95 + 0.507*b
  E <- longitud/sqrt(area*10000)
  m <- (E/2)^(0.7*b)
  Qp <- m*(k^(1/u))*(slope^(v/u))*(crunoff^(1/u))*(area^(w/u))
  A <- sum(area)
  c <- sum(crunoff*area)/sum(area)

  if(method == 'serial'){
    S <- (sum(longitud)/sum(longitud/sqrt(slope)))^2
    Es <- sum(longitud)/sqrt(sum(area*10000))
    df <- data.frame(c,S,A,L=max(longitud))
  }

  if(method == 'parallel'){
    S <- sum(slope*Qp)/sum(Qp)
    df1 <- data.frame(L=longitud,Qp)
    Es <- df1$L[df1$Qp==max(Qp)]/sqrt(sum(area*10000))
    df <- data.frame(c,S,A,L=sum(longitud))
  }

  Qps <- (Es/2)^(0.7*b)*(k^(1/u))*(S^(v/u))*(c^(1/u))*(A^(w/u))

  print(Qp)
  print(df)
  print(paste0("Qpeak = ", round(Qps, 3)," m3/s"))

  Qp <- data.frame(Qp = round(Qp, 3))
  if(class(path) == 'character' & nchar(path) > 2){
    write.csv(Qp, paste0(path, 'caquot_',method,'_out.csv'), row.names = F)
  }
  return(Qp)
}

#' @title routing
#' @description Hydrograph routing through an open urban drainage channel and through a wide open channel
#' @param flow.in A vector: Input hydrograph in m3/s
#' @param flow.ref A numeric value: Reference discharge in m3/s
#' @param flow.init A numeric value: initial discharge in m3/s
#' @param channel A numeric value: Reference open width in m
#' @param velocity A numeric value: Reference velocity in m/s
#' @param longitud A numeric value: Travel length in m
#' @param slope A numeric value: channel slope in m/m
#' @param flow.area A numeric value: Q-A relationship exponent of the hydraulic section Q=e*A^m
#' @param cmanning A numeric value: Manning's roughness coefficient
#' @param delta.time A numeric value: Time interval in hours
#' @param method A character: select method
#' @param path An option: path
#' @return Routed hydrograph and discharge time series
#' @examples routing(inflow, flow.ref, velocity, slope, channel, longitud, flow.area, delta.time, width, flow.init, cmanning, method)
#' @export

routing <- function(flow.in = inflow, flow.ref = 1.5, velocity = 2.1, slope = 0.01, channel = 2,
                    longitud = 2500, flow.area = 1.31, delta.time = 0.05, width = 20,
                    flow.init = 0, cmanning = 0.05, method = 'mcunge', path = NA){
  qi <- flow.in
  vl <- velocity
  sl <- slope
  nm <- cmanning
  lr <- longitud
  dt <- delta.time
  cw <- width
  To <- channel
  qa <- flow.area
  qr <- flow.ref
  qn <- flow.init
  if(method == 'mcunge'){
    x <- 0.5*(1-(qr/To)/(sl*qa*vl*lr))
    K <- lr/(qa*vl)
    c0 <- (-K*x + 0.5*dt*3600)/(K - K*x + 0.5*dt*3600)
    c1 <- (K*x + 0.5*dt*3600)/(K - K*x + 0.5*dt*3600)
    c2 <- (K - K*x - 0.5*dt*3600)/(K - K*x + 0.5*dt*3600)
    outflow <- rep(0, length(qi))
    outflow[1] <- qn
    for (i in 2:length(qi)){
      outflow[i] <- c0*qi[i] + c1*qi[i-1] + c2*outflow[i-1]
    }
    print(paste0("Qpeak_in = ", round(max(qi),3), " m3/s | Qpeak_out = ", round(max(outflow),3)," m3/s"))
  }

  if(method == 'convex'){
    Qpk <- max(qi)
    Q34 <- 3*Qpk/4
    y34 <- (Q34*nm)/(cw*sl^0.5)^0.6
    CX <- 1.5*y34^(2/3)*sl^0.5*dt*60*60/(nm*lr)
    outflow <- rep(0, length(qi))
    outflow[1] <- qn
    for (i in 2:length(qi)){
      outflow[i] <- CX*qi[i-1] + (1-CX)*outflow[i-1]
    }
    print(paste0("Qpeak_in = ", round(max(qi),3)," m3/s | Qpeak_out = ", round(max(outflow),3),"m3/s | Routing_coef = ",round(CX,2)))
  }

  #
  time <- seq(0,(length(qi)-1)*dt,dt)
  Qp <- data.frame(Hours=time, Inflow = qi, Outflow=outflow)

  plot(x = Qp$Hours,y = Qp$Inflow,type="l", lwd=2, xlab="Time [hr]", ylab="Discharge [m3/s]")
  lines(x = Qp$Hours,y = Qp$Outflow,type="l", lwd=2, lty=3, col = 'red')

  Qp[,-1] <- round(Qp[,-1], 2)
  if(class(path) == 'character' & nchar(path) > 2){
    write.csv(Qp, paste0(path, 'routing_',method,'_out.csv'), row.names = F)
  }
  return(Qp)
}

#' @title pollutant
#' @description Estimation of pollutant discharge and concentration by wash-off from impervious and pervious areas.
#' @param flow.in A vector: Input hydrograph in m3/s
#' @param area A numeric value: Unit drainage area in km²
#' @param solids A numeric value: Initial amount of solids accumulated prior to the rain in kg
#' @param kc A numeric value: Wash-off coefficient in 1/mm
#' @param intensity A vector: Input 30-min intensities in mm/hr
#' @param kt A numeric value: Soil erodibility factor in tonnes/acre or tons/ha
#' @param longitud A numeric value: Flow length in m
#' @param slope A numeric value: Slope in the flow direction in m/m
#' @param coefficient A numeric value: Cropping management factor
#' @param param A numeric value: Erosion control practice factor
#' @param delta.time A numeric value: Time interval in hours
#' @param method A character: select method
#' @param path An option: path
#' @return Maximal pollutant discharge and minimal pollutant concentration, pollutegraphs and output file with time series in m3/s
#' @examples pollutant(flow.in, intensity, area, solids, kc, kt, delta.time, longitud, slope, coefficient, param, method, path)
#' @export

pollutant <- function(flow.in = inflow, intensity = c(7.62,12.7,33.02,10.16,7.62,5.08), area = 0.2023,
                      solids = 2131.88, kc = 0.1811, kt = 0.27,
                      delta.time = 0.05, longitud=122, slope = 0.001, coefficient=0.003, param = 1,
                      method = 'impervious', path = NA){
  dt <- delta.time
  lg <- longitud
  ip <- intensity
  if(method == 'impervious'){
    qi <- flow.in
    x <- seq(0,dt*(length(qi)-1),dt)
    r1 <- qi*1000*3600/(area*1000000)
    ra <- c()
    deltaV <- c()
    p1 <- c()
    p1[1] <- solids
    deltaP <- c()
    for (i in 1:length(qi)) {
      ra[i] <- (r1[i]+r1[i+1])/2
      ra[length(qi)] <- r1[length(qi)]/2
      deltaV[i] <- (qi[i]+qi[i+1])*dt*3600/2
      deltaV[length(qi)] <- qi[length(qi)]*dt*3600/2
      p1[i+1] <- p1[i]*exp(-kc*ra[i]*dt)
      deltaP[i] <- p1[i]-p1[i+1]
    }
    C <- deltaP*1000/deltaV
    W <- deltaP/dt

    par(mfrow =c(1,2))

    layout(matrix(c(1,2), nrow = 1), widths = c(1, 1))
    par(mar=c(4,4,2,4))
    plot(x,W, type="l", lwd=2, xlab=" ", ylab=" ", col="black")
    axis(side=2, col="black")
    mtext("Pollutant discharge [kg/hr]",side=2, col="black", line=2.5, font=2)
    mtext("Time [hr]", side=1, line=2)
    par(new=TRUE)
    plot(x,inflow, type="l", axes=F, lwd=1, lty=3, xlab=" ", ylab=" ", col = 'red')
    axis(4, col="black" )
    mtext("Discharge (m3/s)", side=4, line=2)

    plot(x,C, type="l", lwd=2, xlab=" ", ylab=" ", col="black")
    axis(side=2, col="black")
    mtext("Pollutant concentration [mg/l]",side=2, col="black", line=2.5, font=2)
    mtext("Time [hr]", side=1, line=2)
    par(new=TRUE)
    plot(x,qi, type="l", axes=FALSE, lwd=1, lty=3, xlab=" ", ylab=" ", col = 'red')
    axis(4, col="black" )
    mtext("Discharge [m3/s]", side=4, line=2)

    df <- data.frame(Hours=x, Discharge=qi, Pollutant_discharge=W, Pollutant_concentration=C)

    print(paste0("Ppeak = ", round(max(W),3), " kg/hours | Cmin = ",
                 round(min(C, na.rm = T),3)," mg/l"))
    return(df)
  }

  if(method == 'pervious'){
    x <- seq(0,0.5*(length(ip)-1),0.5)
    E <- (9.16+3.31*log10(ip/25.4))*(ip/25.4)*0.5
    i30 <- max(ip/25.4)
    R <- E*i30
    Ls <- (lg*3.281)^0.5*(0.0076+0.53*slope+0.76*slope^2)
    deltaP <- (2000/2.205)*(area*247.105)*R*kt*Ls*coefficient*param
    W <- deltaP/0.5
    par(mfrow =c(1,1))
    plot(x,W, ylim = range(2*W), type="l", lwd=2, xlab=" ", ylab=" ", col="black")
    par(new = TRUE)
    plot(x,ip, ylim = rev(range(2*ip)), type="h", axes=FALSE, lwd=1, xlab=" ", ylab=" ", col="black")
    axis(side=4)
    mtext("Pollutant discharge [kg/hr]",side=2, col="black", line=2.5, font=2)
    mtext("Intensity [mm/hr]",side=4, col="black", line=2.5, font=1)
    mtext("Time [hr]", side=1, line=2)
    par(mfrow =c(1,1))

    df <- data.frame(Hours=x, Pollutant_discharge=W)

    print(paste0("Ppeak = ", round(max(W),3), " kg/hr | Total load  = ",
                 round(sum(W)*0.5,3)," kg"))

    return(df)
  }

  if(class(path) == 'character' & nchar(path) > 2){
    write.csv(df, paste0(path, 'pollutant_',method,'_out.csv'), row.names = F)
  }
}

#' @title idf
#' @description Estimation of Intensity-Duration-Frequency adjustment from SCS storms
#' @param precipitation A numeric value: Input design precipitation of 24hr in mm
#' @param type An option: SCS storm type option (1,1a,2,3)
#' @param path An option: path
#' @return Montana Intensity-Duration-Frequency equation and curve
#' @examples idf(precipitation, type, path)
#' @export

idf <- function(precipitation = 48, type = '1', path = NA){
  scs1 <- c(0,0.0175,0.035,0.0555,0.076,0.1005,
            0.125,0.156,0.194,0.254,0.515,0.624,
            0.682,0.727,0.767,0.7985,0.83,0.854,
            0.878,0.902,0.926,0.9445,0.963,0.9815,1)
  scs1a <- c(0,0.025,0.05,0.083,0.116,0.161,0.206,
             0.268,0.425,0.52,0.577,0.624,0.664,
             0.701,0.736,0.768,0.8,0.8265,0.853,
             0.8795,0.906,0.9295,0.953,0.9765,1)
  scs2 <- c(0,0.011,0.022,0.035,0.048,0.064,0.08,
            0.098,0.12,0.147,0.181,0.235,0.663,
            0.772,0.82,0.85,0.88,0.898,0.916,0.934,
            0.952,0.964,0.976,0.988,1)
  scs3 <- c(0,0.01,0.02,0.0315,0.043,0.0575,0.072,
            0.089,0.115,0.148,0.189,0.25,0.5,0.751,
            0.811,0.8485,0.886,0.90375,0.9215,0.93925,
            0.957,0.96775,0.9785,0.98925,1)
  if(type=="1") {
    pacm <- precipitation*scs1
    p <- c()
    int2 <- c()
    int3 <- c()
    int4 <- c()
    for (i in 1:24) {
      p[i] <- pacm[i+1]-pacm[i]
      int2[i] <- (p[i+2]+p[i+1])/2
      int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
      int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
    }
  }  else if(type=="1a") {
    pacm <- precipitation*scs1a
    p <- c()
    int2 <- c()
    int3 <- c()
    int4 <- c()
    for (i in 1:24) {
      p[i] <- pacm[i+1]-pacm[i]
      int2[i] <- (p[i+2]+p[i+1])/2
      int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
      int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
    }
  }  else if(type=="2"){
    pacm <- precipitation*scs2
    p <- c()
    int2 <- c()
    int3 <- c()
    int4 <- c()
    for (i in 1:24) {
      p[i] <- pacm[i+1]-pacm[i]
      int2[i] <- (p[i+2]+p[i+1])/2
      int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
      int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
    }
  }  else if(type=="3"){
    pacm <- precipitation*scs3
    p <- c()
    int2 <- c()
    int3 <- c()
    int4 <- c()
    for (i in 1:24) {
      p[i] <- pacm[i+1]-pacm[i]
      int2[i] <- (p[i+2]+p[i+1])/2
      int3[i] <- (p[i+3]+p[i+2]+p[i+1])/3
      int4[i] <- (p[i+4]+p[i+3]+p[i+2]+p[i+1])/4
    }
  }  else stop(sQuote(x), " not implemented")
  ps <- max(p[!is.na(p)])
  int2s <- suppressWarnings(max(int2[!is.na(int2)]))
  int3s <- suppressWarnings(max(int3[!is.na(int3)]))
  int4s <- suppressWarnings(max(int4[!is.na(int4)]))
  int24 <- precipitation/24
  int24max <- c(ps, int2s, int3s, int4s, int24)
  D <- c(1,2,3,4,24)
  model <- suppressWarnings(lm(log(int24max) ~ log(D)))
  a <- exp(model$coefficients[1])
  b <- model$coefficients[2]
  x <- c(1:24)
  y <- a*x^b
  plot(x,y,type="l", lwd=2, xlab="Duration [hr]", ylab="Intensity [mm/hr]")
  print(sprintf("Intensity = a*Duration^b: a = %f, b = %f", a, b))
  file_out <- data.frame(Duration = x, Intensity = y)
  return(file_out)

  if(class(path) == 'character' & nchar(path) > 2){
    write.csv(file_out, paste0(path, 'idf_',type,'_out.csv'), row.names = F)
  }
}

#' @title lagtime
#' @description Estimation of time concentration and lag time
#' @param longitude A numeric value: Input design longitude in km
#' @param area A numeric value: Input design area in km2
#' @param slope A numeric value: Input design slope in percentage
#' @param altitudiff A numeric value: Input design attitudinal difference in m
#' @param path An option: path
#' @param load_plot An option: Generate box plots and view result
#' @return table time concentration and lag-time
#' @examples lagtime(longitude, area, slope, altitudiff, path)
#' @export

lagtime <- function(longitude = lon, area =area, slope = spc,
                    altitudiff = alt, id = NA, path =NA, load_plot = F){
  if(class(id) != 'character'){
    id <- 1:length(longitude)
  }
  tab_tm <- list()
  for (i in 1:length(longitude)) {
    L <- longitude[i]
    A <- area[i]
    S <- slope[i]
    H <- altitudiff[i]
    #tc
    tc_brw <- 0.2426*(L/((A^0.1)*(S/100)^0.2)) #Bransby_Williams

    tc_krp <- 0.4*0.0663*((L^2)/(S/100))^0.385 #Kirpich

    tc_krb <- ifelse(L<=0.1 & A<=0.04 & S <= 1, 1.4394/60*((L*1000)*n/(S/100)^0.5)^0.467, NA) #Kerby

    tc_jhc <- ifelse(A >= 65, 0.0543*(L/(S/100))^0.5, NA) #Johnstone_Cross

    tc_clf <- (60*((0.87075*(L)^3)/H)^0.385)/60 #California

    tc_clk <- 0.335* (A/(S/100)^0.5)^0.593 #Clark

    tc_gnd<- (4*((A)^0.5)+1.5*L)/(25.3*(((S/100)*L)^0.5))
    tc_gnd <- ifelse(L/3600 >= tc_gnd & tc_gnd >= (L/3600 +1.5), tc_gnd, NA) #Giandotti

    tc_psn <- ifelse(0.04<=L/(A^0.5)&L/(A^0.5)<=0.13, abs(0.108*((A*L)^(1/3)))/((S/100)^0.5), NA) #Passini

    tc_tmz <- 0.3*(L/((S/100)^0.25))^0.76 #Temez

    tc_prz <- L/(72*((H/L)^0.6)) #Perez

    tc_plg <- 0.76*((A)^0.38) #Pilgrim

    tc_usb <- ifelse(A >= 1, 2-0.5*log10(A), 2)*((0.87*L^2)/(1000*(S/100)))^0.385 #USBR

    tc_val <- 1.7694*((A)^0.325)*((L)^-0.096)*((S)^-0.290) #Valencia Zuluaga

    tc_vnt <- ifelse(0.04<= L/(A^0.5)& L/(A^0.5)<=0.14,  L/(A^0.5)*((A^0.5)/S), NA) #Ventura Heras

    #tl
    tc_ner <- 2.8*(L/(S/100*1000)^0.5)^0.47 #NERC

    tc_mim <- 0.43*A^0.418 #Mimikou

    tc_ner <- 0.000326*(1000*L/(S/100)^0.5)^0.79 #Watt-Chow

    tc_haz <- 0.2685*L^0.841 #Haktanir-Sezen

    #
    tab_tc <- data.frame(type = 'Tc',
                         method = c('Bransby_Williams', 'Kirpich', 'Kerby', 'Johnstone_Cross',
                                    'California', 'Clark', 'Giandotti', 'Passini',
                                    'Temez', 'Perez', 'Pilgrim', 'USBR',
                                    'Valencia_Zuluaga', 'Ventura_Heras'),
                         time = round(c(tc_brw, tc_krp, tc_krb, tc_jhc, tc_clf, tc_clk, tc_gnd,
                                        tc_psn, tc_tmz, tc_prz, tc_plg, tc_usb, tc_val, tc_vnt),6))

    tab_tl <- data.frame(type = 'Tl',
                         method = c('NERC', 'Mimikous','Watt-Chow', 'Haktanir-Sezen'),
                         time = round(c(tc_ner, tc_mim, tc_ner, tc_haz),6))

    tab_tm[[i]] <- cbind(data.frame(sbs = id[i]), rbind(tab_tc, tab_tl))
  }
  tab_tm <- do.call(rbind, tab_tm)
  tab_smr <- tapply(X = tab_tm$time, INDEX = list(tab_tm$sbs, tab_tm$type), FUN = median, na.rm = T)

  if(load_plot == T){
    boxplot(time ~ type + sbs, data = tab_tm, col = c('#00abea', '#82b000'),
            ylab= "Duration [hr]", show.names = F, xlab=NULL)
    axis(1, labels= id, at = seq(1.5, nrow(tab_smr)*2-0.5, by = 2))
    legend("topright", fill = c('#00abea', '#82b000'), inset = c(0,-0.15),
           legend = c("Tc", "Tl"), xpd= TRUE, bty="n", ncol = nrow(tab_smr))

    cat(paste0(id,' | Tc ', round(tab_smr[,1], 6), ' hr | ',
               'Tl ', round(tab_smr[,2], 6), ' hr'), sep="\n")
  }
  if(class(path) == 'character' & nchar(path) > 2){
    write.csv(tab_tm, paste0(path, 'timelag_out.csv'), row.names = F)
  }
  return(tab_tm)
}
