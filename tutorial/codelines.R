# Suggested code lines for running hydRopUrban functions
# Change the input and output path and files C:/.../
# Edit input values

library(hydRopUrban)

#rational()
db_sbs <- read.csv('tutorial/database1.csv')
qd_fl <- rational(crunoff = db_sbs$c, intensity = db_sbs$i, area =  db_sbs$A,
                  time.con = db_sbs$Tc, delta.time = 0.05, path = 'C:/')

qd_fl <- rational(crunoff = db_sbs$c, intensity = db_sbs$i, area = db_sbs$A,
                  time.con = db_sbs$Tc, delta.time = 0.05, duration =  2, method = 'modified', path = 'C:/')

qd_fl <- rational(crunoff = db_sbs$c, intensity = db_sbs$i, area = db_sbs$A,
                  time.con = db_sbs$Tc, delta.time = 0.02, method = 'universal')

#caquot()
db_sbs <- read.csv('tutorial/database2.csv')
caquot(crunoff = db_sbs$c, slope = db_sbs$S, area =  db_sbs$A, longitud = db_sbs$L,
       a = 5, b = -0.6, method = 'parallel', path = 'C:/')

#routing()
inflow <- qd_fl$Discharge

qr_mc <- routing(flow.in = inflow, flow.ref = 1.5, velocity = 2.1, slope = 0.01,
                 channel = 20, longitud =  2500, flow.area = 1.31, flow.init = 0,
                 delta.time = 0.05, method = 'mcunge', path = 'C:/')

qr_cv <- routing(flow.in = inflow, flow.init = 0, channel = 20, longitud = 2500,
                 slope = 0.0005, cmanning = 0.05, delta.time = 0.1, method = 'convex', path = 'C:/')

#pollutant
inflow <- qd_fl$Discharge
poll_imp <- pollutant(flow.in = inflow, area = 0.2023, solids = 2131.88,
                      kc = 0.1811, delta.time = 0.05, method = 'impervious')

int30 <- c(7.62,12.7,33.02,10.16,7.62,5.08)
poll_imp <- pollutant(intensity = int30, area = 0.0121, kt = 0.27,
                      longitud = 122, slope = 0.001, delta.time = 0.05,
                      coefficient=0.003, param = 1, method = 'pervious', path = 'C:/')

#idf
idf_df <- idf(precipitation =20, type="3", path = 'C:/')

#lagtime
sbs <- read.csv('tutorial/database_sbs.csv')

ltm_df <- lagtime(area = sbs$area, longitud = sbs$lon, slope = sbs$spc, altitudiff = sbs$dfa,
                  id = sbs$ID, load_plot = TRUE)
