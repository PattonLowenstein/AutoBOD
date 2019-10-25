### AutoBOD Functions
# first is Monte Carlo Slope Estimation
# followed by the two air saturation and O2 concentration calculations


### AutoBOD Monte Carlo Slope Simulation

# General Idea: Sample two rows from a bottle dataframe a certain number of times (currently sampling )
# Put each of those into a df, then pull their times and their O2 concs
# So the total df might look like this

#   Row_1   Row_2   T_1   T_2   O2_1    O2_2    dT    dO2   dO2/dT

# then we throw out any dT less than an hour

# So the plan is to write a function to do this

Monte_Carlo_Slope_Sim <- function(Bottle){
  library(tidyverse)
  num_rows <- length(Bottle[,1])  # store the number of observations within the time range
  
  empty_slopes <- data.frame(Iteration = c(1:(num_rows%/%2)),  #                    Then set up an empty data frame with half the rows (with no remainder)
                             Row_1 = as.numeric(rep(NA, (num_rows%/%2))),  #       Set up an empty dataframe as detailed in the intro
                             Row_2 = as.numeric(rep(NA, (num_rows%/%2))),
                             Time_1 = as.numeric(rep(NA, (num_rows%/%2))),
                             Time_2 = as.numeric(rep(NA, (num_rows%/%2))),
                             O2_1 = as.numeric(rep(NA, (num_rows%/%2))),
                             O2_2 = as.numeric(rep(NA, (num_rows%/%2))),
                             dT = as.numeric(rep(NA, (num_rows%/%2))),
                             dO2 = as.numeric(rep(NA, (num_rows%/%2))),
                             dO2_dT_per_hour = as.numeric(rep(NA, (num_rows%/%2))))
  
  samples <- replicate(num_rows%/%2, sample(c(1:num_rows), 2, replace = FALSE)) # generate the rows in the bottle data to sample
  
  slopes <- empty_slopes %>% mutate(Row_1 = samples[1, Iteration], #              Now we're basically just pulling all of our info from the Bottle df
                                    Row_2 = samples[2, Iteration], #              And populating the new df with calcs, then we can filter it so 
                                    Time_1 = as.numeric(Bottle$unclassdatetime[Row_1]), #     the minimum dT is an hour
                                    Time_2 = as.numeric(Bottle$unclassdatetime[Row_2]),
                                    O2_1 = as.numeric(Bottle$o2conc_umol_L[Row_1]),
                                    O2_2 = as.numeric(Bottle$o2conc_umol_L[Row_2]),
                                    dT = Time_1 - Time_2,
                                    dO2 = O2_1 - O2_2,
                                    dO2_dT_per_hour = (dO2/dT)*3600)
  
  slopes_with_ok_range <- slopes %>% filter(abs(dT) > 3600) # only include slopes where the time difference was greater than an hour (in seconds)
  
  slope_statistics <- data.frame(Number_of_Pairs = length(slopes_with_ok_range$Iteration),
                                 Avg_dO2_umol_hour = mean(slopes_with_ok_range$dO2_dT_per_hour),
                                 Slope_SE = sd(slopes_with_ok_range$dO2_dT_per_hour)/sqrt(length(slopes_with_ok_range$Iteration)))
  
  return(slope_statistics)
}


calc_air_sat <- function(phase, IRBotT){
  cal0 <-  60.21 #B6
  cal100 <-  27.42 #B7
  airpres <-  981 #B8
  T0 <-  20 #E6
  T100 <-  20 #E7
  dF_k <-  -0.0847 #B12
  f1 <-  0.833 #B11
  dksv_k <-  0.000416 #B13
  m <-  34 #B14
  
  tan_psi0_t100 <-  tan(((cal0+dF_k*(T100-T0)))*pi/180) #D11
  tan_psi100_t100  <-  tan(cal100*pi/180) #D13
  
  A <-  tan_psi100_t100/tan_psi0_t100*1/m*100^2 #F11
  B <-  tan_psi100_t100/tan_psi0_t100*100+tan_psi100_t100/tan_psi0_t100*100/m-f1*100/m-100+f1*100 #F12
  C <-  tan_psi100_t100/tan_psi0_t100-1 #F13
  
  ksv_t100 <-  (-B+sqrt(B^2-4*A*C))/(2*A) #H11
  return(as.numeric(-((tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))*(ksv_t100+(dksv_k*(IRBotT-T100)))+(tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-f1*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-(ksv_t100+(dksv_k*(IRBotT-T100)))+f1*(ksv_t100+(dksv_k*(IRBotT-T100))))+(sqrt(((((tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))*(ksv_t100+(dksv_k*(IRBotT-T100)))+(tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-f1*1/m*(ksv_t100+(dksv_k*(IRBotT-T100)))-(ksv_t100+(dksv_k*(IRBotT-T100)))+f1*(ksv_t100+(dksv_k*(IRBotT-T100))))^2))-4*((tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*((ksv_t100+(dksv_k*(IRBotT-T100)))^2))*((tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))-1))))/(2*((tan(phase*pi/180))/(tan((cal0+(dF_k*(IRBotT-T0)))*pi/180))*1/m*((ksv_t100+(dksv_k*(IRBotT-T100)))^2))))} #oxygen = airsat*20.9/100

calc_o2_conc <- function(airsat, IRBotT){
  
  cal0 <-  60.21 #B6
  cal100 <-  27.42 #B7
  airpres <-  981 #B8
  T0 <-  20 #E6
  T100 <-  20 #E7
  dF_k <-  -0.0847 #B12
  f1 <-  0.833 #B11
  dksv_k <-  0.000416 #B13
  m <-  34 #B14
  
  tan_psi0_t100 <-  tan(((cal0+dF_k*(T100-T0)))*pi/180) #D11
  tan_psi100_t100  <-  tan(cal100*pi/180) #D13
  
  A <-  tan_psi100_t100/tan_psi0_t100*1/m*100^2 #F11
  B <-  tan_psi100_t100/tan_psi0_t100*100+tan_psi100_t100/tan_psi0_t100*100/m-f1*100/m-100+f1*100 #F12
  C <-  tan_psi100_t100/tan_psi0_t100-1 #F13
  
  ksv_t100 <-  (-B+sqrt(B^2-4*A*C))/(2*A) #H11
  
  return(((airpres-exp(52.57-6690.9/(273.15+IRBotT)-4.681*log(273.15+IRBotT)))/1013)*
           airsat/100.*0.2095*(48.998-1.335*IRBotT+0.02755*IRBotT^2-
                                 0.000322*IRBotT^3+0.000001598*IRBotT^4)*32/22.414)
}

###########
## Testing it all out
###########
# 
# setwd("C:/Users/TSQ/Desktop/Daniel Lowenstein/AutoBOD/UCSD_Experiment")
# autoBOD1 <- read.csv("AE1916Resp200m_Chance_English.csv", sep = ',', header = FALSE)
# colnames(autoBOD1) <- c("amplitude", "phase", "airTemp", "oxygen", "error", "encoder", "bottle", "sample", "Date", "Time", "IRdetT", "IRBotT")
# autoBOD1$phase <- sapply(autoBOD1$phase, function(x) as.numeric(as.character(x)))
# autoBOD1$airTemp <- sapply(autoBOD1$airTemp, function(x) as.numeric(as.character(x)))
# autoBOD1 <- autoBOD1%>% mutate(new_phase = phase/100, 
#                                airsat = calc_air_sat(phase = new_phase, IRBotT = IRBotT), 
#                                o2conc = calc_o2_conc(airsat = airsat, IRBotT = IRBotT), 
#                                o2conc_umol_L = o2conc*31.25, 
#                                datetime = as.POSIXct(lubridate::mdy_hms(paste(Date, Time, sep = " "))), 
#                                unclassdatetime = unclass(as.POSIXct(datetime)))
# 
# autoBOD1_bottle3 <- autoBOD1 %>% filter(bottle == 3)
# 
# Bottle <- autoBOD1_bottle3
# Range_Min <- 1577837035
# Range_Max <- 1578222267
# mean(Range_Min+Range_Max)
# a <- as.POSIXct(1578222267, origin = "1970-01-01 00:00.00 UTC")
# 
# monte_carlo_estimates <- Monte_Carlo_Slope_Sim(Bottle = Bottle, Range_Min = Range_Min, Range_Max = Range_Max)
# 
# ggplot(Bottle, aes(x = datetime, y = o2conc_umol_L))+
#   geom_point()+
#   ggtitle("Sample AutoBOD File Bottle 3")+
#   annotate(geom = "text", 
#            color = "red", 
#            y = 248, 
#            x = as.POSIXct(mean(c(Range_Min, Range_Max)), origin = "1970-01-01 00:00.00 UTC"), 
#            label = paste0("Number of Pairs = ", monte_carlo_estimates$Number_of_Pairs, ", ", 
#                           "Mean dO2 per hour = ", round(monte_carlo_estimates$Avg_dO2_umol_hour, digits = 4), ", ", 
#                           "Slope Standard Error =", round(monte_carlo_estimates$Slope_SE, digits = 4)))
#   