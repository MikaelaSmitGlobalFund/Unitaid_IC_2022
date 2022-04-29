# UNITAID INVESTMENT CASE
# Version 1: 28th April 2022 by Mikaela Smit


rm(list = ls())

#######################
# Central Switchboard #
#######################

# Install packages if neccesary
if(firstrun>0) {
  install.packages("dplyr")
}

# Load Libraries
library(dplyr)
library(ggplot2)
library(data.table)   # For like function (%like%)


#---- Choose correct files
malaria_data             <- "2022_04_06_Impact_of_innovation_malaria.csv"
malarria_bycountry_data  <-"2022_04_25_Impact_of_innovation_malaria_country.csv"


#---- Set working directory
wd   <- setwd("/Users/mc1405/Dropbox/The Global Fund/Investment Case - Unitaid/2022-2024 IC/Modelling Work/outputs/")

#---- Harmonize variable names

malaria_cases_ic  <- "cases_Replenishment"
malaria_cases_cf  <- "cases_Counterfactual"
malaria_deaths_ic <- "deaths_Replenishment"
malaria_deaths_cf <- "deaths_Counterfactual"



#---- Define year range
start_year <- 2019
end_year   <- 2030

#---- Define scenario for each year
scenario <- c("Follow targets", "Follow targets", "Continued disruption", "Mix", "Follow targets", "Follow targets", "Follow targets", "Follow targets", "Follow targets", "Follow targets", "Follow targets", "Follow targets")
scenario_yr <- data.frame(year = start_year:end_year, 
                          scenario = scenario)
                          
                          
                          
####################
# Read in data #
####################

malaria                 <- read.csv(paste0(wd, "/", malaria_data), check.names = FALSE)
malaria_bycountry       <- read.csv(paste0(wd, "/", malarria_bycountry_data), check.names = FALSE)

# Make necessary tidies
malaria$pre <- as.character(malaria$pre)
malaria_bycountry$pre <- as.character(malaria_bycountry$pre)



####################
# Process the data #
####################


# Define final output table across countries
malaria_adj <- data.frame(year=start_year:end_year, 
                          cases_ic=integer(length(start_year:end_year)),
                          cases_cf=integer(length(start_year:end_year)), 
                          deaths_ic=integer(length(start_year:end_year)),
                          deaths_cf=integer(length(start_year:end_year)), stringsAsFactors = FALSE)

# Define final output table by country
iso = unique(malaria_bycountry$ISO)
length_adj = length(iso)*length(start_year:end_year)

malaria_by_country_adj <- data.frame(ISO =character(0), 
                          year=integer(0), 
                          cases_ic=integer(0),
                          cases_cf=integer(0), 
                          deaths_ic=integer(0),
                          deaths_cf=integer(0), stringsAsFactors = FALSE)




# Prepare loop
yr = start_year


# Run loop
for(yr in start_year:end_year){
  print(yr)
  
  # First for aggregate data
  temp <- malaria %>% dplyr::filter(pre == scenario_yr$scenario[scenario_yr$year==yr] &( year == yr))
  
  if(scenario_yr$scenario[yr-start_year+1] =="Mix"){
    a<-malaria %>% dplyr::filter( year == yr)
    a<-a %>% mutate_all((~as.numeric(as.character(.))))
    b<-colMeans(a)
    temp<-as.data.frame.list(b)
    
  }
  
  malaria_adj$cases_ic[yr-start_year+1] <-temp$cases_Replenishment
  malaria_adj$cases_cf[yr-start_year+1] <-temp$cases_Counterfactual
  malaria_adj$deaths_ic[yr-start_year+1] <-temp$deaths_Replenishment
  malaria_adj$deaths_cf[yr-start_year+1] <-temp$deaths_Counterfactual
  
   # Second by country data  
  temp <- malaria_bycountry %>% dplyr::filter(pre == scenario_yr$scenario[scenario_yr$year==yr] & (year == yr))
  
  if(scenario_yr$scenario[yr-start_year+1] =="Mix"){
    a<-malaria_bycountry %>% dplyr::filter( year == yr)
    b<- a %>% group_by(ISO)%>% summarise_at(vars(cases_Replenishment), list(cases_Replenishment = mean))
    c<- a %>% group_by(ISO)%>% summarise_at(vars(cases_Counterfactual), list(cases_Counterfactual = mean))
    d<- a %>% group_by(ISO)%>% summarise_at(vars(deaths_Replenishment), list(deaths_Replenishment = mean))
    e<- a %>% group_by(ISO)%>% summarise_at(vars(deaths_Counterfactual), list(deaths_Counterfactual = mean))
    
    temp <- merge (b, c, by = "ISO")
    temp <- merge (temp, d, by = "ISO")
    temp <- merge (temp, e, by = "ISO")
    
    temp <- cbind(year=yr, temp)
    temp <- temp[,c(2, 1, 3, 4,5, 6)]

    
  }
  
  # Keep only the columns we need
  temp <- temp[c("ISO","year", "cases_Replenishment", "cases_Counterfactual", "deaths_Replenishment", "deaths_Counterfactual")]
  
  # Add first set of data
  if(yr==start_year){
    malaria_by_country_adj <-temp
  }else{
    malaria_by_country_adj <-rbind(malaria_by_country_adj, temp)
  }
  
}    



#######################
# Run graphs to check #
#######################


# Plot Malaria Cases
ggplot(data = malaria_adj, aes(x = year)) +
  geom_point(aes(year, cases_ic), colour = "black") +
  geom_line(aes(year, cases_ic, colour = "UNITAID IC")) +
  geom_point(aes(year, cases_cf), colour = "black") +
  geom_line(aes(year, cases_cf, colour = "UNITAID CF")) +
  labs(x = "Year", y = " Number of Cases", title = " Number of Cases") +
  scale_y_continuous(limits = c(0, 300000000)) +
  labs(color = "Legend")

# Plot Malaria Deaths
ggplot(data = malaria_adj, aes(x = year)) +
  geom_point(aes(year, deaths_ic), colour = "black") +
  geom_line(aes(year, deaths_ic, colour = "UNITAID IC")) +
  geom_point(aes(year, deaths_cf), colour = "black") +
  geom_line(aes(year, deaths_cf, colour = "UNITAID CF")) +
  labs(x = "Year", y = " Number of Cases", title = " Number of Cases") +
  scale_y_continuous(limits = c(0, 1000000)) +
  labs(color = "Legend")

  


##################
# Code Graveyard #
##################

ggplot()

plot(y=malaria_adj$cases_ic, col = "blue", x=malaria_adj$year, ylim = c(0, 300000000), axes=FALSE, ann=FALSE)
lines(y=malaria_adj$cases_cf, col = "red", x=malaria_adj$year)
title(xlab="Year", col.lab(0,100000000, 200000000))
title(ylab="Cases")
box()


ggplot(data = malaria_adj, 
       mapping = aes(x=year, 
                     y = cases_ic))+
  geom_line(aes(colour = "UNITAID IC"))+
  geom_point()+
  geom_line(aes(y = cases_cf))+
  labs(color = "Legend")

ggplot(data = malaria_adj, 
       mapping = aes(x=year, 
                     y = cases_ic))+
  geom_point(size = 2, colour = "red")+
  geom_line(colour = "red")+
  labs(color = "Legend")


ggplot(malaria_adj, aes(year, cases_ic))+
  geom_point(size = 2, colour = "red")+
  geom_line(colour = "red")+
  labs(color = "Legend")


ggplot(malaria_adj, aes(year, y=value, colour = variable)) +
  geom_point(aes(y = cases_ic, colour = "red")) +
  # geom_line(colour = "red")+
  geom_point(aes(y = cases_cf, colour = "blue"))+
  labs(color = "Legend")

  
  
  
  