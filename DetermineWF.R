#### Citation Information ####
# Please cite the following publications in using this information
# For the publication:
# Peer and Chini (2020) "Global Virtual Water Trade of Energy" ERL
#
# For the dataset and scripts
# Chini and Peer (2020) "The Water Footprint of Global Energy Trade
#   from 2010 to 2018" Scientific Data
#



## Script to convert trade data to water footprints
# Will import previous file generated from CompileTradeData.R
# Will add three columns to the file Average, Min and Max WF
# Each fuel source will have its own set of operations for conversion
# Directly imports output from reviseOutlier.R
## Written by: C. Chini
## Date Created: 16 Mar 2020
#
#
#
## Current Version: 1.3
#
## Version history (Editor, Date, Description)
# 25 Mar 2020--C.Chini added all remaining energy types except for Electricity
# 14 Apr 2020--R.Peer updated code to include electricity, referenced from 'getElectricityWF.r'
# 23 Apr 2020--R.Peer updates from error-fixing in Total Energy Trade 2010-2018
# 24 Apr 2020--C.Chini updates for new run to identify errors from 14 apr to 23 apr changes

#Set working directories
comtrade.wd <- 'ComTrade Data Directory'
electricity.wd <- 'Electricity Data Directory'
results.wd <- 'Results Directory'

setwd(comtrade.wd)

#Import Trade data formatted in CompileTradeData.R
energy.data<-read.csv("TotalEnergyTrade2010-2018_outliersremoved.csv",header=TRUE,sep=",", stringsAsFactors = FALSE)


#Add in the requisite columns with default of zero
energy.data$WF.mean.m3<-0
energy.data$WF.min.m3<-0
energy.data$WF.max.m3<-0


## First we will start with Firewood and charcoal because that is what I completed first

# Load firewood data
firewood<-read.csv("FirewoodWF_m3kg.csv", header=TRUE, sep=",")
# Load charcoal data
charcoal<-read.csv("CharcoalWF_m3kg.csv", header=TRUE, sep=",")
# Load Biodiesel data
biodiesel<-read.csv("BiodieselWF_m3kg-withrange.csv", header = TRUE, sep = ",")
# Load Natural Gas data
lng<-read.csv("LNGWF_m3kg.csv", header = TRUE, sep = ",")
# Load Propane data
propane<-read.csv("PropaneWF_m3kg.csv", header = TRUE, sep = ",")
# Load Butane data
butane<-read.csv("ButaneWF_m3kg.csv", header = TRUE, sep = ",")


# This will be inefficient but it is the way my brain works
# Cycle through all the data and if firewood or charcoal are the energy sources, then compute WFs

# Which countries do not have values
firewood.check<-matrix(NA,nrow=1, ncol=1)
charcoal.check<-matrix(NA,nrow=1, ncol=1)
biofuel.check<-matrix(NA,nrow=1, ncol=1)
lng.check<-matrix(NA,nrow=1, ncol=1)
prop.check<-matrix(NA,nrow=1, ncol=1)
but.check<-matrix(NA,nrow=1, ncol=1)


## Define our static global factors in m3 water/kg
# Crude Oil
crudeoil.mean<-0.011
crudeoil.min<-0.002
crudeoil.max<-0.048

# NotCrude Oil
notcrudeoil.mean<-0.0127
notcrudeoil.min<-0.0032
notcrudeoil.max<-0.0502

# Coal
coal.mean<-0.36/1000
coal.min<-0.18/1000
coal.max<-4.2/1000

#Lignite
lignite.mean<-0.36/1000
lignite.min<-0.10/1000
lignite.max<-0.72/1000

for(e in 1:nrow(energy.data)){
  
  #First get energy and unit
  energy<-energy.data$Commodity[e]
  unit<-energy.data$Unit[e]

  if(energy=="Fuelwood"){
    # Check units to make sure they are in kg
    
      origin<-energy.data$Reporter.Code[e]
      factors<-firewood[firewood$CNTRY_CODE==origin,]
      
      # Make sure we have a match in firewood
      if(nrow(factors)!=1)
      {
        firewood.check<-rbind(firewood.check,origin)
      }
      else{# Now that we have only one match, we can compute factors
        #Extract these factors
        avg<-factors$MEAN
        minimum<-factors$MIN
        maximum<-factors$MAX
        
        energy.data$WF.mean.m3[e]<-avg*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-minimum*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-maximum*energy.data$Quantity[e]
      }
      
    
      }
  # Now do the same thing for Charcoal
  if(energy=="Charcoal"){
    # Check units to make sure they are in kg
    
      origin<-energy.data$Reporter.Code[e]
      factors<-charcoal[charcoal$CNTRY_CODE==origin,]
      
      # Make sure we have a match in charcoal
      if(nrow(factors)!=1)
      {
        charcoal.check<-rbind(charcoal.check,origin)
      }
      else{# Now that we have only one match, we can compute factors
        #Extract these factors
        avg<-factors$MEAN
        minimum<-factors$MIN
        maximum<-factors$MAX
        
        energy.data$WF.mean.m3[e]<-avg*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-minimum*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-maximum*energy.data$Quantity[e]
      }
    
  }
  # Now do the same thing for LNG, Propane, and Butane
  if(energy=="LNG"){
    # Check units to make sure they are in kg
    
      origin<-energy.data$Reporter.Code[e]
      factors<-lng[lng$Reporter.Code==origin,]
      
      # Make sure we have a match in firewood
      if(nrow(factors)!=1)
      {
        lng.check<-rbind(lng.check,origin)
      }
      else{# Now that we have only one match, we can compute factors
        #Extract these factors
        avg<-factors$mean
        minimum<-factors$min
        maximum<-factors$max
        
        energy.data$WF.mean.m3[e]<-avg*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-minimum*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-maximum*energy.data$Quantity[e]
      }
    
  }
  if(energy=="Propane"){
    # Check units to make sure they are in kg
    
      origin<-energy.data$Reporter.Code[e]
      factors<-propane[propane$Reporter.Code==origin,]
      
      # Make sure we have a match in firewood
      if(nrow(factors)!=1)
      {
        prop.check<-rbind(prop.check,origin)
      }
      else{# Now that we have only one match, we can compute factors
        #Extract these factors
        avg<-factors$mean
        minimum<-factors$min
        maximum<-factors$max
        
        energy.data$WF.mean.m3[e]<-avg*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-minimum*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-maximum*energy.data$Quantity[e]
      }
    
  }
  if(energy=="Butane"){
    # Check units to make sure they are in kg
    
      origin<-energy.data$Reporter.Code[e]
      factors<-butane[butane$Reporter.Code==origin,]
      
      # Make sure we have a match in firewood
      if(nrow(factors)!=1)
      {
        but.check<-rbind(but.check,origin)
      }
      else{# Now that we have only one match, we can compute factors
        #Extract these factors
        avg<-factors$mean
        minimum<-factors$min
        maximum<-factors$max
        
        energy.data$WF.mean.m3[e]<-avg*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-minimum*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-maximum*energy.data$Quantity[e]
      }
    
  }
  #Now do a slightly different thing for biodiesel
  if(energy=="Biofuel"){
      origin<-energy.data$Reporter.Code[e]
      yr<-energy.data$Year[e]
      factors<-biodiesel[biodiesel$Country.Code==origin & biodiesel$year==yr,]
      # Make sure we have a match in charcoal
      if(nrow(factors)!=1)
      {
        biofuel.check<-rbind(biofuel.check,origin)
      }
      else{# Now that we have only one match, we can compute factors
        #Extract these factors
      # Now that we have only one match, we can compute factors
        #Extract these factors
        avg<-factors$mean
        minimum<-factors$min
        maximum<-factors$max
        
        energy.data$WF.mean.m3[e]<-avg*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-minimum*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-maximum*energy.data$Quantity[e]
      }
  }
  
  # For the next few fuels, we only have static global factors
  if(energy=="Coal"){
    # Check units to make sure they are in kg
    
        energy.data$WF.mean.m3[e]<-coal.mean*energy.data$Quantity[e]
        energy.data$WF.min.m3[e]<-coal.min*energy.data$Quantity[e]
        energy.data$WF.max.m3[e]<-coal.max*energy.data$Quantity[e]
      
  }
  if(energy=="Lignite"){
    # Check units to make sure they are in kg
      energy.data$WF.mean.m3[e]<-lignite.mean*energy.data$Quantity[e]
      energy.data$WF.min.m3[e]<-lignite.min*energy.data$Quantity[e]
      energy.data$WF.max.m3[e]<-lignite.max*energy.data$Quantity[e]
    
  }
  if(energy=="Crudeoil"){
    # Check units to make sure they are in kg
      energy.data$WF.mean.m3[e]<-crudeoil.mean*energy.data$Quantity[e]
      energy.data$WF.min.m3[e]<-crudeoil.min*energy.data$Quantity[e]
      energy.data$WF.max.m3[e]<-crudeoil.max*energy.data$Quantity[e]
    
  }
  if(energy=="Oil-NotCrude"){
    # Check units to make sure they are in kg

      energy.data$WF.mean.m3[e]<-notcrudeoil.mean*energy.data$Quantity[e]
      energy.data$WF.min.m3[e]<-notcrudeoil.min*energy.data$Quantity[e]
      energy.data$WF.max.m3[e]<-notcrudeoil.max*energy.data$Quantity[e]
    }
    
  
}

#Remove the first two columns which were holdovers from indices
energy.data<-energy.data[,3:18]

## Add in electricity WF data from "getElectricityWF.r"
setwd(electricity.wd)
electricity.data <- read.csv('WF-electricity.csv', row.names = 1)

electricity.data<-electricity.data[,3:18]
#Remove the current 0-value electricity WF
energy.data <- energy.data[which(energy.data$Commodity != 'Electricity'),]
#Append the calculated electricity WF
energy.data <- rbind(energy.data, electricity.data)

## Export to csv
setwd(results.wd)
write.csv(energy.data,"EnergyWF_Trade.csv")


