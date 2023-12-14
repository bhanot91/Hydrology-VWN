## Function to check the connection of two countries for Electricity Trade
## Written by: C. Chini
## Date Created: 28 Jan 2020
#
#
#
## Current Version: original
#
## Version history (Editor, Date, Description)
# 1. 
#
ElectricConnection<-function(origin, destination,
                         directory="default"){
  ## Function has three inputs, two required
  # Origin (required): is a number of a UN Comtrade country code
  # Destination (required) is a number of a UN Comtrade country code
  # Directory is optional, with an optionally defined default directory
  #
  ## Function returns a boolean of whether the connection makes sense
  ## Reads in a database of neighboring countries and those connected by
  ## an undersea connection
  # 
  # 
  
  ## Set working directory
  
  setwd(directory)
  
  ## Time to get in the good stuff
  # First load in the requisite neighboring country database
  electric.connections<-read.csv("ElectricityConnections.csv",sep=",",header=TRUE)
  
  
  # Subset the data based on origin and destination (to check both directions and avoid errors)
  subset.origin<-electric.connections[electric.connections$Country1.Number==origin,]
  
  subset.destination<-electric.connections[electric.connections$Country1.Number==destination,]
  
  # Check if neighbor is valid
  # Return true if valid neighbor, return false if not
  
  if(destination %in% subset.origin$Country2.Number | 
     origin %in% subset.destination$Country2.Number){
    return(TRUE)
  } else{
    return(FALSE)
  }
  
}