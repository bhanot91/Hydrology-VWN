#### Citation Information ####
# Please cite the following publications in using this information
# For the publication:
# Peer and Chini (2020) "Global Virtual Water Trade of Energy" ERL
#
# For the dataset and scripts
# Chini and Peer (2020) "The Water Footprint of Global Energy Trade
#   from 2010 to 2018" Scientific Data
#


## Script to combine and export all trade data into a single data frame
## This data frame will be in a long-data format and will facilitate
## better data analytics and be accessible for visualization using iGraph
## Written by: C. Chini
## Date Created: 16 Mar 2020
#
#
#
## Current Version: 2.0
#
## Version history (Editor, Date, Description)
# 
# v2.0 Chini 28 April 2020: Revised compile script to account for imports and export data
# v2.0.1 06 May 2020: uncovered error in electricity data and manually set netweight column to zero


# Set Directories
comtrade.wd <- "Comtrade Data Directory"
scripts.wd<-"Script Directory"  
setwd(comtrade.wd)


# Load in new dataframes
# These dataframes are direct exports of the script that interfaces with
# UN Comtrade API

electricity<-read.csv("Electricity2010-2018.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
biofuels<-read.csv("Biofuels2010-2018.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
coal<-read.csv("Coal2010-2018.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
oil<-read.csv("Oil2010-2018.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
hydrocarbons<-read.csv("Hydrocarbons2010-2018.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)

#Bind all dataframes together
total.data<-rbind(electricity, biofuels, coal, oil, hydrocarbons)

#Combine the two quantity columns by determining the maximum value
# First there is an error in the electricity as column 31 should be zero
# but is not in several places.
total.data$netweight_kg[total.data$commodity=="Electrical energy"]<-0

total.data$qty.rev<-do.call(pmax, c(total.data[c(29,31)],list(na.rm=TRUE)))
  
# V2.0 Added in trade flow
revised.dataframe<-c("year", "trade_flow","reporter_code","reporter","reporter_iso", "partner_code",
                     "partner", "partner_iso", "commodity_code","commodity", "qty_unit",
                     "qty.rev", "trade_value_usd")

total.data<-total.data[,revised.dataframe]
colnames(total.data)<-c("Year", "Trade.Flow","Reporter.Code","Reporter","Reporter.ISO", "Partner.Code",
                        "Partner", "Partner.ISO", "Commodity.Code","Commodity", "Unit","Quantity",
                        "Trade.Value.USD")
# Revise Commodity Names to be more concise
## Define commodity codes
# Biodiesel--3826
total.data[total.data$Commodity.Code==3826,10]<-"Biofuel"
# Fuelwood--4401
total.data[total.data$Commodity.Code==4401,10]<-"Fuelwood"
# Charcoal--4402
total.data[total.data$Commodity.Code==4402,10]<-"Charcoal"

# Coal--2701
total.data[total.data$Commodity.Code==2701,10]<-"Coal"
# Lignite--2702
total.data[total.data$Commodity.Code==2702,10]<-"Lignite"

# Crude Oil--2709
total.data[total.data$Commodity.Code==2709,10]<-"Crudeoil"
# Not Crude Oil--2710
total.data[total.data$Commodity.Code==2710,10]<-"Oil-NotCrude"

# Electricity--2716
total.data[total.data$Commodity.Code==2716,10]<-"Electricity"

# Natural gas--271111
total.data[total.data$Commodity.Code==271111,10]<-"LNG"
# Butane--271113
total.data[total.data$Commodity.Code==271113,10]<-"Butane"
# Propane--271112
total.data[total.data$Commodity.Code==271112,10]<-"Propane"



# Remove all partners that are 'world' --Partner code 0
total.data<-total.data[total.data$Partner.Code!=0,]

# Remove all partners that are 'NES' --Partner code 899
total.data<-total.data[total.data$Partner.Code!=899,]
#also all trade partners that are not specified
# 490 Asia, nes
# 637	North America and Central America, nes
# 568	Other Europe, nes
# 473	LAIA, nes
# 577	Other Africa, nes
# 838	Free Zones
# 527	Oceania, nes
# 839	Special Categories

total.data<-total.data[(total.data$Partner.Code!=490 &
                         total.data$Partner.Code!=637 &
                         total.data$Partner.Code!=568 &
                         total.data$Partner.Code!=473 &
                         total.data$Partner.Code!=577 &
                         total.data$Partner.Code!=838 &
                         total.data$Partner.Code!=527 &
                         total.data$Partner.Code!=839),]

#Now we need to verify all electric connections
# For simplicity and since it is relatively small dataset we will go through everything
# Add column for true connections, default is connection is appropriate value = TRUE
total.data$connection<-TRUE

#Source the two files we need
source("ElectricConnection.R")
#source("NGConnection.R")

for(i in 1:nrow(total.data)){
  if(total.data$Commodity[i]=="Electricity")
  {
    total.data$connection[i]<-ElectricConnection(total.data$Reporter.Code[i], total.data$Partner.Code[i], 
                                                 directory = comtrade.wd)
  }
  # if(total.data$Commodity[i]=="LNG")
  # {
  #   total.data$connection[i]<-NGConnection(total.data$Reporter.Code[i], total.data$Partner.Code[i])
  # }
}

# Remove all not practical Electric connections
total.data<-total.data[total.data$connection==TRUE,]

#Now remove the final column, since we do not need it anymore
total.data<-total.data[,1:length(total.data)-1]

### New to v2.0
## Need to rectify imports and exports
# We want to keep all three columns for reporter and partner (Code, Country, ISO)
# Remainder of the code is written for exports, so we need to flip import partners and reporters

#Need to create new columns for reporter and partner
# These values are the same for exports
total.data$Reporter.rev[total.data$Trade.Flow=="Export"]<-total.data$Reporter[total.data$Trade.Flow=="Export"]
total.data$Reporter.Code.rev[total.data$Trade.Flow=="Export"]<-total.data$Reporter.Code[total.data$Trade.Flow=="Export"]
total.data$Reporter.ISO.rev[total.data$Trade.Flow=="Export"]<-total.data$Reporter.ISO[total.data$Trade.Flow=="Export"]
total.data$Partner.rev[total.data$Trade.Flow=="Export"]<-total.data$Partner[total.data$Trade.Flow=="Export"]
total.data$Partner.Code.rev[total.data$Trade.Flow=="Export"]<-total.data$Partner.Code[total.data$Trade.Flow=="Export"]
total.data$Partner.ISO.rev[total.data$Trade.Flow=="Export"]<-total.data$Partner.ISO[total.data$Trade.Flow=="Export"]

#These values are reversed for imports
# These values are the same for exports
total.data$Reporter.rev[total.data$Trade.Flow=="Import"]<-total.data$Partner[total.data$Trade.Flow=="Import"]
total.data$Reporter.Code.rev[total.data$Trade.Flow=="Import"]<-total.data$Partner.Code[total.data$Trade.Flow=="Import"]
total.data$Reporter.ISO.rev[total.data$Trade.Flow=="Import"]<-total.data$Partner.ISO[total.data$Trade.Flow=="Import"]
total.data$Partner.rev[total.data$Trade.Flow=="Import"]<-total.data$Reporter[total.data$Trade.Flow=="Import"]
total.data$Partner.Code.rev[total.data$Trade.Flow=="Import"]<-total.data$Reporter.Code[total.data$Trade.Flow=="Import"]
total.data$Partner.ISO.rev[total.data$Trade.Flow=="Import"]<-total.data$Reporter.ISO[total.data$Trade.Flow=="Import"]

#Now I need to collapse this dataframe back to its original state.
revised.dataframe<-c("Year", "Trade.Flow","Reporter.Code.rev","Reporter.rev","Reporter.ISO.rev", "Partner.Code.rev",
                     "Partner.rev", "Partner.ISO.rev", "Commodity.Code","Commodity", "Unit","Quantity",
                     "Trade.Value.USD")
total.data<-total.data[,revised.dataframe]
colnames(total.data)<-c("Year", "Trade.Flow","Reporter.Code","Reporter","Reporter.ISO", "Partner.Code",
                        "Partner", "Partner.ISO", "Commodity.Code","Commodity", "Unit","Quantity",
                        "Trade.Value.USD")

### End edits for v 2.0

## Okay, now we have some data errors that we need to fix,
## specifically with respect to NA or Zero Values as quantities
setwd(comtrade.wd)
#Export zero and NA values
zeros<-total.data[total.data$Quantity==0,]
na.quantity<-total.data[is.na(total.data$Quantity),]
write.csv(zeros,"ZeroQuantities.csv")
write.csv(na.quantity,"NAQuantities.csv")

# Now remove those data (we will bring them back in below)
total.data<-total.data[total.data$Quantity>0,]
total.data<-total.data[!(is.na(total.data$Quantity)),]
# Output data without zeros to fill zero-values manually
write.csv(total.data, 'TotalEnergyTradeNoZeros.csv')

###Begin more edits for v2.0
# Completed outside R post-processing of the zeros and na values
# This includes manually combining the two files and removing NAs
# Now we will bring that data back in and run a script to fill the data blanks

null.values<-read.csv("RevisedZeroandNAQuantities-5.csv", sep=",", header=TRUE, stringsAsFactors = FALSE)
colnames(null.values)<-c("Year", "Trade.Flow","Reporter.Code","Reporter","Reporter.ISO", "Partner.Code",
                         "Partner", "Partner.ISO", "Commodity.Code","Commodity", "Unit","Quantity",
                         "Trade.Value.USD")
source("filldatagaps.R")
revised.values<-fill.data.gaps(null.values,total.data)

revised.values<-revised.values[is.finite(revised.values$Quantity),1:13]
### End edits for v2.0
total.data<-rbind(total.data,revised.values)

# Keep the maximum quantity
agg.by=list(total.data$Year, total.data$Reporter.Code, total.data$Partner.Code, total.data$Commodity)

total.data.rev<-aggregate(total.data$Quantity,by=agg.by,FUN=max)
colnames(total.data.rev)<-c("Year","Reporter.Code","Partner.Code",
                            "Commodity","Quantity")

# Merge revised data with initial list
total.data.rev.2<-merge(total.data.rev,total.data)

total.data.rev.2$dupl<-duplicated(total.data.rev.2[,c("Year","Reporter.Code","Partner.Code",
                              "Commodity","Quantity")])

#Remove duplicates (values that have imports/exports with the same quantity)
total.data<-total.data.rev.2[total.data.rev.2$dupl==FALSE,1:13]


# This final dataset contains unique values of trade but has not been checked for quality

write.csv(total.data,"TotalEnergyTrade2010-2018.csv")


