#### Citation Information ####
# Please cite the following publications in using this information
# For the publication:
# Peer and Chini (2020) "Global Virtual Water Trade of Energy" ERL
#
# For the dataset and scripts
# Chini and Peer (2020) "The Water Footprint of Global Energy Trade
#   from 2010 to 2018" Scientific Data
#

### Script to cycle through all data and determine any outliers
# The script will then correct outliers based on a prescribed formula
# Inputs are the direct outputs of CompileTradeData.R script


#Set working directories
comtrade.wd <- "Comtrade Data Directory"
scripts.wd<-"Scripts Directory"  
setwd(comtrade.wd)

energy.data<-read.csv("TotalEnergyTrade2010-2018.csv", header=TRUE, sep=",", stringsAsFactors = FALSE)
# Reorder these column names
energy.data<-energy.data[c("X", "Year", "Trade.Flow", "Reporter.Code", "Reporter", "Reporter.ISO",
              "Partner.Code", "Partner", "Partner.ISO", "Commodity.Code", "Commodity",
              "Unit", "Quantity", 'Trade.Value.USD')]

# The underlying logic surrounding this cleaning is that larger than expected
# values should be removed
# Therefore, we need to create a range of 'expected values' for each commodity traded

# Expected values can be defined based on the total quantity in previous years or
# the expected unit value of the commodity multiplied by its trade value

# This cannot be a static range, however, to allow for fluctuations in interannual
# trade and peak years
# Therefore, we will focus our efforts on values that are both greater in quantity
# AND greater in unit cost than expected

# First we will create a list of all possible trades and their median values
# Call this quantity.bound


agg.by.1<-list(Reporter.Code=energy.data$Reporter.Code,
               Partner.Code=energy.data$Partner.Code,
               Commodity=energy.data$Commodity)

quantity.bound <- aggregate(energy.data["Quantity"], by=agg.by.1, FUN=median)
colnames(quantity.bound)<-c("Reporter.Code","Partner.Code","Commodity","Quantity.Bound")

# Now calculate the expected unit value for the commodity
# We want this to be based on all trade values originating for a commodity
# additionally, we want a value for all paired trade (similar to above)

energy.data$Unit.Value<-energy.data$Quantity/energy.data$Trade.Value.USD
# By paired trade
unitvalue.bound.1<-aggregate(energy.data["Unit.Value"], by=agg.by.1, FUN=median)
colnames(unitvalue.bound.1)<-c("Reporter.Code","Partner.Code","Commodity","Unit.Value.Bound.1")

# By origin country
agg.by.2<-list(Reporter.Code=energy.data$Reporter.Code,
               Commodity=energy.data$Commodity)

unitvalue.bound.2 <- aggregate(energy.data["Unit.Value"], by=agg.by.2, FUN=median)
colnames(unitvalue.bound.2)<-c("Reporter.Code","Commodity","Unit.Value.Bound.2")



# Now match these values back to the original data
energy.data<-merge(energy.data, quantity.bound, by=c("Reporter.Code","Partner.Code","Commodity"))

energy.data<-merge(energy.data, unitvalue.bound.1, by=c("Reporter.Code","Partner.Code","Commodity"))

energy.data<-merge(energy.data, unitvalue.bound.2, by.x=c("Reporter.Code","Commodity"), by.y=c("Reporter.Code","Commodity"))

# Create a new column to flag variables that exceed our criteria
# Flag if they meet all three criteria

energy.data$Flag<-FALSE

energy.data$Flag<-ifelse(energy.data$Quantity/energy.data$Quantity.Bound>5 &
                           energy.data$Unit.Value/energy.data$Unit.Value.Bound.2>5
                         & energy.data$Unit.Value/energy.data$Unit.Value.Bound.1>5, TRUE, FALSE)
# determines 1408 true values


# Revise the quantity data
energy.data$Quantity.rev<-ifelse(energy.data$Flag==TRUE,
                                 energy.data$Trade.Value.USD*energy.data$Unit.Value.Bound.2,
                                 energy.data$Quantity)

# There are also a lot of values of Quantity =1
# For these values, let's adjust them based on the exected unit value #2

energy.data$Quantity.rev<-ifelse(energy.data$Quantity==1,
                                 energy.data$Trade.Value.USD*energy.data$Unit.Value.Bound.2,
                                 energy.data$Quantity.rev)


#Subset and export the Data
energy.data.rev<-energy.data[,c("X", "Year", "Trade.Flow", "Reporter.Code", "Reporter", "Reporter.ISO",
                           "Partner.Code", "Partner", "Partner.ISO", "Commodity.Code", "Commodity",
                           "Unit", "Quantity.rev", 'Trade.Value.USD')]

# Rename the data to match the other scripts
colnames(energy.data.rev)<-c("X", "Year", "Trade.Flow", "Reporter.Code", "Reporter", "Reporter.ISO",
                            "Partner.Code", "Partner", "Partner.ISO", "Commodity.Code", "Commodity",
                            "Unit", "Quantity", 'Trade.Value.USD')

# Write csv
write.csv(energy.data.rev,"TotalEnergyTrade2010-2018_outliersremoved.csv")
