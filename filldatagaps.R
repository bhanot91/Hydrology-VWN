#### Citation Information ####
# Please cite the following publications in using this information
# For the publication:
# Peer and Chini (2020) "Global Virtual Water Trade of Energy" ERL
#
# For the dataset and scripts
# Chini and Peer (2020) "The Water Footprint of Global Energy Trade
#   from 2010 to 2018" Scientific Data
#

## Script to identify and fill in data gaps associated with zero or null values
# This function is intended to be called within the CompileTradeData.R file
# Function receives two inputs: a list of null values, and a list of total values
# The function will then cycle through all the null values and determine the median
#    value of cost/unit from the following:
#     1. Cost/unit from link in previous year
#     2. Cost/unit from link in following year
#     3. Cost/unit for current year for all commodity exports from origin country
# In the event that none of these values exist, we will manually evaluate the remainder

fill.data.gaps<-function(null.values, total.data){
  # Add new column to both inputs
  null.values$unitcost<-0
  total.data$unitcost<-total.data$Quantity/total.data$Trade.Value.USD
  # Cycle through null values
  for(index in 1:nrow(null.values)){
    #Retrieve data from null.values
    yr<-null.values$Year[index]
    rep<-null.values$Reporter.ISO[index]
    part<-null.values$Partner.ISO[index]
    com<-null.values$Commodity[index]
    val<-null.values$Trade.Value.USD[index]
    
    # Get cost/unit 1
    opt1<-total.data$unitcost[total.data$Year==yr-1 &
                                total.data$Reporter.ISO==rep &
                                total.data$Partner.ISO==part &
                                total.data$Commodity==com]
    # Get cost/unit 2
    opt2<-total.data$unitcost[total.data$Year==yr+1 &
                                total.data$Reporter.ISO==rep &
                                total.data$Partner.ISO==part &
                                total.data$Commodity==com]
    # Get cost/unit 3
    opt3<-mean(total.data$unitcost[total.data$Year==yr &
                                total.data$Reporter.ISO==rep &
                                total.data$Commodity==com])
    
    # Now determine the median of these three options
    null.values$unitcost[index]<-median(c(opt1,opt2,opt3), na.rm=TRUE)
    null.values$Quantity[index]<-null.values$unitcost[index]*val
  }
  
  return(null.values)
}