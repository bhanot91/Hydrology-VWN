#### Citation Information ####
# Please cite the following publications in using this information
# For the publication:
# Peer and Chini (2020) "Global Virtual Water Trade of Energy" ERL
#
# For the dataset and scripts
# Chini and Peer (2020) "The Water Footprint of Global Energy Trade
#   from 2010 to 2018" Scientific Data
#

### Script to download all data from UN Comtrade
# Uses comtradr data to interface with API
# Required library for UN Comtrade API 
library(comtradr)
 
# Clear cache
 rm(list=ls())
 gc()

#Set directory
# setwd("Comtrade Data Directory")

# Unique country codes is a list of all UN countries 
# These data are provided in the Supporting Information from
# Peer and Chini (2020)
unique.country.codes<-read.csv("UniqueCountryCodes.csv", encoding = "UTF-8")
unique.country.codes$Name<-as.character(unique.country.codes$Name)

## Define commodity codes
# Biodiesel--3826
# Fuelwood--4401
# Charcoal--4402

# Coal--2701
# Lignite--2702

# Crude Oil--2709
# Not Crude Oil--2710

# Electricity--2716

# Natural gas--271111
# Butane--271113
# Propane--271112

# Authorization code
# Authorization code is necessary to increase download limits from the default
# limit of 100 queries an hour
tk<-"authorization code"
ct_register_token(tk)
ct_get_remaining_hourly_queries()
getOption("comtradr")$comtrade

# Set Empty data frames

data.raw<-ct_search(reporters = "USA", 
                         partners = "all", 
                         trade_direction = c("exports","imports"), 
                         start_date = 2010, 
                         end_date = 2010,
                         commod_codes = "2716")

biofuels<-data.raw[0,]
electricity<-data.raw[0,]
oil<-data.raw[0,]
coal<-data.raw[0,]
hydrocarbon<-data.raw[0,]


for(rep in seq(1,nrow(unique.country.codes),5)){
  if(rep<215){
    reporter<-c(unique.country.codes$Name[rep],
              unique.country.codes$Name[rep+1],
              unique.country.codes$Name[rep+2],
              unique.country.codes$Name[rep+3],
              unique.country.codes$Name[rep+4])
  }
  else{
    reporter<-c(unique.country.codes$Name[rep],
                unique.country.codes$Name[rep+1])
  }
  
  # Biofuels
  bf1<-ct_search(reporters = reporter,
                      partners = "all",
                      trade_direction = c("exports","imports"),
                      start_date = 2010,
                      end_date = 2014,
                      commod_codes = c("3826","4401","4402"))
  ct_get_remaining_hourly_queries()
  bf2<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2015,
                 end_date = 2018,
                 commod_codes = c("3826","4401","4402"))
  biofuels<-rbind(biofuels,bf1, bf2)
  ct_get_remaining_hourly_queries()
  #Electricity
  e1<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2010,
                 end_date = 2014,
                 commod_codes = "2716")
  e2<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2015,
                 end_date = 2018,
                 commod_codes = "2716")
  electricity<-rbind(electricity,e1, e2)

  #Oil
  o1<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2010,
                 end_date = 2014,
                 commod_codes = c("2709","2710"))
  o2<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2015,
                 end_date = 2018,
                 commod_codes = c("2709","2710"))
  oil<-rbind(oil,o1, o2)

  #Coal
  c1<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2010,
                 end_date = 2014,
                 commod_codes = c("2701","2702"))
  c2<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2015,
                 end_date = 2018,
                 commod_codes = c("2701","2702"))
  coal<-rbind(coal,c1, c2)

  # Natural gas--271111
  # Butane--271113
  # Propane--271112

  #Hydrocarbon
  hc1<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2010,
                 end_date = 2014,
                 commod_codes = c("271111","271112","271113"))
  hc2<-ct_search(reporters = reporter,
                 partners = "all",
                 trade_direction = c("exports","imports"),
                 start_date = 2015,
                 end_date = 2018,
                 commod_codes = c("271111","271112","271113"))
  hydrocarbon<-rbind(hydrocarbon,hc1, hc2)
}

write.csv(biofuels,"Biofuels2010-2018.csv")
write.csv(electricity,"Electricity2010-2018.csv")
write.csv(oil,"Oil2010-2018.csv")
write.csv(coal,"Coal2010-2018.csv")
write.csv(hydrocarbon,"Hydrocarbons2010-2018.csv")


