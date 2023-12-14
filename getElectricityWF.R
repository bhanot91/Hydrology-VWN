#### Citation Information ####
# Please cite the following publications in using this information
# For the publication:
# Peer and Chini (2020) "Global Virtual Water Trade of Energy" ERL
#
# For the dataset and scripts
# Chini and Peer (2020) "The Water Footprint of Global Energy Trade
#   from 2010 to 2018" Scientific Data
#


## Script to calculate country-level electricity water footprints and electricity trade WF
# Importing scraped IEA electricity mix (by country)
# Determines fraction of wet-cooled generation
# Calculates water consumption (fresh) for each country in each year
# Add three columns to the file Average, Min and Max WF 
# (consistent with DetermineWF.r output for integration)
## Written by: R. Peer
## Date Created: 3 April 2020
#
#
#
## Current Version: 1.4
#
## Version history (Editor, Date, Description)
# Apr 10, 2020 -- R.Peer, Manually adjust IEA country names to match Comtrade data
# Apr 15, 2020 -- R.Peer, Manually add electricity mixes for Bhutan, Laos, Lesotho, Rwanda, Uganda
# Apr 23, 2020 -- R.Peer, New Total Energy 2010-2018 spreadsheet with updated error-catching
# May 22, 2020 -- R.Peer, New input spreadsheet for missing IEA countries

#Set project working directories
project.wd <- 'Electricity Working Directory'
comtrade.wd <- 'Comtrade Data Directory'

#Import electricity data from IEA scraping and make long-form dataframe
setwd(project.wd)

electricity.mix2010 <- read.csv('IEA-electricity-mix-2010-GWh.csv')
electricity.mix2011 <- read.csv('IEA-electricity-mix-2011-GWh.csv')
electricity.mix2012 <- read.csv('IEA-electricity-mix-2012-GWh.csv')
electricity.mix2013 <- read.csv('IEA-electricity-mix-2013-GWh.csv')
electricity.mix2014 <- read.csv('IEA-electricity-mix-2014-GWh.csv')
electricity.mix2015 <- read.csv('IEA-electricity-mix-2015-GWh.csv')
electricity.mix2016 <- read.csv('IEA-electricity-mix-2016-GWh.csv')
electricity.mix2017 <- read.csv('IEA-electricity-mix-2017-GWh.csv')
#2018 data are incomplete from IEA, so use 2017 data for now
electricity.mix2018 <- read.csv('IEA-electricity-mix-2018-GWh.csv')

electricity.mix2010$year <- 2010
electricity.mix2011$year <- 2011
electricity.mix2012$year <- 2012
electricity.mix2013$year <- 2013
electricity.mix2014$year <- 2014
electricity.mix2015$year <- 2015
electricity.mix2016$year <- 2016
electricity.mix2017$year <- 2017
electricity.mix2018$year <- 2018

electricity.mix <- rbind(electricity.mix2010,electricity.mix2011,electricity.mix2012,
                         electricity.mix2013,electricity.mix2013,electricity.mix2014,
                         electricity.mix2015,electricity.mix2016,electricity.mix2017,
                         electricity.mix2018)

#Add in missing data in IEA that have trade data in UN Comtrade (see SI for sources)
missing.emix <- read.csv('IEA-missing-countries.csv')
electricity.mix <- rbind(electricity.mix,missing.emix)

#Assign code for Davies et al. (2013) global cooling mix
electricity.mix['Daviescode'] <- 0

#Regional breakdown in the Davies paper based on GCAMv3 14 geopolitical regions
africa = c('Burundi','Comoros','Djibouti','Eritrea','Ethiopia','Kenya','Madagascar',
           'Mauritius','Reunion','Rwanda','Sudan','Somalia','Uganda','Algeria',
           'Egypt','Libya','Morocco','Tunisia','Zambia', 'Zimbabwe', 'Benin', 
           'Burkina Faso', 'Central African Republic', "Côte d'Ivoire", 'Cameroon', 
           'Dem. Rep. of the Congo', 'Congo', 'Cape Verde', 'Gabon', 
           'Ghana', 'Guinea', 'Gambia', 'Guinea-Bissau', 'Equatorial Guinea', 
           'Liberia', 'Mali', 'Mauritania', 'Niger', 'Nigeria', 'Senegal',
           'Sierra Leone', 'Sao Tome and Principe', 'Chad', 'Togo', 'South Africa',
           'Angola','Botswana', 'Mozambique','Namibia', 'United Rep. of Tanzania',
           'South Sudan','Lesotho')
usa = c('USA')
canada = c('Canada')
western.europe = c('Greenland','Iceland','United Kingdom','Norway','Sweden',
                   'Finland','Portugal','Spain','France','Germany','Belgium',
                   'Luxembourg','Netherlands','Italy','Switzerland','Denmark',
                   'Malta','Greece','Austria','Turkey','Cyprus','Ireland',
                   'Gibraltar')
eastern.europe = c('Albania','Bosnia Herzegovina','Croatia','North Macedonia',
                   'Bulgaria','Romania','Kosovo','Serbia','Montenegro','Hungary',
                   'Slovenia','Austria','Slovakia','Czechia','Poland')
former.soviet = c('Rep. of Moldova','Ukraine','Belarus','Lithuania','Latvia','Estonia',
                  'Georgia','Armenia','Azerbaijan','Kazakhstan','Uzbekistan',
                  'Kyrgyzstan','Turkmenistan','Tajikistan', 'Russian Federation')
middle.east = c('Iran','Iraq','syria','Lebanon','Israel','Jordan','Saudi Arabia',
                'United Arab Emirates','Oman','Yemen','Qatar','Bahrain', 'Kuwait',
                'Syria')
southeast.asia = c('Afganistan','Pakistan','Nepal','Bhutan','Bangladesh','Myanmar',
                   'Thailand','Laos', 'Malaysia', 'Singapore','Indonesia',
                   'Philippines', 'Papau New Guinea', 'Brunei','Srilanka',
                   "Lao People's Dem. Rep.")
central.asia = c('Cambodia','Viet Nam','China', 'China, Hong Kong SAR', 'Mongolia',
                 "Dem. People's Rep. of Korea")
japan = c('Japan')
aus.nz = c('Australia','New Zealand')
latin.america = c('Mexico','Brazil','Argentina','Chile','Guatemala', 'Honduras',
                  'El Salvador','Nicaragua','Costa Rica','Panama','Colombia',
                  'Venezuela','Ecuador','Guyana','Suriname','French Guiana',
                  'Peru','Bolivia','Paraguay','Uruguay', 'Cuba',
                  'Dominican Republic', 'Curaçao','Jamaica', 'Trinidad', 'Haiti')
south.korea = c('Korea')
india = c('India')

electricity.mix$Daviescode[electricity.mix$short %in% africa] <- 9
electricity.mix$Daviescode[electricity.mix$short %in% usa] <- 1
electricity.mix$Daviescode[electricity.mix$short %in% canada] <- 2
electricity.mix$Daviescode[electricity.mix$short %in% western.europe] <- 3
electricity.mix$Daviescode[electricity.mix$short %in% eastern.europe] <- 12
electricity.mix$Daviescode[electricity.mix$short %in% former.soviet] <- 6
electricity.mix$Daviescode[electricity.mix$short %in% middle.east] <- 8
electricity.mix$Daviescode[electricity.mix$short %in% southeast.asia] <- 11
electricity.mix$Daviescode[electricity.mix$short %in% central.asia] <- 7
electricity.mix$Daviescode[electricity.mix$short %in% japan] <- 4
electricity.mix$Daviescode[electricity.mix$short %in% aus.nz] <- 5
electricity.mix$Daviescode[electricity.mix$short %in% latin.america] <- 10
electricity.mix$Daviescode[electricity.mix$short %in% south.korea] <- 13
electricity.mix$Daviescode[electricity.mix$short %in% india] <- 14

#Apply Davies data to get generation that is wet-cooled for each country
davies.fractions <- read.csv('Davies-fractions.csv')

#Remove any country collections from IEA database (e.g., OECD, etc.)
electricity.mix <- electricity.mix[!(electricity.mix$Daviescode == 0),]

#Make df for generation that is wet-cooled (GWh) with freshwater (excluding seawater)
wetcooled.e <- electricity.mix[c(1,16,2:11,13,14,17)]

wetcooled.e$coal.ot <- davies.fractions$coal.ot[match(electricity.mix$Daviescode, 
                                                          davies.fractions$Daviescode)
                                                    ] * electricity.mix$Coal
wetcooled.e$coal.tower <- davies.fractions$coal.tower[match(electricity.mix$Daviescode, 
                                                          davies.fractions$Daviescode)
                                                    ] * electricity.mix$Coal
wetcooled.e$coal.pond <- davies.fractions$coal.pond[match(electricity.mix$Daviescode, 
                                                          davies.fractions$Daviescode)
                                                    ] * electricity.mix$Coal
#Fossil = oil+waste+other+biofuels ("other fossil/biofuels")
wetcooled.e$fossil.ot <- davies.fractions$fossil.ot[match(electricity.mix$Daviescode, 
                                                          davies.fractions$Daviescode)
                                                    ] * rowSums(electricity.mix[c(
                                                      3,9,10,13)], na.rm = T)
wetcooled.e$fossil.tower <- davies.fractions$fossil.tower[match(electricity.mix$Daviescode, 
                                                              davies.fractions$Daviescode)
                                                        ] * rowSums(electricity.mix[c(
                                                          3,9,10,13)], na.rm = T)
wetcooled.e$fossil.pond <- davies.fractions$fossil.pond[match(electricity.mix$Daviescode, 
                                                              davies.fractions$Daviescode)
                                                        ] * rowSums(electricity.mix[c(
                                                          3,9,10,13)], na.rm = T)
#Assume all NG follows NGCC ratios from Davies et al. 
wetcooled.e$ng.ot <- davies.fractions$ngcc.ot[match(electricity.mix$Daviescode, 
                                                      davies.fractions$Daviescode)
                                                ] * electricity.mix$Natural.gas
wetcooled.e$ng.tower <- davies.fractions$ngcc.tower[match(electricity.mix$Daviescode, 
                                                  davies.fractions$Daviescode)
                                            ] * electricity.mix$Natural.gas
wetcooled.e$ng.pond <- davies.fractions$ngcc.pond[match(electricity.mix$Daviescode,
                                                  davies.fractions$Daviescode)
                                            ] * electricity.mix$Natural.gas
wetcooled.e$nuc.ot <- davies.fractions$nuc.ot[match(electricity.mix$Daviescode, 
                                                  davies.fractions$Daviescode)
                                            ] * electricity.mix$Nuclear
wetcooled.e$nuc.tower <- davies.fractions$nuc.tower[match(electricity.mix$Daviescode, 
                                                    davies.fractions$Daviescode)
                                              ] * electricity.mix$Nuclear
wetcooled.e$nuc.pond <- davies.fractions$nuc.pond[match(electricity.mix$Daviescode, 
                                                    davies.fractions$Daviescode)
                                              ] * electricity.mix$Nuclear

#Macknick values for technology-based consumption rates (m3/Mwh)
coal.ot.min <- 0.4
coal.ot.med <- 1.0
coal.ot.max <- 1.2

coal.tower.min <- 1.8
coal.tower.med <- 2.6
coal.tower.max <- 4.2

coal.pond.min <- 1.1
coal.pond.med <- 2.1
coal.pond.max <- 2.7

fossil.ot.min <- 0.4
fossil.ot.med <- 0.9
fossil.ot.max <- 1.1

fossil.tower.min <- 2.5
fossil.tower.med <- 3.1
fossil.tower.max <- 4.4

fossil.pond.min <- 2.1
fossil.pond.med <- 2.3
fossil.pond.max <- 2.7

ng.ot.min <- 0.4
ng.ot.med <- 0.4
ng.ot.max <- 0.4

ng.tower.min <- 0.5
ng.tower.med <- 0.8
ng.tower.max <- 1.1

ng.pond.min <- 0.9
ng.pond.med <- 0.9
ng.pond.max <- 0.9

nuc.ot.min <- 0.4
nuc.ot.med <- 1.0
nuc.ot.max <- 1.5

nuc.tower.min <- 2.2
nuc.tower.med <- 2.5
nuc.tower.max <- 3.2

nuc.pond.min <- 2.1
nuc.pond.med <- 2.3
nuc.pond.max <- 2.7

geo.min <- 6.4 #assume all geothermal is binary
geo.med <- 14
geo.max <- 15

csp.min <- 2.8
csp.med <- 3.0
csp.max <- 3.3

pv.min <- 0
pv.med <- 0
pv.max <- 0

wind.min <- 0
wind.med <- 0
wind.max <- 0

#Calculate water use 
water.consumption.med <- wetcooled.e[c(1,2)]
water.consumption.med$coal.ot <- wetcooled.e$coal.ot*1000*coal.ot.med 
water.consumption.med$coal.tower <- wetcooled.e$coal.tower*1000*coal.tower.med
water.consumption.med$coal.pond <- wetcooled.e$coal.pond*1000*coal.pond.med
water.consumption.med$fossil.ot <-wetcooled.e$fossil.ot*1000*fossil.ot.med
water.consumption.med$fossil.tower <- wetcooled.e$fossil.tower*1000*fossil.tower.med
water.consumption.med$fossil.pond <- wetcooled.e$fossil.pond*1000*fossil.pond.med
water.consumption.med$ng.ot <- wetcooled.e$ng.ot*1000*ng.ot.med
water.consumption.med$ng.tower <- wetcooled.e$ng.tower*1000*ng.tower.med
water.consumption.med$ng.pond <- wetcooled.e$ng.pond*1000*ng.pond.med
water.consumption.med$nuc.ot <- wetcooled.e$nuc.ot*1000*nuc.ot.med
water.consumption.med$nuc.tower <- wetcooled.e$nuc.tower*1000*nuc.tower.med
water.consumption.med$nuc.pond <- wetcooled.e$nuc.pond*1000*nuc.pond.med
water.consumption.med$geothermal <- wetcooled.e$Geothermal*1000*geo.med
water.consumption.med$solar.thermal <- wetcooled.e$Solar.thermal*1000*csp.med
water.consumption.med$solar.pv <- wetcooled.e$Solar.PV*1000*pv.med
water.consumption.med$wind <- wetcooled.e$Wind*1000*wind.med

hydro.wcrates <- read.csv('Mekonnen-hydro-water-rates.csv')
water.consumption.med$hydro <- wetcooled.e$Hydro*1000*hydro.wcrates$water.consumption.m3.MWh[match(wetcooled.e$short,hydro.wcrates$country)]

water.consumption.min <- wetcooled.e[c(1,2)]
water.consumption.min$coal.ot <- wetcooled.e$coal.ot*1000*coal.ot.min
water.consumption.min$coal.tower <- wetcooled.e$coal.tower*1000*coal.tower.min
water.consumption.min$coal.pond <- wetcooled.e$coal.pond*1000*coal.pond.min
water.consumption.min$fossil.ot <-wetcooled.e$fossil.ot*1000*fossil.ot.min
water.consumption.min$fossil.tower <- wetcooled.e$fossil.tower*1000*fossil.tower.min
water.consumption.min$fossil.pond <- wetcooled.e$fossil.pond*1000*fossil.pond.min
water.consumption.min$ng.ot <- wetcooled.e$ng.ot*1000*ng.ot.min
water.consumption.min$ng.tower <- wetcooled.e$ng.tower*1000*ng.tower.min
water.consumption.min$ng.pond <- wetcooled.e$ng.pond*1000*ng.pond.min
water.consumption.min$nuc.ot <- wetcooled.e$nuc.ot*1000*nuc.ot.min
water.consumption.min$nuc.tower <- wetcooled.e$nuc.tower*1000*nuc.tower.min
water.consumption.min$nuc.pond <- wetcooled.e$nuc.pond*1000*nuc.pond.min
water.consumption.min$geothermal <- wetcooled.e$Geothermal*1000*geo.min
water.consumption.min$solar.thermal <- wetcooled.e$Solar.thermal*1000*csp.min
water.consumption.min$solar.pv <- wetcooled.e$Solar.PV*1000*pv.min
water.consumption.min$wind <- wetcooled.e$Wind*1000*wind.min
water.consumption.min$hydro <- water.consumption.med$hydro

water.consumption.max <- wetcooled.e[c(1,2)]
water.consumption.max$coal.ot <- wetcooled.e$coal.ot*1000*coal.ot.max
water.consumption.max$coal.tower <- wetcooled.e$coal.tower*1000*coal.tower.max
water.consumption.max$coal.pond <- wetcooled.e$coal.pond*1000*coal.pond.max
water.consumption.max$fossil.ot <-wetcooled.e$fossil.ot*1000*fossil.ot.max
water.consumption.max$fossil.tower <- wetcooled.e$fossil.tower*1000*fossil.tower.max
water.consumption.max$fossil.pond <- wetcooled.e$fossil.pond*1000*fossil.pond.max
water.consumption.max$ng.ot <- wetcooled.e$ng.ot*1000*ng.ot.max
water.consumption.max$ng.tower <- wetcooled.e$ng.tower*1000*ng.tower.max
water.consumption.max$ng.pond <- wetcooled.e$ng.pond*1000*ng.pond.max
water.consumption.max$nuc.ot <- wetcooled.e$nuc.ot*1000*nuc.ot.max
water.consumption.max$nuc.tower <- wetcooled.e$nuc.tower*1000*nuc.tower.max
water.consumption.max$nuc.pond <- wetcooled.e$nuc.pond*1000*nuc.pond.max
water.consumption.max$geothermal <- wetcooled.e$Geothermal*1000*geo.max
water.consumption.max$solar.thermal <- wetcooled.e$Solar.thermal*1000*csp.max
water.consumption.max$solar.pv <- wetcooled.e$Solar.PV*1000*pv.max
water.consumption.max$wind <- wetcooled.e$Wind*1000*wind.max
water.consumption.max$hydro <- water.consumption.med$hydro

#Aggregate to total country level consumption
water.consumption.med$med <- rowSums(water.consumption.med[c(3:19)],na.rm = T)
water.consumption.min$min <- rowSums(water.consumption.min[c(3:19)],na.rm = T)
water.consumption.max$max <- rowSums(water.consumption.max[c(3:19)],na.rm = T)

#Create (fresh) water consumption rates for each country
elec.wc <- electricity.mix[c(1,16,15)]
elec.wc$min <- water.consumption.min$min
elec.wc$med <- water.consumption.med$med
elec.wc$max <- water.consumption.max$max

elec.wc$wf.min <- elec.wc$min / (elec.wc$Total.production*1000) #total e- water footprint in m3/MWh
elec.wc$wf.med <- elec.wc$med / (elec.wc$Total.production*1000)
elec.wc$wf.max <- elec.wc$max / (elec.wc$Total.production*1000)

#Output country-level water consumption rates as csv
country.wcrates <- elec.wc
names(country.wcrates) <- c("Country","Year","Generation.GWh","WC.min.m3","WC.med.m3","WC.max.m3",
                            "WCR.min.m3perMWh","WCR.med.m3perMWh","WCR.max.m3perMWh")
write.csv(country.wcrates[c(1,2,8,7,9)], file = 'ElectricityWaterIntensity.csv', row.names = F)

#Get electricity trade data
setwd(comtrade.wd)

#Import Trade data formatted in CompileTradeData.R
energy.data <- read.csv('TotalEnergyTrade2010-2018_outliersremoved.csv',
                        header=TRUE, sep=",", stringsAsFactors = FALSE)

#Isolate electricity data to manipulate
electricity.data <- energy.data[which(energy.data$Commodity == 'Electricity'),]

electricity.trade <- merge(electricity.data, elec.wc[c(1,2,7:9)], by.x = c('Reporter','Year'),
                          by.y = c('short','year'), all.x=T)

electricity.trade <- unique(electricity.trade)

#Calculate virtual water exports (cubic meters)
electricity.trade$WF.mean.m3 <- electricity.trade$Quantity * electricity.trade$wf.med
electricity.trade$WF.min.m3 <- electricity.trade$Quantity * electricity.trade$wf.min
electricity.trade$WF.max.m3 <- electricity.trade$Quantity * electricity.trade$wf.max

#reorganize columns to match other commodities in "DetermineWF.r"
electricity.trade <- electricity.trade[c(3,4,2,5,6,1,7:21)]

#Save results as csv
setwd(project.wd)
write.csv(electricity.trade[c(1:15,19:21)], file = 'WF-electricity.csv')
