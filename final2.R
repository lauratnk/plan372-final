#load libraries
library(tidycensus)
library(tidyverse)
library(sf)
library(dplyr)

#Load Pittsburgh stop data
stop_data = read.csv("wprdc_stop_data.csv")

tT#Filter stop data to only include pre-pandemic records, buses and weekday data
stop_data = filter(stop_data, time_period == "Pre-pandemic")
stop_data = filter(stop_data, mode %in% c("Bus", "Busway"))
stop_data = filter(stop_data, serviceday == "Weekday")

#Summarize bus stop boarding and alighting totals
stop_totals = group_by(stop_data, stop_id, longitude, latitude) %>%
  summarise(
    total_ons = sum(total_ons), 
    total_offs = sum(total_offs),
    days = sum(days),
    avg_ons = (sum(total_ons)/sum(days)),
    avg_offs = (sum(total_offs)/sum(days)),
    avg_on_and_off = ((sum(total_offs) + sum(total_ons))/sum(days))
  )


#export to CSV
write.csv(stop_totals,"pittsburg_stop_totals.csv")

#import modified CSV
cbg_stop_data = read.csv("cbg_bus_stop_totals2.csv")


#Summarize boarding and alighting totals for each census block group
census_block_totals = group_by(cbg_stop_data, GEOID) %>%
  summarise(
    total_ons = sum(total_ons), 
    total_offs = sum(total_offs),
    days = sum(days),
    avg_ons = (sum(total_ons)/sum(days)),
    avg_offs = (sum(total_offs)/sum(days)),
    avg_on_and_off = ((sum(total_offs) + sum(total_ons))/sum(days))
  )

#load ACS 5-year census data
acs_vars = load_variables(2021, "acs5")
acs_vars = filter(acs_vars, geography == "block group")

#create census data table with relevant social, economic and demographic variables
censusdata = get_acs(geography = "block group", 
                     variables=c("totalpop" = "B01003_001", 
                                 "ownernovehicle" = "B25044_003",
                                 "owner1vehicle" = "B25044_004",
                                 "renternovehicle" = "B25044_010",
                                 "renter1vehicle" = "B25044_011",
                                 "vehiclesavail" = "B25046_001", 
                                 "wfh" = "B08301_021", 
                                 "commutetransit" = "B08301_010", 
                                 "commutecar" = "B08301_002", 
                                 "ls10k" = "B19001_002",
                                 "i10kto15k" = "B19001_003",
                                 "i15kto20k" = "B19001_004",
                                 "i20kto25k" = "B19001_005",
                                 "i25kto30k" = "B19001_006",
                                 "i30kto35k" = "B19001_007",
                                 "i35kto40k" = "B19001_008",
                                 "i40kto45k" = "B19001_009",
                                 "i45kto50k" = "B19001_010",
                                 "i50kto60k" = "B19001_011",
                                 "i60kto75k" = "B19001_012",
                                 "i75kto100k" = "B19001_013",
                                 "i100kto125k" = "B19001_014",
                                 "i125kto150k" = "B19001_015",
                                 "i150kto200k" = "B19001_016",
                                 "more200k" = "B19001_017", 
                                 "employed" = "B23025_004", 
                                 "unemployed" = "B23025_005" 
                     ),
                     year = 2021,
                     state = "PA",
                     survey = "acs5",
                     output = "wide",
                     geometry = F)

#before joining, convert each tables' GEOID column to characters
census_block_totals$GEOID = as.character(census_block_totals$GEOID)
censusdata$GEOID = as.character(censusdata$GEOID)

#perform join
join1 = left_join(census_block_totals, censusdata, by = c("GEOID" = "GEOID"))

#regression model 1
## this model can be cumulative 
passenger_model1 = lm(avg_on_and_off~
                        totalpopE + 
                        ownernovehicleE + 
                        owner1vehicleE + 
                        renternovehicleE + 
                        renter1vehicleE + 
                        vehiclesavailE + 
                        wfhE + 
                        commutetransitE + 
                        commutecarE + 
                        ls10kE + 
                        i10kto15kE + 
                        i15kto20kE + 
                        i20kto25kE + 
                        i25kto30kE + 
                        i30kto35kE + 
                        i35kto40kE + 
                        i40kto45kE + 
                        i45kto50kE + 
                        i50kto60kE + 
                        i60kto75kE + 
                        i75kto100kE + 
                        i100kto125kE + 
                        i125kto150kE + 
                        i150kto200kE + 
                        more200kE + 
                        employedE + 
                        unemployedE 
                        ,join1)
summary(passenger_model1)

#regression model 2
#removed values with p values <0.7
passenger_model2 = lm(avg_on_and_off~
                        totalpopE + 
                        ownernovehicleE + 
                        owner1vehicleE + 
                        renternovehicleE + 
                        renter1vehicleE + 
                        vehiclesavailE + 
                        wfhE + 
                        commutetransitE + 
                        commutecarE + 
                        i10kto15kE + 
                        i15kto20kE + 
                        i20kto25kE + 
                        i25kto30kE + 
                        i35kto40kE + 
                        i40kto45kE + 
                        i50kto60kE + 
                        i60kto75kE + 
                        i75kto100kE + 
                        i150kto200kE + 
                        more200kE + 
                        employedE + 
                        unemployedE 
                      ,join1)
summary(passenger_model2)

#regression model 3
#removed values with p values <0.5
passenger_model3 = lm(avg_on_and_off~
                        totalpopE + 
                        renternovehicleE + 
                        renter1vehicleE + 
                        vehiclesavailE + 
                        wfhE + 
                        commutetransitE + 
                        commutecarE + 
                        i10kto15kE + 
                        i15kto20kE + 
                        i25kto30kE + 
                        i35kto40kE + 
                        i50kto60kE + 
                        i75kto100kE + 
                        i150kto200kE + 
                        more200kE + 
                        employedE + 
                        unemployedE 
                      ,join1)
summary(passenger_model3)

#regression model 4
#removed values with p values <0.4
passenger_model4 = lm(avg_on_and_off~
                        totalpopE + 
                        renternovehicleE + 
                        renter1vehicleE + 
                        vehiclesavailE + 
                        wfhE + 
                        commutetransitE + 
                        commutecarE + 
                        i10kto15kE + 
                        i35kto40kE + 
                        i50kto60kE + 
                        i150kto200kE + 
                        more200kE + 
                        employedE + 
                        unemployedE 
                      ,join1)
summary(passenger_model4)


#forecast

forecast_system = select(join1, GEOID, avg_on_and_off)

#forcast for passenger model 1
summary(passenger_model1)
forecast_system$passenger_model1 = (
  (10.962087) 
  + (join1$totalpopE)* -0.011996
  + (join1$ownernovehicleE)*0.030601
  + (join1$owner1vehicleE)*0.018106
  + (join1$renternovehicleE)* 0.071228
    + (join1$renter1vehicleE)* 0.022537
    + (join1$vehiclesavailE)* 0.011859
    + (join1$wfhE)* -0.092593
    + (join1$commutetransitE)* -0.040988
    + (join1$commutecarE)* -0.088747
    + (join1$ls10kE)* 0.006782
    + (join1$i10kto15kE)* -0.056591
    + (join1$i15kto20kE)* -0.041237
    + (join1$i20kto25kE)* -0.014814
    + (join1$i25kto30kE)* -0.032693
    + (join1$i30kto35kE)* -0.012376
    + (join1$i35kto40kE)* -0.062770
    + (join1$i40kto45kE)* -0.002759
    + (join1$i45kto50kE)* -0.053644
    + (join1$i50kto60kE)* 0.028328
    + (join1$i60kto75kE)* -0.013782
    + (join1$i75kto100kE)* -0.031631
    + (join1$i100kto125kE)* -0.007024
    + (join1$i125kto150kE)* -0.005408
    + (join1$i150kto200kE)* 0.017998
    + (join1$more200kE)* 0.014687
    + (join1$employedE)* 0.088996
    + (join1$unemployedE)*-0.020330
)

#forcast for passenger model 2
summary(passenger_model2)
forecast_system$passenger_model2 = (
  (11.1266762) 
  + (join1$totalpopE)* -0.0114898
  + (join1$ownernovehicleE)*0.0241495
  + (join1$owner1vehicleE)*0.0106557
  + (join1$renternovehicleE)* 0.0678563
  + (join1$renter1vehicleE)* 0.0177446
  + (join1$vehiclesavailE)* 0.0092389
  + (join1$wfhE)* -0.0919668
  + (join1$commutetransitE)* -0.0413961
  + (join1$commutecarE)* -0.0907999
  + (join1$i10kto15kE)* -0.0494289
  + (join1$i15kto20kE)* -0.0354031
  + (join1$i20kto25kE)* -0.0092684
  + (join1$i25kto30kE)* -0.0302216
  + (join1$i35kto40kE)* -0.0599870
  + (join1$i40kto45kE)* 0.0008683
  + (join1$i50kto60kE)* 0.0373063
  + (join1$i60kto75kE)* -0.0068087
  + (join1$i75kto100kE)* -0.0216162
  + (join1$i150kto200kE)* 0.0274727
  + (join1$more200kE)* 0.0219635
  + (join1$employedE)* 0.0882117
  + (join1$unemployedE)*-0.0208520
)

#forcast for passenger model 3
summary(passenger_model3)
forecast_system$passenger_model3 = (
  (11.539945) 
  + (join1$totalpopE)* -0.010843
  + (join1$renternovehicleE)* 0.062190
  + (join1$renter1vehicleE)* 0.013849
  + (join1$vehiclesavailE)* 0.008963
  + (join1$wfhE)* -0.093508
  + (join1$commutetransitE)* -0.041220
  + (join1$commutecarE)* -0.092975
  + (join1$i10kto15kE)* -0.041613
  + (join1$i15kto20kE)* -0.029622
  + (join1$i25kto30kE)* -0.025173
  + (join1$i35kto40kE)* -0.053889
  + (join1$i50kto60kE)* 0.044092
  + (join1$i75kto100kE)* -0.017639
  + (join1$i150kto200kE)* 0.031377
  + (join1$more200kE)* 0.021736
  + (join1$employedE)* 0.089312
  + (join1$unemployedE)* -0.021140
)

#forcast for passenger model 4
summary(passenger_model4)
forecast_system$passenger_model4 = (
  (11.176829) 
  + (join1$totalpopE)* -0.011272
  + (join1$renternovehicleE)* 0.055713
  + (join1$renter1vehicleE)* 0.011110
  + (join1$vehiclesavailE)* 0.005920
  + (join1$wfhE)* -0.092997
  + (join1$commutetransitE)* -0.040462
  + (join1$commutecarE)* -0.092506
  + (join1$i10kto15kE)* -0.036605
  + (join1$i35kto40kE)* -0.052164
  + (join1$i50kto60kE)* 0.049230
  + (join1$i150kto200kE)* 0.039491
  + (join1$more200kE)* 0.029948
  + (join1$employedE)* 0.089559
  + (join1$unemployedE)* -0.021113
)

#495 out of 1100 CBGs had incomplete data, which resulted in many NA values in the prediction models

#plot passenger volume forecasts vs passenger volume totals
ggplot(forecast_system, aes(x=passenger_model1, y=avg_on_and_off)) +
  geom_point() +
  xlim(0,75)+
  ylim(0,75)+
  geom_smooth()
ggplot(forecast_system, aes(x=passenger_model2, y=avg_on_and_off)) +
  geom_point() +
  xlim(0,75)+
  ylim(0,75)+
  geom_smooth()
ggplot(forecast_system, aes(x=passenger_model3, y=avg_on_and_off)) +
  geom_point() +
  xlim(0,75)+
  ylim(0,75)+
  geom_smooth()
ggplot(forecast_system, aes(x=passenger_model4, y=avg_on_and_off)) +
  geom_point() +
  xlim(0,75)+
  ylim(0,75)+
  geom_smooth()

#percent change histograms
#calculate percent change
forecast_system$model1_per_change = (forecast_system$passenger_model1 - forecast_system$avg_on_and_off)/ forecast_system$passenger_model1
forecast_system$model2_per_change = (forecast_system$passenger_model2 - forecast_system$avg_on_and_off)/ forecast_system$passenger_model2
forecast_system$model3_per_change = (forecast_system$passenger_model3 - forecast_system$avg_on_and_off)/ forecast_system$passenger_model3
forecast_system$model4_per_change = (forecast_system$passenger_model4 - forecast_system$avg_on_and_off)/ forecast_system$passenger_model4


ggplot(forecast_system,aes(x=model1_per_change))+ 
  geom_histogram(binwidth=1)+
  xlim(-25,25) +
  ylim(0,275)

ggplot(forecast_system,aes(x=model2_per_change))+ 
  geom_histogram(binwidth=1)+
  xlim(-25,25) +
  ylim(0,275)

ggplot(forecast_system,aes(x=model3_per_change))+ 
  geom_histogram(binwidth=1)+
  xlim(-25,25) +
  ylim(0,275)

ggplot(forecast_system,aes(x=model4_per_change))+ 
  geom_histogram(binwidth=1)+
  xlim(-25,25) +
  ylim(0,275)
