require(pacman)

p_load(tidyverse,tidycensus,sf,corrplot,ggthemes,lfe)

# make a dataframe with county level variables

vars <- load_variables(year=2018,dataset="acs5")

##### median housing costs, commute time, others
# source: Census

census_data <- get_acs(geography = "county",
                       year=2018,
                       variables=c(
                         total="B01003_001",
                         median_hh_inc = "B19013_001",
                         hh_size= "B08202_001",
                         housing_cost = "B25105_001",
                         renters = "B07013_003",
                         divorced = "B06008_004",
                         lessHS = "B06009_002",
                         college = "B06009_005",
                         grad = "B06009_006",
                         workers = "B08012_001",
                         commute5 = "B08012_002",
                         commute10 = "B08012_003",
                         commute15 = "B08012_004",
                         commute20 = "B08012_005",
                         commute25 = "B08012_006",
                         commute30 = "B08012_007",
                         commute35 = "B08012_008",
                         commute40 = "B08012_009",
                         commute45 = "B08012_010",
                         commute60 = "B08012_011",
                         commute90 = "B08012_012",
                         commute90plus = "B08012_013"), 
                       geometry=T,                
                       shift_geo=F,
                       output="wide")

census_data <- census_data %>% dplyr::select(which(str_detect(names(census_data),"E")))
names(census_data) <- str_remove_all(names(census_data),"E$")
names(census_data)[2] <- "NAME"

census_data$divorced <- census_data$divorced/census_data$total
census_data$lessHS <- census_data$lessHS/census_data$total
 
census_data$travel_median <- NA

for (i in 1:nrow(census_data)) {
  if (!is.na(census_data$workers[i])){
  census_data$travel_median[i] <- median(c(rep(2.5,census_data$commute5[i]),
                                         rep(7.5,census_data$commute10[i]),
                                         rep(12.5,census_data$commute15[i]),
                                         rep(17.5,census_data$commute20[i]),
                                         rep(22.5,census_data$commute25[i]),
                                         rep(27.5,census_data$commute30[i]),
                                         rep(32.5,census_data$commute35[i]),
                                         rep(37.5,census_data$commute40[i]),
                                         rep(42.5,census_data$commute45[i]),
                                         rep(52.5,census_data$commute60[i]),
                                         rep(75,census_data$commute90[i]),
                                         rep(100,census_data$commute90plus[i])))
  }
}

census_data$grad <- census_data$grad/census_data$total
census_data$college <- census_data$college/census_data$total

######## politics ##########
# source: MIT election lab, https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ

votes <- read.delim("https://dataverse.harvard.edu/api/access/datafile/:persistentId?persistentId=doi:10.7910/DVN/VOQCHQ/HEIJCQ")
votes <- votes %>% dplyr::filter(year == 2016)

votes$party <- if_else(is.na(votes$party),"other",as.character(votes$party))

votes <- votes %>% dplyr::select(-c("year","version","office","candidate"))

votes <- votes %>% pivot_wider(names_from=party,values_from=c(candidatevotes))

votes$FIPS <- ifelse(str_length(votes$FIPS)==4,paste0(0,votes$FIPS),votes$FIPS)

votes$dem_prop <- votes$democrat / votes$totalvotes
votes$rep_prop <- votes$republican / votes$totalvotes
votes$other_prop <- votes$other/votes$totalvotes

###### health ###########
#source: countyhealthrankings.org https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020_0.csv

health <- read.csv("~/location-preferences/analytic_data2020_0.csv",skip=1)
health$vcrimerate2 <- health$v043_rawvalue
health$airpollution <- health$v125_rawvalue
health$waterpollution <- health$v124_rawvalue
health$educ <- health$v021_rawvalue
health$lifeexp <- health$v147_rawvalue
health$vax <- health$v155_rawvalue #higher values are good


health2 <- health %>% dplyr::select(fipscode,vax,lifeexp,airpollution,educ,vcrimerate2)
health2$fipscode <- ifelse(str_length(health2$fipscode)==4,paste0("0",health2$fipscode),health2$fipscode)

dat <- left_join(census_data,health2,by=c("GEOID"="fipscode"))
dat <- left_join(dat,votes,by=c("GEOID"="FIPS"))

summary(lm(lifeexp ~ housing_cost + airpollution + dem_prop + other_prop + vax + median_hh_inc + educ,data=dat))


########### violent crime ###############

load("~/location-preferences/UCR/DS0001/37059-0001-Data.rda")
crime <- da37059.0001
rm(da37059.0001)

crime$FIPS_ST <- ifelse(str_length(crime$FIPS_ST)==1,paste0("0",crime$FIPS_ST),crime$FIPS_ST)
crime$FIPS_CTY <- ifelse(str_length(crime$FIPS_CTY)==1,paste0("0",crime$FIPS_CTY),crime$FIPS_CTY)
crime$FIPS_CTY <- ifelse(str_length(crime$FIPS_CTY)==2,paste0("0",crime$FIPS_CTY),crime$FIPS_CTY)
crime$FIPS <- paste0(crime$FIPS_ST,crime$FIPS_CTY)

crime <- crime %>% dplyr::select(FIPS,P1VLNT)
names(crime) <- c("GEOID","vcrimerate")

dat <- left_join(dat,crime)

dat$vcrimerate <- 100000*(dat$vcrimerate/dat$total)
######## urbanicity ##########
# source: USDA

urban <- read.csv("~/location-preferences/urbanicity.csv")
names(urban)[1] <- "FIPS"
urban <- urban %>% dplyr::select(FIPS,RUCC_2013,Description)
urban$FIPS <- as.character(urban$FIPS)
 
dat <- left_join(dat,urban,by=c("GEOID"="FIPS"))
summary(lm(lifeexp ~ housing_cost + vcrimerate + airpollution + rep_prop + vax + median_hh_inc + educ + factor(RUCC_2013),data=dat))
summary(lm(lifeexp ~ factor(RUCC_2013),data=dat))

######## max temperature ############
#source: noaa, ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20200904

# noaa doesn't use regular state fips, but does use county fips, go figure
# set up a rosetta stone between noaa codes/fips codes
statecodes <- read.table("~/location-preferences/noaa_state_codes.txt",sep="\t",colClasses = "character")
statecodes$V1 <- trimws(statecodes$V1)
statecodes$V2 <- trimws(statecodes$V2)
statecodes2 <- fips_codes %>% dplyr::select(state_code,state_name)
statecodes <- left_join(statecodes,statecodes2,by=c("V2"="state_name"))
statecodes <- unique(statecodes)
names(statecodes) <- c("noaastate","statename","statefips")

####### not too hot ########


#noaa <- read.table("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-tmaxcy-v1.0.0-20200904")

temp <- noaa

temp$V1 <- ifelse(str_length(temp$V1)==10,paste0("0",temp$V1),temp$V1)

temp$year <- substr(temp$V1,8,11)
temp <- temp %>% dplyr::filter(year>=2010)

temp$noaastate <- substr(as.character(temp$V1),1,2)
temp <- left_join(temp,statecodes)
temp$countyfips <- substr(temp$V1,3,5)
temp$fips <- paste0(temp$statefips,temp$countyfips)

temp2 <- temp %>% group_by(fips) %>% summarise(jan=mean(V2),
                                               feb=mean(V3),
                                               mar=mean(V4),
                                               apr=mean(V5),
                                               may=mean(V6),
                                               jun=mean(V7),
                                               jul=mean(V8),
                                               aug=mean(V9),
                                               sep=mean(V10),
                                               oct=mean(V11),
                                               nov=mean(V12),
                                               dec=mean(V13))

temp2$maxtemp <- NA

for (i in 1:nrow(temp2)) {
  temp2$maxtemp[i] <- max(temp2[i,2:13])
}

temp2 <- temp2 %>% dplyr::select(fips,maxtemp)


dat <- left_join(dat,temp2,by=c("GEOID"="fips"))

#### not too cold ########

#noaa2 <- read.table("ftp://ftp.ncdc.noaa.gov/pub/data/cirs/climdiv/climdiv-tmincy-v1.0.0-20200904")

temp3 <- noaa2

temp3$V1 <- ifelse(str_length(temp3$V1)==10,paste0("0",temp3$V1),temp3$V1)

temp3$year <- substr(temp3$V1,8,11)
temp3 <- temp3 %>% dplyr::filter(year>=2010)

temp3$noaastate <- substr(as.character(temp3$V1),1,2)
temp3 <- left_join(temp3,statecodes)
temp3$countyfips <- substr(temp3$V1,3,5)
temp3$fips <- paste0(temp3$statefips,temp3$countyfips)


temp4 <- temp3 %>% group_by(fips) %>% summarise(jan=mean(V2),
                                               feb=mean(V3),
                                               mar=mean(V4),
                                               apr=mean(V5),
                                               may=mean(V6),
                                               jun=mean(V7),
                                               jul=mean(V8),
                                               aug=mean(V9),
                                               sep=mean(V10),
                                               oct=mean(V11),
                                               nov=mean(V12),
                                               dec=mean(V13))

temp4$mintemp <- NA

for (i in 1:nrow(temp4)) {
  temp4$mintemp[i] <- min(temp4[i,2:13])
}

temp4 <- temp4 %>% dplyr::select(fips,mintemp)


dat <- left_join(dat,temp4,by=c("GEOID"="fips"))


######### city MSAs for distance calc ##################
# source: Census


city <- get_estimates(
  geography = "metropolitan statistical area/micropolitan statistical area",
  product = "population",
  year = "2016",
  geometry = T
)

city <- city %>% dplyr::filter(variable=="POP")
city <- city %>% dplyr::filter(!str_detect(NAME,"Micro Area"))
city <- city %>% dplyr::filter(!str_detect(NAME,"PR"))


city$NAME <- str_remove_all(city$NAME," Metro Area")

dat$centroid <- st_centroid(dat$geometry)
city$centroid <- st_centroid(city$geometry)
city2 <- city %>% dplyr::filter(value>1000000)


for (i in 1:nrow(dat)) {
  city$dist <- st_distance(dat$centroid[i],city$centroid) %>% t()
  city2$dist <- st_distance(dat$centroid[i],city2$centroid) %>% t()
  
  closest <- city$NAME[which(city$dist==min(city$dist))]
  closestbig <- city2$NAME[which(city2$dist==min(city2$dist))]
  
  dat$dist_to_city[i] <- city$dist[which(city$dist==min(city$dist))]
  dat$dist_to_citybig[i] <- city2$dist[which(city2$dist==min(city2$dist))]
  
  dat$MSA_pop[i] <- city$value[which(city$dist==min(city$dist))]
  dat$MSA_popbig[i] <- city2$value[which(city2$dist==min(city2$dist))]
  
  dat$closest_city[i] <- closest
  dat$closest_citybig[i] <- closestbig
}

dat$dist_to_city <- as.numeric(dat$dist_to_city)/1609.34
dat$dist_to_citybig <- as.numeric(dat$dist_to_citybig)/1609.34

############# climate damages #################
# source: http://www.impactlab.org/research/estimating-economic-damage-from-climate-change-in-the-united-states/

climate <- read.csv("~/location-preferences/county_damages_by_sector.csv")
names(climate) <- str_remove_all(names(climate),"\\.")
climate <- climate %>% mutate(climate_damage = Totaldamagescountyincome * CountyIncomein2012 * .01)

climate <- climate %>% dplyr::select(CountyFIPScode,Totaldamagescountyincome)
climate$CountyFIPScode <- ifelse(str_length(climate$CountyFIPScode)==4,paste0("0",climate$CountyFIPScode),climate$CountyFIPScode)
names(climate) <- c("GEOID","climate_damage")

dat <- left_join(dat,climate)


############ center everything ##################

dat$climate_damage_c <- (dat$climate_damage - mean(dat$climate_damage,na.rm=T))/sd(dat$climate_damage,na.rm=T)
dat$dist_to_city_c <- (dat$dist_to_city - mean(dat$dist_to_city,na.rm=T))/sd(dat$dist_to_city,na.rm=T)
dat$dist_to_citybig_c <- (dat$dist_to_citybig - mean(dat$dist_to_citybig,na.rm=T))/sd(dat$dist_to_citybig,na.rm=T)
dat$travel_median_c <- (dat$travel_median - mean(dat$travel_median,na.rm=T))/sd(dat$travel_median,na.rm=T)
dat$vax_c <- (dat$vax - mean(dat$vax,na.rm=T))/sd(dat$vax,na.rm=T)
dat$dem_prop_c <- (dat$dem_prop - mean(dat$dem_prop,na.rm=T))/sd(dat$dem_prop,na.rm=T)
dat$maxtemp_c <- (dat$maxtemp - mean(dat$maxtemp,na.rm=T))/sd(dat$maxtemp,na.rm=T)
dat$mintemp_c <- (dat$mintemp - mean(dat$mintemp,na.rm=T))/sd(dat$mintemp,na.rm=T)
dat$airpollution_c <- (dat$airpollution - mean(dat$airpollution,na.rm=T))/sd(dat$airpollution,na.rm=T)

dat$grad_c <- (dat$grad - mean(dat$grad,na.rm=T))/sd(dat$grad,na.rm=T)
dat$college_c <- (dat$college - mean(dat$college,na.rm=T))/sd(dat$college,na.rm=T)
dat$lessHS_c <- (dat$lessHS - mean(dat$lessHS,na.rm=T))/sd(dat$lessHS,na.rm=T)

dat$vcrimerate_c <- (dat$vcrimerate2 - mean(dat$vcrimerate2,na.rm=T))/sd(dat$vcrimerate2,na.rm=T)
dat$divorced_c <- (dat$divorced - mean(dat$divorced,na.rm=T))/sd(dat$divorced,na.rm=T)
dat$educ_c <- (dat$educ - mean(dat$educ,na.rm=T))/sd(dat$educ,na.rm=T)
dat$housing_cost_c <- (dat$housing_cost - mean(dat$housing_cost,na.rm=T))/sd(dat$housing_cost,na.rm=T)
dat$other_prop_c <- (dat$other_prop - mean(dat$other_prop,na.rm=T))/sd(dat$other_prop,na.rm=T)
dat$rep_prop_c <- (dat$rep_prop - mean(dat$rep_prop,na.rm=T))/sd(dat$rep_prop,na.rm=T)

dat$state <- str_remove_all(dat$NAME,".{0,50}, ")

dat$impute <- ifelse(is.na(dat$vcrimerate_c) | is.na(dat$educ_c) | is.na(dat$dem_prop_c) | is.na(dat$rep_prop_c),1,0)

######### impute crime values ###############

dat$NAME[which(is.na(dat$vcrimerate2))]

vreg <- lm(vcrimerate_c ~ vcrimerate + total + median_hh_inc + divorced_c + dist_to_citybig + dist_to_city + state,data=dat[which(!dat$state %in% c("Puerto Rico", "Florida")),])

dat$pred[which(!dat$state %in% c("Puerto Rico", "Florida"))] <- predict(vreg,dat[which(!dat$state %in% c("Puerto Rico", "Florida")),])

dat$vcrimerate_c <- ifelse(is.na(dat$vcrimerate_c),dat$pred,dat$vcrimerate_c)

ggplot(dat[which(!dat$state %in% c("Puerto Rico", "Alaska","Hawaii")),]) +
  geom_sf(aes(fill=vcrimerate_c),color=NA) +
  scale_fill_viridis_c()

######## use imputed values for missing education ########

ereg <- lm(educ_c ~  total + lessHS_c + dist_to_citybig + I(dist_to_citybig^2) + dist_to_city + median_hh_inc + vcrimerate_c,data=dat[which(!dat$state %in% c("Puerto Rico")),])
dat$pred[which(!dat$state %in% c("Puerto Rico"))] <- predict(ereg,dat[which(!dat$state %in% c("Puerto Rico")),])

ggplot(dat) +
  geom_point(aes(x=educ_c,pred)) +
  geom_smooth(aes(x=educ_c,pred),method="lm")

summary(ereg)
summary(dat$pred)

dat$educ_c <- ifelse(is.na(dat$educ_c),dat$pred,dat$educ_c)

########## use imputed values for dem_prop, rep_prop, other_prop ############

preg <- lm(dem_prop_c ~  total + lessHS_c + grad_c + college_c + dist_to_citybig + I(dist_to_citybig^2) + dist_to_city + median_hh_inc + vcrimerate_c,data=dat[which(!dat$state %in% c("Puerto Rico")),])
dat$pred[which(!dat$state %in% c("Puerto Rico"))] <- predict(preg,dat[which(!dat$state %in% c("Puerto Rico")),])

ggplot(dat) +
  geom_point(aes(x=dem_prop_c,pred)) +
  geom_smooth(aes(x=dem_prop_c,pred),method="lm")

summary(preg)
summary(dat$pred)

dat$dem_prop_c <- ifelse(is.na(dat$dem_prop_c),dat$pred,dat$dem_prop_c)

preg2 <- lm(rep_prop_c ~  state + total + lessHS_c + grad_c + college_c + dist_to_citybig + I(dist_to_citybig^2) + dist_to_city + median_hh_inc + vcrimerate_c,data=dat[which(!dat$state %in% c("Puerto Rico")),])
dat$pred[which(!dat$state %in% c("Puerto Rico"))] <- predict(preg2,dat[which(!dat$state %in% c("Puerto Rico")),])

ggplot(dat) +
  geom_point(aes(x=rep_prop_c,pred)) +
  geom_smooth(aes(x=rep_prop_c,pred),method="lm")

summary(preg2)
summary(dat$pred)

dat$rep_prop_c <- ifelse(is.na(dat$rep_prop_c),dat$pred,dat$rep_prop_c)

###################


forcor <- dat %>% as.data.frame() %>% dplyr::select(-geometry,-centroid)

res <- cor(forcor[,c(6,47:57)],use="complete.obs")


corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

summary(lm(housing_cost ~ dist_to_city + travel_median,data=dat[which(!str_detect(dat$NAME,"Alaska|Hawaii|Puerto Rico")),]))

ggplot(dat) + 
  geom_point(aes(x=travel_median_c,y=housing_cost)) + 
  geom_smooth(aes(x=travel_median_c,y=housing_cost))


summary(lm(housing_cost ~ climate_damage_c + total + travel_median_c + dist_to_city_c + dist_to_citybig_c + dem_prop_c + educ_c + vax_c + maxtemp_c + mintemp_c + airpollution_c + vcrimerate_c,data=dat[which(!str_detect(dat$NAME,"Alaska|Hawaii|Puerto Rico")),]))

ggplot(dat[which(!str_detect(dat$NAME,"Alaska|Hawaii|Puerto Rico")),]) +
  geom_sf(aes(fill=dem_prop*vax),color=NA) +
  scale_fill_viridis_c()

county <- dat  %>%  dplyr::select(GEOID,NAME,
                                  housing_cost,housing_cost_c,
                                  climate_damage,climate_damage_c,
                                  dist_to_city,dist_to_city_c,
                                  dist_to_citybig,dist_to_citybig_c,
                                  dem_prop,dem_prop_c,
                                  rep_prop,rep_prop_c,
                                  other_prop,other_prop_c,
                                  educ,educ_c,
                                  maxtemp,maxtemp_c,
                                  mintemp,mintemp_c,
                                  airpollution,airpollution_c,
                                  vcrimerate,vcrimerate_c,
                                  vax,vax_c,
                                  travel_median,travel_median_c,
                                  grad,grad_c,
                                  divorced,divorced_c,
                                  closest_city,
                                  MSA_pop,
                                  closest_citybig,
                                  MSA_popbig,
                                  impute,
                                  geometry
)

county$state_name <- str_extract(county$NAME,", .{1,30}") %>% str_remove_all(", ")
county$state_abb <- state.abb[match(county$state_name,state.name)]


saveRDS(county, file = "~/location-preferences/cleaned_data.rds")


test <- readRDS("~/location-preferences/cleaned_data.rds")

ggplot(test[which(!str_detect(test$NAME,"Alaska|Hawaii|Puerto Rico")),]) +
  geom_sf(aes(fill=dem_prop_c),color=NA) +
  scale_fill_viridis_c() +
  labs(fill="Score") +
  theme_map()
