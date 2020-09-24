require(pacman)

p_load(tidyverse,tidycensus,sf)

# make a dataframe with county level variables

vars <- load_variables(year=2018,dataset="acs5")


census_data <- get_acs(geography = "county",
                       year=2018,
                       variables=c(
                         total="B01003_001",
                         median_hh_inc = "B19013_001",
                         hh_size= "B08202_001",
                         housing_cost = "B25105_001",
                         renters = "B07013_003"), 
                       geometry=T,                
                       shift_geo=F,
                       output="wide")

census_data <- census_data %>% dplyr::select(which(str_detect(names(census_data),"E")))
names(census_data) <- str_remove_all(names(census_data),"E$")
names(census_data)[2] <- "NAME"


######## politics ##########

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

health <- read.csv("~/location-preferences/analytic_data2020_0.csv",skip=1)
health$vcrimerate <- health$v043_rawvalue
health$airpollution <- health$v125_rawvalue
health$waterpollution <- health$v124_rawvalue
health$educ <- health$v021_rawvalue
health$lifeexp <- health$v147_rawvalue
health$vax <- health$v155_rawvalue #higher values are good


health2 <- health %>% dplyr::select(fipscode,vcrimerate,vax,lifeexp,airpollution,educ)
health2$fipscode <- ifelse(str_length(health2$fipscode)==4,paste0("0",health2$fipscode),health2$fipscode)

dat <- left_join(census_data,health2,by=c("GEOID"="fipscode"))
dat <- left_join(dat,votes,by=c("GEOID"="FIPS"))

summary(lm(lifeexp ~ housing_cost + vcrimerate + airpollution + dem_prop + other_prop + vax + median_hh_inc + educ,data=dat))

######## urbanicity ##########

urban <- read.csv("~/location-preferences/urbanicity.csv")
names(urban)[1] <- "FIPS"
urban <- urban %>% dplyr::select(FIPS,RUCC_2013,Description)
urban$FIPS <- as.character(urban$FIPS)
 
dat <- left_join(dat,urban,by=c("GEOID"="FIPS"))
summary(lm(lifeexp ~ housing_cost + vcrimerate + airpollution + rep_prop + vax + median_hh_inc + educ + factor(RUCC_2013),data=dat))
summary(lm(lifeexp ~ factor(RUCC_2013),data=dat))

######## max temperature ############

# noaa doesn't use regular state fips, but does use county fips, go figure
# set up a rosetta stone between noaa codes/fips codes
statecodes <- read.table("~/location-preferences/noaa_state_codes.txt",sep="\t",colClasses = "character")
statecodes$V1 <- trimws(statecodes$V1)
statecodes$V2 <- trimws(statecodes$V2)
statecodes2 <- fips_codes %>% dplyr::select(state_code,state_name)
statecodes <- left_join(statecodes,statecodes2,by=c("V2"="state_name"))
statecodes <- unique(statecodes)
names(statecodes) <- c("noaastate","statename","statefips")

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


dat <- dat %>% mutate(u = -housing_cost - maxtemp - (15*rep_prop)^2 + 1000*vax - vcrimerate - 10*airpollution + 20*educ + 100*I(RUCC_2013==6) + 50*I(RUCC_2013==4))
dat[which(dat$u==max(dat$u,na.rm=T)),]
summary(dat$u)

ggplot(dat) +
  geom_sf(aes(fill=u),color=NA) +
  scale_fill_viridis_c()


######### city MSAs for distance calc ##################


city <- get_estimates(
  geography = "metropolitan statistical area/micropolitan statistical area",
  product = "population",
  year = "2016",
  geometry = T
)

city <- city %>% dplyr::filter(variable=="POP")
city <- city %>% dplyr::filter(!str_detect(NAME,"Micro Area"))
city <- city %>% dplyr::filter(value>1000000)

dat$centroid <- st_centroid(dat$geometry)
city$centroid <- st_centroid(city$geometry)

for (i in 1:nrow(dat)) {
  dat$dist_to_city[i] <- min(st_distance(dat$centroid[i],city$centroid))
}


ggplot(city) +
  geom_sf(data=dat[which(!str_detect(dat$NAME,"Alaska|Hawaii|Puerto Rico")),]) +
  geom_sf(aes(fill=value))

ggplot(dat[which(!str_detect(dat$NAME,"Alaska|Hawaii|Puerto")),]) +
  geom_sf(aes(fill=dist_to_city),color=NA) +
  scale_fill_viridis_c()
