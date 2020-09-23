require(pacman)

p_load(tidyverse,tidycensus)

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
                       geometry=F,                
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
