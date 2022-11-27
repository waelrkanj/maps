
install.packages("tidyverse",dependencies = TRUE)
install.packages("tidycensus",dependencies = TRUE)
install.packages("rgdal",dependencies=TRUE)
install.packages("srvyr",dependencies=TRUE)
install.packages("sf",dependencies=TRUE)
install.packages("kableExtra",dependencies=TRUE)
install.packages("tigris")
install.packages("crsuggest",dependencies=TRUE)
install.packages("mapview",dependencies=TRUE)
install.packages("arsenal")

#https://nycgeo.mattherman.info/
install.packages("remotes")
remotes::install_github("mfherman/nycgeo")

library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(tigris)
library(crsuggest)
library(mapview)
library(rgdal)
library(arsenal)
library(nycgeo)

options(scipen=999)
options(tigris_class = "sf")

dir <- "Urban Informatics/Final Project - Circular Economy"

setwd(dir)


census_api_key("")

#https://api.census.gov/data/2019/acs/acs5/variables.html

#Pull data by tract level
nyc19acs_geo_tract <- get_acs(geography = "tract", 
                        state="NY",
                        county=c("Kings","Queens","Bronx","New York"),
                        variables = c("B01003_001E","B02001_002E","B02001_003E",
                                      "B01002_001","B02001_008E","B19013_001","B17020_002E",
                                      "B23025_001","B23025_002","B19301_001","B06009_001E","B06009_002E","B06009_003E",
                                      "B06009_004E","B06009_005E","B06009_006E"), 
                        year = 2019,
                        output="wide",geometry=TRUE)


nyc19acs_geo_format <- nyc19acs_geo_tract %>%
  select(GEOID,NAME,TotalPopulation = B01003_001E,
         TotalPopulation_WhiteAlone = B02001_002E,
         TotalPopulation_BlackAlone = B02001_003E,
         TotalPopulation_TwoMoreRaces = B02001_008E,
         Population_BelowPovertyLevel = B17020_002E,
         MedianHouseholdIncome = B19013_001E,
         PerCapitaIncome = B19301_001E,
         MedianAge = B01002_001E,
         TotalEducation = B06009_001E,
         LessthanHS = B06009_002E,
         HSGrad = B06009_003E,
         SomeCollege = B06009_004E,
         CollegeGrad = B06009_005E,
         GraduateGrad = B06009_006E) %>%
  separate(NAME,c("Census Tract","County","State"),",") %>%
  mutate(PercentPopPoverty = Population_BelowPovertyLevel/TotalPopulation,
         PercentNonWhite = 1- TotalPopulation_WhiteAlone/TotalPopulation,
         PercentHSEducated = 1 - LessthanHS/TotalEducation,
         PercentCollegeEducation = CollegeGrad/TotalEducation + GraduateGrad/TotalEducation,
         Borough = case_when(County == " Kings County" ~ "Brooklyn",
                             County == " New York County" ~ "Manhattan",
                             County == " Queens County" ~ "Queens",
                             County == " Bronx County" ~ "Bronx"))



#Correlation analysis
temp <- brooklyn_census %>%
  select(PercentPopPoverty,PercentNonWhite,PercentCollegeEducation) %>%
  st_set_geometry(NULL)

brooklyn_cor <- cor(temp ,use = "complete.obs")

write.csv(brooklyn_cor,file="brooklyn_correlation_matrix.csv")

rm(temp)
rm(brooklyn_cor)

temp <- manhattan_census %>%
  select(PercentPopPoverty,PercentNonWhite,PercentCollegeEducation) %>%
  st_set_geometry(NULL)

manhattan_cor <- cor(temp ,use = "complete.obs")

write.csv(manhattan_cor,file="manhattan_correlation_matrix.csv")

nyc19acs_geo_format

temp <- nyc19acs_geo_format %>%
  select(PercentPopPoverty,PercentNonWhite,PercentCollegeEducation) %>%
  st_set_geometry(NULL)

nyc_cor <- cor(temp ,use = "complete.obs")

write.csv(nyc_cor,file="nyc_cor.csv")

rm(nyc_cor)
rm(manhattan_cor)
rm(brooklyn_cor)
rm(temp)


##### Drop off locations import and format ####
electronics_drop_off_locations <- read.csv("Electronics_Drop_Off_Locations_in_NYC.csv",stringsAsFactors = FALSE)

electronics_drop_off_locations <- electronics_drop_off_locations %>%
  select(Site_Name = DropOff_SiteName,Latitude,Longitude,Address) %>%
  mutate(type="Electronics")


foodscrap_drop_off_locations <- read.csv("Food_Scrap_Drop-Off_Locations_in_NYC.csv",stringsAsFactors = FALSE)

foodscrap_drop_off_locations <- foodscrap_drop_off_locations %>%
  select(Site_Name = Food_Scrap_Drop_Off_Site_Name,Latitude,Longitude,Address = Location) %>%
  mutate(type="FoodScraps")



leaf_drop_off_locations <- read.csv("Leaf_Drop-Off_Locations_in_NYC.csv",stringsAsFactors = FALSE)

leaf_drop_off_locations <- leaf_drop_off_locations %>%
  select(Site_Name,Latitude,Longitude,Address) %>%
  mutate(type="Leaves")



public_recycling_bin_locations <- read.csv("Public_Recycling_Bins.csv",stringsAsFactors = FALSE)
names(public_recycling_bin_locations) = c("Borough","Site_type","ParkSiteName","Address","Latitude","Longitude")


public_recycling_bin_locations <- public_recycling_bin_locations %>%
  unite(Site_Name,c(ParkSiteName,Site_type),sep="_") %>%
  select(Site_Name,Latitude,Longitude,Address) %>%
  mutate(type="RecyclingBin")



textile_drop_off_locations <- read.csv("Textile_Drop-Off_Locations_in_NYC.csv",stringsAsFactors = FALSE)

textile_drop_off_locations$Operations_Type<-gsub("'","",textile_drop_off_locations$Operations_Type)


textile_drop_off_locations <- textile_drop_off_locations %>%
  mutate(Operations_Type=recode(Operations_Type, 
                       'Nonprofit Organizations/Thrift Stores'='Nonprofit/Thrift', 
                       'Publicly Accessible Donation Bins'='PublicBin',
                       'Retail Store Takeback Programs' = 'RetailTakeback',
                       'Textile Recycling at Farmers Markets' = 'FarmersMarket')) %>%
  unite(Site_Name,c(Vendor_Name,Operations_Type),sep="_") %>% 
  select(Site_Name,Latitude,Longitude,Address) %>%
  mutate(type="Textiles")


#Append all location data
#Remove leaf drop data as it's underutilized

all_locations <- rbind(electronics_drop_off_locations,
                       foodscrap_drop_off_locations,
                       #leaf_drop_off_locations,
                      # public_recycling_bin_locations,
                       textile_drop_off_locations)

#REMOVE ROWS WITH MISSING LOCATION DATA
all_locations_complete <- na.omit(all_locations)

#CONVERT LOCATIONS DATAFRAME TO DATASET OF GEOMETRY TYPE POINT 
locations_sf <- all_locations_complete %>%
  st_as_sf(coords=c("Longitude","Latitude"),crs=4326)

#RESET CENSUS  AND LOCATIONS TO COORDINATE REFERENCE SYSTEM WGS 1984

st_crs(nyc19acs_geo_format) <- 4326
st_crs(locations_sf) <- 4326


#Create subgroup location dataframes, join to census data
foodlocations_sf <- locations_sf %>% filter(type=="FoodScraps")
foodlocations_joined <- st_join(foodlocations_sf,nyc19acs_geo_format)


electronicslocations_sf <- locations_sf %>% filter(type=="Electronics")
electronicslocations_joined <- st_join(electronicslocations_sf,nyc19acs_geo_format)

textilelocations_sf <- locations_sf %>% filter(type=="Textiles")
textilelocations_joined <- st_join(textilelocations_sf,nyc19acs_geo_format)

leaflocations_sf <- locations_sf %>% filter(type=="Leaves")
leaflocations_joined <- st_join(leaflocations_sf,nyc19acs_geo_format)


#### Analyze distribution/characteristics of census tracts which include drop off locations, by county ####

#Pull distinct tracts for each drop off location type
food_tracts <- foodlocations_joined %>%
  distinct(GEOID) %>%
  left_join(nyc19acs_geo_format)


#### Visualize census tract characteristics for food locations
manhattan_food_tracts <- food_tracts %>%
  filter(Borough == "Manhattan")



electronics_tracts <- electronicslocations_joined %>%
  distinct(GEOID) %>%
  left_join(nyc19acs_geo_format)

leaf_tracts <- leaflocations_joined %>%
  distinct(GEOID) %>%
  left_join(nyc19acs_geo_format)

textile_tracts <- textilelocations_joined %>%
  distinct(GEOID) %>%
  left_join(nyc19acs_geo_format)

# Calculate averages and medians for each County/drop off point combination

food_tract_summary <- food_tracts %>%
  group_by(County) %>%
  summarize(tracts = n(),
            PercentNonWhiteMean = mean(PercentNonWhite, na.rm=TRUE),
            PercentNonWhiteMedian = median(PercentNonWhite, na.rm=TRUE),
            PercentPovertyMean = mean(PercentPopPoverty, na.rm=TRUE),
            PercentPovertyMedian = median(PercentPopPoverty, na.rm=TRUE),
            PerCapitaIncomeMean = mean(PerCapitaIncome, na.rm=TRUE),
            PerCapitaIncomeMedian = median(PerCapitaIncome, na.rm=TRUE))

  
electronics_tract_summary <- electronics_tracts %>%
  group_by(County) %>%
  summarize(tracts = n(),
            PercentNonWhiteMean = mean(PercentNonWhite, na.rm=TRUE),
            PercentNonWhiteMedian = median(PercentNonWhite, na.rm=TRUE),
            PercentPovertyMean = mean(PercentPopPoverty, na.rm=TRUE),
            PercentPovertyMedian = median(PercentPopPoverty, na.rm=TRUE),
            PerCapitaIncomeMean = mean(PerCapitaIncome, na.rm=TRUE),
            PerCapitaIncomeMedian = median(PerCapitaIncome, na.rm=TRUE))

leaf_tract_summary <- leaf_tracts %>%
  group_by(County) %>%
  summarize(tracts = n(),
            PercentNonWhiteMean = mean(PercentNonWhite, na.rm=TRUE),
            PercentNonWhiteMedian = median(PercentNonWhite, na.rm=TRUE),
            PercentPovertyMean = mean(PercentPopPoverty, na.rm=TRUE),
            PercentPovertyMedian = median(PercentPopPoverty, na.rm=TRUE),
            PerCapitaIncomeMean = mean(PerCapitaIncome, na.rm=TRUE),
            PerCapitaIncomeMedian = median(PerCapitaIncome, na.rm=TRUE))


textile_tract_summary <- textile_tracts %>%
  group_by(County) %>%
  summarize(tracts = n(),
            PercentNonWhiteMean = mean(PercentNonWhite, na.rm=TRUE),
            PercentNonWhiteMedian = median(PercentNonWhite, na.rm=TRUE),
            PercentPovertyMean = mean(PercentPopPoverty, na.rm=TRUE),
            PercentPovertyMedian = median(PercentPopPoverty, na.rm=TRUE),
            PerCapitaIncomeMean = mean(PerCapitaIncome, na.rm=TRUE),
            PerCapitaIncomeMedian = median(PerCapitaIncome, na.rm=TRUE))

#Calculate values overall for all census tracts in each County
county_summary <- nyc19acs_geo_format %>%
  group_by(County) %>%
  summarize(tracts = n(),
            PercentNonWhiteMean = mean(PercentNonWhite, na.rm=TRUE),
            PercentNonWhiteMedian = median(PercentNonWhite, na.rm=TRUE),
            PercentPovertyMean = mean(PercentPopPoverty, na.rm=TRUE),
            PercentPovertyMedian = median(PercentPopPoverty, na.rm=TRUE),
            PerCapitaIncomeMean = mean(PerCapitaIncome, na.rm=TRUE),
            PerCapitaIncomeMedian = median(PerCapitaIncome, na.rm=TRUE))
  

#Validate county summary with county level data

nyc19acs_county <- get_acs(geography = "county", 
                              state="NY",
                              county=c("Kings","Queens","Bronx","New York"),
                              variables = c("B01003_001E","B02001_002E","B02001_003E",
                                            "B01002_001","B02001_008E","B19013_001","B17020_002E",
                                            "B23025_001","B23025_002","B19301_001"), 
                              year = 2019,
                              output="wide",geometry=TRUE)

nyc19acs_county_format <- nyc19acs_county %>%
  select(GEOID,NAME,TotalPopulation = B01003_001E,
         TotalPopulation_WhiteAlone = B02001_002E,
         TotalPopulation_BlackAlone = B02001_003E,
         TotalPopulation_TwoMoreRaces = B02001_008E,
         Population_BelowPovertyLevel = B17020_002E,
         MedianHouseholdIncome = B19013_001E,
         PerCapitaIncome = B19301_001E,
         MedianAge = B01002_001E) %>%
  mutate(PercentPopPoverty = Population_BelowPovertyLevel/TotalPopulation,
         PercentNonWhite = 1- TotalPopulation_WhiteAlone/TotalPopulation) %>%
  select(NAME,PercentNonWhite,PercentPopPoverty,PerCapitaIncome)


#### Analyze distribution/characteristics of neighborhoods which include drop off locations, by county ####

#https://www1.nyc.gov/site/planning/data-maps/open-data/census-download-metadata.page
#Mapping Neighborhood tabulation areas to census tracts

#https://nycgeo.mattherman.info/articles/nycgeo.html#joining-with-other-data


nyc2020census_tract_nta_cdta_relationships <- read.csv("nyc2020census_tract_nta_cdta_relationships.csv",stringsAsFactors = FALSE)

tract_cdta_mapping <- nyc2020census_tract_nta_cdta_relationships %>%
  select(GEOID_double=GEOID,CDTAName) %>%
  mutate(GEOID = as.character(GEOID_double)) %>%
  select(-GEOID_double)

food_locations_

locations_joined <- st_join(locations_sf,nyc19acs_geo_format)


foodlocations_cdta <- nyc_point_poly(foodlocations_joined,"cd") %>%
  st_set_geometry(NULL) %>%


cdta_boundaries <- nyc_boundaries(geography="cd",
                                  filter_by = "borough",
                                  region = c("manhattan","queens","brooklyn","bronx"))

st_crs(foodlocations_joined) <- 4326
st_crs(cdta_boundaries) <- 4326


foodlocations_cdta <- nyc_point_poly(foodlocations_joined,"cd") %>%
  st_set_geometry(NULL) %>%
  group_by(cd_name)



##### Distance Analysis (Distance from census tracts to drop off locations) ####


#Calculate distance between each census centroid and a drop off point
dist_food <- nyc19acs_geo_format %>%
  st_centroid() %>%
  st_distance(foodlocations_sf)

dist_electronics <- nyc19acs_geo_format %>%
  st_centroid() %>%
  st_distance(electronicslocations_sf)

dist_textile <- nyc19acs_geo_format %>%
  st_centroid() %>%
  st_distance(textilelocations_sf)


#Distance from nearest location to each census tract, in kilometers
min_dist_electronics <- dist_electronics %>%
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

min_dist_food <- dist_food %>%
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

min_dist_textile <- dist_textile %>%
  apply(1, min) %>%
  as.vector() %>%
  magrittr::divide_by(1000)

hist(min_dist_food)
hist(min_dist_electronics)
hist(min_dist_textile)

##### Buffer zones (250m) Interpolation Analysis #####

#probably change these all to 500m? we'll see
#Create 250m buffer zones around each location
buff350m_food <- st_buffer(foodlocations_sf,dist=350)
buff350m_electronics <- st_buffer(electronicslocations_sf,dist=350)
buff350m_textile <- st_buffer(textilelocations_sf,dist=350)


#Map the buffer zones along with some census data
mapview(buff350m_electronics) + mapview(nyc19acs_geo_format,zcol="PerCapitaIncome")
mapview(buff350m_food) + mapview(nyc19acs_geo_format,zcol="PerCapitaIncome") 
mapview(buff350m_textile) + mapview(nyc19acs_geo_format,zcol="PerCapitaIncome") 

#Interpolation of location data and census metrics
manhattan_census_data_metrics <- nyc19acs_geo_format %>%
  filter(County == " New York County") %>%
  select(5:19)

bronx_census_data_metrics <- nyc19acs_geo_format %>%
  filter(County == " Bronx County") %>%
  select(5:19)

queens_census_data_metrics <- nyc19acs_geo_format %>%
  filter(County == " Queens County") %>%
  select(5:19)

brooklyn_census_data_metrics <- nyc19acs_geo_format %>%
  filter(County == " Kings County") %>%
  select(5:19)

#Food locations
buffer_interpolation_food_manhattan <- st_interpolate_aw(
  manhattan_census_data_metrics,
  buff350m_food,
  extensive=TRUE) %>%
  mutate(category="Food_Manhattan")

buffer_interpolation_food_manhattan_viz <- buffer_interpolation_food_manhattan %>%
mutate(PercentPopPoverty = Population_BelowPovertyLevel/TotalPopulation,
       PercentNonWhite = 1- TotalPopulation_WhiteAlone/TotalPopulation,
       PercentHSEducated = 1 - LessthanHS/TotalEducation,
       PercentCollegeEducation = CollegeGrad/TotalEducation + GraduateGrad/TotalEducation) 

ggplot(buffer_interpolation_food_manhattan_viz, aes(x=PercentPopPoverty, y = PercentCollegeEducation)) + 
  geom_point(size=2, color="blue") + theme_classic() + 
  labs(title="Food Drop Off Locations in Manhattan (350m buffers)",
       x="Percent of Population below Poverty Level", y = "Percent of Population with College Education") + 
  scale_y_continuous(limit = c(0, 1)) + scale_x_continuous(limit=c(0,1))



buffer_interpolation_food_bronx <- st_interpolate_aw(
  bronx_census_data_metrics,
  buff350m_food,
  extensive=TRUE) %>%
  mutate(category="Food_Bronx")

buffer_interpolation_food_queens <- st_interpolate_aw(
  queens_census_data_metrics,
  buff350m_food,
  extensive=TRUE) %>%
  mutate(category="Food_Queens")

buffer_interpolation_food_brooklyn <- st_interpolate_aw(
  brooklyn_census_data_metrics,
  buff350m_food,
  extensive=TRUE) %>%
  mutate(category="Food_Brooklyn")

#Electronics locations
buffer_interpolation_electronics_manhattan <- st_interpolate_aw(
  manhattan_census_data_metrics,
  buff350m_electronics,
  extensive=TRUE) %>%
  mutate(category="Electronics_Manhattan")

buffer_interpolation_electronics_bronx <- st_interpolate_aw(
  bronx_census_data_metrics,
  buff350m_electronics,
  extensive=TRUE) %>%
  mutate(category="Electronics_Bronx")

buffer_interpolation_electronics_queens <- st_interpolate_aw(
  queens_census_data_metrics,
  buff350m_electronics,
  extensive=TRUE) %>%
  mutate(category="Electronics_Queens")

buffer_interpolation_electronics_brooklyn <- st_interpolate_aw(
  brooklyn_census_data_metrics,
  buff350m_electronics,
  extensive=TRUE) %>%
  mutate(category="Electronics_Brooklyn")


#Textile Locations
buffer_interpolation_textile_manhattan <- st_interpolate_aw(
  manhattan_census_data_metrics,
  buff350m_textile,
  extensive=TRUE) %>%
  mutate(category="Textile_Manhattan")

buffer_interpolation_textile_bronx <- st_interpolate_aw(
  bronx_census_data_metrics,
  buff350m_textile,
  extensive=TRUE) %>%
  mutate(category="Textile_Bronx")

buffer_interpolation_textile_queens <- st_interpolate_aw(
  queens_census_data_metrics,
  buff350m_textile,
  extensive=TRUE) %>%
  mutate(category="Textile_Queens")

buffer_interpolation_textile_brooklyn <- st_interpolate_aw(
  brooklyn_census_data_metrics,
  buff350m_textile,
  extensive=TRUE) %>%
  mutate(category="Textile_Brooklyn")
  
buffer_interpolation_all <- rbind(buffer_interpolation_textile_manhattan,
                            buffer_interpolation_textile_bronx,
                            buffer_interpolation_textile_queens,
                            buffer_interpolation_textile_brooklyn,
                            buffer_interpolation_food_manhattan,
                            buffer_interpolation_food_bronx,
                            buffer_interpolation_food_queens,
                            buffer_interpolation_food_brooklyn,
                            buffer_interpolation_electronics_manhattan,
                            buffer_interpolation_electronics_bronx,
                            buffer_interpolation_electronics_queens,
                            buffer_interpolation_electronics_brooklyn) %>%
  
  st_set_geometry(NULL) %>%
  mutate(PercentPopPoverty = Population_BelowPovertyLevel/TotalPopulation,
         PercentNonWhite = 1- TotalPopulation_WhiteAlone/TotalPopulation,
         PercentHSEducated = 1 - LessthanHS/TotalEducation,
         PercentCollegeEducation = CollegeGrad/TotalEducation + GraduateGrad/TotalEducation) 


x <- buffer_interpolation_all %>% group_by(category) %>% summarize(n())

#Create table
buffer_interpolation_350m_summary <- tableby(category ~ ., data = buffer_interpolation_all,
                                             numeric.stats=c("median","mean")) 


summary(buffer_interpolation_250m_summary, title = "Census Buffer Data",text=TRUE)


buffer_interpolation_350m_summary_df <- as.data.frame(buffer_interpolation_350m_summary)


buffer_interpolation_350m_means <- buffer_interpolation_350m_summary_df %>%
  filter(term=="mean" | term=="median") %>%
  filter(variable == "PercentPopPoverty" | variable == "PercentNonWhite" | variable == "PercentHSEducated" | variable == "PercentCollegeEducation" ) %>%
  select(-c("group.term","group.label","strata.term","label","variable.type","Total","test","p.value")) %>%
  pivot_longer(cols=-c("variable","term"),names_to="category",values_to = "value") %>%
  mutate(value_new = str_remove_all(value, "c[(]`50%` = ")) %>%
  select(-value) %>%
  pivot_wider(names_from = term,values_from = value_new) %>%
  separate(category,c("Category","County"),sep="_")

buffer_interpolation_350m_means <- apply(buffer_interpolation_350m_means,2,as.character)


write.csv(buffer_interpolation_350m_means,file="buffer_interpolation_350m_means.csv")

#County Summary
county_summary <- nyc19acs_geo_format %>%
  group_by(County) %>%
  summarize(tracts = n(),
            PercentNonWhite_Mean = mean(PercentNonWhite, na.rm=TRUE),
            PercentNonWhite_Median = median(PercentNonWhite, na.rm=TRUE),
            PercentPoverty_Mean = mean(PercentPopPoverty, na.rm=TRUE),
            PercentPoverty_Median = median(PercentPopPoverty, na.rm=TRUE),
            PercentHSEducated_Mean = mean(PercentHSEducated, na.rm=TRUE),
            PercentHSEducated_Median = median(PercentHSEducated, na.rm=TRUE),
            PercentCollegeEducation_Mean = mean(PercentCollegeEducation,na.rm=TRUE),
            PercentCollegeEducation_Median = median(PercentCollegeEducation,na.rm=TRUE))

county_summary_format <- county_summary %>%
  st_set_geometry(NULL) %>%
  select(-tracts) %>%
  pivot_longer(-County,
               names_to=c("variable","term"),
               names_sep = "_",
               values_to="value") %>%
  pivot_wider(names_from=term,values_from = value)

write.csv(buffer_interpolation_250m_means,file="buffer_interpolation_250m_means.csv")
write.csv(county_summary_format,file="county_summary_format.csv")



#Plot county summary stats

county_summary_viz <- county_summary_format %>%
  select(County,variable,Median) %>%
  pivot_wider(names_from = variable,values_from = Median) %>%
  mutate(Borough = case_when(County == " Kings County" ~ "Brooklyn",
                             County == " New York County" ~ "Manhattan",
                             County == " Queens County" ~ "Queens",
                             County == " Bronx County" ~ "Bronx"))


ggplot(county_summary_viz, aes(x=PercentPoverty, y = PercentCollegeEducation)) + 
  geom_point(size=5, color="blue") + theme_classic() + 
  labs(title="New York City Boroughs",
       x="Percent of Population below Poverty Level", y = "Percent of Population with College Education") + 
  scale_y_continuous(limit = c(0, 1)) + scale_x_continuous(limit=c(0,1)) + 
  geom_text(aes(label = Borough), nudge_y = .05)


#####

#####


nyc_crs <- suggest_crs(nyc19acs_geo)


##### Geomarketing/Location Analysis - Manhattan Only #####

#Criteria:
#Population density > median for manhattan
#PercentPoverty > median for manhattan
#PercentNonWhite > Median for manhattan
#PercentCollege Educated < Median for Manhattan
#Buffer existing food locations > 350M

manhattan_census_data <- nyc19acs_geo_format %>%
  filter(County == " New York County")

manhattan_census_data$area <- manhattan_census_data %>% st_area()

#Have the buffer files
#buff250m_electronics
#buff250m_food
#buff250m_textile


#select tracts where demographic characteristics: 
#population density,percent poverty, percent nonwhite

tractAttributes <- manhattan_census_data %>%
  mutate(PopDensity = TotalPopulation/area,
         medPopDensity = median(PopDensity,na.rm=TRUE),
         density_ind = if_else(PopDensity>=median(PopDensity,na.rm=TRUE),1,0),
         
         medPoverty = median(PercentPopPoverty,na.rm=TRUE),
         poverty_ind = if_else(PercentPopPoverty >= median(PercentPopPoverty,na.rm=TRUE),1,0),
         
         medNonWhite = median(PercentNonWhite,na.rm=TRUE),
         nonwhite_ind = if_else(PercentNonWhite >= median(PercentNonWhite,na.rm=TRUE),1,0),
         
         medCollegeEducated = median(PercentCollegeEducation,na.rm = TRUE),
         education_ind = if_else(PercentCollegeEducation < median(PercentCollegeEducation,na.rm = TRUE),1,0),
         Score=density_ind + poverty_ind + nonwhite_ind + education_ind)


#Food Location Optimization
dissolveFoodBuffers <- buff350m_food %>%
  group_by(type) %>%
  summarize(first(type))

plot(dissolveFoodBuffers[,1])

sf_use_s2(FALSE)

st_crs(dissolveFoodBuffers) <- 4326
st_crs(tractAttributes) <- 4326


tractsNotFoodBuffer <- st_disjoint(dissolveFoodBuffers,tractAttributes,sparse=F)
FoodLocationOptimization <- tractAttributes[tractsNotFoodBuffer,]

plot(foodFinalSelection[,35])

#Electronics Location Optimization
dissolveElectronicsBuffers <- buff350m_electronics %>%
  group_by(type) %>%
  summarize(first(type))

sf_use_s2(FALSE)

st_crs(dissolveElectronicsBuffers) <- 4326

tractsNotElectronicsBuffer <- st_disjoint(dissolveElectronicsBuffers,tractAttributes,sparse=F)
ElectronicsLocationOptimization <- tractAttributes[tractsNotElectronicsBuffer,]

plot(electronicsFinalSelection[,35])


#Textile Location Optimization
dissolveTextileBuffers <- buff350m_textile %>%
  group_by(type) %>%
  summarize(first(type))

sf_use_s2(FALSE)

st_crs(dissolveTextileBuffers) <- 4326

tractsNotTextileBuffer <- st_disjoint(dissolveTextileBuffers,tractAttributes,sparse=F)
TextilesLocationOptimization <- tractAttributes[tractsNotTextileBuffer,]

#Create mapping plots
plot(textileFinalSelection[,35])



mapview(buffer_interpolation_textile_manhattan,col.regions="red",legend=FALSE) + mapview(TextilesLocationOptimization,zcol="Score")

mapview(buffer_interpolation_food_manhattan,col.regions="red",legend=FALSE) + mapview(FoodLocationOptimization,zcol="Score")

mapview(buffer_interpolation_electronics_manhattan,col.regions="red",legend=FALSE) + mapview(ElectronicsLocationOptimization,zcol="Score")


plot <- ggplot() + geom_sf(mapping=aes(fill=Score),data=finalSelection) + theme(panel.grid.minor = element_blank(),
                                                                                panel.background = element_blank(), axis.line = element_line(colour = "black")) +scale_fill_viridis_c()


plot

mapview(finalSelection,zcol="Score")








#####