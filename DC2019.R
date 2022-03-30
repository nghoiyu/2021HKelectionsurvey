#clear the environment
rm(list=ls())

#Set working directory
setwd("~/Dropbox/Data Representation and Visualization in Tableau/Visualization dataset")

#Load the necessary packages
library(sf)
library(ggplot2)
library(rgdal)
library(raster)
library(tidyverse)
library(ggmap)
library(tmap) 
library(sqldf)

#Load and read the PORI's survey data

#Import the 1st telephone survey
PORI_1 <- read.csv(file = 'LC2021R1_15112021-18112021_tel_dataset_pori_v1.csv')
head(PORI_1)

#Import the 2nd telephone survey
PORI_2<- read.csv(file = 'LC2021R4_29112021-03122021_tel_dataset_pori_v1.csv')
head(PORI_2)

#Import the 3rd telephone survey
PORI_3 <- read.csv(file = 'LC2021R6_09122021-14122021_online_dataset_pori_v1.csv')
head(PORI_3)

##Create a new variable for each data set to indicate the order of surveys
PORI_1 <- PORI_1 %>% add_column(survey_no = 1)
PORI_2 <- PORI_2 %>% add_column(survey_no = 2)
PORI_3 <- PORI_3 %>% add_column(survey_no = 3)

#Subset the data sets by selecting relevant variables
#Use SQL language to select necessary variables

#PORI_1
PORI_1 <- sqldf('SELECT surveyCaseid, Weight, A003_ratinggp, V1, 
V3, WillVote, WillVotegp, OwnDistrictAny, District10_2, sex, pobgp,
agegp_TP_7, edugp_TP, v4_2019, inclin, midgp, marital, civicactive, survey_no FROM PORI_1')

#PORI_2  
PORI_2 <- sqldf('SELECT surveyCaseid, Weight, A003_ratinggp, V1, 
V3, WillVote, WillVotegp, District10_2, SupportAny, sex, pobgp,
agegp_TP_7, edugp_TP, v4_2019, inclin, midgp, marital, civicactive, survey_no FROM PORI_2')

#PORI_3   
PORI_3 <- sqldf('SELECT surveyCaseid, Weight, A003_ratinggp, V1, 
V3, WillVote, WillVotegp, District10_2, sex, pobgp, agegp_TP_7, edugp_TP, v4_2019, 
inclin, midgp, marital, civicactive, SupportAny, 
                FollowForum, Campaign_b, Campaign_g, Campaign_d, survey_no FROM PORI_3')
                      
#Combine the data sets into one
PORI_combined <- full_join(PORI_1, PORI_2)
PORI_combined <- full_join(PORI_combined, PORI_3)

#Rename the variables
#rename constituency
PORI_combined$constituency[PORI_combined$District10_2 == 1] <- "HongKongIslandEast"
PORI_combined$constituency[PORI_combined$District10_2 == 2] <- "HongKongIslandWest"
PORI_combined$constituency[PORI_combined$District10_2 == 3] <- "KowloonEast"
PORI_combined$constituency[PORI_combined$District10_2 == 4] <- "KowloonWest"
PORI_combined$constituency[PORI_combined$District10_2 == 5] <- "KowloonCentral"
PORI_combined$constituency[PORI_combined$District10_2 == 6] <- "NewTerritoriesSouthEast"
PORI_combined$constituency[PORI_combined$District10_2 == 7] <- "NewTerritoriesNorth"
PORI_combined$constituency[PORI_combined$District10_2 == 8] <- "NewTerritoriesNorthWest"
PORI_combined$constituency[PORI_combined$District10_2 == 9] <- "NewTerritoriesSouthWest"
PORI_combined$constituency[PORI_combined$District10_2 == 10] <- "NewTerritoriesNorthEast"

#Dealing with missing values
PORI_combined <- PORI_combined %>%                               
  mutate(A003_ratinggp = replace(A003_ratinggp, A003_ratinggp == 8888|A003_ratinggp == -99|
                                   A003_ratinggp == 191, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(A003_ratinggp = replace(A003_ratinggp, A003_ratinggp == 1, "0"))

PORI_combined <- PORI_combined %>%                               
  mutate(A003_ratinggp = replace(A003_ratinggp, A003_ratinggp == 2, "1-49"))

PORI_combined <- PORI_combined %>%                               
  mutate(A003_ratinggp = replace(A003_ratinggp, A003_ratinggp == 3, "50"))

PORI_combined <- PORI_combined %>%                               
  mutate(A003_ratinggp = replace(A003_ratinggp, A003_ratinggp == 4, "51-99"))

PORI_combined <- PORI_combined %>%                               
  mutate(A003_ratinggp = replace(A003_ratinggp, A003_ratinggp == 5, "100"))

PORI_combined <- PORI_combined %>%                               
  mutate(v1 = replace(v1, v1 == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(v3 = replace(v3, v3 == 8888|v3 == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(v3 = replace(v3, v3 == 8888|v3 == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(WillVote = replace(WillVote, WillVote == 8888|WillVote == 8886|WillVote == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(WillVotegp = replace(WillVotegp, WillVotegp == 8888|WillVotegp == 8886|WillVotegp == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(WillVotegp = replace(WillVotegp, WillVotegp == 1, "Yes"))

PORI_combined <- PORI_combined %>%                               
  mutate(WillVotegp = replace(WillVotegp, WillVotegp == 3, "No"))

PORI_combined <- PORI_combined %>%                               
  mutate(OwnDistrictAny = replace(OwnDistrictAny, OwnDistrictAny == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(OwnDistrictAny = replace(OwnDistrictAny, OwnDistrictAny == 1, "At least one"))

PORI_combined <- PORI_combined %>%                               
  mutate(OwnDistrictAny = replace(OwnDistrictAny, OwnDistrictAny == 2, "No"))

PORI_combined <- PORI_combined %>%                               
  mutate(sex = replace(sex, sex == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(sex = replace(sex, sex == 1, "Male"))

PORI_combined <- PORI_combined %>%                               
  mutate(sex = replace(sex, sex == 2, "Female"))

PORI_combined <- PORI_combined %>%                               
  mutate(pobgp = replace(pobgp, pobgp == 8888|pobgp == -99|
                           pobgp == 8881, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(pobgp = replace(pobgp, pobgp == 1, "Hong Kong"))

PORI_combined <- PORI_combined %>%                               
  mutate(pobgp = replace(pobgp, pobgp == 2, "Mainland China"))

PORI_combined <- PORI_combined %>%                               
  mutate(agegp_TP_7 = replace(agegp_TP_7, agegp_TP_7 == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(edugp_TP = replace(edugp_TP, edugp_TP == -99, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(edugp_TP = replace(edugp_TP, edugp_TP == 1, "Primary or below"))

PORI_combined <- PORI_combined %>%                               
  mutate(edugp_TP = replace(edugp_TP, edugp_TP == 2, "Secondary"))

PORI_combined <- PORI_combined %>%                               
  mutate(edugp_TP = replace(edugp_TP, edugp_TP == 3, "Tertiary or above"))

PORI_combined <- PORI_combined %>%                               
  mutate(inclin = replace(inclin, inclin == -99|inclin == 8888|inclin == 8886, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(inclin = replace(inclin, inclin == 1, "Pro-democracy"))

PORI_combined <- PORI_combined %>%                               
  mutate(inclin = replace(inclin, inclin == 2, "Pro-establishment"))

PORI_combined <- PORI_combined %>%                               
  mutate(inclin = replace(inclin, inclin == 3, "Centre"))

PORI_combined <- PORI_combined %>%                               
  mutate(midgp = replace(midgp, midgp == -99|midgp == 8888, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(midgp = replace(midgp, midgp == 1, "Upper class"))

PORI_combined <- PORI_combined %>%                               
  mutate(midgp = replace(midgp, midgp == 2, "Middle class"))

PORI_combined <- PORI_combined %>%                               
  mutate(midgp = replace(midgp, midgp == 3, "Lower class"))

PORI_combined <- PORI_combined %>%                               
  mutate(SupportAny = replace(SupportAny, SupportAny == -99|SupportAny == 8888, NA))

PORI_combined <- PORI_combined %>%                               
  mutate(SupportAny = replace(SupportAny, SupportAny == 1, "Yes"))

PORI_combined <- PORI_combined %>%                               
  mutate(SupportAny = replace(SupportAny, SupportAny == 2, "No"))

#Recode values

#Export the new dataframe as a csv file
write.csv(PORI_combined,"PORI_combined.csv", row.names = FALSE)

#Read the DC shapefile 
DC2019 <- st_read("DCCA_2019_Shapefile 2/DCCA_2019.shp")

#view shapefile metadata
#st_geometry_type(DC2019)

#Plot a Shapefile
#ggplot() + 
  #geom_sf(data = DC2019, size = 2, color = "black", fill = "cyan1") + 
  #ggtitle("AOI Boundary Plot") + 
  #coord_sf()

#Create lists which state which DC constituencies belong to which LC2021 constituencies
HongKongIslandEast <- c(paste0("C0", 1:9), paste0("C1", 0:9), paste0("C2", 0:9), paste0("C3", 0:5), 
                        paste0("B0", 1:9), paste0("B1", 0:3))

HongKongIslandWest <- c(paste0("A0", 1:9), paste0("A1", 0:5), 
                        paste0("D0", 1:9), paste0("D1", 0:7), 
                        paste0("T0", 1:9), paste0("T1", 0))

KowloonEast <- c(paste0("J0", 1:9), paste0("J1", 0:9), paste0("J2", 0:9), paste0("J3", 0:9), paste0("J4", 0),
                 paste0("H2", 1:5))
  
KowloonWest <- c(paste0("E0", 1:9), paste0("E1", 0:9), paste0("E2", 0),
                 paste0("F0", 1:9), paste0("F1", 0:9), paste0("F2", 0:5))
                   
KowloonCentral <- c(paste0("G0", 1:9), paste0("G1", 0:9), paste0("G2", 0:5),
                    paste0("H0", 1:9), paste0("H1", 0:9), paste0("H2", 0))

NewTerritoriesSouthEast <- c(paste0("Q0", 1:9), paste0("Q1", 0:9), paste0("Q2", 0:9), 
                             paste0("R2", 5:9), paste0("R3", 0:7), "R40", "R41")
  
NewTerritoriesNorth <- c(paste0("N0", 1:9), paste0("N1", 0:8),
                         paste0("M1", 5:9), paste0("M2", 0:9), paste0("M3", 0:6))
  
NewTerritoriesNorthWest <- c(paste0("L0", 1:9), paste0("L1", 0:9), paste0("L2", 0:9), paste0("L3", 0:1), 
                             paste0("M0", 1:9), paste0("M1", 0:4), paste0("M3", 7:9))
  
NewTerritoriesSouthWest <- c(paste0("S0", 1:9), paste0("S1", 0:9), paste0("S2", 0:9), paste0("S3", 0:1),
                             paste0("K0", 1:9), paste0("K1", 0:9))
  
NewTerritoriesNorthEast <- c(paste0("P0", 1:9), paste0("P1", 0:9),
                             paste0("R0", 1:9), paste0("R1", 0:9), paste0("R2", 0:4), "R38", "R39")

#Create a new variable in DC2019 to indicate the LC2021 constituency of a DC2019 constituency
#HongKongIslandEast
for (i in HongKongIslandEast){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "HongKongIslandEast"
}

#HongKongIslandWest
for (i in HongKongIslandWest){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "HongKongIslandWest"
}

#KowloonEast
for (i in KowloonEast){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "KowloonEast"
}

#KowloonWest
for (i in KowloonWest){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "KowloonWest"
}

#KowloonCentral
for (i in KowloonCentral){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "KowloonCentral"
}

#NewTerritoriesSouthEast
for (i in NewTerritoriesSouthEast){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "NewTerritoriesSouthEast"
}

#NewTerritoriesNorth
for (i in NewTerritoriesNorth){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "NewTerritoriesNorth"
}

#NewTerritoriesNorthWest
for (i in NewTerritoriesNorthWest){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "NewTerritoriesNorthWest"
}

#NewTerritoriesSouthWest
for (i in NewTerritoriesSouthWest){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "NewTerritoriesSouthWest"
}

#NewTerritoriesNorthEast
for (i in NewTerritoriesNorthEast){
  DC2019$LC2021_GC[DC2019$CACODE == i] <- "NewTerritoriesNorthEast"
}

#Create a dataframe for each LC2021 constituency
HongKongIslandEast_2 <- DC2019[DC2019$LC2021_GC == "HongKongIslandEast", ] %>% 
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "HongKongIslandEast") # return back the data value 

HongKongIslandWest_2 <- DC2019[DC2019$LC2021_GC == "HongKongIslandWest", ] %>% 
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "HongKongIslandWest") # return back the data value 

KowloonEast_2 <- DC2019[DC2019$LC2021_GC == "KowloonEast", ] %>% 
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "KowloonEast") # return back the data value 

KowloonWest_2 <- DC2019[DC2019$LC2021_GC == "KowloonWest", ] %>%
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "KowloonWest") # return back the data value 

KowloonCentral_2 <- DC2019[DC2019$LC2021_GC == "KowloonCentral", ] %>% 
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "KowloonCentral") # return back the data value 

NewTerritoriesSouthEast_2 <- DC2019[DC2019$LC2021_GC == "NewTerritoriesSouthEast", ] %>%
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "NewTerritoriesSouthEast") # return back the data value 

NewTerritoriesNorth_2 <- DC2019[DC2019$LC2021_GC == "NewTerritoriesNorth", ] %>%
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "NewTerritoriesNorth") # return back the data value 

NewTerritoriesNorthWest_2 <- DC2019[DC2019$LC2021_GC == "NewTerritoriesNorthWest", ] %>%
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "NewTerritoriesNorthWest") # return back the data value 

NewTerritoriesSouthWest_2 <- DC2019[DC2019$LC2021_GC == "NewTerritoriesSouthWest", ] %>%
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "NewTerritoriesSouthWest") # return back the data value 

NewTerritoriesNorthEast_2 <- DC2019[DC2019$LC2021_GC == "NewTerritoriesNorthEast", ] %>%
  st_union() %>% # unite to a geometry object
  st_sf() %>% # make the geometry a data frame object
  mutate(constituency = "NewTerritoriesNorthEast") # return back the data value 

#Create a new dataframe by combining individual constituency dataframes
LC2021_combined <- rbind(HongKongIslandEast_2, HongKongIslandWest_2, KowloonEast_2, KowloonWest_2,
                         KowloonCentral_2, NewTerritoriesSouthEast_2, NewTerritoriesNorth_2, 
                         NewTerritoriesNorthWest_2, NewTerritoriesSouthWest_2, 
                         NewTerritoriesNorthEast_2)

#Export the LC constituency object into a shp file
st_write(LC2021_combined, "LC2021_combined.shp")



st_geometry_type(LC2021)

ggplot() + 
  geom_sf(data = LC2021, size = 2, color = "black", fill = "cyan1") + 
  ggtitle("LC2021") + coord_sf()

LC2021_2 <- st_read("LC2021.gdb")

st_geometry_type(LC2021_2)

ggplot() + 
  geom_sf(data = LC2021_2, size = 2, color = "black", fill = "cyan1") + 
  ggtitle("LC2021_2") + coord_sf()

st_write(LC2021_2, "LC2021_2.shp")


