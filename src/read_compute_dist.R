# Read in the two data sources
# Fri Jun 17 14:56:37 2022 ------------------------------


library(tidyverse)
library(sf)

AllCent <- readxl::read_excel(here::here("input", "2020 US Shopping Centers Demographic Comparison.xlsx"),
                              sheet = "PropList_LatLong") 

TangCent <- readxl::read_excel(here::here("input", "CenterReview_AsOf20220510.xlsx"))

#-------- Filter Down inputs --------

CentList <-
  AllCent %>% 
  filter(!State %in% c("AK", "CA", "HI", "WY", "ID", "WA", "MO", "ND", "SD", "NE", "CO", "IA", "MT", "OR", "UT", "NV", "MN", "NM")) %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4269)

TangUS <- 
  TangCent %>% 
  filter(Country == "US") %>% 
  distinct(BldgGroupName, Lat, Long) %>% 
  mutate(Center = str_remove_all(BldgGroupName, " Outlet Center| Outlets| Outlet| - The Arches")) %>% 
  select(Center, Lat, Long) %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4269)

#----- Compare the distances between -----

Test <- 
  TangUS %>% 
  filter(Center == "Blowing Rock") 

# mapview::mapview(CentList)




dist_to_testing <- st_distance(x = Test, y = CentList) %>% 
  units::set_units(mi) %>% 
  as.data.frame() %>% 
  pivot_longer(cols = everything())


BlowEx <-
  cbind(CentList, dist_to_testing) %>% 
  mutate(jnCol = 1) %>% 
  st_drop_geometry() %>% 
  left_join(., Test %>% mutate(jnCol = 1) %>% st_drop_geometry(),
            by= c("jnCol")) %>% 
  select(-jnCol, -name)  %>% 
  mutate(DistBetween = as.numeric(value)) %>% 
  filter(DistBetween <= 100) %>%  #only keeping centers within 100 miles
  arrange(DistBetween)



#----

TangList <- unique(TangUS$Center)
# OrgDat <- BlowEx

datalist = list()

for (i in TangList){
  print(i)

  Test <- 
    TangUS %>% 
    filter(Center %in% i) 

  dist_to_testing <- st_distance(x = Test, y = CentList) %>% 
    units::set_units(mi) %>% 
    as.data.frame() %>% 
    pivot_longer(cols = everything())
  
  
  CombAndClean <-
    cbind(CentList, dist_to_testing) %>% 
    mutate(jnCol = 1) %>% 
    st_drop_geometry() %>% 
    left_join(., Test %>% mutate(jnCol = 1) %>% st_drop_geometry(),
              by= c("jnCol")) %>% 
    select(-jnCol, -name)  %>% 
    mutate(DistBetween = as.numeric(value)) %>% 
    filter(DistBetween <= 100) %>%  #only keeping centers within 100 miles
    arrange(DistBetween)
  
    
  datalist[[i]] <- CombAndClean
  
}


combAll <- do.call(rbind, datalist)






combAll %>% 
  relocate(Center, .before = Center.Name) %>% 
  select(-value) %>% 
  write_csv(., here::here("output", "DistanceToShopping.csv"))














