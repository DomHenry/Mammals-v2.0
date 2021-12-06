## ________________________________________________________________________

## Title:
## Purpose:
## Author:
## Date:

## Libraries

## ________________________________________________________________________


## Add species specific data cleaning













########### ALREADY WRITTEN CODE #####################
# 
# 
# # Import species data -----------------------------------------------------
# load("data output/occ_data_clean.RData")
# latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# # Remove errors (manual - #TODO OC needs to correct these in database)
# occdata_sf <- occdata_sf %>% 
#   filter(occurrenceID != "GCD_00001755") %>% # Suni in the northern cape
#   filter(occurrenceID != "RL_ART015254") %>% 
#   filter(occurrenceID != "RL_CAR042959") %>% # Cheetah in Cape Town
#   filter(occurrenceID != "RL_CAR042958") %>% 
#   filter(occurrenceID != "RL_PRI031600") %>%  # Southerly C.a. schwarzi record
#   filter(occurrenceID != "RL_PRI042596")      # Northerly C.a labiatus record
# 
# # Change these records from samango to white collared
# monkey_change <- c("RL_PRI040313","RL_PRI040475","RL_PRI040476","RL_PRI040474",
#                    "RL_PRI040312","RL_PRI040472","RL_PRI040314","RL_PRI040478",
#                    "RL_PRI040479","RL_PRI040316","RL_PRI040315","RL_PRI040473")
# 
# occdata_sf <- occdata_sf %>% 
#   mutate(scientific_name = case_when(occurrenceID %in% monkey_change ~ "Cercopithecus albogularis schwarzi",
#                                      TRUE ~ scientific_name)) %>% 
#   glimpse()
# 
# # Filter occurence data based on query ------------------------------------
# 
# ## Species and time frame -THIS ASSUMES THERE IS A VALID YEAR - COMMENT OUT LAST TWO LINES FOR MORE DATA
# occ_data <- occdata_sf %>% 
#   filter(scientific_name == query$Value[which(query$Input == "Species")]) #%>% 
# # filter(year >= query$Value[which(query$Input == "Start year")]) %>% 
# # filter(year <= query$Value[which(query$Input == "End year")])
# 
# # Remove out of range Hartmann's Zebra records
# if(query$Value[which(query$Input == "Species")] == "Equus zebra hartmannae"){
#   occ_data <- occ_data %>% 
#     filter(PROVINCE == "Northern Cape")
# }
# 
# # Juliana's Golden Mole - Keep Gauteng records only
# if(query$Value[which(query$Input == "Species")] == "Neamblysomus julianae"){
#   occ_data <- occ_data %>% 
#     filter(PROVINCE == "Gauteng")
# }
# 
# # Remove Wild Dog from WC, EC, NC
# if(query$Value[which(query$Input == "Species")] == "Lycaon pictus"){
#   occ_data <- occ_data %>% 
#     filter(!PROVINCE %in% c("Western Cape","Northern Cape","Eastern Cape", "Gauteng"))
# }
# 
# # Remove Otter from  NC and add Lynch (1983 data from the Free State)
# if(query$Value[which(query$Input == "Species")] == "Hydrictis maculicollis"){
#   
#   latlongCRS <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#   occ_data <- read_csv("data input/SN Otter data/updated_otter_points.csv") %>% 
#     select(tali_id,occurrenceID,order,year,month,day,scientific_name,decimallongitude,decimallatitude) %>% 
#     st_as_sf(coords = c("decimallongitude","decimallatitude"),crs = latlongCRS)
#   
#   za <- st_read("data input/RSA_fixed.shp",crs = latlongCRS)
#   st_crs(occ_data) <- latlongCRS
#   
#   occ_data <- occ_data %>% 
#     st_intersection(za) %>% 
#     distinct(geometry, .keep_all = TRUE)
# }
# 
# ## Date quality
# dateval <- query$Value[which(query$Input == "Date accuracy")]
# 
# if(dateval == "Complete"){
#   
#   occ_data <- occ_data %>% 
#     filter(!is.na(date))
#   
# } else if (dateval == "Year and month"){
#   
#   occ_data <- occ_data %>% 
#     filter(year_check == "valid" & month_check == "valid")
#   
# } else if (dateval == "Year only"){
#   
#   occ_data <- occ_data %>% 
#     filter(year_check == "valid")
#   
# } else if (dateval == "All"){
#   
#   occ_data <- occ_data
#   
# }