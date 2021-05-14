# Create shapefile docs of riding and poll level shapefiles
# While combining them with electoral information

# Load Necessary Packages ####

pkgs <- c("leaflet", "leaflet.extras", 
          "tidyverse", "plotly", "scales")
lapply(pkgs, require, character.only = TRUE)
rm(pkgs)

# Poll Level ####
# Labels to include Riding name, poll name, poll number, winning party and vote shares

# Combine The Data
df.polls <- readRDS("data/old/PollLevelData.rds")
# Get rid of military votes as that cannot be reflected at poll level
df.polls <- filter(df.polls, !poll.station.number %in% c("S/R 1", "S/R 2"))

# Takes out all the letters and numbers after the dash (-) for each cell value, leaving you with the proper poll number
# Then make the row numeric
df.polls$poll.station.number <- sub(pattern = "(^\\d+).*", replacement = "\\1", x = df.polls$poll.station.number) %>% 
  as.numeric()

# Need to aggregate the data by poll station number and then create a new dataframe
df.polls <- aggregate(cbind(LiberalVotes, ConservativeVotes, NDPVotes, BlocVotes, GreenVotes,
                            PeoplesVotes, IndependentVotes, OtherVotes, votes) ~ poll.station.number + district.name + riding_code + year + province + 
                        poll.station.name + WinningParty, df.polls, sum, na.action=na.pass, na.rm=TRUE)

df.polls <- df.polls[, c(2:3,6,1,4:5,7:16)]

head(df.polls)

# Re-create percentage columns
df.polls$LiberalShare <- round((df.polls$LiberalVotes/df.polls$votes) * 100, digits = 1)
df.polls$ConservativeShare <- round((df.polls$ConservativeVotes/df.polls$votes) * 100, digits = 1)
df.polls$NDPShare <- round((df.polls$NDPVotes/df.polls$votes) * 100, digits = 1)
df.polls$BlocShare <- round((df.polls$BlocVotes/df.polls$votes) * 100, digits = 1)
df.polls$GreenShare <- round((df.polls$GreenVotes/df.polls$votes) * 100, digits = 1)
df.polls$IndependentShare <- round((df.polls$IndependentVotes/df.polls$votes) * 100, digits = 1)
df.polls$OtherShare <- round((df.polls$OtherVotes/df.polls$votes) * 100, digits = 1)

saveRDS(df.polls, "data/PollLevelData_Cleaned.rds")

# 2015 ====
#Read in the 2015 shape file
polls_shape.15 <- st_read("maps/Polling Boundaries Data/2015/polling_divisions_boundaries_2015.shp",
                          stringsAsFactors = FALSE) %>% 
  select(riding_code = FED_NUM, poll.station.number = PD_NUM) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 50) %>% # threshold in units used in the object (meters)
  st_transform(4326) %>% #*
  arrange(riding_code)
# Save for use by the Shiny app
#2015
if (file.exists("maps/poll_shapefiles15.gpkg")) { file.remove("maps/poll_shapefiles15.gpkg") } # st_write can't overwrite files on disk
st_write(polls_shape.15, "maps/poll_shapefiles15.gpkg")

# 2019 ====
# Do the same for 2019
polls_shape.19 <- st_read("maps/Polling Boundaries Data/2019/polling_divisions_boundaries_2019.shp",
                          stringsAsFactors = FALSE) %>% 
  select(riding_code = FEDNUM, poll.station.number = PDNUM) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 50) %>% # threshold in units used in the object (meters)
  st_transform(4326) %>% #*
  arrange(riding_code)

if (file.exists("maps/poll_shapefiles19.gpkg")) { file.remove("maps/poll_shapefiles19.gpkg") } # st_write can't overwrite files on disk
st_write(polls_shape.19, "maps/poll_shapefiles19.gpkg")

# Combine the data and save ====
# This will be an input in the app
df.polls.15 <- left_join(polls_shape.15, filter(df.polls, year==2015), by = c("riding_code", "poll.station.number"))
df.polls.19 <- left_join(polls_shape.19, filter(df.polls, year==2019), by = c("riding_code", "poll.station.number"))

# Combine the poll data
df.polls2 <- rbind(df.polls.15, df.polls.19)

# Add the party colours
partyColours <- data.frame(Party = c('Liberal', 'Conservative', 'NDP', 'Bloc', 'Green', 'Peoples', 'Independent', 'Other'),
                           colours = c('#D71920', '#1A4782', '#F58220', '#008080', "#3D9B35", "#4E5180", "#808080", "#808080"), 
                           stringsAsFactors = FALSE)
partyColours <- partyColours %>% 
  rename(WinningParty = Party)
# Combine the colours matched to the winning party
df.polls2 <- left_join(df.polls2, partyColours, by = "WinningParty")

# Save the data file
saveRDS(df.polls2, "data/PollData-WithMaps.rds")

# Riding Level ####
# Labels to include Riding name, winning party and winning candidate name

# Combine the data
df.riding <- readRDS("data/old/RidingLevelData.rds")

#Read in the riding level shape file
riding_shape <- st_read("maps/Riding Boundaries Data/2015 & 2019/federal_electoral_districts_boundaries_2019.shp",
                          stringsAsFactors = FALSE) %>% 
  select(riding_code = FEDNUM) %>% 
  st_simplify(preserveTopology = TRUE, dTolerance = 50) %>% # threshold in units used in the object (meters)
  st_transform(4326) %>% #*
  arrange(riding_code)
# Save for use by the Shiny app
# 2015 & 2019
if (file.exists("maps/riding_shapefiles15&19.gpkg")) { file.remove("maps/riding_shapefiles15&19.gpkg") } # st_write can't overwrite files on disk
st_write(riding_shape, "maps/riding_shapefiles15&19.gpkg")

# Combine the data and save ====
# This will be an input in the app
df.riding.15 <- left_join(riding_shape, filter(df.riding, year==2015), by = c("riding_code"))
df.riding.19 <- left_join(riding_shape, filter(df.riding, year==2019), by = c("riding_code"))

# Combine the riding data
df.riding2 <- rbind(df.riding.15, df.riding.19)

# Add the party colours
partyColours <- data.frame(Party = c('Liberal', 'Conservative', 'NDP', 'Bloc', 'Green', 'Peoples', 'Independent', 'Other'),
                           colours = c('#D71920', '#1A4782', '#F58220', '#008080', "#3D9B35", "#4E5180", "#808080", "#808080"), 
                           stringsAsFactors = FALSE)
partyColours <- partyColours %>% 
  rename(WinningParty = Party)
# Combine the colours matched to the winning party
df.riding2 <- left_join(df.riding2, partyColours, by = "WinningParty")

# Save the data file
saveRDS(df.riding2, "data/RidingData-WithMaps.rds")
