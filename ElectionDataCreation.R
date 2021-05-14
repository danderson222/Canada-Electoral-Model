# Create data files

# Combining the Canadian Electoral Results Data
# https://www.elections.ca/content.aspx?section=res&dir=rep/off/43gedata&document=bypro&lang=e

# Set working directory

# Load packages
library(tidyverse)

# Since the files are in the form of hundreds of csv files, we have read each and combine

# I could probably duplicate this with lapply
# dat.11 <-  list.files(path = "./ElectionData/Input/2011/",
#                    pattern = "*.csv", 
#                    full.names = T) %>% 
#   map_df(~read_csv(., col_types = cols(.default = "c"))) 
# 
# df.11 <- dat.11

dat.15 <-  list.files(path = "./ElectionData/Input/2015/",
                      pattern = "*.csv", 
                      full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

df.15 <- dat.15[,1:18]

dat.19 <-  list.files(path = "./ElectionData/Input/2019/",
                      pattern = "*.csv", 
                      full.names = T) %>% 
  map_df(~read_csv(., col_types = cols(.default = "c"))) 

df.19 <- dat.19
names(df.15)
names(df.19)

# rm(dat.15, dat.19)

# Add a year variable and combine the two dataframes
df.15$year <- 2015
df.19$year <- 2019
df <- rbind(df.15, df.19)

# We need to adjust and fix the column names because we have 18 columns
colnames(df) # What columns do we not need
df <- df[,-c(3, 6:9, 12, 15)]

names <- c("district.number", "district.name", "poll.station.number", "poll.station.name", "electors",
           "last.name", "first.name", "party", "incumbent", "winner", "votes", "year") 
colnames(df) <- names 

# Combine first and last candidate names and poll station names
# Could also do this using transmute
df$candidate.name <- paste(df$last.name, df$first.name, sep=", ") # comma separator
df$polling.station <- paste(df$poll.station.number, df$poll.station.name, sep=" - ") # comma separator
df$polling.station <- paste(df$district.name, df$polling.station, sep= ": ")
# Delete 
df <- df[, -c(6:7)] # Need to keep the polling station number
# Get rid of rows that have no information about votes
inspectdf::inspect_na(df)
df <- df %>% 
  filter(is.na(votes)==FALSE)
# Good, now there are no NAs in our data

# Now create a column for the respective province
df$province.number <- substr(df$district.number, 1, 2)
df$province <- NA
df <- df %>% 
  mutate(province=ifelse(province.number==10, "NL", province)) %>% 
  mutate(province=ifelse(province.number==11, "PEI", province)) %>% 
  mutate(province=ifelse(province.number==12, "NS", province)) %>% 
  mutate(province=ifelse(province.number==13, "NB", province)) %>% 
  mutate(province=ifelse(province.number==24, "QB", province)) %>% 
  mutate(province=ifelse(province.number==35, "ON", province)) %>% 
  mutate(province=ifelse(province.number==46, "MB", province)) %>% 
  mutate(province=ifelse(province.number==47, "SK", province)) %>% 
  mutate(province=ifelse(province.number==48, "AB", province)) %>% 
  mutate(province=ifelse(province.number==59, "BC", province)) %>% 
  mutate(province=ifelse(province.number==60, "YU", province)) %>% 
  mutate(province=ifelse(province.number==61, "NT", province)) %>% 
  mutate(province=ifelse(province.number==62, "NU", province)) %>% 
  select(-province.number)

# Update the classes on the df
df <- as_tibble(df) %>% 
  mutate_at(vars(district.number, electors, votes), as.numeric) %>% 
  mutate_at(vars(district.name, polling.station, incumbent, winner, year, province), as.factor)

# Before doing any other analysis, we will have to split up the datasets by year
# We could probably do this with lapply, but I haven't quite figured that out yet...
df.15 <- df %>% 
  filter(year==2015)
df.19 <- df %>% 
  filter(year==2019)

# Let's count the votes of only legit parties and not random parties (like the Rhinoceros Party, my fav)
legit.parties <- unique(df$party)
legit.parties <- legit.parties[c(2:4,6,9,13,27)]
legit.parties

df.15 <- df.15 %>% 
  mutate(NDPVotes=ifelse(party==legit.parties[1], votes, NA)) %>% 
  mutate(ConservativeVotes=ifelse(party==legit.parties[2], votes, NA)) %>% 
  mutate(GreenVotes=ifelse(party==legit.parties[3], votes, NA)) %>% 
  mutate(LiberalVotes=ifelse(party==legit.parties[4], votes, NA)) %>% 
  mutate(IndependentVotes=ifelse(party==legit.parties[5], votes, NA)) %>% 
  mutate(BlocVotes=ifelse(party==legit.parties[6], votes, NA)) %>% 
  mutate(PeoplesVotes=ifelse(party==legit.parties[7], votes, NA))
riding.df15 <- aggregate(cbind(LiberalVotes, ConservativeVotes, NDPVotes, BlocVotes, GreenVotes,
                               PeoplesVotes, IndependentVotes, votes) ~ district.number, df.15, sum, 
                         na.action=na.pass, na.rm=TRUE)
merge.data.15 <- df.15 %>%
  select("district.number", "district.name", "province", "year") %>% 
  distinct(district.number, .keep_all = TRUE) 
riding.df15 <- merge(riding.df15, merge.data.15, by = "district.number")  
head(riding.df15)
riding.df15$WinningParty <- colnames(riding.df15[2:7])[apply(riding.df15[2:7], 1, which.max)] 
riding.df15$WinningParty <- gsub('.{5}$','',riding.df15$WinningParty)

# Now for 2019
df.19 <- df.19 %>% 
  mutate(NDPVotes=ifelse(party==legit.parties[1], votes, NA)) %>% 
  mutate(ConservativeVotes=ifelse(party==legit.parties[2], votes, NA)) %>% 
  mutate(GreenVotes=ifelse(party==legit.parties[3], votes, NA)) %>% 
  mutate(LiberalVotes=ifelse(party==legit.parties[4], votes, NA)) %>% 
  mutate(IndependentVotes=ifelse(party==legit.parties[5], votes, NA)) %>% 
  mutate(BlocVotes=ifelse(party==legit.parties[6], votes, NA)) %>% 
  mutate(PeoplesVotes=ifelse(party==legit.parties[7], votes, NA))
riding.df19 <- aggregate(cbind(LiberalVotes, ConservativeVotes, NDPVotes, BlocVotes, GreenVotes,
                               PeoplesVotes, IndependentVotes, votes) ~ district.number, df.19, sum, 
                         na.action=na.pass, na.rm=TRUE)
merge.data.19 <- df.19 %>%
  select("district.number", "district.name", "province", "year") %>% 
  distinct(district.number, .keep_all = TRUE) 
riding.df19 <- merge(riding.df19, merge.data.19, by = "district.number")  
head(riding.df19)
riding.df19$WinningParty <- colnames(riding.df19[2:7])[apply(riding.df19[2:7], 1, which.max)] 
riding.df19$WinningParty <- gsub('.{5}$','',riding.df19$WinningParty)

# Get rid of some dataframes to clean up
rm(merge.data.15, merge.data.19)

df2 <- as_tibble(rbind(riding.df15, riding.df19))

# Reorder columns
df2 <- df2[, c(1,10:13, 2:9)]
df2$OtherVotes <- df2$votes - rowSums(df2[,6:12])
df2 <- df2[, c(1:12,14,13)]

# Create percentage columns
df2$LiberalShare <- round((df2$LiberalVotes/df2$votes) * 100, digits = 1)
df2$ConservativeShare <- round((df2$ConservativeVotes/df2$votes) * 100, digits = 1)
df2$NDPShare <- round((df2$NDPVotes/df2$votes) * 100, digits = 1)
df2$BlocShare <- round((df2$BlocVotes/df2$votes) * 100, digits = 1)
df2$GreenShare <- round((df2$GreenVotes/df2$votes) * 100, digits = 1)
df2$IndependentShare <- round((df2$IndependentVotes/df2$votes) * 100, digits = 1)
df2$OtherShare <- round((df2$OtherVotes/df2$votes) * 100, digits = 1)

df2 <- df2 %>% 
  rename(riding_code = district.number)

# Write csv to output folder
path_out <- "ElectionData\\Output\\"
# filename <- paste(path_out, 'RidingLevelData.csv',sep = '')
filename <- paste(path_out, 'RidingLevelData.rds')
saveRDS(df2, filename)
# write.csv(df2, filename, row.names = FALSE) # Don't save in csv because it butchers the accent letters

# Poll Level Data ####

# First remove the non-essential data
rm(df2, riding.df15, riding.df19)


poll.df15 <- aggregate(cbind(LiberalVotes, ConservativeVotes, NDPVotes, BlocVotes, GreenVotes,
                             PeoplesVotes, IndependentVotes, votes) ~ polling.station, df.15, sum, 
                       na.action=na.pass, na.rm=TRUE)
merge.data.15 <- df.15 %>%
  select("district.number", "district.name", "poll.station.number", "poll.station.name", "polling.station", "province", "year") %>% 
  distinct(polling.station, .keep_all = TRUE) 
poll.df15 <- merge(poll.df15, merge.data.15, by = "polling.station")  
head(poll.df15)
poll.df15$WinningParty <- colnames(poll.df15[2:7])[apply(poll.df15[2:7], 1, which.max)] 
poll.df15$WinningParty <- gsub('.{5}$','',poll.df15$WinningParty)

# Now for 2019
df.19 <- df.19 %>% 
  mutate(NDPVotes=ifelse(party==legit.parties[1], votes, NA)) %>% 
  mutate(ConservativeVotes=ifelse(party==legit.parties[2], votes, NA)) %>% 
  mutate(GreenVotes=ifelse(party==legit.parties[3], votes, NA)) %>% 
  mutate(LiberalVotes=ifelse(party==legit.parties[4], votes, NA)) %>% 
  mutate(IndependentVotes=ifelse(party==legit.parties[5], votes, NA)) %>% 
  mutate(BlocVotes=ifelse(party==legit.parties[6], votes, NA)) %>% 
  mutate(PeoplesVotes=ifelse(party==legit.parties[7], votes, NA))
poll.df19 <- aggregate(cbind(LiberalVotes, ConservativeVotes, NDPVotes, BlocVotes, GreenVotes,
                             PeoplesVotes, IndependentVotes, votes) ~ polling.station, df.19, sum, 
                       na.action=na.pass, na.rm=TRUE)
merge.data.19 <- df.19 %>%
  select("district.number", "district.name", "poll.station.number", "poll.station.name", "polling.station", "province", "year") %>% 
  distinct(polling.station, .keep_all = TRUE)
poll.df19 <- merge(poll.df19, merge.data.19, by = "polling.station")  
head(poll.df19)
poll.df19$WinningParty <- colnames(poll.df19[2:7])[apply(poll.df19[2:7], 1, which.max)] 
poll.df19$WinningParty <- gsub('.{5}$','',poll.df19$WinningParty)

df.polls <- as_tibble(rbind(poll.df15, poll.df19))
names(df.polls)

# Reorder columns
df.polls <- df.polls[, c(1,10:16, 2:9)]
df.polls$OtherVotes <- df.polls$votes - rowSums(df.polls[,9:15])
df.polls <- df.polls[, c(1:15,17,16)]

# Create percentage columns
df.polls$LiberalShare <- round((df.polls$LiberalVotes/df.polls$votes) * 100, digits = 1)
df.polls$ConservativeShare <- round((df.polls$ConservativeVotes/df.polls$votes) * 100, digits = 1)
df.polls$NDPShare <- round((df.polls$NDPVotes/df.polls$votes) * 100, digits = 1)
df.polls$BlocShare <- round((df.polls$BlocVotes/df.polls$votes) * 100, digits = 1)
df.polls$GreenShare <- round((df.polls$GreenVotes/df.polls$votes) * 100, digits = 1)
df.polls$IndependentShare <- round((df.polls$IndependentVotes/df.polls$votes) * 100, digits = 1)
df.polls$OtherShare <- round((df.polls$OtherVotes/df.polls$votes) * 100, digits = 1)

df.polls <- df.polls %>% 
  rename(riding_code = district.number)

# Write csv to output folder
path_out <- "ElectionData\\Output\\"
# filename <- paste(path_out, 'RidingLevelData.csv',sep = '')
filename <- paste(path_out, 'PollLevelData.rds')
saveRDS(df.polls, filename)
# write.csv(df2, filename, row.names = FALSE) # Don't save in csv because it butchers the accent letters

# CLEAN UP EVERYTHING #####

# Clear environment
rm(list = ls()) 

# Clear packages
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

# Clear mind :)