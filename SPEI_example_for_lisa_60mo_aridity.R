library('tidyverse') # you probably have this. If not, install

# This installs from my github. Will also install SPEI, and a couple other R packages
devtools::install_github("gremau/rclimtools")
library('rclimtools')

# Set working directory to wherever the script and input data files are. This
# will also be where new files are written
setwd("/Users/lisagarcia/Documents/Paleomidden project/Final R code/SPEI/60Mo_aridity")

################################## BAY1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Bay_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 45.295781

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_BAY1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]


################################## BAY2 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Bay_2.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 36.091914

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_BAY2.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## BAY3 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Bay_3.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 39.182022

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_BAY3.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## BRY1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Bry_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.55594

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_BRY1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## DVJ1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('DVJ_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 36.9578

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_DVJ1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## RM1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('RM_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 37.4937

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_RM1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## RM2 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('RM_2.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.1532

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_RM2.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## RM3 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('RM_3.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 41.2082

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_RM3.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## RM4 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('RM_4.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 43.8211

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_RM4.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## UNM1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('UNM_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 33.192659

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)

# Plot the 12-month SPEI series
plot(test$date, test$spei12mo, pch=20, col='dark grey')
lines(test$date, test$spei12mo, pch=16)
abline(h=0, lty=3)
abline(v=as.Date("1963-04-07"), col="blue")
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_UNM1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## UTC1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('UTC_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.625

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_UTC1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## UTC2 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('UTC_2.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.7228

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_UTC2.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## UTC3 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('UTC_3.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.5833333

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_UTC3.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Farm_1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Farm_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 36.7794

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)

# Plot the 12-month SPEI series
plot(test$date, test$spei12mo, pch=20, col='dark grey')
lines(test$date, test$spei12mo, pch=16)
abline(h=0, lty=3)
abline(v=as.Date("2024-08-31"), col="blue")
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_Farm_1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Farm_2 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Farm_2.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 36.7783

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_Farm_2.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Farm_3 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Farm_3.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 36.7858

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_Farm_3.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Farm_4 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Farm_4.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 36.7864

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_Farm_4.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_1 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_1.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 37.9867

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_Utah_1.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_2 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_2.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4564

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
#write_csv(df_spei, "df_spei_Utah_2.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_3 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_3.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4461

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_3.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_4 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_4.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4808

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_4.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_5 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_5.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4853

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_5.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_6 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_6.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4789

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_6.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_7 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_7.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4725

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_7.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_8 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_8.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.4961

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)

# Plot the 12-month SPEI series
plot(test$date, test$spei12mo, pch=20, col='dark grey')
lines(test$date, test$spei12mo, pch=16)
abline(h=0, lty=3)
abline(v=as.Date("2024-08-24"), col="blue")
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_8.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_9 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_9.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.5703

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_9.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_10 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_10.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.5833

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_10.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_11 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_11.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.5956

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_11.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_12 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_12.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.6083

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_12.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_13 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_13.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.6297

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_13.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_14 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_14.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.6517

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_14.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]

################################## Utah_15 ##################################
# Read a dataset downloaded from PRISM ()
df <- read_csv('Utah_15.csv', skip = 10)

# The PRISM data has a YYYY-MM date column - give it a full date index
tempdt <- as.Date(paste0(df$Date, "-01"), "%Y-%m-%d")
tempdt2 <- paste(df$Date,
                 lubridate::days_in_month(tempdt), sep='-')
df$date <- as.Date(tempdt2, "%Y-%m-%d")

# Create a column with the latitude of the location
df['lat'] <- 38.6797

# Add spei and pet columns to your PRISM dataframe using a function from
# the rclimtools package (uses SPEI package).
df_spei <- add_spei_cols(df, "tmean (degrees C)", "ppt (mm)", scale_mo = 60)

# Look at first few rows of dataframe (note new columns)
head(df_spei)

# Plot the 12-month SPEI series
plot(df_spei$date, df_spei$spei60mo, pch=20, col='dark grey')
lines(df_spei$date, df_spei$spei60mo, pch=16)
abline(h=0, lty=3)
# Monthly observations below the zero line are drier than the mean, based on a
# 12-month rolling window. Observations above the zero line are wetter than
# the mean. Units on the Y axis are basically standard deviations from the mean.

# Write to csv - goes into working directory by default
write_csv(df_spei, "df_spei_Utah_15.csv")


# Calculate the sum of PET and Precipitation
# for every year in the timeseries
annmeans <- df_spei %>% group_by(year) %>%
  summarize(pet_tho = sum(pet_tho), 
            ppt = sum(`ppt (mm)`))

# Calculate the annual mean of PET and PPT for 
# the full timeseries. This can be used to create
# an aridity index for the site (ppt - PET)
site_aridity <- colMeans(annmeans)

# Then just subtract pet from ppt to get an aridity index (more negative = more arid)
ai <- site_aridity[[3]] - site_aridity[[2]]

# An alternative index is ppt/pet (closer to 0 = more arid)
ai2 <- site_aridity[[3]]/site_aridity[[2]]
