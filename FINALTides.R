# Tidal data and plotting
# 14 November 2022

# Tides -------------------------------------------------------------------

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(lubridate)

# data from http://uhslc.soest.hawaii.edu/data/
tides <- read_csv("h221.csv",
                  col_names = c("year", "month", "day", "hour", "swl")) |>
  mutate(datetime = make_datetime(year, month, day, hour)) %>% 
  mutate(swlm = tides$swl/1000)

Tides1 <- tides %>% 
  filter(datetime >= "2022-09-12 15:00:00" & datetime < "2022-09-19 23:00:00") %>% 
  ggplot(aes(x = datetime, y = swlm)) +
  geom_line(colour = "firebrick", size = 0.6) +
  labs(x = "Date", y = "Swell (m)", title = "Tidal Records 1") +
  ylim(0.2, 2.1) +
  theme_bw()

Tides2 <- tides %>% 
  filter(datetime >= "2022-09-30 08:00:00" & datetime < "2022-10-7 23:00:00") %>% 
  ggplot(aes(x = datetime, y = swlm)) +
  geom_line(colour = "firebrick", size = 0.6) +
  labs(x = "Date", y = "Swell (m)", title = "Tidal Records 2") +
  ylim(0.2, 2.1) +
  theme_bw()

ggarrange(Tides1, Tides2)

# Temp data ---------------------------------------------------------------

# First load all the data into objects
Temp1 <- read.csv("Data/MillersPoint/MP_Temp1.csv")
Temp2 <- read.csv("Data/MillersPoint/MP_Temp2.csv")
Temp3 <- read.csv("Data/MillersPoint/MP_Temp3.csv")

# Now for the plots

# Importing dates with times into new column so R can read it properly
Temp1$datetime = mdy_hms(Temp1$Date.Time)
Temp2$datetime = mdy_hms(Temp2$Date.Time)
Temp3$datetime = mdy_hms(Temp3$Date.Time)

# Calculating the temp means
meansMP <- data.frame(cbind(Temp1$Temp, Temp2$Temp, Temp3$Temp)) # combining temps from the 3 df
meansMP$Mean <- ave(meansMP$X1, meansMP$X2, meansMP$X3) # creating new column of the mean

# Mean plot
meanplotMP <- ggplot(meansMP, aes(x = Temp1$datetime, y = Mean)) + # plotting the mean
  geom_line(colour = "firebrick") +
  labs(x = "Date", y = "Temperature (°C)", title = "Miller's Point") +
  ylim(10, 20) +
  theme_bw()


# Plotting the temperatures
MP_temp <- ggplot(Temp1, aes(x = datetime, y = Temp)) + 
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2$Temp), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3$Temp), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meansMP$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Temperature (°C)", title = "Miller's Point") +
  ylim(10, 20) +
  theme_bw()


# Glencairn ---------------------------------------------------------------

# Temp --------------------------------------------------------------------

Temp1G <- read.csv("Data/Glencairn/GC_Temp1.csv")
Temp2G <- read.csv("Data/Glencairn/GC_Temp2.csv")
Temp3G <- read.csv("Data/Glencairn/GC_Temp3.csv")
Temp4G <- read.csv("Data/Glencairn/GC_Temp4.csv")

Temp1G$datetime = mdy_hms(Temp1G$Date.Time)
Temp2G$datetime = mdy_hms(Temp2G$Date.Time)
Temp3G$datetime = mdy_hms(Temp3G$Date.Time)
Temp4G$datetime = mdy_hms(Temp4G$Date.Time)

# Calculate the means
meansGC <- data.frame(cbind(Temp1G$Temp, Temp2G$Temp, Temp3G$Temp, Temp4G$Temp))
meansGC$Mean <- ave(meansGC$X1, meansGC$X2, meansGC$X3, meansGC$X4)

meanplotGC <- ggplot(meansGC, aes(x = Temp1$datetime, y = Mean)) + # plotting the mean
  geom_line(colour = "firebrick") +
  labs(x = "Date", y = "Temperature (°C)", title = "Glencairn") +
  ylim(10, 20) +
  theme_bw()


# Plot
GC_temp <- ggplot(Temp1G, aes(x = datetime, y = Temp)) +
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2G$Temp), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3G$Temp), colour = "navyblue", alpha = 0.8) +
  geom_line(aes(y = Temp4G$Temp), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meansGC$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Temperature (°C)", title = "Glencairn") +
  ylim(10, 20) +
  theme_bw()



# Kommetjie ---------------------------------------------------------------

# Temp --------------------------------------------------------------------

Temp1K <- read.csv("Data/Kommetjie/KO_Temp1.csv")
Temp2K <- read.csv("Data/Kommetjie/KO_Temp2.csv")
Temp3K <- read.csv("Data/Kommetjie/KO_Temp4.csv") # Third temp didn't export

Temp1K$datetime = mdy_hms(Temp1K$Date.Time)
Temp2K$datetime = mdy_hms(Temp2K$Date.Time)
Temp3K$datetime = mdy_hms(Temp3K$Date.Time)


# Calculate the means
meansKO <- data.frame(cbind(Temp1K$Temp, Temp2K$Temp, Temp3K$Temp))
meansKO$Mean <- ave(meansKO$X1, meansKO$X2, meansKO$X3)

meanplotKO <- ggplot(meansKO, aes(x = Temp1K$datetime, y = Mean)) + # plotting the mean
  geom_line(colour = "firebrick") +
  labs(x = "Date", y = "Temperature (°C)", title = "Kommetjie") +
  ylim(10, 20) +
  theme_bw()

# Plot

KO_temp <- ggplot(Temp1K, aes(x = datetime, y = Temp)) +
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2K$Temp), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3K$Temp), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meansKO$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Temperature (°C)", title = "Kommetjie") +
  ylim(10, 20) +
  theme_bw()


# Oudekraal ---------------------------------------------------------------

# Temp --------------------------------------------------------------------

Temp1O <- read.csv("Data/Oudekraal/OU_Temp1.csv")
Temp2O <- read.csv("Data/Oudekraal/OU_Temp2.csv")
Temp3O <- read.csv("Data/Oudekraal/OU_Temp3.csv") # Fourth temp didn't export

Temp1O$datetime = mdy_hms(Temp1O$Date.Time)
Temp2O$datetime = mdy_hms(Temp2O$Date.Time)
Temp3O$datetime = mdy_hms(Temp3O$Date.Time)

# Calculate the means
meansOU <- data.frame(cbind(Temp1O$Temp, Temp2O$Temp, Temp3O$Temp))
meansOU$Mean <- ave(meansOU$X1, meansOU$X2, meansOU$X3)

meanplotOU <- ggplot(meansOU, aes(x = Temp1K$datetime, y = Mean)) + # plotting the mean
  geom_line(colour = "firebrick") +
  labs(x = "Date", y = "Temperature (°C)", title = "Oudekraal") +
  ylim(10, 20) +
  theme_bw()

# Plot
OU_temp <- ggplot(Temp1O, aes(x = datetime, y = Temp)) +
  geom_line(colour = "darkorange1") +
  geom_line(aes(y = Temp2O$Temp), colour = "deepskyblue3")+
  geom_line(aes(y = Temp3O$Temp), colour = "firebrick") +
  geom_line(aes(y = meansOU$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Temperature (°C)", title = "Oudekraal") +
  ylim(10, 20) +
  theme_bw()
OU_temp


# Final plot --------------------------------------------------------------

ggarrange(GC_temp, OU_temp, Tides1, Tides2, ncol = 2, nrow = 2)

