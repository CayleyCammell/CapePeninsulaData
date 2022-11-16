# Final plots for data collected at all four sites
# Temperature, atmospheric and wind data
# 5 October 2022

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(lubridate)

# Miller's Point ----------------------------------------------------------

# Temp --------------------------------------------------------------------

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


# Final Temp --------------------------------------------------------------

ggarrange(MP_temp, KO_temp, GC_temp, OU_temp, common.legend = TRUE)



# Atmospheric data --------------------------------------------------------

# Hourly atmospheric data -------------------------------------------------

otherdataH <- read.csv("CapeTownHourly2022-09-12to2022-10-11.csv")

otherdataH$DateTime <- ymd_hms(otherdataH$datetime)

# Separate two time periods

Hourly1 <- otherdataH %>%
  filter(row_number() < 154) # Time period of MP and GC

Hourly2 <- otherdataH %>%
  filter(row_number() > 425) %>% 
  filter(row_number() < 242) # Time period of KO and OU


# Atmospheric temp plots --------------------------------------------------

H1atmotemp <- ggplot(Hourly1, aes(x = DateTime, y = temp)) +
  geom_line(colour = "deepskyblue3") +
  labs(x = "Date/time", y = "Atmospheric Temperature (°C)", title = "Atmospheric 1") +
  theme_bw()
H1atmotemp


H2atmotemp <- ggplot(Hourly2, aes(x = DateTime, y = temp)) +
  geom_line(colour = "deepskyblue3") +
  labs(x = "Date/time", y = "Atmospheric Temperature (°C)", title = "Atmospheric 2") +
  theme_bw()
H2atmotemp


# Final temperature plot with in situ means and atmo temps separate

alltemps <- ggarrange(meanplotMP, meanplotKO, meanplotGC, meanplotOU, H1atmotemp, H2atmotemp, ncol = 2, nrow = 3, common.legend = TRUE)
alltemps  


# Water temp HOURLY means with atmo temp ----------------------------------

# Miller's Point ----------------------------------------------------------

# Convert to hourly
MPHourlyTemp = aggregate(list(hourlymean = meansMP$Mean), by = list(datetime = cut(as.POSIXct(Temp1$datetime), "hour")), mean) 

# Plot atmo and water temps together
meantempsMP <- ggplot(MPHourlyTemp, aes(x = Hourly1$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "black") +
  geom_line(aes(y = Hourly1$temp), colour = "deepskyblue3") +
  labs(x = "Date", y = "Temperature (°C)", title = "Miller's Point") +
  ylim(8, 28) +
  theme_bw()
meantempsMP


# Glencairn ---------------------------------------------------------------

# Convert to hourly
GCHourlyTemp = aggregate(list(hourlymean = meansGC$Mean), by = list(datetime = cut(as.POSIXct(Temp1G$datetime),"hour")), mean) 


# Plot atmo and water temps together
meantempsGC <- ggplot(GCHourlyTemp, aes(x = Hourly1$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "black") +
  geom_line(aes(y = Hourly1$temp), colour = "deepskyblue3") +
  labs(x = "Date", y = "Temperature (°C)", title = "Glencairn") +
  ylim(8, 28) +
  theme_bw()
meantempsGC

# Kommetjie ---------------------------------------------------------------

# Convert to hourly
KOHourlyTemp = aggregate(list(hourlymean = meansKO$Mean), by = list(datetime = cut(as.POSIXct(Temp1K$datetime),"hour")), mean) 

# Plot atmo and water temps together
meantempsKO <- ggplot(KOHourlyTemp, aes(x = Hourly2$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "black") +
  geom_line(aes(y = Hourly2$temp), colour = "deepskyblue3") +
  labs(x = "Date", y = "Temperature (°C)", title = "Kommetjie") +
  ylim(8, 28) +
  theme_bw()
meantempsKO

# Oudekraal ---------------------------------------------------------------

# Convert to hourly
OUHourlyTemp = aggregate(list(hourlymean = meansOU$Mean), by = list(datetime = cut(as.POSIXct(Temp1O$datetime),"hour")), mean) 

# Plot atmo and water temps together
meantempsOU <- ggplot(OUHourlyTemp, aes(x = Hourly2$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "black") +
  geom_line(aes(y = Hourly2$temp), colour = "deepskyblue3") +
  labs(x = "Date", y = "Temperature (°C)", title = "Oudekraal") +
  ylim(8, 28) +
  theme_bw()
meantempsOU



# View with atmo data -----------------------------------------------------

ggarrange(meantempsMP, meantempsKO, meantempsGC, meantempsOU)


# Wind data ---------------------------------------------------------------

h1wind <- ggplot(Hourly1, aes(x = DateTime, y = windspeed)) +
  geom_line(colour = "deepskyblue3") +
  labs(x = "Date/time", y = "Wind Speed (km/h)", title = "Hourly Wind Speeds 1") +
  theme_bw()  

h2wind <- ggplot(Hourly2, aes(x = DateTime, y = windspeed)) +
  geom_line(colour = "deepskyblue3") +
  labs(x = "Date/time", y = "Wind Speed (km/h)", title = "Hourly Wind Speeds 2") +
  theme_bw() 

ggarrange(h1wind, h2wind)


# With arrows for direction and speed rather ------------------------------

library(scales)

# Method 3

winddir1 <- ggplot(Hourly1) +
  geom_segment(aes(x = DateTime,
                   y = 0,
                   xend = DateTime + lubridate::dhours(windspeed * 1 * -cos((90-winddir) / 360 * 2 * pi)),
                   yend = windspeed * 1 * -sin((90-winddir) / 360 * 2 * pi)),
  arrow = arrow(length = unit(0.1, "cm")) ) +
  geom_point(aes(DateTime, 0), size = 0.5) +
  coord_fixed(3600) +
  labs(x = "Date", y = "", title = "Hourly Wind Data 1") +
  # Create the label box for our wind vector length legend
# geom_label(aes(x = as.Date('2022-09-13 00:00:00'), y = 22, label = '  4 m/s  \n'), size = 5) +
  # Create the segment for the wind vector legend
# geom_segment(aes(x = as.Date('2022-09-13 00:00:00'), xend = as.Date('2022-09-14 00:00:00'), 
                #  y = 21.4, yend = 21.4), size = 0.5, alpha = 0.7) +
# scale_x_date(labels = date_format('%Y-%m-%d %H:%M:%S'), breaks = date_breaks('2 days')) +
# This should plot the label box to show the direction and strength of the wind ....???
  theme(legend.position = "none", 
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank())
winddir1

winddir2 <- ggplot(Hourly2) +
  geom_segment(aes(x = DateTime,
                   y = 0,
                   xend = DateTime + lubridate::dhours(windspeed * 1 * -cos((90-winddir) / 360 * 2 * pi)),
                   yend = windspeed * 1 * -sin((90-winddir) / 360 * 2 * pi),),
               arrow = arrow(length = unit(0.1, "cm")) ) +
  geom_point(aes(DateTime, 0), size = 0.5) +
  coord_fixed(3600) +
  labs(x = "Date", y = "", title = "Hourly Wind Data 2") +
  theme(legend.position = "none", 
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
winddir2

winddir <- ggarrange (winddir1, winddir2)
winddir

ggarrange(meanplotMP, meanplotKO, meanplotGC, meanplotOU, winddir1, winddir2, ncol = 2, nrow = 3)



# On the same plot (not working) ---------------------------------------------


# Plotting with temp and wind on the same plot (not useful, plots the wind lines way too long in proportion to the temperature)

# MP
tempwindMP <- ggplot(MPHourlyTemp, aes(x = Hourly1$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "red", size = 1) +
  geom_segment(aes(x = Hourly1$DateTime,
                   y = 15,
                   xend = Hourly1$DateTime + lubridate::dhours(Hourly1$windspeed * 1 * -cos((90-Hourly1$winddir) / 360 * 2 * pi)),
                   yend = Hourly1$windspeed * 1 * -sin((90-Hourly1$winddir) / 360 * 2 * pi)),
               arrow = arrow(length = unit(0.1, "cm")), yscale) +
  labs(x = "Date", y = "", title = "Miller's Point") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
tempwindMP


# GC
tempwindGC <- ggplot(GCHourlyTemp, aes(x = Hourly1$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "red", size = 1) +
  geom_segment(aes(x = Hourly1$DateTime,
                   y = 15,
                   xend = Hourly1$DateTime + lubridate::dhours(Hourly1$windspeed * 1 * -cos((90-Hourly1$winddir) / 360 * 2 * pi)),
                   yend = Hourly1$windspeed * 1 * -sin((90-Hourly1$winddir) / 360 * 2 * pi)),
               arrow = arrow(length = unit(0.1, "cm")) ) +
  labs(x = "Date", y = "", title = "Glencairn") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
tempwindGC



# KO
tempwindKO <- ggplot(KOHourlyTemp, aes(x = Hourly2$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "red", size = 1) +
  geom_segment(aes(x = Hourly2$DateTime,
                   y = 15,
                   xend = Hourly2$DateTime + lubridate::dhours(Hourly2$windspeed * 1 * -cos((90-Hourly2$winddir) / 360 * 2 * pi)),
                   yend = Hourly2$windspeed * 1 * -sin((90-Hourly2$winddir) / 360 * 2 * pi)),
               arrow = arrow(length = unit(0.1, "cm")) ) +
  labs(x = "Date", y = "", title = "Kommetjie") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
tempwindKO


# OU
tempwindOU <- ggplot(OUHourlyTemp, aes(x = Hourly2$DateTime, y = hourlymean)) + # plotting the mean
  geom_line(colour = "red", size = 1) +
  geom_segment(aes(x = Hourly2$DateTime,
                   y = 15,
                   xend = Hourly2$DateTime + lubridate::dhours(Hourly2$windspeed * 1.5 * -cos((90-Hourly2$winddir) / 360 * 2 * pi)),
                   yend = Hourly2$windspeed * 1 * -sin((90-Hourly2$winddir) / 360 * 2 * pi)),
               arrow = arrow(length = unit(0.1, "cm")) ) +
  labs(x = "Date", y = "", title = "Oudekraal") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
tempwindOU


ggarrange(tempwindMP, tempwindKO, tempwindGC, tempwindOU) # plots the wind lines way to big in proportion






