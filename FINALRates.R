# Calculating the rate of change of temperature
# Have to add columns, going to be busy, hence the separate script
# 31 October 2022

library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(readxl)


# Miller's Point ----------------------------------------------------------

# First load all the data into objects
Temp1 <- read.csv("Data/MillersPoint/MP_Temp1.csv")
Temp2 <- read.csv("Data/MillersPoint/MP_Temp2.csv")
Temp3 <- read.csv("Data/MillersPoint/MP_Temp3.csv")

# Importing dates with times into new column so R can read it properly
Temp1$datetime = mdy_hms(Temp1$Date.Time)
Temp2$datetime = mdy_hms(Temp2$Date.Time)
Temp3$datetime = mdy_hms(Temp3$Date.Time)



# First to add a column called Tempdiff with the temperature differences of each reading compared to the previous one (taken 30s before)
# Done for each dataset

Temp1$Tempdiff <- Temp1$Temp - lag(Temp1$Temp, default = Temp1$Temp[length(Temp1$Temp)], k = 1)
Temp2$Tempdiff <- Temp2$Temp - lag(Temp2$Temp, default = Temp2$Temp[length(Temp2$Temp)], k = 1)
Temp3$Tempdiff <- Temp3$Temp - lag(Temp3$Temp, default = Temp3$Temp[length(Temp3$Temp)], k = 1)

# Calculating the Tempdiff means
meandiffMP <- data.frame(cbind(Temp1$Tempdiff, Temp2$Tempdiff, Temp3$Tempdiff))
meandiffMP$Mean <- ave(meandiffMP$X1, meandiffMP$X2, meandiffMP$X3) # creating new column of the mean

# Plotting the temperature differences
MP_tempdiff <- ggplot(Temp1, aes(x = datetime, y = Tempdiff)) + 
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2$Tempdiff), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3$Tempdiff), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meandiffMP$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Rate of change (°C/30s)", title = "Miller's Point") +
  ylim(-2, 2) +
  theme_bw()
MP_tempdiff


# Glencairn ---------------------------------------------------------------

Temp1G <- read.csv("Data/Glencairn/GC_Temp1.csv")
Temp2G <- read.csv("Data/Glencairn/GC_Temp2.csv")
Temp3G <- read.csv("Data/Glencairn/GC_Temp3.csv")
Temp4G <- read.csv("Data/Glencairn/GC_Temp4.csv")

Temp1G$datetime = mdy_hms(Temp1G$Date.Time)
Temp2G$datetime = mdy_hms(Temp2G$Date.Time)
Temp3G$datetime = mdy_hms(Temp3G$Date.Time)
Temp4G$datetime = mdy_hms(Temp4G$Date.Time)


# Creating the Tempdiff column
Temp1G$Tempdiff <- Temp1G$Temp - lag(Temp1G$Temp, default = Temp1G$Temp[length(Temp1G$Temp)], k = 1)
Temp2G$Tempdiff <- Temp2G$Temp - lag(Temp2G$Temp, default = Temp2G$Temp[length(Temp2G$Temp)], k = 1)
Temp3G$Tempdiff <- Temp3G$Temp - lag(Temp3G$Temp, default = Temp3G$Temp[length(Temp3G$Temp)], k = 1)
Temp4G$Tempdiff <- Temp4G$Temp - lag(Temp4G$Temp, default = Temp4G$Temp[length(Temp4G$Temp)], k = 1)

# Calculating the Tempdiff means
meandiffGC <- data.frame(cbind(Temp1G$Tempdiff, Temp2G$Tempdiff, Temp3G$Tempdiff))
meandiffGC$Mean <- ave(meandiffGC$X1, meandiffGC$X2, meandiffGC$X3) # creating new column of the mean

# Plotting the temperature differences
GC_tempdiff <- ggplot(Temp1G, aes(x = datetime, y = Tempdiff)) + 
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2G$Tempdiff), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3G$Tempdiff), colour = "navyblue", alpha = 0.8) +
  geom_line(aes(y = Temp4G$Tempdiff), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meandiffGC$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Rate of change (°C/30s)", title = "Glencairn") +
  ylim(-2, 2) +
  theme_bw()
GC_tempdiff


# Kommetjie ---------------------------------------------------------------

Temp1K <- read.csv("Data/Kommetjie/KO_Temp1.csv")
Temp2K <- read.csv("Data/Kommetjie/KO_Temp2.csv")
Temp3K <- read.csv("Data/Kommetjie/KO_Temp4.csv") # Third temp didn't export

Temp1K$datetime = mdy_hms(Temp1K$Date.Time)
Temp2K$datetime = mdy_hms(Temp2K$Date.Time)
Temp3K$datetime = mdy_hms(Temp3K$Date.Time)


# Creating the Tempdiff column
Temp1K$Tempdiff <- Temp1K$Temp - lag(Temp1K$Temp, default = Temp1K$Temp[length(Temp1K$Temp)], k = 1)
Temp2K$Tempdiff <- Temp2K$Temp - lag(Temp2K$Temp, default = Temp2K$Temp[length(Temp2K$Temp)], k = 1)
Temp3K$Tempdiff <- Temp3K$Temp - lag(Temp3K$Temp, default = Temp3K$Temp[length(Temp3K$Temp)], k = 1)

# Calculating the Tempdiff means
meandiffKO <- data.frame(cbind(Temp1K$Tempdiff, Temp2K$Tempdiff, Temp3K$Tempdiff))
meandiffKO$Mean <- ave(meandiffKO$X1, meandiffKO$X2, meandiffKO$X3) # creating new column of the mean

# Plotting the temperature differences
KO_tempdiff <- ggplot(Temp1K, aes(x = datetime, y = Tempdiff)) + 
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2K$Tempdiff), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3K$Tempdiff), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meandiffKO$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Rate of change (°C/30s)", title = "Kommetjie") +
  ylim(-2, 2) +
  theme_bw()
KO_tempdiff



# Oudekraal ---------------------------------------------------------------

Temp1O <- read.csv("Data/Oudekraal/OU_Temp1.csv")
Temp2O <- read.csv("Data/Oudekraal/OU_Temp2.csv")
Temp3O <- read.csv("Data/Oudekraal/OU_Temp3.csv") # Fourth temp didn't export

Temp1O$datetime = mdy_hms(Temp1O$Date.Time)
Temp2O$datetime = mdy_hms(Temp2O$Date.Time)
Temp3O$datetime = mdy_hms(Temp3O$Date.Time)

?lag
# Creating the Tempdiff column
Temp1O$Tempdiff <- Temp1O$Temp - lag(Temp1O$Temp, default = Temp1O$Temp[length(Temp1O$Temp)], k = 1)
Temp2O$Tempdiff <- Temp2O$Temp - lag(Temp2O$Temp, default = Temp2O$Temp[length(Temp2O$Temp)], k = 1)
Temp3O$Tempdiff <- Temp3O$Temp - lag(Temp3O$Temp, default = Temp3O$Temp[length(Temp3O$Temp)], k = 1)

# Calculating the Tempdiff means
meandiffOU <- data.frame(cbind(Temp1O$Tempdiff, Temp2O$Tempdiff, Temp3O$Tempdiff))
meandiffOU$Mean <- ave(meandiffOU$X1, meandiffOU$X2, meandiffOU$X3) # creating new column of the mean

# Plotting the temperature differences
OU_tempdiff <- ggplot(Temp1O, aes(x = datetime, y = Tempdiff)) + 
  geom_line(colour = "darkorange1", alpha = 0.8) +
  geom_line(aes(y = Temp2O$Tempdiff), colour = "deepskyblue3", alpha = 0.8)+
  geom_line(aes(y = Temp3O$Tempdiff), colour = "firebrick", alpha = 0.8) +
  geom_line(aes(y = meandiffOU$Mean), colour = "black", alpha = 0.8) + # with the mean
  labs(x = "Date", y = "Rate of change (°C/30s)", title = "Oudekraal") +
  ylim(-2, 2) +
  theme_bw()
OU_tempdiff


# All rates of change in temperature plots --------------------------------

ggarrange(MP_tempdiff, KO_tempdiff, GC_tempdiff, OU_tempdiff, common.legend = TRUE)



# Let's try with just the means? ------------------------------------------

MP_tempdiffmeans <- ggplot(Temp1, aes(x = datetime, y = Tempdiff)) + 
  geom_line(aes(y = meandiffMP$Mean), colour = "firebrick", alpha = 0.8) +
  labs(x = "Date/time", y = "Mean rate of change (°C/hour)", title = "Miller's Point") +
  ylim(-1, 1) +
  theme_bw()
MP_tempdiffmeans


GC_tempdiffmeans <- ggplot(Temp1G, aes(x = datetime, y = Tempdiff)) + 
  geom_line(aes(y = meandiffGC$Mean), colour = "firebrick", alpha = 0.8) +
  labs(x = "Date/time", y = "Mean rate of change (°C/hour)", title = "Glencairn") +
  ylim(-1, 1) +
  theme_bw()
GC_tempdiffmeans


KO_tempdiffmeans <- ggplot(Temp1K, aes(x = datetime, y = Tempdiff)) + 
  geom_line(aes(y = meandiffKO$Mean), colour = "firebrick", alpha = 0.8) +
  labs(x = "Date/time", y = "Mean rate of change (°C/hour)", title = "Kommetjie") +
  ylim(-1, 1) +
  theme_bw()
KO_tempdiffmeans


OU_tempdiffmeans <- ggplot(Temp1O, aes(x = datetime, y = Tempdiff)) + 
  geom_line(aes(y = meandiffOU$Mean), colour = "firebrick", alpha = 0.8) + 
  labs(x = "Date/time", y = "Mean rate of change (°C/hour)", title = "Oudekraal") +
  ylim(-1, 1) +
  theme_bw()
OU_tempdiffmeans


# All together

ggarrange(MP_tempdiffmeans, KO_tempdiffmeans, GC_tempdiffmeans, OU_tempdiffmeans, common.legend = TRUE)



