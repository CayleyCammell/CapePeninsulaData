# Final plots for data collected at all four sites
# Temperature data
# 5 October 2022


library(tidyverse)
library(ggpubr)
library(ggplot2)
library(dplyr)
library(lubridate)


# Miller's Point ----------------------------------------------------------

# Accelerometers ----------------------------------------------------------
# AJ's method -------------------------------------------------------------

library(tibbletime)

# Logger no 1

Acc1M <- read_csv("Data/MillersPoint/MP_Acc1.csv",
                  name_repair = "universal", # Used this so names work everywhere, without quoting: df$name and with(df, name) and lm(name1 ~ name2, data = df) and dplyr::select(df, name) all work.
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"), # i = integer, c = character, d = double
                  skip = 1)
Acc1M$A_gM <- Acc1M$X_g + Acc1M$Y_g + Acc1M$Z_g

Acc1M$date_time <- mdy_hms(Acc1M$date_time)

# Make rolling functions (standard to use later)

mean_roll_60 <- rollify(mean, window = 60)

Acc1_smoothedM <- Acc1M %>% 
  mutate(A_g_60AM = mean_roll_60(A_gM))


# Logger no 2

Acc2M <- read_csv("Data/MillersPoint/MP_Acc2.csv",
                  name_repair = "universal", # Used this so names work everywhere, without quoting: df$name and with(df, name) and lm(name1 ~ name2, data = df) and dplyr::select(df, name) all work.
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"), # i = integer, c = character, d = double
                  skip = 1)
Acc2M$date_time <- mdy_hms(Acc2M$date_time)

Acc2M$A_gM <- Acc2M$X_g + Acc2M$Y_g + Acc2M$Z_g

Acc2_smoothedM <- Acc2M %>% 
  mutate(A_g_60BM = mean_roll_60(A_gM))


# Logger no 3

Acc3M <- read_csv("Data/MillersPoint/MP_Acc3.csv",
                  name_repair = "universal", # Used this so names work everywhere, without quoting: df$name and with(df, name) and lm(name1 ~ name2, data = df) and dplyr::select(df, name) all work.
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"), # i = integer, c = character, d = double
                  skip = 1)
Acc3M$date_time <- mdy_hms(Acc3M$date_time)

Acc3M$A_gM <- Acc3M$X_g + Acc3M$Y_g + Acc3M$Z_g

Acc3_smoothedM <- Acc3M %>% 
  mutate(A_g_60CM = mean_roll_60(A_gM))


# All accelerometers for MP -----------------------------------------------

MP_acc <- ggplot(Acc1_smoothedM, aes(x = date_time, y = A_g_60AM)) + 
  geom_line(colour = "darkorange1") +
  geom_line(aes(y = Acc2_smoothedM$A_g_60BM), colour = "deepskyblue3")+
  geom_line(aes(y = Acc3_smoothedM$A_g_60CM), colour = "firebrick") +
  labs(x = "Date", y = "Acceleration (m/s)", title = "Miller's Point") +
  theme_bw()
MP_acc



# Glencairn ---------------------------------------------------------------


# Accelerometers ----------------------------------------------------------

# Logger no 1

Acc1G <- read_csv("Data/Glencairn/GC_Acc1.csv",
                  name_repair = "universal",
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc1G$A_gG <- Acc1G$X_g + Acc1G$Y_g + Acc1G$Z_g

Acc1G$date_time <- mdy_hms(Acc1G$date_time)

Acc1_smoothedG <- Acc1G %>% 
  mutate(A_g_60AG = mean_roll_60(A_gG))


# Logger no 2

Acc2G <- read_csv("Data/Glencairn/GC_Acc2.csv",
                  name_repair = "universal",
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"), 
                  skip = 1)
Acc2G$date_time <- mdy_hms(Acc2G$date_time)

Acc2G$A_gG <- Acc2G$X_g + Acc2G$Y_g + Acc2G$Z_g

Acc2_smoothedG <- Acc2G %>% 
  mutate(A_g_60BG = mean_roll_60(A_gG))


# Logger no 3

Acc3G <- read_csv("Data/Glencairn/GC_Acc3.csv",
                  name_repair = "universal", 
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc3G$date_time <- mdy_hms(Acc3G$date_time)

Acc3G$A_gG <- Acc3G$X_g + Acc3G$Y_g + Acc3G$Z_g

Acc3_smoothedG <- Acc3G %>% 
  mutate(A_g_60CG = mean_roll_60(A_gG))


# Logger no 4

Acc4G <- read_csv("Data/Glencairn/GC_Acc4.csv",
                  name_repair = "universal", 
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc4G$date_time <- mdy_hms(Acc4G$date_time)

Acc4G$A_gG <- Acc4G$X_g + Acc4G$Y_g + Acc4G$Z_g

Acc4_smoothedG <- Acc4G %>% 
  mutate(A_g_60DG = mean_roll_60(A_gG))


# All accelerometers for GC -----------------------------------------------

GC_acc <- ggplot(Acc1_smoothedG, aes(x = date_time, y = A_g_60AG)) + 
  geom_line(colour = "darkorange1") +
  geom_line(aes(y = Acc2_smoothedG$A_g_60BG), colour = "deepskyblue3")+
  geom_line(aes(y = Acc3_smoothedG$A_g_60CG), colour = "navyblue") +
  geom_line(aes(y = Acc4_smoothedG$A_g_60DG), colour = "firebrick") +
  labs(x = "Date", y = "Acceleration (m/s)", title = "Glencairn") +
  theme_bw()
GC_acc


# Kommetjie ---------------------------------------------------------------


# Accelerometers ----------------------------------------------------------

# Logger no 1

Acc1K <- read_csv("Data/Kommetjie/KO_Acc1.csv",
                  name_repair = "universal",
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc1K$A_gK <- Acc1K$X_g + Acc1K$Y_g + Acc1K$Z_g

Acc1K$date_time <- mdy_hms(Acc1K$date_time)

Acc1_smoothedK <- Acc1K %>% 
  mutate(A_g_60AK = mean_roll_60(A_gK))


# Logger no 2

Acc2K <- read_csv("Data/Kommetjie/KO_Acc2.csv",
                  name_repair = "universal",
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"), 
                  skip = 1)
Acc2K$date_time <- mdy_hms(Acc2K$date_time)

Acc2K$A_gK <- Acc2K$X_g + Acc2K$Y_g + Acc2K$Z_g

Acc2_smoothedK <- Acc2K %>% 
  mutate(A_g_60BK = mean_roll_60(A_gK))


# Logger no 4

Acc4K <- read_csv("Data/Kommetjie/KO_Acc4.csv",
                  name_repair = "universal", 
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc4K$date_time <- mdy_hms(Acc4K$date_time)

Acc4K$A_gK <- Acc4K$X_g + Acc4K$Y_g + Acc4K$Z_g

Acc4_smoothedK <- Acc4K %>% 
  mutate(A_g_60DK = mean_roll_60(A_gK))


# All accelerometers for KO -----------------------------------------------

KO_acc <- ggplot(Acc1_smoothedK, aes(x = date_time, y = A_g_60AK)) + 
  geom_line(colour = "darkorange1") +
  geom_line(aes(y = Acc2_smoothedK$A_g_60BK), colour = "deepskyblue3")+
  geom_line(aes(y = Acc4_smoothedK$A_g_60DK), colour = "firebrick") +
  labs(x = "Date", y = "Acceleration (m/s)", title = "Kommetjie") +
  theme_bw()
KO_acc



# Oudekraal ---------------------------------------------------------------


# Accelerometers ----------------------------------------------------------

# Logger no 1

Acc1O <- read_csv("Data/Oudekraal/OU_Acc1.csv",
                  name_repair = "universal",
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc1O$A_gO <- Acc1O$X_g + Acc1O$Y_g + Acc1O$Z_g

Acc1O$date_time <- mdy_hms(Acc1O$date_time)

Acc1_smoothedO <- Acc1O %>% 
  mutate(A_g_60AO = mean_roll_60(A_gO))


# Logger no 2

Acc2O <- read_csv("Data/Oudekraal/OU_Acc2.csv",
                  name_repair = "universal",
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"), 
                  skip = 1)
Acc2O$date_time <- mdy_hms(Acc2O$date_time)

Acc2O$A_gO <- Acc2O$X_g + Acc2O$Y_g + Acc2O$Z_g

Acc2_smoothedO <- Acc2O %>% 
  mutate(A_g_60BO = mean_roll_60(A_gO))


# Logger no 3

Acc3O <- read_csv("Data/Oudekraal/OU_Acc3.csv",
                  name_repair = "universal", 
                  col_names = c("n", "date_time", "X_g", "Y_g", "Z_g"),
                  col_types = list("i", "c", "d", "d", "d"),
                  skip = 1)
Acc3O$date_time <- mdy_hms(Acc3O$date_time)

Acc3O$A_gO <- Acc3O$X_g + Acc3O$Y_g + Acc3O$Z_g

Acc3_smoothedO <- Acc3O %>% 
  mutate(A_g_60CO = mean_roll_60(A_gO))


# All accelerometers for OU-----------------------------------------------

OU_acc <- ggplot(Acc1_smoothedO, aes(x = date_time, y = A_g_60AO)) + 
  geom_line(colour = "darkorange1") +
  geom_line(aes(y = Acc2_smoothedO$A_g_60BO), colour = "deepskyblue3")+
  geom_line(aes(y = Acc3_smoothedO$A_g_60CO), colour = "firebrick") +
  labs(x = "Date", y = "Acceleration (m/s)", title = "Oudekraal") +
  theme_bw()
OU_acc

# Final Acc ---------------------------------------------------------------

ggarrange(MP_acc, KO_acc, GC_acc, OU_acc, common.legend = TRUE)









# Accelerometer means(NOT USEFUKL-------------------------------------------

# MP

AmeansMP <- data.frame(cbind(Acc1_smoothedM$A_g_60AM, Acc2_smoothedM$A_g_60BM, Acc3_smoothedM$A_g_60CM)) # combining temps from the 3 df
AmeansMP$Mean <- ave(AmeansMP$X1, AmeansMP$X2, AmeansMP$X3) # creating new column of the mean

AmeanplotMP <- ggplot(AmeansMP, aes(x = Acc1_smoothedM$date_time, y = Mean)) + # plotting the mean acc
  geom_line(colour = "firebrick") +
  labs(x = "Date/time", y = "Acceleration (m/s)", title = "Miller's Point") +
  theme_bw()
AmeanplotMP

# KO

AmeansK <- data.frame(cbind(Acc1_smoothedK$A_g_60AK, Acc2_smoothedK$A_g_60BK, Acc4_smoothedK$A_g_60DK)) # combining temps from the 3 df
AmeansK$Mean <- ave(AmeansK$X1, AmeansK$X2, AmeansK$X3) # creating new column of the mean

AmeanplotK <- ggplot(AmeansK, aes(x = Acc1_smoothedK$date_time, y = Mean)) + # plotting the mean acc
  geom_line(colour = "firebrick") +
  labs(x = "Date/time", y = "Acceleration (m/s)", title = "Kommetjie") +
  theme_bw()
AmeanplotK # WHY WON'T THIS LOAD


# GC

AmeansG <- data.frame(cbind(Acc1_smoothedG$A_g_60AG, Acc2_smoothedG$A_g_60BG, Acc3_smoothedG$A_g_60CG)) # combining temps from the 3 df
AmeansG$Mean <- ave(AmeansG$X1, AmeansG$X2, AmeansG$X3) # creating new column of the mean

AmeanplotG <- ggplot(AmeansG, aes(x = Acc1_smoothedG$date_time, y = Mean)) + # plotting the mean acc
  geom_line(colour = "firebrick") +
  labs(x = "Date/time", y = "Acceleration (m/s)", title = "Glencairn") +
  theme_bw()
AmeanplotG

# OU

AmeansO <- data.frame(cbind(Acc1_smoothedO$A_g_60AO, Acc2_smoothedO$A_g_60BO, Acc3_smoothedO$A_g_60CO)) # combining temps from the 3 df
AmeansO$Mean <- ave(AmeansO$X1, AmeansO$X2, AmeansO$X3) # creating new column of the mean

AmeanplotO <- ggplot(AmeansO, aes(x = Acc1_smoothedO$date_time, y = Mean)) + # plotting the mean acc
  geom_line(colour = "firebrick") +
  labs(x = "Date/time", y = "Acceleration (m/s)", title = "Oudekraal") +
  theme_bw()
AmeanplotO

# ggarrange(AmeanplotMP, AmeanplotK, AmeanplotG, AmeanplotO, common.legend = TRUE)
# Kommetjie one not working yet

ggarrange(AmeanplotMP, AmeanplotG, AmeanplotO, common.legend = TRUE)
