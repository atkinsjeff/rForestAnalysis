# Necessary packages

library("phenocamr")
library("phenor")
library("maps")
library("raster")
#library("phenocamapi")


# download a three day time series for a deciduous broadleaf forests at
# the Pace Estate site in central Virginia and estimate phenophases (Spring and Autumn)

phenocamr::download_phenocam(
  frequency = 3,
  veg_type = "DB",
  roi_id = 1000,
  site = "pace",
  phenophase = TRUE,
  out_dir = "./data/phenology"
  
)

# load the time series data
df <- read.table("./data/phenology/pace_DB_1000_3day.csv", header = TRUE, sep = ",")

# read in the transition date file
td <- read.table("./data/phenology/pace_DB_1000_3day_transition_dates.csv",
                 header = TRUE,
                 sep = ",")


# select the rising (spring dates) for 25% threshold of Gcc 90
td <- td[td$direction == "rising" & td$gcc_value == "gcc_90",]

# create a simple line graph of the smooth Green Chromatic Coordinate (Gcc)
# and add points for transition dates
x11()
plot(as.Date(df$date), df$smooth_gcc_90, type = "l", xlab = "Date",
     ylab = "Gcc (90th percentile)")
points(x = as.Date(td$transition_25, origin = "1970-01-01"),
       y = td$threshold_25,
       pch = 19,
       col = "red")

# the first step in phenocam processing is flagging of the outliers
# on the file you visualized in the previous step
detect_outliers("./data/phenology/pace_DB_1000_3day.csv",
                out_dir = "./data/phenology")

# the second step involves smoothing the data using an optimization approach
# we force the procedure as it will be skipped if smoothed data is already
# available
smooth_ts("./data/phenology/pace_DB_1000_3day.csv",
          out_dir = "./data/phenology",
          force = TRUE)

# the third and final step is the generation of phenological transition dates
td <- phenophases("./data/phenology/pace_DB_1000_3day.csv",
                  internal = TRUE,
                  upper_thresh = 0.8)

# Now we have manually set the parameters that were default for our first plot. Note, that here is also a lower and a middle threshold parameter, the order matters so always use the relevant parameter (for parameters, check transition_dates())
# 
# Now we can again plot the annual pattern with the transition dates.

# split out the rising (spring) component for Gcc 90th
td <- td$rising[td$rising$gcc_value == "gcc_90",]

# we can now visualize the upper threshold
x11()
plot(as.Date(df$date), df$smooth_gcc_90, type = "l",
     xlab = "Date",
     ylab = "Gcc (90th percentile)")
points(x = as.Date(td$transition_80, origin = "1970-01-01"),
       y = td$threshold_80,
       pch = 19,
       col = "red")










####################################

# download a three day time series for the San Gabriel region

# first lets list the sites
df.site <- phenocamr::list_sites()


phenocamr::download_phenocam(
  frequency = 3,
  veg_type = "GR",
  roi_id = 2000,
  site = "jasperridge",
  phenophase = TRUE,
  out_dir = "./data/phenology"
)




# load the time series data
df.jasper <- read.table("./data/phenology/jasperridge_GR_2000_3day.csv", header = TRUE, sep = ",")

# read in the transition date file
td.jasper <- read.table("./data/phenology/jasperridge_GR_2000_3day_transition_dates.csv",
                 header = TRUE,
                 sep = ",")


# select the rising (spring dates) for 25% threshold of Gcc 90
td.jasper <- td.jasper[td.jasper$direction == "rising" & td.jasper$gcc_value == "gcc_90",]

# create a simple line graph of the smooth Green Chromatic Coordinate (Gcc)
# and add points for transition dates
x11()
plot(as.Date(df.jasper$date), df.jasper$smooth_gcc_90, type = "l", xlab = "Date",
     ylab = "Gcc (90th percentile)", col = "black")
points(x = as.Date(td.jasper$transition_25, origin = "1970-01-01"),
       y = td.jasper$threshold_25,
       pch = 19,
       col = "red")
lines(as.Date(df$date), df$smooth_gcc_90, type = "l", col = "dodgerblue")
points(x = as.Date(td$transition_80, origin = "1970-01-01"),
       y = td$threshold_25,
       pch = 19,
       col = "red")
       



##################
pace_img <- get_midday_list('pace')



