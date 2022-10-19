
###########################################################################
######################### W E A T H E R
# R E A D   D A T A
########################################################################
setwd( path.shapefiles )

# weather datasets
prcp.mmp = as.data.frame(fread("./daymet/mmp_w_prcp.csv"))
prcp.mun = as.data.frame(fread("./daymet/mx-mun_w_prcp.csv"))
tmax.mmp = as.data.frame(fread("./daymet/mmp_w_tmax.csv"))
tmax.mun = as.data.frame(fread("./daymet/mx-mun_w_tmax.csv"))
tmin.mmp = as.data.frame(fread("./daymet/mmp_w_tmin.csv"))

# mmp data
env = read.dta("./mmp_data/environs.dta")

# crop dataset (with geocodes and weather conditions)
crop = fread("./Mexico crop data/top_crops_sorted_w_info.csv")


#  C R E A T E   D A T A   F O R   M L   M O D E L S
###############################################################

# 1. RAW PRCP:
#             - yearly average total amount of rain per month
#             - yearly average amount of rain per day 
setwd(path.git)
source("./r_code/raw_prcp.R")

# 2. RAW TMAX:
#             - Yearly average temperature per month
#             - Yearly average of # of days above 30C per month.
#             - Yearly average of # CONSECUTIVE of days above 30C per month.
setwd(path.git)
source("./r_code/raw_tmax.R")

# 3. LONG-TERM NORM (1960-1979):
#             - For precipitation only (Tmax not available). Data coming from MMP environment data.
#             - Baseline prcp is total precipitation averaged over months (operation is "sum").
#             - Norm deviations are normalized by SD.
#             - Extremely Wet Days: Annual total precip from days when precip > 99th percentile.
setwd(path.git)
source("./r_code/norm_long-term.R")

# 4. SHORT-TERM NORM:
#             - Baseline prcp is total precipitation averaged over months (operation is "sum").
#             - Baseline tmax is # of days above 30C per month
#             - Norm deviations are normalized by SD.
#             - Proportion of months above the norm
setwd(path.git)
source("./r_code/norm_short-term.R")


# 5. SPELLS (WARM, WET and COLD):
#             - Warm spells: number of times with 6 consecutive days above
#               90th percentile of short-term norm (tmax).
#             - Wet spells: (prcp)
#             - Cold spells: number of times with 6 consecutive days below
#               10th percentile of short-term norm (tmin).

setwd(path.git)
source("./r_code/spells.R")


# 6. SINGLE-DAY MEASURES:
#             - ANNUAL WARMEST DAY: Annual max value of daily max temperature
#             - ANNUAL COLDEST DAY: Annual min value of daily max temperature
#             - ANNUAL WARMEST NIGHT: Annual max value of daily min temperature
setwd(path.git)
source("./r_code/single-days.R")


# 7. MULTIPLE-DAY MEASURES:
#             - WET DAYS: Annual total precip from days when precip > 1 cm.
#             - HEAVY PRCP: Annual count of days when precip > 20mm (> 2cm)
#             - MAX 5-DAY PRCP: Annual max consecutive 5-day precipitation amount
setwd(path.git)
source("./r_code/multiple-days.R")


# 8. PERCENTAGE-BASED MEASURES:
setwd(path.git)
source("./r_code/percentage_based.R")


# 9. GROWING SEASON MEASURES (May 1 - Oct 31)
setwd(path.git)
source("./r_code/growing_measures.R")


# DEVIATIONS FROM WEATHER INFORMATION BASED ON CROPS
#########################################################

# crop dataset: divide for state and for municipality
crop = geocode_char(crop)

crop_est = crop[ nchar(geocode_char) == 2 ]
crop_mun = crop[ nchar(geocode_char) > 2 ]


# weather dataset: add ID for state and for municipality
tmin.mmp = geocode_char(tmin.mmp)

# 10. YEARLY DEVIATION: tmax BASED ON CROP PRODUCTION
# weather data at the state level
devs = weather_deviations(tmin.mmp, crop_est, weather="tmin", type="est")
