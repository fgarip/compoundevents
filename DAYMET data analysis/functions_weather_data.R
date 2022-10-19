############################################################
#########    F U N C T I O N S   W E A T H E R    #########
############################################################


# CRUDE DATA: MONTHLY AVERAGE BY YEAR
########################################################

#  STATE NAMES
mmp_state_names = function(data){
  # get only state names that appear in mmp data
  state.names = sapply( data$geocode, function(x) {
    if(nchar(x) == 8) st = substr(x, start = 1, stop = 1) else {
      if(nchar(x) == 4) st = substr(x, start = 1, stop = 1)  else st = substr(x, start = 1, stop = 2)
      }
    return(as.numeric(st))
  } )
  return(state.names)
}


# MONTH-YEAR COLNAMES
month_year_colnames = function(data, weather){
  # add column names
  months = c("jan", "feb", "mar", "apr",
             "may", "jun", "jul", "ago",
             "sep", "oct", "nov", "dec")
  year.month = c()
  # all colnames with relevant years
  all_colnames = colnames(data[, grep(paste(1980:2017, collapse="|"), colnames(data))])
  # years
  years = unique(substr(all_colnames, 2, 5))
  years = as.character(1980:2017)
  for(y in years) year.month = c(year.month, paste0(months, "-", y))
  year.month = paste0(weather,"-",year.month)

  return(year.month)
}

# ACCOUNT FOR CONSECUTIVE DAYS
consecutive_days = function(chunk){
 consec = apply(chunk, 1, function(x) {
   # count # of consecutive TRUE values
   rle_obj = rle( as.vector(x) )
   # get only TRUE values
   vals_true = rle_obj$lengths[rle_obj$values]
   # subset to obtain repeated items only
   vals_true = vals_true[ vals_true != 1 ]
   if(length(vals_true)==0) vals_true = 0 else vals_true = sum(vals_true)
 } ) 
 return(consec)
}


# ACCOUNT FOR SIX CONSECUTIVE DAYS
consecutive_days_spec = function(chunk, cons_numb){
  consec = apply(chunk, 1, function(x) {
    # count # of consecutive TRUE values
    rle_obj = rle( as.vector(x) )
    # get only TRUE values
    vals_true = rle_obj$lengths[rle_obj$values]
    # obtain sequences for number of TRUE occurrences
    seq = sequence(vals_true)
    # divide each item in the sequence by cons_numb (e.g. 6).
    # Number of specific consecutive occurrences (cons_numb) will be 
    # all divisions >= 1. E.g. if cons_numb==6, all divisions above 1
    # will point to an occurrence where there were 6 consecutive occurrences.
    true_occur = sum(seq/cons_numb >= 1)
    return(true_occur)
  } ) 
  return(consec)
}


# MAXIMUM NUMBER OF CONSECUTIVE DAYS IN A YEAR
consecutive_days_max = function(chunk){
  consec = apply(chunk, 1, function(x) {
    # count # of consecutive TRUE values
    rle_obj = rle( as.vector(x) )
    # get only TRUE values
    vals_true = rle_obj$lengths[rle_obj$values]
    # obtain the maximum number of consecutive TRUE values. 
    max_val = max(vals_true)
    return(max_val)
  } ) 
  return(consec)
}


# CONVERT WEATHER TO MONTHLY CHUNKS
convert_weather_monthly = function(data, weather, operation){

  # add state names to data
  data[,"state"] = mmp_state_names(data)

  # weather_monthly will store values.
  weather_monthly = data[,c("geocode", "state")]

  # get operation parts (if any)
  operation_parts = unlist(strsplit(operation, split="-"))
  # define operation
  if(length(operation_parts) == 1){
    operation = operation
  }else{
    operation=operation_parts[1]
    spec = operation_parts[2]
    }
  
  # all colnames with relevant years
  all_colnames = colnames(data[, grep(paste(1980:2017, collapse="|"), colnames(data))])
  # years
  years = unique(substr(all_colnames, 2, 5))
  # months: 01 - 12
  m = as.character(1:12) 
  months = ifelse(nchar(m)==1, paste0("0",m), m)
  # loop through all years
  for(y in years){
    for(m in months){
      key = paste0("X",y,".",m)
      range.month = grep(key, colnames(data))
      
      # Operations
      if(operation=="sum"){
        operation.monthly = rowSums(data[,range.month])
      } else if(operation=="mean"){
        operation.monthly = rowMeans(data[,range.month])
      } else if(operation=="above30"){
        if(spec == "notconsecutive"){
          operation.monthly = rowSums( data[,range.month] > 30 )  
        } else if(spec == "consecutive") {
          operation.monthly = consecutive_days(data[,range.month] > 30)
        }
      } else if(operation=="90th_percentile"){
        
      }
      # Bind to storing df weathermonthly
      weather_monthly = cbind( weather_monthly, operation.monthly )
    }
  }
  # add month-year colnames
  colnames(weather_monthly) = c("geocode",
                                "state",
                                month_year_colnames(data, weather) )
  return(weather_monthly)
}


# CRUDE DATA FOR WEATHER
crude_weather_yearly = function(data, weather, operation){
  
  # weather monthly
  weather_monthly = convert_weather_monthly(data, weather, operation)
  
  # yearly averages
  years = as.character(1980:2017)
  weather_yearly = weather_monthly[,c("geocode", "state")]
  for(y in years){
    idx_year = grep(y, colnames(weather_monthly))
    year_mean = rowMeans(weather_monthly[,idx_year])
    weather_yearly = cbind(weather_yearly, year_mean)
  }
  # update weather_yearl colnames
  colnames(weather_yearly) = c("geocode", "state", paste0(weather,"-",years))
  
  return(weather_yearly)
}


# COMPUTE SHORT-TERM NORM (1980-1984)
short_term_norm = function(data, weather, operation){
  
  # weather monthly
  weather_monthly = convert_weather_monthly(data, weather, operation)
  
  # state names
  mmp_st_names = mmp_state_names(data)
  
  # years for short term norm: 1980-1984
  years.query = paste(1980:1984,collapse="|")
  years.ids = grep(years.query,colnames(weather_monthly))
  
  # norm mean and sd
  norm_mean = rowMeans( weather_monthly[,years.ids] )
  norm_sd = apply( weather_monthly[,years.ids], 1, sd )
  
  # get short term norm
  shortterm_norm = cbind(state=mmp_st_names, norm_mean=norm_mean, norm_sd=norm_sd)
  
  return(shortterm_norm)
}



# Norm yearly
norm_weather_yearly = function(data, weather){

  # yearly averages
  years = as.character(1985:2017)
  weather_yearly = data[,c("geocode", "state")]
  for(y in years){
    idx_year = grep(y, colnames(data))
    year_mean = rowMeans(data[,idx_year])
    weather_yearly = cbind(weather_yearly, year_mean)
  }
  # update weather_yearl colnames
  colnames(weather_yearly) = c("geocode", "state", paste0(weather,"-",years))

  return(weather_yearly)
}






# DEVIATIONS FROM WEATHER INFO BASED ON CROPS
##################################################

# ADD GEOCODE AS STRING
geocode_char = function(x){
  # convert geocode to character of length == 9.
  x[,"geocode_char"] = as.character(x$geocode)
  if(nchar(x$geocode_char[1])>5){
    x[,"geocode_char"] = ifelse( nchar(x$geocode_char) == 8, paste0("0",x$geocode_char), x$geocode_char )
  }else{
    x[,geocode_char := ifelse( nchar(geocode_char) == 1 | nchar(geocode_char) == 4, paste0("0",geocode_char), geocode_char  )]

  }
  return(x)
}

# ADD GEOCODE FOR STATE AND MUNICIPALITY
geocode_ID = function(x){
    # state ID
    x[,"geocode_est"] = substr(x$geocode_char, 1, 2)
    if(sum(nchar(x$geocode_char) > 2) > 0){
      # municipality ID
      x[,"geocode_mun"] = substr(x$geocode_char, 1, 5)
    }
    return(x)
}


convert_date = function(x){
  # CONVERT DATE to format of columns in mmp weather datasets (i.e. tmin, tmax, prcp)
  dates_map = list(jan="01",feb="02",mar="03",apr="04",may="05",jun="06",
                   jul="07",aug="08",sep="09",oct="10",nov="11",dec="12")
  # months
  months = strsplit(x[,"month_harvested"],split="-")
  months_mapped = lapply(months, function(x) unlist(dates_map[x]) )

  months_matrix = do.call(rbind,months_mapped)
  colnames(months_matrix) = c("init","end")
  # months = strsplit(x,split="-")
  # months_mapped = lapply(months, function(x) unlist(dates_map[x][m]) )
  return(months_matrix)
}


handle_doubles = function(x){

  # handle doubled values
  check_matrix = table(x$geocode_char,x$YearAgricola)==2
  check_ids = which(check_matrix, arr.ind = T)

  # obtain geocode and year of doubled entries
  geo_year = apply(check_ids,1, function(x) {
      geo = rownames(check_matrix)[x[1]]
      year = colnames(check_matrix)[x[2]]
      return(c(geo,year))
    })

  # change YearAgricola for doubled entries: add "-1" or "-2" to YearAgricola
  for(i in 1:ncol(geo_year)){
    x[geocode_char==geo_year[1,i] & YearAgricola==as.numeric(geo_year[2,i]), YearAgricola := paste0(YearAgricola,"-",1:.N)]
  }
  return(x)
}

# LONG TO WIDE FOR CROP
map_months = function(x){

  # divide geocode between for state and municiplaity
  x = geocode_ID(x)

  # change year of double values
  x[,YearAgricola:=as.character(YearAgricola)]
  x = handle_doubles(x)
  piece = as.data.frame(x[,c("YearAgricola","month_harvested"), with=FALSE])
  piece_str = convert_date(piece)
  piece_str = cbind(piece, piece_str)

  # add ID for cases in which initial month is later than end month (e.g. dec > jan)
  piece_str[,"ID_go_to_next_year"] = as.character(piece_str[,"init"]) > as.character(piece_str[,"end"])
  # add ID for cases in which adding 1 to YearAgricola is higher than YearAgricola.
  # this identifies cases in the last year for which the data is available.
  which_is_max_year = max(as.numeric(piece_str[,"YearAgricola"] ))
  piece_str[,"year_max"] = as.numeric(piece_str[,"YearAgricola"] ) + 1 > which_is_max_year

  # construct month_init (this is easy)
  piece_str[,"month_init"] = paste0("X",piece_str[,"YearAgricola"],".",piece_str[,"init"])

  # construct month_end for cases in which month_end needs to add one year to YearAgricola
  select_rows = piece_str[,"ID_go_to_next_year"]
  piece_str[select_rows ,"month_end"] = paste0("X",as.numeric(piece_str[select_rows,"YearAgricola"])+1,".",piece_str[select_rows,"end"])

  # construct month_end for cases in which YearAgricola is the max Year
  select_rows = piece_str[,"ID_go_to_next_year"] & piece_str[,"year_max"]
  piece_str[select_rows,"month_end"] = paste0("X",piece_str[select_rows,"YearAgricola"],".",piece_str[select_rows,"init"])

  # # map initial and last months and create column in format of weather mmp datasets (BEFORE)
  # x[,month_init := lapply(.SD, function(x) paste0("X", YearAgricola, ".", convert_date(x,m=1))), .SDcols=c("month_harvested")]

  # RESHAPE crop dataset from LONG to WIDE
  # crop_wide = reshape(x, timevar="YearAgricola", idvar=c("geocode_char"),
  #                     direction="wide", sep="-", drop=c("Produccion","temp_celcius","rain_annual_mm","geocode"))

  return(x)
}


# get WEATHER DEVIATIONS
weather_deviations = function(weather_df, crop_df, weather, type){

  # FIRST, create month_init and month_end columns
  crop_df = map_months(crop_df)

  # SECOND, define year_range
  # year_range = unique(gsub("\\D+","",crop_df[YearAgricola]))
  # year_range = year_range[year_range!=""]
  year_range = unique(gsub("-.","",crop_df[,YearAgricola]))

  # THIRD, merge datasets
  weather_df = geocode_char(weather_df)
  weather_df = geocode_ID(weather_df)

  # FOURTH,  loop over years and calculate mean deviations
  for(y in year_range){
    cat("Year",y,"\n")
    # get columns for crops in year y
    # cols_crop = weather_crop[,c(paste0("month_init-",y),paste0("month_end-",y))]
    cols_crop = crop_df[grepl(y,YearAgricola),]

    community_deviations = lapply(1:nrow(cols_crop), function(x) {
      if(grepl("NULL", cols_crop[x,month_init])) {
        return(NA)
      }else {
        # get min/max column indices from weather_df
        min_val = min( grep(gsub("*-.","",cols_crop[x,month_init]),colnames(weather_df)) )
        # it is possible that some crops are harvested within only ONE month
        if(grepl("NULL",cols_crop[x,month_end])){
          # use max value within month_init
          max_val = max( grep(gsub("*-.","",cols_crop[x,month_init]),colnames(weather_df)) )
        }else{
          # otherwise, use the max value of month_end
          max_val = max( grep(gsub("*-.","",cols_crop[x,month_end]),colnames(weather_df)) )
        }
        # calculate deviations (tmin, tmax, or prcp)
        if(weather == "tmin"){
          mean_deviation = mean( as.numeric(weather_df[x,min_val:max_val]) - crop_df[x,temp_min] )
        } else if(weather == "tmax"){
          mean_deviation = mean( as.numeric(weather_df[x,min_val:max_val]) - crop_df[x,temp_max])
        }
        }
        return(mean_deviation)
      })

    # combine doubled values
    double_ids = grep("-",cols_crop[,YearAgricola])
    if(length(double_ids) > 0){
      community_deviations[double_ids] = mean(unlist(community_deviations[double_ids]))
      # add column
      cols_crop[,c(paste0(weather,"_devs-",y)) := unlist(community_deviations)]

      # remove one of the doubles adn fix YearAgricola
      even_seq = double_ids[seq(2,length(double_ids),2)]
      cols_crop = cols_crop[-even_seq]
      cols_crop[,YearAgricola := gsub("*-.","",YearAgricola) ]

    }else{
      cols_crop[,c(paste0(weather,"_devs-",y)) := unlist(community_deviations)]
    }
    # build datasets with deviations
    piece = cols_crop[,c("geocode_char",paste0(weather,"_devs-",y)), with=FALSE]
    weather_with_devs = merge(weather_df[,c("geocode",paste0("geocode_",type))],piece,by.x=paste0("geocode_",type),by.y="geocode_char",all.x=TRUE)
    weather_df[,paste0(weather,"_devs-",y)] = weather_with_devs[,paste0(weather,"_devs-",y)]
  }


  return(weather_df)
}


#     # start column
#     community_deviations = sapply(1:nrow(cols_crop), function(x) {
#               if(! grepl("NULL",cols_crop[x,1]) ){
#
#                 # start and end columns from weather_df dataset
#                 col_start = min(grep(gsub("*-.","",cols_crop[1,month_init]), colnames(weather_df)))
#                 col_end  = max(grep(gsub("*-.","",cols_crop[1,month_end]), colnames(weather_df)))
#
#                 # subset of weather_crop based on respective columns
#                 cols_weather_vec = as.numeric( weather_crop[x,col_start:col_end] )
#
#                 # get weather baseline (tmin, tmax, or prcp)
#                 if(weather == "tmin"){
#                   weather_ind = paste0("temp_min-",y)
#                   # calculate deviations
#                   mean_deviation = mean(cols_weather_vec - weather_crop[x,weather_ind])
#                 } else if(weather == "tmax"){
#                   weather_ind = paste0("temp_max-",y)
#                   # calculate deviations
#                   mean_deviation = mean(cols_weather_vec - weather_crop[x,weather_ind])
#                 }
#               }else{
#                 mean_deviation = NA
#               }
#             } )
#     # add deviations to dataset
#     weather_crop[,paste0(weather,"_devs-",y)] = community_deviations
#   }
#   return(weather_crop)
# }
