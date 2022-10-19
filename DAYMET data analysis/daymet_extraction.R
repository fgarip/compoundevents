# read MMP geo codes
setwd(path.ra_filiz)
mmp = read.csv("mmp_w_footprint.csv")
# make geocodes characters
mmp[,'geocode'] = as.character(mmp[,'geocode'])
# add 0 to geocodes of length == 8; otherwise, do not change
mmp[,'geocode'] = ifelse( nchar(mmp$geocode) == 8, paste0('0', mmp$geocode), mmp$geocode )


# get Mexican localities
setwd(path.shapefiles)
cat('\n', 'Reading Mexican shapefiles...', '\n')
mx.loc.mmp = readOGR(".", layer='mx_localities_mmp')
cat('Done!', '\n')

# get all daymet folders
setwd(path.daymet)
daymet_folders = list.dirs(recursive = FALSE)

# time it
start = Sys.time()

# create csv file storing climate information (based on geocode)

# loop through each folder
for( folder in daymet_folders){
  # list all files inside folder
  all.files = list.files( folder )  
  
  # loop over all files within folder
  for(t in 1:length(all.files)){
    # create R object that will store all daymet values
    geo.climate = as.character( mx.loc.mmp@data[,'geocode'] )
    geo.climate = as.data.frame(geo.climate)
    
    # initialize proj.r
    if(t==1) proj.r = ""
    
    cat('\n', 'Reading daymet file...', '\n')
    
    # read prcp as raster brick
    file.path = paste0(folder,'/',all.files[t])
    r = brick(file.path)
    
    # transform mx.loc.mmp only if projections are different.
    if( ! identical(proj.r, proj4string(r)) ) {
      # update projection
      proj.r = proj4string(r)
      # transform mx.loc.mmp 
      mx.loc.mmp.trans = spTransform( mx.loc.mmp, proj.r )  
    }
    cat('Done!', '\n')
    
    # loop over all raster layers in the brick raster
    cat('Loop over all raster...', '\n')
    mx.coor = coordinates(mx.loc.mmp.trans)
    for(i in 1:nlayers(r)){
      val.cells = cellFromXY(r[[i]], mx.coor)
      values = as.data.frame( r[[i]][val.cells] )
      colnames(values) = names(r[[i]])
      # update 
      geo.climate = cbind(geo.climate, values )
    }
    # save as csv file after visiting all files in one folder
    year = sub('.*(\\d+{4}).*$','\\1',all.files[t])
    file.name = paste0('mmp_',sub('./','', folder), year, '.csv')
    print(file.name)
    fwrite(x=geo.climate, file=file.name, append = TRUE)
    cat('Done!', '\n') 
  }
}

# stop timer (this task took 1.5 days)
stop = Sys.time()
stop - start

