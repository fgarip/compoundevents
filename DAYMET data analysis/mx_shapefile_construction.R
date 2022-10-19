
##################### SHAPEFILE MEXICO
setwd(path.shapefiles)


all.files = list.files(pattern='.zip') # only zip files
if(! 'mx_sh' %in% list.files()) dir.create('mx_sh') # create directory to store MX shapefiles


for(i in 1:length(all.files)){
  cat('\n', '########################################################')
  # file name
  file.name = all.files[i] 
  cat('\n', file.name, '\n\n')  # print
  # unzip file
  files = unzip(file.name, exdir='tmp_folder')   
  conjunto = list.files(tmp)[grep('conjunto', list.files(tmp))]
  # build path to tmp folder
  build_path = paste0("tmp_folder/",conjunto, '/.') 
  # extract file pattern to unzip
  pattern = paste0( substr(file.name, 1,2),                  
                  c('m', 'ar', 'territorio_insular') ) # 'a' excluded
  # remove island query if non-existent
  if( length(grep('territorio_insular', files)) == 0 ) pattern = pattern[-3]
  
  # # load object if it exists
  # if(i > 1){
  #   cat('Loading shapefile object...', '\n\n')
  #   shapefile = readOGR(dsn='mx_sh/.', layer='mx_loc')
  #   cat('Done!', '\n\n')
  # }
  
  # loop over shapefiles
  for(l in 1:length(pattern)){
    # read shapefile
    f.read = readOGR(dsn = build_path, layer = pattern[l])
    f.read = readOGR(dsn = build_path, layer = "01mun")
    # it fixs problem with invalid multibyte string
    f.read@data[,'NOMBRE'] = iconv(f.read$NOMBRE)
    if(l==1) {
      # create shapefile  
        shapefile = f.read
      } else{
      # update shapefile
        cat('\t', 'appending...', '\n')
        shapefile = raster::bind(shapefile, f.read)
      }
    }
    # delete tmp folder
    unlink('tmp_folder/', recursive = TRUE)
    # summary info shapefile
    print(summary(shapefile))
    
    # store object
    cat('\n', 'Storing shapefile object...', '\n')
    # remove objects from folder
    # paths_remove = paste0('mx_sh/', list.files('mx_sh/'))
    # file.remove(paths_remove)
    # write shapefile object
    writeOGR(obj=shapefile, dsn='mx_sh/', layer=paste0('mx', substr(file.name, 1,2)), driver = 'ESRI Shapefile')  
    rm(shapefile, f.read)
    gc()
    cat('Done!', '\n')
    
    # visual delimiter
    cat('\n', '########################################################', '\n')
}

# bind all layers together
mx.layers = list.files('mx_sh/')
mx.layers = unique(sub('[[:punct:]].*', '', mx.layers))
mx.shapefile = do.call(raster::bind, lapply(mx.layers, function(x) readOGR("mx_sh/.", layer = x)))
summary(mx.shapefile)

# save shapefile
writeOGR(obj=mx.shapefile, dsn='.', layer='mx_localities', driver = 'ESRI Shapefile')  

