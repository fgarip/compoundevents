#######################################################
# select right mx communities for MMP
#######################################################

# read MMP geo codes
setwd(path.git)
mmp = read_xlsx("MMP161_community_identifiers_with_complete_geocodes-1.xlsx")
mmp = mmp[1:161,] # remove tail
mmp = as.data.frame(mmp)

# make geocodes characters
mmp[,'geocode'] = as.character(mmp[,'geocode'])
# add 0 to geocodes of length == 8; otherwise, do not change
mmp[,'geocode'] = ifelse( nchar(mmp$geocode) == 8, paste0('0', mmp$geocode), mmp$geocode )

####################################################
# read ALL mexican localities
setwd(path.shapefiles)
cat('\n', 'Reading Mexican shapefiles...', '\n')
mx.loc = readOGR(".", layer='mx_localities')
cat('Done!', '\n')

# Keys Labels (in Spanish):
#   CVE_ENT: CLAVE DE ENTIDAD
#   CVE_LOC: CLAVE DE LOCALIDAD
#   CVE_MUN: CLAVE DE MUNICIPIO
#   CVE_AGEB: CLAVE DE AGEB
#   CVE_GEO: CLAVE CONCATENADA

# create geocode by combining STATE (ENT), MUNICIPALITY (MUN), and LOCALITY (LOC) .
handle.geocode = function(x){
  rows = c("CVE_ENT", "CVE_MUN", "CVE_LOC")
  if(sum(is.na(x[rows])) == 0) paste0( x[rows][1], x[rows][2], x[rows][3] )
  else x["CVEGEO"]
}

cat('\n', 'Creating geocode...', '\n')
mx.loc@data[,'geocode'] = apply(mx.loc@data, 1, function(x) handle.geocode(x)) 
cat('Done!', '\n')

# get missing geocodes
missing.geo = mmp$geocode[ ! mmp$geocode %in% mx.loc@data$geocode ]
if( length(missing.geo) > 0 ) cat('\n', 'Missing geocodes:', missing.geo, '\n') 

# save shapefile for MMP Mexican localities only
mx.loc.mmp = subset(mx.loc, geocode %in% mmp$geocode)
writeOGR(obj=mx.loc.mmp, dsn='.', layer='mx_localities_mmp', driver = 'ESRI Shapefile') 

