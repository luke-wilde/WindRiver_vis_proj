##Input: Packages to be installed and loaded into environment
setUp <- function(x){
  #create function to load and install (missing) packages
  
  foo <- function(x){
    for( i in x ){
      #  require returns TRUE invisibly if it was able to load package
      if( ! require( i , character.only = TRUE ) ){
        #  If package was not able to be loaded then re-install
        install.packages( i , dependencies = TRUE )
        #  Load package after installing
        require( i , character.only = TRUE )
      }
    }
  }
  
  foo(x)
  
  options(max.print = 999)
  as.POSIXct(Sys.time(), origin = "1970-01-01")
}


#Convert longlat coordinates to utm or visversa
##Input: xytb object
ll2utm <- function(df, zone = 12, ellps = "WGS84", proj1 = "longlat", proj2 = "utm"){
  
  llcoord <- SpatialPoints(df[,1:2],
                           proj4string=CRS(paste0("+proj=", 
                                                  proj1, 
                                                  " +datum=", 
                                                  ellps)))
  
  utmcoord <- spTransform(llcoord,CRS(paste0("+proj=", 
                                             proj2, 
                                             " +zone=", 
                                             zone, 
                                             " +datum=", 
                                             ellps)))
  df[,1] <- attr(utmcoord,"coords")[,1]
  df[,2] <- attr(utmcoord,"coords")[,2]
  
  df <- df %>% 
    rename(x = 3, 
           y = 4)
  
  return(df)
}

#credit to jcheng5 githubhttps://github.com/rstudio/leaflet/issues/51
makePlotURI <- function(expr, width, height, ...) {
  pngFile <- plotPNG(function() { expr }, width = width, height = height, ...)
  on.exit(unlink(pngFile))
  
  base64 <- httpuv::rawToBase64(readBin(pngFile, raw(1), file.size(pngFile)))
  paste0("data:image/png;base64,", base64)
}
