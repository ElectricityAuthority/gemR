###############################################
### Title: Folder set up                     ##
### Description: Set up folders for current  ##
### run                                      ##
### Date: 7 January 2019                     ##
###############################################

setupFolders <- function(runName){
  
  # Set paths
  outPath <- paste0("Output/", runName)
  progPath <- "Programs/GAMS/"
  
  if(dir.exists(outPath)){
    
    # Remove remove any output directory with the extant runName 
    unlink(outPath, recursive = TRUE)
    
  } 
  
  if(!dir.exists(outPath)){
    
    # Create new directories
    dir.create(outPath)
    
    dir.create(paste0(outPath, "/Archive"))
    dir.create(paste0(outPath, "/GDX"))
    dir.create(paste0(outPath, "/GDX/temp"))
    dir.create(paste0(outPath, "/GDX/temp/AllOut"))
    dir.create(paste0(outPath, "/GDX/temp/RepOut"))
    dir.create(paste0(outPath, "/Processed files"))
    dir.create(paste0(outPath, "/Report"))
    dir.create(paste0(outPath, "/Report/plots"))
    dir.create(paste0(outPath, "/Report/csv"))
    dir.create(paste0(outPath, "/Input data checks"))
    
    # Copy files in to Archive
    invisible(
      file.copy(
        from = paste0(
          progPath
          , c(
            "GEMdeclarations.gms"
            , "GEMdeclarations.g00"
            , "CollectResults.inc"
            , "GEMsolve.gms"
          )
        )
        , to = paste(paste0(outPath, "/Archive")
        )
      )
    )
    
  }
}

