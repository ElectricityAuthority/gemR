###############################################
### Title: archiveGEMdataGDX()               ##
### Description: Relocate GEMdata GDX for    ##
### current run                              ##
### Date: 9 January 2019                     ##
###############################################

archiveGEMdataGDX <- function(runName){
  
  # # Get runName from globalVariables file
  # runName <- read_csv("Data/Setup/globalVariables.csv") %>%
  #   filter(variable == "runName") %>%
  #   .$value
  
  # GDX filepath
  tempGDX <- "Data/GEMsolveInput/GDX/GEMdata.gdx"
  
  # Output path
  outPath <- paste0("Output/", runName, "/GEMdata_", runName, ".gdx")
  
  if(file.exists(tempGDX)){
    
    # Copy file
    invisible(
      file.copy(tempGDX, outPath)
    )
    
    # # Delete temp file
    # invisible(
    #   file.remove(tempGDX)
    # )
    
  }
  
}