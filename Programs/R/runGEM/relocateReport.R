###############################################
### Title: relocateReport()                  ##
### Description: Relocate report file for    ##
### current run                              ##
### Date: 7 January 2019                     ##
###############################################

relocateReport <- function(){
  
  # Get runName from globalVariables file
  runName <- read_csv("Data/Setup/globalVariables.csv") %>%
    filter(variable == "runName") %>%
    .$value
  
  # Report filepath
  tempReport <- "Programs/GAMS/tempReport.txt"
  
  # Output path
  outPath <- paste0("Output/", runName, "/GEMsolveReport - ", runName, ".txt")
  
  if(file.exists(tempReport)){
    
    # Copy file
    invisible(
      file.copy(tempReport, outPath)
    )
    
    # Delete temp file
    invisible(
      file.remove(tempReport)
    )
    
  }
  
}