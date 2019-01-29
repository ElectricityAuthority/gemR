###############################################
### Title: executeGAMS()                     ##
### Desription: This function is used to     ##
### execute GAMS from R.                     ##
### Date: 7 August 2018                      ##
###############################################
executeGAMS <- function(GAMS_filepath, GAMS_filename, GAMS_opts){
  
  # Switch working directory temporarily to where GMS file is
  old_wd <- getwd()
  
  setwd(GAMS_filepath)
  
  # Run gms file with gams
  x <- gams(paste(GAMS_filename, GAMS_opts))
  
  # Set working directory back
  setwd(old_wd)
  
  if(x != 0){
    stop(paste0("GAMS failed with return code: ", x, ". Please investigate."))
  }
  
}