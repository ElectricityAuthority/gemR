###############################################
### Title: Generate GEM GDX file from CSV    ##
### Desription: This function is used to     ##
### create a demand GDX file for use in      ##
### the GEM model.                           ##
### Date: 19 April 2018                      ##
###############################################

convert_CSV_to_GDX <- function(CSV_filename, GMS_filepath, GMS_filename, GDX_output_filename){
  
  x <- paste0(
    "$ontext
Code for generating demand GDX file from a CSV file
$offtext

Sets
r 'Regions' /ni, si /
y 'Modelled calendar years' / 2012 * 2050 /
t 'Time periods (within a year)' / 
    q1  'Quarter 1'
    q2  'Quarter 2'
    q3  'Quarter 3'
    q4  'Quarter 4' /
lb 'Load blocks' / 
    b1l 'Low wind top block'
    b1w 'Windy top block'
    b2l 'Low wind second block'
    b2w 'Windy second block'
    b3l 'Low wind third block'
    b3w 'Windy third block'
    b4  'Fourth block'
    b5  'Fifth block'
    b6  'Sixth/last block' / ;                                
      
Parameter
i_NrgDemand(r,y,t,lb) 'Load by region, year, time period and load block, GWh' /
$ondelim offlisting offdigit include "
    
    , 
    
    CSV_filename
    
    ,
    "
$offdelim onlisting ondigit
  /
  
Execute_Unload '"
    
    , 
    
    GDX_output_filename
    
    ,
    "' i_NrgDemand;
  "
  )
  
  # Switch working directory temporarily to where GMS file is
  old_wd <- getwd()
  
  setwd(GMS_filepath)
  
  # Write text to gms file
  # fileConn <- file(GMS_filename)
  write_lines(x, GMS_filename)
  
  # Run gms file with gams
  gams(GMS_filename)
  
  # Set working directory back
  setwd(old_wd)
  
}
