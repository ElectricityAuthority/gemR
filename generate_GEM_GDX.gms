$ontext
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
$ondelim offlisting offdigit include P:/Market Analytics/EMI/EMI tools/gemR/i_NrgDemand_df.csv
$offdelim onlisting ondigit
  /
  
Execute_Unload 'i_NrgDemand.gdx' i_NrgDemand;
  
