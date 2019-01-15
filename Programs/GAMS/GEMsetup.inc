$setglobal ProgPath "%system.fp%"
$setglobal DataPath "%system.fp%..\..\Data\GEMsolveInput\GDX\"
$setglobal OutPath "%system.fp%..\..\Output\"
$setglobal GEMinputGDX "GEMdata.gdx"
$setglobal GEMoverrideGDX ""
$setglobal useOverrides "0"
$setglobal sprsGEMsolve "0"
$setglobal Mode "0"
$setglobal GRscheduleFile ""
$setglobal GRscheduleRead "0"
$setglobal RunType "1"
$setglobal GEMtype "RMIP"
$setglobal DISPtype "RMIP"
$setglobal calcInputLRMCs "1"
$setglobal NumVertices "4"
$setglobal AClossesNI "0"
$setglobal AClossesSI "0"
$setglobal Solver "Cplex"
$setglobal optcr "0.0075"
$setglobal CPUsecsGEM "20000"
$setglobal Threads "4"
$setglobal limitOutput "0"
$setglobal runName "standard_run2"
$setglobal runVersionName "v1"
$setglobal runNameDesc "Standard run2"
$setglobal firstYear "2018"
$setglobal lastYear "2023"
Scalar GRscheduleWrite / 1 / ;
Scalar hydroOutputScalar / 0.97 / ;
Scalar WACCg / 0.07 / ;
Scalar WACCt / 0.07 / ;
Scalar discRateLow / 0.04 / ;
Scalar discRateMed / 0.07 / ;
Scalar discRateHigh / 0.1 / ;
Scalar taxRate / 0.28 / ;
Scalar depType / 1 / ;
Scalar txPlantLife / 60 / ;
Scalar txDepRate / 0.06 / ;
Scalar randomCapexCostAdjuster / 0 / ;
Scalar txLossesRMIP / 1 / ;
Scalar V2GtechnologyOn / 0 / ;
Scalar renNrgShrOn / 0 / ;
Scalar renCapShrOn / 0 / ;
Scalar niNWpeakCnstrntOn / 0 / ;
Scalar limitNrgByFuelOn / 1 / ;
Scalar reservesOn / 0 / ;
Scalar DCloadFlowOn / 0 / ;
Scalar cGenYr / 2025 / ;
Scalar AnnualMWlimit / 1000 / ;
Scalar noRetire / 2 / ;
Scalar VOLLcap / 500 / ;
Scalar VOLLcost / 10000 / ;
Scalar penaltyViolatePeakLoad / 99999 / ;
Scalar penaltyViolateRenNrg / 99999 / ;
Scalar slackCost / 9999 / ;
Scalar noVOLLblks / 0 / ;
