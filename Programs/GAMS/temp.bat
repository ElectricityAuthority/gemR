if exist report.txt                     erase report.txt /q
if exist run*Progress.txt               erase run*Progress.txt /q
if exist "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run"          rmdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run" /s /q
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run"
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive"
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\GDX"
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\GDX\temp\AllOut"
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\GDX\temp\RepOut"
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Processed files"
mkdir "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Input data checks"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\GEMdeclarations.gms" "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\GEMdeclarations.g00" "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\CollectResults.inc"  "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\GEMdata.gms"         "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\GEMsolve.gms"        "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\GEMreports.gms"      "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
copy "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\\GEMlrmc.gms"         "P:\Market Analytics\EMI\EMI tools\gemR\Programs\GAMS\..\..\Output\\standard_run\Archive\"
