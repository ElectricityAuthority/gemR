#### test rgdx and compress=TRUE and useDomInfo=FALSE with a recent GDX file containing full domain info
#### test both form=['sparse','full']

if (! require(gdxrrw))      stop ("gdxrrw package is not available")
if (0 == igdx(silent=TRUE)) stop ("the gdx shared library has not been loaded")

source ("chkSame.R")
reqIdent <- TRUE

tryCatch({
  print ("testing rgdx handling of GDX file with compress=TRUE and useDomInfo=FALSE")
  rgdx('?')
  fnIn <- "compressTest.gdx"
  if (! file_test ('-f', fnIn)) {
    stop (paste("FAIL: File", fnIn, "does not exist"))
  }
  useDomInfo <- FALSE                   # NOT the default

  source ("tReadCompressBody.R")


  print ("test of rgdx with compress=TRUE and useDomInfo=FALSE passed")
  TRUE   ## all tests passed: return TRUE
},

error = function(ex) { print ("test of rgdx with compress=TRUE and useDomInfo=FALSE failed"); print(ex) ; FALSE }
)
