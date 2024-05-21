library(tidyverse)
WORKDIR <- "c:/work"

dfResult_s <- getSize (
  sTYPE   = "slot",
  sDBPATH = "./tools/rSurveyAssign_vignette_size_1.sqlite"
)
print(dfResult_s)
