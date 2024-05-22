library(tidyverse)
WORKDIR <- "c:/work"

library(rSurveyAssign)
data(surveydata, package = "rSurveyAssign")

lSetting <- makeSetting(
  lSLOT_REQUEST = list(rep(10, 10), rep(10, 10), rep(10, 10)),
  nCAT_MAX      = 2,
  sCAT_ASSIGN   = 'assignable-openclosed-open-none',
  nSLOT_MAX     = 2,
  sSLOT_ASSIGN  = "assignable-shortnum-assignable-allclosed",
)
lSurvey <- makeSurvey(
  mbCAT <- as.matrix(surveydata[, paste0("bCat_", 1:3)]),
  lSLOT <- list(
    as.matrix(surveydata[, paste0("bSlot_1_", 1:10)]),
    as.matrix(surveydata[, paste0("bSlot_2_", 1:10)]),
    as.matrix(surveydata[, paste0("bSlot_3_", 1:10)])
  ),
  lSETTING      = lSetting,
  mnASSIGNCAT   = as.matrix(surveydata[, c("nAssignedCat_1", "nAssignedCat_2")]),
  anPARENTCAT   = as.vector(surveydata$nAssignedCat_Slots),
  mnASSIGNSLOT  = as.matrix(surveydata[, c("nAssignedSlot_1", "nAssignedSlot_2")])
)
dfCheck <- checkSurvey(lSurvey, sVERBOSE = "detail")

