# library(rSurveyAssign)

# 母集団オブジェクトの作成
data(popdata, package = "rSurveyAssign")
mbCat <- as.matrix(popdata[, paste0("bCat_", 1:3)])
lSlot <- list(
  as.matrix(popdata[, paste0("bSlot_1_", 1:10)]),
  as.matrix(popdata[, paste0("bSlot_2_", 1:10)]),
  as.matrix(popdata[, paste0("bSlot_3_", 1:10)])
)
lPop <- makePop(mbCAT = mbCat, lSLOT = lSlot, sVERBOSE = "detail")

lRequest <- lapply(lSlot, function(mbSlot) rep(100, ncol(mbSlot)))
lSetting1 <- makeSetting(
  lSLOT_REQUEST = lRequest,
  nCAT_MAX      = 1,
  sCAT_TYPE     = 'adaptive',
  sCAT_FILTER   = 'open',
  sCAT_ORDER    = 'shortnum',
  sCAT_EXCLUDE  = 'allclosed',
  nSLOT_MAX     = 2,
  sSLOT_TYPE    = 'adaptive',
  sSLOT_FILTER  = 'open',
  sSLOT_ORDER   = 'shortnum',
  sSLOT_EXCLUDE = 'allclosed',
  sVERBOSE      = "detail"
)

# 調査オブジェクトの作成
data(surveydata, package = "rSurveyAssign")
lSurvey <- makeSurvey(
  mbCAT = as.matrix(surveydata[, paste0("bCat_", 1:3)]),
  lSLOT = list(
    as.matrix(surveydata[, paste0("bSlot_1_", 1:10)]),
    as.matrix(surveydata[, paste0("bSlot_2_", 1:10)]),
    as.matrix(surveydata[, paste0("bSlot_3_", 1:10)])
  ),
  lSETTING = makeSetting(
    lSLOT_REQUEST = list(rep(10, 10), rep(10, 10), rep(10, 10)),
    nCAT_MAX      = 2,
    sCAT_TYPE     = 'adaptive',
    sCAT_FILTER   = "open",
    sCAT_ORDER    = "random",
    sCAT_EXCLUDE  = "none",
    nSLOT_MAX     = 2,
    sSLOT_TYPE    = "adaptive",
    sSLOT_FILTER  = "all",
    sSLOT_ORDER   = "shortnum",
    sSLOT_EXCLUDE = "allclosed"
  ),
  mnASSIGNCAT   = as.matrix(surveydata[, c("nAssignedCat_1", "nAssignedCat_2")]),
  anPARENTCAT   = as.vector(surveydata$nAssignedCat_Slots),
  mnASSIGNSLOT  = as.matrix(surveydata[, c("nAssignedSlot_1", "nAssignedSlot_2")])
)
