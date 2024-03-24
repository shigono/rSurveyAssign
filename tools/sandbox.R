# library(rSurveyAssign)
#
# # 母集団オブジェクトの作成
# data(popdata, package = "rSurveyAssign")
# mbCat <- as.matrix(popdata[, paste0("bCat_", 1:3)])
# lSlot <- list(
#   as.matrix(popdata[, paste0("bSlot_1_", 1:10)]),
#   as.matrix(popdata[, paste0("bSlot_2_", 1:10)]),
#   as.matrix(popdata[, paste0("bSlot_3_", 1:10)])
# )
# lPop <- makePop(mbCAT = mbCat, lSLOT = lSlot, sVERBOSE = "detail")
#
# lRequest <- lapply(lSlot, function(mbSlot) rep(100, ncol(mbSlot)))
# lSetting1 <- makeSetting(
#   lSLOT_REQUEST = lRequest,
#   nCAT_MAX      = 1,
#   sCAT_TYPE     = 'adaptive',
#   sCAT_FILTER   = 'open',
#   sCAT_ORDER    = 'shortnum',
#   sCAT_EXCLUDE  = 'allclosed',
#   nSLOT_MAX     = 2,
#   sSLOT_TYPE    = 'adaptive',
#   sSLOT_FILTER  = 'open',
#   sSLOT_ORDER   = 'shortnum',
#   sSLOT_EXCLUDE = 'allclosed',
#   sVERBOSE      = "detail"
# )
# set.seed(123)  # 結果を再現するために乱数のシードを設定している。通常は設定不要

# lResult1 <- simBias (
#   lPOP        = lPop,
#   lSETTING    = lSetting1,
#   nNUMRETRIAL = 10,
#   sVERBOSE    = "simple"
# )
# print(lResult1)

# simBias (
#   lPOP        = lPop,
#   lSETTING    = lSetting1,
#   sDBPATH     = "./tools/rSurveyAssign_vignette_bias_1.sqlite",
#   nNUMTRIAL   = 10,
#   nNUMRETRIAL = 1000,
#   bAPPEND     = FALSE,
#   bPARALLEL   = TRUE,
#   sLOGFILE    = "c:/work/simBias.log"
# )

# dfResult_1s <- getBias("./tools/rSurveyAssign_vignette_bias_1.sqlite", sTYPE = "slot")
# dfResult_2s <- getBias("./tools/rSurveyAssign_vignette_bias_2.sqlite", sTYPE = "slot")

