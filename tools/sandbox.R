library(rSurveyAssign)

# 10000x100の乱数行列をつくって各行をコピーするのと、
# 長さ100の乱数ベクトルを10000回作るのとどっちがはやいか

gStart <- proc.time()[3]

for (j in 10000000){
  mgX <- matrix(
    runif(10000*100, min = 0, max = 1),
    ncol = 100
  )
  for (i in 1:10000)
    gX <- sum(mgX[i,])
}
print(proc.time()[3] - gStart)

for (j in 10000000){
  for (i in 1:10000)
    gX <- sum(runif(100, min = 0, max = 1))
}
print(proc.time()[3] - gStart)


# data(popdata, package = "rSurveyAssign")
# head(popdata)
#
# mbCat <- as.matrix(popdata[, paste0("bCat_", 1:3)])
# lSlot <- list(
#   as.matrix(popdata[, paste0("bSlot_1_", 1:10)]),
#   as.matrix(popdata[, paste0("bSlot_2_", 1:10)]),
#   as.matrix(popdata[, paste0("bSlot_3_", 1:10)])
# )
# lPop <- makePop(mbCAT = mbCat, lSLOT = lSlot)
#
# lSetting <- makeSetting(
#   lSLOT_REQUEST = lapply(lSlot, function(mbSlot) rep(0, ncol(mbSlot))),
#   nCAT_MAX      = 2,
#   nSLOT_MAX     = 2,
#   sCAT_TYPE     = 'adaptive',
#   sCAT_FILTER   = 'open',
#   sCAT_ORDER    = 'random',
#   sCAT_EXCLUDE  = 'none',
#   sSLOT_TYPE    = 'adaptive',
#   sSLOT_FILTER  = 'all',
#   sSLOT_ORDER   = 'shortnum',
#   sSLOT_EXCLUDE = 'allclosed'
# )
#
# set.seed(123)  # 結果を再現するために乱数のシードを設定している。通常は設定不要
#
# dfResult <- simSize (
#   lPOP      = lPop,           # データを指定する
#   lSETTING  = lSetting,        # セッティングを指定する
#   nNUMTRIAL = 2,              # シミュレーション試行数.
#   bPARALLEL = TRUE,
#   sDBPATH = "c:/work/simSize.sqlite",
#   bAPPEND = FALSE,
#   sLOGFILE =  "c:/work/simSize.log",
#   sVERBOSE  = "detail"
# )
#
# getSize(sDBPATH = "c:/work/simSize.sqlite")
