library(usethis)
library(readr)
library(dplyr)
library(tidyr)

# if ("package:rSurveyAssign" %in% search())
#   detach("package:rSurveyAssign", unload = TRUE)
# library(rSurveyAssign)

## popdata - - - - - - - - - - - - - -
# popdata <- read_rds("c:/ono/work/exportData_BH1.1.rds")
# popdata <- popdata[order(popdata$nID),]
# popdata <- as.data.frame(popdata)
# # print(popdata)
# use_data(popdata, overwrite = TRUE)

## surveydata - - - - -
data(popdata)
set.seed(123)
oPop <- makePop(
  mbCAT <- as.matrix(popdata[, paste0("bCat_", 1:3)]),
  lSLOT <- list(
    as.matrix(popdata[, paste0("bSlot_1_", 1:10)]),
    as.matrix(popdata[, paste0("bSlot_2_", 1:10)]),
    as.matrix(popdata[, paste0("bSlot_3_", 1:10)])
  )
)
lRequest <- lapply(oPop$lSLOT, function(mbSlot) rep(10, ncol(mbSlot)))
dfResult <- simForecast (
  lPOP           = oPop,        # データを指定する
  lSLOT_REQUEST  = lRequest,       # 各スロットの目標回収県数
  nMAXCAT        = 2,              # 割付カテゴリ数の上限
  sOPTCAT        = "open_random",  # カテゴリ割付の方法
  nMAXSLOT       = 2,              # 割付スロット数の上限
  sOPTSLOT       = "all_shortnum", # スロット割付の方法
)

mbCAT <- oPop$mbCAT[dfResult$nPerson,]

dfTemp <- dfResult %>%
  dplyr::select(SEQ, nCat1, nCat2) %>%
  pivot_longer(
   cols = starts_with("nCat"),
   names_to = "sVar",
   values_to = "nCategory"
  ) %>%
  mutate(bValue = 1) %>%
  filter(!is.na(nCategory)) %>%
  dplyr::select(SEQ, nCategory, bValue) %>%
  pivot_wider(names_from = nCategory, names_prefix = "bCat_", values_from = bValue)
mbTemp <- tibble(SEQ = seq_len(nrow(mbCAT))) %>%
  left_join(dfTemp, by = "SEQ") %>%
  dplyr::select(one_of(paste0("bCat_", 1:3))) %>%
  as.matrix(.)
mbTemp[is.na(mbTemp)] <- 0

lSlot <- lapply(
  seq_along(oPop$lSLOT),
  function(nCategory){
    out <- oPop$lSLOT[[nCategory]][dfResult$nPerson, ]
    out[mbTemp[,nCategory] == 0, ] <- NA
    return(out)
  }
)
# print(lSlot[[1]][7,])
# stop()

dfTemp <- dfResult[c("nCat1", "nCat2", "nCat", "nSlot1", "nSlot2")]
colnames(dfTemp) <- c(
  "nAssignedCat_1",
  "nAssignedCat_2",
  "nAssignedCat_Slots",
  "nAssignedSlot_1",
  "nAssignedSlot_2"
)
# print(dfTemp[7,])
# stop()

surveydata <- data.frame(
  SEQ = seq_len(nrow(mbCAT)),
  mbCAT,
  dfTemp[c("nAssignedCat_1", "nAssignedCat_2")],
  lSlot[[1]],
  lSlot[[2]],
  lSlot[[3]],
  dfTemp[c("nAssignedCat_Slots", "nAssignedSlot_1", "nAssignedSlot_2")]
)
print(str(surveydata))
use_data(surveydata, overwrite = TRUE)
