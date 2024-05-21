sub_makeSurvey_from_trial <- function(
    lPOP,
    lSETTING,
    dfSubject,
    bUSE_INFO_UNASSIGNED_CAT,
    sVERBOSE = c("simple", "detail", "none")
){
  #' Internal: make virtual survey data based on the returned value from execTrials()
  #'
  #' Internal: 割付シミュレーションの結果に基づいて仮想的な調査データをつくる。
  #'           simBias()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param lPOP `popdata`クラスのオブジェクト。
  #'    母集団データ。\code{\link{makePop}}で生成する。
  #' @param lSETTING `assignsetting`クラスのオブジェクト。
  #'    割付のセッティング。\code{\link{makeSetting}} で生成する。
  #' @param dfSubject データフレーム。
  #'    execTrials()の返し値。
  #' @param bUSE_INFO_UNASSIGNED_CAT 論理値。
  #'    非割付カテゴリに属するスロットへの割付可能性を利用するか。
  #'    TRUEの場合、すべての割付可能カテゴリに属するスロット割付可能性を格納。
  #'    FALSEの場合、割付カテゴリに属するスロット割付可能性のみを格納。
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return makeSurvey()の返し値。
  #'
  # 引数チェックは親任せ

  if (sVERBOSE == "detail") cat("[sub_makeSurvey_from_trial] start.\n")

  # mbSubjectCat_Assignable: 対象者とカテゴリからカテゴリ割付可能性を引く表
  #                          (行を選ばれた対象者順にしたmbCAT)
  #                          仮想調査データを作る際のmbCATになる
  mbSubjectCat_Assignable <- lPOP$mbCAT[dfSubject$nPerson, , drop = FALSE]

  # mbSubjectCat_Assign: 対象者とカテゴリからカテゴリ割付有無を引く表
  if (sVERBOSE == "detail") cat("[sub_makeSurvey_from_trial] make dfSubjectCat_Assign ...\n")
  dfSubjectCat_Assign <- dfSubject %>%
    dplyr::select(.data$nSubject, starts_with("nCat")) %>%
    # 割付カテゴリを縦に
    pivot_longer(
      cols = starts_with("nCat"),
      names_to = "sVar",
      values_to = "nCategory"
    ) %>%
    # 割付カテゴリがNAである行を消して
    filter(!is.na(.data$nCategory)) %>%
    # 行があるということは割り付けられているわけだから値は1
    mutate(bValue = 1) %>%
    # 割付カテゴリを横にする
    dplyr::select(.data$nSubject, .data$nCategory, .data$bValue) %>%
    pivot_wider(
      names_from = .data$nCategory,
      names_prefix = "bCat_",
      values_from = .data$bValue
    ) %>%
    # 対象者を完備させる。full joinを使っている
    full_join(
      dfSubject %>% dplyr::select(.data$nSubject), by = "nSubject"
    ) %>%
    arrange(.data$nSubject) %>%
    # 変数を選んで
    dplyr::select(
      one_of(paste0("bCat_", seq_len(ncol(mbSubjectCat_Assignable))))
    )
  mbSubjectCat_Assign <- as.matrix(dfSubjectCat_Assign)

  # lCatSubjectSlot_Assignable: カテゴリ, 対象者, スロットからスロット割付可能性を引く
  #                             (行を選ばれた対象者にしたlSLOT)
  #                             仮想調査データを作る際のlSLOTになる
  # カテゴリ別に処理
  if (sVERBOSE == "detail") cat("[sub_makeSurvey_from_trial] make lCatSubjectSlot_Assignable ...\n")
  lCatSubjectSlot_Assignable <- lapply(
    seq_along(lPOP$lSLOT),
    function(nCat){
      # 母集団メンバーのスロット割付可能性を引っ張ってきて
      out <- lPOP$lSLOT[[nCat]]
      # 調査対象者のスロット割付可能性に並び替えて
      out <- out[dfSubject$nPerson, ]
      if (!bUSE_INFO_UNASSIGNED_CAT){
        # もしカテゴリに割り付けられていなかったらスロット割付可能性は未知と考え、NAに
        out[mbSubjectCat_Assign[,nCat] == 0, ] <- NA
      }
      return(out)
    }
  )
  mnAssignCat <- as.matrix(dfSubject[, paste0("nCat", seq_len(lSETTING$nCAT_MAX))])
  anParentCat <- as.vector(dfSubject$nParentCat)
  mnAssignSlot <- as.matrix(dfSubject[, paste0("nSlot", seq_len(lSETTING$nSLOT_MAX))])

  # 調査データに変換
  # sVERBOSEはsimpleだったらnoneにする
  if (sVERBOSE == "detail") cat("[sub_makeSurvey_from_trial] make surveydata ...\n")
  oSurvey <- makeSurvey(
    mbCAT         = mbSubjectCat_Assignable,
    lSLOT         = lCatSubjectSlot_Assignable,
    lSETTING      = lSETTING,
    mnASSIGNCAT   = mnAssignCat,
    anPARENTCAT   = anParentCat,
    mnASSIGNSLOT  = mnAssignSlot,
    sVERBOSE      = if_else(sVERBOSE == "detail", "detail", "none")
  )

  if (sVERBOSE == "detail"){
    cat("[sub_makeSurvey_from_trial] end.\n")
  }
  return(oSurvey)
}

simBias <- function(
  lPOP,
  lSETTING,
  sSAMPLING        = c("with_replace", "without_replace", "fixed"),
  nNUMTRIAL        = 1,
  nNUMRETRIAL      = 1000,
  bUSE_INFO_UNASSIGNED_CAT = TRUE,
  bPARALLEL        = FALSE,
  sLOGFILE         = NULL,
  sDBPATH          = NULL,
  sDBTABLE_SUBJECT = "subject",
  sDBTABLE_CAT     = "weight_cat",
  sDBTABLE_SLOT    = "weight_slot",
  bAPPEND          = TRUE,
  sVERBOSE         = c("simple", "detail", "none")
){
  #' run simulations to evaluate bias
  #'
  #' バイアス評価のためのシミュレーションを実行する
  #'
  #' @export
  #'
  #' @param lPOP `popdata`クラスのオブジェクト。
  #'    母集団データ。\code{\link{makePop}}で生成する。
  #' @param lSETTING `assignsetting'クラスのオブジェクト。
  #'    割付のセッティング。\code{\link{makeSetting}}で生成する。
  #' @param sSAMPLING a string.
  #'    抽出方法。以下のいずれか。
  #'    \itemize{
  #'    \item \code{with_replace}: 割付試行によって調査対象者を生成し、再割付試行では
  #'    調査対象者から復元無作為抽出する
  #'    \item \code{without_replace}: 割付試行によって調査対象者を生成し、再割付試行では
  #'    調査対象者から非復元無作為抽出する(調査参加順序を入れ替える)
  #'    \item \code{fixed}: 割付試行によって調査対象者を生成し、再割付試行では
  #'    それらの調査対象者を同じ調査参加順序で再割付する
  #'    }
  #' @param nNUMTRIAL 整数。
  #'    割付試行数。
  #' @param nNUMRETRIAL 整数。
  #'    各割付試行における再割付試行数。
  #' @param bUSE_INFO_UNASSIGNED_CAT 論理値。
  #'    再割付試行において、非割付カテゴリに属するスロットへの割付可能性を利用するか。
  #'    TRUEの場合、すべての割付可能カテゴリに属するスロット割付可能性を用いて
  #'    再割付試行を行う。
  #'    FALSEの場合、割付カテゴリに属するスロット割付可能性のみを用いて
  #'    再割付試行を行う。
  #' @param bPARALLEL 論理値。
  #'    並列処理するか。
  #' @param sLOGFILE 文字列。
  #'    並列処理する場合のログファイル(フルパス)。NULLだとログを出さない。
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス。
  #' @param sDBTABLE_SUBJECT 文字列。
  #'    割付シミュレーションの結果を保存するSQLite DBのテーブル名。
  #' @param sDBTABLE_CAT 文字列。
  #'    再割付シミュレーションの結果得られたカテゴリ割付頻度を保存するSQLite DBのテーブル名。
  #' @param sDBTABLE_SLOT 文字列。
  #'    再割付シミュレーションの結果得られたスロット割付頻度を保存するSQLite DBのテーブル名。
  #' @param bAPPEND 論理値。
  #'    TRUE: 結果をテーブルに追加する。FALSE: 上書きする。
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return
  #'    \itemize{
  #'      \item sDBPATHを指定しない場合は、シミュレーションの結果をデータフレームのリストとして返す。
  #'      \item sDBPATHを指定した場合はNULLを返す。シミュレーションの結果は、
  #'            SQLiteデータベースにテーブルとして保存される。sDBPATH が存在する場合は、
  #'            bAPPEND==FALSEであればSDBPATHに上書きし、bAPPEND==TRUEであれば追加する。
  #'            sDBPATHが存在しない場合は新規作成する。
  #'    }
  #'
  #'    シミュレーションの結果は次の3つのデータフレームからなる。
  #'
  #'    \code{dfSubject}: 割付シミュレーションの結果。
  #'    行はある割付試行のある調査参加者を表す。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nTrial}:           試行番号
  #'    \item \code{nSubject}:         対象者番号 (調査参加順の連番)
  #'    \item \code{nPerson}:          母集団メンバー番号 (\code{lPOP$mbCAT}の行番号)
  #'    \item \code{sRowname}:         \code{lPOP$mbCAT}の行名。\code{lPOP$mbCAT}に行名がない場合はas.character(SEQ)となる
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号(\code{lPOP$mbCAT}の列番号)、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{nMAXCAT}のカテゴリ番号(\code{lPOP$mbCAT}の列番号)、ないし\code{NA}
  #'    \item \code{nCat}:             割付スロットが属するカテゴリ番号(\code{lPOP$mbCAT}の列番号)、ないし\code{NA}
  #'    \item \code{nSlot_1}:          割付スロット\code{1}のスロット番号(\code{lPOP$mbSLOT[[nCat]]}の列番号)、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nMAXSLOT): 割付スロット\code{nMAXSLOT}のスロット番号(\code{lPOP$mbSLOT[[nCat]]}の列番号)、ないし\code{NA}
  #'    }
  #'
  #'    \code{dfStat_Cat}: 再割付シミュレーションにおけるカテゴリ割付頻度。
  #'    行は調査対象者x割付可能カテゴリを表す。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nTrial}:            試行番号
  #'    \item \code{nBlock}:            ブロック番号
  #'    \item \code{nBlockSize}:        ブロックサイズ(ブロック内の再割付試行数)
  #'    \item \code{nSubject}:          調査対象者番号
  #'    \item \code{nCat}:              カテゴリ番号
  #'    \item \code{sRowname}:          \code{lPOP$mbCAT}の行名。\code{lPOP$mbCAT}に行名がない場合はas.character(SEQ)となる
  #'    \item \code{nPerson}:           母集団メンバー番号 (\code{lPOP$mbCAT}の行番号)
  #'    \item \code{bAssign}            割付試行で割付が起きていたか
  #'    \item \code{nNumRetrial}        その人が1回以上出現した再割付試行数
  #'    \item \code{gSumProp}           その人が1回以上出現した再割付試行におけるカテゴリ割付率の合計
  #'    \item \code{gSumSqProp}         その人が1回以上出現した再割付試行におけるカテゴリ割付率の二乗の合計
  #'    \item \code{gAssignablity_Cat}: カテゴリ割付可能度(全カテゴリに占める割付可能カテゴリの割合)
  #'    }
  #'
  #'    \code{dfStat_Slot}: 再割付シミュレーションにおけるスロット割付頻度。
  #'    行は調査対象者x割付可能スロットを表す。
  #'    割付試行において割付可能性が判明しているスロットに
  #'    限定される(代替対象者のスロットは含まれない)。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nTrial}:             試行番号
  #'    \item \code{nBlock}:             ブロック番号
  #'    \item \code{nBlockSize}:         ブロックサイズ(ブロック内の再割付試行数)
  #'    \item \code{nSubject}:           調査対象者番号
  #'    \item \code{sRowname}:           \code{lPOP$mbCAT}の行名。\code{lPOP$mbCAT}に行名がない場合はas.character(SEQ)となる
  #'    \item \code{nPerson}:            母集団メンバー番号 (\code{lPOP$mbCAT}の行番号)
  #'    \item \code{nParentCat}:         スロットが属するカテゴリ番号
  #'    \item \code{nSlot}:              スロット番号
  #'    \item \code{bAssign}             割付試行で割付が起きていたか
  #'    \item \code{nNumRetrial}         人xスロットが出現した試行数
  #'    \item \code{gSumProp}            人xスロットが出現した試行における割付率の合計
  #'    \item \code{gSumSqProp}          人xスロットが出現した試行における割付率の二乗の合計
  #'    \item \code{gAssignablity_Slot}: スロット割付可能度(全スロットに占める割付可能スロットの割合)
  #'    }
  #'
  #' @details
  #'    bAPPEND==TRUEで、sDBPATHが存在し、かつ
  #'    テーブルsDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOTが存在しない場合はエラーとなる。
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom dplyr full_join
  #' @importFrom tidyselect one_of
  #' @importFrom digest digest

  ## あいさつのためsVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[simBias] start.\n")
  }

  ## 引数チェック - - - - -
  ## lPOP, lSETTING はexecTrialでチェックする
  ## sSAMPLING, nNUMRETRIAL, bPARALLEL, sLOGFILEはexecRetrialでチェックする

  ## nNUMTRIAL
  ## 指定されている
  if (is.null(nNUMTRIAL))
    stop("[simBias] nNUMTRIAL is invalid.")

  ## bUSE_INFO_UNASSIGNED_CAT
  ## 期待通り
  if (!(bUSE_INFO_UNASSIGNED_CAT %in% c(TRUE, FALSE)))
    stop("[simBias] bUSE_INFO_UNASSIGNED_CAT is invalid.")

  ## sDBPATH, sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOT
  ## ファイルが存在していない場合、ファイルは作成できるべき
  if (!is.null(sDBPATH) && !file.exists(sDBPATH)){
    stopifnot(file.create(sDBPATH))
    stopifnot(file.remove(sDBPATH))
  }

  ## ファイルが存在しており、追加しろといわれている場合、
  ## 整合性をチェック
  if (!is.null(sDBPATH) && file.exists(sDBPATH) && bAPPEND == TRUE){
    if (!checkDB(sDBPATH, c(sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOT), sCurrentDigest)){
      stop("[simBias] The DB", sDBPATH, "seems invalid. Remove it and retry.")
    }
  }

  ## bAPPEND
  ## 値は期待通り
  stopifnot(bAPPEND %in% c(TRUE, FALSE))

  ## ここからメイン - - - - -
  # ダイジェスト
  sCurrentDigest <- digest(list(lPOP, lSETTING))

  lOut <- lapply(
    seq_len(nNUMTRIAL),
    function(nCurrentTrial){

      # 時間測定開始
      oTime <- proc.time()

      # まず標本抽出を行う。1試行だけ
      # sVERBOSEはsimpleだったらnoneにする
      # dfSubject$nPersonはlPopの行番号を表している
      dfSubject <- execTrials (
        lPOP       = lPOP,
        lSETTING   = lSETTING,
        nNUMTRIAL  = 1,
        bPARALLEL  = FALSE,
        sLOGFILE   = NULL,
        sVERBOSE   = if_else(sVERBOSE == "detail", "detail", "none")
      )
      ## print(head(dfSubject))

      # 仮想的調査データを作成する
      # oSurvey上の行iは、dfSubjectの行iに対応している
      oSurvey <- sub_makeSurvey_from_trial(
        lPOP      = lPOP,
        lSETTING  = lSETTING,
        dfSubject = dfSubject,
        bUSE_INFO_UNASSIGNED_CAT = bUSE_INFO_UNASSIGNED_CAT,
        sVERBOSE  = if_else(sVERBOSE == "detail", "detail", "none")
      )
      ## print(head(oSurvey$mbCAT))

      # カテゴリ割付可能度
      dfAssignability_Cat <- tibble( gAssignability_Cat = rowMeans(oSurvey$mbCAT) ) %>%
        mutate(nSubject = row_number())
      # print(dfAssignability_Cat)
      # stop()

      # スロット割付可能度
      lOut <- lapply(
        seq_along(oSurvey$lSLOT),
        function(nCat){
          out <- tibble( gAssignability_Slot = rowMeans(oSurvey$lSLOT[[nCat]]) ) %>%
            mutate(
              nSubject = row_number(),
              nParentCat = nCat
            )
          return(out)
        }
      )
      dfAssignability_Slot <- bind_rows(lOut)

      # 再割付シミュレーション
      # sVERBOSEはsimpleだったらnoneにする
      lResult <- execRetrials(
        lSURVEY    = oSurvey,
        sSAMPLING  = sSAMPLING,
        nNUMBLOCK  = 1,
        nBLOCKSIZE = nNUMRETRIAL,
        bPARALLEL  = bPARALLEL,
        sLOGFILE   = sLOGFILE,
        sVERBOSE   = if_else(sVERBOSE == "detail", "detail", "none")
      )

      # 出力
      dfSubject <- dfSubject %>%
        mutate(nTrial = nCurrentTrial)

      dfStat_Cat <- lResult$dfStat_Cat %>%
        mutate(nTrial = nCurrentTrial) %>%
        left_join(
          dfSubject %>% dplyr::select(.data$nSubject, .data$sRowname, .data$nPerson),
          by = "nSubject"
        ) %>%
        left_join(dfAssignability_Cat, by = "nSubject")

      dfStat_Slot <- lResult$dfStat_Slot %>%
        mutate(nTrial = nCurrentTrial) %>%
        left_join(
          dfSubject %>% dplyr::select(.data$nSubject, .data$sRowname, .data$nPerson),
          by = "nSubject"
        ) %>%
        left_join(dfAssignability_Slot, by = c("nSubject", "nParentCat"))

      lOut <- list(
        dfSubject = dfSubject,
        dfStat_Cat = dfStat_Cat,
        dfStat_Slot = dfStat_Slot
      )

      # 画面表示
      if (sVERBOSE %in% c("simple", "detail")){
        cat("[simBias] nTrial:", nCurrentTrial, "; extract:", nrow(dfSubject), ";", (proc.time()-oTime)[3], "sec.\n")
      }
      return(lOut)
    }
  )

  dfSubject   <- lapply(lOut, function(lIn) lIn$dfSubject) %>% bind_rows()
  dfStat_Cat  <- lapply(lOut, function(lIn) lIn$dfStat_Cat) %>% bind_rows()
  dfStat_Slot <- lapply(lOut, function(lIn) lIn$dfStat_Slot) %>% bind_rows()

  ### ここから出力

  if (!is.null(sDBPATH)){
    # sDBPATHが指定されたとき
    # ここでreturn()していることに注意

    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))

    # DBに保存
    if (bAPPEND){
      # 追加する場合
      # すでに行われたブロック数を加算し、過去とダブらないようにする
      nNumTrial <- countBias(sDBPATH, sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_CAT)
      dfSubject  <- dfSubject %>% mutate(nTrial = .data$nTrial + nNumTrial)
      dfStat_Cat  <- dfStat_Cat %>% mutate(nTrial = .data$nTrial + nNumTrial)
      dfStat_Slot <- dfStat_Slot %>% mutate(nTrial = .data$nTrial + nNumTrial)
      # 追加
      dbWriteTable(con, sDBTABLE_SUBJECT, dfSubject, append = TRUE)
      dbWriteTable(con, sDBTABLE_CAT, dfStat_Cat, append = TRUE)
      dbWriteTable(con, sDBTABLE_SLOT, dfStat_Slot, append = TRUE)
    } else {
      # 上書きする場合
      dbWriteTable(con, sDBTABLE_SUBJECT, dfSubject, overwrite = TRUE)
      dbWriteTable(con, sDBTABLE_CAT, dfStat_Cat, overwrite = TRUE)
      dbWriteTable(con, sDBTABLE_SLOT, dfStat_Slot, overwrite = TRUE)
      # ダイジェストを記入
      dbWriteTable(con, "digest", data.frame(digest = sCurrentDigest), overwrite = TRUE)
    }
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[simBias] # Accumlated Trials:", countBias(sDBPATH, sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOT), "\n")
    }
    if (sVERBOSE == "detail"){
      cat("[simBias] Done.\n")
    }
    return()

  } else {
    # sDBPathが指定されなかった時
    # ここでreturn()していることに注意

    lOut <- list(
      dfSubject   = dfSubject,
      dfStat_Cat  = dfStat_Cat,
      dfStat_Slot = dfStat_Slot
    )
    if (sVERBOSE == "detail"){
      cat("[simBias] Done.\n")
    }
    return(lOut)
  }

}
getBias <- function(
  sDBPATH,
  sDBTABLE_CAT     = "weight_cat",
  sDBTABLE_SLOT    = "weight_slot",
  sTYPE = c("cat", "slot")
){
  #' get results of simulations to estimate bias
  #'
  #' バイアス評価シミュレーションの結果を取得する
  #'
  #' @export
  #'
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存したSQLite DBのフルパス
  #' @param sDBTABLE_CAT 文字列。
  #'    シミュレーションの結果得られたカテゴリ割付頻度を保存したSQLite DBのテーブル名
  #' @param sDBTABLE_SLOT 文字列。
  #'    シミュレーションの結果得られたスロット割付頻度を保存したSQLite DBのテーブル名
  #' @param sTYPE 文字列。
  #'    取得する結果のタイプ。
  #'    \itemize{
  #'    \item \code{"cat"}:  試行xカテゴリ別のバイアス
  #'    \item \code{"slot"}: 試行xスロット別のバイアス
  #'    }
  #'
  #' @return データフレーム。
  #'
  #'    \code{sTYPE == "cat"}のとき、行はある割付試行におけるあるカテゴリを表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nCat}: カテゴリ番号
  #'    \item \code{nNumAssignable}: 割付可能な対象者数
  #'    \item \code{nNumAssign}:     割付された対象者数
  #'    \item \code{gCVHatP}: 割付確率の推定値の変動係数
  #'    \item \code{gHatCVP}: 割付確率の変動係数の推定値
  #'    \item \code{gHatDeff}: デザイン効果の推定値
  #'    \item \code{gHatESS}: 実質標本サイズの推定値
  #'    \item \code{gCovHatQR}: スケーリングした割り付け確率の推定値と割付可能レシオ(全カテゴリに占める割付可能カテゴリの割合)との共分散の推定値
  #'    \item \code{gBarR_P}: 割付可能対象者における割付可能レシオの平均
  #'    \item \code{gBarR_S}: 割り付けられた対象者における割付可能レシオの平均
  #'    }
  #'
  #'    \code{sTYPE == "slot"}のとき、行はある試行におけるあるカテゴリのあるスロットを表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nParentCat}: カテゴリ番号
  #'    \item \code{nSlot}: スロット番号
  #'    \item \code{nNumAssignable}: 割付可能な対象者数
  #'    \item \code{nNumAssign}:     割付された対象者数
  #'    \item \code{gCVHatP}: 割付確率の推定値の変動係数
  #'    \item \code{gHatCVP}: 割付確率の変動係数の推定値
  #'    \item \code{gHatDeff}: デザイン効果の推定値
  #'    \item \code{gHatESS}: 実質標本サイズの推定値
  #'    \item \code{gCovHatQR}: スケーリングした割り付け確率の推定値と割付可能レシオ(全カテゴリに占める割付可能カテゴリの割合)との共分散の推定値
  #'    \item \code{gBarR_P}: 割付可能対象者における割付可能レシオの平均
  #'    \item \code{gBarR_S}: 割り付けられた対象者における割付可能レシオの平均
  #'    }
  #'
  #' @importFrom stats var
  #' @importFrom stats cov
  #'

  ## 引数チェック - - - - - -
  ## sTYPE
  ## チェックなし。推測する
  sTYPE <- match.arg(sTYPE)

  ## sDBPATH, sDBTABLE_*
  ## ファイルも存在しテーブルも存在する
  if (!checkDB(sDBPATH, c(sDBTABLE_CAT, sDBTABLE_SLOT)))
    stop("The DB is not found or seems invalid.")

  ## ここからメイン - - - - - - -

  # DBに接続
  con <- dbConnect(SQLite(), sDBPATH)
  # この関数を終えるとき、DBとの接続を切るように依頼
  on.exit(dbDisconnect(con))

  if (sTYPE == "cat"){
    # 調査対象者x割付可能カテゴリを行にした再割当試行統計
    dfIn <- tbl(con, sDBTABLE_CAT) %>%
      collect() %>%
      group_by(.data$nTrial, .data$nSubject, .data$nCat, .data$bAssign, .data$gAssignability_Cat) %>%
      summarize(
        nNumRetrial = sum(.data$nNumRetrial),
        gSumProp    = sum(.data$gSumProp),
        gSumSqProp  = sum(.data$gSumSqProp)
      ) %>%
      ungroup() %>%
      mutate(
        # gMeanProp: 割付率のブートストラップ平均
        gMeanProp = .data$gSumProp / .data$nNumRetrial,
        # gVarProp: 割付率のブートストラップ分散
        gVarProp = .data$gSumSqProp / .data$nNumRetrial - .data$gMeanProp^2,
        # gHatP: 割付確率の推定量
        gHatP = .data$gMeanProp,
        # gHatV: 割付確率の推定量の分散の推定量
        gHatV = .data$gVarProp / .data$nNumRetrial
      )
    # print(dfIn)
    # stop()

    # 割付可能対象者ベースの指標
    out.1 <- dfIn %>%
      # 試行xカテゴリごとの処理
      group_by(.data$nTrial, .data$nCat) %>%
      summarize(
        nNumAssignable = n(),
        # CV(\hat{p})
        gBarHatP = mean(.data$gHatP),
        gVarHatP = var(.data$gHatP) * (.data$nNumAssignable - 1) / .data$nNumAssignable,
        gCVHatP  = sqrt(.data$gVarHatP) / .data$gBarHatP,
        # \hat{CV}(p)
        gBarHatV = mean(.data$gHatV),
        gTemp = .data$gCVHatP^2 - .data$gBarHatV / (.data$gBarHatP^2),
        gTemp = if_else(.data$gTemp < 0, 0, .data$gTemp),
        gHatCVP  = sqrt(.data$gTemp),
        # Cov(\hat{q},r)
        gBarR_P  = mean(.data$gAssignability_Cat),
        gCovHatQR = cov( .data$gHatP / .data$gBarHatP, .data$gAssignability_Cat) * (.data$nNumAssignable - 1) / .data$nNumAssignable
      ) %>%
      ungroup() %>%
      dplyr::select(.data$nTrial, .data$nCat, .data$nNumAssignable, .data$gCVHatP, .data$gHatCVP, .data$gBarR_P, .data$gCovHatQR)
    # print(out.1)
    # stop()

    # 割付対象者ベースの指標
    out.2 <- dfIn %>%
      dplyr::filter(.data$bAssign == 1) %>%
      mutate(gHatW = 1/.data$gHatP) %>%
      # 試行xカテゴリごとの処理
      group_by(.data$nTrial, .data$nCat) %>%
      summarize(
        nNumAssign = n(),
        gBarHatW = mean(.data$gHatW),
        gVarHatW = var(.data$gHatW) * (.data$nNumAssign - 1) / .data$nNumAssign,
        gBarR_S   = mean(.data$gAssignability_Cat)
      ) %>%
      ungroup() %>%
      mutate(
        gHatDeff = 1 + .data$gVarHatW / .data$gBarHatW^2,
        gHatESS = .data$nNumAssign / .data$gHatDeff
      ) %>%
      dplyr::select(.data$nTrial, .data$nCat, .data$nNumAssign, .data$gHatDeff, .data$gHatESS, .data$gBarR_S)
    # print(out.2)
    # stop()

    out <- full_join(out.1, out.2, by = c("nTrial", "nCat")) %>%
      dplyr::select(
        .data$nTrial, .data$nCat, .data$nNumAssignable, .data$nNumAssign,
        .data$gCVHatP, .data$gHatCVP, .data$gHatDeff, .data$gHatESS,
        .data$gCovHatQR, .data$gBarR_P, .data$gBarR_S
      )
  }
  if (sTYPE == "slot"){
    # 調査対象者x割付可能カテゴリを行にした再割当試行統計
    dfIn <- tbl(con, sDBTABLE_SLOT) %>%
      collect() %>%
      group_by(.data$nTrial, .data$nSubject, .data$nParentCat, .data$nSlot, .data$bAssign, .data$gAssignability_Slot) %>%
      summarize(
        nNumRetrial = sum(.data$nNumRetrial),
        gSumProp    = sum(.data$gSumProp),
        gSumSqProp  = sum(.data$gSumSqProp)
      ) %>%
      ungroup() %>%
      mutate(
        # gMeanProp: 割付率のブートストラップ平均
        gMeanProp = .data$gSumProp / .data$nNumRetrial,
        # gVarProp: 割付率のブートストラップ分散
        gVarProp = .data$gSumSqProp / .data$nNumRetrial - .data$gMeanProp^2,
        # gHatP: 割付確率の推定量
        gHatP = .data$gMeanProp,
        # gHatV: 割付確率の推定量の分散の推定量
        gHatV = .data$gVarProp / .data$nNumRetrial
      )

    # 割付可能対象者ベースの指標
    out.1 <- dfIn %>%
      # 試行xカテゴリごとの処理
      group_by(.data$nTrial, .data$nParentCat, .data$nSlot) %>%
      summarize(
        nNumAssignable = n(),
        gBarHatP = mean(.data$gHatP),
        gVarHatP = var(.data$gHatP) * (.data$nNumAssignable - 1) / .data$nNumAssignable,
        gCVHatP  = sqrt(.data$gVarHatP) / .data$gBarHatP,
        gBarHatV = mean(.data$gHatV),
        gTemp = .data$gCVHatP^2 - .data$gBarHatV / (.data$gBarHatP^2),
        gTemp = if_else(.data$gTemp < 0, 0, .data$gTemp),
        gHatCVP  = sqrt(.data$gTemp),
        gBarR_P   = mean(.data$gAssignability_Slot),
        gCovHatQR = cov( .data$gHatP / .data$gBarHatP, .data$gAssignability_Slot) * (.data$nNumAssignable - 1) / .data$nNumAssignable
      ) %>%
      ungroup() %>%
      dplyr::select(
        .data$nTrial, .data$nParentCat, .data$nSlot, .data$nNumAssignable, .data$gCVHatP,
        .data$gCovHatQR, .data$gHatCVP, .data$gBarR_P
      )

    # 割付対象者ベースの指標
    out.2 <- dfIn %>%
      dplyr::filter(.data$bAssign == 1) %>%
      mutate(gHatW = 1/.data$gHatP) %>%
      # 試行xカテゴリごとの処理
      group_by(.data$nTrial, .data$nParentCat, .data$nSlot) %>%
      summarize(
        nNumAssign = n(),
        gBarHatW = mean(.data$gHatW),
        gVarHatW = var(.data$gHatW) * (.data$nNumAssign - 1) / .data$nNumAssign,
        gBarR_S   = mean(.data$gAssignability_Slot),
      ) %>%
      ungroup() %>%
      mutate(
        gHatDeff = 1 + .data$gVarHatW / .data$gBarHatW^2,
        gHatESS = .data$nNumAssign / .data$gHatDeff
      ) %>%
      dplyr::select(.data$nTrial, .data$nParentCat, .data$nSlot, .data$nNumAssign, .data$gHatDeff, .data$gHatESS, .data$gBarR_S)

    out <- full_join(out.1, out.2, by = c("nTrial", "nParentCat", "nSlot")) %>%
      dplyr::select(
        .data$nTrial, .data$nParentCat, .data$nSlot, .data$nNumAssignable, .data$nNumAssign,
        .data$gCVHatP, .data$gHatCVP, .data$gHatDeff, .data$gHatESS,
        .data$gCovHatQR, .data$gBarR_P, .data$gBarR_S
      )
  }

  return(out)
}

countBias <- function(
  sDBPATH,
  sDBTABLE_SUBJECT  = "subject",
  sDBTABLE_CAT      = "weight_cat",
  sDBTABLE_SLOT     = "weight_slot"
){
  #' count trials of bias simulations which are done so far
  #'
  #' これまでに実行されたバイアス評価シミュレーションの割付試行数を数える
  #'
  #' @export
  #'
  #' @param sDBPATH 文字列。シミュレーションの結果を保存するSQLite DBのフルパス。実在しなくてもよい。
  #' @param sDBTABLE_SUBJECT 文字列。割付シミュレーションの結果を保存するSQLite DBのテーブル名。
  #' @param sDBTABLE_CAT 文字列。再割付シミュレーションの結果(カテゴリ)を保存するSQLite DBのテーブル名。
  #' @param sDBTABLE_SLOT 文字列。再割付シミュレーションの結果(スロット)を保存するSQLite DBのテーブル名。
  #'
  #' @return 整数。割付試行数。指定されたsDBPATHが存在しない場合は0となる。
  #'
  #' @details  sDBPATHが存在するのにsDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOTが存在しない場合はエラーになる。
  #'
  #'    テーブルによって試行数が異なる場合はエラーになる。
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom DBI dbConnect
  #' @importFrom DBI dbDisconnect
  #' @importFrom DBI dbExistsTable
  #' @importFrom RSQLite SQLite
  #' @importFrom dplyr tbl
  #' @importFrom dplyr distinct
  #' @importFrom dplyr summarize
  #' @importFrom dplyr collect
  #' @importFrom dplyr pull
  #' @importFrom dplyr n
  #' @importFrom rlang .data

  ## 引数チェック - - - - - -
  ## sDBPATH
  ## チェックしない

  ## sDBTABLE_SUBJECT
  ## チェックしない

  ## sDBTABLE_CAT
  ## チェックしない

  ## sDBTABLE_SLOT
  ## チェックしない

  if (!file.exists(sDBPATH)){
    # ファイルが存在しない場合
    out <- 0

  } else {
    # ファイルが存在する場合
    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))

    if (!dbExistsTable(con, sDBTABLE_SUBJECT) | !dbExistsTable(con, sDBTABLE_CAT) | !dbExistsTable(con, sDBTABLE_SLOT))
      stop("The tables are not found.")

    out_subject <- tbl(con, sDBTABLE_SUBJECT) %>%
      distinct(.data$nTrial) %>%
      summarize(nNumTrial = n()) %>%
      collect() %>%
      pull(.data$nNumTrial)

    out_cat <- tbl(con, sDBTABLE_CAT) %>%
      distinct(.data$nTrial) %>%
      summarize(nNumTrial = n()) %>%
      collect() %>%
      pull(.data$nNumTrial)

    out_slot <- tbl(con, sDBTABLE_SLOT) %>%
      distinct(.data$nTrial) %>%
      summarize(nNumTrial = n()) %>%
      collect() %>%
      pull(.data$nNumTrial)

    stopifnot(out_cat == out_subject)
    stopifnot(out_slot == out_subject)
    out <- out_subject
  }
  out <- as.integer(out)
  return(out)
}
