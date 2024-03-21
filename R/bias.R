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
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return makeSurvey()の返し値。

  ## sVERBOSE
  ## 推測する
  sVERBOSE <- match.arg(sVERBOSE)

  if (sVERBOSE == "detail"){
    cat("[sub_makeSurvey_from_trial] start.\n")
  }


  # mbSubjectCat_Assignable: 対象者とカテゴリからカテゴリ割付可能性を引く表
  #                          (行を選ばれた対象者順にしたmbCAT)
  #                          仮想調査データを作る際のmbCATになる
  mbSubjectCat_Assignable <- lPOP$mbCAT[dfSubject$nPerson, , drop = FALSE]

  # mbSubjectCat_Assign: 対象者とカテゴリからカテゴリ割付有無を引く表
  dfSubjectCat_Assign <- dfSubject %>%
    dplyr::select(.data$nSubject, starts_with("nCat")) %>%
    dplyr::select(-.data$nCat) %>%
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
  anParentCat <- as.vector(dfSubject$nCat)
  mnAssignSlot <- as.matrix(dfSubject[, paste0("nSlot", seq_len(lSETTING$nSLOT_MAX))])

  # 調査データに変換
  # sVERBOSEはsimpleだったらnoneにする
  oSurvey <- makeSurvey(
    mbCAT         = mbSubjectCat_Assignable,
    lSLOT         = lCatSubjectSlot_Assignable,
    lSLOT_REQUEST = lSETTING$lSLOT_REQUEST,
    sCAT_TYPE     = lSETTING$sCAT_TYPE,
    sCAT_FILTER   = lSETTING$sCAT_FILTER,
    sCAT_ORDER    = lSETTING$sCAT_ORDER,
    sCAT_EXCLUDE  = lSETTING$sCAT_EXCLUDE,
    sSLOT_TYPE    = lSETTING$sSLOT_TYPE,
    sSLOT_FILTER  = lSETTING$sSLOT_FILTER,
    sSLOT_ORDER   = lSETTING$sSLOT_ORDER,
    sSLOT_EXCLUDE = lSETTING$sSLOT_EXCLUDE,
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
  #'    \itemize{
  #'      \item \code{dfSubject}:
  #'            行はある試行のある調査参加者を表す。
  #'            列は\code{execTrial()}の出力に試行番号 \code{nTrial}を追加したもの。
  #'      \item \code{dfStat_Cat}: 行はある試行のあるカテゴリを表す。
  #'            列は\code{execRetrial()}の出力のうち\code{dfStat_Cat}に試行番号 \code{nTrial}を追加したもの。
  #'      \item \code{dfStat_Slot}: 行はある試行のあるカテゴリのあるスロットを表す。
  #'            列は\code{execRetrial()}の出力のうち\code{dfStat_Slot}に試行番号 \code{nTrial}を追加したもの。
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

  # ダイジェスト
  sCurrentDigest <- digest(list(lPOP, lSETTING))

  ## 引数チェック - - - - -
  ## lPOP, lSETTING はexecTrialでチェックする
  ## nNUMRETRIAL, bPARALLEL, sLOGFILEはexecRetrialでチェックする

  ## nNUMTRIAL
  ## 指定されている
  if (is.null(nNUMTRIAL))
    stop("nNUMTRIAL is invalid.")

  ## bUSE_INFO_UNASSIGNED_CAT
  ## 期待通り
  if (!(bUSE_INFO_UNASSIGNED_CAT %in% c(TRUE, FALSE)))
    stop("bUSE_INFO_UNASSIGNED_CAT is invalid.")

  ## sDBPATH, sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOT
  ## ファイルが存在していない場合、ファイルは作成できるべき
  if (!is.null(sDBPATH) && !file.exists(sDBPATH)){
    file.create(sDBPATH)
    file.remove(sDBPATH)
  }
  ## ファイルが存在しており、追加しろといわれている場合、
  ## 整合性をチェック
  if (!is.null(sDBPATH) && file.exists(sDBPATH) && bAPPEND == TRUE){
    if (!checkDB(sDBPATH, c(sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOT), sCurrentDigest)){
      stop("The DB", sDBPATH, "seems invalid. Remove it and retry.")
    }
  }

  ## bAPPEND
  ## 値は期待通り
  stopifnot(bAPPEND %in% c(TRUE, FALSE))

  ## sVERBOSE
  ## 推測する
  sVERBOSE <- match.arg(sVERBOSE)

  ## ここからメイン - - - - -
  if (sVERBOSE == "detail"){
    cat("[simBias] start.\n")
  }

  lOut <- lapply(
    seq_len(nNUMTRIAL),
    function(nCurrentTrial){

      # 時間測定開始
      oTime <- proc.time()

      # まず標本抽出を行う。1試行だけ
      # sVERBOSEはsimpleだったらnoneにする
      dfSubject <- execTrials (
        lPOP       = lPOP,
        lSETTING   = lSETTING,
        nNUMTRIAL  = 1,
        bPARALLEL  = FALSE,
        sLOGFILE   = NULL,
        sVERBOSE   = if_else(sVERBOSE == "detail", "detail", "none")
      )

      # 仮想的調査データを作成する
      oSurvey <- sub_makeSurvey_from_trial(
        lPOP      = lPOP,
        lSETTING  = lSETTING,
        dfSubject = dfSubject,
        bUSE_INFO_UNASSIGNED_CAT = bUSE_INFO_UNASSIGNED_CAT,
        sVERBOSE  = if_else(sVERBOSE == "detail", "detail", "none")
      )

      # 再割付シミュレーション
      # sVERBOSEはsimpleだったらnoneにする
      lResult <- execRetrials(
        lSURVEY    = oSurvey,
        bREDRAW    = TRUE,
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
        left_join(dfSubject %>% dplyr::select(.data$nSubject, .data$sRowname, .data$nPerson), by = "nSubject")
      dfStat_Slot <- lResult$dfStat_Slot %>%
        mutate(nTrial = nCurrentTrial) %>%
        left_join(dfSubject %>% dplyr::select(.data$nSubject, .data$sRowname, .data$nPerson), by = "nSubject")
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
  sDBTABLE_SUBJECT = "subject",
  sDBTABLE_CAT  = "weight_cat",
  sDBTABLE_SLOT = "weight_slot",
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
  #' @param sDBTABLE_SUBJECT 文字列。
  #'    シミュレーションの結果得られたカテゴリ割付頻度を保存したSQLite DBのテーブル名
  #' @param sDBTABLE_CAT 文字列。
  #'    シミュレーションの結果得られたカテゴリ割付頻度を保存したSQLite DBのテーブル名
  #' @param sDBTABLE_SLOT 文字列。
  #'    シミュレーションの結果得られたスロット割付頻度を保存したSQLite DBのテーブル名
  #' @param sTYPE 文字列。
  #'    取得する結果のタイプ。
  #'    \itemize{
  #'    \item \code{"cat"}: 試行xカテゴリ別のバイアス
  #'    \item \code{"slot"}: 試行xスロット別のバイアス
  #'    }
  #'
  #' @return データフレーム。
  #'
  #'    \code{sTYPE == "cat"}のとき、行はある試行におけるあるカテゴリを表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nCat}: カテゴリ番号
  #'    \item \code{nFreq_Cat}: 割り付けられた対象者の数
  #'    \item \code{gVarWeight_Cat}: 割り付けられた対象者に与えられたウェイトの分散
  #'    \item \code{gDeff_Cat}: 割り付けられた対象者の回答のデザイン効果
  #'    \item \code{gESS_Cat}: 割り付けられた対象者の回答の実質標本サイズ
  #'    }
  #'
  #'    \code{sTYPE == "slot"}のとき、行はある試行におけるあるカテゴリを表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nCat}: カテゴリ番号
  #'    \item \code{nSlot}: スロット番号
  #'    \item \code{nFreq_Slot}: 割り付けられた調査対象者の数
  #'    \item \code{gVarWeight_Slot}: 割り付けられた対象者に与えられたウェイトの分散
  #'    \item \code{gDeff_Slot}: 割り付けられた対象者の回答のデザイン効果
  #'    \item \code{gESS_Slot}: 割り付けられた対象者の回答の実質標本サイズ
  #'    }
  #'
  #' @importFrom stats var
  #'
  # note:
  #   sDBTABLE_SUBJECTは実際には使用していない

  ## 引数チェック - - - - - -
  ## sTYPE
  ## チェックなし。推測する
  sTYPE <- match.arg(sTYPE)

  ## sDBPATH, sDBTABLE_*
  ## ファイルも存在しテーブルも存在する
  if (!checkDB(sDBPATH, c(sDBTABLE_SUBJECT, sDBTABLE_CAT, sDBTABLE_SLOT)))
    stop("The DB is not found or seems invalid.")

  ## ここからメイン - - - - - - -

  # DBに接続
  con <- dbConnect(SQLite(), sDBPATH)
  # この関数を終えるとき、DBとの接続を切るように依頼
  on.exit(dbDisconnect(con))

  if (sTYPE == "cat"){
    out <- tbl(con, sDBTABLE_CAT) %>%
      # 試行x対象者xカテゴリごとの処理
      mutate(
        # 割付確率
        gProb = (.data$nCount_SubjectCat+1)/(.data$nCount_Subject+1),
        # ウェイト
        gWeight = 1/.data$gProb
      ) %>%
      # 試行xカテゴリごとの処理
      # SQLiteではvar()が使えないようなので、ウェイトと割付確率のそれぞれについて
      # 平均と二乗の平均を求める
      group_by(.data$nTrial, .data$nCat) %>%
      summarize(
        nFreq_Cat = n(),
        gMean_Prob_Cat     = mean(.data$gProb, na.rm = TRUE),
        gMean_ProbSq_Cat   = mean(.data$gProb^2, na.rm = TRUE),
        gMean_Weight_Cat   = mean(.data$gWeight, na.rm = TRUE),
        gMean_WeightSq_Cat = mean(.data$gWeight^2, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      # ここでcollect()する
      collect() %>%
      mutate(
        # 分散は、二乗の平均から平均の二乗を引いた値
        gVar_Prob_Cat   = .data$gMean_ProbSq_Cat - .data$gMean_Prob_Cat^2,
        gVar_Weight_Cat = .data$gMean_WeightSq_Cat - .data$gMean_Weight_Cat^2,
        # 割付確率の変動係数
        gCV_Prob_Cat = sqrt(.data$gVar_Prob_Cat) / .data$gMean_Prob_Cat,
        # デザイン効果
        gDeff_Cat = 1 + .data$gVar_Weight_Cat / .data$gMean_Weight_Cat^2,
        # 実質標本サイズ
        gESS_Cat = .data$nFreq_Cat / .data$gDeff_Cat
      ) %>%
      dplyr::select(.data$nTrial, .data$nCat, .data$nFreq_Cat, .data$gCV_Prob_Cat, .data$gDeff_Cat, .data$gESS_Cat)
  }
  if (sTYPE == "slot"){
    out <- tbl(con, sDBTABLE_SLOT) %>%
      # 試行x対象者xカテゴリxスロットごとの処理
      mutate(
        gProb = (.data$nCount_SubjectSlot+1)/(.data$nCount_Subject+1),
        gWeight = 1/.data$gProb
      ) %>%
      # 試行xカテゴリxスロットごとの処理
      # SQLiteではvar()が使えないようなので、ウェイトと割付確率のそれぞれについて
      # 平均と二乗の平均を求める
      group_by(.data$nTrial, .data$nCat, .data$nSlot) %>%
      summarize(
        nFreq_Slot = n(),
        gMean_Prob_Slot     = mean(.data$gProb, na.rm = TRUE),
        gMean_ProbSq_Slot   = mean(.data$gProb^2, na.rm = TRUE),
        gMean_Weight_Slot   = mean(.data$gWeight, na.rm = TRUE),
        gMean_WeightSq_Slot = mean(.data$gWeight^2, na.rm = TRUE)
      ) %>%
      ungroup() %>%
      # ここでcollect()する
      collect() %>%
      mutate(
        # 分散は、二乗の平均から平均の二乗を引いた値
        gVar_Prob_Slot   = .data$gMean_ProbSq_Slot - .data$gMean_Prob_Slot^2,
        gVar_Weight_Slot = .data$gMean_WeightSq_Slot - .data$gMean_Weight_Slot^2,
        # 割付確率の変動係数
        gCV_Prob_Slot = sqrt(.data$gVar_Prob_Slot) / .data$gMean_Prob_Slot,
        # デザイン効果
        gDeff_Slot = 1 + .data$gVar_Weight_Slot / .data$gMean_Weight_Slot^2,
        # 実質標本サイズ
        gESS_Slot = .data$nFreq_Slot / .data$gDeff_Slot
      ) %>%
      dplyr::select(.data$nTrial, .data$nCat, .data$nSlot, .data$nFreq_Slot, .data$gCV_Prob_Slot, .data$gDeff_Slot, .data$gESS_Slot)
  }

  return(out)
}

countBias <- function(
  sDBPATH,
  sDBTABLE_SUBJECT  = "subject",
  sDBTABLE_CAT      = "weight_cat",
  sDBTABLE_SLOT     = "weight_slot"
){
  #' count trials of simulations which are done so far
  #'
  #' これまでに実行されたシミュレーション試行(割付試行)の数を数える
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
