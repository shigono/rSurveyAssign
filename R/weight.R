### weight.R
###   weight (調査ウェイト算出) に関わる関数
###
simWeight <- function(
  lSURVEY,
  bREDRAW       = TRUE,
  nBLOCKSIZE    = 100,
  nNUMBLOCK     = 1,
  bPARALLEL     = FALSE,
  sLOGFILE      = NULL,
  sDBPATH       = NULL,
  sDBTABLE_CAT  = "weight_cat",
  sDBTABLE_SLOT = "weight_slot",
  bAPPEND       = FALSE,
  sVERBOSE      = c("simple", "detail", "none")
){
  #' run simulations for computing weights
  #'
  #' ウェイト算出のためのシミュレーションを実行する
  #'
  #' @export
  #'
  #' @param lSURVEY `surveydata`クラスのオブジェクト。
  #'    調査データ。\code{\link{makeSurvey}}で生成する。
  #' @param bREDRAW 論理値。
  #'    FALSEにすると、再割付試行においてlSURVEYの対象者順を固定する。
  #' @param nNUMBLOCK 整数。
  #'    実行する再割付試行のブロック数。
  #' @param nBLOCKSIZE 整数。
  #'    ブロック内の再割付試行数。
  #' @param bPARALLEL 論理値。
  #'    並列処理するか。
  #' @param sLOGFILE 文字列。
  #'    並列処理する場合のログファイル(フルパス)。NULLだとログを出さない。
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス。
  #'    存在しない場合は作成する。
  #' @param sDBTABLE_CAT 文字列。
  #'    再割付シミュレーションの結果得られたカテゴリ割付頻度を保存するSQLite DBのテーブル名。
  #' @param sDBTABLE_SLOT 文字列。
  #'    再割付シミュレーションの結果得られたスロット割付頻度を保存するSQLite DBのテーブル名。
  #' @param bAPPEND 論理値。
  #'    結果をテーブルに{TRUE: 追加する, FALSE: 上書きする}。
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @details bAPPEND==TRUEで、sDBPATHが存在するがテーブルsDBTABLE_CAT,
  #'    sDBTABLE_SLOTが存在しない場合はエラーとなる。
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
  #'    シミュレーションの結果は次の2つのデータフレームからなる。
  #'
  #'    \code{dfStat_Cat}: カテゴリ割付頻度。行は調査対象者x割付カテゴリを表す。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nBlock}:           ブロック番号
  #'    \item \code{nBlockSize}:       ブロックサイズ(ブロック内の再割付試行数)
  #'    \item \code{nSubject}:         調査対象者番号
  #'    \item \code{nCat}:             カテゴリ番号
  #'    \item \code{nCount_Subject}:    ブロック内の再割付試行で調査対象者が出現した回数
  #'    \item \code{nCount_SubjectCat}: ブロック内の再割付試行で調査対象者が出現しカテゴリに割り付けられた回数
  #'    }
  #'
  #'    \code{dfStat_Slot}: スロット割付頻度。行は調査対象者x割付スロットを表す。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nBlock}:           ブロック番号
  #'    \item \code{nBlockSize}:       ブロックサイズ(ブロック内の再割付試行数)
  #'    \item \code{nSubject}:         調査対象者番号
  #'    \item \code{nCat}:             スロットが属するカテゴリ番号
  #'    \item \code{nSlot}:            スロット番号
  #'    \item \code{nCount_Subject}:   ブロック内の再割付試行で調査対象者が出現した回数
  #'    \item \code{nCount_SubjectSlot}: ブロック内の再割付試行で調査対象者が出現しスロットに割り付けられた回数
  #'    }
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom tibble tibble
  #' @importFrom dplyr left_join
  #' @importFrom dplyr arrange
  #' @importFrom dplyr pull
  #' @importFrom digest digest

  # ダイジェスト
  sCurrentDigest <- digest(lSURVEY)

  ## 引数チェック - - - - -
  ## lSURVEY, bREDRAW, nNUMBLOCK, nBLOCKSIZE, bPARALLEL, sLOGFILEは
  ## execRetrials()でチェックする

  ## sDBPATH, sDBTABLE_CAT, sDBTABLE_SLOT
  ## ファイルが存在していない場合、ファイルは作成できるべき
  if (!is.null(sDBPATH) && !file.exists(sDBPATH)){
    file.create(sDBPATH)
    file.remove(sDBPATH)
  }
  ## ファイルが存在しておりかつ追加しろといわれている場合、
  ## データベースとの整合性をチェック
  if (!is.null(sDBPATH) && file.exists(sDBPATH) && bAPPEND == TRUE){
    if (!checkDB(sDBPATH, c(sDBTABLE_CAT, sDBTABLE_SLOT), sCurrentDigest)){
      stop("The DB ", sDBPATH, " seems invalid. Remove it and retry.")
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
    cat("[simWeight] Hello\n")
  }

  lResult <- execRetrials(
    lSURVEY     = lSURVEY,
    bREDRAW     = bREDRAW,
    nBLOCKSIZE  = nBLOCKSIZE,
    nNUMBLOCK   = nNUMBLOCK,
    bPARALLEL   = bPARALLEL,
    sLOGFILE    = sLOGFILE,
    sVERBOSE    = sVERBOSE
  )

  if (!is.null(sDBPATH)){
    # sDBPATHが指定されたとき
    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))

    # DBに保存
    if (bAPPEND){
      # 追加する場合(実は初回かも知れない)
      # すでに行われたブロック数を加算し、過去とダブらないようにする
      nNumBlock <- countWeight(sDBPATH, sDBTABLE_CAT, sDBTABLE_CAT, sTYPE = "block")
      dfStat_Cat  <- lResult$dfStat_Cat %>% mutate(nBlock = .data$nBlock + nNumBlock)
      dfStat_Slot <- lResult$dfStat_Slot %>% mutate(nBlock = .data$nBlock + nNumBlock)
      dbWriteTable(con, sDBTABLE_CAT, dfStat_Cat, append = TRUE)
      dbWriteTable(con, sDBTABLE_SLOT, dfStat_Slot, append = TRUE)
    } else {
      # 上書きする場合
      dbWriteTable(con, sDBTABLE_CAT, lResult$dfStat_Cat, overwrite = TRUE)
      dbWriteTable(con, sDBTABLE_SLOT, lResult$dfStat_Slot, overwrite = TRUE)
    }
    # ダイジェストを記入
    # (bAPPEND==TRUEでも記入する。実は初回かも知れないから)
    dbWriteTable(con, "digest", data.frame(digest = sCurrentDigest), overwrite = TRUE)
    # 画面表示
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[simWeight] # Accumlated Trials:", countWeight(sDBPATH, sDBTABLE_CAT, sDBTABLE_SLOT, sTYPE = "retrial"), "\n")
    }
    if (sVERBOSE == "detail"){
      cat("[simWeight] Done.\n")
    }
    return()
  } else {
      # sDBPathが指定されなかった時
      if (sVERBOSE == "detail"){
        cat("[simWeight] Done.\n")
      }
      return(lResult)
  }
}
getWeight <- function(
  lSURVEY,
  sDBPATH,
  sDBTABLE_CAT  = "weight_cat",
  sDBTABLE_SLOT = "weight_slot",
  bCHECKDB = TRUE
){
  #' get results of simulations for computing weights
  #'
  #' ウェイト算出シミュレーションの結果を取得する
  #'
  #' @export
  #'
  #' @param lSURVEY `surveydata`クラスのオブジェクト。
  #'    調査データ。\code{\link{makeSurvey}}で生成する。
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存したSQLite DBのフルパス
  #' @param sDBTABLE_CAT 文字列。
  #'    シミュレーションの結果得られたカテゴリ割付頻度を保存したSQLite DBのテーブル名
  #' @param sDBTABLE_SLOT 文字列。
  #'    シミュレーションの結果得られたスロット割付頻度を保存したSQLite DBのテーブル名
  #' @param bCHECKDB 論理値。
  #'    指定されたSQLite DBをチェックするか。
  #'
  #' @return a list. 要素は次の通り。
  #'   \itemize{
  #'   \item \code{dfWeight_Cat}: a data frame. 行はある対象者のある割付カテゴリ
  #'   \item \code{dfWeight_Slot}: a data frame. 行はある対象者のある割付スロット
  #'   }
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom dplyr tbl
  #' @importFrom dplyr collect
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr summarize
  #' @importFrom dplyr n
  #' @importFrom dplyr filter
  #' @importFrom dplyr row_number
  #' @importFrom tidyselect starts_with
  #' @importFrom tidyr pivot_longer
  #' @importFrom DBI dbConnect
  #' @importFrom DBI dbDisconnect
  #' @importFrom RSQLite dbWriteTable
  #' @importFrom RSQLite SQLite
  #' @importFrom rlang .data
  #' @importFrom digest digest

  # ダイジェスト
  sCurrentDigest <- digest(lSURVEY)

  ## 引数チェック - - - - - -
  ## sDBPATH, sDBTABLE_CAT, sDBTABLE_SLOT
  if (bCHECKDB){
    ## ファイルが存在しダイジェストと一致する
    if (!checkDB(sDBPATH, c(sDBTABLE_CAT, sDBTABLE_SLOT), sCurrentDigest))
      stop("The DB ", sDBPATH, " seems invalid.")
  }

  ## ここからメイン - - - - - - -

  # DBに接続
  con <- dbConnect(SQLite(), sDBPATH)
  # この関数を終えるとき、DBとの接続を切るように依頼
  on.exit(dbDisconnect(con))

  # カテゴリ割付頻度
  dfStat_Cat <- tbl(con, sDBTABLE_CAT) %>%
    dplyr::filter(.data$bAssign == 1) %>%
    group_by(.data$nSubject, .data$nCat) %>%
    summarize(
      nNumRetrial       = sum(.data$nBlockSize, na.rm=TRUE),
      nCount_SubjectCat = sum(.data$nCount_SubjectCat, na.rm=TRUE),
      nCount_Subject    = sum(.data$nCount_Subject, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    collect()

  # スロット割付頻度
  dfStat_Slot <- tbl(con, sDBTABLE_SLOT) %>%
    dplyr::filter(.data$bAssign == 1) %>%
    group_by(.data$nSubject, .data$nCat, .data$nSlot) %>%
    summarize(
      nNumRetrial        = sum(.data$nBlockSize, na.rm=TRUE),
      nCount_SubjectSlot = sum(.data$nCount_SubjectSlot, na.rm=TRUE),
      nCount_Subject     = sum(.data$nCount_Subject, na.rm=TRUE)
    ) %>%
    ungroup() %>%
    collect()

  # カテゴリのウェイト
  mnAssignCat <- lSURVEY$mnASSIGNCAT
  colnames(mnAssignCat) <- paste0("nCat_", seq_len(ncol(mnAssignCat)))
  dfWeight_Cat <- as_tibble(mnAssignCat) %>%
    mutate(
      nSubject = row_number()
    ) %>%
    pivot_longer(
      cols = starts_with("nCat_"),
      names_to = "sVar",
      values_to = "nCat"
    ) %>%
    dplyr::select(-.data$sVar) %>%
    # NAを除外
    filter(!is.na(.data$nCat)) %>%
    # dfStat_Catをくっつける.
    left_join(dfStat_Cat, by = c("nSubject", "nCat")) %>%
    replace_na(list(nCount_SubjectCat = 0, nCount_Subject = 0)) %>%
    # ウェイト計算
    mutate(
      gProb = (.data$nCount_SubjectCat+1)/(.data$nCount_Subject+1),
      gWeight = 1/.data$gProb
    ) %>%
    # 規準化
    mutate(gWeight = .data$gWeight / mean(.data$gWeight))

  # スロットのウェイト
  mnAssignSlot <- lSURVEY$mnASSIGNSLOT
  colnames(mnAssignSlot) <- paste0("nSlot_", seq_len(ncol(mnAssignSlot)))
  dfWeight_Slot <- as_tibble(mnAssignSlot) %>%
    mutate(
      nSubject = row_number(),
      nCat = lSURVEY$anPARENTCAT
    ) %>%
    pivot_longer(
      cols = starts_with("nSlot_"),
      names_to = "sVar",
      values_to = "nSlot"
    ) %>%
    dplyr::select(-.data$sVar) %>%
    # NAを除外
    filter(!is.na(.data$nSlot)) %>%
    # dfStat_slotをくっつける.
    left_join(dfStat_Slot, by = c("nSubject", "nCat", "nSlot")) %>%
    replace_na(list(nCount_SubjectSlot = 0, nCount_Subject = 0)) %>%
    # ウェイト計算
    mutate(
      gProb = (.data$nCount_SubjectSlot+1)/(.data$nCount_Subject+1),
      gWeight = 1/.data$gProb
    ) %>%
    # 規準化
    mutate(gWeight = .data$gWeight / mean(.data$gWeight))

  lOut <- list(
    dfWeight_Cat  = dfWeight_Cat,
    dfWeight_Slot = dfWeight_Slot
  )
  return(lOut)
}
countWeight <- function(
  sDBPATH,
  sDBTABLE_CAT  = "weight_cat",
  sDBTABLE_SLOT = "weight_slot",
  sTYPE = c("retrial", "block")
){
  #' count trials of weight simulations which are done so far
  #'
  #' これまでに実行されたウェイト算出シミュレーションの再割付試行数を数える
  #'
  #' @export
  #'
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス。
  #'    実在しなくてもよい。
  #' @param sDBTABLE_CAT 文字列。
  #'    シミュレーションの結果(カテゴリ)を保存するSQLite DBのテーブル名。
  #'    実在しなくてもよい。
  #' @param sDBTABLE_SLOT 文字列。
  #'    シミュレーションの結果(スロット)を保存するSQLite DBのテーブル名。
  #'    実在しなくてもよい。
  #' @param sTYPE 文字列。返し値の種類。以下のいずれか:
  #' \itemize{
  #' \item \code{"retrial"}: 再割付試行数を返す
  #' \item \code{"block"}: 再割付試行のブロック数を返す
  #' }
  #'
  #' @return an integer.
  #'    再割付試行数ないしブロック数。
  #'    指定されたsDBPATH, sDBTABLE_CAT, sDBTABLE_SLOTが存在しない場合は0となる。
  #'
  #' @details
  #'    sDBPATHが存在し、sDBTABLE_CAT, sDBTABLE_SLOTのいずれかのみが存在する場合は
  #'    エラーとなる。
  #'
  #'    sDBPATHが存在し、sDBTABLE_CATとsDBTABLE_SLOTで試行数が異なる場合はエラーになる。
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

  ## sDBTABLE_CAT, sDBTABLE_SLOT
  ## あとでチェックする(sDBPATHが存在しないかもしれないから)

  ## sTYPE
  ## チェックなし。推測する
  sTYPE <- match.arg(sTYPE)

  ## ここからメイン - - - - - -

  if (!file.exists(sDBPATH)){
    # ファイルが存在しない場合
    out <- 0

  } else {
    # ファイルが存在する場合
    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))

    # trap: テーブルは両方存在するか、両方存在しないかのどちらか
    if (dbExistsTable(con, sDBTABLE_CAT) != dbExistsTable(con, sDBTABLE_SLOT))
      stop("only one of two tables is found.")

    if (dbExistsTable(con, sDBTABLE_CAT)){
      # テーブルが存在する場合
      if (sTYPE == "retrial"){
        out_cat <- tbl(con, sDBTABLE_CAT) %>%
          distinct(.data$nBlock, .data$nBlockSize) %>%
          summarize(nNumRetrial = sum(.data$nBlockSize, na.rm=TRUE)) %>%
          collect() %>%
          pull(.data$nNumRetrial)
        out_slot <- tbl(con, sDBTABLE_SLOT) %>%
          distinct(.data$nBlock, .data$nBlockSize) %>%
          summarize(nNumRetrial = sum(.data$nBlockSize, na.rm=TRUE)) %>%
          collect() %>%
          pull(.data$nNumRetrial)
      }
      if (sTYPE == "block"){
        out_cat <- tbl(con, sDBTABLE_CAT) %>%
          distinct(.data$nBlock) %>%
          summarize(nNumBlock = n()) %>%
          collect() %>%
          pull(.data$nNumBlock)
        out_slot <- tbl(con, sDBTABLE_SLOT) %>%
          distinct(.data$nBlock) %>%
          summarize(nNumBlock = n()) %>%
          collect() %>%
          pull(.data$nNumBlock)
      }
      # trap: 行数がちがう
      if (out_cat != out_slot){
        stop("the tables are not compatible.")
      }
      out <- out_cat
    } else {
      # テーブルが存在する場合
      out <- 0
    }

  }
  out <- as.integer(out)

  return(out)
}
trimWeight <- function(
  agWEIGHT,
  gLIMIT = 5
){
  #' trim Weight
  #'
  #' ウェイトを切り詰める
  #'
  #' @export
  #'
  #' @param agWEIGHT 数値ベクトル。ウェイト。
  #'
  #' @param gLIMIT 切り詰める限界。
  #'
  #' @return 数値ベクトル。agWEIGHTが上限gLIMIT, 下限1/gLIMITの範囲に
  #' 収まっていたらそのまま返す。そうでないときは、
  #' この範囲に切り詰め、かつ平均を1としたベクトルを返す。

  while(max(agWEIGHT, na.rm=TRUE) > gLIMIT | min(agWEIGHT, na.rm=TRUE) < 1/gLIMIT){
    agWEIGHT <- if_else(agWEIGHT > gLIMIT, gLIMIT, agWEIGHT)
    agWEIGHT <- if_else(agWEIGHT < 1/gLIMIT, 1/gLIMIT, agWEIGHT)
    agWEIGHT <- agWEIGHT / mean(agWEIGHT, na.rm=TRUE)
  }

  return(agWEIGHT)
}
# - - - - - -
