### size.R
###   size (標本サイズ予測) に関わる関数
###
simSize <- function(
  lPOP,
  lSETTING,
  nNUMTRIAL  = 1,
  bPARALLEL  = FALSE,
  sLOGFILE   = NULL,
  sDBPATH    = NULL,
  sDBTABLE   = "size",
  bAPPEND    = TRUE,
  sVERBOSE   = c("simple", "detail", "none")
){
  #' run simulations for forecasting sample size
  #'
  #' 標本サイズ推定のためのシミュレーションを行う
  #'
  #' @export
  #'
  #' @param lPOP `popdata`クラスのオブジェクト。
  #'    母集団データ。\code{\link{makePop}}で生成する。
  #' @param lSETTING `assignsetting`クラスのオブジェクト。
  #'    割付のセッティング。\code{\link{makeSetting}} で生成する。
  #'    lSLOT_REQUESTに指定されたスロット名はlPOPのスロット名と一致すること。
  #' @param nNUMTRIAL 整数。
  #'    割付シミュレーションの試行数。
  #' @param bPARALLEL 論理値。
  #'    並列処理するか。
  #' @param sLOGFILE 文字列。
  #'    並列処理する場合のログファイル(フルパス)。NULLだとログを出さない。
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス。
  #' @param sDBTABLE 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのテーブル名。
  #' @param bAPPEND 論理値。
  #'    TRUE: 結果をテーブルに追加する。FALSE: 上書きする。
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return
  #'    \itemize{
  #'      \item sDBPATHを指定しない場合は、シミュレーションの結果をデータフレームとして返す。
  #'      \item sDBPATHを指定した場合はNULLを返す。シミュレーションの結果は、
  #'            SQLiteデータベースにテーブルとして保存される。sDBPATH が存在する場合は、
  #'            bAPPEND==FALSEであればSDBPATHに上書きし、bAPPEND==TRUEであれば追加する。
  #'            sDBPATHが存在しない場合は新規作成する。
  #'    }
  #'
  #'    シミュレーションの結果は以下の形式を持つ。
  #'    行はある試行のある調査参加者を表す。0行かもしれないことに注意。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nSubject}:         対象者番号 (調査参加順の連番)
  #'    \item \code{nPerson}:          母集団メンバー番号 (\code{lPOP$mbCAT}の行番号)
  #'    \item \code{sRowname}:         \code{lPOP$mbCAT}の行名。\code{lPOP$mbCAT}に行名がない場合はas.character(SEQ)となる
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号(\code{lPOP$mbCAT}の列番号)、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{nMAXCAT}のカテゴリ番号(\code{lPOP$mbCAT}の列番号)、ないし\code{NA}
  #'    \item \code{nParentCat}:       割付スロットが属するカテゴリ番号(\code{lPOP$mbCAT}の列番号)、ないし\code{NA}
  #'    \item \code{nSlot_1}:          割付スロット\code{1}のスロット番号(\code{lPOP$mbSLOT[[nCat]]}の列番号)、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nMAXSLOT): 割付スロット\code{nMAXSLOT}のスロット番号(\code{lPOP$mbSLOT[[nCat]]}の列番号)、ないし\code{NA}
  #'    }
  #'
  #' @details bAPPEND==TRUEで、sDBPATHが存在するがテーブルsDBTABLEが存在しない場合は
  #' エラーとなる。
  #'
  #' @importFrom DBI dbConnect
  #' @importFrom DBI dbDisconnect
  #' @importFrom DBI dbListTables
  #' @importFrom RSQLite dbWriteTable
  #' @importFrom RSQLite SQLite
  #' @importFrom digest digest

  ## あいさつ
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[simSize] Hi.\n")
  }

  ## 引数チェック - - - - - - -
  ## lPOP, lSETTING, nNUMTRIAL, bPARALLEL, sLOGFILEはexecTrialsでチェックする

  ## bAPPEND
  ## 期待通り
  stopifnot(bAPPEND %in% c(TRUE, FALSE))

  ## sDBPATH, sDBTABLE
  if (!is.null(sDBPATH)){
    ## 指定されたら
    if (!file.exists(sDBPATH)){
      # 存在しなかったら
      # ファイルは作成できるべき
      file.create(sDBPATH)
      file.remove(sDBPATH)
    } else {
      # 存在したら
      if (bAPPEND){
        # 追加の場合
        # データベースとの整合性をチェック
        if (!checkDB(sDBPATH, sDBTABLE, sCurrentDigest)){
          stop("The DB ", sDBPATH, " seems invalid. Remove it and retry.")
        }
      }

    }
  }

  ## ここからメイン - - - - - - -

  # ダイジェスト
  sCurrentDigest <- digest(list(lPOP, lSETTING))

  dfTrial <- execTrials(
    lPOP       = lPOP,
    lSETTING   = lSETTING,
    nNUMTRIAL  = nNUMTRIAL,
    bPARALLEL  = bPARALLEL,
    sLOGFILE   = sLOGFILE,
    sVERBOSE   = sVERBOSE
  )

  if (!is.null(sDBPATH)){
    # sDBPATHが指定されたとき
    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))

    # DBに保存
    if (bAPPEND){
      # 追加する場合
      # すでに行われた試行数を加算し、過去とダブらないようにする
      nNumTrial <- countSize(sDBPATH, sDBTABLE)
      dfTrial <- dfTrial %>%
        mutate(nTrial = .data$nTrial + nNumTrial)
      dbWriteTable(con, sDBTABLE, dfTrial, append = TRUE)
    } else {
      # 上書きする場合
      dbWriteTable(con, sDBTABLE, dfTrial, overwrite = TRUE)
      # ダイジェストを記入
      dbWriteTable(con, "digest", data.frame(digest = sCurrentDigest), overwrite = TRUE)
    }
    # 画面表示
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[simSize] # Accumlated Trials:", countSize(sDBPATH, sDBTABLE), "\n")
    }
    if (sVERBOSE == "detail"){
      cat("[simSize] Done.\n")
    }
    return()
  } else {
    # sDBPathが指定されなかった時
    if (sVERBOSE == "detail"){
      cat("[simSize] Done.\n")
    }
    return(dfTrial)
  }
}
getSize_raw <- function(
  sDBPATH,
  sDBTABLE = "size",
  nTRIAL   = NULL
) {
  #' get rawdata of simulations for forecasting sample size
  #'
  #' 標本サイズ予測シミュレーションのローデータを取得する
  #'
  #' @export
  #'
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス
  #' @param sDBTABLE 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのテーブル名
  #' @param nTRIAL 整数。
  #' 試行番号。存在しないときは0行のデータを返す。
  #' 指定しないときは全試行を返す
  #'
  #' @return データフレーム。列は\code{\link{simSize}}を参照。
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom dplyr tbl
  #' @importFrom dplyr collect
  #' @importFrom dplyr filter
  #' @importFrom DBI dbConnect
  #' @importFrom DBI dbDisconnect
  #' @importFrom RSQLite dbWriteTable
  #' @importFrom RSQLite SQLite

  ## 引数チェック - - - - - -
  ## sDBPATH, sDBTABLE
  ## ファイルも存在テーブルも存在する
  if (!checkDB(sDBPATH, sDBTABLE))
    stop("The DB is not found or seems invalid.")

  ## nTRIAL
  ## チェック無し

  ## ここからメイン - - - - - - -

  # DBに接続
  con <- dbConnect(SQLite(), sDBPATH)
  # この関数を終えるとき、DBとの接続を切るように依頼
  on.exit(dbDisconnect(con))

  if (is.null(nTRIAL)){
    out <- tbl(con, sDBTABLE) %>% collect()
  } else {
    out <- tbl(con, sDBTABLE) %>% filter(.data$nTrial == nTRIAL) %>% collect()
  }

  return(out)
}
getSize <- function(
  sDBPATH,
  sDBTABLE = "size",
  sTYPE    = c("subject", "cat", "slot")
){
  #' get results of simulations for forecasting sample size
  #'
  #' 標本サイズ予測シミュレーションの結果を取得する
  #'
  #' @export
  #'
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス
  #' @param sDBTABLE 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのテーブル名
  #' @param sTYPE 文字列。
  #'    取得する結果のタイプ。以下のいずれか:
  #'    \itemize{
  #'    \item \code{"subject"}: 標本サイズ(調査参加者数)
  #'    \item \code{"cat"}: カテゴリ票数
  #'    \item \code{"slot"}: スロット票数
  #'    }
  #'
  #' @return データフレーム。
  #'
  #'    \code{sTYPE == "subject"}のとき、行は試行を表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nNum_scr}: 調査対象者
  #'    \item \code{nNum_main}: 調査対象者のうち、スロット割付を受けた人数
  #'    }
  #'
  #'    \code{sTYPE == "cat"}のとき、行はある試行におけるあるカテゴリを表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nCat}: カテゴリ番号
  #'    \item \code{nNum}: そのカテゴリを割り付けられた人数
  #'    }
  #'
  #'    \code{sTYPE == "slot"}のとき、行はある試行におけるあるカテゴリを表す。
  #'    列は以下のとおり:
  #'    \itemize{
  #'    \item \code{nTrial}: 試行番号
  #'    \item \code{nCat}:   カテゴリ番号
  #'    \item \code{nSlot}:  スロット番号
  #'    \item \code{nNum}:   そのスロットを割り付けられた人数
  #'    }
  #'
  #'    いずれも、抽出が行われなかった試行については出力されない。
  #'    すべての試行で抽出が行われていない場合は0行のデータフレームとなる。
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom dplyr tbl
  #' @importFrom dplyr collect
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr summarize
  #' @importFrom dplyr n
  #' @importFrom dplyr filter
  #' @importFrom tidyselect starts_with
  #' @importFrom tidyr pivot_longer
  #' @importFrom DBI dbConnect
  #' @importFrom DBI dbDisconnect
  #' @importFrom RSQLite dbWriteTable
  #' @importFrom RSQLite SQLite
  #' @importFrom rlang .data

  ## 引数チェック - - - - - -
  ## sTYPE
  ## チェックなし。推測する
  sTYPE <- match.arg(sTYPE)

  ## sDBPATH, sDBTABLE
  ## ファイルも存在テーブルも存在する
  if (!checkDB(sDBPATH, sDBTABLE))
    stop("The DB is not found or seems invalid.")

  ## ここからメイン - - - - - - -

  # DBに接続
  con <- dbConnect(SQLite(), sDBPATH)
  # この関数を終えるとき、DBとの接続を切るように依頼
  on.exit(dbDisconnect(con))

  if (sTYPE == "subject"){
    out <- tbl(con, sDBTABLE) %>%
      group_by(.data$nTrial) %>%
      summarize(
        nNum_scr = n(),
        nNum_main = sum(!is.na(.data$nParentCat), na.rm = TRUE)
      ) %>%
      ungroup() %>%
      collect()
  }

  if (sTYPE == "cat"){
    out <- tbl(con, sDBTABLE) %>%
      dplyr::select(.data$nTrial, starts_with("nCat")) %>%
      collect() %>%
      pivot_longer(
        cols = starts_with("nCat"),
        names_to = "sVar",
        values_to = "nCat"
      ) %>%
      filter(!is.na(.data$nCat)) %>%
      group_by(.data$nTrial, .data$nCat) %>%
      summarize(nNum = n()) %>%
      ungroup()
  }

  if (sTYPE == "slot"){
    out <- tbl(con, sDBTABLE) %>%
      filter(!is.na(.data$nParentCat)) %>%
      dplyr::select(.data$nTrial, .data$nParentCat, starts_with("nSlot")) %>%
      collect() %>%
      pivot_longer(
        cols = starts_with("nSlot"),
        names_to = "sVar",
        values_to = "nSlot"
      ) %>%
      filter(!is.na(.data$nSlot)) %>%
      group_by(.data$nTrial, .data$nParentCat, .data$nSlot) %>%
      summarize(nNum = n()) %>%
      ungroup()
    # print(out)
    # stop()
  }

  return(out)
}
countSize <- function(
  sDBPATH,
  sDBTABLE = "size"
){
  #' count trials of size simulations which are done so far
  #'
  #' これまでに実行された標本サイズ予測シミュレーションの割付試行を数える
  #'
  #' @export
  #'
  #' @param sDBPATH 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのフルパス。
  #'    実在しなくてもよい。
  #' @param sDBTABLE 文字列。
  #'    シミュレーションの結果を保存するSQLite DBのテーブル名。
  #'
  #' @return 整数。
  #'    試行数。指定されたsDBPATHが存在しない場合は0。
  #'
  #' @details sDBPATHが存在するのにsDBTABLEが存在しない場合はエラー。
  #'
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

  ## 引数チェック - - - - -
  ## sDBPATH
  ## チェックなし

  ## sDBTABLE
  ## チェックなし(sDBPATHが存在しないかも知れないから)

  ## ここからメイン - - - - -

  # ファイルの存在で分岐
  if (!file.exists(sDBPATH)){
    # ファイルが存在しない場合
    out <- 0

  } else {
    # ファイルが存在する場合
    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))

    # trap: テーブルは存在する
    if (!dbExistsTable(con, sDBTABLE))
      stop("the table is not found.")

    out <- tbl(con, sDBTABLE) %>%
      distinct(.data$nTrial) %>%
      summarize(n()) %>%
      collect() %>%
      pull()

  }
  out <- as.integer(out)

  return(out)
}
