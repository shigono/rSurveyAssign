execTrials <- function(
    lPOP,
    lSETTING,
    nNUMTRIAL,
    bPARALLEL,
    sLOGFILE,
    sVERBOSE = c("simple", "detail", "none")
){
  #' Internal: run simulation of assignment
  #'
  #' Internal: 割付シミュレーションを行う.
  #' simSize(), simBias()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param lPOP `popdata`クラスのオブジェクト。
  #'    母集団データ。\code{\link{makePop}}で生成する。
  #' @param lSETTING `assignsetting`クラスのオブジェクト。
  #'    割付のセッティング。\code{\link{makeSetting}} で生成する。
  #'    スロット名はlPOPと一致すること。
  #' @param nNUMTRIAL 整数。
  #'    割付シミュレーションの試行数。
  #' @param bPARALLEL a logical.
  #'    並列処理するか。
  #' @param sLOGFILE a string.
  #'    並列処理する場合のログファイル(フルパス)。NULLだとログを出さない。
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return データフレーム。
  #'    行はある試行のある調査参加者を表す。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
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
  #' @importFrom magrittr "%>%"
  #' @importFrom tibble as_tibble
  #' @importFrom dplyr mutate
  #' @importFrom dplyr bind_rows
  #' @importFrom dplyr rename
  #' @importFrom rlang .data
  #'
  #' @importFrom stats runif
  #' @importFrom foreach foreach
  #' @importFrom foreach "%do%"
  #' @importFrom foreach "%dopar%"
  #' @importFrom parallel detectCores
  #' @importFrom parallel makeCluster
  #' @importFrom parallel stopCluster
  #' @importFrom doParallel registerDoParallel
  #' @importFrom doRNG registerDoRNG
  #' @importFrom doRNG "%dorng%"

  ## あいさつのためsVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[execTrials] Hi.\n")
  }

  # 引数チェック - - - - -
  ## lPOP
  ## クラスは期待通り
  stopifnot("popdata" %in% class(lPOP))

  ## lSETTING
  ## クラス
  stopifnot("assignsetting" %in% class(lSETTING))
  ## lSLOT_REQUESTの要素数はlPOP$mbCATの列数と同じ
  stopifnot(length(lSETTING$lSLOT_REQUEST) == ncol(lPOP$mbCAT))
  ## lSLOT_REQUESTの各要素の長さはlPOP$lSLOTの各要素の長さと同じ
  stopifnot( sapply(lSETTING$lSLOT_REQUEST, length) == sapply(lPOP$lSLOT, ncol) )
  ## lSLOT_REQUESTの要素名はlPOP$lSLOTの要素名と同じ
  stopifnot( unlist(lapply(lSETTING$lSLOT_REQUEST, names)) == unlist(lapply(lPOP$lSLOT, colnames)) )

  ## nNUMTRIAL
  ## 欠損無し
  stopifnot(!is.na(nNUMTRIAL))

  ## bPARALLEL
  ## 値は期待通り
  stopifnot(bPARALLEL %in% c(TRUE, FALSE))

  ## sLOGFILE
  ## 指定されていたら、ファイルは作成できるべき
  if (!is.null(sLOGFILE)){
    file.create(sLOGFILE)
    file.remove(sLOGFILE)
  }

  ## ここからメイン - - - - -
  # nFrameSize: 実際に設定する対象者数上限。
  # nMAXSIZEが指定されていない場合は、対象者数の10倍とする
  if (lSETTING$nSUBJECT_MAX == 0){
    nFrameSize <- nrow(lPOP$mbCAT) * 10
  } else {
    nFrameSize <- lSETTING$nSUBJECT_MAX
  }

  # 標本抽出台帳を作る
  # 対象者のデータから、対象者を無作為復元抽出してつくっている
  # 試行ごとにつくってもよいのだが、この関数の中で
  # 一回だけつくっている。そのかわり試行ごとに標本抽出台帳から無作為非復元抽出する

  # anFrameRow_Person: 台帳の行番号から対象者番号を引くベクトル. 長さnFrameSize
  # 対象者を復元抽出
  if (sVERBOSE == "detail"){
    cat("[execTrials] make anFrameRow_Person ...\n")
  }
  anFrameRow_Person <- sample(seq_len(nrow(lPOP$mbCAT)), nFrameSize, replace = TRUE)

  # mnFrameRowCat_Person: 台帳の行番号とカテゴリから、参照すべき対象者番号を引く行列
  # (各列の中身はanFrameRow_Personと同じ)
  if (sVERBOSE == "detail"){
    cat("[execTrials] make mbPerson_Cat_Alt ...\n")
  }
  mnFrameRowCat_Person <- matrix(
    rep(anFrameRow_Person, ncol(lPOP$mbCAT)),
    ncol = ncol(lPOP$mbCAT),
  )
  colnames(mnFrameRowCat_Person) <- colnames(lPOP$mbCAT)

  nTrial <- NA

  if (bPARALLEL){
    # 並列処理 - - - - - - - - - - -
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[execTrials] start clusters ...\n")
    }

    # ログファイルがすでに存在していたら削除
    if (!is.null(sLOGFILE) && file.exists(sLOGFILE)){
      file.remove(sLOGFILE)
    }

    sLOGFILE <- ifelse(is.null(sLOGFILE), "", sLOGFILE)

    # クラスタをつくる
    cl <- makeCluster(
      detectCores() - 1,
      outfile = sLOGFILE
    )
    # この関数から抜けるときにクラスタを止める
    on.exit(stopCluster(cl))

    registerDoParallel(cl)
    registerDoRNG(NULL)

    if (sVERBOSE %in% c("simple", "detail")){
      cat("[execTrials] start trials (parallel) ...\n")
    }

    lTrial <- foreach(
      nTrial = seq_len(nNUMTRIAL),
      .export = c("execAssign")
    ) %dorng% {

      # 時間測定開始
      oTime <- proc.time()

      # 乱数生成チェック
      gRand <- runif(1)

      # anSEQ_FrameRow: 配信順から台帳の行番号を引くベクトル. 長さnFrameSize
      anSEQ_FrameRow <- sample(seq_len(nFrameSize), nFrameSize, replace = FALSE)

      # スロット割付を行う
      mnTrial <- execAssign (
        anSEQ_PERSON      = anFrameRow_Person[anSEQ_FrameRow],
        mnSEQ_CAT_PERSON  = mnFrameRowCat_Person[anSEQ_FrameRow, , drop = FALSE],
        mbPERSON_CAT_USE  = lPOP$mbCAT,
        lPERSON_SLOT_HIT  = lPOP$lSLOT,
        lSETTING          = lSETTING,
        sVERBOSE          = sVERBOSE
      )
      # 結果に、試行番号, 列名をつける
      dfTrial <- as_tibble(mnTrial, rownames = "sRowname") %>%
        mutate(
          nTrial = nTrial
        )
      # 画面表示(どうせログにしか出ないので常に)
      cat("[execTrials] nTrial:", nTrial, "; extract:", nrow(dfTrial), ";", (proc.time()-oTime)[3], "sec.\n")

      return(dfTrial)
    }
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[execTrials] end trials.\n")
    }


  } else {
    # 逐次処理 - - - - - - - - - - -
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[execTrials] start trials (serial) ...\n")
    }

    lTrial <- foreach(
      nTrial = seq_len(nNUMTRIAL)
    ) %do% {
      # ここから並列処理と同じ

      # 時間測定開始
      oTime <- proc.time()

      # anSEQ_FrameRow: 配信順から台帳の行番号を引くベクトル. 長さnFrameSize
      anSEQ_FrameRow <- sample(seq_len(nFrameSize), nFrameSize, replace = FALSE)

      # スロット割付を行う
      mnTrial <- execAssign (
        anSEQ_PERSON      = anFrameRow_Person[anSEQ_FrameRow],
        mnSEQ_CAT_PERSON  = mnFrameRowCat_Person[anSEQ_FrameRow, , drop = FALSE],
        mbPERSON_CAT_USE  = lPOP$mbCAT,
        lPERSON_SLOT_HIT  = lPOP$lSLOT,
        lSETTING          = lSETTING,
        sVERBOSE          = sVERBOSE
      )
      # 結果に、試行番号, 列名をつける
      dfTrial <- as_tibble(mnTrial, rownames = "sRowname") %>%
        mutate(
          nTrial = nTrial
        )

      # 画面表示
      if (sVERBOSE %in% c("simple", "detail")){
        cat("[execTrials] nTrial:", nTrial, "; extract:", nrow(dfTrial), ";", (proc.time()-oTime)[3], "sec.\n")
      }
      return(dfTrial)
    }
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[execTrials] end trials.\n")
    }
  }

  # 縦に積む
  dfTrial <- bind_rows(lTrial) %>%
    # execAssign()はこれが割付試行なのか再割付試行なのか知らないので、
    # execAssign()からみた調査参加順SEQと対象者番号nPersonを返している。
    # これは割付試行なので、SEQは調査対象者番号であり、
    # nPersonは母集団メンバー番号である。
    # 以後の混乱を避けるため、SEQをnSubjectに変更する
    mutate(
      nSubject = .data$SEQ,
      SEQ = NULL
    )

  if (sVERBOSE == "detail"){
    cat("[execTrials] Done.\n")
  }
  return(dfTrial)
}
makeRetrial <- function(
    lSURVEY,
    sSAMPLING  = c("with_replace", "without_replace", "fixed"),
    sVERBOSE = c("simple", "detail", "none")
){
  #' Internal: run execAssign() once
  #'
  #' Internal: 再割付試行を1試行実行する。
  #' makeRetrialBlock()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param lSURVEY a `surveydata`-class object.
  #'    survey data. See the document of `makeSurvey` for details.
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
  #' @param sVERBOSE a string.
  #'    画面表示レベル。
  #'
  #' @return a data frame.
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{SEQ}:              再割付における調査参加順(連番)
  #'    \item \code{nSubject}:         対象者番号 (\code{lSURVEY$mbCAT}上の行番号)
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{lSURVEY$nMAXCAT}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nCat}:             割付スロットが属するカテゴリ番号、ないし\code{NA}
  #'    \item \code{nSlot_1}:          割付スロット\code{1}のスロット番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nMAXSLOT): 割付スロット\code{lSURVEY$nMAXSLOT}のスロット番号、ないし\code{NA}
  #'    }
  #
  #  引数チェックは親任せ

  if (sVERBOSE == "detail"){
    cat("[makeRetrial] Hi.\n")
  }

  # anSEQ_Subject: 配信順から対象者番号を引くベクトル
  if (sSAMPLING == "with_replace"){
    if (sVERBOSE == "detail") cat("[makeRetrial] sampling with replacement ...\n")
    anSEQ_Subject <- sample(seq_len(nrow(lSURVEY$mbCAT)), nrow(lSURVEY$mbCAT) * 10, replace = TRUE)
  }
  if (sSAMPLING == "without_replace"){
    if (sVERBOSE == "detail") cat("[makeRetrial] sampling without replacement ...\n")
    anSEQ_Subject <- sample(seq_len(nrow(lSURVEY$mbCAT)), nrow(lSURVEY$mbCAT), replace = FALSE)
  }
  if (sSAMPLING == "fixed"){
    if (sVERBOSE == "detail") cat("[makeRetrial] subjects are fixed ...\n")
    anSEQ_Subject <- seq_len(nrow(lSURVEY$mbCAT))
  }

  # mnSubjectCat_Alt: 対象者番号とカテゴリから、代替する対象者番号を引く表
  mnSubjectCat_Alt <- matrix(
    # 列ごとに処理する
    sapply(
      seq_len(ncol(lSURVEY$manSubjectCat_AltSet)),
      function(nCategory){
        # 要素ごとに処理する
        sapply(
          lSURVEY$manSubjectCat_AltSet[, nCategory],
          function(x){
            if (length(x) == 0){
              out <- NA
            }
            if (length(x) == 1){
              out <- x[[1]]
            }
            if (length(x) > 1){
              out <- x[[ sample(seq_len(length(x)), 1) ]]
            }
            return(out)
          }
        )
      }
    ),
    nrow = nrow(lSURVEY$manSubjectCat_AltSet)
  )

  # mnSEQCat_Alt: 配信順とカテゴリから、(代替する)対象者番号を引く行列
  mnSEQCat_Subject <- mnSubjectCat_Alt[anSEQ_Subject, , drop = FALSE]

  # 割付
  mnAssign <- execAssign (
    anSEQ_PERSON      = anSEQ_Subject,
    mnSEQ_CAT_PERSON  = mnSEQCat_Subject,
    mbPERSON_CAT_USE  = lSURVEY$mbCAT,
    lPERSON_SLOT_HIT  = lSURVEY$lSLOT,
    lSETTING          = lSURVEY$lSETTING,
    sVERBOSE          = sVERBOSE
  )

  out <- data.frame(mnAssign)
    # execAssign()はこれが割付試行なのか再割付試行なのか知らないので、
    # 調査参加順SEQと与えられたデータの行番号nPersonを返している。
    # これは再割付試行なので、SEQは再割付試行における調査参加順に過ぎず、
    # nPersonは実は調査対象者番号である。
    # 以後の混乱を避けるため、nPersonをnSubjectに変更する
  out$nSubject <- out$nPerson
  out$nPerson <- NULL

  if (sVERBOSE == "detail"){
    cat("[makeRetrial] done. \n")
  }

  return(out)
}
makeRetrialBlock <- function(
    lSURVEY,
    nBLOCKSIZE,
    sSAMPLING  = c("with_replace", "without_replace", "fixed"),
    bPARALLEL,
    sLOGFILE,
    sVERBOSE = c("simple", "detail", "none")
){
  #' Internal: run execAssign() repeatedly to compute weight
  #'
  #' Internal: 再割付試行を1ブロック実行する。execRetrials()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param lSURVEY a `surveydata`-class object.
  #'    survey data. See the document of `makeSurvey` for details.
  #' @param nBLOCKSIZE 整数。
  #'    試行ブロック内の試行数
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
  #' @param bPARALLEL a logical
  #'    並列処理するか
  #' @param sLOGFILE a string.
  #'    並列処理する場合のログファイルの名前. NULLだとログを出さない
  #' @param sVERBOSE 文字列。画面表示レベル。
  #'
  #' @return a data frame. 行は再割付試行x調査参加順。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nRetrial}:         再割付試行番号
  #'    \item \code{nSubject}:         対象者番号 (\code{lSURVEY$mbCAT}上の行番号)
  #'    \item \code{SEQ}:              再割付試行における調査参加順(連番)
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nCAT_MAX):  割付カテゴリ\code{lSURVEY$lSETTING$nCAT_MAX}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nCat}:             割付スロットが属するカテゴリ番号、ないし\code{NA}
  #'    \item \code{nSlot_1}:          割付スロット\code{1}のスロット番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nSLOT_MAX): 割付スロット\code{lSURVEY$lSETTING$nSLOT_MAX}のスロット番号、ないし\code{NA}
  #'    }
  #'
  #' @importFrom stats runif
  #' @importFrom foreach foreach
  #' @importFrom foreach "%do%"
  #' @importFrom foreach "%dopar%"
  #' @importFrom parallel detectCores
  #' @importFrom parallel makeCluster
  #' @importFrom parallel stopCluster
  #' @importFrom doParallel registerDoParallel
  #' @importFrom doRNG registerDoRNG
  #' @importFrom doRNG "%dorng%"
  #
  # 引数チェックは親任せ

  if (sVERBOSE == "detail")
    cat("[makeRetrialBlock] Hi, \n")

  nRetrial <- NA

  if (bPARALLEL){
    # 並列処理 - - - - - - - - - - - - - - - - - -
    if (sVERBOSE == "detail")
      cat("[makeRetrialBlock] start clusters ...\n")

    # ログファイルがすでに存在していたら削除
    if (!is.null(sLOGFILE) && file.exists(sLOGFILE)){
      file.remove(sLOGFILE)
    }

    sLOGFILE <- ifelse(is.null(sLOGFILE), "", sLOGFILE)

    # クラスタをつくる
    cl <- makeCluster(
      detectCores() - 1,
      outfile = sLOGFILE
    )
    # この関数から抜けるときにクラスタを止める
    on.exit(stopCluster(cl))

    registerDoParallel(cl)
    registerDoRNG(NULL)

    # 画面表示
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[makeRetrialBlock] start replication (parallel) ...\n")
    }

    lAssign <- foreach(
      nRetrial = seq_len(nBLOCKSIZE),
      .export = c("makeRetrial", "execAssign")
    ) %dorng% {
      # (画面表示をmakeRetrial()のなかに叩き込んでいないのは、
      # makeRetrial()からみると何回目のループかわからないから)

      # 時間測定開始
      oTime <- proc.time()
      # 乱数生成チェック
      gRand <- runif(1)
      # 生成
      out <- makeRetrial (
        lSURVEY               = lSURVEY,
        sSAMPLING             = sSAMPLING,
        sVERBOSE              = sVERBOSE
      )
      out$nRetrial <- nRetrial
      # 画面表示(どうせログにしか出ないので常に)
      cat(
        "[makeRetrialBlock]",
        # as.character(Sys.time()), ";",
        "retrial:", nRetrial, ";",
        "rand:", gRand, ";",
        "extract:", nrow(out), ";",
        (proc.time()-oTime)[3], "sec.\n"
      )

      # 出力
      return(out)
    }
  } else {
    # 逐次処理 - - - - - - - - - - - - - - - - - -
    if (sVERBOSE %in% c("simple", "detail")){
      cat("[makeRetrialBlock] start replication (serial) ...\n")
    }

    lAssign <- foreach(
      nRetrial = seq_len(nBLOCKSIZE)
    ) %do% {
      # ここから並列処理と同じ

      # 時間測定開始
      oTime <- proc.time()
      # 乱数生成チェック
      gRand <- runif(1)

      # 生成
      out <- makeRetrial (
        lSURVEY               = lSURVEY,
        sSAMPLING             = sSAMPLING,
        sVERBOSE              = sVERBOSE
      )
      out$nRetrial <- nRetrial

      # 画面表示
      if (sVERBOSE %in% c("simple", "detail")){
        cat(
          "[makeRetrialBlock]",
          # as.character(Sys.time()), ";",
          "retrial:", nRetrial, ";",
          "rand:", gRand, ";",
          "extract:", nrow(out), ";",
          (proc.time()-oTime)[3], "sec.\n"
        )
      }

      # 出力
      return(out)
    }
  }

  # 縦に積む
  if (sVERBOSE == "detail")
    cat("[makeRetrialBlock] bind ...\n")
  out <- bind_rows(lAssign)

  if (sVERBOSE == "detail")
    cat("[makeRetrialBlock] end\n")
  return(out)
}
makeStat_Cat <- function(
    dfReplicateBlock,
    lSURVEY,
    sVERBOSE = c("simple", "detail", "none")
) {
  #' Interal: make statistics for categories
  #'
  #' Internal: カテゴリ割付頻度を集計する。execRetrials()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param dfReplicateBlock a data frame
  #'   対象者のカテゴリ・ウェイトへの割付を反復実行した結果。
  #'   以下の列を持つと期待される
  #'    \itemize{
  #'    \item \code{nRetrial}:         再割付試行番号
  #'    \item \code{nSubject}:         対象者番号 (\code{lSURVEY$mbCAT}上の行番号)
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{nMAXCAT}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nParentCat}:       割付スロットが属するカテゴリ番号、ないし\code{NA}
  #'    \item \code{nSlot_1}:          割付スロット\code{1}のスロット番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nMAXSLOT): 割付スロット\code{nMAXSLOT}のスロット番号、ないし\code{NA}
  #'    }
  #' @param lSURVEY a `surveydata`-class object.
  #'    survey data. See the document of `makeSurvey` for details.
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return a data frame
  #'    行は対象者x割付可能カテゴリ
  #'    \itemize{
  #'    \item \code{nSubject}               対象者番号
  #'    \item \code{nCat}                   カテゴリ番号
  #'    \item \code{bAssign}                lSURVEY上で割付が起きていたか
  #'    \item \code{nNumRetrial}            その人が1回以上出現した再割付試行数
  #'    \item \code{gSumProp}               その人が1回以上出現した再割付試行におけるカテゴリ割付率の合計
  #'    \item \code{gSumSqProp}             その人が1回以上出現した再割付試行におけるカテゴリ割付率の二乗の合計
  #'    }
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom tidyselect starts_with
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr summarize
  #' @importFrom dplyr select
  #' @importFrom dplyr filter
  #' @importFrom dplyr left_join
  #' @importFrom tidyr replace_na
  #' @importFrom tidyr pivot_longer
  #' @importFrom assertr verify
  #'
  # 引数チェックは親任せ

  if (sVERBOSE == "detail") cat("[makeStat_Cat] start.\n")

  # dfAssignable: lSURVEYにおいて割り付け可能だったか
  # 行は nSubject x nCat (完備)
  # {nSubject, nCat, bAssignable}
  if (sVERBOSE == "detail") cat("[makeStat_Cat] make dfAssignable ...\n")
  mbCat <- lSURVEY$mbCAT
  # mbCatの列名を付け替える。このあとで縦にするときに楽だから
  colnames(mbCat) <- paste0("nCat_", seq_len(ncol(mbCat)))
  dfAssignable <- as_tibble(mbCat) %>%
    mutate(
      nSubject = row_number()
    ) %>%
    pivot_longer(
      cols = starts_with("nCat_"),
      names_to = c("sDummy", "nCat"),
      names_sep = "_",
      values_to = "bAssignable"
    ) %>%
    mutate(nCat = as.integer(.data$nCat)) %>%
    dplyr::select(-.data$sDummy)

  # dfAssign: lSURVEY上で割付が起きていたか
  # 行は nSubject x nCat (割付が生じていたもののみ)
  # {nSubject, nCat, bAssign(=1)}
  if (sVERBOSE == "detail") cat("[makeStat_Cat] make dfAssign ...\n")
  mnAssignCat <- lSURVEY$mnASSIGNCAT
  colnames(mnAssignCat) <- paste0("nCat_", seq_len(ncol(mnAssignCat)))
  dfAssign <- as_tibble(mnAssignCat) %>%
    mutate(
      nSubject = row_number()
    ) %>%
    pivot_longer(
      cols = starts_with("nCat_"),
      names_to = "sDummy",
      values_to = "nCat"
    ) %>%
    dplyr::select(-.data$sDummy) %>%
    # NAを除外
    filter(!is.na(.data$nCat)) %>%
    # 割付が起きていました
    mutate(bAssign = 1)

  # dfBase: 再割付試行 x 人 ごとの出現回数
  # 行は nRetrial x nSubject (出現したもののみ)
  # {nRetrial, nSubject, nBase (出現回数。非復元抽出なので複数回出現しうる)}
  if (sVERBOSE == "detail") cat("[makeStat_Cat] make dfBase ...\n")
  dfBase <- dfReplicateBlock %>%
    group_by(.data$nRetrial, .data$nSubject) %>%
    summarize(nBase = n()) %>%
    ungroup()
  # print(dfBase)
  # stop()

  # dfCount: 各再割付試行における対象者xカテゴリの割り付け回数
  # 行は nRetrial x nSubject x nCat (出現したもののみ)
  # {nRetrial, nSubject, nCat, nCount (割付回数)}
  if (sVERBOSE == "detail") cat("[makeStat_Cat] make dfCount ...\n")
  dfCount <- dfReplicateBlock %>%
    # 割付カテゴリを縦にする
    pivot_longer(
      cols = starts_with("nCat"),
      names_to = "sVar",
      values_to = "nCat"
    ) %>%
    filter(!is.na(.data$nCat)) %>%
    group_by(.data$nRetrial, .data$nSubject, .data$nCat) %>%
    summarize(nCount = n()) %>%
    ungroup()

  ###
  ### 話を簡単にするために、再割付試行 x 調査対象者 x カテゴリ を完備させた表をつくる
  ###

  if (sVERBOSE == "detail") cat("[makeStat_Cat] make dfRetrialSubjectCat ...\n")
  dfRetrialSubjectCat <- expand.grid(
    nRetrial = unique(dfReplicateBlock$nRetrial),
    nSubject = seq_len(nrow(lSURVEY$mbCAT))
  ) %>%
    left_join(dfAssignable, by = c("nSubject")) %>%
    # nBaseをつける
    left_join(dfBase, by = c("nRetrial", "nSubject")) %>%
    replace_na(list(nBase = 0)) %>%
    # nCountをつける
    left_join(dfCount, by = c("nRetrial", "nSubject", "nCat")) %>%
    replace_na(list(nCount = 0)) %>%
    # trap: 割付可能なスロットに対してのみ割付は起きる
    verify(!(.data$bAssignable == 0 & .data$nCount >  0)) %>%
    # 割付可能なスロットに絞る
    dplyr::filter(.data$bAssignable == 1) %>%
    dplyr::select(-.data$bAssignable) %>%
    # 再割付試行 x 調査対象者 x スロットごとの割付率(通常は0か1)
    mutate(gProp = .data$nCount / .data$nBase)
  # print(dfRetrialSubjectCat)
  # stop()

  ### 人xスロット別に集計する
  ### 全く試行に出現しなかった人がいるかもだから、nBase>0でフィルタリングしてはいけない
  if (sVERBOSE == "detail") cat("[makeStat_Cat] output ...\n")
  out <- dfRetrialSubjectCat %>%
    group_by(.data$nSubject, .data$nCat) %>%
    summarize(
      nNumRetrial = sum(.data$nBase > 0),
      gSumProp    = sum(.data$gProp[.data$nBase > 0]),
      gSumSqProp  = sum(.data$gProp[.data$nBase > 0]^2),
    ) %>%
    ungroup() %>%
    # bAssignをつける
    left_join(dfAssign, by = c("nSubject", "nCat")) %>%
    replace_na(list(bAssign = 0))
  # trap: 行数は割付可能カテゴリの延べ数
  stopifnot(nrow(out) == sum(dfAssignable$bAssignable))
  # print(out)
  # stop()

  if (sVERBOSE == "detail") cat("[makeStat_Cat] done.\n")
  return(out)
}
makeStat_Slot <- function(
    dfReplicateBlock,
    lSURVEY,
    sVERBOSE = c("simple", "detail", "none")
) {
  #' Interal: make statistics for slots
  #'
  #' Internal: スロット割付頻度を集計する。execRetrials()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param dfReplicateBlock a data frame
  #'   対象者のカテゴリ・ウェイトへの割付を反復実行した結果。
  #'   以下の列を持つと期待される
  #'    \itemize{
  #'    \item \code{nRetrial}:         再割付試行番号
  #'    \item \code{nSubject}:         対象者番号
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{nMAXCAT}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nParentCat}:       割付スロットが属するカテゴリ番号、ないし\code{NA}
  #'    \item \code{nSlot_1}:          割付スロット\code{1}のスロット番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nMAXSLOT): 割付スロット\code{nMAXSLOT}のスロット番号、ないし\code{NA}
  #'    }
  #' @param lSURVEY a `surveydata`-class object.
  #'    survey data. See the document of `makeSurvey` for details.
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return a data frame
  #'    行は対象者x割付可能スロット。lSURVEY上で割付可能性が判明しているスロットに
  #'    限定されることに注意(つまり、代替対象者のスロットは含まれない)
  #'    列は以下の通り
  #'    \itemize{
  #'    \item \code{nSubject}               対象者番号
  #'    \item \code{nParentCat}             親カテゴリ番号
  #'    \item \code{nSlot}                  スロット番号
  #'    \item \code{bAssign}                lSURVEY上で割付が起きていたか
  #'    \item \code{nNumRetrial}            人xスロットが出現した試行数
  #'    \item \code{gSumProp}               人xスロットが出現した試行における割付率の合計
  #'    \item \code{gSumSqProp}             人xスロットが出現した試行における割付率の二乗の合計
  #'    }
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom tidyselect starts_with
  #' @importFrom dplyr group_by
  #' @importFrom dplyr ungroup
  #' @importFrom dplyr summarize
  #' @importFrom dplyr select
  #' @importFrom dplyr filter
  #' @importFrom dplyr left_join
  #' @importFrom tidyr replace_na
  #' @importFrom tidyr pivot_longer
  #' @importFrom assertr verify
  #'
  # 引数チェックは親任せ

  if (sVERBOSE == "detail") cat("[makeStat_Slot] start.\n")

  # dfAssignable: lSURVEYにおいて割付可能だったか？
  # 行は対象者 x スロット (完備)
  # {nSubject, nParentCat, nSlot, bAssignable(欠損あり)}
  if (sVERBOSE == "detail") cat("[makeStat_Slot] make dfAssignable ...\n")
  lOut <- lapply(
    seq_along(lSURVEY$lSLOT),
    function(nCat){
      mbIn <- lSURVEY$lSLOT[[nCat]]
      # 名前を付け替える。元の名前はここで気にすべきことではない
      colnames(mbIn) <- paste0("Slot_", seq_len(ncol(mbIn)))
      out <- as_tibble(mbIn) %>%
        # nSubject, nParentCatをつけて
        mutate(
          nSubject = row_number(),
          nParentCat = nCat
        ) %>%
        # スロットを縦にする
        pivot_longer(
          cols = starts_with("Slot_"),
          names_to = c("sDummy", "nSlot"),
          names_sep = "_",
          values_to = "bAssignable"
        ) %>%
        dplyr::select(-.data$sDummy) %>%
        mutate(nSlot = as.integer(.data$nSlot))
      return(out)
    }
  )
  dfAssignable <- bind_rows(lOut)
  # print(dfAssignable)
  # stop()

  # dfAssign: lSURVEYでの割付有無
  # 行は対象者 x スロット(割付ありのみ)
  # {nSubject, nParentCat, nSlot, bAssign(=1)}
  if (sVERBOSE == "detail") cat("[makeStat_Slot] make dfAssign ...\n")
  mnAssignSlot <- lSURVEY$mnASSIGNSLOT
  colnames(mnAssignSlot) <- paste0("nSlot_", seq_len(ncol(mnAssignSlot)))
  dfAssign <- as_tibble(mnAssignSlot) %>%
    mutate(
      nSubject = row_number(),
      nParentCat = lSURVEY$anPARENTCAT
    ) %>%
    pivot_longer(
      cols = starts_with("nSlot_"),
      names_to = "sDummy",
      values_to = "nSlot"
    ) %>%
    dplyr::select(-.data$sDummy) %>%
    # NAを除外
    filter(!is.na(.data$nSlot)) %>%
    # 割付が起きていました
    mutate(bAssign = 1)
  # print(dfAssign)
  # stop()

  # dfBase: その試行におけるその人の出現回数 (非復元抽出なので複数回出現しうる)
  # 行は試行 x 対象者 (出現したもののみ)
  if (sVERBOSE == "detail") cat("[makeStat_Slot] make dfBase  ...\n")
  dfBase <- dfReplicateBlock %>%
    group_by(.data$nRetrial, .data$nSubject) %>%
    summarize(nBase = n()) %>%
    ungroup()

  # dfCount: その試行におけるその人のそのスロットの割付回数
  # 行は試行 x 対象者 x スロット (割り付けられたもののみ)
  if (sVERBOSE == "detail") cat("[makeStat_Slot] make dfCount ...\n")
  dfCount <- dfReplicateBlock %>%
    pivot_longer(
      cols = starts_with("nSlot"),
      names_to = "sVar",
      values_to = "nSlot"
    ) %>%
    filter(!is.na(.data$nSlot)) %>%
    group_by(.data$nRetrial, .data$nSubject, .data$nParentCat, .data$nSlot) %>%
    summarize(nCount = n()) %>%
    ungroup()

  ###
  ### 話を簡単にするために、再割付試行 x 調査対象者 x スロット を完備させた表をつくる
  ###

  if (sVERBOSE == "detail") cat("[makeStat_Slot] make dfRetrialSubjectSlot ...\n")
  dfRetrialSubjectSlot <- expand.grid(
    nRetrial = unique(dfReplicateBlock$nRetrial),
    nSubject = seq_len(nrow(lSURVEY$mbCAT))
  ) %>%
    # bAssignableをつける。欠損があることに注意
    # (割り付けされなかったカテゴリ。スロット割付可能性が不明)
    left_join(dfAssignable, by = c("nSubject")) %>%
    # nBaseをつける
    left_join(dfBase, by = c("nRetrial", "nSubject")) %>%
    replace_na(list(nBase = 0)) %>%
    # nCountをつける
    left_join(dfCount, by = c("nRetrial", "nSubject", "nParentCat", "nSlot")) %>%
    replace_na(list(nCount = 0)) %>%
    # trap: 割付不能であることがわかっているスロットに割付は起きない
    verify(!(!is.na(.data$bAssignable) & .data$bAssignable == 0 & .data$nCount >  0)) %>%
    # 割付可能なスロットに絞る
    dplyr::filter(!is.na(.data$bAssignable), .data$bAssignable == 1) %>%
    dplyr::select(-.data$bAssignable) %>%
    # 再割付試行 x 調査対象者 x スロットごとの割付率(通常は0か1)
    mutate(gProp = .data$nCount / .data$nBase)

  ### 人xスロット別に集計する
  ### 全く試行に出現しなかった人がいるかもだから、nBase>0でフィルタリングしてはいけない
  if (sVERBOSE == "detail") cat("[makeStat_Slot] output ...\n")
  out <- dfRetrialSubjectSlot %>%
    group_by(.data$nSubject, .data$nParentCat, .data$nSlot) %>%
    summarize(
      nNumRetrial = sum(.data$nBase > 0),
      gSumProp    = sum(.data$gProp[.data$nBase > 0]),
      gSumSqProp  = sum(.data$gProp[.data$nBase > 0]^2),
    ) %>%
    ungroup() %>%
    # bAssignをつける
    left_join(dfAssign, by = c("nSubject", "nParentCat", "nSlot")) %>%
    replace_na(list(bAssign = 0))
  # trap: 行数は割付可能スロットの延べ数
  # print(nrow(out))
  # print(sum(dfAssignable$bAssignable[!is.na(dfAssignable$bAssignable)]))
  stopifnot(nrow(out) == sum(dfAssignable$bAssignable[!is.na(dfAssignable$bAssignable)]))
  # print(out)
  # stop()

  if (sVERBOSE == "detail") cat("[makeStat_Slot] end.\n")
  return(out)
}
execRetrials <- function(
    lSURVEY,
    sSAMPLING  = c("with_replace", "without_replace", "fixed"),
    nBLOCKSIZE,
    nNUMBLOCK,
    bPARALLEL,
    sLOGFILE,
    sVERBOSE = c("simple", "detail", "none")
){
  #' Internal: run simulations of retrials
  #'
  #' Internal: 再割付シミュレーションを行う.
  #' simWeight(), simBias()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param lSURVEY a `surveydata`-class object.
  #'    調査データ。\code{\link{makeSurvey}}で生成する。
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
  #' @param nNUMBLOCK an integer.
  #'    実行する再割付試行のブロック数。
  #' @param nBLOCKSIZE an integer.
  #'    ブロック内の再割付試行数。
  #' @param bPARALLEL a logical.
  #'    並列処理するか。
  #' @param sLOGFILE a string.
  #'    並列処理する場合のログファイル(フルパス)。NULLだとログを出さない。
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @return
  #'    データフレームのリスト。要素は次の通り:
  #'    \itemize{
  #'    \item \code{dfStat_Cat}: カテゴリ割付頻度
  #'    \item \code{dfStat_Slot}: スロット割付頻度
  #'    }
  #'
  #'    カテゴリ割付頻度: 行は調査対象者x割付可能カテゴリを表す。
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nBlock}:            ブロック番号
  #'    \item \code{nBlockSize}:        ブロックサイズ(ブロック内の再割付試行数)
  #'    \item \code{nSubject}:          調査対象者番号
  #'    \item \code{nCat}:              カテゴリ番号
  #'    \item \code{bAssign}            割付試行で割付が起きていたか
  #'    \item \code{nNumRetrial}        その人が1回以上出現した再割付試行数
  #'    \item \code{gSumProp}           その人が1回以上出現した再割付試行におけるカテゴリ割付率の合計
  #'    \item \code{gSumSqProp}         その人が1回以上出現した再割付試行におけるカテゴリ割付率の二乗の合計
  #'    }
  #'
  #'    スロット割付頻度: 行は調査対象者x割付可能スロットを表す。
  #'    割付試行において割付可能性が判明しているスロットに
  #'    限定されることに注意(つまり、代替対象者のスロットは含まれない)
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{nBlock}:             ブロック番号
  #'    \item \code{nBlockSize}:         ブロックサイズ(ブロック内の再割付試行数)
  #'    \item \code{nSubject}:           調査対象者番号
  #'    \item \code{nParentCat}:         スロットが属するカテゴリ番号
  #'    \item \code{nSlot}:              スロット番号
  #'    \item \code{bAssign}             割付試行で割付が起きていたか
  #'    \item \code{nNumRetrial}         人xスロットが出現した試行数
  #'    \item \code{gSumProp}            人xスロットが出現した試行における割付率の合計
  #'    \item \code{gSumSqProp}          人xスロットが出現した試行における割付率の二乗の合計
  #'    }

  ## あいさつのためsVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[execRetrials] start.\n")
  }

  # 引数チェック - - - - -
  ## lSURVEY
  ## クラスは期待通り
  stopifnot("surveydata" %in% class(lSURVEY))

  ## sSAMPLING
  sSAMPLING <- match.arg(sSAMPLING)

  ## nNUMBLOCK
  ## 未チェック

  ## nBLOCKSIZE
  ## 未チェック

  ## bPARALLEL
  ## 値は期待通り
  stopifnot(bPARALLEL %in% c(TRUE, FALSE))

  ## sLOGFILE
  ## 指定されていたら、ファイルは作成できるべき
  if (!is.null(sLOGFILE)){
    file.create(sLOGFILE)
    file.remove(sLOGFILE)
  }

  # ここからメイン - - - - -

  # ブロックを反復する
  lOut <- lapply(
    seq_len(nNUMBLOCK),
    function(nBlock){
      # 時間測定開始
      oTime <- proc.time()
      # 画面表示
      # makeRetrialBlockをコール
      dfReplicateBlock <- makeRetrialBlock(
        lSURVEY     = lSURVEY,
        sSAMPLING   = sSAMPLING,
        nBLOCKSIZE  = nBLOCKSIZE,
        bPARALLEL   = bPARALLEL,
        sLOGFILE    = sLOGFILE,
        sVERBOSE    = sVERBOSE
      )
      ## print(dfReplicateBlock %>% dplyr::filter(nSubject == 1))

      # 集計
      dfStat_Cat <- makeStat_Cat(
        dfReplicateBlock,
        lSURVEY,
        sVERBOSE = sVERBOSE
      ) %>%
        mutate(
          nBlock = nBlock,
          # ブロック内の反復数をつける
          nBlockSize = nBLOCKSIZE
        )

      dfStat_Slot <- makeStat_Slot(
        dfReplicateBlock,
        lSURVEY,
        sVERBOSE = sVERBOSE
      ) %>%
        mutate(
          nBlock = nBlock,
          # ブロック内の反復数をつける
          nBlockSize = nBLOCKSIZE
        )
      ## print(dfStat_Slot %>% dplyr::filter(nSubject == 1))
      ## stop()

      # 画面表示
      if (sVERBOSE %in% c("simple", "detail")){
        cat("[execRetrials] block", nBlock, ": ", (proc.time() - oTime)[3], "sec. \n")
      }
      lOut <- list(dfStat_Cat = dfStat_Cat, dfStat_Slot = dfStat_Slot)
      return(lOut)
    }
  )
  # 縦に積む
  lOut <- list(
    dfStat_Cat = bind_rows(lapply(lOut, function(x) x$dfStat_Cat)),
    dfStat_Slot = bind_rows(lapply(lOut, function(x) x$dfStat_Slot))
  )

  if (sVERBOSE == "detail"){
    cat("[execRetrials] end.\n")
  }
  return(lOut)
}
