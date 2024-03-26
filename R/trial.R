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
    # 調査参加順SEQと対象者番号nPersonを返している。
    # これは割付試行なので、調査参加順SEQは調査対象者番号であり、
    # 対象者番号nPersonは母集団メンバー番号である。
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
    bREDRAW,
    nMAXSIZE,
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
  #' @param bREDRAW an boolean.
  #'    復元抽出するか。FALSEにすると、lSURVEY上での対象者順を固定する。
  #' @param nMAXSIZE an integer.
  #'    復元抽出する対象者の最大数。bREDRAW=FALSEの場合は無視される
  #' @param sVERBOSE 文字列。画面表示レベル。
  #'
  #' @return a data frame.
  #'    列は以下のとおり(順不同):
  #'    \itemize{
  #'    \item \code{SEQ}:              調査参加順(連番)
  #'    \item \code{nPerson}:          対象者番号 (\code{lSURVEY$mbCAT}上の行番号)
  #'                                   母集団メンバーの番号ではないことに注意
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
  if (bREDRAW){
    if (sVERBOSE == "detail")
      cat("[makeRetrial] redrawing subjects ...\n")
    anSEQ_Subject <- sample(seq_len(nrow(lSURVEY$mbCAT)), nMAXSIZE, replace = T)
  } else{
    if (sVERBOSE == "detail")
      cat("[makeRetrial] subjects are fixed.\n")
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
  if (sVERBOSE == "detail"){
    cat("[makeRetrial] done. \n")
  }

  return(out)
}
makeRetrialBlock <- function(
    lSURVEY,
    nBLOCKSIZE,
    bREDRAW,
    nMAXSIZE,
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
  #' @param bREDRAW an boolean.
  #'    復元抽出するか。FALSEにすると、lSURVEY上での対象者順を固定する。
  #' @param nMAXSIZE an integer.
  #'    復元抽出する対象者の最大数。bREDRAW=FALSEの場合は無視される
  #' @param bPARALLEL a logical
  #'    並列処理するか
  #' @param sLOGFILE a string.
  #'    並列処理する場合のログファイルの名前. NULLだとログを出さない
  #' @param sVERBOSE 文字列。画面表示レベル。
  #'
  #' @return a data frame.
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
        bREDRAW               = bREDRAW,
        nMAXSIZE              = nMAXSIZE,
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
        bREDRAW               = bREDRAW,
        nMAXSIZE              = nMAXSIZE,
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
  # execAssign()はこれが割付試行なのか再割付試行なのか知らないので、
  # 調査参加順SEQと与えられたデータの行番号nPersonを返している。
  # これは再割付試行なので、調査参加順は再割付試行の調査参加順に過ぎず、
  # 行番号は割付試行の調査対象者番号である。
  # 以後の混乱を避けるため、nPersonをnSubjectに変更する
  out$nSubject <- out$nPerson
  out$nPerson <- NULL

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
  #'    \item \code{nSubject}:         対象者番号
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{nMAXCAT}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nCat}:             割付スロットが属するカテゴリ番号、ないし\code{NA}
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
  #'    \item \code{nSubject}          対象者番号
  #'    \item \code{nCat}              カテゴリ番号
  #'    \item \code{bAssign}           lSURVEY上で割付が起きていたか
  #'    \item \code{nCount_SubjectCat} 再試行においてその人にそのカテゴリが割り付けられた回数
  #'    \item \code{nCount_Subject}    再試行におけるその人の出現回数
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

  if (sVERBOSE == "detail"){
    cat("[makeStat_Cat] start.\n")
  }

  # dfStat1: ある対象者が出てきた回数
  if (sVERBOSE == "detail"){
    cat("[makeStat_Cat] make dfStat1 ...\n")
  }
  dfStat1 <- dfReplicateBlock %>%
    group_by(.data$nSubject) %>%
    summarize(nCount_Subject = n()) %>%
    ungroup()

  # dfStat2: ある対象者のあるカテゴリに割付が起きた回数
  if (sVERBOSE == "detail"){
    cat("[makeStat_Cat] make dfStat2 ...\n")
  }
  dfStat2 <- dfReplicateBlock %>%
    dplyr::select(-.data$nCat) %>%
    pivot_longer(
      cols = starts_with("nCat"),
      names_to = "sVar",
      values_to = "nCat"
    ) %>%
    filter(!is.na(.data$nCat)) %>%
    group_by(.data$nSubject, .data$nCat) %>%
    summarize(nCount_SubjectCat = n()) %>%
    ungroup()
  # print(dfStat2 %>% dplyr::filter(nSubject == 1))

  # dfAssign: lSURVEY上で割付が起きていたか
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
  # print(dfAssign %>% dplyr::filter(nSubject == 1))

  # dfAssignable: lSURVEYにおいて割り付け可能だったか
  mbCat <- lSURVEY$mbCAT
  # mbCatの名前を付け替える。ここで気にすべきことではない
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
  # print(dfAssignable %>% dplyr::filter(nSubject == 1))
  # stop()

  # 出力
  if (sVERBOSE == "detail"){
    cat("[makeStat_Cat] output ...\n")
  }

  out <- dfAssignable %>%
    # dfAssignをくっつける
    left_join(dfAssign, by = c("nSubject", "nCat")) %>%
    replace_na(list(bAssign = 0)) %>%
    # dfStat2をくっつける.
    left_join(dfStat2, by = c("nSubject", "nCat")) %>%
    replace_na(list(nCount_SubjectCat = 0)) %>%
    # dfStat1をくっつける
    left_join(dfStat1, by = c("nSubject")) %>%
    replace_na(list(nCount_Subject = 0)) %>%
    # trap: まさか割り付け不能なカテゴリに割付は起きていないよね
    verify(! (.data$bAssignable == 0 & .data$nCount_SubjectCat > 0)) %>%
    # 割付可能カテゴリに絞る
    dplyr::filter(.data$bAssignable == 1) %>%
    dplyr::select(-.data$bAssignable)

  if (sVERBOSE == "detail"){
    cat("[makeStat_Cat] done.\n")
  }
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
  #'    \item \code{nSubject}:         対象者番号
  #'    \item \code{nCat_1}:           割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nMAXCAT):   割付カテゴリ\code{nMAXCAT}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nCat}:             割付スロットが属するカテゴリ番号、ないし\code{NA}
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
  #'    \item \code{nSubject}           対象者番号
  #'    \item \code{nCat}               親カテゴリ番号
  #'    \item \code{nSlot}              スロット番号
  #'    \item \code{bAssign}            lSURVEY上でそのスロットが割り付けられていたか
  #'    \item \code{nCount_SubjectSlot} その人にそのスロットが割り付けられた回数
  #'    \item \code{nCount_Subject}     その人の出現回数
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

  if (sVERBOSE == "detail"){
    cat("[makeStat_Slot] start.\n")
  }

  # dfStat1: ある対象者が出てきた回数
  # {nSubject, nCount_Subject}
  dfStat1 <- dfReplicateBlock %>%
    group_by(.data$nSubject) %>%
    summarize(nCount_Subject = n()) %>%
    ungroup()
  # print(dfStat1)
  # stop()

  # dfStat2: ある対象者のあるスロットに割付が起きた回数
  # {nSubject, nCat, nSlot, nCount_SubjectSlot}
  dfStat2 <- dfReplicateBlock %>%
    pivot_longer(
      cols = starts_with("nSlot"),
      names_to = "sVar",
      values_to = "nSlot"
    ) %>%
    filter(!is.na(.data$nSlot)) %>%
    group_by(.data$nSubject, .data$nCat, .data$nSlot) %>%
    summarize(nCount_SubjectSlot = n()) %>%
    ungroup()
  # print(dfStat2)
  # stop()

  # dfAssign: lSURVEYでの割付有無
  # {nSubject, nCat, nSlot, bAssign}
  mnAssignSlot <- lSURVEY$mnASSIGNSLOT
  colnames(mnAssignSlot) <- paste0("nSlot_", seq_len(ncol(mnAssignSlot)))
  dfAssign <- as_tibble(mnAssignSlot) %>%
    mutate(
      nSubject = row_number(),
      nCat = lSURVEY$anPARENTCAT
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

  # dfAssignable: lSURVEYにおいて割り付け可能だったか
  lOut <- lapply(
    seq_along(lSURVEY$lSLOT),
    function(nCat){
      mbIn <- lSURVEY$lSLOT[[nCat]]
      # 名前を付け替える。元の名前はここで気にすべきことではない
      colnames(mbIn) <- paste0("Slot_", seq_len(ncol(mbIn)))
      out <- as_tibble(mbIn) %>%
        # nSubject, nCatをつけて
        mutate(
          nSubject = row_number(),
          nCat = nCat
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

  # 出力
  out <- dfAssignable %>%
    dplyr::filter(.data$bAssignable == 1) %>%
    dplyr::select(-.data$bAssignable) %>%
    left_join(dfAssign, by = c("nSubject", "nCat", "nSlot")) %>%
    replace_na(list(bAssign = 0)) %>%
    left_join(dfStat2, by = c("nSubject", "nCat", "nSlot")) %>%
    replace_na(list(nCount_SubjectSlot  = 0)) %>%
    left_join(dfStat1, by = c("nSubject")) %>%
    replace_na(list(nCount_Subject = 0))

  if (sVERBOSE == "detail"){
    cat("[makeStat_Slot] end.\n")
  }
  return(out)
}
execRetrials <- function(
    lSURVEY,
    bREDRAW,
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
  #' @param bREDRAW an boolean.
  #'    復元抽出するか。FALSEにすると、lSURVEY上での対象者順を固定する。
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
  #'    \item \code{nCount_Subject}:    ブロック内の再割付試行で調査対象者が出現した回数
  #'    \item \code{nCount_SubjectCat}: ブロック内の再割付試行で調査対象者が出現しカテゴリに割り付けられた回数
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
  #'    \item \code{nCat}:               スロットが属するカテゴリ番号
  #'    \item \code{nSlot}:              スロット番号
  #'    \item \code{bAssign}             割付試行で割付が起きていたか
  #'    \item \code{nCount_Subject}:     ブロック内の再割付試行で調査対象者が出現した回数
  #'    \item \code{nCount_SubjectSlot}: ブロック内の再割付試行で調査対象者が出現しスロットに割り付けられた回数
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
  # nFrameSize: 実際に設定する対象者数上限。
  # nMAXSIZEが指定されていない場合は、対象者数の10倍とする
  if (is.null(lSURVEY$nMAXSIZE)){
    nFrameSize <- nrow(lSURVEY$mbCAT) * 10
  } else {
    nFrameSize <- lSURVEY$nMAXSIZE
  }

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
        bREDRAW     = bREDRAW,
        nBLOCKSIZE  = nBLOCKSIZE,
        nMAXSIZE    = nFrameSize,
        bPARALLEL   = bPARALLEL,
        sLOGFILE    = sLOGFILE,
        sVERBOSE    = sVERBOSE
      )
      ## print(dfReplicateBlock %>% dplyr::filter(nSubject == 1))

      # 集計
      dfStat_Cat <- makeStat_Cat(
        dfReplicateBlock,
        lSURVEY,
        sVERBOSE == sVERBOSE
      ) %>%
        mutate(
          nBlock = nBlock,
          # ブロック内の反復数をつける
          nBlockSize = nBLOCKSIZE
        )
      ## print(dfStat_Cat %>% dplyr::filter(nSubject == 1))
      ## stop()

      dfStat_Slot <- makeStat_Slot(
        dfReplicateBlock,
        lSURVEY,
        sVERBOSE == sVERBOSE
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
