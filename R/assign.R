### assign.R
###   割付のための関数
###   size(標本サイズ予測), weight(ウェイト算出)の両方からコールされる
###
sub_getcat <- function(
  abCatNo_Use,
  abCatNo_Open,
  anCatNo_OpenCount,
  anCatNo_AllCount,
  nCAT_MAX,
  sCAT_TYPE,
  sCAT_FILTER,
  sCAT_ORDER,
  sCAT_EXCLUDE,
  bDEBUG
){
  #' Internal: Assign a participant into categories
  #'
  #' Internal. ある対象者について、割付カテゴリを決定する。
  #'          execAssign()からコールされる
  #'
  #' @keywords internal
  #'
  #' @param abCatNo_Use a binary vector.
  #'    カテゴリの番号から割付可能かどうかを引くベクトル。
  #' @param abCatNo_Open a binary vector.
  #'    カテゴリの番号からオープンかどうかを引くベクトル。
  #' @param anCatNo_OpenCount an integer vector.
  #'    カテゴリの番号からオープンスロット数を引くベクトル。
  #' @param anCatNo_AllCount an integer vector.
  #'    カテゴリの番号からスロット数を引くベクトル。
  #' @param nCAT_MAX 整数。
  #'    ある対象者に割り付けるカテゴリ数の上限。
  #' @param sCAT_TYPE 文字列。
  #'    カテゴリ割付タイプ。
  #' @param sCAT_FILTER 文字列。
  #'    カテゴリ割付の際の絞り込み条件。
  #' @param sCAT_ORDER 文字列。
  #'    カテゴリ割付の際の順序付け条件。
  #' @param sCAT_EXCLUDE 文字列。
  #'    カテゴリ割付の際の除外条件。
  #' @param DEBUG as logical.
  #'    デバッグモード
  #'
  #' @return an integer vector.
  #'    割付カテゴリの番号。長さはnCAT_MAX, 足りない分はNAを詰める
  #'
  #' @importFrom stats runif
  #'
  # notes:
  #   getCandidate()はコールしない。速度を優先したいから

  ## 引数チェック - - - - - - - - - -
  if (bDEBUG){
    ## abCatNo_Use
    ## 欠損無し, 0か1
    stopifnot(!is.na(abCatNo_Use))
    stopifnot(abCatNo_Use %in% 0:1)

    ## abCatNo_Open
    ## 長さはabCatNoUseと同じ、欠損無し、0か1
    stopifnot(length(abCatNo_Open) == length(abCatNo_Use))
    stopifnot(!is.na(abCatNo_Open))
    stopifnot(abCatNo_Open %in% 0:1)

    ## anCatNo_OpenCount
    ## 長さはabCatNoUseと同じ、欠損無し
    stopifnot(length(anCatNo_OpenCount) == length(abCatNo_Use))
    stopifnot(!is.na(anCatNo_OpenCount))

    ## anCatNo_AllCount
    ## 長さはabCatNoUseと同じ、欠損無し、0以上
    stopifnot(length(anCatNo_AllCount) == length(abCatNo_Use))
    stopifnot(!is.na(anCatNo_AllCount))
    stopifnot(anCatNo_AllCount > 0)

    ## nCAT_MAX
    ## 欠損無し
    stopifnot(!is.na(nCAT_MAX))

    ## sCAT_TYPE, sCAT_FILTER, sCAT_ORDER, sCAT_EXCLUDE
    stopifnot(sCAT_TYPE    %in% c("adaptive", "nonadaptive"))
    stopifnot(sCAT_FILTER  %in% c("all", "open"))
    stopifnot(sCAT_ORDER   %in% c("random", "openclosed", "shortnum", "shortratio"))
    stopifnot(sCAT_EXCLUDE %in% c("none", "allclosed"))
  }

  ## ここからメイン - - - - - - - - -
  if (bDEBUG) cat("[sub_getcat] start.\n")

  # 割付可能カテゴリがあるかどうかで分岐する。時間を節約するため
  if (all(abCatNo_Use == 0)){
    # 割付可能カテゴリがない場合
    out <- rep(NA, nCAT_MAX)

  } else {
    # 割付可能カテゴリがある場合

    # 候補の決定
    if (sCAT_TYPE == "adaptive"){
      # 適応的な場合
      anCandCat <- switch(
        sCAT_FILTER,
        "all"  = seq_along(abCatNo_Use)[abCatNo_Use == 1],
        "open" = seq_along(abCatNo_Use)[abCatNo_Use * abCatNo_Open == 1],
        stop()
      )
    } else {
      # 非適応的な場合
      anCandCat <- seq_along(abCatNo_Use)
    }

    # 候補が複数あったら
    if (length(anCandCat) >= 2){
      # 並び変えて
      anCandCat <- switch(
        sCAT_ORDER,
        "random" = sample(anCandCat, length(anCandCat), replace = FALSE),
        "openclosed" = {
          # anSortkey1 <- -1 * abCatNo_Open[anCandCat]
          # anSortkey2 <- as.integer(runif(length(anCandCat), min=0, max=99))
          # anCandCat[order(anSortkey1, anSortkey2)]
          anSortkey <- -1 * abCatNo_Open[anCandCat] * 100 + runif(length(anCandCat), min=0, max=99)
          anSortkey <- as.integer(anSortkey)
          anCandCat[order(anSortkey)]
        },
        "shortnum" = {
          # anSortkey1 <- -1 * anCatNo_OpenCount[anCandCat]
          # anSortkey2 <- as.integer(runif(length(anCandCat), min=0, max=99))
          # anCandCat[order(anSortkey1, anSortkey2)]
          anSortkey <- -1 * anCatNo_OpenCount[anCandCat] * 100 + runif(length(anCandCat), min=0, max=99)
          anSortkey <- as.integer(anSortkey)
          anCandCat[order(anSortkey)]
        },
        "shortratio" = {
          anSortkey1 <- -1 * anCatNo_OpenCount[anCandCat]/anCatNo_AllCount[anCandCat]
          anSortkey2 <- runif(length(anCandCat), min=0, max=99)
          anCandCat[order(anSortkey1, anSortkey2)]
        },
        stop()
      )
      # nCAT_MAX個までを選ぶ
      anCandCat <- anCandCat[seq_len(nCAT_MAX)]
      anCandCat <- anCandCat[!is.na(anCandCat)]
    }

    # 割付可能カテゴリに絞る
    anCandCat <- anCandCat[ abCatNo_Use[anCandCat] == 1 ]

    # 除外条件を適用
    if (sCAT_EXCLUDE == "allclosed"){
      if (sum(abCatNo_Open[anCandCat]) == 0){
        anCandCat = c()
      }
    }

    # 後ろにNAを埋める
    out <- c(anCandCat, rep(NA, nCAT_MAX))[seq_len(nCAT_MAX)]
  }

  if (bDEBUG) cat("[sub_getcat] end.\n")
  return(out)
}
sub_checkcat <- function(
  abCatNo_Use,
  abCatNo_Open,
  anCatNo_OpenCount,
  anCatNo_AllCount,
  nCAT_MAX,
  sCAT_TYPE,
  sCAT_FILTER,
  sCAT_ORDER,
  sCAT_EXCLUDE,
  bDEBUG
){
  #' Internal: Check assigned categories
  #'
  #' Internal. ある対象者について、ありうる割付カテゴリの組み合わせを
  #'           すべて返す。
  #'           checkSurvey()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param abCatNo_Use a binary vector.
  #'    カテゴリの番号から割付可能かどうかを引くベクトル。
  #'
  #' @param abCatNo_Open a binary vector.
  #'    カテゴリの番号からオープンかどうかを引くベクトル。
  #' @param anCatNo_OpenCount an integer vector.
  #'    カテゴリの番号からオープンスロット数を引くベクトル。
  #' @param anCatNo_AllCount an integer vector.
  #'    カテゴリの番号からスロット数を引くベクトル。
  #' @param nCAT_MAX 整数。
  #'    ある対象者に割り付けるカテゴリ数の上限。
  #' @param sCAT_TYPE 文字列。
  #'    カテゴリ割付タイプ。
  #' @param sCAT_FILTER 文字列。
  #'    カテゴリ割付の際の絞り込み条件。
  #' @param sCAT_ORDER 文字列。
  #'    カテゴリ割付の際の順序付け条件。
  #' @param sCAT_EXCLUDE 文字列。
  #'    カテゴリ割付の際の除外条件。
  #' @param DEBUG as logical.
  #'    デバッグモード
  #'
  #' @return a list of integer vectors.
  #'    要素は割付カテゴリのある組み合わせ。ソート済み。

  ## 引数チェック - - - - - - - - - -
  if (bDEBUG){
    ## abCatNo_Use
    ## 欠損無し
    stopifnot(!is.na(abCatNo_Use))
    ## 値は0か1
    stopifnot(abCatNo_Use %in% 0:1)
    ## abCatNo_Open
    ## 長さはabCatNoUseと同じ
    stopifnot(length(abCatNo_Open) == length(abCatNo_Use))
    ## 欠損無し
    stopifnot(!is.na(abCatNo_Open))
    ## 値は0か1
    stopifnot(abCatNo_Open %in% 0:1)

    ## anCatNo_OpenCount
    ## 長さはabCatNoUseと同じ
    stopifnot(length(anCatNo_OpenCount) == length(abCatNo_Use))
    ## 欠損無し
    stopifnot(!is.na(anCatNo_OpenCount))

    ## anCatNo_AllCount
    ## 長さはabCatNoUseと同じ
    stopifnot(length(anCatNo_AllCount) == length(abCatNo_Use))
    ## 欠損無し
    stopifnot(!is.na(anCatNo_AllCount))
    ## 値は0より大きい
    stopifnot(anCatNo_AllCount > 0)

    ## nCAT_MAX
    ## 欠損無し
    stopifnot(!is.na(nCAT_MAX))

    ## sCAT_TYPE, sCAT_FILTER, sCAT_ORDER, sCAT_EXCLUDE
    stopifnot(sCAT_TYPE    %in% c("adaptive", "nonadaptive"))
    stopifnot(sCAT_FILTER  %in% c("all", "open"))
    stopifnot(sCAT_ORDER   %in% c("random", "openclosed", "shortnum", "shortratio"))
    stopifnot(sCAT_EXCLUDE %in% c("none", "allclosed"))
  }

  ## ここからメイン - - - - - - - - -

  # 候補の決定
  # 候補の決定
  if (sCAT_TYPE == "adaptive"){
    anCandCat <- switch(
      sCAT_FILTER,
      "all"  = seq_along(abCatNo_Use)[abCatNo_Use == 1],
      "open" = seq_along(abCatNo_Use)[abCatNo_Use * abCatNo_Open == 1],
      stop()
    )
  } else {
    # 非適応的な場合
    anCandCat <- seq_along(abCatNo_Use)
  }

  # ソートキーの決定
  anSortkey <- switch(
    sCAT_ORDER,
    "random"     = rep(1, length(anCandCat)),
    "openclosed" = abCatNo_Open[anCandCat],
    "shortnum"   = -1 * anCatNo_OpenCount[anCandCat],
    "shortratio" = -1 * anCatNo_OpenCount[anCandCat] / anCatNo_AllCount[anCandCat],
    stop()
  )

  lOut <- getCandidate(anCandCat, anSortkey, nCAT_MAX)

  # nonadaptive の場合、割付不能カテゴリが含まれているので取り除く
  if (sCAT_TYPE == "nonadaptive"){
    lOut <- unique( lapply( lOut, function(anIn) anIn[ abCatNo_Use[anIn] == 1 ] ) )
  }

  # 除外条件を適用
  if (sCAT_EXCLUDE == "allclosed"){
    lOut <- lOut[ sapply(lOut, function(x) sum(abCatNo_Open[x])) > 0 ]
  }

  return(lOut)
}
sub_getslot <- function(
  abSlotNo_Hit,
  abSlotNo_Open,
  anSlotNo_Count,
  anSlotNo_Request,
  nSLOT_MAX,
  sSLOT_TYPE,
  sSLOT_FILTER,
  sSLOT_ORDER,
  sSLOT_EXCLUDE,
  bDEBUG
){
  #' Internal: Assign a participant into slots
  #'
  #' Internal. ある対象者について、あるカテゴリの割付スロットを決定する。
  #'           execAssign()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param abSlotNo_Hit an binary vector.
  #'    スロット番号から割付可能かどうかを引くベクトル
  #' @param abSlotNo_Open an binary vector.
  #'    スロット番号からオープンかどうかを引くベクトル
  #' @param anSlotNo_Count an integer vector.
  #'    スロット番号から獲得票数を引くベクトル
  #' @param anSlotNo_Request an integer vector.
  #'    スロット番号から目標票数を引くベクトル
  #' @param nSLOT_MAX 整数。
  #'    ある対象者に割り付けるスロット数の上限。
  #' @param sSLOT_TYPE 文字列。
  #'    スロット割付タイプ。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_FILTER 文字列。
  #'    スロット割付の際の絞り込み条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_ORDER 文字列。
  #'    スロット割付の際の順序付け条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_EXCLUDE 文字列。
  #'    スロット割付の際の除外条件。指定は必須。詳細はvignetteを参照。
  #' @param DEBUG as logical.
  #'    デバッグモード
  #'
  #' @return an integer vector.
  #'    割付スロットの番号。長さはnSLOT_MAX, 足りない分はNAを詰める
  #'
  #' @importFrom stats runif
  #
  # notes:
  #   getCandidate()はコールしない。速度を優先したいから

  ## 引数チェック - - - - - - - - - -
  if (bDEBUG){
    ## abSlotNo_Hit
    ## 欠損無し, 0か1
    stopifnot(!is.na(abSlotNo_Hit))
    stopifnot(abSlotNo_Hit %in% 0:1)

    ## abSlotNo_Open
    ## 長さはabSlotNo_Hitと同じ、欠損無し、0か1
    stopifnot(length(abSlotNo_Open) == length(abSlotNo_Hit))
    stopifnot(!is.na(abSlotNo_Open))
    stopifnot(abSlotNo_Open %in% 0:1)

    ## anSlotNo_Count
    ## 長さはabSlotNo_Hitと同じ、欠損無し
    stopifnot(length(anSlotNo_Count) == length(abSlotNo_Hit))
    stopifnot(!is.na(anSlotNo_Count))

    ## anSlotNo_Request
    ## 長さはabSlotNo_Hitと同じ、欠損無し
    stopifnot(length(anSlotNo_Request) == length(abSlotNo_Hit))
    stopifnot(!is.na(anSlotNo_Request))

    ## nSLOT_MAX
    ## 欠損無し
    stopifnot(!is.na(nSLOT_MAX))

    ## sSLOT_TYPE, sSLOT_FILTER, sSLOT_ORDER, sSLOT_EXCLUDE
    stopifnot(sSLOT_TYPE    %in% c("adaptive", "nonadaptive"))
    stopifnot(sSLOT_FILTER  %in% c("all", "open"))
    stopifnot(sSLOT_ORDER   %in% c("random", "openclosed", "shortnum", "shortratio"))
    stopifnot(sSLOT_EXCLUDE %in% c("none", "allclosed"))
  }

  ## ここからメイン - - - - - - - - -

  if (bDEBUG) cat("[sub_getslot] start.\n")

  # 割付可能スロットとオープンスロットを表示
  if (bDEBUG) cat("[sub_getslot] Hit slot:",  paste0(seq_along(abSlotNo_Hit)[abSlotNo_Hit == 1], collapse=","), "\n")
  if (bDEBUG) cat("[sub_getslot] Open slot:",  paste0(seq_along(abSlotNo_Open)[abSlotNo_Open == 1], collapse=","), "\n")

  # 割付可能スロットがあるかどうかで分岐する。時間を節約するため
  if (all(abSlotNo_Hit == 0)){
    # 割付可能スロットがない場合
    out <- rep(NA, nSLOT_MAX)

  } else {
    # 割付可能スロットがある場合

    # 候補を決める
    if (sSLOT_TYPE == "adaptive"){
      # 適応的な場合
      anCandSlot <- switch(
        sSLOT_FILTER,
        "all"  = seq_along(abSlotNo_Hit)[abSlotNo_Hit == 1],
        "open" = seq_along(abSlotNo_Hit)[abSlotNo_Hit * abSlotNo_Open == 1],
        stop()
      )
    } else {
      # 非適応的な場合
      anCandSlot <- seq_along(abSlotNo_Hit)
    }
    if (bDEBUG) cat("[sub_getslot] -> anCandSlot (ordered):", paste0(anCandSlot, collapse=","), "\n")

    # 候補が複数あったら
    if (length(anCandSlot) >= 2){
      # 並び変えて
      anCandSlot <- switch(
        sSLOT_ORDER,
        "random" = sample(anCandSlot, length(anCandSlot), replace = FALSE),
        "openclosed" = {

          # shortnumでやった実験の結果に従い、orderの引数をひとつにまとめる
          # anSortkey1 <- -1 * abSlotNo_Open[anCandSlot]
          # anSortkey2 <- as.integer(runif(length(anCandSlot), min=0, max=99))
          # anCandSlot[order(anSortkey1, anSortkey2)]

          anSortkey <- -1 * abSlotNo_Open[anCandSlot] * 100 + runif(length(anCandSlot), min=0, max=99)
          anSortkey <- as.integer(anSortkey)
          anCandSlot[order(anSortkey)]
        },
        "shortnum" = {
          anShortNum <- anSlotNo_Request[anCandSlot] - anSlotNo_Count[anCandSlot]

          # 実験したところ少し遅い模様。orderの引数はひとつのほうがいいのかな?
          # anSortkey1 <- -1 * anShortNum
          # anSortkey2 <- as.integer(runif(length(anCandSlot), min=0, max=99))
          # anCandSlot[order(anSortkey1,  anSortkey2)]

          anSortkey <- -1 * anShortNum * 100 + runif(length(anCandSlot), min=0, max=99)
          anSortkey <- as.integer(anSortkey) # 整数にしたほうが少し速い模様
          anCandSlot[order(anSortkey)]
        },
        "shortratio" = {
          # agShortRatioが0-1の値なので、ソートキーをひとつにまとめるのが面倒
          # そのためここはorderの引数を二つにしている
          agShortRatio <- (anSlotNo_Request[anCandSlot] - anSlotNo_Count[anCandSlot]) /  anSlotNo_Request[anCandSlot]
          anSortkey1 <- -1 * agShortRatio
          anSortkey2 <- as.integer(runif(length(anCandSlot), min=0, max=99))
          anCandSlot[order(anSortkey1, anSortkey2)]
        },
        stop()
      )
      # nSLOT_MAX個までを選ぶ
      anCandSlot <- anCandSlot[seq_len(nSLOT_MAX)]
      anCandSlot <- anCandSlot[!is.na(anCandSlot)]
    }
    if (bDEBUG) cat("[sub_getslot] -> anCandSlot (unordered):", paste0(anCandSlot, collapse=","), "\n")

    # 割付可能スロットに絞る
    anCandSlot <- anCandSlot[ abSlotNo_Hit[anCandSlot] == 1 ]

    # 除外条件を適用
    if (sSLOT_EXCLUDE == "allclosed"){
      if (sum(abSlotNo_Open[anCandSlot]) == 0){
        anCandSlot = c()
      }
    }

    # 後ろにNAを埋める
    out <- c(anCandSlot, rep(NA, nSLOT_MAX))[seq_len(nSLOT_MAX)]
  }

  if (bDEBUG) cat("[sub_getslot] -> out:", paste0(out, collapse=","), "\n")
  if (bDEBUG) cat("[sub_getslot] end.\n")
  return(out)
}
sub_checkslot <- function(
  abSlotNo_Hit,
  abSlotNo_Open,
  anSlotNo_Count,
  anSlotNo_Request,
  nSLOT_MAX,
  sSLOT_TYPE,
  sSLOT_FILTER,
  sSLOT_ORDER,
  sSLOT_EXCLUDE,
  bDEBUG
){
  #' Internal: Check assigned slots
  #'
  #' Internal. ある対象者のあるカテゴリについて、
  #'           ありうる割付スロットの組み合わせをすべて返す。
  #'           checkSurvey()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param abSlotNo_Hit an binary vector.
  #'    スロット番号から割付可能かどうかを引くベクトル
  #' @param abSlotNo_Open an binary vector.
  #'    スロット番号からオープンかどうかを引くベクトル
  #' @param anSlotNo_Count an integer vector.
  #'    スロット番号から獲得票数を引くベクトル
  #' @param anSlotNo_Request an integer vector.
  #'    スロット番号から目標票数を引くベクトル
  #' @param nSLOT_MAX 整数。
  #'    ある対象者に割り付けるスロット数の上限。
  #' @param sSLOT_TYPE 文字列。
  #'    スロット割付タイプ。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_FILTER 文字列。
  #'    スロット割付の際の絞り込み条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_ORDER 文字列。
  #'    スロット割付の際の順序付け条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_EXCLUDE 文字列。
  #'    スロット割付の際の除外条件。指定は必須。詳細はvignetteを参照。
  #' @param DEBUG as logical.
  #'    デバッグモード
  #'
  #' @return a list of integer vectors.
  #'    要素は割付スロットの組み合わせ。ソート済み。

  if (bDEBUG) cat("[sub_checkslot] start.\n")

  ## 引数チェック - - - - - - - - - -
  if (bDEBUG){

    cat("[sub_checkslot] abSlotNo_Hit:", abSlotNo_Hit, "\n")
    cat("[sub_checkslot] abSlotNo_Open:", abSlotNo_Open, "\n")
    cat("[sub_checkslot] anSlotNo_Count:", anSlotNo_Count, "\n")
    cat("[sub_checkslot] anSlotNo_Request:", anSlotNo_Request, "\n")
    cat("[sub_checkslot] nSLOT_MAX:", nSLOT_MAX, "\n")
    cat("[sub_checkslot] sSLOT_TYPE:", sSLOT_TYPE, "\n")
    cat("[sub_checkslot] sSLOT_FILTER:", sSLOT_FILTER, "\n")
    cat("[sub_checkslot] sSLOT_ORDER:", sSLOT_ORDER, "\n")
    cat("[sub_checkslot] sSLOT_EXCLUDE:", sSLOT_EXCLUDE, "\n")

    ## abSlotNo_Hit
    ## 欠損無し, 0か1
    stopifnot(!is.na(abSlotNo_Hit))
    stopifnot(abSlotNo_Hit %in% 0:1)

    ## abSlotNo_Open
    ## 長さはabSlotNo_Hitと同じ、欠損無し、0か1
    stopifnot(length(abSlotNo_Open) == length(abSlotNo_Hit))
    stopifnot(!is.na(abSlotNo_Open))
    stopifnot(abSlotNo_Open %in% 0:1)

    ## anSlotNo_Count
    ## 長さはabSlotNo_Hitと同じ、欠損無し
    stopifnot(length(anSlotNo_Count) == length(abSlotNo_Hit))
    stopifnot(!is.na(anSlotNo_Count))

    ## anSlotNo_Request
    ## 長さはabSlotNo_Hitと同じ、欠損無し
    stopifnot(length(anSlotNo_Request) == length(abSlotNo_Hit))
    stopifnot(!is.na(anSlotNo_Request))

    ## nSLOT_MAX
    ## 欠損無し
    stopifnot(!is.na(nSLOT_MAX))

    ## sSLOT_TYPE, sSLOT_FILTER, sSLOT_ORDER, sSLOT_EXCLUDE
    stopifnot(sSLOT_TYPE    %in% c("adaptive", "nonadaptive"))
    stopifnot(sSLOT_FILTER  %in% c("all", "open"))
    stopifnot(sSLOT_ORDER   %in% c("random", "openclosed", "shortnum", "shortratio"))
    stopifnot(sSLOT_EXCLUDE %in% c("none", "allclosed"))
  }

  ## ここからメイン - - - - - - - - -

  # 候補の決定
  # 候補の決定
  if (sSLOT_TYPE == "adaptive"){
    anCandSlot <- switch(
      sSLOT_FILTER,
      "all"  = seq_along(abSlotNo_Hit)[abSlotNo_Hit == 1],
      "open" = seq_along(abSlotNo_Hit)[abSlotNo_Hit * abSlotNo_Open == 1],
      stop()
    )
  } else {
    # 非適応的な場合
    anCandSlot <- seq_along(abSlotNo_Hit)
  }
  if (bDEBUG) cat("[sub_checkslot] anCandSlot:", anCandSlot, "\n")

  # ソートキーの決定
  anSortkey <- switch(
    sSLOT_ORDER,
    "random"     = rep(1, length(anCandSlot)),
    "openclosed"  = -1 * abSlotNo_Open[anCandSlot],
    "shortnum"   = {
      anShortNum <- anSlotNo_Request[anCandSlot] - anSlotNo_Count[anCandSlot]
      -1 * anShortNum
    },
    "shortratio" = {
      agShortRatio <- (anSlotNo_Request[anCandSlot] - anSlotNo_Count[anCandSlot]) /  anSlotNo_Request[anCandSlot]
      -1 * agShortRatio
    },
    stop()
  )

  lOut <- getCandidate(
    anCandSlot,
    anSortkey,
    nSLOT_MAX,
    bDEBUG = bDEBUG
  )

  # nonadp の場合、割付不能カテゴリが含まれているので取り除く
  if (sSLOT_TYPE == "nonadaptive"){
    lOut <- unique( lapply( lOut, function(anIn) anIn[ abSlotNo_Hit[anIn] == 1 ] ) )
  }

  # 除外条件を適用
  if (sSLOT_EXCLUDE == "allclosed"){
    lOut <- lOut[ sapply(lOut, function(x) sum(abSlotNo_Open[x])) > 0 ]
  }

  if (bDEBUG) {
    cat("[sub_checkslot] lOut:\n")
    print(lOut)
    cat("[sub_checkslot] end.\n")
  }
  return(lOut)
}
execAssign <- function(
  anSEQ_PERSON,
  mnSEQ_CAT_PERSON,
  mbPERSON_CAT_USE,
  lPERSON_SLOT_HIT,
  lSLOT_REQUEST,
  nCAT_MAX,
  sCAT_TYPE,
  sCAT_FILTER,
  sCAT_ORDER,
  sCAT_EXCLUDE,
  nSLOT_MAX,
  sSLOT_TYPE,
  sSLOT_FILTER,
  sSLOT_ORDER,
  sSLOT_EXCLUDE,
  bCHECKCOMPLETE = TRUE,
  bVERBOSE       = FALSE
){
  #' Internal: Assign participants into 'categories' and 'slots'
  #'
  #' Internal: 対象者をカテゴリ・スロットに割り付ける。
  #'
  #' @export
  #'
  #' @keywords internal
  #'
  #' @param anSEQ_PERSON an integer vector.
  #'    \code{anSEQ_PERSON[i]}は、\code{i}番目に調査に参加する対象者の
  #'    \code{mbPERSON_CAT_USE}上の行番号。欠損不可。
  #'
  #' @param mnSEQ_CAT_PERSON an integer matrix.
  #'    \code{mnSEQ_CAT_PERSON[i,j]}は、\code{i}番目に調査に参加する
  #'    対象者の、カテゴリ\code{j}のスロット割付可能性を調べる際に参照すべき、
  #'    \code{lPERSON_SLOT_HIT[[j]]}の行番号。
  #'    欠損を許すが、
  #'    \code{mbPERSON_CAT_USE[anSEQ_PERSON[i],j] == 1}のとき、\code{mnSEQ_CAT_PERSON[i,j]}の
  #'    欠損は不可。
  #'
  #' @param mbPERSON_CAT_USE an binary matrix.
  #'    対象者のカテゴリ割付可能性を表す。\code{mbPERSON_CAT_USE[i,j]}は以下を表す。
  #'    \itemize{
  #'    \item 1: 調査対象者iはカテゴリjについて割付可能。
  #'    \item 0: 調査対象者iはカテゴリjについて割付不能。
  #'    }
  #'    欠損を許すが、\code{mbPERSON_CAT_USE[anSEQ_PERSON, ]}における欠損は不可。
  #'
  #' @param lPERSON_SLOT_HIT a list of binary matrices.
  #'    対象者のスロット割付可能性を表す。
  #'    要素jの行列の要素(i,k)は以下を表す。
  #'    \itemize{
  #'    \item 1: 調査対象者iはカテゴリjのスロットkについて割付可能。
  #'    \item 0: 調査対象者iはカテゴリjのスロットkについて割付不能。
  #'    }
  #'    欠損を許すが、
  #'    \code{mbPERSON_CAT_USE[i,j] == 1}のとき、
  #'    \code{lPERSON_SLOT_HIT[[j]][i, ]}における欠損は不可。
  #'
  #' @param lSLOT_REQUEST a list of integer vectors.
  #'    各スロットに割り付ける対象者数の下限。
  #'    要素jのベクトルの要素kは, カテゴリjのスロットkに割り付ける対象者の下限を表す。
  #'
  #' @param nCAT_MAX an integer.
  #'    ある対象者に割り付けるカテゴリ数の上限。
  #' @param sCAT_TYPE a string.
  #'    カテゴリ割付タイプ。詳細はvignetteを参照。
  #' @param sCAT_FILTER a string.
  #'    カテゴリ割付の際の絞り込み条件。詳細はvignetteを参照。
  #' @param sCAT_ORDER a string.
  #'    カテゴリ割付の際の順序付け条件。詳細はvignetteを参照。
  #' @param sCAT_EXCLUDE a.string
  #'    カテゴリ割付の際の除外条件。詳細はvignetteを参照。
  #'
  #' @param nSLOT_MAX an integer.
  #'    ある対象者に割り付けるスロット数の上限。
  #' @param sSLOT_TYPE a string.
  #'    スロット割付タイプ。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_FILTER a string.
  #'    スロット割付の際の絞り込み条件。詳細はvignetteを参照。
  #' @param sSLOT_ORDER a string.
  #'    スロット割付の際の順序付け条件。詳細はvignetteを参照。
  #' @param sSLOT_EXCLUDE a string.
  #'    スロット割付の際の除外条件。指定は必須。詳細はvignetteを参照。
  #'
  #' @param bCHECKCOMPLETE a logical.
  #'    \code{anSEQ_PERSON}を使い切っても
  #'    全スロットがクローズしなかったらエラーを発生させる
  #'
  #' @param bVERBOSE a logical.
  #'    詳細を画面表示する
  #'
  #' @return an integer matrix.
  #'    行は全スロットがクローズするまでの対象者。0行かもしれないことに注意。
  #'    行名はmbPERSON_CAT_USEの該当行の行名。
  #'    列は、左から順に以下の通り。ただし、
  #'    カテゴリ番号とは\code{mbPERSON_CAT_USE}における列番号を指し、
  #'    スロット番号とは\code{lPERSON_SLOT_HIT}の当該カテゴリにおける列番号を指す。
  #'    \itemize{
  #'    \item \code{SEQ}: 調査参加順(連番)
  #'    \item \code{nPerson}: 対象者番号 (\code{mbPERSON_CAT_USE}上の行番号)
  #'    \item \code{nCat_1}: 割付カテゴリ\code{1}のカテゴリ番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nCat_}(nCAT_MAX): 割付カテゴリ\code{nCAT_MAX}のカテゴリ番号、ないし\code{NA}
  #'    \item \code{nCat}: 割付スロットが属するカテゴリ番号、ないし\code{NA}
  #'    \item \code{nSlot_1}: 割付スロット\code{1}のスロット番号、ないし\code{NA}
  #'    \item ...
  #'    \item \code{nSlot_}(nSLOT_MAX): 割付スロット\code{nSLOT_MAX}のスロット番号、ないし\code{NA}
  #'    }
  #
  # notes:
  #   - この関数は実行速度が大事！
  #   - 呼び出し元が並列処理していることを想定すること
  #   - 外のサブルーチンを呼ばないこと (呼んでもいいけど、並列処理の際にロードが必要になる)

  # - - - -
  bDEBUG = FALSE
  # - - - -

  if (bVERBOSE){
    cat("[execAssign] Start!\n")
  }

  ## 引数チェック - - - - - - -
  if (bDEBUG){
    ## anSEQ_PERSON
    ## 指定された対象者番号は実在する
    stopifnot(anSEQ_PERSON %in% seq_len(nrow(mbPERSON_CAT_USE)))

    ## mnSEQ_CAT_PERSON
    ## 行列である
    stopifnot(is.matrix(mnSEQ_CAT_PERSON))
    # print(mnSEQ_CAT_PERSON)
    # print(is.matrix(mnSEQ_CAT_PERSON))
    ## 行数はanSEQ_PERSONの行数と一致する.
    stopifnot(nrow(mnSEQ_CAT_PERSON) == length(anSEQ_PERSON))
    ## 指定された対象者番号は実在する
    x <- as.vector(mnSEQ_CAT_PERSON)
    stopifnot(x[!is.na(x)] %in% seq_len(nrow(mbPERSON_CAT_USE)))
    ## mbPERSON_CAT_USE[anSEQ_PERSON[i],j] == 1のとき、mnSEQ_CAT_PERSON[i,j]の欠損は不可。
    y <- as.vector(mbPERSON_CAT_USE[anSEQ_PERSON, ])
    stopifnot( !(y == 1 & is.na(x)) )

    ## mbPERSON_CAT_USE
    ## 列数はmnSEQ_CAT_PERSONの列数と一致する
    ## mbPERSON_CAT_USE[anSEQ_PERSON, ] の欠損は不可
    stopifnot(ncol(mbPERSON_CAT_USE) == ncol(mnSEQ_CAT_PERSON))
    stopifnot(!is.na(mbPERSON_CAT_USE[anSEQ_PERSON, ]))

    ## lPERSON_SLOT_HIT
    ## 要素数はmnSEQ_CAT_PERSONの列数と一致する
    stopifnot(length(lPERSON_SLOT_HIT) == ncol(mbPERSON_CAT_USE))
    ## 各要素の行数はmbPERSON_CAT_USEの行数と一致する
    stopifnot(sapply(lPERSON_SLOT_HIT, nrow) == nrow(mbPERSON_CAT_USE))
    # ## mbPERSON_CAT_USE[i,j] == 1のとき、
    # ## lPERSON_SLOT_HIT[[j]][i, ]}における欠損は不可。
    # for (j in seq_len(ncol(mbPERSON_CAT_USE))){
    #   z <- apply(lPERSON_SLOT_HIT[[j]], 1, function(x) sum(is.na(x)))
    #   stopifnot( !(!is.na(mbPERSON_CAT_USE[,j]) & mbPERSON_CAT_USE[,j] == 1 & z > 0))
    # }

    ## lSLOT_REQUEST
    ## 要素数はmnSEQ_CAT_PERSONの列数と一致する
    ## 各要素の長さはlPERSON_SLOT_HITの各要素の長さと同じ
    ## 欠損はない
    stopifnot(length(lSLOT_REQUEST) == ncol(mnSEQ_CAT_PERSON))
    stopifnot(sapply(lSLOT_REQUEST, length) == sapply(lPERSON_SLOT_HIT, ncol))
    stopifnot(sapply(lSLOT_REQUEST, function(x) sum(is.na(x))) == 0)

    ## nCAT_MAX
    ## 欠損無し
    stopifnot(!is.na(nCAT_MAX))

    ## sCAT_TYPE, sCAT_FILTER, sCAT_ORDER, sCAT_EXCLUDE
    stopifnot(sCAT_TYPE    %in% c("adaptive", "nonadaptive"))
    stopifnot(sCAT_FILTER  %in% c("all", "open"))
    stopifnot(sCAT_ORDER   %in% c("random", "openclosed", "shortnum", "shortratio"))
    stopifnot(sCAT_EXCLUDE %in% c("none", "allclosed"))

    ## nSLOT_MAX
    ## 欠損無し
    stopifnot(!is.na(nSLOT_MAX))

    ## sSLOT_TYPE, sSLOT_FILTER, sSLOT_ORDER, sSLOT_EXCLUDE
    stopifnot(sSLOT_TYPE    %in% c("adaptive", "nonadaptive"))
    stopifnot(sSLOT_FILTER  %in% c("all", "open"))
    stopifnot(sSLOT_ORDER   %in% c("random", "openclosed", "shortnum", "shortratio"))
    stopifnot(sSLOT_EXCLUDE %in% c("none", "allclosed"))

    ## bCHECKCOMPLETE
    ## 値は期待通り
    stopifnot(bCHECKCOMPLETE %in% c(TRUE, FALSE))

    ## bVERBOSE
    ## 値は期待通り
    stopifnot(bVERBOSE %in% c(TRUE, FALSE))
  }

  ## ここからメイン - - - - - - - -

  # 引数のコピーに過ぎないベクトル(状態を表現しないベクトル)
  # カテゴリ番号からスロット数を引くベクトル
  anCatNo_NumSlot <- sapply(lSLOT_REQUEST, length)
  # スロット通番から目標票数を引くベクトル
  anLoc_Request <- unlist(lSLOT_REQUEST)
  # スロット通番からカテゴリ番号を引くベクトル
  anLoc_CatNo <- rep(seq_along(lSLOT_REQUEST), sapply(lSLOT_REQUEST, length))

  # スロットの状態ベクトル
  # スロット通番から現在の獲得票数を引くベクトル。ここでは初期値0
  anLoc_Count <- rep(0, length(unlist(lSLOT_REQUEST)))
  # スロット通番からオープン有無を引くベクトル。目標票数がすべてゼロかもなので真面目に算出
  abLoc_Open <- as.integer( anLoc_Count < anLoc_Request)

  # カテゴリの状態ベクトル
  # カテゴリ番号からオープンしているスロット数を引くベクトル。ここでは目標票数が0以上であるスロットの数
  anCatNo_Count <- sapply(lSLOT_REQUEST, function(x) sum(x>0))
  # カテゴリ番号からオープン有無を引くベクトル。目標票数がすべてゼロかもなので真面目に算出
  abCatNo_Open <- as.integer(anCatNo_Count > 0)

  # mnOut: カテゴリ割付履歴。速度を稼ぐため、大きめの固定サイズの数値行列にしている
  # 行: 抽出した人
  # 列: (SEQ, nPerson, 割付カテゴリ1の番号, 割付カテゴリ2の番号, ..., カテゴリの番号, 割付スロット1の番号, 割付スロット2の番号, ...)
  mnOut <- matrix(
    NA,
    nrow = length(anSEQ_PERSON),
    ncol = 2 + nCAT_MAX + 1 + nSLOT_MAX
  )
  colnames(mnOut) <- c(
    "SEQ",
    "nPerson",
    paste0("nCat", seq_len(nCAT_MAX)),
    "nCat",
    paste0("nSlot", seq_len(nSLOT_MAX))
  )

  if (all(abCatNo_Open == 0)) {
    # もしすべてのカテゴリが最初からクローズドだったら
    # mnOutを0行に削って返す
    out <- mnOut[FALSE,]

  } else {
    # 通常はこちら
    # ひとりづつ抽出し、スロット割付を行う
    for (i in seq_along(anSEQ_PERSON)){

      # 行番号
      nPerson <- anSEQ_PERSON[i]
      if (bVERBOSE){
        cat("[execAssign]", i, ": nPerson=", nPerson, " - - - - \n")
      }

      # その人のカテゴリ割付可能性をとってくる
      abCatNo_Use <- mbPERSON_CAT_USE[nPerson, ]
      # trap: 欠損はない
      if (bDEBUG){
        stopifnot(!is.na(abCatNo_Use))
      }
      if (bVERBOSE){
        cat("[execAssign] assignability to categories:", abCatNo_Use, "\n")
      }

      # 割付カテゴリを決定
      anAssignedCat <- sub_getcat(
        abCatNo_Use,
        abCatNo_Open,
        anCatNo_Count,
        anCatNo_NumSlot,
        nCAT_MAX,
        sCAT_TYPE,
        sCAT_FILTER,
        sCAT_ORDER,
        sCAT_EXCLUDE,
        bDEBUG = bDEBUG
      )
      if (bVERBOSE){
        cat("[execAssign] assigned categories:", anAssignedCat, "\n")
      }

      # NAをとる
      anAssignedCat_Compress <- anAssignedCat[!is.na(anAssignedCat)]

      # 割付カテゴリについてループ
      nParentCat <- NA
      anAssignedSlot <- rep(NA, nSLOT_MAX)
      for (nAssignedCat in anAssignedCat_Compress){
        if (bVERBOSE){
          cat("[execAssign] searching slots of assigned category", nAssignedCat, "... \n")
          cat("[execAssign] alternate person:", mnSEQ_CAT_PERSON[i, nAssignedCat], "\n")
        }

        # スロットの割付可能性をとってくる
        abSlotNo_Hit <- lPERSON_SLOT_HIT[[nAssignedCat]][mnSEQ_CAT_PERSON[i, nAssignedCat], ]

        if (bDEBUG){
          ## trap: 欠損はない
          stopifnot(!is.na(abSlotNo_Hit))
        }

        # さすがに長すぎる...
        # if (bVERBOSE){
        #   cat("[execAssign] abSlotNo_Hit:", abSlotNo_Hit, "\n")
        # }

        # 割付スロットを決定
        anAssignedSlot <- sub_getslot(
          abSlotNo_Hit,
          abLoc_Open[anLoc_CatNo == nAssignedCat],
          anLoc_Count[anLoc_CatNo == nAssignedCat],
          anLoc_Request[anLoc_CatNo == nAssignedCat],
          nSLOT_MAX,
          sSLOT_TYPE,
          sSLOT_FILTER,
          sSLOT_ORDER,
          sSLOT_EXCLUDE,
          bDEBUG = bDEBUG
        )

        # 脱出
        if (any(!is.na(anAssignedSlot))){
          nParentCat <- nAssignedCat
          break
        }
      }
      if (bVERBOSE){
        cat("[execAssign] parent category of assigned slots:", nParentCat, "\n")
        cat("[execAssign] assigned slots:", anAssignedSlot, "\n")
      }

      # 割付履歴に書き込む
      anOut <- c(i, nPerson, anAssignedCat, nParentCat, anAssignedSlot)
      if (bVERBOSE){
        cat("[execAssign] output:", anOut, "\n")
      }
      mnOut[i, ] <- anOut

      # ここからこの対象者についての終了処理

      if (!is.na(nParentCat)){
        # スロットの獲得票数を加算
        # naを詰める
        anAssignedSlot_Compressed <- anAssignedSlot[!is.na(anAssignedSlot)]
        # スロットの獲得票数を取り出して
        anSlotNo_Count <- anLoc_Count[anLoc_CatNo == nParentCat]
        # 加算して
        anSlotNo_Count[anAssignedSlot_Compressed] <- anSlotNo_Count[anAssignedSlot_Compressed] + 1
        # 戻す
        anLoc_Count[anLoc_CatNo == nParentCat] <- anSlotNo_Count
        if (bVERBOSE){
          cat("[execAssign] # total number of acquired votes:", sum(anLoc_Count), "\n")
        }

        # オープンスロット判定
        abLoc_Open[anLoc_CatNo == nParentCat] <- as.integer(
          anLoc_Count[anLoc_CatNo == nParentCat] < anLoc_Request[anLoc_CatNo == nParentCat]
        )
        if (bVERBOSE){
          cat("[execAssign] # total number of opened slots:", sum(abLoc_Open), "\n")
        }

        # 各カテゴリのオープンスロット数
        anCatNo_Count[nParentCat] <- sum(abLoc_Open[anLoc_CatNo == nParentCat])
        if (bVERBOSE){
          cat("[execAssign] # number of opened slots of each category:", anCatNo_Count, "\n")
        }

        # 各カテゴリのオープン判定
        abCatNo_Open <- as.integer(anCatNo_Count > 0)
        if (bVERBOSE){
          cat("[execAssign] # number of opened categories:", anCatNo_Count, "\n")
        }
      }

      # すべてのカテゴリがクローズしたら脱出
      if (all(abCatNo_Open == 0)) break
    }
    # 割付履歴のうち、実際には値を格納しなかった行を削除して結合
    out <- mnOut[seq_len(i), ]
    # 行名として, mbPERSON_CAT_USEの行名を与える
    rownames(out) <- rownames(mbPERSON_CAT_USE)[anSEQ_PERSON[seq_len(i)]]
  }

  if (bCHECKCOMPLETE){
    # すべてのスロットがクローズしていなかったらエラー
    if (any(anLoc_Count < anLoc_Request))
      stop("[execAssign] Error: A survey was terminated with insufficient sample size.")
  }

  if (bVERBOSE){
    cat("[execAssign] End\n")
  }
  return(out)
}
checkSurvey <- function(
  lSURVEY,
  bVERBOSE = FALSE
){
  #' check assigned categories and slots in survey data
  #'
  #' 調査データを受け取り、
  #' カテゴリ・スロット割付が正しく行われていたことを確認する。
  #'
  #' @export
  #'
  #' @param lSURVEY an object of `surveydata` class.。
  #'                調査データ。\code{\link{makeSurvey}}で生成する。
  #' @param bVERBOSE a logical.
  #'                詳細を画面表示するか。
  #'
  #' @return a data frame.
  #'    割付結果とその検証。
  #'
  #'    以下の列を持つ。
  #'    \itemize{
  #'    \item \code{nPerson}:                   対象者番号。1からの連番
  #'    \item \code{bCat_(j)}:                  カテゴリjへの割付可能性
  #'    \item \code{bCatSlot_(j)_(k)}:          カテゴリjのスロットkへの割付可能性
  #'    \item \code{nCount_Cat_(j)}:            この対象者の割付終了時点でのカテゴリjへの割付人数
  #'    \item \code{nCount_CatSlot_(j)_(k)}:    この対象者の割付終了時点でのカテゴリj, スロットkへの割付人数
  #'    \item \code{nAssignCat_(c)}:            この対象者のc番目の割付カテゴリ(順序に意味はない)
  #'    \item \code{nParentCat}:                この対象者の割付スロットが属するカテゴリ
  #'    \item \code{nAssignSlot_(s)}:           この対象者のs番目の割付スロット(順序に意味はない)
  #'    \item \code{bValid}:                    チェックを通過したか: {1:通過した, 0:通過しなかった}
  #'    \item \code{sCheckMsg}:                 チェックを通過しなかった理由。通過したときは""。
  #'    }
  #'
  # note:
  #   ウェイト算出時に使う関数なので、
  #   本来 weighting.R に置くべきだが、コードが execAssign()と似ている
  #   ので、assign.Rに置いている

  # - - - -
  bDEBUG <- FALSE
  # - - - -

  if (bVERBOSE){
    cat("[checkSurvey] Start!\n")
  }

  ## 引数チェック - - - - -
  ## lSURVEY
  ## クラスは期待通り
  stopifnot(class(lSURVEY) == "surveydata")

  ## bVERBOSE
  ## 値は期待通り
  stopifnot(bVERBOSE %in% c(TRUE, FALSE))

  ## ここからメイン - - - - - - - -

  # カテゴリ番号からスロット数を引くベクトル
  anCatNo_NumSlot <- sapply(lSURVEY$lSLOT, ncol)
  # カテゴリ番号からオープンしているスロット数を引くベクトル。ここでは全スロットの数
  anCatNo_Count <- anCatNo_NumSlot
  # カテゴリ番号からオープン有無を引くベクトル。ここでは初期値1
  abCatNo_Open <- rep(1, length(anCatNo_NumSlot))

  # スロット通番から現在の獲得票数を引くベクトル。ここでは初期値0
  anLoc_Count <- rep(0, sum(anCatNo_NumSlot))
  # スロット通番から目標票数を引くベクトル。
  anLoc_Request <- unlist(lSURVEY$lSLOT_REQUEST)
  # スロット通番からオープン有無を引くベクトル。ここでは初期値1
  abLoc_Open <- rep(1, sum(anCatNo_NumSlot))
  # スロット通番からカテゴリ番号を引くベクトル
  anLoc_CatNo <- rep(seq_along(anCatNo_NumSlot), anCatNo_NumSlot)

  ## 出力
  ## mnPersonCatNo_Count
  ## 行は人、列はカテゴリ。その対象者を割り付けた後のカウントを格納
  mnPersonCatNo_Count <- matrix(NA, nrow = nrow(lSURVEY$mbCAT), ncol = ncol(lSURVEY$mbCAT))
  colnames(mnPersonCatNo_Count) <- paste0("nCount_Cat_", seq_len(ncol(lSURVEY$mbCAT)))

  ## mnPersonLoc_Count
  ## 行は人、列はカテゴリxスロット。その対象者を割り付けた後のカウントを格納
  mnPersonLoc_Count   <- matrix(NA, nrow = nrow(lSURVEY$mbCAT), ncol = length(anLoc_Count))
  colnames(mnPersonLoc_Count) <- unlist(
    lapply(
      seq_along(anCatNo_NumSlot),
      function(j) paste0("nCount_CatSlot_", j, "_", seq_len(anCatNo_NumSlot[j]))
    )
  )

  ## asCheckMsg
  ## 行は人。エラーが見つかったときのメッセージを格納
  asCheckMsg <- rep(NA, nrow(lSURVEY$mbCAT))

  # ひとりづつ抽出し、割付を行う
  for (nPerson in seq_len(nrow(lSURVEY$mbCAT))){

    # if (nPerson == 442){
    #   bDEBUG <- TRUE
    #   bVERBOSE <- TRUE
    # } else {
    #   bDEBUG <- FALSE
    #   bVERBOSE <- FALSE
    # }

    if (bVERBOSE){
      cat("[checkSurvey] nPerson:", nPerson, " - - - - \n")
    }

    # この人のエラーメッセージのベクトル
    asCurrentMsg <- c()

    ### Phase 1. カテゴリの割り付けのチェック - - - - - - - - - - - - - -
    if (bVERBOSE){
      cat("[checkSurvey] Phase 1.\n")
    }

    # その人のカテゴリ割付可能性をとってくる
    abCatNo_Use <- lSURVEY$mbCAT[nPerson, ]

    if (bVERBOSE){
      cat("[checkSurvey] abCatNo_Use:", abCatNo_Use, "\n")
      cat("[checkSurvey] abCatNo_Open:", abCatNo_Open, "\n")
      cat("[checkSurvey] anCatNo_Count:", anCatNo_Count, "\n")
    }

    # 割付カテゴリの候補
    lPossibleCat <- sub_checkcat(
      abCatNo_Use,
      abCatNo_Open,
      anCatNo_Count,
      anCatNo_NumSlot,
      ncol(lSURVEY$mnASSIGNCAT),
      lSURVEY$sCAT_TYPE,
      lSURVEY$sCAT_FILTER,
      lSURVEY$sCAT_ORDER,
      lSURVEY$sCAT_EXCLUDE,
      bDEBUG = bDEBUG
    )

    # メッセージ用
    if (length(lPossibleCat) <= 10){
      sPossibleCat <- paste0(
        sapply(
          lPossibleCat,
          function(x) paste0("{", paste0(x, collapse = ","), "}")
        ),
        collapse = ","
      )
    } else {
      sPossibleCat <- paste0(
        sapply(
          lPossibleCat[1:10],
          function(x) paste0("{", paste0(x, collapse = ","), "}")
        ),
        collapse = ","
      )
      sPossibleCat <- paste0(sPossibleCat, " etc.")
    }

    if (bVERBOSE){
      cat("[checkSurvey] lPossibleCat:", sPossibleCat, "\n")
    }

    # 実際の割付カテゴリ
    anAssignCat <- lSURVEY$mnASSIGNCAT[nPerson,]
    anAssignCat <- anAssignCat[!is.na(anAssignCat)]
    anAssignCat <- sort(anAssignCat)
    if (bVERBOSE){
      cat("[checkSurvey] anAssignCat:", anAssignCat, "\n")
    }

    # エラーチェック
    # 割付カテゴリは、候補のなかに含まれている。ないし候補が存在しない
    abMatch <- sapply(lPossibleCat, function(x) all(sort(x) == anAssignCat))
    if (length(lPossibleCat) > 0 & all(!abMatch)){
      sNewMsg <- paste0(
        "Assigned to unexpected categories. Possible categories are:",
        sPossibleCat
      )
      if (bVERBOSE){
        cat("[checkSurvey] Error:", sNewMsg, "\n")
      }
      asCurrentMsg <- c(asCurrentMsg, sNewMsg)
    }

    ### Phase 2. 親カテゴリの割り付けのチェック - - - - - - - - - - - - - -
    if (bVERBOSE){
      cat("[checkSurvey] Phase 2.\n")
    }

    # 割付カテゴリごとに割付スロットの候補をとってくる
    lPossibleCatSlot <- lapply(
      anAssignCat,
      function(nAssignCat){
        if (bDEBUG){
          cat("[checkSurvey] checking at nAssignCat=", nAssignCat, "\n")
        }

        # その割付カテゴリでのスロットの割付可能性
        abSlotNo_Hit <- lSURVEY$lSLOT[[nAssignCat]][nPerson, ]

        # 割付スロットの候補
        lPossibleSlot <- sub_checkslot(
          abSlotNo_Hit,
          abLoc_Open[anLoc_CatNo == nAssignCat],
          anLoc_Count[anLoc_CatNo == nAssignCat],
          anLoc_Request[anLoc_CatNo == nAssignCat],
          ncol(lSURVEY$mnASSIGNSLOT),
          lSURVEY$sSLOT_TYPE,
          lSURVEY$sSLOT_FILTER,
          lSURVEY$sSLOT_ORDER,
          lSURVEY$sSLOT_EXCLUDE,
          bDEBUG = bDEBUG
        )
        return(lPossibleSlot)
      }
    )

    # 親カテゴリの候補
    anPossibleParent <- anAssignCat[sapply(lPossibleCatSlot, length) > 0]
    if (bVERBOSE){
      cat("[checkSurvey] anPossibleParent:", anPossibleParent, "\n")
    }

    # 親カテゴリ
    # 親カテゴリが割付カテゴリのなかに含まれていることはmakeSurvey()で確認済
    nParentCat <- lSURVEY$anPARENTCAT[nPerson]
    nParentCat <- nParentCat[!is.na(nParentCat)]
    if (bVERBOSE){
      cat("[checkSurvey] nParentCat:", nParentCat, "\n")
    }

    # エラーチェック
    # 親カテゴリの有無は親カテゴリ候補の有無に合致し、
    # もし親カテゴリがあるならばそれは親カテゴリ候補に入っているはず
    if (!(
      ((length(nParentCat) > 0) == (length(anPossibleParent) > 0))
      &
      ((length(nParentCat) == 0) || (nParentCat %in% anPossibleParent))
    )){
      sNewMsg <- paste0(
        "Unexpected parent category:", nParentCat, ". Possible categories are:",
        paste0(anPossibleParent, collapse = ",")
      )
      if (bVERBOSE){
        cat("[checkSurvey] Error:", sNewMsg, "\n")
      }
      asCurrentMsg <- c(asCurrentMsg, sNewMsg)
    }

    ### Phase 3. スロットの割り付けのチェック - - - - - - - - - - - - - -
    if (bVERBOSE){
      cat("[checkSurvey] Phase 3.\n")
    }

    # 親カテゴリがある場合
    if (length(nParentCat) > 0){

      # めんどくさいので、割付スロットの候補をもう一度調べる
      # その割付カテゴリでのスロットの割付可能性
      abSlotNo_Hit <- lSURVEY$lSLOT[[nParentCat]][nPerson, ]
      # 割付スロットの候補
      lPossibleSlot <- sub_checkslot(
        abSlotNo_Hit,
        abLoc_Open[anLoc_CatNo == nParentCat],
        anLoc_Count[anLoc_CatNo == nParentCat],
        anLoc_Request[anLoc_CatNo == nParentCat],
        ncol(lSURVEY$mnASSIGNSLOT),
        lSURVEY$sSLOT_TYPE,
        lSURVEY$sSLOT_FILTER,
        lSURVEY$sSLOT_ORDER,
        lSURVEY$sSLOT_EXCLUDE,
        bDEBUG = bDEBUG
      )
      if (bDEBUG){
        cat("[checkSurvey] lPossibleSlot: \n")
        print(lPossibleSlot)
      }

      # メッセージ用
      if (length(lPossibleSlot) <= 10){
        sPossibleSlot <- paste0(
          sapply(
            lPossibleSlot,
            function(x) paste0("{", paste0(x, collapse = ","), "}")
          ),
          collapse = ","
        )
      } else {
        sPossibleSlot <- paste0(
          sapply(
            lPossibleSlot[1:10],
            function(x) paste0("{", paste0(x, collapse = ","), "}")
          ),
          collapse = ","
        )
        sPossibleSlot <- paste0(sPossibleSlot, " etc.")
      }
      if (bVERBOSE){
        cat("[checkSurvey] anPossibleSlot:", sPossibleSlot, "\n")
      }

      # 実際の割付スロット
      anAssignSlot <- lSURVEY$mnASSIGNSLOT[nPerson,]
      anAssignSlot <- anAssignSlot[!is.na(anAssignSlot)]
      anAssignSlot <- sort(anAssignSlot)
      if (bVERBOSE){
        cat("[checkSurvey] anAssignSlot:", anAssignSlot, "\n")
      }

      # エラーチェック
      # 割付スロットは、候補のなかに含まれている
      abMatch <- sapply(lPossibleSlot, function(x) all(sort(x) == anAssignSlot))

      if (all(!abMatch)){
        sNewMsg <- paste0(
          "Assigned to unexpected slots in category", nParentCat, ". Possible slots are:",
          sPossibleSlot
        )
        if (bVERBOSE){
          cat("[checkSurvey] Error:", sNewMsg, "\n")
        }
        asCurrentMsg <- c(asCurrentMsg, sNewMsg)
      }

      ### Phase 4. この対象者についての終了処理 - - - - - - - - - - - - - -
      if (bVERBOSE){
        cat("[checkSurvey] Phase 4.\n")
      }
      # スロットの獲得票数を加算
      # スロットの獲得票数を取り出して
      anSlotNo_Count <- anLoc_Count[anLoc_CatNo == nParentCat]
      # 加算して
      anSlotNo_Count[anAssignSlot] <- anSlotNo_Count[anAssignSlot] + 1
      # 戻す
      anLoc_Count[anLoc_CatNo == nParentCat] <- anSlotNo_Count
      # オープンスロット判定
      abLoc_Open[anLoc_CatNo == nParentCat] <- as.integer(
        anLoc_Count[anLoc_CatNo == nParentCat] < anLoc_Request[anLoc_CatNo == nParentCat]
      )
      # 各カテゴリのオープンスロット数
      anCatNo_Count[nParentCat] <- sum(abLoc_Open[anLoc_CatNo == nParentCat])
      # 各カテゴリのオープン判定
      abCatNo_Open <- as.integer(anCatNo_Count > 0)
    }

    # 割付履歴に書き込む
    mnPersonCatNo_Count[nPerson,] <- anCatNo_Count
    mnPersonLoc_Count[nPerson,] <- anLoc_Count
    asCheckMsg[nPerson] <- paste0(asCurrentMsg, collapse=";")

    ### if (nPerson == 442) stop()

  }

  # すべてのカテゴリがクローズしていない場合、最後の対象者をエラーにする
  if (!all(abCatNo_Open == 0)) {
    asCheckMsg[nPerson] <- paste0(
      c(
        asCheckMsg[nPerson],
        "Not all categories are closed."
      ),
      collapse = ";"
    )
  }

  ## 格納
  mbCat <- lSURVEY$mbCAT
  colnames(mbCat) <- paste0("bCat_", seq_len(ncol(mbCat)))
  mbSlot <- do.call(cbind, lSURVEY$lSLOT)
  colnames(mbSlot) <- unlist(
    lapply(
      seq_along(anCatNo_NumSlot),
      function(j) paste0("bCatSlot_", j, "_", seq_len(anCatNo_NumSlot[j]))
    )
  )
  mnAssignCat <- lSURVEY$mnASSIGNCAT
  colnames(mnAssignCat) <- paste0("nAssignCat_", seq_len(ncol(mnAssignCat)))
  mnAssignSlot <- lSURVEY$mnASSIGNSLOT
  colnames(mnAssignSlot) <- paste0("nAssignSlot_", seq_len(ncol(mnAssignSlot)))

  mnPersonLoc_Short <- sweep(-mnPersonLoc_Count, 2, anLoc_Request, FUN = "+")
  colnames(mnPersonLoc_Short) <- sub("Count", "Short", colnames(mnPersonLoc_Short))

  out <- data.frame(
    nPerson = seq_len(nrow(mbCat)),
    mbCat,
    mbSlot,
    mnPersonCatNo_Count,
    mnPersonLoc_Count,
    mnPersonLoc_Short,
    mnAssignCat,
    nParentCat = lSURVEY$anPARENTCAT,
    mnAssignSlot,
    bValid = ifelse(asCheckMsg == "", 1, 0),
    sCheckMsg = asCheckMsg
  )
  cat("[checkSurvey]", sum(out$bValid != 1), "errors are found.\n")
  if (bVERBOSE){
    cat("[checkSurvey] End\n")
  }
  return(out)
}
