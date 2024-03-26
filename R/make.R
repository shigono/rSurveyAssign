### make.R
###   データ作成の関数
###
makePop <- function(
    mbCAT,
    lSLOT,
    sVERBOSE = c("simple", "none", "detail")
){
  #' make population data
  #'
  #' 母集団データを作成する。
  #'
  #' @export
  #'
  #' @param mbCAT an integer matrix.
  #'    母集団メンバーのカテゴリ割付可能性を表す。
  #'
  #'    \code{mbCAT[i,j]}は以下を表す。欠損不可。
  #'    \itemize{
  #'    \item 1: 母集団メンバーiはカテゴリjについて割付可能。
  #'    \item 0: 母集団メンバーiはカテゴリjについて割付不能。
  #'    }
  #'
  #'    列に名前を付けるとカテゴリ名とみなされる。
  #'    名前を付けるならばすべての列に重複なく名前をつけること。
  #'
  #' @param lSLOT a list of integer matrices.
  #'    母集団メンバーのスロット割付可能性を表す。
  #'
  #'    \code{lSLOT[[j]][i,k]}は以下を表す。
  #'    \itemize{
  #'    \item 1: 母集団メンバーiはカテゴリjのスロットkについて割付可能。
  #'    \item 0: 母集団メンバーiはカテゴリjのスロットkについて割付不能。
  #'    }
  #'
  #'    \code{mbCAT[i,j] == 0}のとき、
  #'    \code{lSLOT[[j]][i, ]}はすべて欠損として扱われる
  #'    (すなわち、割付不能カテゴリに属するスロットへの割付可能性は
  #'    無視される)。
  #'
  #'    \code{mbCAT[i,j] == 1}のとき、
  #'    \code{lSLOT[[j]][i, ]}は欠損不可
  #'    (すなわち、割付不能カテゴリに属するスロットへの割付可能性は
  #'    すべて記述する必要がある)。
  #'
  #'    列に名前を付けた場合はスロット名とみなされる。
  #'    名前を付けるならばすべての列に重複なく名前をつけること。
  #'
  #' @param sVERBOSE a string.
  #'    画面表示レベル。
  #'
  #' @return an object of `popdata` class.
  #' その実体は以下の要素を持つリスト。
  #' \itemize{
  #' \item \code{mbCAT} 整数行列。
  #'                    引数mbCATとして与えられた行列。
  #'                    列名がついていなかった場合は、
  #'                    列名"Cat_(j)"が付与される。
  #' \item \code{lSLOT} 整数行列のリスト。
  #'                    引数lSLOTとして与えられたリスト。
  #'                    ただし、割付不能カテゴリに属するスロットへの
  #'                    割付可能性はすべて欠損に置き換えられている。
  #'                    引数lSLOTの要素に列名が付いていなかった場合は、
  #'                    列名"Slot_(j)_(k)"が付与される。
  #' }

  ## あいさつのためsVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[makePop] start.\n")
  }

  ## 引数チェック - - - - - - -
  ## mbCAT
  ## クラス
  stopifnot(is.matrix(mbCAT))
  ## 欠損を含まない
  if (any(is.na(mbCAT)))
    stop("[makePop] Error: mbCAT should have no NA.")
  ## 値は予期通り
  if (any(!(mbCAT %in% 0:1)))
    stop("[makePop] Error: The elements of mbCAT should be either 0 or 1.")
  ## 列名を取得する(あとで使います)
  asName_Cat <- colnames(mbCAT)
  if (!is.null(asName_Cat)){
    # もし列名が付いているならば列名はユニーク
    stopifnot(anyDuplicated(asName_Cat) == 0)
  }

  ### lSLOT
  ## クラス
  stopifnot(is.list(lSLOT))
  stopifnot(sapply(lSLOT, function(x) "matrix" %in% class(x)))
  ## 要素数はmbCATの列数と同じ
  if (length(lSLOT) != ncol(mbCAT))
    stop("[makePop] Error: The length of lSLOT should be equal to the number of columns of mbCAT.")
  ## 各要素の行数はmbCATの行数と同じ
  if (any(sapply(lSLOT, nrow) != nrow(mbCAT)))
    stop("[makePop] Error: The number of rows of each element in lSLOT should be equal to the number of rows of mbCAT.")
  for (i in seq_along(lSLOT)){
    mbSlot <- lSLOT[[i]]
    # 各要素に含まれている値は0, 1, NAのいずれか
    if (any (!( mbSlot[!is.na(mbSlot)] %in% c(0,1) )))
      stop("[makePop] Error: lSLOT[[", i, "]] has invalid values.")
    # カテゴリが割付可能な時、スロットの割付可能性は欠損不可
    if (any(is.na(mbSlot[mbCAT[,i] == 1,]))){
      stop("[makePop] Error: lSLOT[[", i, "]] has unexpected NAs. ???")
    }
  }
  ## 名前を取得
  asName_Slot <- unlist(lapply(lSLOT, names)) # あとで使います
  if (!is.null(asName_Slot)){
    # もし列名が付いているならば
    # すべての要素にもれなく名前がついている
    stopifnot (length(asName_Slot) == length(unlist(lSLOT)))
    # 重複はない
    stopifnot (anyDuplicated(asName_Slot) == 0)
  }

  ## ここからメイン - - - - - -
  # mbCATへの名前付与
  if (is.null(asName_Cat))
    colnames(mbCAT) <- paste0("Cat_", seq_len(ncol(mbCAT)))

  # lSLOTの修正
  lSLOT <- lapply(
    seq_along(lSLOT),
    function(nCurrentCat){
      out <- lSLOT[[nCurrentCat]]
      # 割付不能カテゴリのスロット割付可能性を欠損にする
      out[mbCAT[, nCurrentCat] == 0, ] <- NA
      # ひとつでも欠損がある行はすべて欠損にする
      out[apply(out, 1, function(x) sum(is.na(x)) > 0),] <- NA
      return(out)
    }
  )

  # lSLOTへの名前付与
  if (is.null(asName_Slot)){
    cat("[makePop] assign colnames to lSLOT ...\n")
    lSLOT <- lapply(
      seq_along(lSLOT),
      function(nCat){
        out <- lSLOT[[nCat]]
        colnames(out) <- paste0("Slot_", nCat, "_", seq_len(ncol(out)))
        return(out)
      }
    )
  }

  ## メッセージ - - - -
  if (sVERBOSE %in% c("simple", "detail")){
    cat("[makePop] # of categories:", ncol(mbCAT), "\n")
    cat("[makePop] # of slots:", paste0(sapply(lSLOT, ncol), collapse = ","), "\n")
    cat("[makePop] # of members:", nrow(mbCAT), "\n")
    nCount <- sum(as.vector(mbCAT))
    gMean  <- nCount / ncol(mbCAT)
    cat("[makePop] # of member-category pairs which are assignable:", nCount, sprintf("(%0.1f/category)", gMean), "\n")
    nCount <- sum(sapply(lSLOT, function(x) sum(as.vector(x), na.rm=T)))
    gMean  <- nCount / sum(sapply(lSLOT, ncol))
    cat("[makePop] # of member-slot pairs which are assignable:", nCount, sprintf("(%0.1f/slot)", gMean), "\n")
  }

  ## 警告 - - - -
  # mbCATにすべて0の列があったら警告する
  if (any(colSums(mbCAT) == 0)){
    warning("[makePop] There is a column whose values are all 0 in mbCAT.")
  }
  # lSLOTにすべて0の列があったら警告する
  if (any(unlist(lapply(lSLOT, function(x) colSums(x, na.rm=TRUE))) == 0)){
    warning("[makePop] There is a column whose values are all 0 in lSLOT.")
  }

  ## 出力 - - - -
  lOut <- list(
    mbCAT = mbCAT,
    lSLOT = lSLOT
  )
  class(lOut) <- "popdata"

  if (sVERBOSE == "detail"){
    cat("[makePop] end.\n")
  }
  return(lOut)
}
makeSetting <- function(
  lSLOT_REQUEST,
  nCAT_MAX,
  nSLOT_MAX,
  sCAT_TYPE     = c("adaptive", "nonadaptive"),
  sCAT_FILTER   = c("all", "open"),
  sCAT_ORDER    = c("random", "openclosed", "shortnum", "shortratio"),
  sCAT_EXCLUDE  = c("none", "allclosed"),
  sSLOT_TYPE    = c("adaptive", "nonadaptive"),
  sSLOT_FILTER  = c("all", "open"),
  sSLOT_ORDER   = c("random", "openclosed", "shortnum", "shortratio"),
  sSLOT_EXCLUDE = c("none", "allclosed"),
  nSUBJECT_MAX  = 0,
  sVERBOSE = c("simple", "none", "detail")
){
  #' make setting
  #'
  #' 割付のセッティングを作成する。
  #'
  #' @export
  #'
  #' @param lSLOT_REQUEST a list of (named) integer vectors.
  #'    各スロットに割り付ける対象者数の目標。
  #'    要素jのベクトルの要素kは, カテゴリjのスロットkに割り付ける対象者の下限を表す。
  #'    欠損不可。
  #'    要素に名前がついている場合、スロット名とみなされるので、
  #'    名前を付けるならばすべての要素に重複なく名前をつけること。
  #' @param nCAT_MAX a integer.
  #'    ある対象者に割り付けるカテゴリ数の上限。
  #' @param sCAT_TYPE a string.
  #'    カテゴリ割付タイプ。詳細はvignetteを参照。
  #' @param sCAT_FILTER a string.
  #'    カテゴリ割付の際の絞り込み条件。詳細はvignetteを参照。
  #' @param sCAT_ORDER a string.
  #'    カテゴリ割付の際の順序付け条件。詳細はvignetteを参照。
  #' @param sCAT_EXCLUDE a string.
  #'    カテゴリ割付の際の除外条件。詳細はvignetteを参照。
  #' @param nSLOT_MAX an integer.
  #'    ある対象者に割り付けるスロット数の上限。
  #' @param sSLOT_TYPE a string.
  #'    スロット割付タイプ。詳細はvignetteを参照。
  #' @param sSLOT_FILTER a string.
  #'    スロット割付の際の絞り込み条件。詳細はvignetteを参照。
  #' @param sSLOT_ORDER a string.
  #'    スロット割付の際の順序付け条件。詳細はvignetteを参照。
  #' @param sSLOT_EXCLUDE a string.
  #'    スロット割付の際の除外条件。指定は必須。詳細はvignetteを参照。
  #' @param nSUBJECT_MAX an integer.
  #'    抽出する対象者数の上限。0以上の整数。
  #'
  #'    0より大きい値が指定された場合、
  #'    対象者抽出は、
  #'    対象者数がnSUBJECT_MAXに達するか、すべてのスロットの割付対象者数が
  #'    lSLOT_REQUESTに設定された目標に達したときに終了する。
  #'
  #'    0が指定された場合、
  #'    対象者抽出は、すべてのスロットの割付対象者数が目標に達したときにのみ正常終了する。
  #'    対象者数が母集団サイズの10倍を超えても
  #'    割付対象者数が目標に到達しなかったときはエラーとなる。
  #' @param sVERBOSE a string.
  #'    画面表示レベル。
  #'
  #' @return an object of `assignsetting` class.
  #'         その実体は、この関数の引数を要素として持つリスト。
  #'         ただし、引数lSLOT_REQUESTの要素に名前がついていない場合は、
  #'         名前"Slot_(j)_(k)"が付与される。

  ## あいさつのため、sVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[makeSetting] start.\n")
  }

  ## 引数チェック - - - - - - -
  ## lSLOT_REQUEST
  ## listである
  stopifnot(is.list(lSLOT_REQUEST))
  ## 欠損はない
  stopifnot( sapply(lSLOT_REQUEST, function(x) sum(is.na(x))) == 0)
  ## 名前を取得
  asName_Slot <- unlist(lapply(lSLOT_REQUEST, names)) # あとで使います
  if (!is.null(asName_Slot)){
    # すべての要素にもれなく名前がついている
    stopifnot (length(asName_Slot) == length(unlist(lSLOT_REQUEST)))
    # 重複はない
    stopifnot (anyDuplicated(asName_Slot) == 0)
  }

  ## nCAT_MAX
  ## 0以上のスカラー
  stopifnot(is.numeric(nCAT_MAX))
  stopifnot(nCAT_MAX > 0)

  ## sCAT_TYPE
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_TYPE) == 1)
  ## あとはチェックなし。推測する
  sCAT_TYPE <- match.arg(sCAT_TYPE)

  ## sCAT_FILTER
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_FILTER) == 1)
  ## あとはチェックなし。推測する
  sCAT_FILTER <- match.arg(sCAT_FILTER)

  ## sCAT_ORDER
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_ORDER) == 1)
  ## あとはチェックなし。推測する
  sCAT_ORDER <- match.arg(sCAT_ORDER)

  ## sCAT_EXCLUDE
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_EXCLUDE) == 1)
  ## あとはチェックなし。推測する
  sCAT_EXCLUDE <- match.arg(sCAT_EXCLUDE)

  ## nSLOT_MAX
  stopifnot(is.numeric(nSLOT_MAX))
  stopifnot(nSLOT_MAX > 0)

  ## sSLOT_TYPE
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_TYPE) == 1)
  ## あとはチェックなし。推測する
  sSLOT_TYPE <- match.arg(sSLOT_TYPE)

  ## sSLOT_FILTER
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_FILTER) == 1)
  ## あとはチェックなし。推測する
  sSLOT_FILTER <- match.arg(sSLOT_FILTER)

  ## sSLOT_ORDER
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_ORDER) == 1)
  ## あとはチェックなし。推測する
  sSLOT_ORDER <- match.arg(sSLOT_ORDER)

  ## sSLOT_EXCLUDE
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_EXCLUDE) == 1)
  ## あとはチェックなし。推測する
  sSLOT_EXCLUDE <- match.arg(sSLOT_EXCLUDE)

  ## nSUBJECT_MAX
  ## とにかく指定していることが必要
  stopifnot(length(nSUBJECT_MAX) == 1)
  ## 整数であること
  stopifnot(nSUBJECT_MAX == as.integer(nSUBJECT_MAX))
  ## 0以上であること
  stopifnot(nSUBJECT_MAX >= 0)

  # メイン - - - - -
  # lSLOT_REQUESTへの名前付与
  if (is.null(asName_Slot)){
    cat("[makeSetting] assign names to lSLOT_REQUEST ...\n")
    lSLOT_REQUEST <- lapply(
      seq_along(lSLOT_REQUEST),
      function(nCat){
        out <- lSLOT_REQUEST[[nCat]]
        names(out) <- paste0("Slot_", nCat, "_", seq_along(out))
        return(out)
      }
    )
  }

  # 出力 - - - - -
  lOut <- list(
    lSLOT_REQUEST = lSLOT_REQUEST,
    nCAT_MAX      = nCAT_MAX,
    sCAT_TYPE     = sCAT_TYPE,
    sCAT_FILTER   = sCAT_FILTER,
    sCAT_ORDER    = sCAT_ORDER,
    sCAT_EXCLUDE  = sCAT_EXCLUDE,
    nSLOT_MAX     = nSLOT_MAX,
    sSLOT_TYPE    = sSLOT_TYPE,
    sSLOT_FILTER  = sSLOT_FILTER,
    sSLOT_ORDER   = sSLOT_ORDER,
    sSLOT_EXCLUDE = sSLOT_EXCLUDE,
    nSUBJECT_MAX  = nSUBJECT_MAX
  )
  class(lOut) <- "assignsetting"

  if (sVERBOSE == "detail"){
    cat("[makeSetting] end.\n")
  }
  return(lOut)
}
sub_makeSubjectCat_Alt <- function(
  mbCAT,
  lSLOT,
  sVERBOSE = c("simple", "none", "detail")
){
  #' Internal: make a table to extract a set of alternative persons from person and category
  #'
  #' Internal: 対象者番号とカテゴリから, 代替する対象者番号群を引く表を作る
  #'           makeSurvey()からコールされる
  #'
  #' @keywords internal
  #'
  #' @param mbCAT 整数行列。
  #'    調査対象者のカテゴリ割付可能性。
  #'    \code{mbCAT[i,j]}は以下を表す。欠損不可。
  #'    \itemize{
  #'    \item 1: 調査対象者iはカテゴリjについて割付可能。
  #'    \item 0: 調査対象者iはカテゴリjについて割付不能。
  #'    }
  #' @param lSLOT 整数行列のリスト。
  #'    調査対象者のスロット割付可能性。
  #'    \code{lSLOT[[j]][i,k]}は以下を表す。
  #'    \itemize{
  #'    \item 1: 調査対象者iはカテゴリjのスロットkについて割付可能。
  #'    \item 0: 調査対象者iはカテゴリjのスロットkについて割付不能。
  #'    }
  #'    欠損についてはDetailsを参照。
  #' @param sVERBOSE a string.
  #'    画面表示レベル。
  #'
  #' @return リスト。以下の要素を持つ。
  #' \itemize{
  #' \item \code{manSubjectCat_AltSet}
  #'       整数ベクトルの行列。行は対象者(mbCATの行),
  #'       列はカテゴリ(mbCATの列), 要素はその対象者のスロット割付可能性を調べるときに
  #'       参照すべき対象者番号のベクトル。mbCAT[i,j] == 1のときは長さ1以上のベクトル、
  #'       mbCAT[i,j] == 0のときはNULL.
  #' \item \code{mnSubjectCat_AltStatus}
  #'       整数ベクトルの行列。行は対象者(mbCATの行),
  #'       列はカテゴリ(mbCATの列), 要素は以下のいずれか:
  #'       \itemize{
  #'         \item NA: 割付不能カテゴリ
  #'         \item 0: スロット割付可能性が既知
  #'         \item 1: スロット割付可能性が未知であるためカテゴリ割付可能性が同じ対象者で代替
  #'         \item 2: スロット割付可能性が未知であるため割付カテゴリ数が同じ対象者で代替
  #'         \item 3: スロット割付可能性が未知であるためランダムに選んだ対象者で代替
  #'       }
  #'  }
  #'
  #' @importFrom magrittr "%>%"
  #' @importFrom tidyr pivot_wider
  #' @importFrom tidyr pivot_longer
  #' @importFrom tidyr complete
  #' @importFrom tidyr nest
  #' @importFrom purrr map
  #' @importFrom dplyr mutate
  #' @importFrom dplyr if_else
  #' @importFrom dplyr row_number

  ## あいさつのため、sVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[sub_makeSubjectCat_Alt] start.\n")
  }

  # mbCATの列名を決めておく
  colnames(mbCAT) <- paste0("bAssignable_", seq_len(ncol(mbCAT)))

  # dfCatGroup: カテゴリ割付可能性からグループ番号を引く表
  dfCatGroup <- as_tibble(unique(mbCAT)) %>%
    mutate(
      nCatGroup = row_number()
    )

  # mbAvailable: スロット割付可能性がわかっているか
  mbAvailable <- matrix(
    sapply(
      lSLOT,
      function(mbSlot) apply(mbSlot, 1, function(x) sum(is.na(x))==0)
    ),
    ncol = ncol(mbCAT)
  )
  colnames(mbAvailable) <- paste0("bAvailable_", seq_len(ncol(mbAvailable)))

  # dfSubjectCat_Status: 行は対象者xカテゴリ
  # {nSubject, nCategory, bAssignable, bAvailable, nNumCat, nCatGroup}
  dfSubjectCat_Status <- as_tibble(cbind(mbCAT, mbAvailable)) %>%
    # nCatGroupをもらってくる
    left_join(dfCatGroup, by = colnames(mbCAT)) %>%
    # nSubject, nNumCatを作成
    mutate(
      nSubject = seq_len(nrow(mbCAT)),
      nNumCat = rowSums(mbCAT)
    ) %>%
    # カテゴリを縦に
    pivot_longer(
      cols = c(starts_with("bAssignable_"), starts_with("bAvailable")),
      names_to = c("sVar", "nCategory"),
      names_sep = "_",
      values_to = "bValue"
    ) %>%
    mutate(nCategory = as.integer(.data$nCategory)) %>%
    pivot_wider(names_from = .data$sVar, values_from = .data$bValue)

  # dfGiver1: カテゴリ割付可能性のグループが同じ人で代替する場合の提供者
  # {nCategory, nCatGroup, lGiver1(要素が対象者番号ベクトルである列)}
  dfGiver1 <- dfSubjectCat_Status %>%
    filter(.data$bAvailable == 1) %>%
    dplyr::select(.data$nCategory, .data$nCatGroup, .data$nSubject) %>%
    group_by(.data$nCategory, .data$nCatGroup) %>%
    nest(oSubject = .data$nSubject) %>%
    ungroup() %>%
    mutate(lGiver1 = map(.data$oSubject, function(x) x$nSubject)) %>%
    dplyr::select(-.data$oSubject) %>%
    complete(.data$nCategory, .data$nCatGroup)
  # print(dfGiver1)

  # dfGiver2: 割付可能カテゴリ数が同じ人で代替する場合の提供者
  # {nCategory, nNumCat, lGiver2(要素が対象者番号ベクトルである列)}
  dfGiver2 <- dfSubjectCat_Status %>%
    filter(.data$bAvailable == 1) %>%
    dplyr::select(.data$nCategory, .data$nNumCat, .data$nSubject) %>%
    group_by(.data$nCategory, .data$nNumCat) %>%
    nest(oSubject = .data$nSubject) %>%
    ungroup() %>%
    mutate(lGiver2 = map(.data$oSubject, function(x) x$nSubject)) %>%
    dplyr::select(-.data$oSubject) %>%
    complete(.data$nCategory, .data$nNumCat)

  # dfGiver2: ランダムに代替する場合の提供者
  # {nCategory, lGiver3(要素が対象者番号ベクトルである列)}
  dfGiver3 <- dfSubjectCat_Status %>%
    filter(.data$bAvailable == 1) %>%
    dplyr::select(.data$nCategory, .data$nSubject) %>%
    group_by(.data$nCategory) %>%
    nest(oSubject = .data$nSubject) %>%
    ungroup() %>%
    mutate(lGiver3 = map(.data$oSubject, function(x) x$nSubject)) %>%
    dplyr::select(-.data$oSubject) %>%
    complete(.data$nCategory)

  dfSubjectCat_Status <- dfSubjectCat_Status %>%
    # lGiver1, lGiver2, lGiver3をもらってくる
    left_join(dfGiver1, by = c("nCategory", "nCatGroup")) %>%
    left_join(dfGiver2, by = c("nCategory", "nNumCat")) %>%
    left_join(dfGiver3, by = c("nCategory")) %>%
    mutate(
      # lGiver1, lGiver2の人数を調べておく
      nNumGiver1 = sapply(.data$lGiver1, length),
      nNumGiver2 = sapply(.data$lGiver2, length),
      # nStatus: NA(カテゴリ割付不能)
      #          0(スロット割付可能性判明),
      #          1(カテゴリ割付可能性グループで補完),
      #          2(割付可能カテゴリ数で補完),
      #          3(ランダムに補完)
      nStatus = as.integer(NA),
      nStatus = if_else(.data$bAssignable == 1 & .data$bAvailable == 1, 0L, .data$nStatus),
      nStatus = if_else(.data$bAssignable == 1 & .data$bAvailable == 0 & .data$nNumGiver1 > 0, 1L, .data$nStatus),
      nStatus = if_else(.data$bAssignable == 1 & .data$bAvailable == 0 & .data$nNumGiver1 == 0 & .data$nNumGiver2 > 0, 2L, .data$nStatus),
      nStatus = if_else(.data$bAssignable == 1 & .data$bAvailable == 0 & .data$nNumGiver1 == 0 & .data$nNumGiver2 == 0, 3L, .data$nStatus),
      # 代替が必要ない場合
      lSelf = as.list(.data$nSubject),
      # 代替を決定する
      lSubject = lapply(seq_along(.data$nSubject), function(x) c()),
      lSubject = if_else(!is.na(.data$nStatus) & .data$nStatus == 0, .data$lSelf, .data$lSubject),
      lSubject = if_else(!is.na(.data$nStatus) & .data$nStatus == 1, .data$lGiver1, .data$lSubject),
      lSubject = if_else(!is.na(.data$nStatus) & .data$nStatus == 2, .data$lGiver2, .data$lSubject),
      lSubject = if_else(!is.na(.data$nStatus) & .data$nStatus == 3, .data$lGiver3, .data$lSubject)
    ) %>%
    dplyr::select(.data$nSubject, .data$nCategory, .data$nStatus, .data$lSubject) %>%
    arrange(.data$nCategory, .data$nSubject)

  manSubjectCat_AltSet <- matrix(
    dfSubjectCat_Status$lSubject,
    nrow = nrow(mbCAT),
    ncol = ncol(mbCAT)
  )
  mnSubjectCat_AltStatus <- matrix(
    dfSubjectCat_Status$nStatus,
    nrow = nrow(mbCAT),
    ncol = ncol(mbCAT)
  )

  lOut <- list(
    manSubjectCat_AltSet = manSubjectCat_AltSet,
    mnSubjectCat_AltStatus = mnSubjectCat_AltStatus
  )

  if (sVERBOSE == "detail"){
    cat("[sub_makeSubjectCat_Alt] end.\n")
  }
  return(lOut)
}
makeSurvey <- function(
  mbCAT,
  lSLOT,
  lSLOT_REQUEST,
  mnASSIGNCAT,
  anPARENTCAT,
  mnASSIGNSLOT,
  sCAT_TYPE     = c("adaptive", "nonadaptive"),
  sCAT_FILTER   = c("all", "open"),
  sCAT_ORDER    = c("random", "openclosed", "shortnum", "shortratio"),
  sCAT_EXCLUDE  = c("none", "allclosed"),
  sSLOT_TYPE    = c("adaptive", "nonadaptive"),
  sSLOT_FILTER  = c("all", "open"),
  sSLOT_ORDER   = c("random", "openclosed", "shortnum", "shortratio"),
  sSLOT_EXCLUDE = c("none", "allclosed"),
  nSUBJECT_MAX  = NULL,
  sVERBOSE = c("simple", "none", "detail")
){
  #' make survey data
  #'
  #' 調査データを作成する.
  #'
  #' @export
  #'
  #' @param mbCAT 整数行列。
  #'    調査対象者のカテゴリ割付可能性。
  #'    \code{mbCAT[i,j]}は以下を表す。欠損不可。
  #'    \itemize{
  #'    \item 1: 調査対象者iはカテゴリjについて割付可能。
  #'    \item 0: 調査対象者iはカテゴリjについて割付不能。
  #'    }
  #'    列に名前を付けるとカテゴリ名とみなされる。
  #'    名前を付けるならばすべての列に重複なく名前をつけること。
  #'
  #' @param lSLOT 整数行列のリスト。
  #'    調査対象者のスロット割付可能性。
  #'    \code{lSLOT[[j]][i,k]}は以下を表す。
  #'    \itemize{
  #'    \item 1: 調査対象者iはカテゴリjのスロットkについて割付可能。
  #'    \item 0: 調査対象者iはカテゴリjのスロットkについて割付不能。
  #'    }
  #'    欠損についてはDetailsを参照。
  #'    列に名前を付けるとスロット名とみなされる。
  #'    名前を付けるならばすべての列に重複なく名前をつけること。
  #'
  #' @param lSLOT_REQUEST 整数ベクトルのリスト。
  #'    各スロットに割り付ける対象者数の下限。
  #'    要素jのベクトルの要素kは, カテゴリjのスロットkに割り付ける対象者の下限を表す。
  #'    欠損なし。
  #'    要素の列名はlSLOTの要素の列名と一致していること。
  #'
  #' @param mnASSIGNCAT 整数行列。
  #'    調査対象者の割付カテゴリ。列数は、割付カテゴリ数の上限。
  #'    \code{mnASSIGNCAT[i,j]}は、調査対象者iのj番目の割付カテゴリ番号(mbCATの列番号)を表す。
  #'    調査対象者iの割付カテゴリ数が列数より小さい場合はNAを埋める。
  #'    列順には意味がない。
  #'
  #' @param anPARENTCAT 整数ベクトル。
  #'    調査対象者の割付スロットが属するカテゴリ。
  #'    \code{anPARENTCAT[i]}は、調査対象者iの割付スロットが属するカテゴリ番号(mbCATの列番号)を表す。
  #'    調査対象者iがスロットに割り付けられなかった場合はNAを埋める。
  #'
  #' @param mnASSIGNSLOT 整数行列。
  #'    調査対象者の割付スロット。列数は、割付スロット数の上限。
  #'    \code{mnASSIGNSLOT[i,j]}は、調査対象者iの割付スロット番号(lSLOT[[anPARENTCAT[i]]]の列番号)を
  #'    表す。調査対象者iの割付スロット数が列数より小さい場合はNAを埋める。
  #'    列順には意味がない。
  #'
  #' @param sCAT_TYPE 文字列。
  #'    カテゴリ割付タイプ。指定は必須。詳細はvignetteを参照。
  #' @param sCAT_FILTER 文字列。
  #'    カテゴリ割付の際の絞り込み条件。指定は必須。詳細はvignetteを参照。
  #' @param sCAT_ORDER 文字列。
  #'    カテゴリ割付の際の順序付け条件。指定は必須。詳細はvignetteを参照。
  #' @param sCAT_EXCLUDE 文字列。
  #'    カテゴリ割付の際の除外条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_TYPE 文字列。
  #'    スロット割付タイプ。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_FILTER 文字列。
  #'    スロット割付の際の絞り込み条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_ORDER 文字列。
  #'    スロット割付の際の順序付け条件。指定は必須。詳細はvignetteを参照。
  #' @param sSLOT_EXCLUDE 文字列。
  #'    スロット割付の際の除外条件。指定は必須。詳細はvignetteを参照。
  #'
  #' @param nSUBJECT_MAX 整数。
  #'    抽出する対象者数の上限。
  #'
  #'   \code{nSUBJECT_MAX}が指定された場合、
  #'    対象者抽出は、
  #'    対象者数がnSUBJECT_MAXに達するか、すべてのスロットの割付対象者数が
  #'    lSLOT_REQUESTに設定された目標に達したときに終了する。
  #'
  #'   \code{nSUBJECT_MAX}がNULLの場合、
  #'    対象者抽出は、すべてのスロットの割付対象者数が目標に達したときにのみ正常終了する。
  #'    対象者数が母集団サイズの10倍を超えても
  #'    割付対象者数が目標に到達しなかったときはエラーとなる。
  #'
  #' @param sVERBOSE 文字列。
  #'    画面表示レベル。
  #'
  #' @details
  #'   \code{lSLOT}の欠損は以下のように扱われる。
  #'   \itemize{
  #'   \item Case 1. \code{mbCAT[i,j] == 0}のとき、
  #'   \code{lSLOT[[j]][i, ]}は無視される。すなわち、
  #'   ある対象者があるカテゴリに対して割付不能な時、
  #'   その対象者のそのカテゴリに属するスロットへの割付可能性は未知として扱われ、
  #'   指定しても無視される。
  #'   \item Case 2. \code{mbCAT[i,j] == 1}であり\code{j} が\code{mnASSIGNCAT[i,]}に含まれているとき、
  #'   \code{lSLOT[[j]][i, ]}の欠損は許されない。すなわち、
  #'   ある対象者があるカテゴリに割り付けられた場合、
  #'   その対象者のそのカテゴリに属するスロットへの割付可能性は既知であるはずなので、
  #'   指定しなければならない。
  #'   \item Case 3. \code{mbCAT[i,j] == 1}であり\code{j} が\code{mnASSIGNCAT[i,]}に含まれていないとき、
  #'   \code{lSLOT[[j]][i, ]}の欠損はある程度まで許容される。すなわち、
  #'   ある対象者があるカテゴリに割付可能であったが割り付けられなかった場合、
  #'   その対象者のそのカテゴリに属するスロットへの割付可能性は、分かっている限りにおいて
  #'   指定すればよい。ある行に欠損がある場合、その行はすべて欠損とみなされる。
  #'   }
  #'
  #'   Case 3.に相当する調査対象者も、
  #'   再割付シミュレーションにおいてカテゴリ j に割り付けられる可能性がある。
  #'   そのため、jに属するスロットへの割付可能性が欠損である調査対象者については、
  #'   それらの欠損を補完する必要が生じる。このとき、
  #'   \code{SurveyAssign}パッケージによるシミュレーションでは、
  #'   調査対象者 i の割付可能性を
  #'   他の調査対象者 i' の割付可能性によって置き換える。
  #'   i' はシミュレーション試行ごとに、次の手順で選ばれる。
  #'   \itemize{
  #'   \item Step 1. jに属するスロットへの割付可能性が既知であり、
  #'   かつJ個のカテゴリへの割付可能性が i と同一である人の中からランダムに選ぶ。
  #'   \item Step 2. みつからなかった場合は、jに属するスロットへの割付可能性が既知であり、
  #'   かつ割付可能なカテゴリの数が同一である人の中からランダムに選ぶ。
  #'   \item Step 3. みつからなかった場合は、jに属するスロットへの割付可能性が既知である
  #'   人の中からランダムに選ぶ。
  #'   }
  #'
  #'   \code{makeSurvey}は、今後のシミュレーションの際に
  #'   上記の手順での補完が可能かどうかをチェックし、Step 1で補完できない人が
  #'   出現した場合には警告を表示する。
  #'
  #' @return `surveydata`クラスのオブジェクト。その実体は以下の要素を持つリスト。
  #' \itemize{
  #' \item \code{mbCAT}         整数行列。引数mbCATの値。
  #'                            列名がついていなかった場合は、
  #'                            列名"Cat_(j)"が付与される。
  #' \item \code{lSLOT}         整数行列のリスト。引数lSLOTの値。ただし、割付不能カテゴリの
  #'                            スロット割付可能性はすべてNAに変更される。また、要素の行にひとつでもNAがあったら
  #'                            その行はみなNAに変更される。
  #'                            列名がついていなかった場合は、
  #'                            列名"Slot_(j)_(k)"が付与される。
  #' \item \code{lSLOT_REQUEST} 整数ベクトルのリスト。引数lSLOT_REQUESTの値。
  #'                            名前がついていなかった場合は、
  #'                            名前"Slot_(j)_(k)"が付与される。
  #' \item \code{sCAT_TYPE}     文字列。引数sCAT_TYPEの値。
  #' \item \code{sCAT_FILTER}   文字列。引数sCAT_FILTERの値。
  #' \item \code{sCAT_ORDER}    文字列。引数sCAT_ORDERの値。
  #' \item \code{sCAT_EXCLUDE}  文字列。引数sCAT_EXCLUDEの値。
  #' \item \code{sSLOT_TYPE}    文字列。引数sSLOT_TYPEの値。
  #' \item \code{sSLOT_FILTER}  文字列。引数sSLOT_FILTERの値。
  #' \item \code{sSLOT_ORDER}   文字列。引数sSLOT_ORDERの値。
  #' \item \code{sSLOT_EXCLUDE} 文字列。引数sSLOT_EXCLUDEの値。
  #' \item \code{mnASSIGNCAT}   整数行列。引数mnASSIGNCATの値。
  #' \item \code{anPARENTCAT}   整数ベクトル。引数anPARENTCATの値。
  #' \item \code{mnASSIGNSLOT}  整数行列。引数mnASSIGNSLOTの値。
  #' \item \code{nSUBJECT_MAX}  整数。引数nSUBJECT_MAXの値。
  #' \item \code{manSubjectCat_AltSubject} 整数ベクトルを要素とする行列。行は対象者(mbCATの行),
  #'                                       列はカテゴリ(mbCATの列),
  #'                                       要素はその対象者のスロット割付可能性を調べるときに
  #'                                       参照すべき対象者番号のベクトル。
  #' \item \code{mnSubjectCat_AltStatus}   整数ベクトルの行列。行は対象者(mbCATの行),
  #'                                       列はカテゴリ(mbCATの列), 要素は以下のいずれか:
  #' \itemize{
  #' \item NA: 割付不能カテゴリ
  #' \item 0: スロット割付可能性が既知
  #' \item 1: スロット割付可能性が未知であるためカテゴリ割付可能性が同じ対象者で代替
  #' \item 2: スロット割付可能性が未知であるため割付カテゴリ数が同じ対象者で代替
  #' \item 3: スロット割付可能性が未知であるためランダムに選んだ対象者で代替
  #' }
  #' }

  ## あいさつのためsVERBOSEのみ先に確定する
  sVERBOSE <- match.arg(sVERBOSE)
  if (sVERBOSE == "detail"){
    cat("[makeSurvey] start.\n")
  }

  ## 引数チェック - - - - - - -
  ## mbCAT
  ## 欠損を含まない
  if (any(is.na(mbCAT)))
    stop("mbCAT should have no NA.")
  ## 値は予期通り
  if (any(!(mbCAT %in% 0:1)))
    stop("The elements of mbCAT should be either 0 or 1.")
  ## 列名を取得する(あとで使います)
  asName_Cat <- colnames(mbCAT)
  if (!is.null(asName_Cat)){
    # もし列名が付いているならば列名はユニーク
    stopifnot(anyDuplicated(asName_Cat) == 0)
  }

  ### lSLOT
  ## 要素数はmbCATの列数と同じ
  if (length(lSLOT) != ncol(mbCAT))
    stop("The length of lSLOT should be equal to the number of columns of mbCAT.")
  ## 各要素の行数はmbCATの行数と同じ
  if (any(sapply(lSLOT, nrow) != nrow(mbCAT)))
    stop("The number of rows of each element in lSLOT should be equal to the number of rows of mbCAT.")
  ## 値は0, 1, NAのいずれか
  for (j in seq_along(lSLOT)){
    abSlot <- as.vector(lSLOT[[j]])
    abInvalid <- !is.na(abSlot) & !(abSlot %in% 0:1)
    if (any( abInvalid )){
      stop("lSLOT[[", j, "]] has unexpected values.")
    }
  }
  ## 名前を取得(あとで使います)
  asName_Slot <- unlist(lapply(lSLOT, names))
  if (!is.null(asName_Slot)){
    # もし列名が付いているならば
    # すべての要素にもれなく名前がついている
    stopifnot (length(asName_Slot) == length(unlist(lSLOT)))
    # 重複はない
    stopifnot (anyDuplicated(asName_Slot) == 0)
  }
  ## スロットに異常な欠損がないかどうかを調べたいのだが、
  ## いまは面倒なので、あとでmbSubjectCat_Assignを作ってからチェックする

  ## lSLOT_REQUEST
  ## 要素数はmbCATの列数と同じ
  if (length(lSLOT_REQUEST) != ncol(mbCAT))
    stop("The length of lSLOT_REQUEST should be equal to the number of columns of mbCAT.")
  ## 各要素の長さはlSLOTの各要素の列数と同じ
  if (any( sapply(lSLOT_REQUEST, length) != sapply(lSLOT, ncol) ))
    stop("The length of each element in lSLOT_REQUEST should be equal to the number of columns of the corresponding element in lSLOT.")
  ## 欠損はない
  if (any(sapply(lSLOT_REQUEST, function(x) sum(is.na(x))) != 0))
    stop("The elements of lSLOT_REQUEST should have no NA.")
  ## 列名はlSLOTと同じ
  stopifnot(unlist(lapply(lSLOT_REQUEST, names)) == asName_Slot)

  ## sCAT_TYPE
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_TYPE) == 1)
  ## あとはチェックなし。推測する
  sCAT_TYPE <- match.arg(sCAT_TYPE)

  ## sCAT_FILTER
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_FILTER) == 1)
  ## あとはチェックなし。推測する
  sCAT_FILTER <- match.arg(sCAT_FILTER)

  ## sCAT_ORDER
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_ORDER) == 1)
  ## あとはチェックなし。推測する
  sCAT_ORDER <- match.arg(sCAT_ORDER)

  ## sCAT_EXCLUDE
  ## とにかく指定していることが必要
  stopifnot(length(sCAT_EXCLUDE) == 1)
  ## あとはチェックなし。推測する
  sCAT_EXCLUDE <- match.arg(sCAT_EXCLUDE)

  ## sSLOT_TYPE
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_TYPE) == 1)
  ## あとはチェックなし。推測する
  sSLOT_TYPE <- match.arg(sSLOT_TYPE)

  ## sSLOT_FILTER
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_FILTER) == 1)
  ## あとはチェックなし。推測する
  sSLOT_FILTER <- match.arg(sSLOT_FILTER)

  ## sSLOT_ORDER
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_ORDER) == 1)
  ## あとはチェックなし。推測する
  sSLOT_ORDER <- match.arg(sSLOT_ORDER)

  ## sSLOT_EXCLUDE
  ## とにかく指定していることが必要
  stopifnot(length(sSLOT_EXCLUDE) == 1)
  ## あとはチェックなし。推測する
  sSLOT_EXCLUDE <- match.arg(sSLOT_EXCLUDE)

  ## mnASSIGNCAT
  ## 行数はmbCATと同じ
  if (nrow(mnASSIGNCAT) != nrow(mbCAT))
    stop("The number of rows of mnASSIGNCAT should be equal to the number of rows of mbCAT.")
  ## 指定されたカテゴリ番号は実在する
  if (any(!(mnASSIGNCAT[!is.na(mnASSIGNCAT)] %in% seq_len(ncol(mbCAT)))))
    stop("Unexpected values in mnASSIGNCAT.")
  for (i in seq_len(nrow(mnASSIGNCAT))){
    x <- mnASSIGNCAT[i,]
    x <- x[!is.na(x)]
    ## 指定されたカテゴリ番号には重複がない
    if (anyDuplicated(x) != 0)
      stop("Duplicated values in mnASSIGNCAT. Check row ", i, ".")
    ## 指定されたカテゴリ番号は割付可能
    if (any( mbCAT[i, x] != 1))
      stop("Unexpected values in mnASSIGNCAT. Check row ", i, ".")
  }

  ## anPARENTCAT
  ## 長さはmnCATと同じ
  if (length(anPARENTCAT) != nrow(mbCAT))
    stop("The length of anPARENTCAT should be equal to the number of rows of mbCAT.")
  ## 指定されたカテゴリ番号は実在する
  if (any(!(anPARENTCAT[!is.na(anPARENTCAT)] %in% seq_len(ncol(mbCAT)))))
    stop("Unexpected values in anPARENTCAT.")
  ## 指定されたカテゴリ番号は割付カテゴリ番号に含まれている
  for (i in seq_len(length(anPARENTCAT))){
    if (!is.na(anPARENTCAT[i]) & !(anPARENTCAT[i] %in% mnASSIGNCAT[i,]))
      stop("Unexpected values in anPARENTCAT. Check the row ", i, ".")
  }

  ## mnASSIGNSLOT
  ## 行数はmbCATと同じ
  if (nrow(mnASSIGNSLOT) != nrow(mbCAT))
    stop("The number of rows of mnASSIGNSLOT should be equal to the number of rows of mbCAT.")
  ## 各行について処理
  for (i in seq_len(nrow(mnASSIGNSLOT))){
    # 親カテゴリと割付スロットをとってくる
    nCat <- anPARENTCAT[i]
    anSlot <- mnASSIGNSLOT[i,]
    anSlot <- anSlot[!is.na(anSlot)]
    # 割付スロットには重複がない
    if (anyDuplicated(anSlot) != 0)
      stop("Duplicated values in mnASSIGNSLOT. Check the row ", i, ".")
    if (!is.na(nCat)){
      # もし親カテゴリが存在したら
      # 指定されたスロット番号は実在する
      if (any(!(anSlot %in% seq_len(ncol(lSLOT[[nCat]])))))
        stop("Unexpected values in mnASSIGNSLOT. Check the row ", i, ".")
      # 指定されたスロット番号が割付可能かどうかはわかっている
      if (any(is.na(lSLOT[[nCat]][i, anSlot])))
        stop("a subject seems to be assigned to a slot whose assignabity is unknown. Check the row ", i, ".")
      if (any(lSLOT[[nCat]][i, anSlot] != 1))
        stop("a subject seems to be assigned to a invalid slot. Check the row ", i, ".")

    } else {
      # もし親カテゴリが存在しなかったら
      # スロット番号は指定されていない
      if (length(anSlot) > 0)
        stop("unexpected values in mnASSIGNSLOT. Check the row ", i, ".")
    }
  }

  ## nSUBJECT_MAX
  stopifnot(is.null(nSUBJECT_MAX) || !is.na(nSUBJECT_MAX))

  ## ここからメイン - - - - - -

  ## mbSubjectCat_Assign: 行に対象者、列にカテゴリ、値は割付有無
  mnSpec <- cbind(
    rep( seq_len(nrow(mbCAT)), ncol(mnASSIGNCAT) ),
    as.vector(mnASSIGNCAT)
  )
  mnSpec <- mnSpec[!is.na(mnSpec[,2]), ]
  mbSubjectCat_Assign <- matrix(0, nrow = nrow(mbCAT), ncol = ncol(mbCAT))
  mbSubjectCat_Assign[mnSpec] <- 1
  # print(mnASSIGNCAT[1:10,])
  # print(mbSubjectCat_Assign[1:10,])
  # stop()

  ## 入力チェック再訪: 割り付けられたカテゴリのスロットに欠損はない
  for (j in seq_along(lSLOT)){
    anNumNA <- apply(lSLOT[[j]], 1, function(x) sum(is.na(x)))
    if (any (mbSubjectCat_Assign[, j] == 1 & anNumNA > 0))
      stop("lSLOT[[", j, "]] has unexpected NA.")
  }

  # mbCATへの名前付与
  if (is.null(asName_Cat))
    colnames(mbCAT) <- paste0("Cat_", seq_len(ncol(mbCAT)))

  ## lSLOTの修正
  lSLOT <- lapply(
    seq_along(lSLOT),
    function(nCurrentCategory){
      out <- lSLOT[[nCurrentCategory]]
      # 割付不能カテゴリのスロット割付可能性をすべてNAにする
      out[mbCAT[, nCurrentCategory] == 0, ] <- NA
      # 行にひとつでもNAがあったらみなNAにする
      out[apply(out, 1, function(x) sum(is.na(x))) > 0, ] <- NA
      return(out)
    }
  )

  # lSLOT, lSLOT_REQUESTへの名前付与
  if (is.null(asName_Slot)){
    lSLOT <- lapply(
      seq_along(lSLOT),
      function(nCat){
        out <- lSLOT[[nCat]]
        colnames(out) <- paste0("Slot_", nCat, "_", seq_len(ncol(out)))
        return(out)
      }
    )
    lSLOT_REQUEST <- lapply(
      seq_along(lSLOT_REQUEST),
      function(nCat){
        out <- lSLOT_REQUEST[[nCat]]
        names(out) <- paste0("Slot_", nCat, "_", seq_along(out))
        return(out)
      }
    )
  }

  # 代替対象者の情報を取得
  lSubjectCat_Alt <- sub_makeSubjectCat_Alt(mbCAT, lSLOT)

  ### メッセージ - - - - -

  if (sVERBOSE %in% c("simple", "detail")){
    cat("[makeSurvey] # of categories:", ncol(mbCAT), "\n")
    cat("[makeSurvey] # of slots:", paste0(sapply(lSLOT, ncol), collapse = ","), "\n")
    cat("[makeSurvey] # subjects:", nrow(mbCAT), "\n")
    cat("[makeSurvey] # of pairs of subject-category:\n")
    cat("[makeSurvey]   total:", nrow(mbCAT) * ncol(mbCAT), "\n")

    nCount <- sum(as.vector(mbCAT))
    gMean <- nCount / ncol(mbCAT)
    cat("[makeSurvey]   assignable:", nCount, sprintf("(%0.1f/category)", gMean), "\n")

    nCount <- sum(as.vector(mnASSIGNCAT), na.rm=T)
    gMean <- nCount / ncol(mbCAT)
    cat("[makeSurvey]   assigned:", nCount, sprintf("(%0.1f/category)", gMean), "\n")

    nCount <- sum(lSubjectCat_Alt$mnSubjectCat_AltStatus > 0, na.rm = TRUE)
    gMean <- nCount /  ncol(mbCAT)
    cat("[makeSurvey]   assignable but assignabilities to slots are unknown:", nCount, sprintf("(%0.1f/category)", gMean), "\n")

    cat("[makeSurvey]     each of these subjects will be complemented by:\n")
    nCount <- sum(lSubjectCat_Alt$mnSubjectCat_AltStatus == 1, na.rm = TRUE)
    cat("[makeSurvey]     a subject who has the same pattern of assignable categories:", nCount, "\n")

    nCount <- sum(lSubjectCat_Alt$mnSubjectCat_AltStatus == 2, na.rm = TRUE)
    cat("[makeSurvey]     a subject who has the same number of assignable categories:", nCount, "\n")

    nCount <- sum(lSubjectCat_Alt$mnSubjectCat_AltStatus == 3, na.rm = TRUE)
    cat("[makeSurvey]     a subject who is randomly selected:", nCount, "\n")

    cat("[makeSurvey] # of pairs of subject-slot:\n")

    nCount <- sum(colSums(mbCAT) * sapply(lSLOT, ncol)) # カテゴリごとに割付可能者数xスロット数を求め合計
    gMean <- nCount / sum(sapply(lSLOT, ncol))
    cat("[makeSurvey]   in assignable categories:", nCount, sprintf("(%0.1f/slot)", gMean), "\n")

    nCount <- sum(sapply(lSLOT, function(x) sum(x, na.rm=T)))
    gMean <- nCount / sum(sapply(lSLOT, ncol))
    cat("[makeSurvey]   known to be assignable:", nCount, sprintf("(%0.1f/slot)", gMean), "\n")

    nCount <- sum(!is.na(mnASSIGNSLOT))
    gMean <- nCount / sum(sapply(lSLOT, ncol))
    cat("[makeSurvey]   assigned:", nCount, sprintf("(%0.1f/slot)", gMean), "\n")
  }

  ### 警告 - - - - -

  # mbCATにすべて0の列があったら警告する
  if (any(colSums(mbCAT) == 0)){
    warning("There is a column whose values are all 0 in mbCAT.")
  }
  # lSLOTにすべて0の列があったら警告する
  if (any(unlist(lapply(lSLOT, function(x) colSums(x, na.rm=TRUE))) == 0)){
    warning("There is a column whose values are all 0 in lSLOT.")
  }

  # スロット割付可能性の代替が難しい箇所があったら警告する
  mnStatus <- lSubjectCat_Alt$dfSubjectCat_AltStatus
  if (any(mnStatus[!is.na(mnStatus)] > 1)){
    warning("There is a subject whose assignabilities to slots are missing and difficult to be complemented.")
  }

  ### 出力 - - - - -

  out <- list(
    mbCAT                  = mbCAT,
    lSLOT                  = lSLOT,
    lSLOT_REQUEST          = lSLOT_REQUEST,
    sCAT_TYPE              = sCAT_TYPE,
    sCAT_FILTER            = sCAT_FILTER,
    sCAT_ORDER             = sCAT_ORDER,
    sCAT_EXCLUDE           = sCAT_EXCLUDE,
    sSLOT_TYPE             = sSLOT_TYPE,
    sSLOT_FILTER           = sSLOT_FILTER,
    sSLOT_ORDER            = sSLOT_ORDER,
    sSLOT_EXCLUDE          = sSLOT_EXCLUDE,
    mnASSIGNCAT            = mnASSIGNCAT,
    anPARENTCAT            = anPARENTCAT,
    mnASSIGNSLOT           = mnASSIGNSLOT,
    nSUBJECT_MAX           = nSUBJECT_MAX,
    manSubjectCat_AltSet   = lSubjectCat_Alt$manSubjectCat_AltSet,
    mnSubjectCat_AltStatus = lSubjectCat_Alt$mnSubjectCat_AltStatus
  )
  class(out) <- "surveydata"

  if (sVERBOSE == "detail"){
    cat("[makeSurvey] end.\n")
  }
  return(out)
}
