#' population data
#'
#' vignetteでの説明に用いる母集団データ。
#'
#' 母集団をサイズ10000の有限集団として近似的に表現している。
#' カテゴリ数3, 各カテゴリは10スロットを持つ。
#'
#' 以下の列を持つ。
#' \itemize{
#' \item \code{nID} 母集団メンバーID
#' \item \code{nCat_1} カテゴリ1の割付可能性(0:割付不能, 1:割付可能)
#' \item \code{nCat_2} カテゴリ2の割付可能性(0:割付不能, 1:割付可能)
#' \item \code{nCat_3} カテゴリ3の割付可能性(0:割付不能, 1:割付可能)
#' \item \code{bSlot_1_1} カテゴリ1, スロット1の割付可能性(0:割付不能, 1:割付可能, NA:カテゴリ1は割付不能)
#' \item \code{bSlot_1_2} カテゴリ1, スロット2の割付可能性(0:割付不能, 1:割付可能, NA:カテゴリ1は割付不能)
#' \item ...
#' \item \code{bSlot_3_10} カテゴリ3, スロット10の割付可能性(0:割付不能, 1:割付可能, NA:カテゴリ3は割付不能)
#' }
#'
#' @docType data
#'
#' @usage data(popdata)
#'
#' @keywords datasets
#'
"popdata"
