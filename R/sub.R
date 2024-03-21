### sub.R
###   汎用関数
###
getCandidate <- function(
  anCANDIDATE,
  agSORTKEY,
  nSIZE,
  bDEBUG = FALSE
){
  #' Internal: get nSIZE elements from anCANDIDATE in the order of anSORTKEY.
  #' Return all possible combinations if anSORTKEY has tied values.
  #'
  #' Internal: anCANDIDATEからanSORTKEYが小さい順にnSIZE個の要素を取り出す。
  #' anSORTKEYにタイがあるときはありうる取り出し方をすべて返す。
  #' sub_checkcat(), sub_checkslot()からコールされる。
  #'
  #' @keywords internal
  #'
  #' @param anCANDIDATE an integer vector.
  #'    候補。長さ0以上、重複なし。欠損不可。
  #' @param agSORTKEY an numerical vector.
  #'    ソートキー。anCANDIDATEと同じ長さ。タイが許される。欠損不可。
  #' @param nSIZE an integer.
  #'    取り出す要素数。0以上の整数。欠損不可
  #' @param DEBUG as logical.
  #'    デバッグモード
  #'
  #' @return a list.
  #'     要素はある取り出し方による要素ベクトル。長さはnSIZE
  #'
  #' @importFrom utils combn

  if (bDEBUG){
    cat("[getCandidate] start.\n")
  }

  ## 引数チェック - - - - -
  ## anCANDIDATE
  ## 候補に重複はない
  stopifnot(anyDuplicated(anCANDIDATE) == 0)
  ## 欠損無し
  stopifnot(!is.na(anCANDIDATE))

  ## anSORTKEY
  ## 長さanCANDIDATEと同じ
  stopifnot(length(agSORTKEY) == length(anCANDIDATE))
  ## 欠損無し
  stopifnot(!is.na(agSORTKEY))

  ## nSIZE
  ## 0以上
  stopifnot(nSIZE >= 0)
  ## 欠損でない
  stopifnot(!is.na(nSIZE))

  ## ここからメイン - - - -

  if (bDEBUG){
    cat("[getCandidate] anCANDIDATE:", anCANDIDATE, "\n")
    cat("[getCandidate] agSORTKEY:", agSORTKEY, "\n")
    cat("[getCandidate] nSIZE:", nSIZE, "\n")
  }

  ## lCandidate: anCANDIDATEをanSORTKEYが小さい順に分割したリスト
  #              以下、このリストの要素を候補グループと呼ぶ
  lCandidate <- split(anCANDIDATE, rank(agSORTKEY, ties.method = "min"))
  if (bDEBUG){
    cat("[getCandidate] lCandidate:\n")
    print(lCandidate)
  }

  ## anGroup_Size: 候補グループ番号から候補グループのサイズを引くベクトル
  anGroup_Size <- sapply(lCandidate, length)
  ## anGroup_CumSize: 候補グループ番号から候補グループの累積サイズを引くベクトル
  anGroup_CumSize <- cumsum(anGroup_Size)
  ## anGroup_LagCumSize: 候補グループ番号から、その前のグループまでの累積サイズを引くベクトル
  anGroup_LagCumSize <- dplyr::lag(anGroup_CumSize, n = 1L, default = 0L)
  ## anGroup_Expected: 候補グループ番号から、その候補グループから選ぶべき数を引くベクトル
  anGroup_Expected <- ifelse(anGroup_CumSize <= nSIZE, anGroup_Size, pmax(nSIZE - anGroup_LagCumSize, 0))
  if (bDEBUG){
    cat("[getCandidate] anGroup_Expected:", anGroup_Expected, "\n")
  }

  ## lCandidateとanGroup_Expcetedから、必要がない要素を削る
  lCandidate <- lCandidate[anGroup_Expected > 0]
  anGroup_Expected <- anGroup_Expected[anGroup_Expected > 0]

  ## lCombn: 各候補グループのなかで並び替えて行列にする。列がひとつのグループ内組み合わせを表す
  lCombn <- lapply(
    seq_along(lCandidate),
    function(nID){

      if (bDEBUG){
        cat("[getCandidate] nID:", nID, "\n")
        cat("[getCandidate] lCandidate[[nID]]:", lCandidate[[nID]], "\n")
        cat("[getCandidate] anGroup_Expected[nID]:", anGroup_Expected[nID], "\n")
      }

      ## combn(x, m) は、xがベクトルならxからm個を取り出して列にした行列を返すが、
      ## xがスカラーなら seq(x)からm個を取り出して列にした行列を返してしまう
      if (length(lCandidate[[nID]]) == 1){
        stopifnot(anGroup_Expected[nID] == 1)
        out <- as.matrix(lCandidate[[nID]])
      } else {
        out <- combn(lCandidate[[nID]], anGroup_Expected[nID])
      }

      if (bDEBUG){
        cat("[getCandidate] out:\n")
        print(out)
      }
      return(out)
    }
  )
  if (bDEBUG){
    cat("[getCandidate] lCombn:\n")
    print(lCombn)
  }

  ## mnSelectCol: 各候補グループからどのグループ内組み合わせをとってくるか。
  # 行がひとつのグループ間組み合わせを表す
  mnSelectCol <- as.matrix(expand.grid(lapply(lCombn, function(x) seq_len(ncol(x)))))

  ## グループ間組み合わせでループ
  lOut <- lapply(
    seq_len(nrow(mnSelectCol)),
    function(nID){
      # 各候補グループからひとつのグループ内組み合わせを取ってくる
      out <- lapply(
        seq_len(ncol(mnSelectCol)),
        function(nGroup) lCombn[[nGroup]][, mnSelectCol[nID, nGroup]]
      )
      # 結合
      out <- unlist(out)
      return(out)
    }
  )

  if (bDEBUG){
    cat("[getCandidate] lOut:\n")
    print(lOut)
    cat("[getCandidate] end.\n")
  }
  return(lOut)
}
checkDB <- function(
  sDBPATH,
  asTABLE,
  sDIGEST = NULL
){
  #' Internal: check database
  #'
  #' Internal: データベースファイルのチェック
  #'
  #' @keywords internal
  #'
  #' @param sDBPATH 文字列。データベースファイルへのパス
  #' @param asTABLE 文字列ベクトル。データベースのテーブル名。
  #' @param sDIGEST 文字列。ダイジェスト。nullの場合はチェックしない
  #'
  #' @return 論理値。以下の条件を満たしたときにTRUE, そうでないときにFALSEを返す。
  #' \itemize{
  #' \item データベースファイルsDBPATHが実在する
  #' \item そこにasTABLEとテーブル"digest"が含まれる
  #' \item (sDIGESTが指定された場合のみ) その列の値はsDIGESTに一致する
  #' }

  out <- FALSE

  # ファイルが存在したときのみ処理
  if (file.exists(sDBPATH)){
    # DBに接続
    con <- dbConnect(SQLite(), sDBPATH)
    # この関数を終えるとき、DBとの接続を切るように依頼
    on.exit(dbDisconnect(con))
    # テーブル名を取得する
    asTable <- dbListTables(con)
    # 期待される"テーブルが存在したときのみ処理
    if (all(c("digest", asTABLE) %in% asTable)){
      if (!is.null(sDIGEST)){
        # sDIGESTが指定された場合
        # ダイジェストを取得する
        sDBDigest <- tbl(con, "digest") %>% pull(.data$digest)
        # ダイジェストが一致したらTRUE
        if (sDIGEST == sDBDigest){
          out <- TRUE
        }
      } else {
        # sDIGESTが指定されなかった場合
        # TRUE
        out <- TRUE
      }
    }
  }

  return(out)
}
ignore_unused_imports <- function(){
  # https://r-pkgs.org/dependencies-in-practice.html
  dbplyr::tbl_lazy
}
