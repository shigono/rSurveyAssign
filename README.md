
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rSurveyAssign: tools for simulation of assignment in web surveys

<!-- badges: start -->
<!-- badges: end -->

This package contains several tools to simulate different types of
assignment processes in web surveys. With this package, users can
estimate sample sizes and for future surveys, assess bias in future
surveys, and calculate survey weights for actual surveys that have been
conducted.

このパッケージは、web調査における対象者割付についてのシミュレーションを行うためのパッケージです。これから行う調査について標本サイズを見積もったり、バイアスを評価したり、すでに行った調査について調査ウェイトを求めたりすることができます。

### インストール

GitHubからインストールできます。

``` r
# install.packages("devtools")
devtools::install_github("shigono/rSurveyAssign", build_manual = TRUE, build_vignettes = TRUE)
```

### 使い方

vignetteをご覧ください。

``` r
vignette(topic = "vignette", package = "rSurveyAssign")
```

vignetteは
[パッケージのwebサイト](https://shigono.github.io/rSurveyAssign/)
でも公開されています。 メニューの”Articles”の下にある[rSurveyAssign:
tools for simulation of assignment in web
surveys](https://shigono.github.io/rSurveyAssign/articles/vignette.html)をご覧ください。
