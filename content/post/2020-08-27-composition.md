---
title: Composition
author: technocrat
date: '2020-08-27'
slug: composition
categories:
  - R
  - Programming
tags:
  - design
  - heuristics
---

Just as visualizing tensors degrades as $n > 3$, juggling pieces in an `R` script can get jiggly when they do not `%>%` neatly. A [post](https://community.rstudio.com/t/appending-dataframes-to-list-named-by-variable/77689/5) recently reminded me so.

Despite strides in deconstructing `R` to make it more commercially attractive no obvious point (aside from the richness of statistical subroutines) in using it appears if its `functional` nature is neglected.

If conceptualized as school algebra ($f(x)=y$) writ large, every discrete `R` tasks is to find or compose $f$.

``` r
# https://community.rstudio.com/t/appending-dataframes-to-list-named-by-variable/77689/5
suppressPackageStartupMessages({library(dplyr)
                                library(purrr)
                                library(tidyr)
                              })
f <- function(x) {
  map(1:length(mk_vs()),sub_df) %>% set_names(.,mk_names(mk_vs(),x))
}

mk_synth_df <- function() {
  set.seed(137)
  ID <- sample(1:100, 50, replace=TRUE)
  N <- sample(1:100, 50, replace = TRUE)
  ratio <- rnorm(50, mean = 0.5, sd=0.1)
  data.frame(ID,N,ratio)
}

mk_list <- function() {
  set.seed(137)
  list("item1" = sample(1:100, 100), "item2" = sample(1:100, 100))
}

mk_vs <- function() {
  set.seed(137)
  v1 <- sample(1:100, 10, replace = TRUE)
  v2 <- sample(1:100, 10, replace = TRUE)
  v3 <- sample(1:100, 10, replace = TRUE)
  list("res1" = v1, "res2" = v2, "res3" = v3)
}

mk_names <- function(x,y) {
  paste(names(x)[1:length(x)],y, sep="")
}

sub_df <- function(x) {
  drop_na(mk_synth_df()[mk_vs()[[x]],])
}

f("_dFList")
#> $res1_dFList
#>    ID  N     ratio
#> 34 47 27 0.4427454
#> 8  96 22 0.5997265
#> 38 71 44 0.6615295
#> 39 14 83 0.4865537
#> 35 15 59 0.4820364
#> 
#> $res2_dFList
#>    ID  N     ratio
#> 30 35 89 0.6159864
#> 48 38 43 0.3970454
#> 13 13 21 0.4866509
#> 43 51 14 0.4953379
#> 14 43 55 0.5479219
#> 22 48 96 0.5725216
#> 4  38 44 0.4549781
#> 6  39 25 0.7056911
#> 
#> $res3_dFList
#>      ID  N     ratio
#> 15   14 89 0.5237554
#> 48   38 43 0.3970454
#> 32   86 89 0.5995506
#> 13   13 21 0.4866509
#> 14   43 55 0.5479219
#> 35   15 59 0.4820364
#> 35.1 15 59 0.4820364
```

<sup>Created on 2020-08-27 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>
