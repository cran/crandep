## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(crandep)
library(dplyr)
library(igraph)

## -----------------------------------------------------------------------------
get_dep("dplyr", "Imports")
get_dep("MASS", c("depends", "suggests"))

## -----------------------------------------------------------------------------
get_dep("xts", "LinkingTo")
get_dep("xts", "linking to")

## -----------------------------------------------------------------------------
get_dep("abc", c("depends", "reverse_depends"))
get_dep("xts", c("linking to", "reverse linking to"))

## ---- echo=FALSE--------------------------------------------------------------
data.frame(from = "A", to = "B", type = "c", reverse = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
data.frame(from = "B", to = "A", type = "c", reverse = TRUE)

## -----------------------------------------------------------------------------
df0.abc <- get_dep("abc", "all")
df0.abc
df0.rstan <- get_dep("rstan", "all") # too many rows to display
dplyr::count(df0.rstan, type, reverse) # hence the summary using count()

## -----------------------------------------------------------------------------
df0.imports <- rbind(
  get_dep("ggplot2", "Imports"),
  get_dep("dplyr", "Imports"),
  get_dep("tidyr", "Imports"),
  get_dep("readr", "Imports"),
  get_dep("purrr", "Imports"),
  get_dep("tibble", "Imports"),
  get_dep("stringr", "Imports"),
  get_dep("forcats", "Imports")
)
head(df0.imports)
tail(df0.imports)

## ---- out.width="660px", out.height="660px", fig.width=12, fig.height=12, fig.show="hold"----
g0.imports <- igraph::graph_from_data_frame(df0.imports)
set.seed(1457L)
old.par <- par(mar = rep(0.0, 4))
plot(g0.imports, vertex.label.cex = 1.5)
par(old.par)

## -----------------------------------------------------------------------------
igraph::is_dag(g0.imports)

## ---- out.width="660px", out.height="660px", fig.width=12, fig.height=12, fig.show="hold"----
df0.nodes <-
  data.frame(
    name = c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats"),
    stringsAsFactors = FALSE
  )
g0.core <- df_to_graph(df0.imports, df0.nodes)
set.seed(259L)
old.par <- par(mar = rep(0.0, 4))
plot(g0.core, vertex.label.cex = 1.5)
par(old.par)

## -----------------------------------------------------------------------------
topo_sort_kahn(g0.core)

## -----------------------------------------------------------------------------
set.seed(387L); topo_sort_kahn(g0.core, random = TRUE)

## -----------------------------------------------------------------------------
df0.topo <- topo_sort_kahn(g0.imports)
head(df0.topo)
tail(df0.topo)

