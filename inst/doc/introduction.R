## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(crandep)
library(dplyr)
library(ggplot2)
library(igraph)

## -----------------------------------------------------------------------------
get_dep_all("dplyr", "Imports")
get_dep_all("MASS", "depends")
get_dep_all("MASS", "dePends") # should give same result

## -----------------------------------------------------------------------------
get_dep_df("dplyr", c("imports", "LinkingTo"))

## -----------------------------------------------------------------------------
get_dep_all("abc", "depends")
get_dep_all("abc", "reverse_depends")
get_dep_df("abc", c("depends", "reverse_depends"))

## ---- echo=FALSE--------------------------------------------------------------
data.frame(from = "A", to = "B", type = "c", reverse = FALSE)

## ---- echo=FALSE--------------------------------------------------------------
data.frame(from = "B", to = "A", type = "c", reverse = TRUE)

## -----------------------------------------------------------------------------
df0.imports <- rbind(
    get_dep_df("ggplot2", "Imports"),
    get_dep_df("dplyr", "Imports"),
    get_dep_df("tidyr", "Imports"),
    get_dep_df("readr", "Imports"),
    get_dep_df("purrr", "Imports"),
    get_dep_df("tibble", "Imports"),
    get_dep_df("stringr", "Imports"),
    get_dep_df("forcats", "Imports")
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
df0.nodes <- data.frame(name = c("ggplot2", "dplyr", "tidyr", "readr", "purrr", "tibble", "stringr", "forcats"), stringsAsFactors = FALSE)
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

## -----------------------------------------------------------------------------
data(cran_dependencies)
cran_dependencies

## -----------------------------------------------------------------------------
g0.depends <- cran_dependencies %>%
    dplyr::filter(type == "depends" & !reverse) %>%
    df_to_graph(nodelist = dplyr::rename(cran_dependencies, name = from))
g0.rev_depends <- cran_dependencies %>%
    dplyr::filter(type == "depends" & reverse) %>%
    df_to_graph(nodelist = dplyr::rename(cran_dependencies, name = from))
g0.depends
g0.rev_depends

## -----------------------------------------------------------------------------
df1.rev_depends <- cran_dependencies %>%
    dplyr::filter(type == "depends" & reverse) %>%
    df_to_graph(nodelist = NULL, gc = FALSE) %>%
    igraph::as_data_frame() # to obtain the edge list
df1.depends <- cran_dependencies %>%
    dplyr::filter(type == "depends" & !reverse) %>%
    df_to_graph(nodelist = NULL, gc = FALSE) %>%
    igraph::as_data_frame()
dfa.diff.depends <- dplyr::anti_join(
    df1.rev_depends,
    df1.depends,
    c("from" = "to", "to" = "from")
)
head(dfa.diff.depends)

## -----------------------------------------------------------------------------
dfb.diff.depends <- dplyr::anti_join(
    df1.depends,
    df1.rev_depends,
    c("from" = "to", "to" = "from")
)
head(dfb.diff.depends)

## -----------------------------------------------------------------------------
df0.summary <- dplyr::count(cran_dependencies, from, type, reverse)
df0.summary

## -----------------------------------------------------------------------------
df0.summary %>%
    dplyr::filter(reverse) %>%
    dplyr::group_by(type) %>%
    dplyr::top_n(1, n)

## ---- out.width="660px", out.height="660px", fig.width=9, fig.height=9--------
df1.summary <- df0.summary %>%
    dplyr::count(type, reverse, n)
gg0.summary <- df1.summary %>%
    dplyr::mutate(reverse = ifelse(reverse, "reverse", "forward")) %>%
    ggplot2::ggplot() +
    ggplot2::geom_point(ggplot2::aes(n, nn)) +
    ggplot2::facet_grid(type ~ reverse) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(x = "Degree", y = "Number of packages") +
    ggplot2::theme_bw(20)
gg0.summary

