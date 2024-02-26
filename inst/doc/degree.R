## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message = FALSE---------------------------------------------------
library(crandep)
library(igraph)
library(dplyr)
library(ggplot2)

## -----------------------------------------------------------------------------
g0.imports <- get_graph_all_packages(type = "imports")
d0.imports <- g0.imports |> igraph::degree(mode = "in")
df0.imports <-
  data.frame(name = names(d0.imports), degree = as.integer(d0.imports)) |>
  dplyr::arrange(dplyr::desc(degree), name)
head(df0.imports, 10)

## -----------------------------------------------------------------------------
g0.rev_imports <- get_graph_all_packages(type = "reverse imports")
d0.rev_imports <- g0.rev_imports |> igraph::degree(mode = "out") # note the difference to above
df0.rev_imports <-
  data.frame(name = names(d0.rev_imports), degree = as.integer(d0.rev_imports)) |>
  dplyr::arrange(dplyr::desc(degree), name)
head(df0.rev_imports, 10)

## -----------------------------------------------------------------------------
identical(df0.imports, df0.rev_imports)
setdiff(df0.imports, df0.rev_imports)
setdiff(df0.rev_imports, df0.imports)

## -----------------------------------------------------------------------------
df1.imports <- df0.imports |>
  dplyr::filter(degree > 0L) |> # to prevent warning when plotting on log-log scale
  dplyr::count(degree, name = "frequency") |>
  dplyr::arrange(degree) |>
  dplyr::mutate(survival = 1.0 - cumsum(frequency) / sum(frequency))

## -----------------------------------------------------------------------------
plot_log_log <- function(df) {
  ## useful function for below
  df |>
    ggplot2::ggplot() +
    ggplot2::scale_x_log10() +
    ggplot2::scale_y_log10() +
    ggplot2::theme_bw(12)
}
gg0 <- df1.imports |>
  plot_log_log() +
  ggplot2::geom_point(aes(degree, frequency), size = 0.75) +
  ggplot2::coord_cartesian(ylim = c(1L, 2e+3L))
gg0

## ----results = FALSE----------------------------------------------------------
x <- df1.imports$degree
counts <- df1.imports$frequency
alpha0 <- 1.5 # initial value
theta0 <- 1.0 # initial value, but fixed to 1.0 for discrete power law
m_alpha <- 0.0 # prior mean
s_alpha <- 10.0 # prior standard deviation
set.seed(3075L)
mcmc0.imports <-
  mcmc_pol(
    x = x,
    count = counts,
    alpha = alpha0,
    theta = theta0,
    a_alpha = m_alpha,
    b_alpha = s_alpha,
    a_theta = 1.0,
    b_theta = 1.0,
    a_pseudo = 10.0,
    b_pseudo = 1.0,
    pr_power = 1.0,
    iter = 2500L,
    thin = 1L,
    burn = 500L,
    freq = 500L,
    invt = 1.0
  ) # takes seconds

## -----------------------------------------------------------------------------
mcmc0.imports$pars |>
  ggplot2::ggplot() +
  ggplot2::geom_density(aes(alpha)) +
  ggplot2::theme_bw(12)

## -----------------------------------------------------------------------------
marg0.imports <-
  marg_pow(
    data.frame(x = x, count = counts),
    lower = 1.001,
    upper = 20.0,
    m_alpha = m_alpha,
    s_alpha = s_alpha
  )
ggplot2::last_plot() +
  ggplot2::geom_line(ggplot2::aes(alpha, density), marg0.imports$posterior, col = 2) +
  ggplot2::coord_cartesian(xlim = range(mcmc0.imports$pars$alpha))

## -----------------------------------------------------------------------------
n0 <- sum(df1.imports$frequency) # TOTAL number of data points in x
## or n0 <- length(x)
df0.fitted <- mcmc0.imports$fitted
gg1 <- df1.imports |>
  plot_log_log() +
  ggplot2::geom_point(aes(degree, frequency), size = 0.75) +
  ggplot2::geom_line(aes(x, f_med * n0), data = df0.fitted, col = 4, lty = 2) +
  ggplot2::geom_line(aes(x, f_025 * n0), data = df0.fitted, col = 2, lty = 3) +
  ggplot2::geom_line(aes(x, f_975 * n0), data = df0.fitted, col = 2, lty = 3) +
  ggplot2::coord_cartesian(ylim = c(1L, 2e+3L))
gg1

## -----------------------------------------------------------------------------
gg2 <- df1.imports |>
  plot_log_log() +
  ggplot2::geom_point(aes(degree, survival), size = 0.75) +
  ggplot2::geom_line(aes(x, S_med), data = df0.fitted, col = 4, lty = 2) +
  ggplot2::geom_line(aes(x, S_025), data = df0.fitted, col = 2, lty = 3) +
  ggplot2::geom_line(aes(x, S_975), data = df0.fitted, col = 2, lty = 3)
gg2

## -----------------------------------------------------------------------------
gg3 <- df1.imports |>
  filter(degree > 1L) |>
  dplyr::mutate(
    survival = 1.0 - cumsum(frequency) / sum(frequency),
    survival.mix = Smix2(degree, 1606, 1.73, 1.00, 0.237, 4.03, 0.003)
  ) |>
  ggplot2::ggplot() +
  ggplot2::geom_point(aes(degree, survival), size = 0.75) +
  ggplot2::geom_line(aes(degree, survival.mix), col = 4, lty = 2, lwd = 1.2) +
  ggplot2::scale_x_log10() +
  ggplot2::scale_y_log10() +
  ggplot2::theme_bw(12)
gg3

