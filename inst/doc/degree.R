## -----------------------------------------------------------------------------
library(crandep)
library(igraph)
library(dplyr)
library(ggplot2)
data(cran_dependencies)

## -----------------------------------------------------------------------------
g0.depends <- cran_dependencies %>%
    dplyr::filter(type == "depends" & !reverse) %>%
	df_to_graph(nodelist = dplyr::rename(cran_dependencies, name = from))
d0.depends <- g0.depends %>% igraph::degree(mode = "in")
df0.depends <-
    data.frame(name = names(d0.depends), degree = as.integer(d0.depends)) %>%
    dplyr::arrange(dplyr::desc(degree), name)
head(df0.depends, 10)

## -----------------------------------------------------------------------------
g0.rev_depends <- cran_dependencies %>%
    dplyr::filter(type == "depends" & reverse) %>% # note the difference to above
	df_to_graph(nodelist = dplyr::rename(cran_dependencies, name = from))
d0.rev_depends <- g0.rev_depends %>% igraph::degree(mode = "out") # note the difference to above
df0.rev_depends <-
    data.frame(name = names(d0.rev_depends), degree = as.integer(d0.rev_depends)) %>%
    dplyr::arrange(dplyr::desc(degree), name)
head(df0.rev_depends, 10)
identical(df0.depends, df0.rev_depends) # expected TRUE

## -----------------------------------------------------------------------------
df1.depends <- df0.depends %>%
	dplyr::filter(degree > 0L) %>% # to prevent warning when plotting on log-log scale
    dplyr::count(degree, name = "frequency") %>%
	dplyr::arrange(dplyr::desc(degree)) %>%
	dplyr::mutate(survival = cumsum(frequency)/sum(frequency))

## -----------------------------------------------------------------------------
gg0 <- df1.depends %>%
	ggplot2::ggplot() +
	ggplot2::geom_point(aes(degree, frequency), size = 0.75) +
	ggplot2::scale_x_log10() +
	ggplot2::scale_y_log10() +
	ggplot2::coord_cartesian(ylim = c(1L, 1e+3L)) +
	ggplot2::theme_bw(12)
gg0

## ----results = FALSE----------------------------------------------------------
x <- dplyr::filter(df0.depends, degree > 0L)$degree # data
u <- 1L # threshold
xi1 <- 1.0 # initial value
a_xi1 <- 0.0 # lower bound of uniform distribution
b_xi1 <- 100.0 # upper bound of uniform distribution
set.seed(3075L)
mcmc0.depends <- mcmc_upp(x = x, u = u, xi1 = xi1, a_xi1 = a_xi1, b_xi1 = b_xi1) # takes seconds

## -----------------------------------------------------------------------------
mcmc0.depends %>%
    ggplot2::ggplot() +
	ggplot2::geom_density(aes(xi1)) +
	ggplot2::theme_bw(12)

## -----------------------------------------------------------------------------
mcmc0.depends <- mcmc0.depends %>%
    dplyr::mutate(alpha = 1.0 / xi1 + 1.0)
mcmc0.depends %>%
	ggplot2::ggplot() +
	ggplot2::geom_density(aes(alpha)) +
	ggplot2::theme_bw(12)

## -----------------------------------------------------------------------------
n0 <- length(x) ## or sum(df1.depends$frequency)
freq0 <- n0 * dupp(x = df1.depends$degree, u = 1L, xi1 = mean(mcmc0.depends$xi1))
surv0 <- Supp(x = df1.depends$degree, u = 1L, xi1 = mean(mcmc0.depends$xi1))
df1.depends <- df1.depends %>%
    dplyr::mutate(frequency.fitted = freq0, survival.fitted = surv0)

## -----------------------------------------------------------------------------
gg1 <- df1.depends %>%
    ggplot2::ggplot() +
	ggplot2::geom_point(aes(degree, frequency), size = 0.75) +
	ggplot2::geom_line(aes(degree, frequency.fitted), col = 2, lty = 2) +
	ggplot2::scale_x_log10() +
	ggplot2::scale_y_log10() +
	ggplot2::coord_cartesian(ylim = c(1L, 1e+3L)) +
	ggplot2::theme_bw(12)
gg1

## -----------------------------------------------------------------------------
gg2 <- df1.depends %>%
    ggplot2::ggplot() +
	ggplot2::geom_point(aes(degree, survival), size = 0.75) +
	ggplot2::geom_line(aes(degree, survival.fitted), col = 2, lty = 2) +
	ggplot2::scale_x_log10() +
	ggplot2::scale_y_log10() +
	ggplot2::theme_bw(12)
gg2

