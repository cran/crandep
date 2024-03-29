#' @rdname get_dep
#' @export
get_dep_df <- function(name, type, scrape = TRUE) {
    .Deprecated("get_dep")
    get_dep(name, type, scrape)
}

#' @rdname get_dep
#' @export
get_dep_all <- function(name, type, scrape = TRUE) {
    .Deprecated("get_dep")
    get_dep(name, type, scrape)
}

#' Multiple types of dependencies
#'
#' \code{get_dep} returns a data frame of multiple types of dependencies of a package
#' @param name String, name of the package
#' @param type A character vector that contains one or more of the following dependency words: "Depends", "Imports", "LinkingTo", "Suggests", "Enhances", "Reverse depends", "Reverse imports", "Reverse linking to", "Reverse suggests", "Reverse enhances", up to letter case and space replaced by underscore. Alternatively, if 'type = "all"', all ten dependencies will be obtained.
#' @param scrape Boolean. If 'TRUE' (default), the page of the package will be scraped. If 'FALSE', tools::CRAN_package_db() will be used. Whether the argument equals 'TRUE' or 'FALSE' should not affect the output, but only the time taken. Usually, the former is faster than the latter for a single package.
#' @importFrom tools CRAN_package_db
#' @importFrom stringr str_detect
#' @return A data frame of dependencies
#' @examples
#' get_dep("dplyr", c("Imports", "Depends"))
#' get_dep("MASS", c("Suggests", "Depends", "Imports"), TRUE) # FALSE will give same result
#' @seealso \code{\link{get_dep_all_packages}} for the dependencies of all CRAN packages, and \code{\link{get_graph_all_packages}} for obtaining directly a network of dependencies as an igraph object
#' @export
get_dep <- function(name, type, scrape = TRUE) {
    type <- check_dep_word(type)
    l0 <- list()
    if (scrape) {
        html0 <- html_text_vec(cran_url(name))
        for (i in seq_along(type)) {
            typei <- type[i]
            v0 <- get_dep_vec(get_dep_str(html0, typei))
            l0[[i]] <- data.frame(from = name, to = v0, type = typei, stringsAsFactors = FALSE)
        }
    } else {
        db1 <- try(tools::CRAN_package_db(), silent = TRUE)
        if (inherits(db1, "try-error")) {
            stop("get_dep() uses tools::CRAN_package_db() which fails. Check Internet connection.")
        } else {
            df1 <- as.data.frame(db1)
            for (i in seq_along(type)) {
                typei <- type[i]
                str0 <- df1[df1$Package == name, typei]
                v0 <- get_dep_vec(str0[1L]) # some packages have multiple rows
                l0[[i]] <- data.frame(from = name, to = v0, type = typei, stringsAsFactors = FALSE)
            }
        }
    }
    df0 <- do.call(rbind, l0)
    df0 <- df0[!is.na(df0$to),]
    df0$type <- tolower(df0$type)
    df0$type <- ifelse(df0$type == "linkingto", "linking to", df0$type)
    df0$reverse <- stringr::str_detect(df0$type, "reverse ")
    df0$type <- ifelse(df0$reverse, substr(df0$type, 9L, nchar(df0$type)), df0$type)
    unique(df0) # there are duplicates
}

#' Reshape the data frame of dependencies
#' 
#' @param x A character vector of dependencies, each element of which corresponds to an individual package
#' @param names A character vector of package names of the same length as x 
#' @return A data frame of dependencies
#' @keywords internal
reshape_dep <- function(x, names) {
    y <- lapply(x, get_dep_vec)
    df0 <- data.frame(
        from = rep(names, sapply(y, length)),
        to = unlist(y),
        stringsAsFactors = FALSE
    )
}

#' Dependencies of all CRAN packages
#'
#' \code{get_dep_all_packages} returns the data frame of dependencies of all packages currently available on CRAN.
#'
#' Unlike \code{get_dep}, there is no boolean argument `scrape`, as it is much faster to obtain the dependencies of all packages via `tools::CRAN_package_db()`.
#' @importFrom tools CRAN_package_db
#' @importFrom dplyr bind_rows
#' @return A data frame of dependencies of all CRAN packages
#' @examples
#' \dontrun{
#' df.cran <- get_dep_all_packages()
#' }
#' @seealso \code{\link{get_dep}} for multiple types of dependencies, and \code{\link{get_graph_all_packages}} for obtaining directly a network of dependencies as an igraph object
#' @export
get_dep_all_packages <- function() {
    db0 <- try(tools::CRAN_package_db(), silent = TRUE)
    if (inherits(db0, "try-error")) {
        stop("get_dep_all_packages() uses tools::CRAN_package_db() which fails. Check Internet connection.")
    } else {
        df0 <- as.data.frame(db0, stringsAsFactors = FALSE)
        pkgnames <- df0$Package
        df1 <- dplyr::bind_rows(
                          `FALSE` = dplyr::bind_rows(
                                               imports = reshape_dep(df0$Imports, pkgnames),
                                               depends = reshape_dep(df0$Depends, pkgnames),
                                               `linking to` = reshape_dep(df0$LinkingTo, pkgnames),
                                               suggests = reshape_dep(df0$Suggests, pkgnames),
                                               enhances = reshape_dep(df0$Enhances, pkgnames),
                                               .id = "type"
                                           ),
                          `TRUE` = dplyr::bind_rows(
                                              imports = reshape_dep(df0$`Reverse imports`, pkgnames),
                                              depends = reshape_dep(df0$`Reverse depends`, pkgnames),
                                              `linking to` = reshape_dep(df0$`Reverse linking to`, pkgnames),
                                              suggests = reshape_dep(df0$`Reverse suggests`, pkgnames),
                                              enhances = reshape_dep(df0$`Reverse enhances`, pkgnames),
                                              .id = "type"
                                          ),
                          .id = "reverse"
                      )
        df2 <- data.frame(
            from = df1$from,
            to = df1$to,
            type = df1$type,
            reverse = as.logical(df1$reverse),
            stringsAsFactors = FALSE
        )
        df2 <- df2[!is.na(df2$to),]
        df2 <- df2[df2$to != "",] # there are rows with "" - may need to fix from source get_dep_vec()
        return(unique(df2))
    }
}
