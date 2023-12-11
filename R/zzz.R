PACKAGE_NAME <- NULL

#' @importFrom memoise memoise

.onLoad <-
    function(libname, pkgname)
{
    PACKAGE_NAME <<- pkgname
    if (as.logical(Sys.getenv("GRANTPUBCITE_MEMOISE", TRUE))) {
        reporter_endpoint <<- memoise(reporter_endpoint)
        icite_one_chunk <<- memoise(icite_one_chunk)
    }
}
