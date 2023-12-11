#' @importFrom memoise memoise

.onLoad <-
    function(...)
{
    if (as.logical(Sys.getenv("GRANTPUBCITE_MEMOISE", TRUE))) {
        reporter_endpoint <<- memoise(reporter_endpoint)
        icite_one_chunk <<- memoise(icite_one_chunk)
    }
}
