is_character <-
    function(x, allow_NA = FALSE, allow_zchar =  FALSE)
{
    is.character(x) &&
        (allow_NA || !any(is.na(x))) &&
        (allow_zchar || all(nzchar(x)))
}

is_numeric <-
    function(x, allow_NA = FALSE)
{
    is.numeric(x) && (allow_NA || !any(is.na(x)))
}

is_scalar_character <-
    function(x, allow_NA = FALSE, allow_zchar = FALSE)
{
    is_character(x, allow_NA, allow_zchar) && length(x) == 1L
}

is_scalar_logical <-
    function(x, allow_NA = FALSE)
{
    is.logical(x) &&
        (allow_NA || !any(is.na(x))) &&
        length(x) == 1L
}

is_scalar_numeric <-
    function(x, allow_NA = FALSE)
{
    is_numeric(x) && length(x) == 1L
}

#' 'include_fields' are for some reason CamelCase in the API, but the
#' return values are snake_case. `camelize()` takes a foo_bar_baz and
#' transforms it to FooBarBaz
#'
#' @noRd
camelize <-
    function(x)
{
    ## first letter...
    x <- sub("^([a-z])", "\\U\\1", x, perl = TRUE)
    ## ...any letter after `_`
    gsub("_([a-z])", "\\U\\1", x, perl = TRUE)
}

#' @importFrom rjsoncons jmespath
#'
#' @importFrom jsonlite fromJSON
.jmespath <-
    function(x, path)
{
    jmespath(x, path) |> fromJSON()
}

#' this is a non-exported convenience function used in the vignette to
#' (a) provides a consistent presentation of DT::datatable without
#' rownames and with pageLength of 5, which means that long tables
#' don't overwhelm articles, and (b) only transforms the table when
#' not interactive, so I can see the standard tibble in my
#' editor. Articles use this as `datatable <-
#' grantpubcite:::datatable()` in a code chunk with `include = FALSE`,
#' so that the user sees only `datatable()` when evaluated
#'
#' @noRd
datatable <-
    function(x, ...)
{
    if (interactive()) {
        x
    } else {
        DT::datatable(
                x, ...,
                rownames = FALSE,
                options = list(
                    pageLength = 5,
                    scrollX = TRUE
                )
            )
    }
}
