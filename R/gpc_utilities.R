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

#' @importFrom rjsoncons jmespath
#'
#' @importFrom jsonlite fromJSON
.jmespath <-
    function(x, path)
{
    jmespath(x, path) |> fromJSON()
}

#' @rdname gpc_utilities
#'
#' @title Utilities for 'grantpubcite' use in articles
#'
#' @description `gpc_camelize()` takes an input 'snake_case' vector
#'     and transforms it to CamelCase.
#'
#' @param x a character vector without NA and with all non-zero-number
#'     of letters in each element.
#'
#' @details `gpc_camelize()` is useful in standardizing the user
#'     interface to the API, where the input 'include_fields' are
#'     CamelCase in the API, but the return values are
#'     snake_case. Using `gpc_camelize()` (internally) allows the user
#'     to interact with the API using a consistent `snake_case` style.
#'
#' @examples
#' gpc_camelize(c("full_foa", "core_project_num"))
#'
#' @export
gpc_camelize <-
    function(x)
{
    stopifnot(
        is_character(x)
    )

    ## first letter...
    x <- sub("^([a-z])", "\\U\\1", x, perl = TRUE)
    ## ...any letter after `_`
    gsub("_([a-z])", "\\U\\1", x, perl = TRUE)
}

#' @rdname gpc_utilities
#'
#' @description `gpc_columns_clean()` is used internally to ensure
#'     consistent column formatting.
#'
#' @details `gpc_columns_clean()` performs the following:
#' - Remove leading or trailing whitespace on 'character' columns.
#' - Coerce columns whose name ends with `_date` to `as.Date()`.
#' - Coerce 'pmid', 'year' / 'fiscal year' and 'citation_count' to
#'   integer()
#' - Remove 'group' annotation from tibbles created during
#'   processing.
#'
#' @importFrom dplyr mutate across ends_with ungroup contains
#'
#' @examples
#' projects <- tribble(
#'     ~contact_pi_name, ~project_start_date,    ~pmid, ~project_title,
#'     "Ima Pi ",        "2018-07-15T12:07:00Z", "123", "This is the title",
#'     "Iman Other ",    "2021-01-14T12:01:00Z", "456", "A short title"
#' )
#'
#' clean <-
#'     projects |>
#'     gpc_columns_clean()
#' clean           # no trailing whitespace; dates as Date objects;
#'
#' @export
gpc_columns_clean <-
    function(.data)
{
    stopifnot(
        inherits(.data, "tbl_df")
    )

    .data |>
        ## trim leading training whitespace
        mutate(across(is.character, trimws)) |>
        ## coerce '_date' columns to Date
        mutate(across(ends_with("_date"), as.Date)) |>
        ## coerce 'pmid', 'fiscal_year' / 'year', 'citation_count' to
        ## integer
        mutate(across(
            contains(c(
                "pmid", "year", "fiscal_year", "citation_count"
            )),
            as.integer
        )) |>
        ungroup()
}

#' @rdname gpc_utilities
#'
#' @title Utilities for 'grantpubcite' use in articles
#'
#' @description `gpc_colnames_standardize()` transforms column names
#'     to shorter equivalents, primary useful for (horizontal)
#'     contraction of table displays.
#'
#' @param .data A tibble, usually from an invocation of
#'     `reporter_projects()` or `icite()`. `.data` can also be NULL for
#'     `gpc_colnames_standardize()`, in which case the return value is
#'     a tibble summarizing how column names 'from'
#'     `reporter_projects()` or `icite()` are translated 'to' display
#'     column names.
#'
#' @importFrom dplyr tibble
#'
#' @examples
#' gpc_colnames_standardize()      # summarize column name standardization
#' gpc_colnames_standardize(clean) # shorter column names
#'
#' @export
gpc_colnames_standardize <-
    function(.data = NULL)
{
    stopifnot(
        is.null(.data) || inherits(.data, "tbl_df")
    )

    ## map, entries are `from = "to"`
    map <- c(
        full_foa = "foa",
        core_project_num = "project",
        coreproject = "project",
        project_start_date = "start_date",
        project_end_date = "end_date",
        fiscal_year = "year",
        award_amount = "amount",
        contact_pi_name = "contact_pi",
        project_title = "title",
        citation_count = "citn",
        relative_citation_ratio = "rcr",
        field_citation_rate = "fcr",
        cited_by_core_project_num = "cited_by_project"
    )

    if (is.null(.data)) {
        tibble(from = names(map), to = unname(map))
    } else {
        update <- colnames(.data) %in% names(map)
        colnames(.data)[update] <- map[colnames(.data)[update]]
        .data
    }
}

#' @rdname gpc_utilities
#'
#' @description `gpc_datatable()` standardizes column names for
#'     consistent display. In non-interactive sessions it retured a
#'     `DT::datatable()` for interactive navigation.
#'
#' @details `gpc_datatable()` is a convenience function used in
#'     vignettes to (a) standardize column names for better
#'     formatting; (b) provides a consistent presentation of
#'     DT::datatable without rownames, with pageLength of 5, and
#'     horizontal scrolling, which means that long tables don't
#'     overwhelm articles or spill into margins, and (c) only
#'     transforms the table when not interactive.
#'
#' @details Articles typcially use `gpc_datatable()` in a code chunk
#'     with `include = FALSE`, so that the user sees the datatable but
#'     not code.
#'
#' @examples
#' clean |>             # standarized colnames;
#'     gpc_datatable()  # datatable in non-interactive session
#'
#' @importFrom DT datatable formatStyle
#' @export
gpc_datatable <-
    function(.data, truncate_columns = TRUE, width = 40L)
{
    stopifnot(
        inherits(.data, "tbl_df")
    )

    .data <- gpc_colnames_standardize(.data)

    if (interactive()) {
        .data
    } else {
        columnDefs <- gpc_datatable_columnDefs(.data, truncate_columns, width)
        dt <- datatable(
            .data,
            rownames = FALSE,
            options = list(
                pageLength = 5,
                scrollX = TRUE,
                columnDefs = columnDefs
            )
        )
        ## vertical align all columns at top
        ## https://stackoverflow.com/a/42909391/547331
        dt |>
            formatStyle(seq_along(.data) - 1L, `vertical-align` = "top")
    }
}

#' @rdname gpc_utilities
#'
#' @description `gpc_datatable_download()` adds download buttons to
#'     tables
#'
#' @examples
#' clean |>
#'     gpc_datatable_download()
#'
#' @export
gpc_datatable_download <-
    function(.data, truncate_columns = FALSE, width = 40L)
{
    stopifnot(
        inherits(.data, "tbl_df")
    )

    .data <- gpc_colnames_standardize(.data)

    if (interactive()) {
        .data
    } else {
        columnDefs <- gpc_datatable_columnDefs(.data, truncate_columns, width)
        dt <- datatable(
            .data,
            extensions = "Buttons",
            rownames = FALSE,
            options = list(
                scrollX = TRUE,
                pageLength = 5,
                dom = 'Bftp',
                buttons = c('copy', 'csv', 'excel'),
                columnDefs = columnDefs
            )
        )
        dt |>
            formatStyle(seq_along(.data) - 1L, `vertical-align` = "top")
    }
}

gpc_datatable_columnDefs <-
    function(.data, truncate_columns, width = 40)
{
    ## truncate display of wide columns
    ## see section 4.4 of https://rstudio.github.io/DT/options.html
    truncate <- vapply(
        .data,
        \(x) is.character(x) && any(nchar(x) > width),
        logical(1)
    )
    if (!truncate_columns || !any(truncate))
        return(list(list()))

    javascript <- r"(
        function(data, type, row, meta) {
            test = type === 'display' && data != null && data.length > %d
            return test ?
                '<span title=\"' + data + '\">' + data.substr(0, %d) + '...</span>' :
                data;
        }
    )"

    list(list(
        targets = which(unname(truncate)) - 1L, # no 'row numbers'
        render = JS(sprintf(javascript, width, width))
    ))
}

#' @rdname gpc_utilities
#'
#' @description `gpc_short_title()` shortens titles, e.g., for use as
#'     labels in interactive plots.
#'
#' @param x character(), e.g., `project_title` to be truncated.
#'
#' @param to integer(1) maximum number of characters to retain in
#'     `x`.
#'
#' @examples
#' clean |>
#'     mutate(short_title = gpc_shorten(project_title, 13))
#'
#' @export
gpc_shorten <-
    function(x, to = 40L)
{
    stopifnot(
        is_character(x, allow_NA = TRUE, allow_zchar = TRUE),
        is_scalar_numeric(to)
    )

    to_truncate <- !is.na(x) & nzchar(x)
    x_nzchar <- x[to_truncate]
    truncate <- nchar(x_nzchar) > to
    short <- substr(x_nzchar, 1L, ifelse(truncate, to - 3, to))
    x[to_truncate] <- sprintf("%s%s", short, ifelse(truncate, "...", ""))
    x
}
