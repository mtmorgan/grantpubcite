##
## NIH reporter: https://api.reporter.nih.gov/
##

REPORTER_URL <- "https://api.reporter.nih.gov"

REPORTER_PROJECTS <- paste0(REPORTER_URL, "/v2/projects/search")

REPORTER_PUBLICATIONS <- paste0(REPORTER_URL, "/v2/publications/search")

REPORTER_LIMIT <- 10000L

#' @importFrom jsonlite unbox toJSON fromJSON
#'
#' @importFrom dplyr as_tibble select all_of bind_rows
reporter_endpoint <-
    function(url, ..., include_fields, limit, verbose)
{
    stopifnot(
        `all '...' arguments must be named` =
            (length(list(...)) == 0L) || is_character(names(list(...))),
        is.null(include_fields) || is_character(include_fields),
        is.null(limit) || is_scalar_numeric(limit),
        is_scalar_logical(verbose)
    )

    query <- c(
        list(criteria = list(...)),
        if (!is.null(include_fields))
            list(include_fields = camelize(include_fields))
    )

    offset <- 0L
    page_limit <- as.integer(min(limit, 500L))
    result <- NULL
    repeat {
        ## page through results
        full_query <-
            c(
                query,
                list(offset = unbox(offset), limit = unbox(page_limit))
            ) |>
            toJSON()
        if (verbose && offset == 0L)
            message(full_query)

        response <- post(url, full_query, httr::content_type_json())
        if (verbose) {
            txt <- paste0(
                "fields in first record: ",
                paste(.jmespath(response, "keys(results[0])"), collapse = ", ")
            )
            if (offset == 0L)
                message(paste(strwrap(txt, exdent = 4L), collapse = "\n"))
            message(
                offset + .jmespath(response, "length(results)"), " of ",
                .jmespath(response, "meta.total"), " records"
            )
        }
        too_many_records <-
            ## first iteration only
            offset == 0L &&
            .jmespath(response, "meta.total") > REPORTER_LIMIT
        if (too_many_records) {
            txt <- paste0(
                "More than ", REPORTER_LIMIT, " records would be returned by ",
                "this query; a likely cause is that the argument names do not ",
                "match allowed criteria defined in the API documentation. ",
                if (!verbose)
                    "Using `verbose = TRUE` may help to understand the query."
            )
            stop(
                paste(strwrap(txt, exdent = 4L), collapse = "\n"),
                call. = FALSE
            )
        }

        total <- .jmespath(response, "meta.total")
        tbl <- fromJSON(response)$results |> as_tibble()
        if (!is.null(include_fields))
            tbl <- select(tbl, all_of(include_fields))
        result <- bind_rows(result, tbl)

        if (NROW(result) == min(limit, total))
            break
        offset <- offset + page_limit
    }

    result
}

#' @rdname user_interface
#'
#' @title Query projects, publications and citations
#'
#' @details The NIH reporter API used for `reporter_projects()` and
#'     `reporter_publications()` is documented at
#'     \url{https://api.reporter.nih.gov/}.
#'
#' @param ... named arguments descrbing fields in the query. Values
#'     are from the 'Schema' linked to the API description.
#'
#' @param include_fields `character()` of fields to include. The
#'     default (null) returns all available fields.
#'
#' @param limit integer(1) return a maximum of `limit` records
#'     matching search criteria.
#'
#' @param verbose logical(1) report JSON used in search criteria, and
#'     a summary of responses prior to processing to their final
#'     `tibble` representation.
#'
#' @return `reporter_projects()` returns a tibble with selected
#'     columns. Available columns are described in the schema
#'     available on the API documentation page.
#'
#' @examples
#' foas <- c(        # one or more criteria, e.g., foa number(s)
#'     "PAR-15-334", # ITCR (R21)”
#'     "PAR-15-332", # ITCR Early-Stage Development (U01)
#'     "PAR-15-331", # ITCR Advanced Development (U24)
#'     "PAR-15-333"  # ITCR Sustained Support (U24)
#' )
#'
#' ## use `limit = 1` to see possible values for fields to be included
#' reporter_projects(foa = foas, limit = 1L) |>
#'     glimpse()
#'
#' ## select fields of interest
#' include_fields <- c(
#'     "full_foa",
#'     "core_project_num",
#'     "fiscal_year",
#'     "award_amount",
#'     "contact_pi_name",
#'     "project_title",
#'     "project_start_date",
#'     "project_end_date"
#' )
#' projects <- reporter_projects(foa = foas, include_fields = include_fields)
#' projects
#'
#' @export
reporter_projects <-
    function(..., include_fields = NULL, limit = NULL, verbose = FALSE)
{
    reporter_endpoint(
        REPORTER_PROJECTS, ..., include_fields = include_fields,
        limit = limit, verbose = verbose
    )
}

#' @rdname user_interface
#'
#' @return `reporter_publications()` returns a tibble with columns
#'     `coreproject`, `pmid`, and `applid`.
#'
#' @examples
#' core_project_nums <- pull(projects, "core_project_num")
#' publications <- reporter_publications(core_project_nums = core_project_nums)
#' publications
#'
#' @export
reporter_publications <-
    function(..., limit = NULL, verbose = FALSE)
{
    reporter_endpoint(
        REPORTER_PUBLICATIONS, ..., include_fields = NULL,
        limit = limit, verbose = verbose
    )
}

##
## iCite: https://icite.od.nih.gov/api.
##

ICITE_URL <- "https://icite.od.nih.gov/api"

ICITE_PUBS <- paste0(ICITE_URL, "/pubs")

#' @importFrom readr read_csv

icite_one_chunk <-
    function(pmids, include_fields, verbose)
{
    ## formulate query
    fl <- ifelse (is.null(include_fields), "", "&fl=")
    query <- paste0(
        "?pmids=", paste0(pmids, collapse = ","),
        fl, paste0(include_fields, collapse = ","),
        "&format=csv"
    )

    url <- paste0(ICITE_PUBS, query)
    content <- get(url)

    tbl <- read_csv(content, show_col_types = FALSE)
    if (!is.null(include_fields)) # order results
        tbl <- select(tbl, all_of(include_fields))

    tbl
}

#' @rdname user_interface
#'
#' @details The `icite()` API is described at
#'     \url{https://icite.od.nih.gov/api}.
#'
#' @param tbl For `icite()`, `tbl` must contain a column `pmid` with
#'     PubMed ids, for instance as in the tibble derived from
#'     `reporter_publications()`.
#'
#' @return `icite()` returns a tibble with columns defined by
#'     `include_fields`.
#'
#' @examples
#' ## which fields are available in icite?
#' icite(slice(publications, 1L)) |>
#'     glimpse()
#'
#' include_fields <- c(
#'     "pmid", "year", "citation_count", "relative_citation_ratio",
#'     "doi"
#' )
#' icite(publications, include_fields)
#'
#' @importFrom dplyr pull bind_rows
#'
#' @export
icite <-
    function(tbl, include_fields = NULL, verbose = FALSE)
{
    stopifnot(
        "pmid" %in% colnames(tbl),
        is_numeric(pull(tbl, "pmid")),
        is.null(include_fields) || is_character(include_fields),
        is_scalar_logical(verbose)
    )
    pmids <- pull(tbl, "pmid")

    ## limit 1000 per request
    result <- NULL
    pmid_chunks <- split(pmids, ceiling(seq_along(pmids) / 1000L))
    for (pmid_chunk in pmid_chunks) {
        result_1 <- icite_one_chunk(pmid_chunk, include_fields, verbose)
        result <- bind_rows(result, result_1)
    }

    class(result) <- c("cite_tbl", class(result))
    result
}
