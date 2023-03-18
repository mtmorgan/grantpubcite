#' @rdname program
#'
#' @title Program-level queries and summaries
#'
#' @description `program_summary()` finds all projects, publications,
#'     and citations funded by the Funding Opportunity Announcements
#'     (FOAs) in `tbl`, and summarizes by year the number of projects,
#'     total award ammount, publications, citation count, and relative
#'     citation ratio.
#'
#' @param tbl A tibble with column `full_foa` giving the FOAs that
#'     define the program.
#'
#' @details `program_summary()` can return rows corresponding to years
#'     before and after the years the FOAs were active. Values before
#'     the FOA activity usually represent a grantee assigning credit
#'     to the FOA for a citation published prior to the start of the
#'     FOA, or to a project funded by FOAs not included in the input
#'     `tbl` but with the same core project number as a funded
#'     project. Values after the end of the FOA represent publications
#'     that acknowledge the FOA after the termination of the grant.
#'
#' @return `program_summary()` returns a tibble ordered by fiscal year
#'     summarizing project activity.
#'
#' @examples
#' foas <- tribble(
#'     ~full_foa,       ~description,
#'     "RFA-CA-19-039", "Early-Stage Development of ...",
#'     "RFA-CA-19-038", "Development of Innovative ...",
#'     "RFA-CA-19-040", "Advanced Development of ...",
#'     "RFA-CA-19-041", "Sustained Support..."
#' )
#'
#' program_summary(foas)
#' 
#' @importFrom dplyr full_join .data
#'
#' @export
program_summary <-
    function(tbl)
{
    stopifnot(
        inherits(tbl, "tbl_df"),
        "full_foa" %in% colnames(tbl)
    )

    ## data collection

    project_include_fields <- c(
        "full_foa", "core_project_num", "fiscal_year", "award_amount"
    )
    projects <- reporter_projects(
        foa = foas$full_foa,
        include_fields = project_include_fields
    )

    core_project_num <-
        pull(projects, "core_project_num") |>
        unique()
    publications <- reporter_publications(core_project_nums = core_project_num)

    icite_include_fields <- c(
        "pmid", "year", "citation_count", "relative_citation_ratio"
    )
    citations <-
        icite(publications, include_fields = icite_include_fields) |>
        rename(fiscal_year = "year")

    ## summary

    program_awards <-
        projects |>
        group_by(.data$full_foa, .data$core_project_num, .data$fiscal_year) |>
        summarize(award_amount = sum(.data$award_amount)) |>
        group_by(.data$fiscal_year) |>
        summarize(
            project = length(unique(.data$core_project_num)),
            award_amount = sum(.data$award_amount)
        )

    program_citations <-
        publications |>
        left_join(citations, by = "pmid") |>
        group_by(.data$fiscal_year) |>
        summarize(
            publication = length(unique(.data$pmid)),
            citation_count = sum(.data$citation_count),
            relative_citation_ratio =
                sum(.data$relative_citation_ratio, na.rm = TRUE)
        )

    program_awards |>
        full_join(program_citations, by = "fiscal_year") |>
        mutate(fiscal_year = as.integer(.data$fiscal_year)) |>
        arrange(fiscal_year)
}
