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
#'     "RFA-CA-19-041", "Sustained Support of..."
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
        foa = tbl$full_foa,
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

## program_projects

program_projects_by_foa <-
    function(projects)
{
    projects |>
        arrange(.data$fiscal_year, .data$award_amount) |>
        group_by(.data$full_foa, .data$core_project_num) |>
        summarize(
            years = length(unique(.data$fiscal_year)),
            award_amount = sum(.data$award_amount),
            contact_pi_name = tail(.data$contact_pi_name, 1L),
            project_title = tail(.data$project_title, 1L)
        )
}

program_projects_by_project_num <-
    function(projects)
{
    projects |>
        arrange(.data$fiscal_year, .data$award_amount) |>
        group_by(.data$core_project_num) |>
        summarize(
            full_foa = tail(.data$full_foa, 1L),
            years = length(unique(.data$fiscal_year)),
            award_amount = sum(.data$award_amount),
            contact_pi_name = tail(.data$contact_pi_name, 1L),
            project_title = tail(.data$project_title, 1L)
        ) |>
        select("full_foa", everything())
}

#' @rdname program
#'
#' @description `program_projects()` returns a tibble of project
#'     numbers and standardized contact PI name and project title.
#'
#' @param by character(1) indicating how program projects are
#'     summarized -- by `full_foa` so that projects funded by more
#'     than one FOA are reported for each FOA, or `core_project_num`
#'     so that the summary is by project number..
#'
#' @details `program_projects()` provides a single row for each
#'     project. It chooses as `full_foa` the most recent FOA under
#'     which the project was funded. It chooses as `contact_pi_name`
#'     and `project_title` the name of the contact PI and project
#'     title of the largest award in the most recent year of funding.
#'
#' @examples
#' program_projects(foas)
#'
#' @export
program_projects <-
    function(tbl, by = c("full_foa", "core_project_num"))
{
    stopifnot(
        inherits(tbl, "tbl_df"),
        "full_foa" %in% colnames(tbl)
    )
    by <- match.arg(by)

    ## data collection

    project_include_fields = c(
        "full_foa", "core_project_num",
        "fiscal_year", "award_amount",
        "contact_pi_name", "project_title"
    )

    projects <- reporter_projects(
        foa = tbl$full_foa,
        include_fields = project_include_fields
    )

    ## summary
    if (identical(by, "full_foa")) {
        program_projects_by_foa(projects)
    } else {
        program_projects_by_project_num(projects)
    }
}
