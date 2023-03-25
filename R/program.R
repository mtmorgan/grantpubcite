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
#'     summarizing project activity. Columns are
#'
#' - `year`: integer() year of program.
#' - `project`: integer() number of active projects.
#' - `amount`: integer() award amount to active projects.
#' - `publications`: integer() number of publications.
#' - `citation_count`: integer() citations to publications in year.
#' - `relative_citation_ratio`: numeric() sum of relative citation
#'   ratios for all publications in year.
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
#' @importFrom dplyr summarize group_by arrange rename left_join
#'     full_join .data
#'
#' @importFrom utils tail
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
            citation_count = as.integer(sum(.data$citation_count)),
            relative_citation_ratio =
                sum(.data$relative_citation_ratio, na.rm = TRUE)
        )

    program_awards |>
        full_join(program_citations, by = "fiscal_year") |>
        mutate(fiscal_year = as.integer(.data$fiscal_year)) |>
        arrange(.data$fiscal_year) |>
        gpc_colnames_standardize()
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

#' @importFrom dplyr everything
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
#'     summarized -- by `foa` so that projects funded by more than one
#'     FOA are reported for each FOA, or `project` so that the summary
#'     is by project number across FOA.
#'
#' @details `program_projects()` provides a single row for each
#'     project. It chooses as `full_foa` the most recent FOA under
#'     which the project was funded. It chooses as `contact_pi_name`
#'     and `project_title` the name of the contact PI and project
#'     title of the largest award in the most recent year of funding.
#'
#' @return `program_projects()` returns a tibble summarizing projects
#'     funded under the FOAs. With `by = "full_foa"` (default),
#'     columns are:
#'
#' - `foa`: character() full FOA funding the project.
#' - `project`: character() core project number.
#' - `years`: integer() fiscal years of funding; may differ from
#'   project duration.
#' - `amount`: integer() award amount across fiscal years.
#' - `contact_pi`: character() name of most-recent contact PI for FOA
#'   and core project number.
#' - `title`: character() project title.
#'
#' @return With `by = "project"`, columns are the same but with
#'     `amount` and most recent contact PI and project title
#'     summarized over all FOAs under which a project was funded.
#'
#' @examples
#' program_projects(foas)
#'
#' @export
program_projects <-
    function(tbl, by = c("foa", "project"))
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

    if (identical(by, "foa")) {
        projects <- program_projects_by_foa(projects)
    } else {
        projects <- program_projects_by_project_num(projects)
    }

    projects |>
        gpc_colnames_standardize()
}

#' @rdname program
#'
#' @description `program_publications()` retrieves basic information
#'     for all projects in the program.
#'
#' @return `program_publications()` returns a tibble with columns:
#'
#' - `foa`: character() full FOA funding the project.
#' - `project`: character() core project number.
#' - `pmid`: integer() PubMed identifier.
#' - `year`, `title`, `authors`, `journal`,`doi`: publication information.
#' - `citation_count`: integer() number of publicatons citing this
#'   publication, from `icite()`.
#' - `relative_citation_ratio`: relative citation ratio, as defined by
#'   `icite()`.
#'
#' @examples
#' pubs <- program_publications(foas)
#'
#' ## note that individual publications can be represented by more
#' ## than one project
#' pubs
#'
#' ## unique publications
#' unique_pubs <-
#'     pubs |>
#'     select(-c("foa", "project")) |>
#'     distinct()
#' unique_pubs
#'
#' ## publications per project
#' pubs |>
#'     count(project, sort = TRUE) |>
#'     left_join(program_projects(foas, by = "project"), by = "project")
#'
#' ## collaborations; see `?copublication`, `?cocitation`
#' pubs |>
#'     count(pmid, sort = TRUE, name = "n_collab") |>
#'     left_join(unique_pubs, by = "pmid")
#'
#' @export
program_publications <-
    function(tbl)
{
    ## data collection

    projects <- program_projects(tbl, by = "project")
    core_project_nums <- unique(pull(projects, "project"))
    publications <-
        reporter_publications(core_project_nums = core_project_nums) |>
        select(-"applid")

    icite_include_fields <- c(
        "pmid", "year", "title", "authors", "journal", "doi",
        "citation_count", "relative_citation_ratio"
    )
    citations <- icite(publications, include_fields = icite_include_fields)

    ## summary

    publications |>
        inner_join(projects, by = c(coreproject = "project")) |>
        select("foa", project = "coreproject", "pmid") |>
        inner_join(citations, by = "pmid") |>
        gpc_colnames_standardize() |>
        gpc_columns_clean()
}
