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
#' @param tbl A tibble with column `opportunity_number` giving the
#'     FOAs that define the program.
#'
#' @param verbose logical(1) report additional detail about progress
#'     of reporter and icite queries.
#'
#' @details `program_summary()` can return rows corresponding to years
#'     before and after the years the FOAs were active. Values before
#'     the FOA activity represent either a project funded through
#'     other programs, or a grantee assigning credit to the FOA for a
#'     citation published prior to the start of the FOA, or to a
#'     project funded by FOAs not included in the input `tbl` but with
#'     the same core project number as a funded project. Values after
#'     the end of the FOA represent publications that acknowledge the
#'     FOA after the termination of the grant.
#'
#' @return `program_summary()` returns a tibble ordered by fiscal year
#'     summarizing project activity. Columns are
#'
#' - `fiscal_year`: integer() year of program.
#' - `program`: integer() number of active projects funded by this program.
#' - `project`: integer() number of projects funded by any program.
#'
#' - `program_amount`: integer() award amount to active projects from
#'   this program.
#' - `project_amount`: integer() award amount across all programs.
#' - `publications`: integer() number of publications.
#' - `citation_count`: integer() citations to publications in year.
#' - `relative_citation_ratio`: numeric() sum of relative citation
#'   ratios for all publications in year.
#'
#' @examples
#' foas <- tribble(
#'     ~opportunity_number, ~description,
#'     "RFA-CA-19-039",     "Early-Stage Development of ...",
#'     "RFA-CA-19-038",     "Development of Innovative ...",
#'     "RFA-CA-19-040",     "Advanced Development of ...",
#'     "RFA-CA-19-041",     "Sustained Support of..."
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
    function(tbl, verbose = FALSE)
{
    stopifnot(
        inherits(tbl, "tbl_df"),
        "opportunity_number" %in% colnames(tbl)
    )

    ## data collection

    project_include_fields <- c(
        "opportunity_number", "core_project_num", "fiscal_year", "award_amount"
    )
    projects <- reporter_projects(
        foa = tbl$opportunity_number,
        include_fields = project_include_fields,
        verbose = verbose
    )

    ## publications in NIH Reporter
    project_nums <-
        projects |>
        distinct(.data$core_project_num) |>
        pull()
    publications <- reporter_publications(
        core_project_nums = project_nums,
        verbose = verbose
    )

    ## citations in iCite
    icite_include_fields <-
        c("pmid", "year", "citation_count", "relative_citation_ratio")
    citations <-
        icite(
            publications,
            include_fields = icite_include_fields,
            verbose = verbose
        ) |>
        rename(fiscal_year = "year")

    ## summary

    project_summary <-
        projects |>
        project_summary()

    program_amount <-
        projects |>
        group_by(
            .data$opportunity_number, .data$core_project_num, .data$fiscal_year
        ) |>
        summarize(award_amount = sum(.data$award_amount)) |>
        group_by(.data$fiscal_year) |>
        summarize(
            program = length(unique(.data$core_project_num)),
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
        ) |>
        ## remove records where NIH Reporter has a pmid, but iCite does not
        filter(!is.na(fiscal_year))

    fiscal_year <-
        program_amount |>
        full_join(program_citations, by = "fiscal_year") |>
        mutate(fiscal_year = as.integer(.data$fiscal_year)) |>
        arrange(.data$fiscal_year)

    fiscal_year |>
        full_join(project_summary, by = "fiscal_year") |>
        select(
            "fiscal_year",
            "program", "project",
            program_amount = "award_amount", "project_amount",
            everything()
        ) |>
        arrange(fiscal_year) |>
        gpc_columns_clean()
}

## program_projects

program_projects_by_foa <-
    function(projects)
{
    projects |>
        arrange(.data$fiscal_year, .data$award_amount) |>
        group_by(.data$opportunity_number, .data$core_project_num) |>
        summarize(
            project_start_date = min(.data$project_start_date, na.rm = TRUE),
            project_end_date = max(.data$project_end_date, na.rm = TRUE),
            contact_pi_name = tail(.data$contact_pi_name, 1L),
            project_title = tail(.data$project_title, 1L),
            fiscal_year = length(unique(.data$fiscal_year)),
            award_amount = sum(.data$award_amount, na.rm = TRUE),
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
            opportunity_number = tail(.data$opportunity_number, 1L),
            project_start_date = min(.data$project_start_date, na.rm = TRUE),
            project_end_date = max(.data$project_end_date, na.rm = TRUE),
            contact_pi_name = tail(.data$contact_pi_name, 1L),
            project_title = tail(.data$project_title, 1L),
            fiscal_year = length(unique(.data$fiscal_year)),
            award_amount = sum(.data$award_amount, na.rm = TRUE)
        ) |>
        select("opportunity_number", everything())
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
#'     project. It chooses as `opportunity_number` the most recent FOA
#'     under which the project was funded. It chooses as
#'     `contact_pi_name` and `project_title` the name of the contact
#'     PI and project title of the largest award in the most recent
#'     year of funding.
#'
#' @return `program_projects()` returns a tibble summarizing projects
#'     funded under the FOAs. With `by = "foa"` (default), columns
#'     are:
#'
#' - `opportunity_number`: character() full FOA funding the project.
#' - `core_project_num`: character() core project number.
#' - `project_start_date`: date() start date of project
#' - `project_end_date`: date() end date of project; maybe in the future
#' - `contact_pi_name`: character() name of most-recent contact PI for FOA
#'   and core project number.
#' - `project_title`: character() project title.
#' - `fiscal_year`: integer() fiscal years of funding; may differ from
#'   project duration (end date - start date.
#' - `award_amount`: integer() award amount through programs in the FOAs.
#" - `project_amount`: integer() total award amount across all funding sources.
#'
#' @return With `by = "project"`, columns are the same but with
#'     `project_start_date`, `project_end_date`, `fiscal_year`,
#'     `award_amount` and most recent contact PI and project title
#'     summarized over all FOAs under which a project was funded.
#'
#' @examples
#' program_projects(foas)
#'
#' @export
program_projects <-
    function(tbl, by = c("foa", "project"), verbose = FALSE)
{
    stopifnot(
        inherits(tbl, "tbl_df"),
        "opportunity_number" %in% colnames(tbl),
        is_scalar_logical(verbose)
    )
    by <- match.arg(by)

    ## data collection

    project_include_fields = c(
        "opportunity_number", "core_project_num", "subproject_id",
        "project_start_date", "project_end_date",
        "contact_pi_name", "project_title",
        "fiscal_year", "award_amount"
    )

    projects <- reporter_projects(
        foa = tbl$opportunity_number,
        include_fields = project_include_fields,
        verbose = verbose
    )

    ## sub-project award amounts are included in core project award
    ## amount; include only main award amount (!is.na(subproject_id)).
    projects <-
        projects |>
        filter(is.na(.data$subproject_id)) |>
        select(-("subproject_id"))

    ## summary
    if (identical(by, "foa")) {
        projects <- program_projects_by_foa(projects)
    } else {
        projects <- program_projects_by_project_num(projects)
    }

    ## project funding across all programs
    project_amount <- project_amount(projects, verbose)

    projects <-
        ## append 'project_amount' to 'projects'
        projects |>
        left_join(project_amount, by = "core_project_num")

    projects |>
        gpc_columns_clean()
}

#' @rdname program
#'
#' @description `program_publications()` retrieves basic information
#'     for all projects in the program.
#'
#' @return `program_publications()` returns a tibble with columns:
#'
#' - `opportunity_number`: character() full FOA funding the project.
#' - `core_project_num`: character() core project number.
#' - `pmid`: integer() PubMed identifier.
#' - `year`, `title`, `authors`, `journal`,`doi`: publication information.
#' - `citn`: integer() number of publicatons citing this
#'   publication, from `icite()`.
#' - `rcr`: relative citation ratio, as defined by `icite()`.
#' - `fcr`: field citation rate, as defined by `icite()`.
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
#'     select(-c("opportunity_number", "core_project_num")) |>
#'     distinct()
#' unique_pubs
#'
#' ## publications per project
#' pubs |>
#'     count(core_project_num, sort = TRUE) |>
#'     left_join(program_projects(foas, by = "project"), by = "core_project_num")
#'
#' ## collaborations; see `?copublication`, `?cocitation`
#' pubs |>
#'     count(pmid, sort = TRUE, name = "n_collab") |>
#'     left_join(unique_pubs, by = "pmid")
#'
#' @export
program_publications <-
    function(tbl, verbose = FALSE)
{
    ## data collection

    projects <- program_projects(tbl, by = "project", verbose = verbose)
    core_project_nums <- unique(pull(projects, "core_project_num"))
    publications <-
        reporter_publications(
            core_project_nums = core_project_nums,
            verbose = verbose
        ) |>
        select(-"applid")

    icite_include_fields <- c(
        "pmid", "year", "title", "authors", "journal", "doi",
        "citation_count", "relative_citation_ratio", "field_citation_rate"
    )
    citations <- icite(
        publications,
        include_fields = icite_include_fields,
        verbose = verbose
    )

    ## summary

    publications |>
        inner_join(projects, by = c(coreproject = "core_project_num")) |>
        select("opportunity_number", core_project_num = "coreproject", "pmid") |>
        inner_join(citations, by = "pmid") |>
        gpc_columns_clean()
}
