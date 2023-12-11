#' @importFrom dplyr filter count distinct inner_join right_join n

collaboration_data_collection <-
   function(tbl)
{
    stopifnot(
        inherits(tbl, "tbl_df"),
        "opportunity_number" %in% colnames(tbl)
    )

    ## data collection

    projects <- reporter_projects(
        foa = pull(tbl, "opportunity_number"),
        include_fields = "core_project_num"
    )

    core_project_nums <- pull(projects, "core_project_num")
    publications <-
        reporter_publications(core_project_nums = core_project_nums) |>
        rename(core_project_num = "coreproject") |>
        select("core_project_num", "pmid")

    icite_include_fields <- c(
        "pmid", "citation_count", "relative_citation_ratio",
        "cited_by"
    )
    citations <- icite(
        publications |> select("pmid") |> distinct(),
        include_fields = icite_include_fields
    )

    ## publication summary -- n publication, citation count, rcr.
    ## shared between copublication and cocitation

    publ_summary <-
        left_join(publications, citations, by = "pmid") |>
        group_by(.data$core_project_num) |>
        summarize(
            n = n(),
            citn = sum(.data$citation_count),
            rcr = sum(.data$relative_citation_ratio, na.rm = TRUE)
        )

    ## return value
    list(
        projects = projects,
        publications = publications,
        citations = citations,
        publ_summary = publ_summary
    )
}

copublication_pmid_project <-
    function(tbl)
{
    stopifnot(
        ## tbl of pmid / project pairs.
        inherits(tbl, "tbl_df"),
        "pmid" %in% colnames(tbl)
    )

    ## filter pmid / project publications to just the
    ## collaborative ones
    tbl |>
        count(.data$pmid) |>
        filter(.data$n > 1L) |>
        select("pmid") |>
        inner_join(tbl, by = "pmid")
}

#' @rdname collaboration
#'
#' @title Program project collaboration through publication and citation
#'
#' @description `copublication_data()` returns a tibble enumerating
#'     all pmid acknowledging more than one project.
#'
#' @param tbl a tibble with column `opportunity_number` containing the
#'     Funding Opportunity Announcements defining the projects of
#'     interest.
#'
#' @param exclude character() pmid to exclude from summary.
#'
#' @return `copublication_data()` returns a tibble with the following
#'     columns.
#'
#' - `core_project_num.x`, `core_project_num.y`: character() pairwise
#'   project collaboration.
#' - `n`: intger() number of collaborations between projects.
#'
#' @examples
#' foas <- tribble(
#'     ~opportunity_number,       ~description,
#'     "RFA-CA-19-039", "Early-Stage Development of ...",
#'     "RFA-CA-19-038", "Development of Innovative ...",
#'     "RFA-CA-19-040", "Advanced Development of ...",
#'     "RFA-CA-19-041", "Sustained Support of ..."
#' )
#'
#' copublication_data(foas)
#'
#' @export
copublication_data <-
    function(tbl, exclude = NULL)
{
    stopifnot(
        is.null(exclude) || is_character(exclude)
    )
    ## data collection

    data <- collaboration_data_collection(tbl)
    publications <- data$publications

    ## summary

    pmid_project <-
        copublication_pmid_project(publications) |>
        filter(!.data$pmid %in% exclude)

    collaborative_publications <-
        pmid_project |>
        ## outer product of collaborations
        left_join(pmid_project, by = "pmid") |>
        ## made unique and without 'self collaboration'
        filter(.data$core_project_num.x < .data$core_project_num.y) |>
        count(.data$core_project_num.x, .data$core_project_num.y, sort = TRUE)

    collaborative_publications |>
        gpc_columns_clean()
}

#' @rdname collaboration
#'
#' @description `copublication()` summarizes collaboration between
#'     projects at the time of publication.
#'
#' @return `copublication()` returns a tibble with columns
#'
#' - `core_project_num`: character() project numbers involved in
#'   collaboration
#'
#' - `n`: integer() total number of publications from the
#'   project.
#'
#' - `citn`: integer() total number of citations to project
#'   publications.
#'
#' - `rcr`: numeric() total relative citation rate of project
#'   publications.
#'
#' - `collab`: integer() number of distinct collaborators, across all
#'   collaborative publications.
#'
#' - `n_collab`: integer() number of collaborative publications.
#'
#' - `citn_collab`: numeric() total 'citation_count' for collaborative
#'   publications.
#'
#' - `rcr_collab`: numeric() total 'relative citation ratio' for
#'   collaborative publications.
#'
#' @examples
#' copublication(foas)
#'
#' @export
copublication <-
    function(tbl)
{
    ## data collection

    data <- collaboration_data_collection(tbl)
    publications <- data$publications
    citations <- data$citations
    publ_summary <- data$publ_summary

    ## summary

    pmid_project <- copublication_pmid_project(publications)
        publications |>
        count(.data$pmid) |>
        filter(.data$n > 1L) |>
        select("pmid") |>
        inner_join(publications, by = "pmid")

    copub_data <- copublication_data(tbl)
    n_collab <-
        bind_rows(
            select(copub_data, core_project_num = .data$core_project_num.x),
            select(copub_data, core_project_num = .data$core_project_num.y)
        ) |>
        count(.data$core_project_num, name = "collab")

    stat_collab <-
        pmid_project |>
        left_join(citations, by = "pmid") |>
        group_by(.data$core_project_num) |>
        summarize(
            n_collab = n(),
            citn_collab = sum(.data$citation_count),
            rcr_collab = sum(.data$relative_citation_ratio, na.rm = TRUE)
        )

    ## synthesize summaries and return
    publ_summary |>
        left_join(n_collab, by = "core_project_num") |>
        left_join(stat_collab, by = "core_project_num") |>
        gpc_columns_clean()
}

#' @rdname collaboration
#'
#' @description `cocitation_data()` returns a tibble enumerating all
#'     pmid cited by other program pmid.
#'
#' @return `cocitation_data()` returns a tibble with the following
#'     columns.
#'
#' - `pmid`: integer() pmid of original publication.
#' - `cited_by`: integer() pmid of publication citing original
#'   publication.
#' - `core_project_num`: character() project number of project
#'   acknowledged in original publication.
#' - `cited_by_core_project_num`: character() project number of
#'   project citing the original publication.
#'
#' @examples
#' cocite_data <- cocitation_data(foas)
#' cocite_data
#'
#' ## pmid / core_project_num (and cited_by / cited_by_core_project_num)
#' ## reflects the fact that a single pmid may acknowledge several
#' ## projects
#' cocite_data |>
#'     distinct(pmid, core_project_num) |>
#'     count(pmid, sort = TRUE)
#'
#' ## cocitation 'edges' between projects, and their weights
#' cocite_data |>
#'     count(core_project_num, cited_by_core_project_num, sort = TRUE)
#'
#' @export
cocitation_data <-
    function(tbl)
{
    ## data collection

    data <- collaboration_data_collection(tbl)
    publications <- data$publications
    citations <- data$citations

    ## summary -- collaborative citation

    ## pmid / cited_by as tibble
    cited_by <-
        strsplit(citations$cited_by, "[[:blank:]]+") |>
        lapply(unique)
    pmid_cited_by <- tibble(
        pmid = rep(citations$pmid, lengths(cited_by)),
        cited_by = as.numeric(unlist(cited_by))
    ) |>
        filter(.data$pmid != .data$cited_by)

    ## restrict citations to program publications, translate pmid to project
    collaborative_citations <-
        pmid_cited_by |>
        ## filter pmid_cited_by to included only program publications
        inner_join(
            publications |> distinct(.data$pmid),
            by = c(cited_by = "pmid")
        ) |>
        ## translate pmid and cited_by to project number
        left_join(publications, by = "pmid") |>
        left_join(publications, by = c(cited_by = "pmid")) |>
        rename(
            core_project_num = "core_project_num.x",
            cited_by_core_project_num = "core_project_num.y"
        )

    collaborative_citations |>
        gpc_columns_clean()
}

#' @rdname collaboration
#'
#' @description `cocitation()` summarizes collaborations between
#'     projects through citation.
#'
#' @return `cocitation()` returns a tibble with columns
#'
#' - `project`: character() project number associated with
#'   publications.
#'
#' - `n`: integer() total number of publications from the
#'   project.
#'
#' - `citn`: integer() total number of citations to project
#'   publications.
#'
#' - `rcr`: numeric() total relative citation rate of project
#'   publications.
#'
#' - `n_self_citn`: integer() number of citations to other
#'   publications of the same project.
#'
#' - `n_collab_citn`: integer() number of publications cited by other
#'    projects.
#'
#' - `n_collab`: integer() total number of projects
#'   citing this project.
#'
#' - `rcr_citn`: numeric() total 'relative citation ratio' of
#'   collaborative cited publications.
#'
#' @examples
#' cocitation(foas)
#'
#' @export
cocitation <-
    function(tbl)
{
    ## data collection

    data <- collaboration_data_collection(tbl)
    citations <- data$citations
    publ_summary <- data$publ_summary
    cocite_data <- cocitation_data(tbl)

    ## number of self citatons
    collab_self_summary <-
        cocite_data |>
        filter(.data$core_project_num == .data$cited_by_core_project_num) |>
        count(.data$core_project_num, name = "n_self_citn")

    ## number and number of collaborative citations
    collab_summary <-
        cocite_data |>
        ## exclude self-citations
        filter(.data$core_project_num != .data$cited_by_core_project_num) |>
        group_by(.data$core_project_num) |>
        summarize(
            n_collab_citn = n(),
            n_collab = length(unique(.data$cited_by_core_project_num))
        )

    ## rcr of collaborative publications
    rcr_summary <-
        cocite_data |>
        distinct(.data$pmid, .data$core_project_num) |>
        left_join(
            citations |> select(.data$pmid, .data$relative_citation_ratio),
            by = "pmid"
        ) |>
        group_by(.data$core_project_num) |>
        summarize(
            rcr_citn = sum(.data$relative_citation_ratio, na.rm = TRUE)
        )

    ## synthesize summaries and return

    publ_summary |>
        left_join(collab_self_summary, by = "core_project_num") |>
        left_join(collab_summary, by = "core_project_num") |>
        left_join(rcr_summary, by = "core_project_num") |>
        gpc_columns_clean()
}
