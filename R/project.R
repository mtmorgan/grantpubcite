## projects can be funded from programs outside the current FOAs;
## find the total funding for the project
project_amount <-
    function(tbl, verbose = FALSE)
{        
    stopifnot(
        inherits(tbl, "tbl_df"),
        "core_project_num" %in% colnames(tbl),
        is_scalar_logical(verbose)
    )

    project_nums <-
        ## distinct project numbers
        tbl |>
        distinct(.data$core_project_num) |>
        pull()

    ## query for funded amount across all programs
    include_fields <- c("core_project_num", "subproject_id", "award_amount")
    project_amount <- reporter_projects(
        project_nums = project_nums,
        include_fields = include_fields,
        verbose = verbose
    )

    ## exclude subprojects (so award_amount is not double-counted)
    ## and summarize over all award years
    project_amount <-
        project_amount |>
        filter(is.na(.data$subproject_id))  |>
        group_by(.data$core_project_num) |>
        summarize(project_amount = sum(.data$award_amount, na.rm = TRUE))

    project_amount |>
        gpc_columns_clean()
}


project_summary <-
    function(tbl, verbose = FALSE)
{
    stopifnot(
        inherits(tbl, "tbl_df"),
        "core_project_num" %in% colnames(tbl),
        is_scalar_logical(verbose)
    )

    project_nums <-
        ## distinct project numbers
        projects |>
        distinct(.data$core_project_num) |>
        pull()

    include_fields <- c(
        "core_project_num", "subproject_id",
        "fiscal_year", "award_amount"
    )
    project_summary <- reporter_projects(
        project_nums = project_nums,
        include_fields = include_fields,
        verbose = verbose
    )

    project_summary |>
        filter(is.na(subproject_id)) |>
        group_by(.data$fiscal_year) |>
        summarize(
            project = n(),
            project_amount = sum(.data$award_amount, na.rm = TRUE)
        ) |>
        gpc_columns_clean()
}
