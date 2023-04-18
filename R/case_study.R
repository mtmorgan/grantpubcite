#' @importFrom readr read_csv

case_study_foa <-
    function(name)
{
    stopifnot(
        is_scalar_character(name)
    )
    file_name <- paste0("case_study_foa_", name, ".csv")

    fl <- system.file(package = "grantpubcite", file_name)
    read_csv(fl, show_col_types = FALSE)
}

#' @rdname case_study
#'
#' @title Utilities for Input of Case Study Funding Opportunity
#'     Announcements
#'
#' @description `case_study_foa_imat()` FOAs for the IMAT program.
#'
#' @examples
#' case_study_foa_imat()
#'
#' @export
case_study_foa_imat <-
    function()
{
    full_tag_levels <- c(
        "R61 Mol Analysis", "R61 Biospec",
        "R33 Mol Analysis", "R33 Biospec",
        "SBIR",
        "R01", "U01", "P01", "P50", "U54", "U2C"
    )
    case_study_foa("imat") |>
        mutate(full_tag = factor(full_tag, levels = full_tag_levels))
}

#' @rdname case_study
#'
#' @description `case_study_foa_iotn()` FOAs for the IOTN program.
#'
#' @examples
#' case_study_foa_iotn()
#'
#' @export
case_study_foa_iotn <-
    function()
{
    tag_levels <- c("UG3/UH3", "U01", "DMRC", "CIDR", "U54")
    case_study_foa("iotn") |>
        mutate(tag = factor(tag, levels = tag_levels))
}

#' @rdname case_study
#'
#' @description `case_study_foa_itcr()` FOAs for the ITCR program.
#'
#' @examples
#' case_study_foa_itcr()
#'
#' @export
case_study_foa_itcr <-
    function()
{
    tag_levels <- c(
        "Innovative", "Early-Stage", "Advanced", "Sustained", "Education"
    )
    case_study_foa("itcr") |>
        mutate(foa_tag = factor(foa_tag, levels = tag_levels))
}
