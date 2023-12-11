#' @importFrom httr user_agent
pkg_user_agent <-
    function()
{
    file_path <- system.file(package = PACKAGE_NAME, "DESCRIPTION")
    dcf <- read.dcf(file_path, c("Package", "Version", "URL"))
    agent <- sprintf(
        "%s/%s (%s)",
        dcf[, "Package"], dcf[, "Version"], dcf[, "URL"]
    )
    user_agent(agent)
}

#' @importFrom httr GET stop_for_status content
get <-
    function(url, ..., as = "text")
{
    response <- GET(url, ..., pkg_user_agent())
    stop_for_status(response)
    content(response, as = as, encoding = "UTF-8")
}

#' @importFrom httr POST
post <-
    function(url, body, ..., as = "text")
{
    response <- POST(url, body = body, ..., pkg_user_agent())
    stop_for_status(response)
    content(response, as = as, encoding = "UTF-8")
}
