#' @importFrom httr GET stop_for_status content
get <-
    function(url, ..., as = "text")
{
    response <- GET(url, ...)
    stop_for_status(response)
    content(response, as = as, encoding = "UTF-8")
}

#' @importFrom httr POST
post <-
    function(url, body, ..., as = "text")
{
    response <- POST(url, body = body, ...)
    stop_for_status(response)
    content(response, as = as, encoding = "UTF-8")
}
