#' Helper function for details (i.e. date, compilation status etc. )
#'
#' This function retrieves the details from last file creation and can be used to modify
#' download file creation behavior.
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param quid Questionnaire ID
#' @param version Questionnaire version
#' @param format one of Tab, Csv or Excel
#'
#' @export
#'
#' @import data.table

suso_details_lastexport <- function(url=suso_get_api_key("susoServer"),
                                    usr = suso_get_api_key("susoUser"),
                                    pass = suso_get_api_key("susoPass"),
                                    quid = "", version = 1, format = "STATA") {
    ## BASE URL
    url <- parse_url(url)
    url$scheme <- "https"
    ## QUEST ID
    quid = paste0(str_replace_all(quid, "-", ""), "$", version)
    ## PATH FOR: details on export status
    url$path <- file.path("api", "v1", "export", format, quid, "details")
    ## BUILD URL
    url <- build_url(url)
    ## API CALL

    test_detail <- GET(url = url, authenticate(usr, pass, type = "basic"))
    stop_for_status(test_detail, task = url)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    ## OUTPUT
    return(test_json)
}
