#' Helper function for details (i.e. date, compilation status etc. )
#'
#' This function retrieves the details from last file creation and can be used to modify
#' download file creation behavior.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param pid Export process id, required if \emph{quid} and \emph{version} is NULL.
#' @param quid Questionnaire ID
#' @param version Questionnaire version
#' @param format one of Tabular, STATA or SPSS
#'
#' @export
#'
#' @import data.table

suso_details_lastexport <- function(server = suso_get_api_key("susoServer"),
                                    apiUser = suso_get_api_key("susoUser"),
                                    apiPass = suso_get_api_key("susoPass"),
                                    token = NULL,
                                    workspace = NULL,
                                    quid = NULL,
                                    version = NULL,
                                    pid = NULL, format = "STATA") {
    ## workspace default
    workspace<-.ws_default(ws = workspace)

    ## BASE URL
    url <- parse_url(url = server)
    url$scheme <- "https"
    # Set the authentication
    auth<-authenticate(apiUser, apiPass, type = "basic")

    if(is.null(pid)) {
        ## 1. get list of export processes
        url$path <- file.path(workspace, "api", "v2", "export")

    } else {
        ## 2. get details about specific export processes
        url$path <- file.path(workspace, "api", "v2", "export", pid)
    }

    ## BUILD URL
    url <- build_url(url)
    ## API CALL
    test_detail <- GET(url = url, auth)
    stop_for_status(test_detail, task = url)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- data.table(fromJSON(aJsonFile))
    ## OUTPUT
    return(test_json)
}
