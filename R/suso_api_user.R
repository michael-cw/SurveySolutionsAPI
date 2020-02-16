#' Survey Solutions API call for list of supervisors
#'
#'
#' @export
suso_getSV <- function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass")) {
    ## Define the api
    url <- ifelse(str_count(url, "https://") == 1, url, paste0("https://", url))
    url = paste0(url, "/api/v1/supervisors")
    test_detail <- GET(url = paste0(url, "?limit=200"), authenticate(usr, pass, type = "basic"))
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    return(test_json)
}


#' Survey Solutions API call for list of interviewers
#'
#'
#' @export
suso_getINT <- function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
    sv_id = NULL) {
    url <- ifelse(str_count(url, "https://") == 1, url, paste0("https://", url))
    ## Define the api
    url = paste0(url, "/api/v1/supervisors/")
    test_detail <- GET(url = paste0(url, sv_id, "/interviewers?limit=200"), authenticate(usr, pass, type = "basic"))

    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    return(test_json)
}

