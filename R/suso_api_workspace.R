#' Survey Solutions API call for workspace
#'
#'
#' \code{suso_getWorkspace} allows you to get the list of workspaces, information about individual workspace names
#' as well as workspace statuses. Workspaces as well as Workspaces information can only be accessed, if credentials
#' are eligible. For more details please read \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param workspace If workspace name is provide requests are made regarding this specific workspace
#' @param status if status is \emph{TRUE} worskpace must be not NULL and status information about the specific
#' workspace is requested
#'
#'
#' @export


suso_getWorkspace <- function(server = suso_get_api_key("susoServer"),
                              apiUser = suso_get_api_key("susoUser"),
                              apiPass = suso_get_api_key("susoPass"),
                              token = NULL,
                              workspace = NULL,
                              status = FALSE) {


  ## Set temporary file
  aJsonFile <- tempfile(fileext = ".json")
  # Define the API
  url <- httr::parse_url(url = server)
  url$scheme <- "https"
  # Set the authentication
  auth<-authenticate(apiUser, apiPass, type = "basic")

  if(is.null(workspace)) {
    ## 1. get list of workspaces
    ## 1.1 define final path
    url$path<-file.path("primary","api", "v1", "workspaces")
    ## 1.2 call
    test_detail <- GET(url = build_url(url),
                       auth,
                       httr::write_disk(aJsonFile, overwrite = T))
    check_response(test_detail)
    test_json <- jsonlite::fromJSON(aJsonFile)
    test_json<-data.table(test_json$Workspaces)
    test_json<-test_json[]
  } else if(!is.null(workspace)) {
    ## 2. get workspace details & status -->status disbled because of 403 error

    url$path <- ifelse(status==FALSE,
                       file.path(workspace,"api", "v1", "workspaces", workspace),
                       file.path(workspace,"api", "v1", "workspaces", workspace))

    test_detail <- GET(url = build_url(url),
                       auth,
                       httr::write_disk(aJsonFile, overwrite = T))
    check_response(test_detail)
    test_json <- jsonlite::fromJSON(aJsonFile)
    test_json<-data.table(t(test_json))
    test_json<-test_json[]
  }
  ##########################################################
  return(test_json)
}
