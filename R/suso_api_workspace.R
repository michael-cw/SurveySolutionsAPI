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
#' @examples
#' \dontrun{
#' # This assumes, that suso_PwCheck(workspace = "myworkspace") was
#' # sucessful
#'
#' # shows all workspaces in the system AND the user has access to
#' suso_createWorkspace(
#'           workspace = "myworkspace",
#'           status = F)
#'
#' # shows details for specific workspace myworkspace
#' suso_createWorkspace(
#'           workspace = "myworkspace",
#'           status = F)
#' }
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


#' Survey Solutions API call to create workspace
#'
#'
#' @description \code{suso_createWorkspace} Allows you to create a workspace with a specific system name and display name.
#' For more details please read \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}. To run this command
#' you require admin credentials.
#'
#' @details Be aware, that for using this call you require the admin credentials, and not the regular API user credentials.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions ADMIN user
#' @param apiPass Survey Solutions ADMIN password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param new_workspace The name used by the system for this workspace. Make sure you follow the rules outlined
#' under \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}
#' @param your_workspace The workspace the API user has currently access to. To get access to the new workspace, the workspace needs to be
#' assigned
#' @param displayName The name visible to the users
#'
#' @examples
#' \dontrun{
#' # Use Admin Credentials!
#' suso_createWorkspace(
#'           your_workspace = "myworkspace",
#'           new_workspace = "myworkspace1",
#'           displayName = "SpecialWorkspace",
#'           apiUser = "xxxxxx",
#'           apiPass = "xxxxxx")
#' }
#'
#'
#' @export


suso_createWorkspace <- function(server = suso_get_api_key("susoServer"),
                                 apiUser = suso_get_api_key("susoUser"),
                                 apiPass = suso_get_api_key("susoPass"),
                                 token = NULL,
                                 new_workspace = NULL,
                                 your_workspace = NULL,
                                 displayName = NULL) {


  ## Set temporary file
  aJsonFile <- tempfile(fileext = ".json")
  # Define the API
  url <- httr::parse_url(url = server)
  url$scheme <- "https"
  # Set the authentication
  auth<-authenticate(apiUser, apiPass, type = "basic")


  url$path <- file.path(your_workspace,"api", "v1", "workspaces", fsep = "/")

  test_detail <- POST(url = build_url(url),
                      body = list(Name=new_workspace, DisplayName=displayName),
                      encode = "json",
                      auth,
                      httr::write_disk(aJsonFile, overwrite = T))
  if(test_detail$status_code%in%c(401, 403)) stop("Your are not authorized for this operation. Have you used admin credentials?")
  test_json <- jsonlite::fromJSON(aJsonFile)
  test_json<-data.table(t(test_json))
  test_json<-test_json[]

  ##########################################################
  return(test_json)
}



#' Survey Solutions API call to assign workspace
#'
#'
#' @description \code{suso_assignWorkspace} Allows you to assign a workspace to a specific user.
#' For more details please read \url{https://docs.mysurvey.solutions/headquarters/accounts/workspaces/}. To run this command
#' you require admin credentials.
#'
#' @details Be aware, that for using this call you require admin credentials, and not the regular API user credentials.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions ADMIN user
#' @param apiPass Survey Solutions ADMIN password
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param assign_workspace The workspace which you want to assign to the new user
#' @param your_workspace The workspace the API user has currently access to.
#' @param uid The User ID of the user to be assigned.
#' @param sv_id The supervisor's ID to which the interviewer should be assigned to, if it is a supervisor who is assigned, just use the same
#' as in \emph{uid}.
#'
#' @return If succesfull, returns a data.table with the details as well as the Status message "Worspaces list updated".
#'
#' @examples
#' \dontrun{
#' # Use Admin Credentials!
#' suso_assignWorkspace(
#'           your_workspace = "myworkspace",
#'           assign_workspace = "myworkspace1",
#'           uid = "xxx-xxx-xxx-xxx-xxx",
#'           sv_id = "xxx-xxx-xxx-xxx-xxx",
#'           apiUser = "xxxxxx",
#'           apiPass = "xxxxxx")
#' }
#'
#'
#' @export


suso_assignWorkspace <- function(server = suso_get_api_key("susoServer"),
                                 apiUser = suso_get_api_key("susoUser"),
                                 apiPass = suso_get_api_key("susoPass"),
                                 token = NULL,
                                 assign_workspace = NULL,
                                 your_workspace = NULL,
                                 uid = NULL,
                                 sv_id = NULL) {


  ## Set temporary file
  aJsonFile <- tempfile(fileext = ".json")
  # Define the API
  url <- httr::parse_url(url = server)
  url$scheme <- "https"
  # Set the authentication
  auth<-authenticate(apiUser, apiPass, type = "basic")


  url$path <- file.path(your_workspace,"api", "v1", "workspaces", "assign", fsep = "/")
  # request body
  js_ch<-list(
      UserIds=I(uid),
      Workspaces=data.table(
        Workspace=assign_workspace,
        SupervisorId=sv_id
        ),
      Mode=jsonlite::unbox("Assign")
    )

  test_detail <- POST(url = build_url(url),
                      body = js_ch,
                      httr::accept_json(),
                      httr::add_headers(charset="utf-8"),
                      encode = "json",
                      auth,
                      httr::write_disk(aJsonFile, overwrite = T))
  if(test_detail$status_code%in%c(401, 403)) stop("Your are not authorized for this operation. Have you used admin credentials?")
  if(test_detail$status_code==204) {
    test_json<-data.table::data.table(UserIds = uid, Workspace = assign_workspace, SupervisorId = sv_id, Status = "Workspaces list updated")
  }

  ##########################################################
  return(test_json)
}





