#' Survey Solutions API call for list of supervisors
#'
#' Gets list of supervisors
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#' @examples
#' \dontrun{
#' suso_getSV(
#'           workspace = "myworkspace"
#'           )
#' # or without any workspace and receive the default workspace
#' suso_getSV()
#'
#' }
#'
#'
#' @export
suso_getSV <- function(url=suso_get_api_key("susoServer"),
                       usr = suso_get_api_key("susoUser"),
                       pass = suso_get_api_key("susoPass"),
                       workspace = NULL,
                       token = NULL) {
    ## default workspace
    workspace<-.ws_default(ws=workspace)
    ##  BASE URL
    url<-parse_url(suso_get_api_key("susoServer"))
    url$scheme<-"https"
    url$path<-file.path(workspace,"api", "v1", "supervisors", fsep = "/")
    url$query<-list(limit = 200)
    # Set the authentication
    auth<-authenticate(usr, pass, type = "basic")

    ## API call
    test_detail <- GET(url = build_url(url = url), auth)
    check_response(test_detail, 200)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(test_json$Users)
    # Set date time to utc with lubridate
    if(nrow(test_json)>0) test_json[,CreationDate:=as_datetime(CreationDate)][]
    return(test_json)
}


#' Survey Solutions API call for list of interviewers
#'
#'
#' Get list of all interviewers by supervisor
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param sv_id supervisor id
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#'
#' @examples
#' \dontrun{
#' suso_getINT(
#'           workspace = "myworkspace",
#'           sv_id = "xxxx-xxxx-xxxx-xxx"
#'           )
#' }
#'
#' @export
suso_getINT <- function(url=suso_get_api_key("susoServer"),
                        usr = suso_get_api_key("susoUser"),
                        pass = suso_get_api_key("susoPass"),
                        sv_id = NULL,
                        workspace = NULL,
                        token = NULL) {
    ## default workspace
    workspace<-.ws_default(ws = workspace)
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path(workspace, "api", "v1", "supervisors", sv_id, "interviewers", fsep = "/")
    url$query<-list(limit = 200)
    ## Authentication
    auth<-authenticate(usr, pass, type = "basic")
    test_detail <- GET(url = build_url(url = url), auth)

    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(test_json$Users)
    # Set date time to utc with lubridate
    if(nrow(test_json)>0) test_json[,CreationDate:=as_datetime(CreationDate)][]
    return(test_json)
}




#' Survey Solutions API call for info on interviewers
#'
#'
#' Get details of interviewers
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param int_id interviewer id
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#' @examples
#' \dontrun{
#' suso_getINT_info(
#'           workspace = "myworkspace",
#'           int_id = "xxxx-xxxx-xxxx-xxx"
#'           )
#' }
#'
#' @export
suso_getINT_info<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                           int_id = NULL, workspace = NULL, token = NULL) {
    ## default workspace
    workspace<-.ws_default(ws = workspace)
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path(workspace,"api", "v1", "interviewers", int_id, fsep = "/")
    auth<-authenticate(usr, pass, type = "basic")
    test_detail <- GET(url = build_url(url = url), auth)
    check_response(test_detail, 200)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(t(unlist(test_json)))
    # Set date time to utc with lubridate
    if(nrow(test_json)>0) test_json[,CreationDate:=as_datetime(CreationDate)][]
    return(test_json)
}


#' Survey Solutions API call for audit log on interviewers
#'
#'
#' Get log of interviewer
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param int_id interviewer id
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#' @param startDate Start data for the period of interest, date must be provided as character in the format:
#' YYYY-MM-DD, and will start at 00:00.00 server time of the specified day
#' @param endDate End date for the period of interest, date must be provided ascharacter in the format:
#' YYYY-MM-DD, and will end at 23:59:59 server time of the specified day. If not end date is provided, the current
#' date will be used.
#'
#' @examples
#' \dontrun{
#' # without start or end date, the last 7 days of log file will be shown
#' suso_getINT_log(
#'           workspace = "myworkspace",
#'           int_id = "xxxx-xxxx-xxxx-xxx"
#'           )
#' # if you want to extent this period beyond the 7 days, just provide a start date
#' suso_getINT_log(
#'           workspace = "myworkspace",
#'           int_id = "xxxx-xxxx-xxxx-xxx",
#'           startDate = "2023-01-01"
#'           )
#' # or if you want it for a specific period, provide start and end date
#' suso_getINT_log(
#'           workspace = "myworkspace",
#'           int_id = "xxxx-xxxx-xxxx-xxx",
#'           startDate = "2023-01-01",
#'           endDate = "2023-02-01"
#'           )
#' # if you provide both, start and end date for the same day, you will receive
#' # all data for said day, from midnight until the 23:59:59
#' suso_getINT_log(
#'           workspace = "myworkspace",
#'           int_id = "xxxx-xxxx-xxxx-xxx",
#'           startDate = "2023-02-14",
#'           endDate = "2023-02-14")
#' }
#'
#'
#' @export
#'
#'
suso_getINT_log<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                          int_id = NULL, workspace = NULL, token = NULL, startDate = NULL, endDate = NULL) {
    ## default workspace
    workspace<-.ws_default(ws = workspace)
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path(workspace, "api", "v1", "interviewers", int_id, "actions-log", fsep = "/")

    # check start/end & format
    if(!is.null(startDate)) startDate<-paste(startDate, "00:00:00", sep = "T")
    if(!is.null(endDate)) endDate<-paste(endDate, "23:59:59", sep = "T")

    if(!is.null(startDate) && is.null(endDate)) endDate<-lubridate::format_ISO8601(Sys.time())

    url$query<-list(start=startDate, end=endDate)
    auth<-authenticate(usr, pass, type = "basic")
    test_detail <- GET(url = build_url(url = url), auth)
    check_response(test_detail, 200)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Convert to data.table
    test_json<-data.table::data.table(test_json)
    # Convert Date
    if(nrow(test_json)>0){
        # # Set date time to utc with lubridate
        test_json[,Time:=lubridate::as_datetime(Time)][]
    }
    return(test_json)
}

#' Survey Solutions API call any user
#'
#'
#' Get any user's info
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param uid user id
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#' @examples
#' \dontrun{
#' suso_getUSR(
#'           workspace = "myworkspace",
#'           uid = "xxxx-xxxx-xxxx-xxx"
#'           )
#' }
#'
#' @export
#'
suso_getUSR<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                      uid = NULL, workspace = NULL, token = NULL) {
    ## default workspace
    workspace<-.ws_default(ws = workspace)
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path(workspace,"api", "v1", "users", uid, fsep = "/")
    auth<-authenticate(usr, pass, type = "basic")
    test_detail <- GET(url = build_url(url = url), auth)
    check_response(test_detail, 200)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(t(unlist(test_json)))
    # Set date time to utc with lubridate
    test_json[,CreationDate:=as_datetime(CreationDate)][]
    return(test_json)
}


#' Survey Solutions API call archive user
#'
#'
#' (Un-)Archive user
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param uid user id
#' @param archive if TRUE user will be archived or statys archived, if FALSE user will be unarchived or stays unarchived
#' @param workspace If workspace name is provide requests are made regarding this specific workspace, if
#' no workspace is provided defaults to primary workspace.
#' @param token If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored
#'
#'
#' @examples
#' \dontrun{
#' # you can archive a user by archive=T
#' suso_archUSR(
#'           workspace = "myworkspace",
#'           uid = "xxxx-xxxx-xxxx-xxx",
#'           archive = TRUE
#'           )
#' # and unarchive a user by archive=F
#' suso_archUSR(
#'           workspace = "myworkspace",
#'           uid = "xxxx-xxxx-xxxx-xxx",
#'           archive = FALSE
#'           )
#' }
#'
#' @export
#'
suso_archUSR<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                       uid = NULL, archive = F, workspace = NULL, token = NULL) {
    ## default workspace
    workspace<-.ws_default(ws = workspace)
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    arch<-ifelse(archive, "archive", "unarchive")
    url$path<-file.path(workspace,"api", "v1", "users", uid, arch, fsep = "/")
    test_detail <- PATCH(url = build_url(url = url), authenticate(usr, pass, type = "basic"))
    check_response(test_detail, 200)
    test_json<-data.table(user=c(uid), ArchDate = as_datetime(Sys.time()), IsArchived = archive)
    return(test_json)
}

