#' Survey Solutions API call for list of supervisors
#'
#' Gets list of supervisors
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#'
#'
#' @export
suso_getSV <- function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass")) {
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path("api", "v1", "supervisors")
    url$query<-list(limit = 200)
    test_detail <- GET(url = build_url(url = url), authenticate(usr, pass, type = "basic"))
    check_response(test_detail, 200)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(test_json$Users)
    # Set date time to utc with lubridate
    test_json[,CreationDate:=as_datetime(CreationDate)][]
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
#'
#' @export
suso_getINT <- function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                        sv_id = NULL) {
    ############
    # 1. create variables
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path("api", "v1", "supervisors", sv_id, "interviewers")
    url$query<-list(limit = 200)
    test_detail <- GET(url = build_url(url = url), authenticate(usr, pass, type = "basic"))

    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    test_json<-data.table(test_json$Users)
    # Set date time to utc with lubridate
    test_json[,CreationDate:=as_datetime(CreationDate)][]
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
#'
#' @export
suso_getINT_info<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                           int_id = NULL) {
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path("api", "v1", "interviewers", int_id)
    test_detail <- GET(url = build_url(url = url), authenticate(usr, pass, type = "basic"))
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


#' Survey Solutions API call for audit log on interviewers
#'
#'
#' Get log of interviewer
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param int_id interviewer id
#'
#' @export
#'
suso_getINT_log<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                           int_id = NULL) {
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path("api", "v1", "interviewers", int_id, "actions-log")
    test_detail <- GET(url = build_url(url = url), authenticate(usr, pass, type = "basic"))
    check_response(test_detail, 200)
    aJsonFile <- tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json <- fromJSON(aJsonFile)
    # Export only records
    # test_json<-data.table(t(unlist(test_json)))
    # # Set date time to utc with lubridate
    # test_json[,CreationDate:=as_datetime(CreationDate)][]
    return(test_json)
}

#' Survey Solutions API call any user
#'
#'
#' Get user info
#'
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param uid user id
#'
#' @export
#'
suso_getUSR<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                          uid = NULL) {
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    url$path<-file.path("api", "v1", "users", uid)
    test_detail <- GET(url = build_url(url = url), authenticate(usr, pass, type = "basic"))
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
#'
#' @export
#'
suso_archUSR<-function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                      uid = NULL, archive = F) {
    ##  BASE URL
    url<-httr::parse_url(paste0(url))
    url$scheme<-"https"
    arch<-ifelse(archive, "archive", "unarchive")
    url$path<-file.path("api", "v1", "users", uid, arch)
    print(build_url(url = url))
    test_detail <- PATCH(url = build_url(url = url), authenticate(usr, pass, type = "basic"))
    check_response(test_detail, 200)
    test_json<-data.table(user=c(uid), ArchDate = as_datetime(Sys.time()), IsArchived = archive)
    return(test_json)
}

