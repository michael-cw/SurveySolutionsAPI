#' Survey Solutions API call for User Creation
#'
#'
#' Creates Survey Solutions users (observers, interviewers or supervisors).
#'
#' @param userlist dataframe with upload data
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param showUser userName in console
#'
#'
#' @details Dataframe needs to be provided with the mandatory columns
#' for user creation which are: Role, UserName, Password and Supervisor (in case of interviewer),
#' optional you can also provide FullName, PhoneNumber and Email. Return value is a data.table, which includes
#' the user information as well as the response's status code. Important is also that the UserName and Password are
#' provided in the required format.
#'
#'
#' @export
#'
#'

suso_createUSER <- function(userlist = NULL,
                            server = suso_get_api_key("susoServer"),
                            apiUser = suso_get_api_key("susoUser"),
                            apiPass = suso_get_api_key("susoPass"),
                            workspace = NULL,
                            token = NULL,
                            showUser = T) {
  ## workspace default
  workspace<-.ws_default(ws = workspace)
  ############
  # 1. create variables
  ##  BASE URL
  url<-httr::parse_url(paste0(server))
  url$scheme<-"https"
  url$path<-file.path(workspace,"api", "v1", "users")
  # Set the authentication
  auth<-httr::authenticate(apiUser, apiPass, type = "basic")

  ## Check user file
  ## 1. check mandatory variables
  vn<-names(userlist)
  stopifnot(
    sum(c("Role", "UserName", "Password", "Supervisor") %in% vn)==4
  )

  ## 2. check optional variables
  if(!("FullName" %in% vn)) {
    userlist$FullName<-character(nrow(userlist))
  }
  if(!("PhoneNumber" %in% vn)) {
    userlist$PhoneNumber<-character(nrow(userlist))
  }
  if(!("Email" %in% vn)) {
    userlist$Email<-character(nrow(userlist))
  }

  ## 3. Preparation for call
  aJsonFile<-tempfile()
  status_list <- list()

  for(i in 1:nrow(userlist)){
    if(showUser) print(userlist$UserName[i])
    #jsonlite::toJSON(((userlist[i, .SD, .SDcols = names(userlist)])), auto_unbox = T)
    js_ch<-list(
      Role=jsonlite::unbox(userlist$Role[i]),
      UserName=jsonlite::unbox(userlist$UserName[i]),
      FullName=jsonlite::unbox(userlist$FullName[i]),
      PhoneNumber=jsonlite::unbox(userlist$PhoneNumber[i]),
      Password=jsonlite::unbox(userlist$Password[i]),
      Supervisor=jsonlite::unbox(userlist$Supervisor[i])
    )
    test_post<-httr::POST(url = httr::build_url(url),
                          httr::accept_json(),
                          httr::add_headers(charset="utf-8"),
                          auth,
                          body=js_ch,
                          encode = "json",
                          httr::write_disk(aJsonFile, overwrite = T))
    if(test_post$status_code==400 | test_post$status_code==200) {
      tmp<-jsonlite::fromJSON(aJsonFile)
      tmp<-data.table::data.table(t(unlist(tmp)))
      tmp<-cbind(tmp, userlist[i,])
      tmp$status_code<-test_post$status_code
      status_list[[i]]<-tmp
    } else {
      next()
    }
  }
  status_list<-rbindlist(status_list, fill = T)
  return(status_list)
}

