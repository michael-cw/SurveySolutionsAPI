#' Survey Solutions API credentials
#'
#'
#' (this function has been inspired by the googleway package
#' \url{https://github.com/SymbolixAU/googleway},
#' an excellent package to use google geo-spatial API)
#' Retrieves the list of Survey Solutions credentials that have been set.
#'
#' @export
#' @import readr
#' @import stringr
suso_keys <- function() getOption("SurveySolutionsAPI")


print.suso_api <- function(x, ...) {

  for (i in 1:length(x)) {

    cat("Survey Solutions API credentials\n")

    for (j in 1:length(x[[i]])){
      cat(" - ", names(x[[i]])[j], ": ")
      key <- x[[i]][[j]]
      cat(ifelse(is.na(key), "", key), "\n")
    }
  }
}

#' Set Credentials
#'
#' Sets API credentials so it's available for all API calls. See details
#'
#' @param suso_server Survey Solutions server address
#' @param suso_user Survey Solutions API user
#' @param suso_password Survey Solutions API password
#' @param suso_token If Survey Solutions server token is provided \emph{suso_user} and \emph{suso_password} will be ignored
#'
#' @details
#' Use \code{suso_set_key} to make API keys available for all the \code{suso_}
#' functions, so you don't need to specify the credentials parameter within those
#' functions. The server address can be provided with or without https:\\ suffix,
#' nevertheless if it is missing, then the suffix will be added.
#'
#' In case \emph{suso_token} is provided, only token authentication will be attempted. For details on token authentication
#' in Survey Solutions please see \url{https://docs.mysurvey.solutions/headquarters/accounts/token-based-authentication/}.
#'
#'
#'
#' @export
#'
suso_set_key <- function(
  suso_server = "",
  suso_user = "",
  suso_password = "",
  suso_token = ""
) {
  # get options
  options <- getOption("SurveySolutionsAPI")
  # sanitize string to ssl (http?)
  suso_server<-ifelse(stringr::str_count(suso_server, "https://")==1,
                      suso_server, paste0("https://", suso_server))
  # add to object
  options[['suso']][['susoServer']] <- suso_server
  options[['suso']][['susoUser']] <- suso_user
  options[['suso']][['susoPass']] <- suso_password
  class(options) <- "suso_api"
  options(SurveySolutionsAPI = options)
  invisible(NULL)

}


#' Clear Credentials
#'
#' Clears all the API credentials
#'
#' @export
suso_clear_keys <- function() {

  options <- list(
    suso = list(
      susoServer = NA_character_,
      susoUser = NA_character_,
      susoPass = NA_character_
    )
  )
  attr(options, "class") <- "suso_api"
  options(SurveySolutionsAPI = options)

}

#' Get credentials
#'
#' Get API credentials
#'
#' Get credentials, used as input in API calls
#'
#' @param api One of susoServer, susoUser, susoPass
#'
#' @import data.table
#'
#' @export
#'
suso_get_api_key <- function(api = c("susoServer", "susoUser", "susoPass")) {


  ## Return value for selected API component
  api <- getOption("SurveySolutionsAPI")[['suso']][[api]]
  if(is.na(api)) return(suso_get_default_key(api))

  return(api)
}

#' Checks if credentials are present
#'
#' Helper function
#'
#' @param api one of susoServer, susoUser or susoPass
#'
#' @export
suso_get_default_key <- function(api = c("susoServer", "susoUser", "susoPass")) {
  key <- getOption("SurveySolutionsAPI")[['suso']][[api]]
  if(is.na(key)) stop("No API credentials available Use either suso_set_key() to set a key, or provide it as a function argument directly", call. = F)
  return(key)
}


#' Utility function to check if credentials are correct
#'
#' This function returns a 200 status if the correct credentials have been provided. If credentials are correct but
#' user is not eligible to access the workspace, then a 403 error is returned.
#'
#' @param server Survey Solutions Server
#' @param apiUser API user
#' @param apiPass API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#'
#'
#' @export
suso_PwCheck<-function(server=suso_get_api_key("susoServer"),
                       apiUser=suso_get_api_key("susoUser"),
                       apiPass=suso_get_api_key("susoPass"),
                       workspace = NULL,
                       token = NULL) {
  ## workspace default
  workspace<-.ws_default(ws = workspace)
  ## Define the api
  url <- httr::parse_url(url = server)
  url$scheme <- "https"
  url$path <- file.path(workspace, "api", "v1", "supervisors")
  url$query<-"limit=200"
  ## Authentication
  auth<-authenticate(apiUser, apiPass, type = "basic")
  print(build_url(url))
  ## Request
  test_detail<-tryCatch(
    {GET(url = build_url(url),
         auth)},
    error=function(e) {a<-data.frame(status_code=400); return(a)}
  )
  return(test_detail)
}




















