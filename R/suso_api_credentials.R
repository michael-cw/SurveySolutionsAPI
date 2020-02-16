#' Survey Solutions API credentials
#'
#'
#' (this function has been inspired by the googleway package \link{https://github.com/SymbolixAU/googleway},
#' an excellent package to use google geo-spatial API)
#' Retrieves the list of Survey Solutions credentials that have been set.
#'
#' @export
suso_keys <- function() getOption("SurveySolutionsAPI")


#' @export
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
#'
#' @details
#' Use \code{suso_set_key} to make API keys available for all the \code{suso_}
#' functions, so you don't need to specify the credentials parameter within those
#' functions.
#'
#'
#'
#' @export
suso_set_key <- function(
  suso_server = "",
  suso_user = "",
  suso_password = ""
) {

  options <- getOption("SurveySolutionsAPI")

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
#' @export
suso_get_api_key <- function(api = c("susoServer", "susoUser", "susoPass")) {


  ## Return value for selected API component
  api <- getOption("SurveySolutionsAPI")[['suso']][[api]]
  if(is.na(api)) return(suso_get_default_key(api))

  return(api)
}


suso_get_default_key <- function(api = c("susoServer", "susoUser", "susoPass")) {
  key <- getOption("SurveySolutionsAPI")[['suso']][[api]]
  if(is.na(key)) stop("No API credentials available Use either suso_set_key() to set a key, or provide it as a function argument directly")
  return(key)
}
