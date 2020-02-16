.onLoad <- function(...){
  ## taken from googleway package
  ##  creates options list

  if(is.null(getOption("SurveySolutionsAPI"))) {

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
}
