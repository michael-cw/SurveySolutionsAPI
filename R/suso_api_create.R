#' Survey Solutions API call for Assignment Creation
#'
#'
#' ATTENTION: NOT CHECKED YET
#' @export

suso_createASS <- function(df = UPLOADdataCHECK, url = suso_get_api_key("susoServer"),
                      usr = suso_get_api_key("susoUser"),
                      pass = suso_get_api_key("susoPass"),
                      QUID = NULL,
                      version = NULL) {
    ## LIB Load the libraries
    url <- ifelse(str_count(url, "https://") == 1, url, paste0("https://", url))
    ## API parameters
    url = paste0(url, "/api/v1/assignments")
    quid = paste0(QUID, "$", version)
    ## the post
    ResponsibleName <- df$ResponsibleName
    df[, `:=`(ResponsibleName, NULL)]
    status_list <- list()

    for (i in 1:nrow(df)) {
        print(ResponsibleName[i])
        js_ch <- list(Responsible = unbox(ResponsibleName[i]), Quantity = unbox(1), QuestionnaireId = unbox(quid), IdentifyingData = data.table(Variable = c(names(df)),
                                                                                                                                                Identity = rep("", length(names(df))), Answer = c(unlist(df[i, ], use.names = F))))
        test_post <- httr::POST(url = url, accept_json(), add_headers(charset = "utf-8"), authenticate(usr, pass, type = "basic"),
                                body = js_ch, encode = "json")
        ## ATTENTION: USE THE RESPONSE
        aJsonFile <- tempfile()
        writeBin(content(test_post, "raw"), aJsonFile)
        status_list[[i]] <- fromJSON(aJsonFile)
    }
    return(status_list)
}





































