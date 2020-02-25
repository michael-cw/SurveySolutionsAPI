#' Survey Solutions API call for Assignment Creation
#'
#'
#' ATTENTION: CURRENTLY ONLY TESTED WITHOUT PREFILLED DATA
#'
#' @param df dataframe with upload data
#' @param url Survey Solutions server address
#' @param user Survey Solutions API user
#' @param pass Survey Solutions API password
#' @details df must be with the following columns: \emph{ResponsibleName, Quantity, [variables]}
#' @export
#'
#'

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
    Quantity <- df$Quantity
    df[, `:=`(ResponsibleName, NULL)][, `:=`(Quantity, NULL)]
    status_list <- list()

    for (i in 1:nrow(df)) {
        # js_ch <- list(Responsible = unbox(ResponsibleName[i]), Quantity = unbox(1), QuestionnaireId = unbox(quid), IdentifyingData = data.table(Variable = c(names(df)),
        #                                                                                                                                         Identity = rep("", length(names(df))), Answer = c(unlist(df[i, ], use.names = F))))
        js_ch <- list(Responsible = (ResponsibleName[i]), Quantity = Quantity[i], QuestionnaireId = (quid), use.names = F)
        test_post <- httr::POST(url = url, accept_json(), add_headers(charset = "utf-8"), authenticate(usr, pass, type = "basic"),
                                body = js_ch, encode = "json")
        if (status_code(test_post)==200) {
            status_list[[ResponsibleName[i]]]<-data.table(Responsible = (ResponsibleName[i]), Quantity = Quantity[i], QuestionnaireId = (quid))
        }
    }
    status_list<-rbindlist(status_list)
    return(status_list)
}





































