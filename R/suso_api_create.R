#' Survey Solutions API call for Assignment Creation
#'
#'
#' Creates assignment with prefilled data.
#'
#' @param df dataframe with upload data
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param QUID the questionnaire id
#' @param version the questionnaire version
#' @details Dataframe needs to be provided with columns
#' for prefilling data, as well as \emph{Quantity} and \emph{ResponsibleName}. Return value is a data.table,
#' with the prefilling data, if successful
#' @export
#'
#'

suso_createASS <- function(df = UPLOADdataCHECK,
                           server = suso_get_api_key("susoServer"),
                           apiUser = suso_get_api_key("susoUser"),
                           apiPass = suso_get_api_key("susoPass"),
                           QUID = NULL,
                           version = NULL) {
    ############
    # 1. create variables
    ##  BASE URL
    url<-httr::parse_url(paste0(server))
    url$scheme<-"https"
    url$path<-file.path("api", "v1", "assignments")
    usr<-apiUser
    pass<-apiPass
    ## Preparation for call
    aJsonFile<-tempfile()
    quid=paste0(QUID,"$", version)
    ResponsibleName <- df$ResponsibleName
    Quantity <- df$Quantity
    df[, `:=`(ResponsibleName, NULL)][, `:=`(Quantity, NULL)]
    status_list <- list()

    for(i in 1:nrow(df)){
        print(ResponsibleName[i])
        js_ch<-list(Responsible=unbox(ResponsibleName[i]),
                    Quantity=unbox(1),
                    QuestionnaireId=unbox(quid),
                    IdentifyingData=data.table(Variable=c(names(df)),
                                               Identity=rep("", length(names(df))),
                                               Answer=c(unlist(df[i,],
                                                               use.names = F))))
        test_post<-httr::POST(url = build_url(url),
                              accept_json(),add_headers(charset="utf-8"),
                              authenticate(usr, pass, type = "basic"),
                              body=js_ch, encode = "json",
                              httr::write_disk(aJsonFile, overwrite = T))
        httr::stop_for_status(test_post, "Assignment creation failed")
        tmp<-fromJSON(aJsonFile)
        tmp<-data.table(t(unlist(tmp)))
        status_list[[i]]<-tmp
    }
    status_list<-rbindlist(status_list)
    return(status_list)
}

