#' Survey Solutions API call for questionnaire
#'
#'
#' \code{suso_getQuestDetails} implements all Questionnaire API commands. If \emph{operation.type = structure}
#' the return file will be a list, whith the content of the raw json string containing the questionnaire variables
#' @param url Survey Solutions server address
#' @param usr Survey Solutions API user
#' @param pass Survey Solutions API password
#' @param quid \emph{QuestionnaireId} for which details should be exported
#' @param version questionnaire version
#' @param operation.type if \emph{list} is specified a list of all questionnaires on the server. If
#' \emph{statuses} a vector of all questionnaire statuses. If \emph{structure} is specified, it returns a list
#' containing the content of the questionnaire's json string. To make use of it, put the output into
#' \code{suso_transform_fullMeta} and receive a data.table with all questionnaire elements. If \emph{interviews}, all interviews
#' for a specific questionnaire.
#' @export
#'
#' @import data.table

suso_getQuestDetails <- function(url=suso_get_api_key("susoServer"), usr = suso_get_api_key("susoUser"), pass = suso_get_api_key("susoPass"),
                                 quid = NULL, version = NULL, operation.type = c("list", "statuses", "structure", "interviews")) {
    ## Set temporary file
    aJsonFile <- tempfile(fileext = ".json")
    ## Define the api
    url <- parse_url(url)
    url$scheme <- "https"
    url$path <- "/api/v1/questionnaires"
    ## Default operation type is 'list'
    operation.type = ifelse(is.null(operation.type), "list", operation.type)

    # 1. Get all Questionnaires on the server
    if (operation.type == "list") {
        url$query <- list(limit = 40)
        test_detail <- GET(url = build_url(url), authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
        test_json <- fromJSON(aJsonFile)
        qTot <- test_json$TotalCount
        ## 1.1. Check if more than 40, append rest
        if (qTot > 40) {
            qTotRest <- qTot
            repCalls <- ceiling(qTotRest/40)
            for (i in 2:repCalls) {
                url$query <- list(limit = 40, offset = i)
                test_detail <- GET(url = build_url(url), authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
                test_json_tmp <- fromJSON(aJsonFile)
                test_json$Questionnaires <- rbind(test_json$Questionnaires, test_json_tmp$Questionnaires)
            }
        }
        # Only data.table of interviews is returned
        test_json<-data.table(test_json$Questionnaires)
        # Set date time to utc with lubridate
        test_json[,LastEntryDate:=as_datetime(LastEntryDate)][]
        return(test_json)
        # 2. Get all STATUS on server
    } else if (operation.type == "statuses") {
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, "statuses")), authenticate(usr, pass, type = "basic"),
                           write_disk(aJsonFile, overwrite = T))
        test_json <- fromJSON(aJsonFile)

        # 3. Get Questionnaire JSON string as list
    } else if (operation.type == "structure") {
        if (is.null(quid) | is.null(version))
            stop("Quid and/or version missing.")
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "document")), authenticate(usr,
                                                                                                                      pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
        test_json <- tidyjson::read_json(aJsonFile)
        test_json <- suso_transform_fullMeta(test_json)
        # 4. Get INTERVIEWS for specific questionnaires
    } else if (operation.type == "interviews") {
        if (is.null(quid) | is.null(version))
            stop("Quid and/or version missing.")
        url$query <- list(limit = 40)
        test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "interviews")),
                           authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
        test_json <- fromJSON(aJsonFile)
        qTot <- test_json$TotalCount
        ## 1.1. Check if more than 40, append rest
        if (qTot > 40) {
            qTotRest <- qTot
            repCalls <- ceiling(qTotRest/40)
            for (i in 2:repCalls) {
                url$query <- list(limit = 40, offset = i)
                test_detail <- GET(url = modify_url(url, path = file.path(url$path, quid, version, "interviews")),
                                   authenticate(usr, pass, type = "basic"), write_disk(aJsonFile, overwrite = T))
                test_json_tmp <- fromJSON(aJsonFile)
                test_json$Interviews <- rbind(test_json$Interviews, test_json_tmp$Interviews)
            }
        }
        # Only data.table of interviews is returned
        test_json<-data.table(test_json$Interviews)
        # Set date time to utc with lubridate
        test_json[,LastEntryDate:=as_datetime(LastEntryDate)][]
    }
    return(test_json)
    ########################## F I N#########################################################
}


#' Helper function to transform list of questionnair structure to single data.table with all variables
#'
#' Uses tidyjson package
#'
#' \code{suso_transform_fullMeta} transforms the list containing the structure (\emph{operation.type = structure})
#' is transformed into a single data.table
#' with all variable names, types etc.. This also works with json strings manually exported from the server.
#'
#'
#' @param input returned by \code{suso_getQuestDetails} structure operation
#'
#'
#' @export
#' @import tidyjson
#'
#' @import data.table

suso_transform_fullMeta <- function(input = NULL) {
    ##########################################
    ## v2.0 with tidyjson rewritten

    qfinal <-bind_rows(
        ######################################################
        ## first
        input %>% spread_values(
            Id = jstring("Id"),
            LastEntryDate = jstring("LastEntryDate")
        ) %>% enter_object("Children") %>% gather_array("L0") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title")
            ),
        ## second
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")

            ),
        ## third
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## fourth
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## fifth
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## sixth
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            enter_object("Children") %>% gather_array("L5") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## seventh
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            enter_object("Children") %>% gather_array("L5") %>%
            enter_object("Children") %>% gather_array("L6") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            ),
        ## eight
        input %>% enter_object("Children") %>% gather_array("L0") %>%
            enter_object("Children") %>% gather_array("L1") %>%
            enter_object("Children") %>% gather_array("L2") %>%
            enter_object("Children") %>% gather_array("L3") %>%
            enter_object("Children") %>% gather_array("L4") %>%
            enter_object("Children") %>% gather_array("L5") %>%
            enter_object("Children") %>% gather_array("L6") %>%
            enter_object("Children") %>% gather_array("L7") %>%
            spread_values(
                type = jstring("$type"),
                PublicKey = jstring("PublicKey"),
                Title = jstring("Title"),
                VariableName = jstring("VariableName"),
                QuestionScope = jnumber("QuestionScope"),
                QuestionText = jstring("QuestionText"),
                Featured = jlogical("Featured")
            )
        ###########################################################
    ) %>% select_if(col_selector)
    qfinal<-data.table(qfinal)
    qfinal<-qfinal[,document.id:=NULL][]
    return(qfinal)
}






################ Testing not run qestStruct<-getQuestDetails(quid = 'b4c78852-c1d7-4532-ba3f-7cc35ead489a', version = 5, operation.type
################ = 'structure') a<-fullMeta()

# b<-rbindlist(a, idcol = 'section') ## to check q47 qestList<-data.table(getQuestDetails()$Questionnaires)
# fullMETAlist<-list() for (q in 1:nrow(qestList)) { print(q) tmp_q<-qestList[q] qestStruct<-getQuestDetails(quid =
# tmp_q$QuestionnaireId, version = tmp_q$Version, operation.type = 'structure')
# fullMETAlist[[qestStruct$Title]]<-fullMeta(qestStruct) }
