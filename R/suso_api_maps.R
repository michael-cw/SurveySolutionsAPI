#' Upload map to server
#'
#' Allows the user to upload a zip file of background maps and boundary files
#'
#' @details Resources for upload must meet the requirements specified under:
#' \url{https://docs.mysurvey.solutions/headquarters/mapsmanage/map-formats/},
#' Attention: this uses the GraphQL API, not the REST API.
#'
#' @param server Survey Solutions server address
#' @param apiUser Survey Solutions API user
#' @param apiPass Survey Solutions API password
#' @param workspace server workspace, if nothing provided, defaults to primary
#' @param token If Survey Solutions server token is provided \emph{apiUser} and \emph{apiPass} will be ignored
#' @param path_to_zip either path to zip file or directory of files which will automatically be zipped as required
#'
#' @return Returns a data.table, with information about the uploaded maps
#'
#' @examples
#' \dontrun{
#' # either upload the content of a folder
#' suso_mapupload(workspace = "myworkspace",
#'               path_to_zip = "./mapfiles/")
#'
#' # or a zip file with the files included
#' suso_mapupload(workspace = "myworkspace",
#'               path_to_zip = "../mapfiles/maps.zip")
#' }
#'
#'
#'
#'
#' @export


suso_mapupload <- function(server= suso_get_api_key("susoServer"),
                           apiUser=suso_get_api_key("susoUser"),
                           apiPass=suso_get_api_key("susoPass"),
                           workspace = NULL,
                           token = NULL,
                           path_to_zip = NULL) {
  # Check parameters
  stopifnot(
    !is.null(server),
    !is.null(workspace),
    !is.null(path_to_zip)
  )

  fi<-file.info(path_to_zip)
  if(fi$isdir) {
    # directory to zip in temporary
    fl<-list.files(path_to_zip,
                   pattern = "(.shp)|(.shx)|(.dbf)|(.prj)|(.tpk)|(.tif)|(.tiff)|(.mmpk)",
                   full.names = T)
    tmpzip<-tempfile(fileext = ".zip")
    zip::zip(tmpzip, files = fl, mode = "cherry-pick")
    path_to_zip<-tmpzip

  } else if(!fi$isdir && tools::file_ext(path_to_zip)=="zip") {
    #path_to_zip

  } else {
    stop("File is neither a directory nor a .zip file")
  }

  url <- httr::parse_url(server)
  url$scheme <- "https"
  url$path<-"graphql"
  auth <- httr::authenticate(apiUser, apiPass, type = "basic")
  # define the mutation
  mutation<-glue::glue('{"query":"mutation(\\
  $file: Upload! $workspace: String) \\
  {uploadMap(file: $file workspace: $workspace) \\
  {xMaxVal\\
   yMaxVal\\
   xMinVal\\
   yMinVal\\
   wkid\\
   fileName\\
   size\\
   maxScale\\
   minScale\\
   shapeType\\
   importDateUtc\\
   uploadedBy\\
   users { userName }}}",\\
  "variables":{"file":null  "workspace": "<<workspace>>"}}',
                       .open = "<<", .close = ">>")

  # create the form
  files = list(
    `operations` = mutation,
    `map` = '{ "0": ["variables.file"] }',
    `0` = httr::upload_file(path_to_zip, type = "application/zip")
  )

  # send the post
  response <- httr::POST(httr::build_url(url),
                         body = files,
                         encode = "multipart",
                         httr::user_agent("r api v2"),
                         httr::accept_json(),
                         auth)
  # check the status code
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }

  result <- httr::content(response, "text", encoding = "UTF-8")
  result<-jsonlite::fromJSON(result)
  result<-data.table::data.table(result$data$uploadMap)
  if(nrow(result)>0) result[,importDateUtc:=lubridate::as_datetime(importDateUtc)][]
  return(result)
}




#' Receive maps currently uploaded to the server
#'
#' Allows the user to retrieve filtered or unfiltered map data.
#'
#' @details Attention: this uses the GraphQL API, not the REST API.
#'
#' @param server GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param apiUser your API username
#' @param apiPass API password
#' @param fileName name of the map on the server
#' @param importDateUtc Import date
#' @param size Size of the map
#' @param users Users to whom the maps are assigned
#' @param sortby_filename sort maps by file name, either ASC for ascending or DESC for descending
#' @param sortby_importeddateutc sort maps by import date in utc, either ASC for ascending or DESC for descending
#' @param sortby_size sort by map size, either ASC for ascending or DESC for descending
#' @param take take the specified integer numeber of maps
#' @param skip skip the first integer number of maps
#'
#' @return Returns a data.table, with all the maps and additonal information. If multiple users are assigned to a map,
#' the table is expanded, such that there is one user per map.
#'
#' @examples
#' \dontrun{
#' suso_mapinfo(workspace = "myworkspace")
#' }
#'
#' @export

suso_mapinfo <- function(server= suso_get_api_key("susoServer"),
                         apiUser=suso_get_api_key("susoUser"),
                         apiPass=suso_get_api_key("susoPass"),
                         workspace = NULL,
                         fileName = NULL,
                         importDateUtc = NULL,
                         size = NULL,
                         users = NULL,
                         sortby_filename = NULL,
                         sortby_importeddateutc = NULL,
                         sortby_size = NULL,
                         take = NULL,
                         skip = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(server)
  )

  # build the endpoint
  url <- httr::parse_url(server)
  url$scheme <- "https"
  url$path<-"graphql"

  # authentication
  auth<-httr::authenticate(apiUser, apiPass, type = "basic")


  # define your query
  query <- sprintf('
      query($workspace: String $where: MapsFilter $order: [MapsSort!] $take: Int $skip: Int) {
        maps(workspace: $workspace where: $where order: $order take: $take skip: $skip) {
              totalCount
              filteredCount
              nodes {
                fileName
                maxScale
                minScale
                shapeType
                shapesCount
                size
                importDateUtc
                uploadedBy
                users {
                  userName
                }
                xMaxVal
                yMaxVal
                xMinVal
                yMinVal
                wkid
              }
            }
      }
  ')
  ################################
  # create the variables list
  # 1. Top level
  variables <- list()
  if (!is.null(workspace)) {
    variables$workspace <- workspace
  }
  # 2. Filter
  # 2.1 Filter default is NULL
  variables$where<-NULL

  if (!is.null(fileName)) {
    variables$where$fileName$eq <- fileName
  }
  if (!is.null(importDateUtc)) {
    variables$where$importDateUtc$eq <- importDateUtc
  }

  if (!is.null(size)) {
    #size<-checkInput(size)
    variables$where$size$eq <- size
  }

  ## User must be updated!!
  if (!is.null(users)) {
    variables$where$users$all$userName$eq <- users
  }

  ## Sort
  if (!is.null(sortby_filename)) {
    stopifnot(
      sortby_filename %in% c("ASC", "DESC")
    )
    variables$order$fileName <- sortby_filename
  }

  if (!is.null(sortby_importeddateutc)) {
    stopifnot(
      sortby_importeddateutc %in% c("ASC", "DESC")
    )
    variables$order$importDateUtc <- sortby_importeddateutc
  }

  if (!is.null(sortby_size)) {
    stopifnot(
      sortby_size %in% c("ASC", "DESC")
    )
    variables$order$size <- sortby_size
  }

  if (!is.null(take)) {
    stopifnot(
      (take%%1==0)
    )
    variables$take <- take
  }

  if (!is.null(skip)) {
    stopifnot(
      (skip%%1==0)
    )
    variables$skip <- skip
  }

  # create the body of the request
  body <- list(query = query)
  if (!is.null(variables)) {
    body$variables <- variables
  }

  response <- httr::POST(httr::build_url(url),
                         body = body,
                         encode = "json",
                         httr::content_type_json(),
                         httr::user_agent("r api v1"),
                         httr::accept_json(),
                         auth)

  # check the status code
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }

  # parse the JSON response
  result <- httr::content(response, "text", encoding = "UTF-8")
  result<-jsonlite::fromJSON(result, flatten = T)
  # get counts and prepare for loop
  tc<-result$data$maps$totalCount
  fc<-result$data$maps$filteredCount
  if(fc>0){
  rc<-nrow(result$data$maps$nodes)
  resultdt<-result$data$maps$nodes
  # extract user list with tidyr::unnest
  resultdt<-tidyr::unnest(resultdt, "users", keep_empty = T)

  # create data.table
  resultdt<-data.table::data.table(resultdt)
  if(nrow(resultdt)>0) {
    #resultdt[, users:=gsub("c\\(|\\)", "",sapply(users, paste, collapse=""))]
    resultdt[,importDateUtc:=lubridate::as_datetime(importDateUtc)][]
  }
  } else if(fc==0) {
    resultdt<-data.table(NULL)
  }

  return(resultdt)
}


#' Assigns a map to a user
#'
#' Allows the user to assign a map to an interviewer to be used in CAPI data collection.
#'
#' @param server GraphQL endpoint of your server
#' @param workspace Server Workspace, if NULL uses default
#' @param apiUser your API username
#' @param apiPass API password
#' @param fileName the name of the map file on the server
#' @param userName the name of the interviewer to whom the map will be assigned to
#'
#'
#' @examples
#' \dontrun{
#' suso_mapassign(workspace = "myworkspace",
#'               fileName = "Lat9264Lon625_ALL.tif",
#'               userName = "INT0004")
#' }
#'
#' @export




suso_mapassign <- function(server= suso_get_api_key("susoServer"),
                           apiUser=suso_get_api_key("susoUser"),
                           apiPass=suso_get_api_key("susoPass"),
                           workspace = NULL,
                           fileName = NULL,
                           userName = NULL) {
  # define the endpoint for your GraphQL server
  stopifnot(
    !is.null(server)
  )

  # build the endpoint
  url <- httr::parse_url(server)
  url$scheme <- "https"
  url$path<-"graphql"

  # authentication
  auth<-httr::authenticate(apiUser, apiPass, type = "basic")
  # define your query
  query <- sprintf('
          mutation($workspace: String $fileName: String! $userName: String!) {
                addUserToMap(workspace: $workspace fileName: $fileName userName: $userName) {
                  fileName
                  size
                  maxScale
                  minScale
                  shapeType
                  shapesCount
                  importDateUtc
                  uploadedBy
                  users {
                    userName
                  }
                  xMaxVal
                  yMaxVal
                  xMinVal
                  yMinVal
                  wkid
                }
          }
  ')
  ################################
  # create the variables list
  # 1. Top level
  variables <- list()
  if (!is.null(workspace)) {
    variables$workspace <- workspace
  }

  if (!is.null(fileName)) {
    variables$fileName <- fileName
  }

  if (!is.null(userName)) {
    variables$userName <- userName
  }

  # create the body of the request
  body <- list(query = query)
  if (!is.null(variables)) {
    body$variables <- variables
  }



  response <- httr::POST(httr::build_url(url),
                         body = body,
                         encode = "json",
                         httr::content_type_json(),
                         httr::user_agent("r api v1"),
                         httr::accept_json(),
                         auth)


  # check the status code
  if (response$status_code != 200) {
    stop("Error: ", response$status_code)
  }

  # parse the JSON response
  result <- httr::content(response, "text", encoding = "UTF-8")
  result<-jsonlite::fromJSON(result)
  result<-result$data$addUserToMap
  result<-data.table::data.table(
    fileName=result$fileName,
    user=userName,
    shapeType=result$shapeType,
    importDateUtc=lubridate::as_datetime(result$importDateUtc)
  )
  return(result)
}





