% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suso_api_maps.R
\name{suso_mapassign}
\alias{suso_mapassign}
\title{Assigns a map to a user}
\usage{
suso_mapassign(
  server = suso_get_api_key("susoServer"),
  apiUser = suso_get_api_key("susoUser"),
  apiPass = suso_get_api_key("susoPass"),
  workspace = NULL,
  fileName = NULL,
  userName = NULL
)
}
\arguments{
\item{server}{GraphQL endpoint of your server}

\item{apiUser}{your API username}

\item{apiPass}{API password}

\item{workspace}{Server Workspace, if NULL uses default}

\item{fileName}{the name of the map file on the server}

\item{userName}{the name of the interviewer to whom the map will be assigned to}
}
\description{
Allows the user to assign a map to an interviewer to be used in CAPI data collection.
}
\examples{
\dontrun{
suso_mapassign(workspace = "myworkspace",
              fileName = "Lat9264Lon625_ALL.tif",
              userName = "INT0004")
}

}
