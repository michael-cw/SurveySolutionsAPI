% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/suso_api_user.R
\name{suso_getINT_info}
\alias{suso_getINT_info}
\title{Survey Solutions API call for info on interviewers}
\usage{
suso_getINT_info(
  url = suso_get_api_key("susoServer"),
  usr = suso_get_api_key("susoUser"),
  pass = suso_get_api_key("susoPass"),
  int_id = NULL,
  workspace = NULL,
  token = NULL
)
}
\arguments{
\item{url}{Survey Solutions server address}

\item{usr}{Survey Solutions API user}

\item{pass}{Survey Solutions API password}

\item{int_id}{interviewer id}

\item{workspace}{If workspace name is provide requests are made regarding this specific workspace, if
no workspace is provided defaults to primary workspace.}

\item{token}{If Survey Solutions server token is provided \emph{usr} and \emph{pass} will be ignored}
}
\description{
Get details of interviewers
}
\examples{
\dontrun{
suso_getINT_info(
          workspace = "myworkspace",
          int_id = "xxxx-xxxx-xxxx-xxx"
          )
}

}
