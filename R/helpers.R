# Check hex colours
#
# Checks for valid hexadecimal value
#
# @param response response from \code{httr::GET()}
# @param status valid status, i.e. 200, 300
check_response <- function(response = test_detail, status = 200){
  if (status_code(response)!=status) {
    stop("Invalid request! Please check your input parameters.", call. = F)
  }
}
