#' @title removeDups
#'
#' @description Remove duplicate date/times
#'
#' @importFrom dataRetrieval readNWISuv
#' @importFrom dataRetrieval renameNWISColumns
#' @importFrom dplyr mutate group_by summarise
#'
#' @export
#' @param datetime POSIXct date/time
#'
#' @examples
#' \dontrun{library(dataRetrieval)
#' site <- '040851385'
#' parameterCd <- '00060'
#' UVPList <- c("00010","00045","00060","00065","00095","00300","00400","63680","99133")
#' startDate <- "2015-09-01"
#' endDate <- "2015-09-10"
#' UV <- readNWISuv(site, UVPList, startDate="", endDate="")
#' UV_2 <- removeDups(UV)
#' }
#'
removeDups <- function(UV) {
  dup_index <- !duplicated(UV$datetime)
  UV <- UV[dup_index,]
}
