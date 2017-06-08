#' @title fillIn
#'
#' @description Fill in any missing rows so that dataset is a complete 5-min dataset
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
fillIn <- function(UV) {
  dateSeq <- data.frame(datetime=seq.POSIXt(min(trunc.POSIXt(UV$datetime, "day"),na.rm = TRUE),
                                            max(UV$datetime,na.rm = TRUE), by = 5*60))
  UV <- merge(dateSeq, UV, all=TRUE)
}
