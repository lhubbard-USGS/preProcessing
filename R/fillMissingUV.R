#' @title fillMissingUV
#'
#' @description Fill in missing UV data with Daily flow data
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
fillMissingUV <- function(UV) {
  dates <- unique(as.Date(UV$datetime))
  missingDay <- c()
  incompleteDay <- c()
  names(UV)[names(UV) == 'X_00060_00000'] <- 'Flow' #POTENTIAL ISSUE
  library(dplyr)
  UV$DATE <- as.Date(UV$datetime)
  startDate <- ""
  endDate <- ""
  DailyQ <- renameNWISColumns(readNWISdv(site,"00060",statCd="00003")) #startDate,endDate,
  DailyQ$DATE <- DailyQ$Date
  dailyFlow <- DailyQ[,c("DATE","Flow")]
  names(dailyFlow) <- c("DATE","EstFlow")
  dayUV <- left_join(UV, dailyFlow, copy=FALSE)
  dayUV$Flow[is.na(dayUV$Flow)] <- dayUV$EstFlow[is.na(dayUV$Flow)]
  dayUV$EstFlow <- NULL
  if(length(which(is.na(dayUV$datetime))) > 0){
    newLines <- as.POSIXct(paste(dayUV$DATE[is.na(dayUV$datetime)],"12:00:00"))
    attr(newLines, "tzone") <- "UTC"
    dayUV$datetime[is.na(dayUV$datetime)] <- newLines
  }
#'
  UV2 <- UV
  ##### Insert DV in days with all missing flow ####
  for(i in 2:length(dates)-1){
    checkday <- UV2[as.Date(UV2$datetime) == dates[i],]
    if(all(is.na(checkday$Flow))){
      UV2[as.Date(UV2$datetime) == dates[i],"Flow"] <- dayUV[as.Date(dayUV$datetime) == dates[i],"Flow"]
      missingDay <- c(missingDay, i)
    }
  }
}
