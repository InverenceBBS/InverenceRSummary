################################################################################
# FILE: /code/INV/InverenceRSummary/R/fields_info.R
# PURPOSE: Extract fields information from a data frame and other related
#          functions.
################################################################################

require(lubridate)
require(DescTools)

###############################################################################
#' Convert all factors to strings in a data frame
#'
#' @description Convert all factors to strings in a data frame
#'
#' @param df A data.frame
#'
#' @return A data.frame
#'
ForceAllFactorAsString <- function(df)
###############################################################################
{
  isfctr <- sapply(df, is.factor)
  df[isfctr] <- lapply(df[isfctr], function(f) { as.character(f) })
  return(df)
}

###############################################################################
#' Convert string fields to factors in a data frame
#'
#' @description Convert string fields to factors in a data frame when they have no more than max_distinct_values distinct values
#'
#' @param sdf A data.frame
#'
#' @return A data.frame
#'
SmartStringAsFactor <- function(sdf, max_distinct_values=100)
###############################################################################
{
  fdf = sdf
  n = ncol(fdf)
  m = nrow(fdf)
  for(f in 1:n) {
    if(is.character(fdf[[f]])) {
      u = length(unique(fdf[[f]]))
      if(u<max_distinct_values) {
        fdf[[f]] <- as.factor(fdf[[f]])
      }
    }
  }
  return(fdf)
}


#############################################################################
#' Extract fields information and some statistics from a data frame
#'
#' @description Extract fields information and some statistics from a data frame
#'
#' @param data A data.frame
#'
#' @return A data.frame with the next structure
#'  $ id          : chr  the field name
#'  $ type        : chr  the field type
#'  $ missing     : int  number of missing values
#'  $ miss_pcn    : int  percent of missing values
#'  $ zero_pcn    : int  percent of zero values in numeric fields
#'  $ nlevels     : int  number of distinct values
#'  $ entropyRatio: num  entropy ratio
#'  $ mean        : chr  mean value when it has sense
#'  $ sd          : chr  standard value when it has sense..
#'  $ min         : chr  minimum value
#'  $ q001        : chr  00.1% percentile
#'  $ q010        : chr  10.0% percentile
#'  $ q100        : chr  25.0% percentile
#'  $ q250        : chr  50.0% percentile
#'  $ q500        : chr  75.0% percentile
#'  $ q750        : chr  75.0% percentile
#'  $ q900        : chr  90.0% percentile
#'  $ q990        : chr  99.0% percentile
#'  $ q999        : chr  99.9% percentile
#'  $ max         : chr  minimum value
#'
GetFieldsInfo <- function(data, digits=4, id_fields=NULL)
#############################################################################
{
  #data=prod_ing
  #str(data)
  pxxx = c(0.001,0.010,0.100,0.250,0.500,0.750,0.900,0.990,0.999)
  fullEntropy = Entropy(as.numeric(1:nrow(data)))
  ColStats <- function(j) {
    #j=8
    cat(paste0('[',j,'] summary of ',class(data[,j]),' ',colnames(data)[j],'\n'))
    v=data[,j]
    #str(v)
    zeroes = NaN
    no_missing = sum(!is.na(v))
    missing = sum(is.na(v))
    is_num = is.numeric(v)
    is_dte = is.Date(v) | is.POSIXt(v)
    if(is.character(v)) {
      v = factor(v,exclude=NULL) }
    if(is.Date(v)) {
      nlevels = length(unique(v))
    } else if(is.POSIXt(v)) {
      w = strftime(v,"%Y-%m-%d")
      v = as.Date(w)
      nlevels = length(unique(w))
      rm(w)
    } else if(is.factor(v)) {
      nlevels = nlevels(v)
      v= suppressWarnings(as.numeric(v))
    } else if(is.logical(v)) {
      nlevels = length(unique(v))
      v=suppressWarnings(as.numeric(v))
      is_num=TRUE
    } else if(is.integer(v)) {
      nlevels = length(unique(v))
      zeroes = sum(v==0,na.rm = TRUE)
    } else if(is.double(v)) {
      nlevels = length(unique(round(v,digits=digits)))
      zeroes = sum(v==0,na.rm = TRUE)
    }
    if(nlevels>1) {
      entropyRatio = Entropy(v[!is.na(v)])/fullEntropy
    } else {
      entropyRatio = 0
    }
    if(is_num) {
      qxxx=round(unname(quantile(v,pxxx,na.rm = TRUE)),digits)
      st = c(
        type = class(data[,j])[1],
        missing = missing,
        miss_pcn = round(missing/nrow(data),4)*100,
        zero_pcn = round(zeroes/nrow(data),4)*100,
        nlevels = nlevels,
        entropyRatio = round(entropyRatio,digits),
        #distinct=length(unique(v)),
        mean = round(mean(v,na.rm = TRUE),digits),
        sd = round(sd(v,na.rm = TRUE),digits),
        min= round(min(v,na.rm = TRUE),digits),
        q001=qxxx[1],  q010=qxxx[2], q100=qxxx[3],
        q250=qxxx[4],  q500=qxxx[5], q750=qxxx[6],
        q900=qxxx[7],  q990=qxxx[8], q999=qxxx[9],
        max=round(max(v,na.rm = TRUE),digits)   )
    } else if(is_dte) {
      qxxx = strftime(unname(quantile(v,pxxx,na.rm = TRUE, type = 1)))
      st = c(
        type = class(data[,j])[1],
        missing = missing,
        miss_pcn = round(missing/nrow(data),4)*100,
        zero_pcn = NA,
        nlevels = nlevels,
        entropyRatio = round(entropyRatio,digits),
        #distinct=length(unique(v)),
        mean = strftime(mean(v,na.rm = TRUE)),
        sd = NA,
        min=strftime(min(v,na.rm = TRUE)),
        q001=qxxx[1],  q010=qxxx[2], q100=qxxx[3],
        q250=qxxx[4],  q500=qxxx[5], q750=qxxx[6],
        q900=qxxx[7],  q990=qxxx[8], q999=qxxx[9],
        max=strftime(max(v,na.rm = TRUE))   )
    } else {
      v = rep(NaN,nrow(data))
      st = c(
        type = class(data[,j])[1],
        missing = missing,
        miss_pcn = round(missing/nrow(data),4)*100,
        zero_pcn = round(zeroes/nrow(data),4)*100,
        nlevels = nlevels,
        entropyRatio = round(entropyRatio,digits),
        #distinct=length(unique(v)),
        mean = NaN,
        sd = NaN,
        min  = substr(min(data[,j],na.rm = TRUE),1,16),
        q001 = NaN, q010 = NaN, q100 = NaN,
        q250 = NaN, q500 = NaN, q750 = NaN,
        q900 = NaN, q990 = NaN, q999 = NaN,
        max = substr(max(data[,j],na.rm = TRUE),1,16)   )
    }
    # toc()
    rm(v)
    gc()
  }
  stats = as.data.frame(do.call(rbind, lapply(1:ncol(data), function(j){
    suppressWarnings(ColStats(j))
  })), stringsAsFactors = FALSE)
  stats = cbind(id=colnames(data),stats)
  rownames(stats) = NULL
  if(!is.null(id_fields)) {
    stats = cbind(data.frame(id_fields),stats)
  }
  stats = ForceAllFactorAsString(stats)
  stats$missing  = as.integer(stats$missing)
  stats$miss_pcn  = as.integer(stats$miss_pcn)
  stats$zero_pcn  = as.integer(stats$zero_pcn)
  stats$nlevels  = as.numeric(stats$nlevels)
  stats$entropyRatio = as.numeric(stats$entropyRatio)
  stats
}
