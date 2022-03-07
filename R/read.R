#' lst:read_diabetes:one
#' Used as counter example
read_diabetes_patient <- function(path) {
  flog.info("Loading file %s", path)
  classes <- c('character','character','numeric','numeric')
  o <- read.delim(path, header=FALSE, colClasses=classes)
  colnames(o) <- c('date','time','feature','value')
  o$date <- as.Date(o$date, format='%m-%d-%Y')
  o
}


#read_diabetes <- function(#*#idx=1:70, base='private/diabetes'#*#){
#  #*#read_one <- function(i) {
#    path <- sprintf('%s/data-%02i',base,i)#*#
#    flog.info("Loading file %s", path)
#    classes <- c('character','character','numeric','numeric')
#    o <- read.delim(path, header=FALSE, colClasses=classes)
#    colnames(o) <- c('date','time','feature','value')
#    o$date <- as.Date(o$date, format='%m-%d-%Y')
#    #*#o$id <- i#*#
#    o
#  #*#}#*#
#  #*#do.call(rbind, lapply(idx, read_one))#*#
#}



#' Read the diabetes dataset
#'
#' Reads in all the individual patient files and consolidates into a single
#' data frame.
#'
#' @param base The base path of the diabetes files
#' @return A data.frame of the merged files
read_diabetes <- function(idx=1:70, base='private/diabetes'){
  read_one <- function(i) {
    path <- sprintf('%s/data-%02i',base,i)
    flog.info("Loading file %s", path)
    classes <- c('character','character','numeric','numeric')
    #*#o <- ftry(read.delim(path, header=FALSE, colClasses=classes),
      error=function(e) NULL)
    if (is.null(o)) return(NULL)#*#

    colnames(o) <- c('date','time','feature','value')
    o$date <- as.Date(o$date, format='%m-%d-%Y')
    o$id <- i
    o
  }
  do.call(rbind, lapply(idx, read_one))
}


#' Apply transformations to make diabetes usable
#'
#' @example
#' \dontrun{
#' df <- transform_diabetes(read_diabetes())
#' }
transform_diabetes <- function(raw) {
  df <- filter_events(raw)
  df$ts <- as_POSIX(df$date, df$time)
  flog.info("Removing %s bad timestamps", length(which(is.na(df$ts))))
  df <- df[!is.na(df$ts),]
  flog.info("Final size is %s rows", nrow(df))
  df
}



