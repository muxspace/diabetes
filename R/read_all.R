# Original in read.R
read_diabetes_patient <- function(path) {
  flog.info("Loading file %s", path)
  classes <- c('character','character','numeric','numeric')
  o <- read.delim(path, header=FALSE, colClasses=classes)
  colnames(o) <- c('date','time','feature','value')
  o$date <- as.Date(o$date, format='%m-%d-%Y')
  o
}


read_diabetes <- function(idx=1:70, base='private/diabetes'){
  read_one <- function(i) {
    path <- sprintf('%s/data-%02i',base,i)
    o <- read_diabetes_patient(path)
    o$id <- i
    o
  }
  do.call(rbind, lapply(idx, read_one))
}
