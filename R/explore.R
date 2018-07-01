
################################## CHAPTER 2 #################################
# Section 2.2.2

#' Show the number of events per patient
#'
#' @examples
#' \dontrun{
#' df <- read_diabetes()
#' get_event_table(df)
#' }
get_event_table <- function(df) {
  table(df$id, df$feature)
}


# Features 33, 34, 58, 60, 62 are fairly dense
# 33: Regular insulin dose
# 34: NPH insulin dose
# 58: Pre-breakfast blood glucose measurement
# 60: Pre-lunch blood glucose measurement
# 62: Pre-supper blood glucose measurement

#' Distribution of duration of study participation in days
#'
#' @param df Diabetes data frame produced by \link{read_diabetes}
#' @examples
#' \dontrun{
#' plot_patient_duration(get_patient_duration(df))
#' }
get_patient_duration <- function(df) {
  tapply(df$date, df$id, function(date) max(date) - min(date))
}


# Section 2.2.3

#' Filter out unused events
#'
#' @param df Diabetes data frame produced by \link{read_diabetes}
#' @examples
#' \dontrun{
#' df <- filter_events(df)
#' }
filter_events <- function(df) {
  keep <- c(33,34,58,60,62)
  df[df$feature %in% keep,]
}


#' Get the times associated with events for a given feature
#'
#' @param df Diabetes data frame produced by \link{read_diabetes}
#' @param feature The feature to get times for
#' @param id Limit to a specific patient
#' @examples
#' \dontrun{
#' table(get_event_times(df, 58))
#' }
get_event_times <- function(df, feature, id=NULL) {
  if (is.null(id))
    df$time[df$feature==feature]
  else
    df$time[df$feature==feature & df$id==id]
}


# Some times are 0-padded, so to normalize, the date and time values should
# be converted to a POSIX datetime.
#' Create a POSIX timestamp from a date and time
#'
#' Some entries might be NA due to bad times
as_POSIX <- function(date, time) {
  strptime(sprintf("%sT%s", date,time), format='%Y-%m-%dT%H:%M')
}

# Section 2.4
#' Normalize times to a standard set
#'
#' @param df Diabetes data frame produced by \link{read_diabetes}
normalize_times <- function(df) {
}



################################ CHAPTER 3 ###################################

plot_patient_duration <- function(pd) {
  hist(pd, main='Patient duration in days', xlab='duration (days)')
}

plot_duration_vs_events <- function(df) {
  de <- tapply(df$date, df$id,
    function(date) c(max(date) - min(date), length(date)) )
  # What's the structure of de? Not sure, check in interpreter
}

# de is a list
plot_duration_vs_events <- function(df) {
  de <- tapply(df$date, df$id,
    function(date) c(max(date) - min(date), length(date)) )
  de <- do.call(rbind, de)
  plot(de, xlab='duration (days)', ylab='events',
    main="Number of events vs. duration of participation")
}

# Add in a filter for specific feature
plot_duration_vs_events <- function(df, feature=NULL) {
  if (!is.null(feature)) df <- df[df$feature==feature,]
  de <- tapply(df$date, df$id,
    function(date) c(max(date) - min(date), length(date)) )
  de <- do.call(rbind, as.list(de))
  plot(de, xlab='duration (days)', ylab='events',
    main="Number of events vs. duration of participation")
  text(de, labels=unique(df$id), pos=1)
}


plot_glucose_and_insulin <- function(df, id, inf=NULL, sup=NULL) {
  opar <- par(mfrow=c(2,1))
  on.exit(par(opar))
  glucose <- c(58,60,62)
  insulin <- c(33,34)
  with(df[df$id==id & df$feature %in% glucose & between(df$date,inf,sup),], {
    plot(ts, value, type='l', col='gray', main='Glucose (mg)')
    points(ts, value, col=feature)
  })
  with(df[df$id==id & df$feature %in% insulin & between(df$date,inf,sup),], {
    plot(ts, value, type='l', col='gray', main='Insulin (units)')
  })
}
