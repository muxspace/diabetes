between <- function(date, inf, sup) {
  if (is.null(inf) && is.null(sup)) return(TRUE)
  if (is.null(inf)) return(date < as.Date(sup))
  if (is.null(sup)) return(date >= as.Date(inf))
  date >= as.Date(inf) & date < as.Date(sup)
}
