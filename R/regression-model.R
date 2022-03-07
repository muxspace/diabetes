

#' Example 3.6 (Section 3.3.1, Automated pipelines)
#' @return A fit model
train_glucose_pipeline <- function(model.out='private/glucose.RData', ...) {
  df <- transform_diabetes(read_diabetes(...))
  glucose <- construct_glucose_features(df)
  pair <- split_glucose(glucose)
  model <- fit_glucose(pair$train)
  save(model, file=model.out)
  list(model=model, train=pair$train, test=pair$test, raw=df)
}





#' Example 3.7 (Section 3.3.1, Automated pipelines)
#' @return The prediction
predict_glucose_pipeline <- function(data, model, pred.out) {
  df <- transform_diabetes(data)
  glucose <- construct_glucose_features(df)
  pred <- predict_glucose(glucose, model)

  pred.1 <- read.csv(pred.out, stringsAsFactors=FALSE)
  write.csv(rbind(pred.1,pred), file=pred.out, row.names=FALSE)
  pred
}


#' Construct glucose features
#'
#' Use vectorization to collect deltas
#'
#' Example 3.6 (Section 3.3.2, Error handling)
#' Route error messages to a file
construct_glucose_features <- function(df) {
  glucose <- c(58,60,62)
  fn <- function(panel) {
    panel <- panel[order(panel$ts),]
    features <- with(panel[panel$feature %in% glucose,], {
      flog.info("Work on patient %s", panel$id[1])
      glucose <- value
      t1.time <- diff(ts)
      t1.diff <- diff(value)
      t2.time <- diff(ts, 2)
      t2.diff <- diff(value, 2)
      if (panel$id == 22) browser()
      list(glucose=glucose[-c(1,2)],
        t1.time=t1.time[-1], t1.diff=t1.diff[-1],
        t2.time=t2.time, t2.diff=t2.diff)
    })

    if (any(sapply(features,length) == 0)) return(NULL)
    features$id <- panel$id[1]
    as.data.frame(features)
  }
  do.call(rbind, by(df, df$id, fn))
}


#' Construct glucose features
#'
#' Use vectorization to collect deltas
#'
#' Example 3.3 (Section 3.2, Model design)
construct_glucose_features <- function(df) {
  glucose <- c(58,60,62)
  fn <- function(panel) {
    panel <- panel[order(panel$ts),]
    features <- with(panel[panel$feature %in% glucose,], {
      flog.info("Work on patient %s", panel$id[1])
      glucose <- value
      t1.time <- diff(ts)
      t1.diff <- diff(value)
      t2.time <- diff(ts, 2)
      t2.diff <- diff(value, 2)
      list(glucose=glucose[-c(1,2)],
        t1.time=t1.time[-1], t1.diff=t1.diff[-1],
        t2.time=t2.time, t2.diff=t2.diff)
    })

    if (any(sapply(features,length) == 0)) {
      flog.warn("Skipping patient %s with no glucose measurements",panel$id[1])
      return(NULL)
    }
    features$id <- panel$id[1]
    as.data.frame(features)
  }
  do.call(rbind, by(df, df$id, fn))
}



#' Construct glucose features
#'
#' Use a rolling map operation to collect deltas
#construct_glucose_features <- function(df) {
#}


#' Create a training and test set for the glucose data
#' 
#' Verify in a test with sapply(pair, nrow)
#'
#' Example 3.3 (Section 3.2, Model design)
split_glucose <- function(df, test.ratio=.2, seed=NULL) {
  if (!is.null(seed)) set.seed(seed)
  is.test <- sample(c(TRUE,FALSE), nrow(df),
    replace=TRUE, prob=c(test.ratio, 1-test.ratio))
  list(train=df[!is.test,], test=df[is.test,])
}

#' Fit glucose measurement
#'
#' Explain glucose level based on previous glucose measurements
#'
#' Example 3.3 (Section 3.2, Model design)
#' @example
#' \dontrun{
#' df <- transform_diabetes(read_diabetes())
#' glucose <- construct_glucose_features(df)
#' pair <- split_glucose(glucose)
#' model <- fit_glucose(pair$train)
#' }
fit_glucose <- function(df) {
  randomForest(glucose ~ t1.time + t1.diff + t2.time + t2.diff, df)
}


#' Predict glucose measurement for new data
#' 
#' Generally useful to provide consistent behavior for one or many entries.
#' This is easy to do with vectorization.
#'
#' Example 3.3 (Section 3.2, Model design)
#' @example
#' \dontrun{
#' df <- transform_diabetes(read_diabetes())
#' glucose <- construct_glucose_features(df)
#' pair <- split_glucose(glucose)
#' model <- fit_glucose(pair$train)
#' pred <- predict_glucose(pair$test, model)
#' }
predict_glucose <- function(x, model) {
  predict(model, newdata=x)
}



#' Example 3.3 (Section 3.2, Model design)
#' @example
#' \dontrun{
#' pred <- predict_glucose(pair$test, model)
#' err <- error_glucose(pred, pair$test$glucose)
#' hist(err$pct.error)
#' }
error_glucose <- function(pred, real) {
  pct.error <- (pred - real) / real
  rmse <- sqrt(sum((pred - real)^2) / length(real))
  list(rmse=rmse, pct.error=pct.error)
}


#' Example 3.8 (Section 3.4, Reporting and visualization)
error_glucose <- function(pred, real) {
  pct.error <- (pred - real) / real
  rmse <- sqrt(sum((pred - real)^2) / length(real))
  list(rmse=rmse, mpe=mean(pct.error), mae=mean(abs(pct.error)),
    pct.error=pct.error)
}



#' @example
#' \dontrun{
#' pred <- predict_glucose(pair$test, model)
#' err <- error_glucose(pred, pair$test$glucose)
#'
#' pdf('11-glucose_pct_error.pdf', width=8, height=6)
#' plot_glucose_error(err)
#' dev.off()
#' }
plot_glucose_error <- function(err) {
  hist(err$pct.error, breaks='scott', main='Glucose prediction error',
    xlab='Percent error')
}





#' Construct glucose features
#'
#' Fix the issues associated with time dependence
#'
#' Example 3.4 (Section 3.2, Model design)
construct_glucose_features <- function(df) {
  glucose <- c(58,60,62)
  fn <- function(panel) {
    panel <- panel[order(panel$ts),]
    features <- with(panel[panel$feature %in% glucose,], {
      flog.info("Work on patient %s", panel$id[1])
      glucose <- value[-c(1,2)]
      t1.time <- diff(ts)[-1]
      t1.val <- value[-c(1,length(value))]
      t2.time <- diff(ts, 2)
      t2.val <- value[-(length(value) - (1:2))]
      t2.diff <- t1.val - t2.val
      list(glucose=glucose,
        t1.time=t1.time, t1.val=t1.val,
        t2.time=t2.time, t2.val=t2.val, t2.diff=t2.diff)
    })

    if (any(sapply(features,length) == 0)) {
      flog.warn("Skipping patient %s with no glucose measurements",panel$id[1])
      return(NULL)
    }
    features$id <- panel$id[1]
    as.data.frame(features)
  }
  do.call(rbind, by(df, df$id, fn))
}


#' Fit glucose measurement
#'
#' Adjust model to use new featurs
#'
#' Example 3.4 (Section 3.2, Model design)
#' @example
#' \dontrun{
#' df <- transform_diabetes(read_diabetes())
#' glucose <- construct_glucose_features(df)
#' pair <- split_glucose(glucose)
#' model <- fit_glucose(pair$train)
#' }
fit_glucose <- function(df) {
  cols <- setdiff(colnames(df), c("id"))
  randomForest(glucose ~ ., df[,cols])
}



# Example 3.9
predict_glucose_pipeline <- function(data, model, pred.out) {
  flog.appender(appender.tee('errors.log'))
  on.exit(flog.appender(appender.console()))

  df <- transform_diabetes(data)
  glucose <- construct_glucose_features(df)
  
  glucose$group <- 1:nrow(glucose) %% 10
  pred <- do.call(rbind, lapply(1:10, function(g) {
    flog.info("Predicting on group %s",g)
    o <- NULL
    ftry(o <- predict_glucose(glucose[glucose$group==g,], model),
      error=function(e) return())
    o
  }))

  pred.1 <- read.csv(pred.out, stringsAsFactors=FALSE)
  write.csv(rbind(pred.1,pred), file=pred.out, row.names=FALSE)
  pred
}




train_glucose_pipeline <- function(model.out='private/glucose.RData', ...) {
  flog.info("Read diabetes training set")
  df <- transform_diabetes(read_diabetes(...))
  flog.info("Construct features")
  glucose <- construct_glucose_features(df)
  pair <- split_glucose(glucose)
  flog.info("Fit model")
  model <- fit_glucose(pair$train)
  flog.info("Save model to %s (currently in %s)", model.out, getwd())
  save(model, file=model.out)
  list(model=model, train=pair$train, test=pair$test, raw=df)
}

