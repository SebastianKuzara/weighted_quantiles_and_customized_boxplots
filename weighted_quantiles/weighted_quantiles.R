

weightedQuantiles <- function(x, probs=c(0, 0.25, 0.5, 0.75, 1.0), weights=NULL, na.rm=TRUE) {
  
  require(data.table)
  x <- copy(x)
  
  if( is.null(weights) ) {
    weights <- rep(1, times = length(x) )
  }
  
  dt <- data.table(
    Values = x,
    Weights = weights
  )
  if( any(dt[["Weights"]]<=0.0) ) {
    warning("There are weights less or equal to zero. Those rows will be removed.")
    dt <- dt[Weights>0]
  }
  if( na.rm ) {
    dt <- dt[!is.na(Values)]
  }
  stopifnot( all(!is.na(dt[["Values"]])) )
  
  setorder(dt, Values)
  dt[,WeightsCum:=cumsum(Weights)/sum(Weights) ]
  
  results <- getQuantiles(dt, probs)
  
  return(results)
  
}



getQuantiles <- function(x, probs) {
  
  x <- copy(x)
  
  quantile_results <- data.table(
    Quantile=probs,
    Value=NA_real_
  )
  
  for( q in probs ) {
    bound_cum_weights <- searchQuantileProbs(x, prob = q)
    
    if( length(bound_cum_weights)==1 ) {
      val <- x[WeightsCum==bound_cum_weights][["Values"]]
      quantile_results[Quantile==q, Value:=val]
    } else {
      sel_rows <- x[WeightsCum%in%bound_cum_weights]
      w <- abs(bound_cum_weights-q)
      val <- sel_rows[1][["Values"]] * w[2] + sel_rows[2][["Values"]] * w[1]
      val <- val/sum(w)
      quantile_results[Quantile==q, Value:=val]
      rm(w, sel_rows)
    }
    rm(bound_cum_weights,val)
  }
  
  return(quantile_results)
  
}



searchQuantileProbs <- function(x, prob) {
  
  x <- copy(x)
  
  stopifnot(is.data.table(x))
  stopifnot(identical( names(x), c("Values", "Weights", "WeightsCum") ))
  
  lower_prob <- max( c(x[WeightsCum<=prob][["WeightsCum"]], min(x[["WeightsCum"]]) ) )
  upper_prob <- min( c(x[WeightsCum>=prob][["WeightsCum"]], max(x[["WeightsCum"]]) )  )
  values <- unique( c(lower_prob, upper_prob) )
  # values <- removeNAOrInf(values)
  
  return( values )
}



removeNAOrInf <- function(x) {
  x <- x[!is.na(x)&!is.infinite(x)]
  return(x)
}














