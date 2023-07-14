dpwu <- function(
    x,
    time, 
    rate){
  
  num <- stats::stepfun(x = time, y = c(0, rate, 0))(x) 
  
  num / sum(time[-1] * rate)
  
}

