spearman_cor <- function(data=data, x=x, y=y){
  #is a dataframe?
  if(! is.data.frame(data))
    stop("'data' must be a dataframe")
  #is a column?
  x <- as.character(substitute(x))
  y <- as.character(substitute(y))
  if(! x  %in% colnames(data))
    stop(paste0(x, " - undefined columns selected"))
  if(! y  %in% colnames(data))
    stop(paste0(y , " - undefined columns selected"))
  
  require(dplyr)
  
  a <- data[,x]
  b <- data[,y]
  a_rank <-  rank(a, na.last="keep")
  b_rank <-  rank(b, na.last="keep")
  m_a <- mean(a_rank)
  m_b <- mean(b_rank)
  cov_t <- tibble(a_rank, b_rank) %>% 
    mutate(cov = (a_rank - m_a) * (b_rank - m_b))
  cov <- sum(cov_t$cov)/(nrow(cov_t) - 1)
  cov/((sd(a_rank))*(sd(b_rank)))
}