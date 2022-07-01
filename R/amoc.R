
amoc <- function(X_futu, X_curr, gamma, lambda, delta, eps = 0.001)
{
    part2local <- function(parti_vec)
    {
	N = length(parti_vec)
  	localization = c()
  	r = N
  	l = parti_vec[r]
  	localization = c(l, localization)
  	while(r > 0)
	{
	   r = l
    	   l = parti_vec[r]
    	   localization = c(l, localization)
  	}
  	return(localization[-1])
    }
  DP_result = rcpp_DP_VAR1(X_futu,X_curr,gamma=1,lambda=1,delta=5,eps=.001)
  result = append(DP_result, list(cpt = part2local(DP_result$partition)))
  return(result)
}