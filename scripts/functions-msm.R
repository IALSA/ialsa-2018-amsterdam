# ---- define-support-functions ----------------------
get_crude_Q <- function(
  ds
  ,Q
  ,cov_names
){
  formula_ <- as.formula(paste0("state ~ ",cov_names))
  Q_crude <- crudeinits.msm(
    formula = formula_, 
    subject = id, 
    qmatrix = Q, 
    data = ds,     
    censor        = c(-1,-2),
    censor.states = list(c(1,2,3), c(1,2,3)) 
  )
  return(Q_crude)
}

msm_summary <- function(
  model
){
  cat("\n-2loglik =", model$minus2loglik,"\n")
  cat("Convergence code =", model$opt$convergence,"\n")
  p    <- model$opt$par
  p.se <- sqrt(diag(solve(1/2*model$opt$hessian)))
  print(cbind(p=round(p,digits),
              se=round(p.se,digits),"Wald ChiSq"=round((p/p.se)^2,digits),
              "Pr>ChiSq"=round(1-pchisq((p/p.se)^2,df=1),digits)),
        quote=FALSE)
}

msm_details <- function(
  model
){ 
  # intensity matrix
  cat("\n Intensity matrix : \n")
  print(qmatrix.msm(model)) 
  # qmatrix.msm(model, covariates = list(male = 0))
  # transition probability matrix
  t_ <- 2
  cat("\n Transition probability matrix for t = ", t_," : \n")
  print(pmatrix.msm(model, t = t_)) # t = time, in original metric
  # misclassification matrix
  cat("\n Misclassification matrix : \n")
  suppressWarnings(print(ematrix.msm(model), warnings=F))
  # hazard ratios
  # cat("\n Hazard ratios : \n")
  # print(hazard.msm(model))
  # mean sojourn times
  cat("\n Mean sojourn times : \n")
  print(sojourn.msm(model))
  # probability that each state is next
  cat("\n Probability that each state is next : \n")
  suppressWarnings(print(pnext.msm(model)))
  # total length of stay
  cat("\n  Total length of stay : \n")
  print(totlos.msm(model))
  # expected number of visits to the state
  cat("\n Expected number of visits to the state : \n")
  suppressWarnings(print(envisits.msm(model)))
  # ratio of transition intensities
  # qratio.msm(model,ind1 = c(2,1), ind2 = c(1,2))
}

# adds neat styling to your knitr table
neat <- function(x){
  # knitr.table.format = "html"
  x_t <- x %>%
    # x %>%
    # knitr::kable() %>%
    knitr::kable(format="html") %>%
    kableExtra::kable_styling(
      bootstrap_options = c("striped", "hover", "condensed","responsive"),
      # bootstrap_options = c( "condensed"),
      full_width = F,
      position = "left"
    )
  # cat("\n",x_t,"\n")
  # print(x_t, n=n_)
  return(x_t)
}
# ds %>% distinct(id) %>% count() %>% neat(10)
