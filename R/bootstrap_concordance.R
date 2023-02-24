#' @description Functions for comparing the concordance of two models train on the same data using the bootstrap method


#' @rdname bootstrap_concordance_ev
#' @title concordance_bootstrap_ev
#' @name bootstrap_concordance_ev
#' @description Uses a BS sample for an external dataset comparing two models
#' @param fit1 The first model
#' @param fit2 The second model
#' @param method The class of the model e.g. "lm" or "coxph"
#' @param data the data
#' @param samples the number of bootstrap samples
#' @returns a vector with the mean difference, bootstrap std dev, t value and p value
#' @export
concordance_bootstrap_ev=function(fit1, fit2, method, data, samples=1000)
{

  newdata=cbind(data, #the bs sample
                predict.fit1=predict(fit1, newdata=data), #the predictions from the model, these will be some monotonic function of the linear predictor
                predict.fit2=predict(fit2, newdata=data))

  formula1=fit1$call$formula
  formula2=fit2$call$formula
  #getting the concordance based on the new predictions but existing outcomes

  formula1a=as.formula(paste(rlang::f_lhs(formula1), "~predict.fit1",sep=""))
  formula2a=as.formula(paste(rlang::f_lhs(formula2), "~predict.fit2",sep=""))

  out=concordance_bootstrap(formula1a, formula2a, method, data=newdata, samples=1000)

  res=out %>% as.data.frame() %>%
    rename("fit1"="V1",
           "fit2"="V2") %>%
    mutate(diff=fit1-fit2)
  check=abs(mean(res$diff))/sd(res$diff)

  ret=c("Difference"=mean(res$diff), "sd"=sd(res$diff), "t"=check, "p"=2*(1-pnorm(check)))
  return(ret)
}

#' @rdname bootstrap_concordance_cv
#' @title concordance_bootstrap_cv
#' @description Uses a BS sample for comparing two models in a cross validation
#' @param formula1 The formula for the first model
#' @param formula2 The formula for the second model
#' @param method The class of the model e.g. "lm" or "coxph"
#' @param data the data
#' @param samples the number of bootstrap samples
#' @param p the proportion of data in the main fold and 1-p is used for the validation sample.
#' @returns a vector with the mean difference, bootstrap std dev, t value and p value
#' @export
concordance_bootstrap_cv=function(formula1, formula2, method, data, p=0.8, samples=200)
{
  out=matrix(0, ncol=2, nrow=samples)
  for(i in 1:samples)
  {
    # print(i)
    #you might want that to check progress because it is quite slow

    #bootstrap sample
    index=caret::createDataPartition(1:nrow(data), p=p, list=FALSE, times=1)
    fit1=do.call(method, args = list(formula=formula1, data=data[index,] ))
    fit2=do.call(method, args = list(formula=formula2, data=data[index,]))

    newdata=cbind(data[-index,], #the bs sample
                  predict.fit1=predict(fit1, newdata=data[-index,]), #the predictions from the model, these will be some monotonic function of the linear predictor
                  predict.fit2=predict(fit2, newdata=data[-index,]))

    #getting the concordance based on the new predictions but existing outcomes

    formula1a=as.formula(paste(rlang::f_lhs(formula1), "~predict.fit1",sep=""))
    formula2a=as.formula(paste(rlang::f_lhs(formula2), "~predict.fit2",sep=""))
    out[i,]=concordance_bootstrap( formula1a, formula2a, method, newdata, samples=1)
  }
  res=out %>% as.data.frame() %>%
    rename("fit1"="V1",
           "fit2"="V2") %>%
    mutate(diff=fit1-fit2)
  check=abs(mean(res$diff))/sd(res$diff)

  ret=c("Difference"=mean(res$diff), "sd"=sd(res$diff), "t"=check, "p"=2*(1-pnorm(check)))
  return(ret)

}

# Helpers -----------------------------------------------------------------

concordance_bootstrap=function( formula1, formula2, method, data, samples=1000)
{

  out=matrix(0, ncol=2, nrow=samples)
  for(i in 1:samples)
  {
    test=data %>% sample_n(nrow(data), replace=T)

    x=do.call(method, args = list(formula=formula1, data=test )) %>% survival::concordance()
    y=do.call(method, args = list(formula=formula2, data=test ))%>% survival::concordance()



    out[i,]=c(x$concordance,y$concordance)
  }

  return(out)
}
