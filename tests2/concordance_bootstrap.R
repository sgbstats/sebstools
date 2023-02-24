library(sn)
library(tidyverse)
library(truncnorm)
library(survival)
library(MASS)

#genrating some data
plarate=function(eos)
{
  exa=case_when(eos<0.1~4*eos+0.5,
                eos<0.2~3*eos+0.6,
                eos<0.3~2*eos+0.8,
                eos<0.4~eos+1.1,
                T~0.5*eos+1.3)
  return(exa)
}




rsnt=function(n, min=0, max=NULL,xi=0.065, omega=0.175, alpha=5.5)
{
  x1=rsn(2*n, xi=xi, omega = omega, alpha=alpha)
  if(!is.null(min)&is.null(max))
  {
    x= x1[x1>min][1:n]
  }else if(is.null(min)&!is.null(max))
  {
    x= x1[x1<max][1:n]
  }else if(!is.null(min)&!is.null(max))
  {
    x= x1[x1>min][x1<max][1:n]
  }
  return(x)
}


id=seq(from=1, to=1000)
trt=c(rep("ICS", 500),rep("PLA", 500))
eos=rsnt(1000)
age=rnorm(1000, 70, 10)

data=data.frame(id, trt, eos, age) %>% group_by(id) %>%
  #singh et al 2020
  mutate(exa=case_when(trt=="ICS"~rpois(1, lambda=0.6+age*0.1),
                       trt=="PLA"~rpois(1, lambda=plarate(eos)+age*0.1))) %>%
  ungroup()
fit1=MASS::glm.nb(exa~trt*eos, data=data)
fit2=MASS::glm.nb(exa~trt*eos+age, data=data)

id=seq(from=1, to=200)
trt=c(rep("ICS", 100),rep("PLA", 100))
eos=rsnt(200)
age=rnorm(200, 70, 10)
data2=data.frame(id, trt, eos, age) %>% group_by(id) %>%
  #singh et al 2020
  mutate(exa=case_when(trt=="ICS"~rpois(1, lambda=0.6+age*0.1),
                       trt=="PLA"~rpois(1, lambda=plarate(eos)+age*0.1))) %>%
  ungroup()

data3=cbind(data2, fit1=predict(fit1, newdata=data2),fit2=predict(fit2, newdata=data2))


concordance_bootstrap_ev(fit1, fit2, data = data2, method = "glm.nb", samples = 100)



