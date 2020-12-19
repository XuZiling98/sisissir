#' @title Simulation of SIS models using R
#' @description In the SIS model, an infected person may recover or die, and once recovered or died he or she will not be infected again.After the initial value of SIS model is given, the figure of the model is drawn and the simulation results are given.
#' @import deSolve
#' @param i0 The proportion of the population initially infected.
#' @param beta The average daily effective contact of infected persons.
#' @param gam The average rate at which an infected person recovers (or dies).
#' @param t_range a vector:Time step.If null,the default is t_range=seq(1,100,1).
#' @return a list including 'infe':Percentage of people infected;susc':Percentage of susceptible persons; 't.stable':The number of infected people reached a minimum period of stability; 'per.stable':The percentage of people infected at a stable level.
#' @examples
#' \dontrun{
#' SIS <- sis(i0=0.01,beta=1.2,gam=0.4,t_range=seq(0,100,1))
#' plot(SIS$infe,type='l',col='red',xlab = 'time' , ylab='percentage',
#' main='SIS model',ylim = c(0,1))
#'   lines(SIS$susc,type='l',col='blue')
#'   legend('right',c('infective percentage','susceptible percentage'),
#'   col=c('red','blue'),lty=c(1,1))
#' }
#' @export
sis <- function(i0,beta,gam,t_range=seq(0,100,1)){
  s0 <- 1-i0
  devi <- function(t,i,parms) list((beta-gam-beta*i)*i)
  infe <- ode(y = i0, times = t_range, func = devi, parms = NULL)
  susc <- cbind(t_range,1-infe[,2])

  if(infe[length(t_range),2]-infe[length(t_range)-1,2]==0){
    t.stable <- infe[infe[length(t_range),2]-infe[,2]<1e-6,][1,1]
    per.stable <- infe[t.stable,2]
  } else {
    t.stable <-'The time step is too short'
    per.stable <-'The time step is too short'
  }
  return(list(infe=infe, susc=susc,t.stable=t.stable,per.stable=per.stable))
}