#' @title Simulation of SIR models using R
#' @description In the SIR model, an infected person may recover or die, and once recovered or died he or she will not be infected again.After the initial value of SIR model is given, the figure of the model is drawn and the simulation results are given.
#' @import deSolve
#' @param i0 The proportion of the population initially infected.
#' @param r0 The proportion of the population initially recovering or dying.
#' @param beta The average daily effective contact of infected persons.
#' @param gam The average rate at which an infected person recovers (or dies).
#' @param t_range a vector:Time step.If null,the default is t_range=seq(1,100,1).
#' @return a list including 'infe':Percentage of people infected;susc':Percentage of susceptible persons; 'reco':Percentage of people who recover (or die);t.peak':The peak of the number of infected people; 't.stop':The fastest time when the number of infected people is close to zero. 
#' @examples
#' \dontrun{
#' SIR <- sir(i0=0.01,r0=0,beta=1.2,gam=0.4,t_range=seq(0,100,1))
#' plot(SIR$infe,type='l',col='red',xlab = 'time' , ylab='percentage',
#' main='SIR model',ylim = c(0,1))
#'   lines(SIR$susc,type='l',col='blue')
#'   lines(SIR$reco,type='l',col='green')
#'   legend('right',c('infective percentage','susceptible percentage',
#'   'recovery percentage'),col=c('red','blue','green'),lty=c(1,1,1))
#' }
#' @export
sir <- function(i0,r0,beta,gam,t_range=seq(0,100,1)){
  s0 <- 1-i0-r0
  devr <- function(t,r,parms) list(gam*(1-r-s0*exp(-beta*r/gam)))
  reco <- ode(y = r0, times = t_range, func = devr, parms = NULL)
  s <- s0*exp(-beta*reco[,2]/gam)
  susc <- cbind(t_range,s0*exp(-beta*reco[,2]/gam))
  infe <- cbind(t_range,pmax(1-susc[,2]-reco[,2],0))
  t.peak <- infe[which.max(infe[,2]),1]  
  if(infe[length(t_range),2]<1e-8) t.stop <- infe[infe[,2]<1e-8,][1,1] else 
    t.stop <-'The time step is too short'
  return(list(infe=infe, susc=susc, reco=reco,t.peak=t.peak,t.stop=t.stop))
}