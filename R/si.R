#' @title Simulation of SI models using R
#' @description In SI models, once infected, it remains infected.After the initial value of SI model is given, the figure of the model is drawn and the simulation results are given.
#' @import deSolve
#' @param i0 The proportion of the population initially infected.
#' @param beta The average daily effective contact of infected persons.
#' @param t_range a vector:Time step.If null,the default is t_range=seq(1,100,1).
#' @return a list including 'infe':Percentage of people infected per day; 'susc':Percentage of susceptible persons per day; 'I':The total number of people infected; 'S':The total number of people susceptible; 'in.time':The fastest time to get all infected. 
#' @examples
#' \dontrun{
#' SI <- si(i0=1e-4,beta=1.2,t_range=seq(0,100,1))
#' plot(SI$infe,type='l',col='red',xlab = 'time' , ylab='percentage',
#' main='SI model')
#'   lines(SI$susc,type='l',col='blue')
#'   legend('right',c('infective percentage','susceptible percentage'),
#'   col=c('red','blue'),lty=c(1,1))
#' }
#' @export
si <- function(i0,beta,t_range=seq(1,100,1)){
  devi <- function(t,i,parms) list(beta * i *(1-i))
  infe <- ode(y = i0, times = t_range, func = devi, parms = NULL)
  susc <- cbind(t_range,1-infe[,2])
  if(infe[length(t_range),2]==1) in.time <- infe[infe[,2]>=1,][1,1] else 
    in.time <-'The time step is too short'
  return(list(infe=infe, susc=susc,in.time=in.time))
}