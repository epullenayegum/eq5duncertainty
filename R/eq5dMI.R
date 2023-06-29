#' Create 5-digit code indicating health state & look up in allstates
#'
#' @param data The data containing the EQ-5D-5L responses
#' @param dimnames A character vector of lenth 5 giving the names in the dataset of the columns corresponding to the responses to mobility, self-care, usual activities, pain/discomfort and anxiety/depression, respectively. Defaults to c("MO","SC","UA","PD","AD")
#' @param statename The name to be given to the 5-digit state created by the function
#' @param stateidname The name to be given to the state id column created by the function
#' @return the original dataset provided plus two extra columns: one named statename with value equal to the five-digit code capturing the response to the EQ-5D-5L questionnaire, and another named stateidname, capturing the state id (i.e., a number from 1 to 3125) of the respondent's health state
#' @export
#' @examples

#' # simulate a dataset
#'  EQdata.sim<-array(dim=c(500,5))
#' colnames(EQdata.sim)<- c("MO", "SC", "UA", "PD", "AD")
#' EQdata.sim <- as.data.frame(EQdata.sim)
#' for (i in 1:nrow(EQdata.sim)){  EQdata.sim[i,] <- floor(runif(5, min=1, max=5))}
#' EQdata.sim$age <- round(abs(sample(rnorm(500, mean=65, sd=25))))
#' EQdata.sim$cancer.site <- as.factor(sample(c("breast", "prostate", "colorectal", "other"), size = 500, replace = T, prob = c(0.21, 0.19, 0.12, 0.48)))
#' EQdata.sim$sex <- ifelse(EQdata.sim$cancer.site=="breast","female","male")
#' head(EQdata.sim)
#' EQdata.sim <- merge.dim(data=EQdata.sim,dimnames=c("MO","SC","UA","PD","AD"),statename="state",allstates=allstates,stateidname)
#' head(EQdata.sim)

merge.dim <- function(data,dimnames=c("MO","SC","UA","PD","AD"),statename,allstates=allstates,stateidname){
  
  state <- data[,names(data)%in%dimnames[1]]*10000+ data[,names(data)%in%dimnames[2]]*1000+ data[,names(data)%in%dimnames[3]]*100+ data[,names(data)%in%dimnames[4]]*10 + data[,names(data)%in%dimnames[5]]
  d <- cbind(data,state)
  names(d)[ncol(d)] <- statename
  
  d <- cbind(d,allstates$idnum[match(d[,ncol(d)],allstates$state)])
  names(d)[ncol(d)] <- stateidname
  return(d)
}



#' Add utility to data
#' @param data The data containing the EQ-5D-5L responses
#' @param dimnames A character vector of lenth 5 giving the names in the dataset of the columns corresponding to the responses to mobility, self-care, usual activities, pain/discomfort and anxiety/depression, respectively. Defaults to c("MO","SC","UA","PD","AD")
#' @param utilityname The name to be given to the utility corresponding to each health state
#' @param stateidname The name to be given to the state id column created by the function
#' @param impnum The imputation number from mu.pred3125 to use when looking up the utilities
#' @return the original dataset provided plus one extra column named utilityname giving the utility for each corresponding to the health state
#' @export
#' @examples

#' # simulate a dataset
#'  EQdata.sim<-array(dim=c(500,5))
#' colnames(EQdata.sim)<- c("MO", "SC", "UA", "PD", "AD")
#' EQdata.sim <- as.data.frame(EQdata.sim)
#' for (i in 1:nrow(EQdata.sim)){  EQdata.sim[i,] <- floor(runif(5, min=1, max=5))}
#' EQdata.sim$age <- round(abs(sample(rnorm(500, mean=65, sd=25))))
#' EQdata.sim$cancer.site <- as.factor(sample(c("breast", "prostate", "colorectal", "other"), size = 500, replace = T, prob = c(0.21, 0.19, 0.12, 0.48)))
#' EQdata.sim$sex <- ifelse(EQdata.sim$cancer.site=="breast","female","male")
#' head(EQdata.sim)
#' EQdata.sim <- add.utility(data=EQdata.sim,dimnames=c("MO","SC","UA","PD","AD"),utilityname="utility",impnum=1)
#' head(EQdata.sim)

add.utility <- function(data,dimnames=c("MO","SC","UA","PD","AD"),utilityname,allstates=allstates,mu.pred=mu.pred3125,impnum){
  state <- data[,names(data)%in%dimnames[1]]*10000+ data[,names(data)%in%dimnames[2]]*1000+ data[,names(data)%in%dimnames[3]]*100+ data[,names(data)%in%dimnames[4]]*10 + data[,names(data)%in%dimnames[5]]
  
  stateid <- allstates$idnum[match(state,allstates$state)]
  #  data <- cbind(d,mu.pred[impnum,data[,names(data)%in%stateidname]])
  data <- cbind(data,mu.pred[impnum,stateid])
  names(data)[ncol(data)] <- utilityname
  return(data)
}


#' use a single imputation to impute utilities, and apply a user-defined function to the resulting dataset
#' @param impnum The imputation number to use
#' @param data The data containing the EQ-5D-5L responses
#' @param userfx The function to be applied to the imputed utilities
#' @param dimnames A character vector of lenth 5 giving the names in the dataset of the columns corresponding to the responses to mobility, self-care, usual activities, pain/discomfort and anxiety/depression, respectively. Defaults to c("MO","SC","UA","PD","AD")
#' @param statename The name to be given to the 5-digit state created by the function
#' @param stateidname The name to be given to the state id column created by the function
#' @param utilityname The name to be given to the utility corresponding to each health state
#' @param impnum The imputation number from mu.pred3125 to use when looking up the utilities
#' @return The result of applying userfx to the dataset using a single imputed value set
#' @export
#' @examples

#' # simulate a dataset
#'  EQdata.sim<-array(dim=c(500,5))
#' colnames(EQdata.sim)<- c("MO", "SC", "UA", "PD", "AD")
#' EQdata.sim <- as.data.frame(EQdata.sim)
#' for (i in 1:nrow(EQdata.sim)){  EQdata.sim[i,] <- floor(runif(5, min=1, max=5))}
#' EQdata.sim$age <- round(abs(sample(rnorm(500, mean=65, sd=25))))
#' EQdata.sim$cancer.site <- as.factor(sample(c("breast", "prostate", "colorectal", "other"), size = 500, replace = T, prob = c(0.21, 0.19, 0.12, 0.48)))
#' EQdata.sim$sex <- ifelse(EQdata.sim$cancer.site=="breast","female","male")
#' head(EQdata.sim)
#' 
#' mean.sd.fn <- function(data){
#' mean.sd <- c(mean(data$utility), sd(data$utility)/sqrt(nrow(data)))
#' return(list(estimate=mean.sd[1],se=mean.sd[2]))
#' }

#' singleimp(impnum=1,data=EQdata.sim,userfx=mean.sd.fn,dimnames=c("MO","SC","UA","PD","AD"),statename="state",stateidname="stateid",utilityname="utility")



singleimp <- function(impnum,data, userfx,dimnames,statename,stateidname,utilityname,allstates=allstates,mu.pred=mu.pred3125,...){
  # col <- (1:ncol(newdata))[as.numeric(colnames(newdata))%in%utilityname] # find which column is called idnum containing the state number, returns a number
  
  
  data <- merge.dim(data=data,dimnames=c("MO","SC","UA","PD","AD"),statename=statename,allstates,stateidname=stateidname)
  data <- add.utility(data,dimnames=c("MO","SC","UA","PD","AD"),utilityname=utilityname,allstates,mu.pred=mu.pred,impnum=impnum)
  ans <- userfx(data,...)
  return(ans)
}



#' Create multiply imputed utilities, apply a user-defined function, and combine the results using Rubin's Rules 

#' @param data The data containing the EQ-5D-5L responses
#' @param userfx The function to be applied to the imputed utilities
#' @param dimnames A character vector of lenth 5 giving the names in the dataset of the columns corresponding to the responses to mobility, self-care, usual activities, pain/discomfort and anxiety/depression, respectively. Defaults to c("MO","SC","UA","PD","AD")
#' @param statename The name to be given to the 5-digit state created by the function
#' @param stateidname The name to be given to the state id column created by the function
#' @param utilityname The name to be given to the utility corresponding to each health state
#' @param numimp The number of imputations to use
#' @return The result of using multiple imputation to apply userfx to the data
#' @export
#' @examples

#' # simulate a dataset
#'  EQdata.sim<-array(dim=c(500,5))
#' colnames(EQdata.sim)<- c("MO", "SC", "UA", "PD", "AD")
#' EQdata.sim <- as.data.frame(EQdata.sim)
#' for (i in 1:nrow(EQdata.sim)){  EQdata.sim[i,] <- floor(runif(5, min=1, max=5))}
#' EQdata.sim$age <- round(abs(sample(rnorm(500, mean=65, sd=25))))
#' EQdata.sim$cancer.site <- as.factor(sample(c("breast", "prostate", "colorectal", "other"), size = 500, replace = T, prob = c(0.21, 0.19, 0.12, 0.48)))
#' EQdata.sim$sex <- ifelse(EQdata.sim$cancer.site=="breast","female","male")
#' head(EQdata.sim)
#' 
#' mean.sd.fn <- function(data){
#' mean.sd <- c(mean(data$utility), sd(data$utility)/sqrt(nrow(data)))
#' return(list(estimate=mean.sd[1],se=mean.sd[2]))
#' }

#' multimp.eq5d(data=EQdata.sim,userfx=mean.sd.fn,dimnames=c("MO","SC","UA","PD","AD"),statename="state",stateidname="stateid",utilityname="utility",numimp=100)

multimp.eq5d <- function(data, userfx,dimnames,statename,stateidname,utilityname,numimp,...){
  mu.pred <- mu.pred3125
  ans.imputed <- unlist(lapply(1:numimp,singleimp,data,userfx,dimnames,statename,stateidname,utilityname,allstates,mu.pred,...))
  dimans <- length(ans.imputed)/(2*numimp)
  ans.mat <- array(ans.imputed,dim=c(dimans,2,numimp))
  ans.mat[,2,] <- ans.mat[,2,]^2
  ans.mean <- apply(ans.mat,1:2,mean)
  ans.between <- apply(array(ans.mat[,1,],c(dimans,numimp)),1,var)
  ans.pooled <- ans.mean[,1]
  ans.within <- ans.mean[,2]
  ans.sd <- sqrt(ans.within + (1+1/numimp)*ans.between)
  return(list(est=ans.pooled,se=ans.sd))
  
}

