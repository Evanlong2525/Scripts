##running TTR with extracted data

install.packages("DEoptim") 
##on Windows have to install rtools for this to work: https://cran.r-project.org/bin/windows/Rtools/
install.packages("Thorn.tar.gz", repos = NULL, type="source") 
library(Thorn) 
library(DEoptim)


## more environment data

enrivon.files<-list.files(pattern="^$_environ.Rdata") #load all files starting with pr_ "^" files starting with this pattern
#will need to loop through PR folder for multiple species
for(i in 1:length(pr.files))
{
  load(pr.files[i]) 
  names(lonlatenv) 
  #will have to double check this hardcoded stuff matches what the extraction script outputs
  names(lonlatenv[c(1:54,91:133)])
  
  ##clear out rows with NA
  lonlatenv[lonlatenv == -9999]<-NA 
  a<-which(is.na((apply(lonlatenv[,c(5:54,91:133)],1,sum)))) 
  if (length(a) > 0 ) lonlatenv<-lonlatenv[-a,]
  
  ###sample data points to fit model
  MPS<-2000 # max no data points to sample 
  obs<-lonlatenv[,4] # 4th column is the p/a column 
  w1 <- which( obs > 0 ) # which elements presence data 
  w0 <- which( obs == 0 ) # which elements background data
  # depending on the length sample either w1 or MPS data points: 
  if(length(w1)>MPS) isample1 <- sample( w1, MPS) 
  if(length(w1)<=MPS) isample1 <- w1 
  isample0 <- sample( w0, min(length(w0),length(isample1))) 
  isample <- c(isample1,isample0) # combine the indices 
  sd<-lonlatenv[isample,] # create a subsample data frame
  
  ##more cleaning
  a<-which(is.na((apply(sd[,c(5:54,91:133)],1,sum)))) 
  if (length(a) > 0 ) sd<-sd[-a,] 
  names(sd)[4]<-"obs"
  
  ####load support script--contains formatting functions
  source("thorn_support_functions.R")
  #format and normalize (between 0 and 1)
  c.data<-prodcdata(sd)
  
  ######fit TTR
  ####call DEoptim
  ##"get.ss is a wrapper which runs the model for a given set of parameters 
  ##for all sample locations and returns a negative log-likelihood."
  
  SolDEoptim<-DEoptim(get.ss, upper=up, lower=lo, 
                      control=DEoptim.control(NP = 50, itermax = 500, 
                                              trace = T, CR=0.5, F=0.5, 
                                              initialpop = NULL, strategy = 2, 
                                              parallelType=0, 
                                              parVar=c("ThornLoop.C","cloglog","c.data")) )
  
  
  ####extract info
  ##"get.abu uses the solution (stored in bestmem) to calculate the 
  ##modelled biomass for each sampled data point." 
  bestmem <- SolDEoptim$optim$bestmem 
  abu.sol<-get.abu(bestmem,c.data) 
  ##" MODEL is the modelled biomass transformed by the cloglog function." (complementary log log)
  MODEL<-cloglog( (log(abu.sol+1e-20)/bestmem[29]) , inverse=TRUE )
  ##"Probopti uses the confusion matrix to ﬁnd the best cutoﬀ point for converting 
  ##probability presence into predicted presence. "
  probopti <- optimise(findpr, interval=c(0,1),
                       maximum = TRUE, MODEL=MODEL, DATA=sd$obs)$maximum 
  ##-->create a presence absence data ﬁeld-->list called ftobj 1.
  ftobj_1<-list(pars=bestmem, # the parameters 
                abu=abu.sol, # the modelled biomass 
                pa=ifelse(MODEL>probopti,1,0), # the predicted presence,absence 
                obs=sd$obs, # the data presence, absence 
                lon=sd$lon, # the longitude 
                lat=sd$lat ) # the latitude 
  FILENAME<-"ft_EV.Rdata" # a filename 
  save(ftobj_1,file=FILENAME) # save the results (ideal parameters for the plant for each variable)
}