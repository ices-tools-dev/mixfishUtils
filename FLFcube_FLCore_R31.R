
###########################################################################################
###                                                                                     ###
###                     FLFCube - Fcube with FLR                                        ###
###                                                                                     ###
###                 Fcube is a model written in the R language to perform               ###
###                 Fishery and Fleet Forecasts (FFF = Fcube).                          ###
###                                                                                     ###
###   Author : Clara Ulrich, DTU-Aqua, <clu@aqua.dtu.dk>                                ###
###                                                                                     ###
###   Credit :  Dorleta Garcia, AZTI, <dgarcia@suk.azti.es>                             ###                                            ###
###             Katell Hamon, IFREMER <katell.hamon@ifremer.fr>                         ###
###             Alberto G. Murta, IPIMAR, <amurta@ipimar.pt>                            ###
###             colleagues from DTU-Aqua, ICES SGMIXMAN,                                ###
###                  AFRAME and EFIMAS EU FP6 projects                                  ###
###                                                                                     ###
### Last update : 30 March 2009,                                                        ###
###                                                                                     ###
### Runs with R 2.8.1                                                                   ###
###           FLCore tagged release 2.2                                                 ###
###                                                                                     ###
###########################################################################################




#----Fcube control ---------------------

validFLFcube.control <- function(object) {
	if(object@avg.nyrs < 1)               return("Invalid number of years for calculation of means")
	return(TRUE)
}

setClass("FLFcube.control",
   representation(
      #effort.rule ="FLPar",
      effort.rule ="array",
      mgmt.year   ="numeric",
      data.yrs    ="integer",
      target.F    ="FLPar"),
      prototype=prototype(
      #effort.rule =FLPar("max")
      effort.rule =array("max"),
      mgmt.year   =as.numeric(NA),
      data.yrs    =as.integer(NA),
      target.F    =FLPar()),
   validity=validFLFcube.control
)

###################################################################

# ----------FLFcube method -------

setGeneric("FLFcube", function(fleets,stocks,control,...){
        standardGeneric("FLFcube")
})

setMethod("FLFcube", signature(fleets="FLFleets",stocks="FLStocks",control="FLFcube.control"),
function(fleets,stocks, control,silent=TRUE){
#fleets=fl.pred; stocks=wg.stock; control=fc.ctl;silent=Fc.silent
#fleets=fl.f3; stocks=OM; control=f3.ctrl;silent=Fc.silent
#fleets=fl.f3; stocks=wg.stock; control=f3.ctrl.YY;silent=Fc.silent
  

  #------------------------------------------
  ##  1. get inputs
  #------------------------------------------
    yr.rge  <- ac(control@data.yrs)
    
    sp.lst <- names(stocks)
    range <- t(sapply(stocks, function (x) x@range))
    
    dat.past <- slot.fleet(fleets,"landings")
    dat.past <- dat.past[dat.past$year %in%yr.rge,]
    names(dat.past) <- gsub("qname","spp",names(dat.past))
   dat.past$spp <- as.character(dat.past$spp)
   #dat.past$iter <- as.numeric(as.character(dat.past$iter))
    dat.past$metier <- paste(dat.past$fleet,dat.past$metier, sep=".")

if(length(grep("rev",control@effort.rule,ignore.case=T))>0 | length(grep("val",control@effort.rule,ignore.case=T))>0) {
    rev. <-lapply(fleets, function(x) {
          mt. <- lapply(x@metiers,function(x1) {
                        res<-as.data.frame(lapply(x1@catches,revenue))
                        res$fleet <-name(x)
                        res$metier <- x1@name
                        #if (!is.na(x1@gear)) res$metier <- x1@gear else res$metier <- x1@name
                        res})
          mt. <- eval(parse(text=paste('rbind(mt.[[',paste(seq(length(mt.)),collapse=']],mt.[['),']])',sep='')))
          })
    rev.past <- eval(parse(text=paste('rbind(rev.[[',paste(seq(length(rev.)),collapse=']],rev.[['),']])',sep='')))
    rev.past <- rev.past[rev.past$year %in%yr.rge,]
    names(rev.past) <- gsub("qname","spp",names(rev.past))
    names(rev.past) <- gsub("data","revenue",names(rev.past))
    rev.past$metier <- paste(rev.past$fleet,rev.past$metier, sep=".")
    rev.past$spp <- as.character(rev.past$spp)
    #rev.past$iter <- as.numeric(as.character(rev.past$iter))
    dat.past <- merge(dat.past,rev.past)
   }
    #effort paste used only in the case of NA catchability
    eff.past <-lapply(fleets,function(x) {
                      res <- as.data.frame(FLQuants(effort(x)[,yr.rge]))
                      res$fleet <- name(x)
                      res})
    eff.past <- eval(parse(text=paste('rbind(eff.past[[',paste(seq(length(eff.past)),collapse=']],eff.past[['),']])',sep='')))
    #eff.past$iter <- as.numeric(as.character(eff.past$iter))
    
    q.fut <-slot.fleet(fleets,"catch.q")
    names(q.fut) <- gsub("qname","spp",names(q.fut))
    q.fut <- q.fut[q.fut$year==control@mgmt.year,]
    q.fut$spp <- as.character(q.fut$spp)
    #q.fut$iter <- as.numeric(as.character(q.fut$iter))
    
    
    sel.fut <- slot.fleet(fleets,"landings.sel")
    names(sel.fut) <- gsub("qname","spp",names(sel.fut))
    sel.fut <- sel.fut[sel.fut$year==control@mgmt.year,]

    dat.fut <-NULL
    for (S in sp.lst) {
               qq. <- q.fut[q.fut$spp==S,]
               age.q <- as.character(unique(qq.$age))
               if (!age.q=="all") {
               qq. <- qq.[qq.$age %in% c(range[S,"minfbar"]:range[S,"maxfbar"]),]
               qq. <- aggregate(qq.$catch.q,list(year=qq.$year,unit=qq.$unit,season=qq.$season, area=qq.$area,
                                    iter=qq.$iter,spp=qq.$spp,fleet=qq.$fleet,metier=qq.$metier),mean,na.rm=T)}
               names(qq.)<-gsub("x","catch.q",names(qq.))
    
               sel. <- sel.fut[sel.fut$spp==S,]
               age.s <- as.character(unique(sel.$age))
                if (length(age.s)>1) {
               sel. <- sel.[sel.$age %in% c(range[S,"minfbar"]:range[S,"maxfbar"]),]
               sel. <- aggregate(sel.$landings.sel,list(year=sel.$year,unit=sel.$unit,season=sel.$season, area=sel.$area,
                                    iter=sel.$iter,spp=sel.$spp,fleet=sel.$fleet,metier=sel.$metier),mean,na.rm=T)
                } else sel.$age <- qq.$age
               names(sel.)<-gsub("x","landings.sel",names(sel.))
    
               res. <- merge(qq., sel.)
               res.$land.q <- res.$catch.q*res.$landings.sel
               dat.fut <- rbind(dat.fut,res.)
               }
               
    dat.fut$metier <- paste(dat.fut$fleet,dat.fut$metier, sep=".")
    
    eff.fut <- slot.fleet(fleets,"effshare")
    eff.fut <- eff.fut[eff.fut$year==control@mgmt.year,]
    eff.fut$metier <- paste(eff.fut$fleet,eff.fut$metier, sep=".")
    #eff.fut$iter <- as.numeric(as.character(eff.fut$iter))
    
     #end of inputs data
    
    #------------------------------------------
    ##  2. call Fcube
    #------------------------------------------
    
    # call Fcube
    
    fcube.res <- fcube.main(dat.past=dat.past, eff.past=eff.past, dat.fut=dat.fut,eff.fut=eff.fut, control=control)
    
    #------------------------------------------
    ##  3. output in FLFleet format
    #------------------------------------------
    
    res<-FLFleets(lapply(fleets,function(x) {
                  n <- name(x)
                  it. <- dim(effort(x))[6]
                  x@effort[,as.character(control@mgmt.year),,,,seq(it.)]  <- fcube.res[[1]][n,]
                  x}))
                  
    if (!silent) ret. <- list(fleets=res,effort.stock=fcube.res[[2]],value.weight=fcube.res[[3]]) else ret. <- res
    return(ret.)

})


############################################################################

# ----------Fcube - main (is called from the FLFcube method -------

fcube.main <- function(dat.past, eff.past, dat.fut, eff.fut, control) {

  n.fl <- sort(unique(dat.past$fleet))
  n.mt <- sort(unique(dat.past$metier))
  n.st <- sort(unique(dat.past$spp))
  n.it <- max(length(levels(dat.past$iter)),length(levels(dat.fut$iter)),length(levels(eff.fut$iter)))

  #check the rule
  rule <- control@effort.rule

  if (!(length(rule)==1 | length(rule)==length(n.fl) | length(rule)==length(n.fl)*n.it))
              stop("effort rule must contain either 1 or nb.fleets or nb.fleets*n.it terms!")

  #if(length(rule)==1) rule <-FLPar(rep(c(rule),length(n.fl)*n.it),params=n.fl,iter=seq(n.it)) else
  #if(length(rule)==length(n.fl)) rule <-FLPar(rep(c(rule),n.it),params=names(rule),iter=seq(n.it))
  
  if(length(rule)==1) rule <-array(rep(c(rule),length(n.fl)*n.it),dim=c(length(n.fl),n.it),dimnames=list(params=n.fl,iter=seq(n.it))) else
  if(length(rule)==length(n.fl)) rule <-array(rep(c(rule),n.it),dim=c(length(n.fl),n.it),dimnames=list(params=names(rule),iter=seq(n.it)))
  

  if (any(is.na(match(rownames(rule),n.fl)))) stop("names in effort rule must match fleets names")

## relative stability
  rel.stab  <-  tapply(dat.past$landings, list(dat.past$spp, dat.past$fleet, dat.past$iter), sum, na.rm=T)
  if (n.it>1 && all(is.na(rel.stab[,,2:n.it])))  rel.stab[,,2:n.it] <- rel.stab[,,1]

  rel.stab[is.na(rel.stab)] <- 0
  rel.stab <- sweep(rel.stab,c(1,3),apply(rel.stab,c(1,3),sum),FUN="/")
  
## target species as FLPar
  targ <- control@target.F[sort(rownames(control@target.F))]
  #if (!length(dims(targ)$iter)==length(dims(rule)$iter)) {
  if (!length(dimnames(targ)$iter)==length(dimnames(rule)$iter)) {
      if (dimnames(targ)$iter=="1"){
             targ <- propagate(targ,iter=n.it)
             for (i in 2:n.it) iter(targ,i) <- iter(targ,1)} else stop("iterations in target.F must be 1 or n")}
                 

## Catchability  - as input
  dat.sort <- dat.fut[order(dat.fut$metier),]
  meanQ <- tapply(dat.sort$land.q, list(dat.sort$spp, dat.sort$metier, dat.sort$iter), FUN=max)
  
  # case of propagate not on catchability
  if (n.it>1 && dim(meanQ)[3]==1) meanQ <- array(rep(meanQ,n.it),dim=c(dim(meanQ)[1:2],n.it),dimnames=list(dimnames(meanQ)[[1]],dimnames(meanQ)[[2]],seq(n.it)))
  if (n.it>1 && all(is.na(meanQ[,,2:n.it])))  meanQ[,,2:n.it] <- meanQ[,,1]
  
    ## nota - if a metier does not catch a stock, meanQ will stand as NA. But if a metier catch a stock but
    ## has not catchability estimates because of no F estimates, then meanQ will stand as NaN.
    
          ##! SUGGESTION - IF THERE ARE SOME MISSING DATA, THEN THE CORRESPONDING EFFORT IS SET AS THE STATUS QUO WHICH IMPLIES
      # - if the fleet doesn't catch the species regulated, then its effort is assumed as unchanged
      # - if the species to regulate has no data, the effort of fleets is unchanged
      # - if the value scenario is chosen, the fleet is assumed to use an unchanged effort for the particular species,
          # which does not affect the result since the value is 0

      #this is done by checking the presence of the stock in the fleet's catches in the rel.stab matrix
      mean.past.eff <- tapply(eff.past$data, list(eff.past$fleet, eff.past$iter), mean, na.rm=T)

    
## prop effort - as input
   next.prop.effort <- tapply(eff.fut$effshare, list(eff.fut$fleet, eff.fut$metier, eff.fut$iter), FUN=max)
   if (n.it>1 && all(is.na(next.prop.effort[,,2:n.it])))  next.prop.effort[,,2:n.it] <- next.prop.effort[,,1]

## Prognosis for next year
  nextQ <-  array(dim=c(length(n.st),length(n.fl), length(n.mt) , n.it), dimnames=list(n.st, n.fl, n.mt, seq(n.it)))

  for(i in seq(length(n.st)))
    nextQ[i,,,] <- sweep(next.prop.effort, c(2,3), meanQ[i,,], FUN="*")

  nextQ <- apply(nextQ, c(1:2,4), sum, na.rm=TRUE)
  #targ <- control@target.F[sort(rownames(control@target.F))]

  if (all(rownames(rel.stab)==rownames(targ))) nextF <- sweep(rel.stab, c(1,3),targ, FUN="*") else stop("no match between stock@spp and target.F names!")

  nextEffort.stock <- sweep(nextF, c(1:3), nextQ, FUN="/")
  nextEffort.stock[nextEffort.stock==Inf]<-NaN
  


  #weighting coefficients based on value - only calculated if necessary
value.weight=NA

if(length(grep("rev",rule,ignore.case=T))>0 | length(grep("val",rule,ignore.case=T))>0) {
  value.table   <-  tapply(dat.past$revenue, list(dat.past$spp, dat.past$fleet,dat.past$iter), FUN=sum)
  value.table[is.na(value.table)] <- 0
  value.weight  <-  sweep(value.table,2:3,apply(value.table,2:3,sum,na.rm=T),FUN="/")
}

## Next year effort ####
man.Effort <- matrix(NA,nrow=length(n.fl), ncol=n.it,dimnames=list(n.fl,seq(n.it))) #just to make the form of output!

#if(n.it==1) index=expression(i) else index=expression(i,j)

for (i in n.fl) {
for (j in seq(n.it)) {

    for (k in n.st) {
    if (is.na(nextEffort.stock[k,i,j]) & rel.stab[k,i,j]>0)  nextEffort.stock[k,i,j] <-mean.past.eff[i,j]
    }

  if(length(grep("max",rule[i,j],ignore.case=T))>0)
    man.Effort[i,j] <- max(nextEffort.stock[,i,j],na.rm=T)
  else
    if(length(grep("min",rule[i,j],ignore.case=T))>0)
        man.Effort[i,j] <- min(nextEffort.stock[,i,j],na.rm=T)      
    else
        if(tolower(rule[i,j]) %in% tolower(stock.names)){
            man.Effort[i,j] <- nextEffort.stock[grep(rule[i,j],stock.names,ignore.case=T,value=T),i,j]

            #correction from Dorleta - if a fleet does not catch a species then its effort is not
              #constrained  - but here I chose to put it as equal to before rather than maximum
            if(is.na(man.Effort[i,j]))     man.Effort[i,j] <- mean.past.eff[i,j]
        }
        else
            if(length(grep("rev",rule[i,j],ignore.case=T))>0 | length(grep("val",rule[i,j],ignore.case=T))>0){
                            man.Effort[i,j] <- sum(value.weight[,i,j]*nextEffort.stock[,i,j],na.rm=T)
                            #if a fleet does not have weighting because of missing revenue data
                            #then its effort is equal to before 
                            if(all(is.nan(value.weight[,i,j]))) man.Effort[i,j] <- mean.past.eff[i,j]
            }         
  
  else
            if(length(grep("sq",rule[i,j],ignore.case=T))>0)
    man.Effort[i,j] <- mean.past.eff[i,j]                                                     
                                                            
       else

  stop("Please check the input to the effort rule (control@effort.rule), to be specified using max, min, rev, sq or a stock name")
}}

return(res=list(final=man.Effort,effort.stock=nextEffort.stock,value.weight=value.weight))
}

##################################################
##################################################
#additional function - slot.fleet(extract the slot of a fleet as datframe

# ----------slot.fleet method -------
setGeneric("slot.fleet", function(fleets,slot.){
        standardGeneric("slot.fleet")
})

setMethod("slot.fleet", signature(fleets="FLFleets"),
function(fleets,slot.)
{

sl. <- eval(parse("",text=slot.))

res <-lapply(fleets, function (x) {
        mt. <- lapply(x@metiers, function(x1) {
                            res. <- as.data.frame(sl.(x1))
                            names(res.)[which(names(res.)=="data")]   <- slot.
                            res.$fleet  <- x@name
                            res.$metier <- x1@name
                            #if (!is.na(x1@gear)) res.$metier <- x1@gear else res.$metier <- x1@name
                            return(res.)})
          mt. <- eval(parse(text=paste('rbind(mt.[[',paste(seq(length(mt.)),collapse=']],mt.[['),']])',sep='')))
          })
res <- eval(parse(text=paste('rbind(res[[',paste(seq(length(res)),collapse=']],res[['),']])',sep='')))

return(res)
})


## ADDITIONAL FUNCTIONS

#
# Sums(FLQuants)	{{{
setGeneric('Sums', function(object, ...)
		standardGeneric('Sums')
)
setMethod('Sums', signature(object='FLQuants'),
	function(object, na.rm=FALSE, ...) {
		if(length(object) == 1)
			return(object[[1]])
		eval(parse(text=paste('object[[', paste(seq(length(object)),
			collapse=']] + object[['), ']]', sep='')))
	}
)



setGeneric('spp', function(object)
  standardGeneric('spp')
)
setMethod('spp',signature(object="FLFleet"),
          function(object) {
            unique(unlist(lapply(object@metiers, function(x) names(x@catches)))) }
)

