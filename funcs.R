OEMCatch<-function(stk,start,end=NULL,plusgroup=NULL,oem.deviates=NULL)
    {
    if (is.null(end)) end<-start
    yrs<-as.character(end:start)

    stk <-window(stk, start=start, end=end)

    if (!is.null(oem.deviates))
    stk@catch.n[,yrs] <- stk@catch.n[,yrs]*oem.deviates[,yrs]

    if (!is.null(plusgroup))
       stk<-setPlusGroup(stk,plusgroup)

    return(stk)
    }
    
    

OEMIndex<-function(stk,deviates,start,end="missing",plusgroup="missing")
     {
     if (missing(end)) end<-start
     yrs<-start:end

     if (!missing(plusgroup))
        index<-setPlusGroup(trim(stk,year=yrs),plusgroup)@stock.n
     else
        index<-trim(stk,year=yrs)@stock.n

     ## unbiased population estimates
     return(index*deviates[,ac(yrs)])
     }
     
PerfAssm.st<-function(stk,start,end=NULL,plusgroup=NULL,oem.deviates=NULL)
    {
    if (is.null(end)) end<-start
    yrs<-as.character(end:start)

    stk <-window(stk, start=start, end=end)

    if (!is.null(oem.deviates)){
    stk@stock.n[,yrs] <- stk@stock.n[,yrs]*oem.deviates[,yrs]
    stk@harvest[,yrs] <- stk@harvest[,yrs]*oem.deviates[,yrs]
    }
    
    if (!is.null(plusgroup))
       stk<-setPlusGroup(stk,plusgroup)

    return(stk)
    }

CatchWeightToFmult <- function(x, Ny,.f, M, wt, PropL, Cy) {   

    abs(sum((x*.f*Ny)/ (M+(x*.f))*(1-exp(-(x*.f)-M))* PropL *wt,  na.rm=TRUE)-Cy)
    }

CatchNumberToF <- function(x,Ny, M, Cy) {   

    abs((x*Ny)/ (M+x)*(1-exp(-x-M)) -Cy)
    }
    
    
totalStk <- function(stk, Units){
    landings(stk) <- computeLandings(stk)
    discards(stk) <- computeDiscards(stk)
    catch.n(stk)  <- landings.n(stk)+discards.n(stk)
    catch.wt(stk) <-(landings.n(stk)*landings.wt(stk)+discards.n(stk)*discards.wt(stk))/catch.n(stk)
    catch(stk)    <- computeCatch(stk)
    stock(stk)    <- computeStock(stk)
    units(stk)[1:17] <- as.list(c(rep(Units,4), "NA", "NA", "f", "NA", "NA"))
    return(stk)
    }

 # CORRECTED AREASUMS FOR FLCORE
setMethod("areaSums", signature(x="FLQuant"),
function (x, ...) 
{
    .local <- function (x, na.rm = TRUE) 
    {
        return(apply(x, c(1:4, 6), sum, na.rm = na.rm))
    }
    .local(x, ...)
} )



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


# Geometric Mean
gm_mean = function(x, na.rm=TRUE, zero.propagate = FALSE){
  if(any(x < 0, na.rm = TRUE)){
    return(NaN)
  }
  if(zero.propagate){
    if(any(x == 0, na.rm = TRUE)){
      return(0)
    }
    exp(mean(log(x), na.rm = na.rm))
  } else {
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
  }
}
