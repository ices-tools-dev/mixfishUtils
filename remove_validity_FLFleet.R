####TRYING TO REDUCE COMPUTING TIME........
setClass('FLFleet',
	representation('FLComp',
		effort='FLQuant',
		fcost='FLQuant',
		capacity='FLQuant',
		crewshare ="FLQuant",
		metiers='FLMetiers'),
	prototype(name=character(0), desc=character(0),
		range= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1)),
		effort=FLQuant(), fcost=FLQuant(), capacity=FLQuant(),
		crewshare=FLQuant(), metiers=FLMetiers()))


setClass('FLMetier',
	representation('FLComp',
		gear='character',
		effshare='FLQuant',
		vcost='FLQuant',
		catches='FLCatches'),
	prototype(name=character(0), desc=character(0),
		range= unlist(list(min=0, max=0, plusgroup=NA, minyear=1, maxyear=1)),
		gear=character(0), catches=new('FLCatches'), effshare=FLQuant(1), vcost=FLQuant(NA)))


setClass("FLCatch",
    representation(
		'FLComp',
      landings    = "FLQuant", landings.n = "FLQuant",
		  landings.wt = "FLQuant", landings.sel = "FLQuant",
      discards    = "FLQuant", discards.n = "FLQuant",
      discards.wt = "FLQuant", discards.sel= "FLQuant",
		  catch.q = "FLQuant", price       = "FLQuant"),
    prototype=prototype(
		name		= character(0),
		desc		= character(0),
	  range       = as.numeric(c(min=NA, max=NA, plusgroup=NA,
			minyear=NA, maxyear=NA)),
    landings = new("FLQuant"), landings.n = new("FLQuant"),
    landings.wt = new("FLQuant"), landings.sel = new("FLQuant"),
    discards = new("FLQuant"), discards.n  = new("FLQuant"),
    discards.wt = new("FLQuant"), discards.sel= new("FLQuant"),
    catch.q     = new("FLQuant"), price = new("FLQuant"))
)

