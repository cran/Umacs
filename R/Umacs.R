# ================================================================================
# Umacs - Universal Markov chain Sampler
# Updated 2007-02-10 (R ver. 2.3 compatible)
# (c) 2004-2007 Jouni Kerman <kerman@stat.columbia.edu>
# ================================================================================

# NAME:
#   DEFAULT
# AUTHOR:
#   Jouni Kerman <jouni@kerman.com>
# REFERENCES:
#   Kerman, Jouni. Umacs: A Universal Markov Chain Sampler. Technical report, Columbia University, New York.
# SEEALSO:
#  [[Umacs-package]] for an overview of the Umacs package.
#
#  [[Sampler]] for how to create a sampler function; see the vignette for details.
# 
#  Package `rv'.
# 
# KEYWORD: iteration
#

.onLoad <- function (lib, pkg) {
  if (is.null(options("Umacs"))) {
    options("Umacs"=list(VERBOSE=TRUE, DEBUG=FALSE))
  }
  require(methods)
}

# NAME:
#   Umacs-package - Creating Iterative Samplers
# DESCRIPTION:
#
#   Please refer to the vignette: [vignette("Umacs")].
#

#---end Umacsheader.R---


if (!isGeneric('code'))
  setGeneric('code', function (obj, ...) standardGeneric('code'))

if (!isGeneric('templates'))
  setGeneric('templates', function (obj, ...) standardGeneric('templates'))

# NAME:
#   templates-methods - Get the `templates' of an object
# DESCRIPTION:
#  Used internally in [[Umacs]].
# METHODS:
#


if (!isGeneric('run'))
  setGeneric('run', function (obj, name, envir, ...) standardGeneric('run'))

if (!isGeneric('snippets'))
  setGeneric('snippets', function (obj, ...) standardGeneric('snippets'))

# NAME:
#   snippets-methods - Get the `snippets' of an object
# DESCRIPTION:
#  Used internally in [[Umacs]].
# METHODS:
#  

if (!isGeneric('as.snippet'))
  setGeneric('as.snippet', function (obj, ...) standardGeneric('as.snippet'))

##DELETE##if (!isGeneric('is.snippet'))
##DELETE##  setGeneric('is.snippet', function (obj, ...) standardGeneric('is.snippet'))

if (!isGeneric('as.snippetlist'))
  setGeneric('as.snippetlist', function (obj, ...) standardGeneric('as.snippetlist'))

##DELETE##if (!isGeneric('is.snippetlist'))
##DELETE##  setGeneric('is.snippetlist', function (obj, ...) standardGeneric('is.snippetlist'))


if (!isGeneric('init'))
  setGeneric('init', function (obj, name, id) standardGeneric('init'))

# begin NOTE
# "'length<-' is a primitive function;  methods can be defined,
#  but the generic function is implicit, and cannot be changed."
# end   NOTE.

if (!isGeneric('name'))
  setGeneric('name', function (obj, ...) standardGeneric('name'))

if (!isGeneric('env'))
  setGeneric('env', function (obj, ...) standardGeneric('env'))

if (!isGeneric('name<-'))
  setGeneric('name<-', function (obj, value) standardGeneric('name<-'))

if (!isGeneric('value<-'))
  setGeneric('value<-', function (obj, value) standardGeneric('value<-'))

if (!isGeneric('constantParam'))
  setGeneric('constantParam', function (obj, ...) standardGeneric('constantParam'))

if (!isGeneric('mcmcdiag'))
  setGeneric('mcmcdiag', function (obj, ...) standardGeneric('mcmcdiag'))

#if (!isGeneric('names'))
#  setGeneric('names', function (x, ...) standardGeneric('names'))

# get rid of 'as.matrix'!

#if (!isGeneric('as.matrix'))
#  setGeneric('as.matrix', function (obj, ...) standardGeneric('as.matrix'))
#
# util.R - common utilities for the Umacs package
#

#
# Options are initialized in .onLoad.
#

.VERBOSE <- function () {
  x <- options("Umacs")$VERBOSE
  if (is.logical(x)) x else FALSE
}

.myc <- function (x)
{
  paste("c(",paste(x,collapse=","),")", sep="")
}

.show <- function (x)
{
  cat(deparse(substitute(x)),"<-", .myc(x), "\n")
}


.begin <- function (x) {
  name <- deparse(substitute(x))
  cat("+++BEGIN+++", name,"+++++++++++++++++\n")
  cat("Old value\n")
  eval.parent(substitute(.show(x)))
}

.end <- function (x) {
  name <- deparse(substitute(x))
  cat("+++END+++++", name,"+++++++++++++++++\n")
}



say <- function (...) # NOEXPORT
{
  if (.VERBOSE()) cat(..., sep="")
}

.is.error <- function (x)
{
  class(x) == 'try-error'
}

.suffix <- function (x)
{
  suff <- list(
    name='',
    "NA"='.NA',
    scalar.names='.names',
    logpost='.logpost'
  )
  suff[x]
}

.paste.names <- function (name, x)
{
  na <- paste(name, unlist(.suffix(x)), sep='')
  names(na) <- na
  na
}

.flatten.list <- function (x)
{
  if (!is.list(x)) return(x)
  if (length(x)==0) return(list())
  head <- if (is.list(x[[1]])) .flatten.list(x[[1]]) else x[1]
  c(head, .flatten.list(x[-1]))
}


# .permut, dimindex

.permut <- function(dims) # NOEXPORT
{
  tl <- prod(dims)
  co <- NULL
  p <- 1
  for (i in 1:length(dims)) {
    d <- dims[i]
    x <- rep(1:d, each=p, length.out=tl)
    co <- cbind(co, x)
    p <- p*d
  }
  dimnames(co) <- NULL
  co
}

.dimindex <- function(x)
{
  ix <- .permut( if (is.null(x)) length(x) else x )
  paste('[',apply(ix, 1, paste, collapse=','),']', sep='')
}

# .get.param.names - 

.get.param.names <- function(name, value, dim.=NULL)
{
  if (!missing(value)) {
    dim. <- if (is.null(dim(value))) length(value) else dim(value)
  }
  if (length(dim.)<1) return('')
  if (length(dim.)==1 && dim.==1) return(name)
  pn <- paste(name, .dimindex(dim.), sep="")
  dim(pn) <- dim.
  pn
}

# 'n' may be a vector



progressBar <- function (n, char.='.', total.dots=80, resume.from=0, silent=FALSE, cat.=NULL)
{
  if (silent) return(function(t, char) return(NULL))
  output <- if (is.function(cat.)) cat. else cat
  progress.bar <- '0%' 
  if (total.dots<40) stop("total.dots must be >= 40")
  ten.percent <- total.dots %/% 10
  total.dots <- 10*ten.percent
  for (i in 1:10) progress.bar <- c(progress.bar,rep(".",ten.percent-2),i*10)
  progress.bar <- c(progress.bar,'%')
  dots.printed.so.far <- NA
  dots.per.iteration <- total.dots/sum(n)
  i <- sum(resume.from)
  #
  function (t, char, n.) {
     if (missing(char)) char <- char.
     if (missing(t)) {
        t <- i + 1
     } else {
        t <- sum(t)
     }
     if (!missing(n.)) {
       n <<- n.
       dots.per.iteration <- total.dots/sum(n)
     }
     if (t==0) {
        dots.printed.so.far <<- NA
        t <- i
     }
     if (is.na(dots.printed.so.far)) {
        nt <- if (length(n)>1 && all(n[1]==n)) paste(length(n),"*",n[1],sep="") 
        else paste(n, collapse=",")
        output(progress.bar,' (', nt, ")\n",sep='')
        dots.printed.so.far <<- 0
     }
     i <<- min(t, sum(n))
     dots.that.must.show <- ceiling(i*dots.per.iteration)
     dots.to.print <- max(0, dots.that.must.show - dots.printed.so.far)
     if (dots.to.print>=1) output(rep(char,dots.to.print),sep='')
     dots.printed.so.far <<- dots.printed.so.far + dots.to.print
     return(i/sum(n)) # proportion done
  }
}

.compute.eigen <- function (Sigma)
{
  # From MASS:::mvrnorm
  e <- eigen(Sigma, sym = TRUE, EISPACK = TRUE)
  ev <- e$values
  if (!all(ev >= -1e-06 * abs(ev[1]))) {
      error("'Sigma' is not positive definite")
  }
  e$vectors %*% diag(sqrt(pmax(ev, 0)), nrow(Sigma))
}

# mvrnorm2
# mu : vector or matrix or a list of vectors
# EigenSigma : a matrix or a list of matrices (of eigenvectors %*% eigenvalues of Sigma)
# EigenSigmaList : a list of matrices (of eigenvectors * eigenvalues of Sigma)

mvrnorm2n <- function (n = 1, mu, EigenSigma, byCol=FALSE)
{
  p <- length(mu)
  Z <- matrix(rnorm(p * n), p)
  X <- drop(mu) + EigenSigma %*% Z
  if (n == 1) drop(X) else t(X)
}

mvrnorm2 <- function (mu, EigenSigma)
{
  p <- length(mu)
  Z <- matrix(rnorm(p), p)
  drop(mu + EigenSigma %*% Z)
}

mvrnorml <- function (mu, EigenSigmaList, byCol=FALSE)
{
  if (is.list(mu)) {
    m <- mu
  } else {
    if (is.null(dim(mu)))
      m <- list(mu) 
    else 
      m <- if (byCol) split(mu,col(mu)) else split(mu,row(mu))
  }
  t(mapply(mvrnorm2, m, EigenSigmaList))
}


# ========================================================================
# .reflect  -  reflect too long a jump back into the allowed region
# ========================================================================
#

.reflect <- function (x, a, b)
{
  w <- b-a
  x.mod <- (x-a)%%(2*w)
  a + ifelse (x.mod<w, x.mod, 2*w-x.mod)
}

.reflect2 <- function (x, center, a, b)
{
  w <- b-a
  x.mod <- (x-center)%%(2*w)
  a + ifelse (x.mod<w, x.mod, 2*w-x.mod)
}

.reflectRight <- function (x, a)
{
  # .reflect with b=Inf
  #
  a + abs(x-a)
}

.reflectLeft <- function (x, b)
{
  # .reflect with a=-Inf
  #
  b - abs(b-x)
}

# ========================================================================
# .ptime  -  seconds into hours, min, sec
# ========================================================================

.ptime <- function(total)
{
  ttime <- trunc(total)
  est.h <- trunc(ttime/3600)
  est.m <- trunc((ttime%%3600)/60)
  est.s <- round(ttime %% 60,1)
  if (est.h+est.m==0)
     paste(est.s,"sec")
   else if (est.h==0)
    paste(est.m,"min",est.s,"sec")
   else
    paste(est.h,"h",est.m,"min",est.s,"sec")
}


.about.zero <- function (x, tol=1e-6)
{
  abs(x)<tol
}


##---

# ================================================================================
# .tracePlotter - trace plot for Markov chain simulations
# ================================================================================

.tracePlotter <- function (label, n.iter,
  n.interval=ceiling(n.iter/10),
  n.min=20, 
  skip=0, 
  colors=c('blue', 'red',  'green', 'orange', 'yellow'),
  silent=FALSE)
{
  ##
  values <- list()
  last.plotted <- list()
  valueRange <- NULL
  outOfRange <- TRUE
  xMax <- 0
  currentRange <- NULL
  thisDevice <- -Inf # -Inf: no window yet; >0: window open
  showWindow <- TRUE
  ##
  .newGraphicsWindow <- match.fun(getOption('device'))
  ##
  .dev.exists <- function () (any(dev.list()==thisDevice))
  .dev.off <- function () {
    if (.dev.exists()) dev.off(thisDevice)
    thisDevice <<- -Inf
    showWindow <<- FALSE
    return(showWindow)
  }
  .dev.set <- function () {
    if (!.dev.exists()) .new.dev()
    if (thisDevice!=dev.cur()) dev.set(thisDevice)
  }
  .new.dev <- function () {
    .newGraphicsWindow()
    showWindow <<- TRUE
    thisDevice <<- dev.cur()
  }
  ##
  if (silent) return(function (new.y=NULL, chain=1, all.y=NULL, n.iter., n.interval.=ceiling(n.iter./10), refresh=FALSE, restart=FALSE) return(FALSE))
  ##
  function (new.y=NULL, chain=1, all.y=NULL, n.iter., n.interval.=ceiling(n.iter./10), refresh=FALSE, restart=FALSE) {
    if (restart) {
      .new.dev()
    }
    if (!missing(n.iter.) && n.iter.!=n.iter) {
      n.iter <<- max(1,ceiling(abs(n.iter.)))
      n.interval <<- ceiling(n.iter/10)
    }
    if (!missing(n.interval.)) n.interval <<- max(1,ceiling(n.interval.))
    if (!is.null(all.y)) {
      if (is.list(all.y)) {
        values <<- all.y
      } else {
        values[[chain]] <<- all.y
      }
    } else if (!is.null(new.y)) {
      if (length(values)<chain) {
        values[[chain]] <<- new.y
        last.plotted[[chain]] <<- 1
        if (is.null(valueRange)) valueRange <<- c(new.y, new.y)
      } else {
        values[[chain]] <<- c(values[[chain]], new.y)
      }
    } else {
      return(showWindow)
    }
    to <- length(values[[chain]])
    outOfRange <<- outOfRange || (new.y>valueRange[2] || new.y<valueRange[1] || to>xMax)
    valueRange <<- range(valueRange, new.y)
##cat("to=",to,"n.iter=",n.iter,"\n")
    if ((to %% n.interval != 0 && to<n.iter)) return(showWindow)
##cat("%%=",to %% n.interval,"n.iter=",n.iter,"\n")
    # If user closed the window; keep it closed but keep updating values[[]],
    # so we can use it next time.
    if (!showWindow) return(showWindow)
    # Is the value out of the current plotting range?
    if (outOfRange) {
      .dev.set()
      plot(1:n.iter, seq(from=valueRange[1], to=valueRange[2], length=n.iter),
        main=paste('Trace plot of', label),
        xlab='Iterations', 
        ylab=label,
        type='n')
      xMax <<- n.iter
      refresh.chains <- seq(along=values)
      for (i in seq(along=values)) last.plotted[[i]] <<- 1
      outOfRange <<- FALSE
    } else {
      refresh.chains <- chain
    }
    for (j in refresh.chains) {
      from <- last.plotted[[j]]
      y <- values[[j]]
      to <- length(y)
      if (to<=from) next
      s0 <- seq(from, to-1)
      s1 <- seq(from+1, to)
      if (!.dev.exists()) return(.dev.off()) else .dev.set()
      color <- colors[ ((j-1) %% length(colors)) + 1 ]
      e <- try(
        segments(
          x0=s0,         # every t except last
          y0=y[s0],      # every sim except last
          x1=s1,         # every t except first
          y1=y[s1],      # every sim except first
          col=color      # different color for each chain
        ), silent=TRUE)
      if (.is.error(e)) return(.dev.off())
      last.plotted[[j]] <<- to
    }
    return(showWindow)
  }
}

# Adaptation plot

.adaptPlot <- function (samplerfun, varName) {
  e <- samplerfun(env=T)
  n.chains <- e$sampler$n.chains
  par(mfrow=c(1,n.chains))
  for (j in 1:n.chains) {
    ## DEBUG: en, os, op saved
en <<-    env <- e$chain[[j]]$updater[[varName]]
os <<-  os <-  old.scales <- env$old.scales
op <<-  op <-  old.avg.p.jumps <- env$old.avg.p.jumps
    lm0 <- lm(-log(old.avg.p.jumps)~old.scales-1)
    b0 <- lm0$coeff
    plot(old.scales, old.avg.p.jumps, ylim=0:1, xlim=c(0,max(old.scales)))
    curve(exp(-b0*x), from=0, add=TRUE, col='black')
    title("Black curve exp(-b0*x); Red curve exp(-b1*sqrt(x)); Blue curve 2atan(2/x/b)/pi")
    lm1 <<- lm(-log(old.avg.p.jumps)~sqrt(old.scales)-1)
    b1 <<- lm1$coeff
    curve(exp(-b1*sqrt(x)), from=0, add=TRUE, col='red')
    lm1 <<- lm(-log(old.avg.p.jumps)~old.scales^2-1)
    b2 <<- lm1$coeff
    curve(exp(-b2*x^2), from=0, add=TRUE, col='orange')
fp <<- f <- function(p) tan(pi*p/2)/2
fop.inv <<-    fop.inv <- 1/f(op)
    include <<- (op>=1e-03)
    if (!any(include)) include <- rep(TRUE,length(os))
# plot(os, fop.inv)
    lm2 <<- lm( fop.inv[include] ~ os[include] - 1)
    b <- lm2$coeff
print(b)
    curve(2*atan(2/x/b)/pi, from=0, add=TRUE, col='blue')
print(c(env$Sigma.scale,mean(env$p.jumps)))
    points(env$Sigma.scale,mean(env$p.jumps), pch=19, col='green')
  }
}



# End util.R

# ========================================================================
# class snippet  -  code snippets
# ========================================================================
#
# Usage:
#   x <- snippet(f <- g)
#   print(x)
#   substit(x) <- list(g=3)
#   y <- snippet({ y <- rnorm(10); z <- y^2})
#   s <- x + y
#   s <- 5*x
#



setClass('snippet', 
  representation(
    code='list', # Holds only one item
    args='list',
    name='character',
    output='numeric'  # 0=code, 1=function call, 2=function definition
  )
)

# NAME:
#   snippet-class - Class "snippet"
# DESCRIPTION:
#   A class whose instances accommodate ``code snippets"
#   created by substitute() or quote() etc.
#
#   Used internally by [[Umacs]]
# DETAILS:
# 


setClass('snippetlist', 
  representation(
    snippets='list'
  )
)

# NAME:
#   snippetlist-class - Class "snippetlist"
# DESCRIPTION:
#   A class whose instance accommodates a list of ``code snippets" of class ``snippet."
#
#   Used internally by [[Umacs]]
# DETAILS:
# 



setClass('codetemplate', 
  representation(
    snippets='snippetlist'
  )
)


# NAME:
#   codetemplate-class - Class "codetemplate"
# DESCRIPTION:
#   A `codetemplate' contains a [snippetlist],
#   which can be merged with `templates'.
#
#   Used internally by [[Umacs]]
#
# DETAILS:
#   More documentation follows (possibly) in later versions.
#


setClass('codetemplatelist', 
  representation(
    slist='list'
  )
)

# NAME:
#   codetemplatelist-class - Class "codetemplatelist"
# DESCRIPTION:
#   A `codetemplatelist' contains a list of [codetemplate]s,
#   containing snippets that can be merged with `templates' at once.
#
#   Used internally by [[Umacs]]
# DETAILS:
#   More documentation follows (possibly) in later versions.
#   


# ================================================================================
# as.language - coerce a string into 'language'
# ================================================================================

as.language <- function (txt)
{
  # Don't keep source.
  # This imposes limitations and causes errors when weaving long code.
  keep.source <- getOption("keep.source")
  if (!keep.source) return(parse(text=txt)[[1]])
  options(keep.source = FALSE)
  lang <- parse(text=txt)[[1]]
  options(keep.source = TRUE)
  lang
}

# ================================================================================
# is.code - is the object "code" as we define it?
# ================================================================================

is.code <- function (x) # NOEXPORT
{
  (is.language(x) || is.character(x) || is.numeric(x) || is.logical(x))
}

# ================================================================================
# .as.list.code - break a language object into a list, used only in .weavecode()
# ================================================================================

.as.list.code <- function (x) # NOEXPORT
{
  if (is.symbol(x) || !is.language(x)) return(list(x))
  if (x[[1]]=="{") x[[1]] <- NULL
  x
}

# ================================================================================
# .as.list.code - break a language object into a list, used only in .weavecode()
# ================================================================================

.is.null.function <- function (x) # NOEXPORT
{
  is.null(as.list(x)[[1]])
}

# ================================================================================
# .weavecode - combine a list of language or function objects into one language object
# ================================================================================

.weavecode <- function(x) # NOEXPORT
{
  # x : a list of objects that satisfy is.code()
  list.x <- c(lapply(x, .as.list.code),recursive=TRUE)
  y <- substitute({})
  for (i in seq(along=list.x)) {
    ## A nice side effect: null items in list.x will not show at all.
    y[[i+1]] <- list.x[[i]]
  }
  y
}

# ================================================================================
# code - retrieve "the code" of an object
# ================================================================================

setMethod('code', signature('snippet'), 
function (obj)
{
  o.type <- obj@output
  if (o.type==1) return(code(.funcall(obj)))
  if (o.type==2) return(code(.fundef(obj)))
  (obj@code)[[1]]
})


# Try to make this obsolete!
#setMethod('code', signature('list'), 
#function (obj)
#{
#  .weavecode(lapply(obj, code))
#})


setMethod('code', signature('ANY'), 
function (obj)
{
  if (is.code(obj)) return(obj)
  y <- obj # Evaluate
  substitute(y)
})


# ================================================================================
# snippet - make a snippet object, given a code snippet and optional environment/list
# ================================================================================
# Note. Will not evaluate x but will evaluate code.


snippet <- function(x=NULL, env=list(), code.=NULL, name=NULL, arguments.=NULL, call.name=NULL)
{
  formals. <- NULL
  output <- 0
  if (!is.null(call.name)) {
    output <- 1
    name <- call.name # DEBUG: slightly kludgy. make more elegant
  }
  ##maybe.name <- substitute(code.)
  e <- if (!is.null(code.)) {
    #if (is.character(code.)) {
    #  is.fun <- is.function(try(func <- match.fun(code.),silent=TRUE))
    #  if (is.fun) {
    #    if (is.null(name)) name <- code.
    #    code. <- func
    #  }
    #}
    if (is.function(code.)) {
      formals. <- formals(code.)
      ##if (is.name(maybe.name) && is.null(name)) name <- maybe.name
      body(code.)
    } else code(code.)
  } else substitute(x)
  if (!is.null(arguments.)) {
    formals. <- arguments.
  }
  if (length(env)>0) {
    co <- eval(substitute(substitute(e, env), list(e=e)))
    # The next step is necessary to get rid of any bound variables in env,
    # and to return pure 'language'.
    co <- as.language(deparse(co))
  } else co <- e
  # Wrap code into list: may be of several types
  obj <- new('snippet', code=list(co), output=output)
  class(obj) <- 'snippet'
  names(obj) <- name
  arguments(obj) <- formals.
  obj
}

# ================================================================================
# arguments - get the arguments (formals)
# ================================================================================

arguments  <- function(x, snippetlist=FALSE)
{
  if (!is.snippet(x)) stop("x must be a snippet")
  a <- x@args
  if (snippetlist) {
    if (length(a)>0) {
      a <- lapply(a, function(x) if (length(x)==0 || nchar(as.character(x))==0) NULL else x)
      a <- as.snippetlist(a)
    } else a <- snippetlist()
  }
  a
}

"arguments<-" <- function(x, value)
{
  if (!is.snippet(x)) stop("x must be a snippet")
  rhs <- value
  if (is.character(rhs)) {
    rhs <- sapply(rhs, function (x) NULL) # List with names, NULL values
  }
  nv <- names(rhs)
  rhs <- if (length(nv)<1) NULL else rhs[nv]
  if (length(rhs)==0) rhs <- list()
  x@args <- rhs
  x
}

# ================================================================================
# names   - get the name of the snippet
# names<- - set the name of the snippet
# ================================================================================

names.snippet <- function(obj)
{
  obj@name
}

"names<-.snippet" <- function(obj, value)
{
  obj@name <- as.character(value)
  obj
}

# ================================================================================
# substit<- - substitute the symbols in the snippet by a given list of values
# ================================================================================

"substit<-" <- function(x, value)
{
  if (!is.snippet(x)) stop("substit(..) <- ... works only with snippet objects.")
  rhs <- value
  if (is.snippetlist(rhs)) rhs <- as.list(rhs)
  else if (!is.list(rhs))
    stop("A snippet can be assigned a list or a snippetlist only")
  if (length(rhs)==0) return(x)
  env <- lapply(rhs, code)
  snippet(code=code(x), env=env)
}


# ================================================================================
# is.snippet - is the object a snippet?
# ================================================================================

is.snippet <- function (obj) is(obj, 'snippet')

# ================================================================================
# as.snippet - coerce to a snippet, if possible
# ================================================================================

setMethod('as.snippet', signature('snippet'),
function (obj)
{
  return(obj)
})

setMethod('as.snippet', signature('function'),
function (obj)
{
  snippet(code=obj)
})

setMethod('as.snippet', signature('list'),
function (obj)
{
  s <- lapply(obj, as.snippet) # Make sure that the components are snippets...
  s <- lapply(s, code)         # Extract the code...
  as.snippet(.weavecode(s))    # And bind them together.
})

#setMethod('as.snippet', signature('character'),
#function (obj)
#{
#  ##DEBUG: is this code necessary? ##an.expression <- parse(text=obj)
#  ##language <- as.list(an.expression)[[1]] ## extract call
#  ##END DEBUG## snippet(code=language)
#  snippet(code=obj)
#})

setMethod('as.snippet', signature('ANY'),
function (obj)
{
  if (is.code(obj))
    snippet(code=obj)
  else if (is.null(obj))
    snippet()
  else {
    stop("Cannot coerce object of type '",typeof(obj),"' to snippet")
  }
})

# ================================================================================
# as.function - coerce to a function
# ================================================================================

as.function.snippet <- function(x)
{
  f <- function () NULL
  body(f) <- code(x) # set first body, then set formals. Not the other way around.
  formals(f) <- arguments(x)
  f
}

# ================================================================================
# run - run a snippet, in a specified environment or in GlobalEnv
# ================================================================================

setMethod('run', signature(obj='snippet', name='missing', envir='list'), 
function (obj, envir=NULL)
{
  if (is.null(envir)) return(eval(code(obj)))
  f <- substitute(function () fun, list(fun=code(obj)))
  .temp <- eval(f, envir)
  .temp()
})


# ================================================================================
# print.snippet - print a snippet on the console
# ================================================================================

print.snippet <- function (x)
{
  name <- names(x)
  if (length(name)>0) 
    cat("A code snippet, name='",name,"'",sep="")
  else
    cat("An unnamed code snippet")
  args. <- arguments(x)
  if (length(args.)>0) {
    cat(", arguments=",paste(names(args.)))
  }
  cat("\n")
  print(code(x))
}

# ================================================================================
# +.snippet - merge two snippets into one
# ================================================================================

"+.snippet" <- function(x,y)
{
  as.snippet(.weavecode(list(code(x),code(y))))
}

# ================================================================================
# *.snippet - multiply a snippet and merge into one
# ================================================================================

"*.snippet" <- function(n,y)
{
  if (is.numeric(y)) {
    temp <- n; n <- y; y <- temp
  }
  if (is.snippet(n)) {
    stop("*.snippet: Cannot multiply snippet by snippet")
  }
  as.snippet(.weavecode(rep(list(code(y)), n)))
}

# ================================================================================
# is.null.snippet - is the code inside a snippet NULL?
# ================================================================================

is.null.snippet <- function (x)
{
  is.null(code(x))
}

# ================================================================================
# c.snippet - concatenate (merge) snippets
# ================================================================================

c.snippet <- function(..., recursive=FALSE)
{
  codelst <- lapply(list(...), code)
  as.snippet(.weavecode(codelst))
}

# ================================================================================
# .funcall - make a function call snippet out of a snippet
# ================================================================================

.funcall <- function(x)
{
  if (!is.snippet(x)) stop("x must be a snippet")
  name <- names(x)
  if (length(name)<1) name <- paste('unnamed',trunc(runif(1)*10000),sep='')
  params <- arguments(x) # formals
  np <- names(params)
  notdots <- (np!='...')
  np <- np[ notdots ]
  params <- params[ notdots ]
  Call <- as.language(paste(name, '(', paste(names(params), collapse=','), ')'))
  snippet(code=Call)
}

# ================================================================================
# .fundef - make a function definition snippet out of a snippet
# ================================================================================

.fundef <- function(x)
{
  if (!is.snippet(x)) stop("x must be a snippet")
  sn <- snippet(PARAM <- FUN)
  substit(sn) <- list(PARAM=names(x), FUN=as.function(x))
  sn
}

# ================================================================================
# timer - compare the execution times of two snippets
# ================================================================================

timerOLD <- function(s1, s2, n=1)
{
  if (!is.snippet(s1) && !is.snippet(s2)) stop("s1, s2 must be snippets")
  .t <- matrix(NA, n, 2)
  cat("Comparing expr 1 to expr 2. Printing the ratios 100*time2/time1-100.\n")
  cat("Positive %: [1] is faster. Negative %: [2] is faster\n")
  for (.i in seq(length=n)) {
    .t[.i,1] <- max(na.omit(system.time(run(s1))))
    .t[.i,2] <- max(na.omit(system.time(run(s2))))
     cat(round(.t[.i,1],1),":",round(.t[.i,2],1)," ")
     ratio <- .t[.i,2]/.t[.i,1]
     if (ratio>1) cat('+')
     cat(round(100*ratio-100,0), '%\n', sep='')
  }
  mean1 <- mean(.t[,1],trim=0.1)
  mean2 <- mean(.t[,2],trim=0.1)
  cat("\n(Trimmed) mean of time 1: ",mean1,"\n")
  cat("(Trimmed) mean of time 2: ",mean2,"\n")
  ratio <- mean1/mean2
  if (ratio<1)
    cat('[1] is ', round(100/ratio-100,0), '% faster\n')
   else
    cat('[2] is ', round(100*ratio-100  ,0), '% faster\n')
  .t
}

timer <- function(s1, s2, n=1)
{
  if (!is.snippet(s1) && !is.snippet(s2)) stop("s1, s2 must be snippets")
  .t <- matrix(NA, n, 2)
  cat("Comparing expr 1 to expr 2. Printing the ratios 100*time2/time1-100.\n")
  cat("Positive %: [1] is faster. Negative %: [2] is faster\n")
  for (.i in seq(length=n)) {
    .t[.i,1] <- max(na.omit(system.time(run(s1))))
    .t[.i,2] <- max(na.omit(system.time(run(s2))))
     cat(round(.t[.i,1],1),":",round(.t[.i,2],1)," ")
     ratio <- .t[.i,2]/(.t[.i,1]+1e-22)
     if (ratio>1) cat('+')
     cat(round(100*ratio-100,0), '%\n', sep='')
  }
  mean1 <- mean(.t[,1],trim=0.1)
  mean2 <- mean(.t[,2],trim=0.1)
  cat("\n(Trimmed) mean of time 1: ",mean1,"\n")
  cat("(Trimmed) mean of time 2: ",mean2,"\n")
  ratio <- mean1/mean2
  if (ratio<1)
    cat('[1] is ', round(100/ratio-100,0), '% faster\n')
   else
    cat('[2] is ', round(100*ratio-100  ,0), '% faster\n')
  .t
}

# 
# ================================================================================
# llapply: 
#   X : a list
#   Y : a list
# Compute a list with (FUN(X[[1]],Y), FUN(X[[2]], Y), ...
# where FUN(x,Y) is a list FUN(x,Y[[1]]), FUN(x,Y[[2]]), ...
# ================================================================================
# 

llapply <- function (X, Y, FUN, ...)
{
  FUN <- match.fun(FUN)
  if (!is.list(X)) X <- as.list(X)
  if (!is.list(Y)) Y <- as.list(Y)
  yxFUN <- function(x, y, ...) FUN(y, x, ...)
  iFUN <- function(x, Y, ...) lapply(Y, yxFUN, x, ...)
  l <- lapply ( X, iFUN, Y, ...)
  l
}

# ================================================================================
# lllapply - apply llapply to two lists of lists
#   X : a list of lists
#   Y : a list
# For each X[[1]], X[[2]], ... compute llapply(X[[i]], Y).
# ================================================================================

lllapply <- function (X, Y, FUN, ...)
{
  FUN <- match.fun(FUN)
  if (!is.list(X)) X <- as.list(X)
  if (!is.list(Y)) Y <- as.list(Y)
  lapply( X, function (x,y, ...) llapply(x, Y, FUN, ...) )
}

# ========================================================================
# lcollapse - apply a function to a certain level of a list.
# ========================================================================

lcollapse <- function (x, level=1, FUN, ...)
{
  if (level<1) return (FUN(x))
  lapply( x, lcollapse,  level-1, FUN, ...)
}


# ================================================================================
# snippetlist class : a list of snippets
# ================================================================================


# ================================================================================
# as.snippetlist
# ================================================================================

setMethod('as.snippetlist', signature('snippetlist'),
function (obj)
{
   return(obj)
})

setMethod('as.snippetlist', signature('list'),
function (obj)
{
  slist <- obj
  if (is.null(s.names <- names(slist))) {
    s.names <- rep('', times=length(slist))
  }
  if (any(s.names=='')) {
    s.names[ s.names=='' ] <- 'NA'
    names(slist) <- s.names
  }
  for (i in seq(along=slist)) {
    slist[[i]] <- as.snippet(slist[[i]])
  }
  new('snippetlist', snippets=slist)
}) # as.snippetlist for list

snippetlist <- function (...)
{
  slist <- list(...)
  as.snippetlist(slist)
}

# ================================================================================
# is.snippetlist - is the object a snippetlist?
# ================================================================================

is.snippetlist <- function (obj) is(obj, 'snippetlist')

# ================================================================================
# snippets.snippetlist - return snippetlist as a plain list of snippets
# ================================================================================

setMethod('snippets', signature('snippetlist'), 
function (obj)
{
  obj@snippets
})

# ================================================================================
# as.list.snippetlist
# ================================================================================

as.list.snippetlist <- function (obj)
{
  snippets(obj)
}

# ================================================================================
# as.snippet.snippetlist - bind the snippets in a snippetlist into a single snippet
# ================================================================================

setMethod('as.snippet', signature('snippetlist'),
function (obj)
{
  as.snippet(as.list(obj))
})

# ================================================================================
# Various : print.snippetlist, clist, c.snippetlist
# ================================================================================


print.snippetlist <- function (x)
{
  s <- snippets(x)
  cat('A snippetlist object containing a list (of length ',length(s),') of the following snippets:\n',sep="")
  print(s)
}


clist <- function (X, Y)
{
  Y.names <- names(Y)
  if (length(Y.names)>0) {
    lapply(Y.names, function (name) { X[[name]] <<- Y[[name]]; Y[[name]] <<- NULL })
  }
  c(X,Y)
}

c.snippetlist <- function (..., recursive=FALSE)
{
  slist <- list(...)
  if (length(slist)<=1) return(slist)
  slist.new <- snippets(slist[[1]]) # The components of slist are lists
  for (i in seq(from=2, to=length(slist))) {
     slist.new <- clist(slist.new, snippets(slist[[i]]))
  }
  as.snippetlist(slist.new)
}

# ================================================================================
# [.snippetlist - 
# ================================================================================

"[.snippetlist" <- function (x, i)
{
  as.snippetlist(as.list(x)[i])
}

# ================================================================================
# [[.snippetlist - 
# ================================================================================

"[[.snippetlist" <- function (x, i)
{
  as.list(x)[[i]]
}

# ================================================================================
# $.snippetlist - 
# ================================================================================

"$.snippetlist" <- function (x, i)
{
  as.list(x)[[i]]
}

# ================================================================================
# $<-.snippetlist - 
# ================================================================================

"$<-.snippetlist" <- function (x, i, value)
{
  xl <- as.list(x)
  xl[[i]] <- value
  as.snippetlist(xl)
}

# ================================================================================
# as.snippet.snippetlist - return the list as one single snippet
# ================================================================================

setMethod('as.snippet', signature('snippetlist'),
function (obj)
{
  as.snippet(lapply(snippets(obj), code))
})

# ================================================================================
# run.snippetlist
# ================================================================================

setMethod('run', signature(obj='snippetlist', name='character', envir='list'), 
function (obj, name, envir=NULL)
{
  snip <- obj[[name]]
  run(snip, envir=envir)
})



# ================================================================================
# .snippetapply
# ================================================================================

.snippetapply <- function(x, y)
{
  if (!(is.snippet(x))) stop('x must be a snippet')
  if (!is.snippetlist(y)) stop('second argument must be a snippetlist')
  arg <- arguments(x, snippetlist=TRUE)
  arg <- c(arg, y) # Another snippetlist
  substit(x) <- arg
  x
}

# ================================================================================
# codetemplate class - 
# ================================================================================

codetemplate <- function (snippets=snippetlist())
{
  s <- as.snippetlist(snippets)
  new('codetemplate', snippets=s)
}

setMethod('templates', signature('codetemplate'), 
function (obj)
{
  snippetlist()
})

# snippets() gives the template-processed snippets.

setMethod('snippets', signature('codetemplate'), 
function (obj)
{
# This routine is quite slow.
  templs <- templates(obj)
  snips <- obj@snippets
  slist <- lapply(as.list(templs), .snippetapply, snips)
  new.snips <- as.snippetlist(slist)
  new.snips <- c(snips, new.snips) # In this order.
  # Now apply the templates again with the larger set of snippets.
  a <- lapply(as.list(templs), .snippetapply, new.snips)
  as.snippetlist(a) ## DEBUG: changed 2005/05/24. Ok?? Looks ok. 2005/06/14.
})

setMethod('as.snippet', signature('codetemplate'), 
function (obj)
{
  as.snippet(snippets(obj))
})

setMethod('run', signature(obj='codetemplate', name='character', envir='list'), 
function (obj, name, envir=NULL)
{
  run(snippets(obj), name, envir)
})

# ================================================================================
# codetemplatelist class - 
# ================================================================================

codetemplatelist <- function (slist=list(snippetlist()))
{
  s <- lapply(slist, as.snippetlist)
  new('codetemplatelist', slist=s)
}

setMethod('snippets', signature('codetemplatelist'),
function (obj)
{
  x <- lapply(obj@slist, snippets)
  x <- lapply(x, as.list)
  all.names <- unique(unlist(lapply(x, names)))
  ## do not match with '[[' since it matches partially. We want exact, or NULL. Use el().
  list.of.snippets <- sapply(all.names, function (name) as.snippet(lapply(x, 'el', name)))
  snips <- as.snippetlist(list.of.snippets)
  #
  templs <- templates(obj)
  slist <- lapply(as.list(templs), .snippetapply, snips)
  c(snips, as.snippetlist(slist))
})

setMethod('as.snippet', signature('codetemplatelist'), 
function (obj)
{
  as.snippet(snippets(obj))
})

setMethod('run', signature(obj='codetemplatelist', name='character', envir='list'), 
function (obj, name, envir=NULL)
{
  run(snippets(obj), name, envir=envir)
})

# end of snippet.R


# ========================================================================
# class Parameter (Base class for SamplingSchemes and Data class)
# ========================================================================
#

setClass('Parameter', 
  representation(
    id='numeric',
    name='character',
    stemname='character',
    basename='character',
    savename='character', # Currently not used.
    impute='logical',
    length='numeric',
    total.length='numeric',
    dimension='numeric',
    check.ok='logical',
    vector.names='character',
    value='numeric',
    missing='numeric'
  ),
  contains='codetemplate'
)

# NAME:
#  Parameter-class - Class definition of "Parameter" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  This is the base class for all model parameters that need to be 
#  updated within the sampler loop.
#  [Parameter] is the parent class for ``sampling schemes" ([[SamplingScheme-class]], 
#  which contain such as [[Gibbs]] and [[Metropolis]]),
#  and ``raw code" ([[RawCode-class]]). 
#
# SLOTS:
#  ~ id   : an unique id
#  ~ name : full specification of the parameter name, with brackets and but without suffixes.
#  ~ stemname : `stem' of the name, not including brackets or suffixes, (e.g. ``theta").
#  ~ basename : the name of the parameter, including brackets, but not including suffixes such as ``.mis" (e.g. "theta[1]")
#  ~ savename : the parameter specification as the user specified it, (e.g. "theta[1].mis")
#  ~ impute : (not used)
#  ~ length : length of the actual 'missing' values: [length(theta.mis)]
#  ~ total.length : length of the 'stemname' vector : [length(theta)]
#  ~ dimension : 
#  ~ check.ok : TRUE if parameter check went ok
#  ~ vector.names : (not used)
#  ~ value : 
#  ~ missing :
#


# ========================================================================
# .mis.name - suffix to identity parameters within data vectors: "missing values"
# ========================================================================

.mis.name <- function ()
{
  ".mis"
}

# ========================================================================
# constantParam.Parameter
# =======================================================================

setMethod('constantParam', signature('Parameter'),
function (obj)
{
  FALSE # By default, not. Only 'Data' and 'DFunction' are constants.
})



# ========================================================================
# names.Parameter
# =======================================================================

setMethod('name', signature('Parameter'),
function (obj)
{
  obj@name
})

# ========================================================================
# env.Parameter
# =======================================================================

setMethod('env', signature('Parameter'),
function (obj)
{
  list()
})


# ========================================================================
# init.Parameter - 
# ========================================================================

setMethod('init', signature(obj='Parameter', name='character', id='numeric'),
function (obj, name, id)
{
  if (nchar(name)<1) {
    name <- "noname"
  }
  basename <- unlist(strsplit(name, paste('\\', .mis.name(), '$', sep='')))
  contains.mis <- (nchar(basename)<nchar(name))
  if (contains.mis) {
    obj@savename <- name
    name <- paste(basename, "[", .paste.names(basename, "NA"), "]", sep="")
  } else {
    obj@savename <- name
  }
  obj@name <- name
  obj@id <- id
  #
  stem <- sub('^([^\\[]+).*$', '\\1', basename)
  imputing <- (name!=stem)
  obj@stemname <- stem
  update.fun <- paste('update.', stem, id, sep='')
  make.update.fun <- paste('makeUpdate.', stem, id, sep='')
  init.fun   <- paste('init', stem, id, sep='.')
  generic.name   <- paste('param', id, sep='.')
  name.nas <- .paste.names(stem, "NA")
  if (imputing) {
    timer.name <- paste(stem, id, sep='.')
  } else {
    timer.name <- paste(stem, sep='.')
  }
  more.snippets <- snippetlist(
    PARAM=as.language(name),
    TIMER.QPARAM=timer.name,
    SAVENAME=as.language(obj@savename),
    QPARAM=name,
    BASEPARAM=as.language(basename),
    STEM=as.language(stem),
    QSTEM=stem,
    UPDATEFUN=as.language(update.fun),
    MAKEUPDATEFUN=as.language(make.update.fun),
    INITFUN=as.language(init.fun),
    MISSING=as.language(name.nas),
    IMPUTING=imputing,
    GPARAM=as.language(generic.name)
  )
  obj@snippets <- c(obj@snippets, more.snippets)
  obj
})


# ========================================================================
# templates.Parameter - 
# ========================================================================

setMethod('templates', signature('Parameter'),
function(obj)
{
  Parameter.templates <- snippetlist(
    tpl.init.param.check = NULL,
    tpl.update.param.check = NULL,
    tpl.update.param.timer = NULL,
    tpl.init.local   = NULL,
    tpl.init.initfun = NULL,
    tpl.init.param   = NULL,
    tpl.init.trace   = NULL,
    tpl.init.sampler = NULL,
    tpl.init.updater = NULL,
    tpl.init.chain   = NULL,
    tpl.update.param = NULL,
    tpl.update.trace = NULL,
    tpl.start.chain  = NULL,
    tpl.end.chain    = NULL,
    tpl.end.sampler  = NULL,
    tpl.get.names    = .tpl.get.names.Parameter
  )
  Parameter.templates
})


# ========================================================================
# template .tpl.get.names.Parameter - 
# ========================================================================

.tpl.get.names.Parameter <- function ()
{
  # Assume tpl.init.param is done. This goes into a function.
  STEM <- .get.param.names(QSTEM, STEM) # Temporarily override STEM.  Do NOT use <<-.
}

# end Parameter.R


# ========================================================================
# class SamplingScheme
# ========================================================================

setClass('SamplingScheme', 
  representation(),
  contains='Parameter'
)

# NAME:
#  SamplingScheme-class - Class definition of "SamplingScheme" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  This is the base class for all model parameters that need a Metropolis-
#  style updating routine.  
#  [SamplingScheme] is the parent class for such classes as
#  [[Gibbs]] and [[Metropolis]]).
# 
# NOTE:
#  [RawCode] ([[RawCode-class]]) inherits directly from [[Parameter-class]].
#


# ========================================================================
# templates.SamplingScheme - 
# ========================================================================

# Use default ones, see template.Parameter

# DEBUG: are 'end.sampler, init.sampler' etc necessary??

setMethod('templates', signature('SamplingScheme'),
function(obj)
{
  SamplingScheme.templates <- snippetlist(
      tpl.init.param.check   = .tpl.init.param.check.SamplingScheme,
      tpl.update.param.check = .tpl.update.param.check.SamplingScheme,
      tpl.update.param.timer = .tpl.update.param.timer.SamplingScheme,
      tpl.init.sampler = .tpl.init.sampler.SamplingScheme,
      tpl.init.initfun = .tpl.init.initfun.SamplingScheme,
      tpl.init.param   = .tpl.init.param.SamplingScheme,
      tpl.init.local   = .tpl.init.local.SamplingScheme,
      tpl.set.env      = .tpl.set.env.SamplingScheme
    )
  c(callNextMethod(), SamplingScheme.templates)
})



# ========================================================================
# template .tpl.init.local.SamplingScheme - 
# ========================================================================

# Initialize the variables so that they be available in the proper closure
# that is, make.chainsampler.

.tpl.init.local.SamplingScheme <- function ()
{
  if (IMPUTING) {
    if (is.null(localized[[QSTEM]])) {
      if (exists(QSTEM)) {
        STEM <- STEM # Global to Local. (The "global" version is in a list given to "env".)
        localized[[QSTEM]] <- TRUE
ifdebug(QSTEM, " is localized.")
      } else {
        cat(QSTEM, " does not exist!\n")
      }
    }
  } else {
    STEM <- NULL
  }
  UPDATEFUN <- NULL
  .Umacs.timer.local$TIMER.QPARAM <- c()
}


# ========================================================================
# template .tpl.init.sampler.SamplingScheme - 
# ========================================================================

.tpl.init.sampler.SamplingScheme <- function () NULL ## Ok???

# ========================================================================
# template .tpl.init.initfun.SamplingScheme - 
# ========================================================================

.tpl.init.initfun.SamplingScheme <- function () INITFUN  <- function () CODEINIT

# ========================================================================
# template .tpl.init.param.SamplingScheme
# ========================================================================

.tpl.init.param.SamplingScheme <- function () PARAM <<- INITFUN()

# ========================================================================
# .tpl.init.param.check.SamplingScheme (Template)
# ========================================================================

.tpl.init.param.check.SamplingScheme <- function()
{
  # This checks only the initialization routine
  .error <<- FALSE
  whisper("Parameter ", QPARAM, ": trying initialization routine... ")
  if (IMPUTING) {
    if (length(PARAM)<1) {
      error(QPARAM, " Trying to impute a zero-length variable")
    }
  }
  .ip <- try(tpl.init.param, silent=TRUE) # Initialize
  if (.is.error(.ip)) {
    whisper("failed!\n>>>")
    error("Could not initialize",QPARAM,":",.ip)
    PARAM <<- numeric(0)
  } else {
    whisper("done. length=",length(PARAM),".\n")
    .check.param.value("Initializer",PARAM)
    if (length(PARAM)==0) error("Parameter ", QPARAM, " has zero length.")
  }
if (debug) {
cat("Value for ", QPARAM, ":\n")
print(PARAM)
}
  errors <<- errors | .error
## verbose <<- FALSE ## Just a kludge to prevent 'whispering'
}

# ========================================================================
# .tpl.update.param.check.SamplingScheme (Template)
# ========================================================================

.tpl.update.param.check.SamplingScheme <- function()
{
  # Assume here that all parameters have been initialized.
  verbose <<- TRUE
  .error <<- FALSE
  param.length <- length(PARAM)
  old.param <- PARAM
  d1 <- dim(old.param)
  whisper("Parameter ", QPARAM, ": trying updating routine... ")
  .up <- try(tpl.update.param, silent=TRUE) # Updating
  d2 <- dim(PARAM)
  if (.is.error(.up)) {
    whisper("failed!\n>>>")
    error("Could not update",QPARAM,":",.up)
    PARAM <<- numeric(0)
  } else {
    whisper("done. length=",length(PARAM),".\n")
    .check.param.value("Updater",PARAM)
    if (length(PARAM)==0) error("Parameter ", QPARAM, " has zero length.")
  }
  if (!.error) {
    dim.mismatch <- ((length(d1) != length(d2)) ||  (!all(d1==d2)))
    if (length(PARAM) != param.length) {
       error("Parameter ", QPARAM, ": Length at initialization:", param.length, " but updater returns a vector of length ", length(PARAM))
    }
    if (dim.mismatch) {
      if (is.null(d1)) d1 <- "NULL"
      if (is.null(d2)) d2 <- "NULL"
      error("Parameter ", QPARAM, ": Dimension at initialization:", d1, " but after updating: ", d2)
    }
  } else {
    PARAM <<- old.param
  }
  errors <<- errors | .error
  "------------------------------------------------------------------------------------"
}


# ========================================================================
# template .tpl.set.env.SamplingScheme - 
# ========================================================================

.tpl.set.env.SamplingScheme <- function ()
{
  ## Maybe move this out to somewhere else? to Sampler?
  if (length(Umacs$chain)<chain) Umacs$chain[[chain]] <<- list()
  ## Don't save the following: it's most duplication and promises.
  ## Umacs$chain[[chain]]$updater$QPARAM <<- .getenv(UPDATEFUN)
}


# ========================================================================
# .tpl.update.param.timer.SamplingScheme (Template)
# ========================================================================

.tpl.update.param.timer.SamplingScheme <- function()
{
  .t1 <- proc.time()
  tpl.update.param
  .elapsed <- (proc.time()-.t1)[1]
  .Umacs.timer.local$TIMER.QPARAM <<- c(.Umacs.timer.local$TIMER.QPARAM, .elapsed)
}

# end SamplingScheme

# ========================================================================
# class RawCode  -  pure code, no named parameter
# ========================================================================
# Any assignment operation updating a parameter
# must use <<- within a RawCode segment..
#

setClass('RawCode', 
  representation(),
  contains='Parameter'
)

# NAME:
#  RawCode-class - Class definition of "RawCode" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[RawCode]] inside a [[Sampler]]
#  function call,
#  which outputs a [RawCode] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to [[Parameter-class]].
# 




# ========================================================================
# RawCode  -  create a RawCode code SamplingScheme
# ========================================================================
#

RawCode <- function(update, init=NULL)
{
  
  snips <- snippetlist(
    code.update       = update,
    code.init.param   = init,
    code.init.chain   = NULL, ## DEBUG: Not used yet
    code.end.chain    = NULL, ## DEBUG: Not used yet
    code.init.sampler = NULL, ## DEBUG: Not used yet
    code.end.sampler  = NULL ## DEBUG: Not used yet
  )
  new('RawCode', snippets=snips)
}

# NAME:
#   RawCode - Generate a RawCode object for Umacs Sampler function
#
# DESCRIPTION:
#  Generates a [RawCode] object that is used to initialize an R function 
#  that performs a Gibbs updating set within the sampler function.
#
# ARGUMENTS:
#  ~ update : An R function to be executed in the iteration loop
#  ~ init : An R function to be executed before the iteration loop (chain); by default no code ([NULL])
#
# DETAILS:
#  [RawCode] is to be used only within the [Sampler] function call;
#  the value of the argument ([x]) is made available (locally)
#  in a sampling function that is built using the Umacs function [Sampler]
#
#  The name of the parameter that will have the value ([x])
#  is __not__ specified here, but only within the [Sampler] function call.
#
# VALUE:
#  An object of class [RawCode]
#  to be further processed by the Umacs function [Sampler]
#
# NOTE:
#  Usually a call to this function is not necessary,
#  since it is implicitly done when an R function is given in the argument list
#  of the [Sampler] function call without a name.
#
#  [RawCode] __must__ be associated with a parameter name.  
#  Components of vector parameters specified to be [RawCode]s cannot be updated,
#  but those specified to be [Data] can be updated. 
#  
#  To specify a local function that is supposed to be available,
#  use the function \code{LocalFunction}.
#



# ========================================================================
# constantParam.Raw
## DEBUG: kludge to prevent RawCode "parameters" from being collected.
# =======================================================================

setMethod('constantParam', signature('RawCode'),
function (obj)
{
  TRUE
})

# ========================================================================
# templates.RawCode - 
# ========================================================================

setMethod('templates', signature('RawCode'),
function(obj)
{
  parent.templates <- callNextMethod()
  RawCode.templates <- snippetlist(
    tpl.init.sampler     = .tpl.init.sampler.RawCode,
    tpl.init.initfun     = .tpl.init.initfun.RawCode,
    tpl.init.param       = .tpl.init.param.RawCode,
    tpl.init.updater     = .tpl.init.updater.RawCode,
    tpl.init.chain       = .tpl.init.chain.RawCode,
    tpl.update.param     = .tpl.update.param.RawCode,
    tpl.get.names        = NULL
  )
  c(parent.templates, RawCode.templates)
})

# ========================================================================
# template .tpl.init.sampler.RawCode - 
# ========================================================================

.tpl.init.sampler.RawCode <- function ()
{
  code.init.sampler
}

# ========================================================================
# template .tpl.init.initfun.RawCode - 
# ========================================================================

.tpl.init.initfun.RawCode <- function ()
{
  INITFUN <- function () code.init.param
}

# ========================================================================
# template .tpl.init.param.RawCode - 
# ========================================================================

.tpl.init.param.RawCode <- function () INITFUN()

# ========================================================================
# template .tpl.init.updater.RawCode - 
# ========================================================================

.tpl.init.updater.RawCode <- function () NULL

# ========================================================================
# template .tpl.init.chain.RawCode - 
# ========================================================================

.tpl.init.chain.RawCode <- function () code.init.chain

# ========================================================================
# template .tpl.update.param.RawCode - 
# ========================================================================

.tpl.update.param.RawCode <- function ()
{
  code.update
}


# End RawCode.R
# ========================================================================
# class Gibbs  -  Gibbs SamplingScheme
# ========================================================================

setClass('Gibbs', 
  representation(),
  contains='SamplingScheme'
)

# NAME:
#  Gibbs-class - Class definition of "Gibbs" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[Gibbs]] inside a [[Sampler]]
#  function call,
#  which outputs a [Gibbs] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [SamplingScheme] ([[SamplingScheme-class]])
# 



# ========================================================================
# Gibbs  -  specify a Gibbs SamplingScheme
# ========================================================================
# , chain.init=NULL, chain.end=NULL, sampler.init=NULL, sampler.end=NULL)

Gibbs <- function(update, init)
{
  snips <- snippetlist(
    code.update       = update,
    code.init.param   = init,
    CODEUPDATE = update,
    CODEINIT   = init
  )
  new('Gibbs', snippets=snips)
}

# NAME:
#   Gibbs - Generate a Gibbs object for Umacs Sampler function
#
# DESCRIPTION:
#  Creates a [Gibbs] object that is used to initialize 
#  an R function that performs a Gibbs updating step within the 
#  sampler function generated by [[Sampler]].
#
#  The sampler is built using the Umacs function [[Sampler]];
#  and the arguments ([update] and [init]) are embedded in the sampler function. 
#
# ARGUMENTS:
#  ~ update : An R function, with no arguments,
#             that samples directly from a (fully) conditional distribution.
#             May refer directly to variables that are specified in the [[Sampler]]
#             function call. Must return the updated value. 
#  ~ init : An R function that returns a (random) starting point for
#           a Markov chain for the parameter.
#           To be executed before the iteration loop (before each chain). 
#
# DETAILS:
#  [Gibbs] is to be used only within the [Sampler] function call.
#  The arguments of the update and init functions are ignored.
#  The body of the function can contain references to any parameters
#  that are defined in the [Sampler] function call, or to any 
#  variables in an enclosing environment (such as the Global Environment).
#
#  The name of the parameter that is being updated is __not__ specified in the
#  Gibbs object (and not in any of the argument functions either);
#  a call [Gibbs] __must__ be associated with a parameter name within
#  a [Sampler] function call.
#  
# VALUE:
#  An object of class [Gibbs],
#  to be further processed by the Umacs function [Sampler].
#
# SEEALSO:
#  [[Umacs]] and [[Sampler]] for examples on how to create sampler functions.
#


# ========================================================================
# templates.Gibbs
# ========================================================================

setMethod('templates', signature('Gibbs'),
function(obj)
{
  parent.templates <- callNextMethod()
  ##if (is.null(obj@snippets$CODEUPDATE)) { ## Not a good idea. Should be 'data' instead.
  ##  tpl.update.param <- NULL
  ##} else {
    tpl.update.param <- .tpl.update.param.Gibbs
  ##}
#print(tpl.update.param) #?Why does this work?
  Gibbs.templates <- snippetlist(
    tpl.init.updater = .tpl.init.updater.Gibbs,
    tpl.update.param = .tpl.update.param.Gibbs
  )
  c(parent.templates, Gibbs.templates)
})

# ========================================================================
# template .tpl.init.updater.Gibbs - 
# ========================================================================

.tpl.init.updater.Gibbs <- function () UPDATEFUN <<- function () CODEUPDATE

# ========================================================================
# template .tpl.update.param.Gibbs - 
# ========================================================================

.tpl.update.param.Gibbs <- function () PARAM <<- UPDATEFUN()


# end Gibbs


# ========================================================================
# class Data
# ========================================================================
# Difference between Data and Variable?


setClass('Data', 
  # Data is in the 1st element of the list in slot 'data'.
  representation(data='list', initfun='logical'),
  contains='Parameter'
)

# NAME:
#  Data-class - Class definition of "Data" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[Data]] inside a [[Sampler]]
#  function call,
#  which outputs a [Data] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to [[Parameter-class]].
# 



Data <- function (x)
{
  initfun <- is.function(x)
  if (initfun) {
    data.itself <- list()
    snips <- snippetlist(CODEINIT=x)
  } else {
    data.itself <- list(x)
    len <- length(x)
    if (len<1) error("Zero-length data vector.")
    snips <- snippetlist()
  }
##.show(initfun)
  new('Data', data=data.itself, initfun=initfun, snippets=snips)
}

# NAME:
#   Data - Specify Umacs Parameter As Data 
# DESCRIPTION:
#  Generates an object of class [Data] that embeds the argument value
#  as an object __locally__ in the sampler function generated by [[Sampler]].
#  The name of the object is taken to be the associated parameter name.
#  
#  Usually a call to this function is not necessary,
#  since [Data] is implicitly called when the value of a named
#  parameter in the argument list of [Sampler] is
#  an R object but not a function,numeric object or an
#  instance of the Umacs class [Parameter].
#   
# arguments:
#  ~ x : a numeric data object to be associated to the model parameter
#
# DETAILS:
#   The parameter is supposed to be `unmodeled data,' specified by
#   the likelihood. 
#
# NOTE:
#   The [[Sampler]] function recognizes numeric, named parameters
#   and wraps them by default in [Data(...)], so usually this function
#   is never explicitly called by the user.
#
#   To embed an R function locally into the sampler function,
#   use [[LocalFunction]] instead.
#
#   To embed just any non-function object locally into the sampler function,
#   use [[LocalObject]] instead.
#
# VALUE:
#   An object of class [Data].
#

# ========================================================================
# constantParam.Data
# =======================================================================

setMethod('constantParam', signature('Data'),
function (obj)
{
  TRUE
})


# ========================================================================
# env.Data
# =======================================================================
# Using this method when computing the closure ensures that
# the data variable is available within the closure of a 
#
# TODO: make sure that we don't have duplicate global variable when we have local ones!
#

setMethod('env', signature('Data'),
function (obj)
{
  x <- obj@data
  if (length(x)>0) names(x) <- obj@name
  x ## May be empty
})

# ========================================================================
# templates.Data - 
# ========================================================================

setMethod('templates', signature('Data'),
function(obj)
{

  if (obj@initfun) {
    Data.templates <- snippetlist(
      tpl.init.param    = .tpl.init.param.FData,
      tpl.init.param.check   = .tpl.init.param.check.FData,
      tpl.update.param.check = NULL,
      tpl.init.local   = .tpl.init.local.FData,
      tpl.init.sampler = .tpl.init.sampler.FData,
      tpl.init.initfun = .tpl.init.initfun.FData,
      tpl.init.chain   = NULL,
      tpl.update       = NULL,
      tpl.end.chain    = NULL,
      tpl.get.names    = NULL
    )
  } else {
    Data.templates <- snippetlist(
      tpl.init.param    = .tpl.init.param.Data,
      tpl.init.param.check   = .tpl.init.param.check.Data,
      tpl.update.param.check = NULL,
      tpl.init.local   = .tpl.init.local.Data,
      tpl.init.sampler = .tpl.init.sampler.Data,
      tpl.init.initfun = .tpl.init.initfun.Data,
      tpl.init.chain   = NULL,
      tpl.update       = NULL,
      tpl.end.chain    = NULL,
      tpl.get.names    = NULL
  )
  }
  c(callNextMethod(), Data.templates)
})


# ========================================================================
# template .tpl.init.local.Data / FData
# ========================================================================

.tpl.init.local.Data <- function () NULL
.tpl.init.local.FData <- function () NULL


# ========================================================================
# template .tpl.init.sampler.Data / FData
# ========================================================================

.tpl.init.sampler.Data <- NULL
.tpl.init.sampler.FData <- NULL

# ========================================================================
# template .tpl.init.initfun.Data / FData
# ========================================================================

.tpl.init.initfun.Data <- function ()
{
  # A variable local to the chain, so must not use <<-.
  MISSING <- if (length(PARAM)>0) which(is.na(PARAM)) else numeric(0) # Not <<-.
}

.tpl.init.initfun.FData <- function ()
{
  # A variable local to the chain, so must not use <<-.
  INITFUN <- function () CODEINIT
  PARAM <<- INITFUN()
  MISSING <- if (length(PARAM)>0) which(is.na(PARAM)) else numeric(0) # Not <<-.
}

# ========================================================================
# template .tpl.init.param.Data / FData
# ========================================================================

.tpl.init.param.Data  <- function () NULL
.tpl.init.param.FData <- function () PARAM <<- INITFUN()

# ========================================================================
# .tpl.init.param.check.Data / FData
# ========================================================================

.tpl.init.param.check.Data <- function()
{
  # This checks only the initialization routine
  whisper("Data variable ", QPARAM, " of type ", typeof(PARAM), ",")
  if (!is.null(dim(PARAM))) {
    whisper(" of dimensions (", paste(dim(PARAM),collapse=','), ")")
  } else {
    whisper(" of length ", length(PARAM))
  }
  whisper(" with ", length(MISSING)," missing values\n")
}

.tpl.init.param.check.FData <- function()
{
  # This checks only the initialization routine
  .error <<- FALSE
  whisper("Unmodeled Data Parameter ", QPARAM, ": trying initialization routine... ")
  if (IMPUTING) {
    if (length(PARAM)<1) {
      error(QPARAM, " Trying to impute a zero-length variable")
    }
  }
  .ip <- try(tpl.init.param, silent=TRUE) # Initialize
  if (.is.error(.ip)) {
    whisper("failed!\n>>>")
    error("Could not initialize",QPARAM,":",.ip)
    PARAM <<- numeric(0)
  } else {
    whisper("done. length=",length(PARAM),".\n")
    .check.param.value("Initializer",PARAM)
    if (length(PARAM)==0) error("Parameter ", QPARAM, " has zero length.")
  }
  if (debug) {
    cat("Value for ", QPARAM, ":\n")
    print(PARAM)
  }
  errors <<- errors | .error
}

# ========================================================================
# class DFunction  -  DFunction SamplingScheme
# ========================================================================
# DFunctions are like Gibbs updates but they're deterministic functions
# They're updated but they are not saved.
# Cf. LocalFunction which is just a local function definition.

setClass('DFunction', 
  representation(),
  contains='SamplingScheme'
)

# NAME:
#  DFunction-class - Class definition of "DFunction" (Umacs package)
#
# DESCRIPTION:
#  This class is used only internally, in Umacs.
#
# DETAILS:
#  Users need only use the function [DFunction] inside a [[Sampler]]
#  function call,
#  which outputs a [Gibbs] object and is directly processed by [[Sampler]].
#  (Alternatively, a call to [DFunction] can be omitted and only a 
#  function specified, as [Sampler]
#  is smart enough to wrap
#
# SLOTS:
#  Refer to the class [SamplingScheme] ([[SamplingScheme-class]])
# 
#

# ========================================================================
# DFunction  -  create a DFunction SamplingScheme
# ========================================================================
# , chain.init=NULL, chain.end=NULL, sampler.init=NULL, sampler.end=NULL)

DFunction <- function(update)
{
  snips <- snippetlist(
    code.update       = update,
    CODEUPDATE = update
  )
  new('DFunction', snippets=snips)
}


# NAME:
#  DFunction - Generate a DFunction object for Umacs Sampler function
#
# DESCRIPTION:
#  Creates a [DFunction] object that is used to embed 
#  an R function that performs a Gibbs updating step 
#  for a parameter that is a __deterministic function__
#   within the 
#  sampler function generated by [[Sampler]].
#
#  The main difference between [DFunction] and [[Gibbs]]
#  is that for [DFunction], there is no initializing function.
#  Consequently it is supposed to be updated only __after__
#  all parameter that it directly depends on are available.
#
#  The sampler is built using the Umacs function [[Sampler]];
#  and the argument function ([update]) is embedded in the sampler function. 
#
# ARGUMENT:
#  ~ update : An R function (with no arguments) that samples directly
#             from a (fully) conditional distribution
#             May refer directly to parameters specified in the [[Sampler]]
#             function call. Must return the updated value. 
#
# DETAILS: 
#  [DFunction] is to be used only within the [Sampler] function call.
#
#  Alternatively one can just supply the function ([update])
#  in the [[Sampler]] function call;
#  [[Sampler]] will call [[DFunction]] for you. 
#
#  The arguments of the update functions are ignored.
#  The body of the function can contain references to any parameters
#  that are defined in the [[Sampler]] function call, or to any 
#  variables in an enclosing environment (such as the Global Environment).
#
#  The name of the parameter that is being updated __not__ specified here,
#   but only in the parameter list in the [[Sampler]] function call.
#
# VALUE:
#  An object of class [[DFunction-class]]
#  to be further processed by the Umacs function [[Sampler]].
# 
# NOTE:
#  [DFunction] __must__ be associated with a parameter name
#  (in the [[Sampler]] function call). 
#  If you specify a plain function without a name, it will be 
#  interpreted as an argument to a [[RawCode]] function call
#  and __not__ as an argument to [[DFunction]].
#


# ========================================================================
# templates.DFunction
# ========================================================================

setMethod('templates', signature('DFunction'),
function(obj)
{
  parent.templates <- callNextMethod()
  DFunction.templates <- snippetlist(
    tpl.update.param       = .tpl.update.param.DFunction,
    tpl.init.updater       = .tpl.init.updater.DFunction,
    tpl.init.param.check   = .tpl.init.param.check.DFunction,
    tpl.update.param.check = .tpl.update.param.check.DFunction,
    tpl.init.initfun       = NULL,
    tpl.init.param         = NULL,
    tpl.get.names          = NULL
  )
  c(parent.templates, DFunction.templates)
})

# ========================================================================
# constantParam.DFunction
# =======================================================================
# This is the other feature what distinguishes DFunction from Gibbs:
# none of the DFunction updaters are saved.
# The biggest feature is that there is no initialization function.

setMethod('constantParam', signature('DFunction'),
function (obj)
{
  TRUE
})


# ========================================================================
# template .tpl.init.updater.DFunction - 
# ========================================================================

.tpl.init.updater.DFunction <- function () UPDATEFUN <<- function () CODEUPDATE

# ========================================================================
# template .tpl.update.param.DFunction - 
# ========================================================================

.tpl.update.param.DFunction <- function () PARAM <<- UPDATEFUN()


# ========================================================================
# .tpl.init.param.check.DFunction (Template)
# ========================================================================

.tpl.init.param.check.DFunction <- function ()
{
  whisper("Parameter ", QPARAM, ": (No initialization needed for a deterministic function.)\n")
}

# ========================================================================
# .tpl.update.param.check.DFunction (Template)
# ========================================================================

.tpl.update.param.check.DFunction <- function ()
{
  # Assume here that all parameters have been initialized.
  verbose <<- TRUE
  .error <<- FALSE
  whisper("Parameter ", QPARAM, ": trying updating routine... ")
  .up <- try(tpl.update.param, silent=TRUE) # Updating
  if (.is.error(.up)) {
    whisper("failed!\n>>>")
    error("Could not update",QPARAM,":",.up)
    PARAM <<- numeric(0)
  }
  errors <<- errors | .error
}


# end DFunction


# ========================================================================
# class LocalObject - define an object local to the sampler function
# ========================================================================
#

setClass('LocalObject', 
  representation(vble='list'),
  contains='Parameter'
)

# NAME:
#  LocalObject-class - Class definition of "LocalObject" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[LocalObject]] inside a [[Sampler]]
#  function call,
#  which outputs a [LocalObject] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [Parameter] ([[Parameter-class]])
# 




# ========================================================================
# LocalObject
# =======================================================================

# NAME:
#  LocalObject - Generate a LocalObject object for Umacs Sampler function
#
# DESCRIPTION:
#  Generates an object that is used to initialize an R object 
#  locally in a sampler function generated by Sampler.
#  Usually a call to this function is not necessary,
#  since it is implicitly done when the value of a named
#  parameter in the argument list of [Sampler] is
#  an R object but not a function, numeric object or an
#  instance of the Umacs class [Parameter].
#
# ARGUMENTS:
#  ~ x : An R object (not a function)
#
# DETAILS:
#  [LocalObject] is to be used only within the [Sampler] function call;
#  the value of the argument ([x]) is made available (locally)
#  in a sampling function that is built using the Umacs function [Sampler].
#
#  The name of the parameter that will have the value 
#  ([x]) is __not__ specified here, but only within the [[Sampler]] function call.
#
#  The components of a parameter vector initialized as [Data] parameters
#  cannot be individually updated using Gibbs and Metropolis updating schemes.
#
#  For local __functions__, use the function [[LocalFunction]] instead.
#
# VALUE:
#  An object of class [[LocalObject]],
#  to be further processed by the Umacs function [[Sampler]].
#
# NOTE:
#  [LocalObject] must be associated with a parameter name.  
#
#  Components of vector parameters specified to be [LocalObject]s cannot be updated,
#  but those specified to be [Data] can be updated. 
#


LocalObject <- function (x)
{
  new('LocalObject', vble=list(x), snippets=snippetlist())
}


# ========================================================================
# constantParam.LocalObject
# =======================================================================

setMethod('constantParam', signature('LocalObject'),
function (obj)
{
  TRUE
})


# ========================================================================
# env.LocalObject
# =======================================================================

setMethod('env', signature('LocalObject'),
function (obj)
{
  name <- obj@name
  x <- list(obj@vble[[1]])
  names(x) <- name
  x
})


# ========================================================================
# init.LocalObject - 
# ========================================================================

setMethod('init', signature(obj='LocalObject', name='character', id='numeric'),
function (obj, name, id)
{
  stem <- sub('^([a-zA-Z.]+).*$', '\\1', name)
  if (stem != name) {
    stop("Illegal name for a function :", name)
  }
  obj@name <- name
  snip <- snippetlist(PARAM=as.language(stem), QPARAM=stem)
  obj@snippets <- snip
  obj
})

# ========================================================================
# templates.LocalObject - 
# ========================================================================

setMethod('templates', signature('LocalObject'),
function(obj)
{
  # Do not use other templates.
  slist <- snippetlist(
      tpl.init.param.check   = .tpl.init.param.check.LocalObject,
      tpl.update.param.check = NULL
  )
  slist
})


# ========================================================================
# .tpl.init.param.check.LocalObject (Template)
# ========================================================================

.tpl.init.param.check.LocalObject <- function()
{
  whisper("LocalObject '",QPARAM, "' of type ", typeof(PARAM), "\n")
}



# end LocalObject.R

# ========================================================================
# class LocalFunction - "local" functions
# ========================================================================
#

setClass('LocalFunction', 
  representation(
    func='list'
  ),
  contains='Parameter'
)


# NAME:
#  LocalFunction-class - Class definition of "LocalFunction" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[LocalFunction]] inside a [[Sampler]]
#  function call,
#  which outputs a [LocalFunction] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [Parameter] ([[Parameter-class]])
# 


# ========================================================================
# constantParam.LocalFunction
# =======================================================================

LocalFunction <- function (x)
{
  new('LocalFunction', func=list(x), snippets=snippetlist())
}

# NAME:
#  LocalFunction -  Generate a LocalFunction object for Umacs Sampler function
# 
# DESCRIPTION:
#  Generates an object that is used to define a __local__ R function 
#  in a sampler function generated by [[Sampler]].
#
#  Usually a call to this function is not necessary,
#  since [LocalFunction] is implicitly done when the value of a __named__
#  parameter in the argument list of [[Sampler]] is an R function.
#
# ARGUMENT:
#  ~ x : An R function
#
# DETAILS:
#  [LocalFunction] is to be used only within the [Sampler] function call;
#  the value of the argument ([x]) is made available (locally)
#  in a sampling function that is built using the Umacs function [Sampler].
# 
# The name of the parameter that will have the value ([x]) is __not__
# specified as an argument, but only within the [[Sampler]] function call.
#
# VALUE:
#  An object of class [LocalFunction],
#  to be further processed by the Umacs function [[Sampler]].
#
# NOTE:
#  A call to [LocalFunction] must be associated with a parameter name.  
#
#  Components of vector parameters specified to be [LocalFunction]s cannot be updated,
#  but those specified to be [Data] can be updated. 
#
#  To insert `raw code' in the iteration loop, use the function [[RawCode]].
#





# ========================================================================
# constantParam.LocalFunction
# =======================================================================

setMethod('constantParam', signature('LocalFunction'),
function (obj)
{
  TRUE
})


# ========================================================================
# env.LocalFunction
# =======================================================================

setMethod('env', signature('LocalFunction'),
function (obj)
{
  return(list())
})


# ========================================================================
# init.LocalFunction - 
# ========================================================================

setMethod('init', signature(obj='LocalFunction', name='character', id='numeric'),
function (obj, name, id)
{
  stem <- sub('^([a-zA-Z.]+).*$', '\\1', name)
  if (stem != name) {
    stop("Illegal name for a function :", name)
  }
  obj@name <- name
  f <- .fundef(snippet(code=obj@func[[1]], name=name))
  snip <- snippetlist(
    PARAM=as.language(stem),
    QPARAM=stem,
    FUNDEF=f
  )
  obj@snippets <- snip
  obj
})

# ========================================================================
# templates.LocalFunction - 
# ========================================================================

setMethod('templates', signature('LocalFunction'),
function(obj)
{
  # Do not use other templates.
  slist <- snippetlist(
      tpl.init.param.check   = .tpl.init.param.check.LocalFunction,
      tpl.update.param.check = NULL,
      tpl.init.local=.tpl.init.local.LocalFunction
  )
  slist
})


# ========================================================================
# .tpl.init.local.LocalFunction (Template)
# ========================================================================

.tpl.init.local.LocalFunction <- function() FUNDEF


# ========================================================================
# .tpl.init.param.check.LocalFunction (Template)
# ========================================================================

.tpl.init.param.check.LocalFunction <- function()
{
  # DEBUG: get rid of this, eventually.
  whisper("Local function '",QPARAM, "'\n")
}



# end LocalFunction.R

# ========================================================================
# class ParameterList : collect all parameters together
# ========================================================================

setClass('ParameterList', 
  representation(
    params='list', # List of Parameter objects
    check.ok='logical'
  ),
  contains='codetemplatelist'
)

# NAME:
#  ParameterList-class - Class definition of "ParameterList" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Wrapper for a list of ``[Parameter]" objects.
# 



# ========================================================================
# .illegalName - is a name illegal?
# ========================================================================

.illegalName <- function (name)
{
  reserved.names <- c(".title", ".Rhat", ".logpost", ".timer")
  tf <- rep(NA, length(name))
  for (i in seq(along=name)) {
    n <- name[i]
    tf[i] <-  ((substr(n, 1, 1)==".") && !(n %in% reserved.names))
  }
  tf
}

# ========================================================================
# Parameters - collect all parameters together and check integrity
# ========================================================================

.Parameters <- function (plist, p.names)
{
  if (length(p.names)<1) p.names <- rep('', length(plist))
  if (any(illegals <- .illegalName(p.names))) {
    stop("The following parameter names are illegal: ", p.names[illegals])
  }
  error <- FALSE
  params <- list()
### say("Parameters and variables:\n")
  for (i in seq(along=plist)) {
    p <- plist[[i]]
    name <- p.names[i]
    if (nchar(name)==0) {
      #if (class(p)!='RawCode') { ## DEBUG: kludge.
      #  error("No parameter name for parameter #",i,"\n")
      #}
      name <- paste('param.', i, sep='') # Must be unique.
      name.unspecified <- TRUE
    } else {
      name.unspecified <- FALSE
    }
    ## parameter names prepended with dots are "Variables"
    isVariable <- (length(grep('^\\.', name))>0) 
    if (!is.name(try(as.name(name),silent=TRUE))) {
      error("Invalid name:",name,"\n")
    }
    if (class(p)=='list') {
      # If it's a list, then p is assumed to be a list of parameter objects
      params <- c(params, .Parameters(p, names(p)))
      next
    } else if (is(p, "Parameter")) {
      # Fine, let it pass.
    } else if (is.data.frame(p)) {
      list.p <- as.list(p)
      params <- c(params, .Parameters(list.p, names(list.p)))
      next
    } else if (is.function(p)) {
      ## A function which has been given a 'dotted' name is a function
      ## that is localized for a chain (LocalFunction)
      ## Or, we have 'raw code' that is executed as is, right there.
      ## DFunctions are deterministic functions.
      p <- if (name.unspecified) RawCode(p)
           else if (isVariable) LocalFunction(p) 
           else DFunction(p)
    } else if (is.numeric(p) || (is.logical(p) && length(p)>0 && all(is.na(p)))) {
      # Either a numeric vector or all NAs. 
      # Note that all(is.na(logical(0))) is TRUE!
      p <- Data(p)
    } else {
      p <- LocalObject(p)
    }
cat(name, ': ', class(p), "\n")
    p <- init(p, name, i)
    params <- c(params, list(p)) # Must use list(p).
  }
  params
}

#debug(.Parameters); cat("DEBUG: debug .Parameters ON")

Parameters <- function (...)
{
  plist <- list(...)
  p.names <- names(plist)
  params <- .Parameters(plist, names(plist))
  say("\n")
  #### save.names <- sapply(params, function (x) x@savename)
  names(params) <- sapply(params, name)
  say("Initializing...")
  slist <- lapply(params, snippets)
  obj <-  new('ParameterList', params=params, slist=slist, check.ok=FALSE)
####oo <<- obj ## DEBUG: remove
  obj <- init(obj)
  say("\n")
  obj
}

# ========================================================================
# constantParam.ParameterList
# =======================================================================

setMethod('constantParam', signature('ParameterList'),
function (obj)
{
  sapply(obj@params, constantParam)
})


# ========================================================================
# names.ParameterList
# =======================================================================

names.ParameterList <- function (x)
{
  sapply(x@params, name)

}
#setMethod('names', signature('ParameterList'),
#function (x)
#{
#  sapply(x@params, name)
#})


# ========================================================================
# env.ParameterList
# =======================================================================

setMethod('env', signature('ParameterList'),
function (obj)
{
  v <- lapply(obj@params, env)
  .flatten.list(v)
})

# ========================================================================
# run.ParameterList
# =======================================================================

setMethod('run', signature(obj='ParameterList', name='character', envir='missing'),
function (obj, name)
{
  run(snippets(obj), name, envir=env(obj))
})

# ========================================================================
# init.ParameterList
# ========================================================================
# Final initialization here, after we have collected
# all parameters together

setMethod('init', signature(obj='ParameterList', name='missing', id='missing'),
function (obj)
{
#cat("DEBUG: Trying to generate the Sampler check function\n")
  obj@check.ok <- SamplerCheck(obj)
#cat('DEBUG: finished generating sampler check\n')
  if (obj@check.ok) say("Parameter check went ok.\n")
  obj
})


# ========================================================================
# templates.ParameterList
# ========================================================================

setMethod('templates', signature('ParameterList'),
function(obj)
{
  snippetlist()
})




# end ParameterList.R

# ========================================================================
# class Sampler : engine for drawing mcmc simulations 
# ========================================================================

setClass('Sampler', 
  representation(
    params='ParameterList',
    all.param.names = 'character',
    check.only      ='logical'),
  contains='codetemplate'
)

# NAME:
#  Sampler-class - Class definition of "Sampler" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[Sampler]].
# 
# SLOTS:
#  ~ params : A single object containing the parameters of the sampler. 
#  ~ all.param.names : names of the parameters that are `random'
#  ~ check.only : flag to 
#  ~ snippets : Snippets of which sampler is built from


# ========================================================================
# env.function  -  get the list of variables associated with the closure
# ========================================================================

setMethod('env', signature('function'),
function (obj)
{
  as.list(environment(obj), all.names=TRUE)
})

# ========================================================================
# DEBUG (TESTING) Sampler  -  make an MCMC sampler function
# ========================================================================

# Somehow .timer gets matched with the last

Sampler <- function (.timer=FALSE, ...)
{
  .timer <- list(...)$.timer
  if (!is.logical(.timer) || is.na(.timer)) .timer <- FALSE
  p <- Parameters(...)
  if (.timer) {
    obj <- .Sampler(p, class='SamplerTimer')
  } else {
    obj <- .Sampler(p)
  }
  run(obj)
}


# ========================================================================
# Sampler  -  make an MCMC sampler function
# ========================================================================

Sampler <- function (...)
{
  p <- Parameters(...)
  obj <- .Sampler(p)
  run(obj)
}

#
# NAME:
#  Sampler - Generate a Markov chain sampler
# DESCRIPTION:
#  Generates a Markov chain sampling function given a list of parameters
# ARGUMENTS:
#  ~ ... : A list of parameters including Gibbs and/or Metropolis updating routines,
#          Raw functions and other objects
# DETAILS:
#  [Sampler] generates an R function to produce simulations 
#  wrapped in an  [mcts] object that can be coerced into 
#  random variable objects, for example. 
#
#  A [Sampler] function call features a named argument list that represents 
#  the named parameters in a Gibbs or Metropolis sampler.
#  The names are specified by the user (e.g. "theta", "alpha") 
#  and the values are either constants (vectors of data frames),
#  special Umacs objects implementing a sampling scheme 
#  ([Gibbs], [Metropolis], etc.).
#  Additionally, the argument list can contain assignments to special internal variables,
#  or unnamed arguments such as raw code to be embedded in the sampling loop.
# 
#  Special Umacs variables that are identified by the prepending dot (e.g. [.logpost])
#  and are not thus actual parameters in the model.
#
#  No user-defined parameter can start with a dot.
#
#  Unnamed arguments are either `raw' functions that are embedded `as is'
#  inside the sampling loop, or special Umacs objects such as [[Trace]] objects,
#   which provide a trace plots of a specified variable. 
#  The code that the user provides in the form of direct sampling functions
#  (via [Gibbs]) or log-posterior functions 
#  (via the various Metropolis updating schemes) can have references to
#  `global' variables that are available in the workspace,
#   but also to `local' variables that are specified within the [Sampler] function call.
# 
#  Ideally, the function should __only__ have references to
#  local variables so that the resulting sampler function is
#  a stand-alone function that encapsulates
#  a complete sampling environment independent of the working environment. 
#
# `Raw code' that is specified as an unnamed function in the argument list
#  is embedded as a function, so any assignments done using the [<-] operator are local.
# To change directly the value of a __parameter__, use the [<<-] operator. 
#
#  The allowed values for the __parameters__ that (are named by the user and)
#  can be passed as arguments are:
#
#  A constant numeric value (vector or array).
#  The variable is made available to the functions embedded in the sampler
#  (but not in Global Environment!), or, 
#  a [SamplingScheme] object that is built using one of
#  the following possible functions:
#  [[Gibbs]], [[Metropolis]], [[SMetropolis]], [[PMetropolis]],
#  [[PSMetropolis]].
#
#  These objects provide the updating and initialization functions for
#  a Gibbs updating step, or the log-posterior function
#  and an initialization function for a Metropolis updating step.
#
#  (Technically speaking, a `Gibbs' updating step is just a special case of
#  a `Metropolis' updating step in the sense that all samples generated
#  using the function of a `Gibbs' updating step are accepted unconditionally.)
#
#  The allowed values for the __unnamed__ objects that can be passed as arguments are a
#  [[Trace]] object, or a plain function (`raw code').
#  The plain function is converted internally first to a [[RawCode]] object,
#  so giving a plain function is made possible just for convenience.
#  See [[RawCode]] for details.
# 
# VALUE:
#  An R function that can be run to produce the simulations.
#
#  See the help page for [[Umacs]] to learn about the usage of the sampling function.
#
#  The function generated by [Sampler] returns an [[mcts]] object that can be 
#  converted into random variable objects.
#

Sampler.example <- function () {
  # don't run
  y <- rnorm(20, mean=100)
  s <- Sampler(y = y, 
    theta = Metropolis(
      logpost = function () -sum((y-theta)^2)/2,
      init = function () rnorm(1)
    )
  )
  # end don't run
}


# ========================================================================
# .Sampler  -  produce a Sampler instance
# ========================================================================

.Sampler <- function(
    params,
    check.only=FALSE,
    class='Sampler')
{
  random.params <- (!constantParam(params))
  names.all.params <- names(params)
  all.params <- names.all.params[ random.params ]
  obj <- new(class, params=params, all.param.names=all.params, check.only=check.only)
  if (!check.only && !params@check.ok) stop("Parameters have errors. Cannot build the sampler.")
  obj <- init(obj)
  obj
}
# ========================================================================
# init.Sampler
# ========================================================================

setMethod('init', signature(obj='Sampler', name='missing', id='missing'),
function (obj)
{
  pts <- obj@all.param.names
  pts.code <- if (length(pts)==1) as.language(pts) else snippet(call.name='c', arguments=pts)
  more.snippets <- snippetlist(
    CHECK.ONLY=obj@check.only,
    ALL.PARAMS=pts.code,
    TITLE=env(obj)$.title,
    DATE=date()
  )
  obj@snippets <- c(snippets(obj@params), more.snippets)
  obj
})

# ========================================================================
# env.Sampler
# =======================================================================

setMethod('env', signature('Sampler'),
function (obj)
{
  v.paramlist <- env(obj@params)
  v.sampler <- list() ## DEBUG: Really, nothing...? All come as constants, through snippets.
  v <- .flatten.list(list(v.paramlist,v.sampler))
  if (is.null(v$.title)) v$.title <- ''
  v
})

# ========================================================================
# run.Sampler
# ========================================================================

setMethod('run', signature(obj='Sampler', name='missing', envir='missing'),
function (obj)
{
  sniplist <- snippets(obj)
  run(obj=sniplist, name='make.sampler', envir=env(obj))
})

# ========================================================================
# templates.Sampler
# ========================================================================

setMethod('templates', signature('Sampler'),
function(obj)
{
  snippetlist(
    make.sampler              = .make.sampler.Sampler,
    tpl.fun.make.chainsampler = .tpl.fun.make.chainsampler.Sampler
  )
})


# ========================================================================
# template .tpl.fun.make.chainsampler.Sampler
# ========================================================================
# 
# This template will be embedded in a function body
#

.tpl.fun.make.chainsampler.Sampler <- function()
{
  # ================================================================================
  # Local timer (for debugging purposes)
  # ================================================================================

  .Umacs.timer.local <- list()

  # ================================================================================
  # All code that has references to a parameter must be inside this template.
  # All variables with assignment by "<-" are "local" to this environment.
  # ================================================================================

  sims       <- NULL    # Simulation matrix
  n.col      <- NA      # Number of columns in matrix 'sims'
  col.names  <- NA      # Column (variable) names of the matrix 'sims'

  next.iteration <- 1  # Next iteration number (absolute, always increasing)

  next.row   <- NA      # Next sims row number

  adapting   <- NA      # TRUE iff we're in the adapting phase (now same as 'burn-in')
  adapt.now  <- NA      # TRUE iff we should adapt right now

  ticks.per.iteration <- NA # calculate time per iteration

  # ================================================================================
  # Make sure that all chain-specific variables are defined in this closure:
  # ================================================================================

  localized <- list()  # Keep track of variables that have been localized.

  # This is used by e.g. SamplingScheme:

  tpl.init.local

  # ================================================================================
  # Initializations
  # ================================================================================

  tpl.init.initfun
  tpl.init.chain

  # ================================================================================
  # Local functions
  # ================================================================================

  .setenv <- function (chain) {
    tpl.set.env
    invisible()
  }

  .refresh <- function () {
    .setenv(chain)
    #
  }

  .get.col.names <- function () {
    # tpl.get.names temporarily overwrites the parameter with a list of its component names.
    # these are gathered together by ALL.PARAMS.
    tpl.get.names
    .a <- ALL.PARAMS
    dim(.a) <- NULL
    .a
  }

  .rearrange.sims <- function () {
    # Rearrange the simulation matrix so that
    # the order goes from oldest to newest.
    n.rows <- nrow(sims)
    if (next.row>1 & next.row<=n.rows) {
      .row.order <- c(next.row:n.rows, 1:(next.row-1))
      sims <<- sims[.row.order,,drop=FALSE]
      next.row <<- 1
    }
  }


# ================================================================================
# The "chainsampler" function: run one single chain
# ================================================================================

# The following function will be returned by make.chainsampler

function(n.iterUntil=NULL, n.simrows=NULL, sims.=FALSE, status=FALSE, debug=FALSE, env=FALSE)
{
  .Umacs.timer.local <<- list()
  # ================================================================================
  # sims/env/status
  # ================================================================================
  if (sims.) {
    dimnames(sims) <<- list(NULL, col.names)
    return(sims)
  } else if (env) {
    return(.setenv(chain)) ## 'chain' is a global variable, set in make.sampler.
  } else if (status) {
    n.sims.saved <- if (is.null(sims)) 0 else nrow(na.omit(sims))
    return(
      list(
        n.iterSoFar=next.iteration-1,
        n.sims.saved=n.sims.saved,
        ticks.per.iteration=ticks.per.iteration
      )
    )
  }
  # ================================================================================
  # Set up if sims is null
  # ================================================================================
  if (is.null(sims)) {
    # Start a new chain afresh
    # ifdebug("DEBUGGING: FOR EACH CHAIN: set.seed(344)\n")
    # if (debug) set.seed(344)
    # ================================================================================
    # Parameter initialization and initial parameter updating
    # ================================================================================
    tpl.init.param
    #
    # We do also one update here to ensure that all parameters have a value.
    #
    tpl.init.updater # Assumes that tpl.init.param has been run.
    #
    n.col <<- length(ALL.PARAMS)
    sims <<- matrix(NA, nrow=n.simrows, ncol=n.col)
    next.iteration <<- 1 ## Iterations finished
    next.row <<- 1
    col.names <<- .get.col.names()
  } else if (n.simrows>0) {
    # ================================================================================
    # 'sims' is not NULL : resume sampling.
    # Check where we get the first NA; this is the place that was last filled in.
    # If all rows were filled in, add rows to accommodate possible new n.iter.
    # ================================================================================
    if (is.na(n.col)) stop("n.col is not initialized (cannot happen)!")
    n.rows <- nrow(sims)
    .add.rows <- (n.simrows-n.rows)
    .rearrange.sims()
    if (.add.rows>0) {
      .add.nas <- matrix(NA, nrow=.add.rows, ncol=n.col)
      sims <<- rbind(sims, .add.nas)
      next.row <<- (n.rows+1) # Point to the next NA.
ifdebug("Adding",.add.rows,"rows to the simulation matrix.")
ifdebug("Now n.rows=", nrow(sims))
    } else if (.add.rows<0) {
ifdebug("Removing",-.add.rows,"rows from the simulation matrix.")
      # Delete (earlier) rows that are not wanted any more.
      if (abs(.add.rows)<n.simrows) {
        .delete.rows <- seq(from=-1, to=.add.rows)
        sims <<- sims[ .delete.rows, ] ## -1 down to .del.rows, all negative.
        next.row <<- 1
      } else {
        stop("ERROR: cannot remove all simulations.\n")
      }
    }
    # ================================================================================
    # Check where to start filling up from.
    # ================================================================================
  } else {
    stop("ERROR: n.simrows is negative (make.chainsampler).")
  } ## if/else is.null(sims)
  ##
  .refresh.now <- FALSE
  ##
  if (next.iteration<=n.iterUntil)
  {
    # ================================================================================
    # Start chain: currently only Trace uses this template.
    # ================================================================================
    #
    tpl.start.chain
    #
    # ================================================================================
    # The chain loop
    # ================================================================================
    #
    .ta <- proc.time()
    .ta.iter <- next.iteration
    while (next.iteration<=n.iterUntil) {
      adapting <<- (next.iteration <= n.burnin) # Global variable
      .a <- adaptNow[next.iteration] # will be NA when iter>n.burnin
      adapt.now <<- ((!is.na(.a)) && .a)
      .save.now  <-  (!adapting && (next.iteration %% n.thin == 0)) # Local.
      .refresh.now <- (next.iteration %% n.trace.interval == 0) # Local.
      # ================================================================================
      # Update parameters
      # ================================================================================
      .t0 <- proc.time() 
      tpl.update.param
      .t0.elapsed <- (proc.time()-.t0)[1]
      .Umacs.timer.local$.total.update <<- c(.Umacs.timer.local$.total.update, .t0.elapsed)
      # ================================================================================
      # Save the parameters
      # ================================================================================
      if (.save.now || save.all) {
        if (next.row>n.simrows) next.row <<- 1
        sims[next.row,] <<- ALL.PARAMS
        next.row <<- next.row + 1
      }
      if (next.iteration>last.iteration) last.iteration <<- next.iteration ## LAST one.
      next.iteration <<- next.iteration + 1 ## The NEXT one.
      if (.refresh.now) .refresh()
    }
    if (.ta.iter<next.iteration) {
      ticks.per.iteration <<- (proc.time()-.ta)[1]/(next.iteration-.ta.iter)
    }
    # ================================================================================
    # End-of chain routines
    # ================================================================================
    tpl.end.chain
  }
  # ================================================================================
  # If the last iteration didn't have .refresh.now=T then execute it now.
  # ================================================================================
  ### debug: print(c(chain,next.iteration,.refresh.now,n.trace.interval,222))
  if (!.refresh.now) .refresh()

  # ================================================================================
  # Prepare the timer, if available
  # ================================================================================
  ## Copy the new times into the global .Umacs.timer function
  ## We must be careful since .Umacs.timer$something <<- something will not work!
  ## Must change the variable itself as a whole.
  ##
  for (i in names(.Umacs.timer)) {
    .Umacs.timer.local[[i]] <<- c(.Umacs.timer[[i]], .Umacs.timer.local[[i]])
  }
  .Umacs.timer <<- .Umacs.timer.local
  # ================================================================================
  # Return the matrix 'sims' (do not rearrange, it's already arranged properly)
  # ================================================================================
  dimnames(sims) <- list(NULL, col.names)
  sims
} # end function
} # end template .chainsampler.Sampler


###_NEW_

# ========================================================================
# template .make.sampler.Sampler
# ========================================================================

.make.sampler.Sampler <- function()
{
  Umacs <- NULL        # List of all variables of all chains, compiled together by .setenv

  .Umacs.timer <- list() # This is needed since SamplerCheck will be run too.

  chainsampler <- list() # List of chain sampler functions

  ## Usage: .unspecified(variable) <- value

  ".unspecified<<-" <- function (x, value) {
    attr(value, 'unspecified') <- TRUE
    value
  }
  ".unspecified<-" <- function (x, value) {
    attr(value, 'unspecified') <- TRUE
    value
  }
  .unspecified  <- function (x) {
    ## Either an NA, without the 'unspecified' attribute,
    ##   or with a TRUE 'unspecified' attribute
    is.na(x) || (!is.null(attr(x, 'unspecified')) && attr(x, 'unspecified'))
  }

  ## don't save  ## sim.chains <- list()   # List of simulations obtained from chain samplers

  errors <- FALSE      # TRUE iff there were any errors
  .error <- FALSE      # TRUE iff an error has occurred (set by function error())

  chain <- NA          # The current chain number

  n.chains <- 3        # number of chains to run
  thin   <- TRUE       # TRUE => Do thinning. FALSE => Just save the last values.
  p.burnin <- 0.5     # (default) proportion of burn-in iterations

  last.iteration <- 0  # The latest iteration number available (updated in each chain),
                       # longest running chain updates this continuously.

  n.iter <- NA         # total number of iterations (including burn-in, thinning)
  n.sims <- NA         # how many simulations to actually save
  n.thin <- NA         # interval for thinning
  n.burnin <- NA       # Number of iterations in the burn-in period

  n.simrows <- NA       # Number of sims to save per sim chain

  n.adapt  <- 50       # Interval of adaptation
  n.block  <- 50       # After burn-in, run chains in parallel in blocks of 50

  n.iterSoFar <- NA    # Total iterations that have been done (for each chain)
  n.iterTotal <- NA    # Total iterations to be done (for each chain)

  n.trace.interval <- 50 # Interval of showing Traces.

  all.chains <- NA      # Sequence of chains 1...n.chains

  do.chains <- NA       # which chains to do now, during this run
  do.chain  <- NA       # an alias for do.chains "do.chain=1"

  save.all <- FALSE    # TRUE iff we save all iterations, including those in 'burn-in' phase

  debug   <- FALSE     # TRUE iff we're debugging
  verbose <- TRUE      # FALSE iff suppress message output

  silent <- FALSE      # TRUE iff keep quiet <=> suppress all output, except
                       # 'whispers' when verbose=TRUE.

# =================================================================================
# LOCAL FUNCTIONS
# =================================================================================

  .getenv <- function (f) {
     ## Return list of non-function objects
     e <- as.list(environment(f), all.names=TRUE)
     omit <- sapply(e, is.function)
     e[!omit]
  }

  .setenv <- function () {
    for (j in seq(along=chainsampler)) {
      chain <<- j
      chainsampler[[j]](env=TRUE)
      if (length(Umacs$chain)<chain) Umacs$chain[[chain]] <<- list()
      Umacs$chain[[j]]$env <<- .getenv(chainsampler[[j]])
    }
    Umacs$sampler <<- .getenv(.sampler)
    Umacs$sampler$Umacs <<- NULL
    Umacs$sampler$chainsampler <<- NULL
    invisible()
  }

  # say() : for usual nonsilent messages
  say  <- function (...) if (!silent) cat(...)

  # whisper() : For parameter check (see SamplingScheme templates) when silent is TRUE
  whisper  <- function (...) if (silent && verbose) cat(..., sep="")

  # chat() : only for most verbose messages
  chat <- function (...) if (!silent && verbose) cat(..., sep="")

  # ifdebug() : for debugging only
  ifdebug <- function (...) if (debug) cat("# DEBUG: ", ..., "\n")

  # error() : print an error message and set the .error flag

  error <- function (...) {
    cat("Error:",...,"\n")
    .error <<- TRUE
    return(FALSE)
  }

  .arg.there <- NULL  # Will be a named vector

# =================================================================================
# CHAIN SAMPLER GENERATOR
# =================================================================================

  make.chainsampler <- function () tpl.fun.make.chainsampler

  # For now assume that every iteration is saved.

# =================================================================================
# SUBCLASS-SPECIFIC INITIALIZATION
# =================================================================================
# Now used by e.g. SamplerCheck

  tpl.init.sampler

# =================================================================================
# INITIALIZING THE TRACE
# =================================================================================
# Now used by Trace

  tpl.init.trace

# =================================================================================
# JUST BEFORE RETURNING THE SAMPLER FUNCTION...
# =================================================================================

  setAdaptNow <- function (n.burnin, adaptNow) {
    if (length(adaptNow)>=n.burnin) return(adaptNow) # n.burnin should be at least 1.
    epochs <- c(8,25,50,75)
    ((1:n.burnin %in% epochs) | (1:n.burnin %% n.adapt == 0))
  }

  adaptNow <- NA

  showProgress <- NULL

  .Rhat <- NULL
  .Rhatmax <- NULL

# The local argument variables n.iter., n.sims., etc (all appended with a dot)
# will be saved in the environment of .sampler as n.iter, n.sims, etc (without a dot);
# also all arguments specified in ... are saved in the local environment
# Eventually we want to make all local variables to be prepended with a dot
#
# ABOUT THE ARGUMENTS:
# The arguments that have a dot '.' appended are "local",
# and the same arguments without the dot are "global".

  .global.arg.names <- c("n.iter", "n.sims", "n.burnin", "n.thin", "n.chains")
  .local.arg.names  <- paste(.global.arg.names, ".", sep="")

# =================================================================================
# THE SAMPLER FUNCTION
# =================================================================================

.sampler <- function(n.iter.=200, more=100, n.sims.=100, n.chains., n.burnin., n.thin.,
  setup.only=FALSE, status=FALSE, restart=FALSE, env=FALSE, ...)
{
  # =================================================================================
  # Any special requests: env/setup.only/debug/status/restart?
  # =================================================================================
  #
  if (env) {
    .setenv()
    return(Umacs)
  } else if (setup.only) return(invisible())
  if (status) {
     stop("# DEBUG: what to return when status is requested?")
  } else if (restart) {
    .yn <- readline("Restarting sampler. The simulation matrix will be reset to empty. Ok? ")
    if (substr(.yn,1,1) %in% c('y','Y')) {
      chainsampler <<- NULL
    } else {
      stop("The simulation matrix was not touched.")
    }
  }
  if (debug) {
    ifdebug("Debugging mode is on. Use switch debug=FALSE to turn off.")
  }
  if ("package:rv" %in% search()) {
    .rvc <- rvcompatibility(1)
    on.exit(rvcompatibility(.rvc))
  }
  #
  # =================================================================================
  # Process arguments
  # =================================================================================
  # Check which of the arguments were specified:
  #
  # .global.args is the list of all arguments that are turned into variables 
  # in this environment.
  #
  .arg.there <<- NULL
  .global.args <- list(...)
  for (i in seq(along=.global.arg.names)) {
    .lname <- .local.arg.names[i]  ## with dots.
    .gname <- .global.arg.names[i] ## without dots
    if (.arg.there[[.gname]] <<- (!eval(substitute(missing(x), list(x=as.name(.lname))))))
    {
      ## If an argument was specified, it was set and saved in a variable;
      ## Now save it in the global args list
      .global.args[[.gname]] <- get(.lname)
    }
  }
  # =================================================================================
  # Save the variables that were specified in the argument list (including "...")
  # =================================================================================
  this.envir <- environment(.sampler)
  all.names   <- names(as.list(this.envir))
  .error <<- FALSE
  for (name in names(.global.args)) {
    if (substr(name,1,1)==".") {
      error("Error: cannot set variables with a leading dot")
    } else if (!(name %in% all.names)) {
      error("Error: variable '", name, "' was not found.\n", sep="")
    } else {
      .arg.there[[name]] <<- TRUE
      assign(name, .global.args[[name]], inherits=TRUE, envir=this.envir)
    }
  }
  if (.error) stop("Errors detected, aborting.")
  # =================================================================================
  # Set iterations, burn-in, thinning, etc.
  # =================================================================================
  # NEXT: make n.burnin and n.thin dependent on the number of 
  # iterations made until now. the goal being the n.sims.
  #
  # n.chain, do.chains, do.chain
  # 
  n.chains <<- floor(n.chains)
  all.chains <<- seq(from=1, to=n.chains)
  if (n.chains<1) {
    stop("You must run at least one chain (n.chains>=1)")
  }
  if (.unspecified(do.chains)) {
    if (.unspecified(do.chain)) { ## an alias for do.chain
      .unspecified(do.chains) <<- all.chains
    } else {
      do.chains <<- unique(do.chain)
    }
  } else {
    if (!.unspecified(do.chain)) {
      do.chains <<- do.chain
    } else {
      do.chains <<- unique(do.chains)
    }
  }
  do.chain <<- NA
  #
  if (length(do.chains)==0 || any(do.chains<1) || any(do.chains>n.chains)) {
    stop("All numbers in vector 'do.chains' must be >=1 and <=",n.chains,"(was:",paste(do.chains,collapse=","),")")
  }
  #
  if (p.burnin<0 || p.burnin>1) {
    stop("p.burning must be between 0 and 1")
  }
  #
  if (.unspecified(n.iter)) {
    if (.unspecified(n.sims)) {
      .unspecified(n.iter) <<- 10
    } else {
      .unspecified(n.iter) <<- ceiling((n.sims/n.chains)/p.burnin)
    }
  } else if (n.iter<1) {
    n.iter <<- NA
    stop("You must do at least 1 iteration.")
  } else {
    n.iter <<- floor(n.iter)
  }
  if (!is.na(last.iteration)) {
    if (last.iteration>n.iter) {
      .unspecified(n.iter) <<- last.iteration
    }  
  }
  #
  if (!missing(more)) {
    n.iter <<- n.iter + more
  }
  #
  if (.unspecified(n.burnin)) {
    .unspecified(n.burnin) <<- floor(n.iter*p.burnin)
  }
  #
  if (.unspecified(n.sims)) {
    .unspecified(n.sims) <<- n.chains*(n.iter - n.burnin)
  }
  #
  n.simrows <<- max(1, ceiling(n.sims/n.chains))
  ##
  if (save.all) {
    thin <<- FALSE
  }
  if (thin) {
    if (.unspecified(n.thin)) {
      .unspecified(n.thin) <<- max(1, floor(n.chains*(n.iter-n.burnin)/n.sims))
    } else if (is.infinite(n.thin)) {
      n.thin <<- n.iter+1
    }
  } else {
    n.thin <<- 1
  }

  adaptNow <<- setAdaptNow(n.burnin, adaptNow)

  .n.get.sims <- n.chains*((n.iter-n.burnin) %/% n.thin)
  if (.n.get.sims<1) {
    cat("Warning: due to extreme thinning, none of the new simulations will be saved.\n")
  }

  # =================================================================================
  # Prepare to iterate
  # =================================================================================
  # Find out how many iterations we have done so far
  # 
  .n.sims.saved <<- rep(0, length.out=n.chains)
  n.iterSoFar <<- rep(0, length.out=n.chains)
  n.iterTotal <<- rep(n.iter, length.out=n.chains)
  for (j in all.chains) {
    if (is.null(chainsampler[j][[1]])) {
      .fun <- make.chainsampler()
      chainsampler[j] <<- list(.fun)
      n.iterSoFar[j] <- 0
    } else {
      .chain.status <- chainsampler[[j]](status=TRUE)
      .n.sims.saved[j] <- .chain.status$n.sims.saved
      n.iterSoFar[j] <- .chain.status$n.iterSoFar
    }
  }
  #
  # =================================================================================
  # Prepare the progress indicator bar
  # =================================================================================
  showProgress <<- progressBar(n=n.iterTotal, resume.from=n.iterSoFar, silent=silent, cat=say)
  # =================================================================================
  # Talk to the user
  # =================================================================================
  say("Umacs sampler: ")
  say("\"", TITLE,"\"\n", sep="")
  say("n.chains=", n.chains)
  say("; n.iter/chain=", n.iter)
  say("; n.burnin/chain=", n.burnin)
  say("; thin=", thin, sep="")
  if (thin) say(" (n.thin=", n.thin,")", sep="")
  say("\n")
  say("n.sims=", n.sims)
  say("; sims/chain=", n.simrows)
  say("\n")
  if (sum(n.iterSoFar)==0) { 
    say("No iterations yet done.\n")
  } else {
    say("So far done  (",paste(n.iterSoFar, collapse=','),") iterations for the ",n.chains," chains.\n", sep="")
  }
  if (sum(.n.sims.saved)==0) {
    say("No simulations yet saved.\n")
  } else {
    say("So far saved (",paste(.n.sims.saved, collapse=','),") iterations for the ",n.chains," chains.\n", sep="")
  }
  if ((!is.na(adaptNow)) && any(adaptNow)) {
    .wan <- which(adaptNow)
    if (length(.wan)<1) {
      say("Adaptation period 1 ...",n.burnin,"is completed for n.iter=",n.iter,"\n")
    } else {
     .wan <- .wan[.wan>last.iteration]
      say("Adapting between iterations #", .wan[1], "...", .wan[length(.wan)] , "\n")
    }
  } else {
    say("No adaptation is needed.\n")
  }
  if (length(do.chains)>1) {
    say("Running chains ", paste(do.chains, collapse=', '), ".", sep="")
  } else {
    say("Running chain", do.chains, ".")
  }
  say("\n")
  # =================================================================================
  # Run the sampler
  # =================================================================================
  .do.iter <- seq(from=0, to=n.iter-1, by=n.block)
  .do.iter <- .do.iter[-1]
  if (!(n.iter %in% .do.iter)) .do.iter <- c(.do.iter, n.iter)
  .announced <- FALSE ## For timing purposes.
  showProgress(n.iterSoFar)
  .chr <- "."
  for (n.iterUntil in .do.iter) {
    if (all(n.iterSoFar>n.iterUntil)) next # Could fix .n.it instead.
    for (j in do.chains) {
      chain <<- j
      chainsampler[[chain]](n.iterUntil, n.simrows, debug=debug)
      n.iterSoFar[j] <- n.iterUntil
#.chr <- "." ## ??? DEMO ONLY
      showProgress(n.iterSoFar, char=.chr)
    }
    chain <<- do.chains[1]
if (FALSE) { ## DEMO ONLY
    tpi <- chainsampler[[chain]](status=TRUE)$ticks.per.iteration
    iterations.remaining <- length(do.chains)*(n.iter-n.iterUntil)
    time.remaining <- 100*iterations.remaining*tpi
    if (!is.na(time.remaining) && time.remaining>20 && !.announced) {
      cat("\nTime remaining: ", time.remaining, " seconds.\n")
      showProgress(0, char=.chr)
      announced <- TRUE
    }
} ## DEMO ONLY
    if (n.iterUntil>n.burnin && n.iter>2 && n.iterUntil<n.iter) {
      local({
        X <- list()
        for (j in all.chains) {
          X[[j]] <- na.omit(chainsampler[[j]](sims=TRUE))
        }
        n <- nrow(X[[1]])
        colmeans <- sapply(X, colMeans)
        if (length(na.omit(colmeans))>=2) {
          Bn <- if (is.null(dim(colmeans))) var(colmeans) else apply(colmeans, 1, var)
          vars <- sapply(X, function (x) apply(x, 2, var))
          W <- if (is.null(dim(vars))) mean(vars) else rowMeans(vars)
          Rhats2 <- n/(n-1) + Bn/W
          .Rhat <<-  sqrt(Rhats2)
          names(.Rhat) <- names(W)
          .Rhatmax <<- max(.Rhat)
          r <- 100*(.Rhatmax-1)
          if (is.na(r)) {
            error(".Rhatmax=",.Rhatmax,"; .Rhat=")
            print(.Rhat)
          } else if (r<10 && r>0) .chr <<- substr(paste(r),1,1)
        }
      })
    }
  }
  sim.chains <- list()
  for (j in all.chains) {
    sim.chains[[j]] <- na.omit(chainsampler[[j]](sims=TRUE))
  }
  say(" done.\n")
  #
  tpl.end.sampler
  #
  return(mcts(chains=sim.chains,
    save.all=save.all,
    n.burnin=ifelse(save.all,n.burnin,0),
    n.iter=n.iter,
    n.sims=n.sims
  ))
  ""
  TITLE # For convenience, this shows when the sampler function is entered in the console.
  DATE
} # end function

} # end template .sampler.Sampler


# End Sampler.R

# ========================================================================
# class SamplerCheck : run the sampler for one iteration, doing a integrity check
# ========================================================================

setClass('SamplerCheck', 
  representation(),
  contains='Sampler'
)

# NAME:
#  SamplerCheck-class - Class definition of "SamplerCheck" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  This class is a derivative of the [[Sampler-class]], used for
#  doing a test run on the sampler before passing the sampling function to the user.
#
# SLOTS:
#   Please refer to [[Sampler-class]].



# ========================================================================
# SamplerCheck  -  produce a SamplerCheck instance
# ========================================================================
#

SamplerCheck <- function(params)
{
  obj <- .Sampler(params, class='SamplerCheck', check.only=TRUE)
  ## DEBUG: sc <<- is for debugging only
  ## sc <<- s <- run(obj)
.sc <<-  s <- run(obj) ## DEBUG: .sc
  s(n.chains=1, n.iter=1, n.sims=1, n.thin=1, n.burnin=0, debug=TRUE)
}

# ========================================================================
# templates.SamplerCheck
# ========================================================================

setMethod('templates', signature('SamplerCheck'),
function(obj)
{
  tpls <- callNextMethod()
  #
  # Make templates to verify init and update functions.
  # Use actual sampler/chainsampler templates but with different
  # tpl.init and tpl.update.param functions (from SamplingScheme, currently)
  #
  s <- list(tpl.init.param=as.language('tpl.init.param.check'),
            tpl.update.param=as.language('tpl.update.param.check'))
  ms  <- tpls$make.sampler
  mcs <- tpls$tpl.fun.make.chainsampler
  substit(ms) <- s
  substit(mcs) <- s
  tpls$make.sampler <- ms
  tpls$tpl.fun.make.chainsampler <- mcs
  tpls$tpl.init.sampler <- .tpl.init.sampler.SamplerCheck
  tpls$tpl.end.sampler <- .tpl.end.sampler.SamplerCheck
  tpls
})

# ========================================================================
# .tpl.init.sampler.SamplerCheck
# ========================================================================

.tpl.init.sampler.SamplerCheck <- function () 
{
  .check.param.value <- function (what, x)
  {
    x.name <- as.character(substitute(x))
    if (is.null(x)) 
      error(what, "of",x.name,"returned NULL")
    else if (any(is.nan(x)))
      error(what, "of",x.name,"returned an NaN")
    else if (any(is.na(x)))
      error(what, "of",x.name,"returned an NA")
    else if (!all(is.numeric(x)))
      error(what, "of",x.name,"returned a value of type",typeof(x))
    else if (length(x)==0)
      error(what,"of", x.name, " has zero length.")
    else return(TRUE)
    print(x)
    return(FALSE)
  }
  silent <- TRUE
}

# ========================================================================
# .tpl.end.sampler.SamplerCheck
# ========================================================================

.tpl.end.sampler.SamplerCheck <- function () 
{
  return(!errors)
}

# End SamplerCheck.R

# ========================================================================
# class Trace  -  trace a scalar parameter or a variable graphically
# ========================================================================

setClass('Trace', 
  representation(),
  contains='RawCode'
)


# NAME:
#  Trace-class - Class definition of "Trace" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[Trace]] inside a [[Sampler]]
#  function call,
#  which outputs a [Trace] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to [[RawCode-class]].
# 


# ========================================================================
# Trace  -  create a Trace code SamplingScheme
# ========================================================================
#

Trace <- function(name, label=name, ylim=NULL)
{
  if (is.function(name)) {
    if (!is.character(label)) stop("No label specified for a trace")
  }
  snips <- snippetlist(
    TRACEPARAM  = as.language(name),
    QTRACEPARAM = name,
    TRACELABEL = label
  )
  new('Trace', snippets=snips)
}

# NAME:
#  Trace - Generate a Trace object for Umacs Sampler function
# 
# DESCRIPTION:
#  Generates an object including the information necessary 
#  to show a trace plot of a scalar parameter in a Umacs sampling function. 
#
#  The sampler is built using the Umacs function [[Sampler]]; 
#  this function is only to be used within the [[Sampler]] function call.
#
# ARGUMENTS:
#  ~ name  : Name of the scalar parameter to be traced, e.g. "\code{theta[1]}". 
#  ~ label : Label to be shown in the trace plot window.
#            By default the same as [name].
#  ~ ylim  : Range of the vertical axis.
#            By default, the range is extended as necessary 
#            to display the highs and the lows of the trace.
#
# DETAILS:
#  If [ylim] is not specified (if it is [NULL]), the range of the vertical
#  axis is adjusted automatically. 
#
#  [Trace] is to be used only within the [[Sampler]] function call.
#  
# VALUE:
#  An object of class [Trace],
#  to be further processed by the Umacs function [Sampler].
#



# ========================================================================
# constantParam.Trace
## DEBUG: kludge to prevent Trace "parameters" from being collected.
## DEBUG: how about 'notSampled'? or 'notSaved'?
# =======================================================================

setMethod('constantParam', signature('Trace'),
function (obj)
{
  TRUE
})

# ========================================================================
# templates.Trace - 
# ========================================================================

setMethod('templates', signature('Trace'),
function(obj)
{
  parent.templates <- callNextMethod()
  Trace.templates <- snippetlist(
    tpl.init.sampler = NULL,
    tpl.init.trace   = .tpl.init.trace.Trace,
    tpl.init.initfun = .tpl.init.initfun.Trace,
    tpl.init.param   = .tpl.init.param.Trace,
    tpl.init.updater = .tpl.init.updater.Trace,
    tpl.init.chain   = .tpl.init.chain.Trace,
    tpl.start.chain  = .tpl.start.chain.Trace,
    tpl.update.param = .tpl.update.param.Trace,
    tpl.update.trace = .tpl.update.trace.Trace,
    tpl.get.names    = .tpl.get.names.Trace
  )
  c(parent.templates, Trace.templates)
})

# ========================================================================
# template .tpl.init.trace.Trace - 
# ========================================================================
# The Trace UPDATEFUN and INITFUN are defined in the sampler closure.
# 'INITFUN' is simply a logical variable.
# 'UPDATEFUN' is generated by the tracePlotter function.
#

.tpl.init.trace.Trace <- function ()
{
  UPDATEFUN <- .tracePlotter(label=QTRACEPARAM, n.iter=n.iter, silent=silent)
  INITFUN <- TRUE
}

# ========================================================================
# template .tpl.init.initfun.Trace - 
# ========================================================================
# Since Trace inherits from RawCode, which has a non-NULL tpl.init.initfun,
# while the Trace INITFUN is defined in the sampler closure,
# we will have to make this template a NULL one

.tpl.init.initfun.Trace <- function () NULL

# ========================================================================
# template .tpl.init.param.Trace - 
# ========================================================================

.tpl.init.param.Trace <- function () NULL

# ========================================================================
# template .tpl.start.chain.Trace - 
# ========================================================================

.tpl.start.chain.Trace <- function ()
{
  # UPDATEFUN returns true or false: FALSE means "stop showing the trace"
  if (INITFUN) INITFUN <<- UPDATEFUN(n.iter=n.iter)
}

# ========================================================================
# template .tpl.init.updater.Trace - 
# ========================================================================

.tpl.init.updater.Trace <- function () NULL

# ========================================================================
# template .tpl.init.chain.Trace - 
# ========================================================================

.tpl.init.chain.Trace <- function () NULL

# ========================================================================
# template .tpl.update.trace.Trace - 
# ========================================================================

.tpl.update.trace.Trace <- function ()
{
  ## ???
}

# ========================================================================
# template .tpl.update.param.Trace - 
# ========================================================================

.tpl.update.param.Trace <- function ()
{
  if (INITFUN) INITFUN <<- UPDATEFUN(TRACEPARAM, chain)
}

# ========================================================================
# template .tpl.get.names.Trace - 
# ========================================================================

.tpl.get.names.Trace <- function () NULL


# End Trace.R
# ========================================================================
# class Metropolis -  Metropolis SamplingScheme
# ========================================================================

setClass('Metropolis',
  representation(),
  contains='SamplingScheme'
)

# NAME:
#  Metropolis-class - Class definition of "Metropolis" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[Metropolis]] inside a [[Sampler]]
#  function call,
#  which outputs a [Metropolis] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [SamplingScheme] ([[SamplingScheme-class]])
# 


# ========================================================================
# Metropolis  -  create new Metropolis SamplingScheme
# ========================================================================

## DEBUG: make it so that we must refer to proposals, adaptation routines
## by a token and not by actual name.

Metropolis <- function(
   logpost=NULL,
   init,
   adapt.scale.function='scaleAdapt.Metropolis',
   scale.jump=NA,
   kernel.jump="Gaussian",
   class.='Metropolis'  # Internal use only
)
{
  if (is.null(proposal <- .kernels.Metropolis[[kernel.jump]])) {
    stop("Unknown kernel.jump: ", kernel.jump)
  }
  if (!is.character(adapt.scale.function)) {
    stop("adapt.scale.function must be a name of a function (of type character)")
  }
  snips <- snippetlist(
    tpl.proposal       = proposal,
    code.logpost       = logpost,
    scaleAdapt         = as.language(adapt.scale.function),
    exists.code.logpost = !is.null(logpost),
    CODEINIT          = init,
    code.Sigma.scale  = scale.jump,
    code.Sigma.scale.specified  = !is.na(scale.jump)
  )
  new(class., snippets=snips)
}


# NAME:
#  Metropolis - Generate a Metropolis object for the Umacs Sampler function
# 
# 
# DESCRIPTION:
#  Generates an object including the information necessary 
#  to generate a Metropolis updating step for a parameter,
#  within the main loop
#  of an iterative sampler function.
#
#  The sampler is built using the Umacs function [[Sampler]];
#  and the arguments ([update] and [init])
#  are embedded in the sampler function. 
#
# ARGUMENTS:
#  ~ logpost : An R function calculating the value (a scalar) of the unnormalized log-posterior function given all other arguments
#  ~ init    : An R function that returns a (random) starting point (scalar, vector, or array) for a Markov chain for the parameter
#  ~ adapt.scale.function : name of the function used to adapt the proposal kernel (usually, just ignore and use the default)
#  ~ scale.jump : The scale of the proposal kernel matrix, if adaptation is not to be used
#  ~ kernel.jump : Proposal-generating density class. Now, the only possible value is `Gaussian'.
#  ~ class. : (For internal use only)
#
# DETAILS:
#  [Metropolis] is to be used only within the [Sampler] function call.
#
#  The arguments of the update and init functions are ignored.
#  The body of the function can contain references to any parameters
#  that are defined in the [[Sampler]] function call, or to any 
#   variables in an enclosing environment (such as the Global Environment).
#
#  The name of the parameter that is being updated is __not__ specified here,
#   but only within the [[Sampler]] function call.
#
# NOTE:
#  If the associated parameter is __scalar-valued__, use [[SMetropolis]] instead.
#  
# VALUE:
#  An object of class \code{Metropolis},
#  to be further processed by the Umacs function [Sampler].
#

# ========================================================================
# templates.Metropolis - 
# ========================================================================

setMethod('templates', signature('Metropolis'),
function(obj)
{
  old.templates <- callNextMethod()
  Metropolis.templates <- snippetlist(
    tpl.proposal     = obj@snippets$tpl.proposal,
    tpl.init.updater = .tpl.init.updater.Metropolis,
    tpl.init.chain   = .tpl.init.chain.Metropolis,
    tpl.update.param = .tpl.update.param.Metropolis,
    tpl.reject       = .tpl.reject.Metropolis,
    tpl.init.Sigma   = .tpl.init.Sigma.Metropolis,
    tpl.adapt        = .tpl.adapt.Metropolis,
    tpl.adaptation   = .tpl.adaptation.Metropolis
  )
  c(old.templates, Metropolis.templates)
})


# ========================================================================
# template .tpl.init.updater.Metropolis - 
# ========================================================================

.tpl.init.updater.Metropolis <- function ()
{ 
   UPDATEFUN <<- MAKEUPDATEFUN()
   ##debug(UPDATEFUN)
}

# ========================================================================
# template .tpl.init.chain.Metropolis - 
# ========================================================================

.tpl.init.chain.Metropolis <- function ()
{
  ## This template creates a function that returns 
  ## an updating function.
  ##
  ## Same for PMetropolis and other Metropolis derivatives
  ##
  MAKEUPDATEFUN  <- function () {
    ## Log posterior: if there is a function specifically
    ## made for this parameter, use that instead.
    if (exists.code.logpost) .logpost <- function () code.logpost
    ## proposal-generating function
    proposal <- function () tpl.proposal
    ## adaptation function
    adapt <- function () tpl.adapt
    ## Variables
    logpost.old <- .logpost() # Used only with tpl.altReject.Metropolis
    ##
    len  <- length(PARAM)
    ## "jumps" : A matrix of all updates made.
    jumps <- NULL
    ## "Jumps" : a list of jumps
    Jumps <- list()
    adapt.from <- 1 # To compute
    p.jumps <- NULL
    n.jumps <- 0
    reject <- NA
    avg.p.jump <- NA
    old.avg.p.jumps <- NULL
    old.scales <- NULL
    sum.p.jumps <- 0
    ##---
    ## Initialize the covariance matrix:
    tpl.init.Sigma
    ##---
    function () {
      tpl.reject
      if (adapting) {
        n.jumps <<- n.jumps + 1
        sum.p.jumps <<- sum.p.jumps + this.jump.prob
        p.jumps <<- cbind(p.jumps, this.jump.prob) # May be unnecessary
        avg.p.jump <<- sum.p.jumps/n.jumps
        old.avg.p.jumps <<- cbind(old.avg.p.jumps, avg.p.jump)
        old.scales <<- cbind(old.scales, Sigma.scale)
        tpl.adaptation
        if (adapt.now) {
          p.jumps <<- jumps <<- NULL
          n.jumps <<- 0
        }
      }
    }
  }
}

# ========================================================================
# template .tpl.init.Sigma.Metropolis - 
# ========================================================================

.tpl.init.Sigma.Metropolis <- function ()
{
  ## This code will be initialized within the
  ## function MAKEUPDATEFUN (see .tpl.init.chain.Metropolis)
  Sigma.scale  <- if (code.Sigma.scale.specified) code.Sigma.scale else 1
  Sigma.shape <- diag(1, len)
  Sigma.jump <- (Sigma.scale^2)*Sigma.shape
  Sigma.jump.eigen  <- .compute.eigen(Sigma.jump)
}


# ========================================================================
# template .tpl.update.param.Metropolis - 
# ========================================================================

.tpl.update.param.Metropolis <- function () UPDATEFUN()

# ========================================================================
# template .tpl.proposal.Metropolis
# ========================================================================

.tpl.proposal.Gaussian.Metropolis <- function () mvrnorm2(PARAM, Sigma.jump.eigen)

# ========================================================================
# .tpl.reject.Metropolis
# ========================================================================

.tpl.reject.Metropolis <- function ()
{
  logpost.current <- .logpost()
  old.value <- PARAM
  PARAM <<- proposal()
  logpost.new <- .logpost()
  lhratio <- (logpost.new - logpost.current)
  if (is.nan(lhratio)) {
    if (logpost.new==-Inf && logpost.current==-Inf) {
      error("Metropolis parameter",QPARAM,"current value",old.value,"was rejected")
      cat("Bad starting value?\n")
    } else {
      error("Metropolis parameter",QPARAM,"has NaN in lh-ratio:")
      print(lhratio)
      cat("Log-post new and log-post current follow:\n")
      print(c(logpost.new, logpost.current))
      cat("Temporarily fixed by setting NaN to -Inf\n")
    }
    lhratio <- -Inf
  }
  if (lhratio>=0) {
    this.jump.prob <- 1
    reject <- FALSE
  } else if (is.infinite(lhratio)) { # -Inf
    this.jump.prob <- 0
    reject <- TRUE
  } else {
    this.jump.prob <- exp(lhratio)
    reject <- ( runif(1) > this.jump.prob )
  }
##ifdebug("LHRATIO=", lhratio,"\n:::::::::::")
  if (reject) PARAM <<- old.value
}

.tpl.altReject.Metropolis <- function ()
{
  ## DEBUG: NOT YET USED
  # This version assumes the Metropolis-sampled variable is independent of
  # other sampled variables (also e.g. when there are no other variables...)
  # so that previous log post value can be saved and don't need to be recomputed
  # every time.
  old.value <- PARAM
  PARAM <<- proposal()
  logpost.new <- .logpost()
  lhratio <- (logpost.new - logpost.old)
  if (lhratio>=0) {
    this.jump.prob <- 1
    reject <- FALSE
  } else if (is.infinite(lhratio)) { # -Inf
    this.jump.prob <- 0
    reject <- TRUE
  } else {
    this.jump.prob <- exp(lhratio)
    reject <- ( runif(1) > this.jump.prob )
  }
## ifdebug("LHRATIO=", lhratio,"\n:::::::::::")
  if (reject) {
    PARAM <<- old.value
  } else {
    logpost.old <<- logpost.new
  }
}

# ========================================================================
# template .tpl.adaptation.Metropolis - 
# ========================================================================

.tpl.adaptation.Metropolis <- function ()
{
  # This parts varies by the Metropolis routine, since there
  # are sometimes several jumps made at once.
  #
  jumps <<- rbind(jumps, PARAM)
  if (adapt.now) {
    adapt()
  }
}

# ========================================================================
# .tpl.adapt.Metropolis  - default metropolis adaptation for vectors
# ========================================================================

.tpl.adapt.Metropolis <- function () # NEW
{
  if (!code.Sigma.scale.specified) {
    Sigma.scale <<- scaleAdapt(PARAM, avg.p.jump, Sigma.scale, old.avg.p.jumps, old.scales)
  }
ifdebug(QPARAM, "ADAPT: avg.p.jump=",avg.p.jump, "->", "new scale ", Sigma.scale,"\n")
ifdebug(QPARAM, "PJUMP: old.avg.p.jumps",old.avg.p.jumps,"\n")
  ## Now adapt the covariance matrix.
  Sigma.emp <- cov(jumps) # each row contains a realized jump
  d <- det(Sigma.emp)
  if (.about.zero(d)) {
ifdebug("DEBUG: Empirical sigma"); if (debug) print(Sigma.emp)
    Sigma.emp <- Sigma.emp + diag(length(diag(Sigma.emp)))*1e-6
    d <- det(Sigma.emp)
  }
  n.adapted <- nrow(old.avg.p.jumps)
  Sigma.emp <- Sigma.emp*exp(-log(d)/nrow(Sigma.emp)) # div by 1/n-th root (n=n. of rows)
  Sigma.shape <<- (n.adapted*Sigma.shape + 1*Sigma.emp)/(n.adapted+1)
  d <- det(Sigma.shape)
  if (.about.zero(d)) {
     stop("DEBUG: Fatal error: Shape matrix has zero determinant.")
  }
  Sigma.shape <<- Sigma.shape*exp(-log(d)/nrow(Sigma.shape)) # 1/n-th root (n=n. of rows)
  Sigma.jump <<- (Sigma.scale^2)*Sigma.shape
  Sigma.jump.eigen <<- .compute.eigen(Sigma.jump)
ifdebug("  Sigma.scale=", Sigma.scale, "; Sigma.shape follows:\n")
if (debug) print(Sigma.shape)
}


# ========================================================================
# scaleAdapt.Metropolis  - default metropolis adaptation for vectors
# ========================================================================


# Sigma.scale <<- scaleAdapt(PARAM, avg.p.jump, Sigma.scale, old.avg.p.jumps, old.scales)

scaleAdapt.Metropolis <- function(x, avg.p.jump, this.scale, old.p.jumps, old.scales, max.multiply=100)
{
  # ARGUMENTS
  #  x : acceptance proababilities
  #  avg.p.jump : average acceptance probability of the latest batch
  #  this.scale : 
  # Find an estimate of the optimal jumping scale
  # given the current scale and avg.p.jump
  # Use the following crude relation:
  # p = exp(-b*scale)
  # from where b can be estimated crudely by b = -log(p)/scale
  # if we are given a pair (p,scale).
  # If p.opt is the optimal scale, scale.opt = -log(p.opt)/b.
  # After we get (p,scale), estimate b and compute
  # scale.opt = -log(p.opt)/b = (log(p.opt)/log(p))*scale.
  #
  opt.ps <- c(0.441, 0.352, 0.316, 0.279, 0.275, 0.266, 0.261, 0.255, 0.261, 0.267, 0.234)
  log.opt.p <- log(opt.ps[min(length(x),length(opt.ps))])
  #
  if (avg.p.jump==0) { # Jumping too far, so reduce scale
    new.scale <- this.scale/max.multiply
  } else if (avg.p.jump==1) { # Jumping too close, so expand scale
    new.scale <- this.scale*max.multiply
  } else {
    #
    # compute coefficient b in relation "p=exp(-b*sqrt(scale))" using
    # linear regression -log(p) ~ sqrt(scale) without intercept.
    #
    new.scale <- this.scale*(log.opt.p/log(avg.p.jump))
    # Assign different weights since earlier draws are not very reliable???
  }
  new.scale
}


# ========================================================================
# kernel names
# ========================================================================

.kernels.Metropolis <- list(
  'Gaussian'=.tpl.proposal.Gaussian.Metropolis,
  'Normal'=.tpl.proposal.Gaussian.Metropolis,
  't'=NULL,
  'Student'=NULL # N/A yet
)



# end Metropolis.R


# ========================================================================
# class SMetropolis -  Scalar Metropolis SamplingScheme
# ========================================================================

setClass('SMetropolis',
  representation(),
  contains='Metropolis'
)

# NAME:
#  SMetropolis-class - Class definition of "SMetropolis" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[SMetropolis]] inside a [[Sampler]]
#  function call,
#  which outputs a [SMetropolis] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [Metropolis] ([[Metropolis-class]])
# 


# ========================================================================
# SMetropolis  -  create new SMetropolis SamplingScheme
# ========================================================================

SMetropolis <- function(
   logpost=NULL,
   init,
   adapt.scale.function='scaleAdapt.Metropolis',
   scale.jump=NA,
   class.='SMetropolis' # Internal use only
)
{
  kernel.jump <- "Gaussian" ## will eventually be an argument.
  if (is.null(proposal <- .kernels.SMetropolis[[kernel.jump]])) {
    stop("Unknown kernel.jump: ", kernel.jump)
  }
  if (!is.character(adapt.scale.function)) {
    stop("adapt.scale.function must be a name of a function (of type character)")
  }
  snips <- snippetlist(
    tpl.proposal       = proposal,
    code.logpost       = logpost,
    scaleAdapt         = as.language(adapt.scale.function),
    exists.code.logpost = !is.null(logpost),
    CODEINIT          = init,
    code.Sigma.scale  = scale.jump,
    code.Sigma.scale.specified  = !is.na(scale.jump)
  )
  new(class., snippets=snips)
}

# NAME: 
#  SMetropolis - Generate an SMetropolis object for the Umacs Sampler function
#  
# DESCRIPTION:
#  Generates an object including the information necessary 
#  to generate an SMetropolis updating step for a parameter,
#  within the main loop
#  of an iterative sampler function.
#
#  The sampler is built using the Umacs function [[Sampler]];
#  and the arguments ([update] and [init])
#  are embedded in the sampler function. 
#
#  See [[Sampler]] for further details.
#
# ARGUMENTS:
#  ~ logpost : An R function calculating the value (a scalar) of the unnormalized log-posterior function given all other arguments
#  ~ init : An R function that returns a (random) starting point (a scalar!) for a Markov chain for the parameter
#  ~ adapt.scale.function : name of the function used to adapt the proposal kernel (usually, just ignore and use the default)
#  ~ scale.jump : The scale of the proposal kernel matrix, if adaptation is not to be used
#  ~ class. : (for internal use only)
#
# DETAILS:
#  [SMetropolis] is a special case of the [Metropolis] sampling scheme;
#  it is optimized for scalar parameters.
#
#  [SMetropolis] is to be used only within the [Sampler] function call.
#
#  The arguments of the update and init functions are ignored.
#  The body of the function can contain references to any parameters
#  that are defined in the [Sampler] function call, or to any 
#  variables in an enclosing environment (such as the Global Environment).
#
#  The name of the parameter that is being updated is __not__ specified here,
#   but only within the [Sampler] function call.
#
# VALUE:
#  An object of class [SMetropolis],
#  to be further processed by the Umacs function [[Sampler]].
#



# ========================================================================
# templates.SMetropolis - 
# ========================================================================

setMethod('templates', signature('SMetropolis'),
function(obj)
{
  old.templates <- callNextMethod()
  SMetropolis.templates <- snippetlist(
    tpl.proposal     = obj@snippets$tpl.proposal,
    tpl.init.Sigma   = .tpl.init.Sigma.SMetropolis,
    tpl.adapt        = .tpl.adapt.SMetropolis,
    tpl.adaptation   = .tpl.adaptation.SMetropolis
  )
  c(old.templates, SMetropolis.templates)
})


# ========================================================================
# template .tpl.init.Sigma.SMetropolis - 
# ========================================================================

.tpl.init.Sigma.SMetropolis <- function ()
{
  Sigma.scale  <- if (code.Sigma.scale.specified) code.Sigma.scale else 1
}


# ========================================================================
# template .tpl.adaptation.SMetropolis - 
# ========================================================================

.tpl.adaptation.SMetropolis <- function ()
{
  if (adapt.now) {
    adapt()
  }
}


# ========================================================================
# adapt.SMetropolis  - 
# ========================================================================

.tpl.adapt.SMetropolis <- function ()
{
  if (!code.Sigma.scale.specified) {
    Sigma.scale <<- scaleAdapt(PARAM, avg.p.jump, Sigma.scale, old.avg.p.jumps, old.scales)
  }
}


# ========================================================================
# .tpl.proposal.Gaussian.SMetropolis
# ========================================================================

.tpl.proposal.Gaussian.SMetropolis <- function () rnorm(1, PARAM, Sigma.scale)

# ========================================================================
# kernel names
# ========================================================================


.kernels.SMetropolis <- list(
  'Gaussian'=.tpl.proposal.Gaussian.SMetropolis,
  'Normal'=.tpl.proposal.Gaussian.SMetropolis,
  't'=NULL,
  'Student'=NULL # N/A yet
)




# end SMetropolis

# ========================================================================
# class PSMetropolis -  Parallel-Scalar Metropolis SamplingScheme
# ========================================================================

setClass('PSMetropolis',
  representation(),
  contains='Metropolis'
)

# NAME:
#  PSMetropolis-class - Class definition of "PSMetropolis" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[PSMetropolis]] inside a [[Sampler]]
#  function call,
#  which outputs a [PSMetropolis] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [Metropolis] ([[Metropolis-class]])
# 


# ========================================================================
# PSMetropolis  -  create new PSMetropolis SamplingScheme
# ========================================================================

## DEBUG: make it so that we must refer to proposals, adaptation routines
## by a token and not by actual name.

PSMetropolis <- function(
   logpost=NULL,
   init,
   proposal=NULL,
   adapt.scale.function='scaleAdapt.PMetropolis',
   scale.jump=NA,
   class.='PSMetropolis'
)
{
  if (is.null(proposal)) {
    proposal <- .tpl.proposal.PSMetropolis
  }
  snips <- snippetlist(
    code.logpost      = logpost,
    tpl.proposal      = proposal,
    scaleAdapt        = as.language(adapt.scale.function),
    exists.code.logpost = !is.null(logpost),
    CODEINIT   = init,
    code.Sigma.scale  = scale.jump,
    code.Sigma.scale.specified  = !is.na(scale.jump)
  )
  new(class., snippets=snips)
}


# NAME:
#  PSMetropolis - Generate an PSMetropolis object for the Umacs Sampler function
# 
# DESCRIPTION:
#  Generates an object including the information necessary 
#  to generate a Metropolis updating step for a parameter,
#  within the main loop
#  of an iterative sampler function.
#
#  The sampler is built using the Umacs function \code{Sampler};
#  and the arguments (\code{update} and \code{init})
#  are embedded in the sampler function. 
#
#  See \code{Sampler} for further details.
#
# ARGUMENTS:
#  ~ logpost : An R function calculating the value (a vector!) of the unnormalized log-posterior function given all other arguments
#  ~ init : An R function that returns a (random) starting point (a vector) for a Markov chain for the parameter
#  ~ proposal : (not used now)
#  ~ adapt.scale.function : name of the function used to adapt the proposal kernel (usually, just ignore and use the default)
#  ~ scale.jump : The scale of the proposal kernel matrix, if adaptation is not to be used
#  ~ class. : (for internal use only; ignore)
# 
# DETAILS:
#   [PSMetropolis] is a special case of the [PMetropolis] sampling scheme.
#  The parameter to be updated is a __vector__ consisting of 
#  __independently updateable scalars__.
#  That is, each component of the vector can be updated independently (``in parallel'')
#  using a (scalar) Metropolis updating step.
#
#  In other words, you would use [PSMetropolis]
#  if conditional on other parameters in the model, 
#  you have independent (exchangeable) vector-valued parameters
#  that you wish to update using the Metropolis algorithm.
#
#  If instead you have conditionally independent __vector-valued__ parameters,
#  that is, a __matrix__ of independently updateable rows or columns,
#  use \code{PMetropolis}.
#
#  An alternative to using [PSMetropolis] is to define 
#  each component as a separate parameter, but this would be very cumbersome and 
#  require lots of repetitive coding, and the code would not be optimized.
#
#  [PSMetropolis] is to be used only within the [[Sampler]] function call.
#
#  The arguments of the [update] and [init] functions are ignored.
#  The body of the function can contain references to any parameters
#  that are defined in the \code{Sampler} function call, or to any 
#  variables in an enclosing environment (such as the Global Environment).
#
#  The name of the parameter that is being updated is __not__ specified here,
#  but only within the __Sampler__ function call.
#
#  See the documentation for examples.
#
# VALUE:
#  An object of class [PSMetropolis]
#  to be further processed by the Umacs function \code{Sampler}.
#



# ========================================================================
# templates.PSMetropolis - 
# ========================================================================

setMethod('templates', signature('PSMetropolis'),
function(obj)
{
  old.templates <- callNextMethod()
  PSMetropolis.templates <- snippetlist(
    tpl.proposal     = obj@snippets$tpl.proposal,
    tpl.reject       = .tpl.reject.PSMetropolis,
    tpl.init.Sigma   = .tpl.init.Sigma.PSMetropolis,
    tpl.adapt        = .tpl.adapt.PSMetropolis,
    tpl.adaptation   = .tpl.adaptation.PSMetropolis
  )
  c(old.templates, PSMetropolis.templates)
})


# ========================================================================
# template .tpl.init.Sigma.PSMetropolis - 
# ========================================================================

.tpl.init.Sigma.PSMetropolis <- function ()
{
  Sigma.scale  <- if (code.Sigma.scale.specified) code.Sigma.scale else rep(1, len)
  if (length(Sigma.scale)!=len) Sigma.scale <- rep(Sigma.scale, length.out=len)
}


# ========================================================================
# template .tpl.adaptation.PSMetropolis - 
# ========================================================================

.tpl.adaptation.PSMetropolis <- function ()
{
  if (adapt.now) {
    adapt()
  }
}

# ========================================================================
# .tpl.proposal.PSMetropolis
# ========================================================================

.tpl.proposal.PSMetropolis <- function () rnorm(length(PARAM), PARAM, Sigma.scale)

# ========================================================================
# .tpl.reject.PSMetropolis
# ========================================================================


.tpl.reject.PSMetropolis <- function ()
{
#cat("++++++++++++++++++++++++++++++++",QPARAM,"++++++++\n")
  logpost.old <- .logpost()
#cat(QPARAM,"logpost.old=",logpost.old,"\n")
  old.value <- PARAM
  PARAM <<- proposal()
#cat(QPARAM,"proposal=",PARAM,"\n")
  logpost.new <- .logpost()
#cat(QPARAM,"logpost.new=",logpost.new,"\n")
  # Logpost must return a vector of the same length as PARAM.
  lhratio <- (logpost.new - logpost.old)
  if (any(is.nan(lhratio))) {
    error("PSMetropolis parameter",QPARAM,"has NaN in lh-ratio:")
    print(lhratio)
    cat("Log-post new and log-post old follow:\n")
    print(logpost.new)
    print(logpost.old)
    cat("\n")
    cat("Temporarily fixed by setting NaN to -Inf\n")
    lhratio[is.nan(lhratio)] <- -Inf
    print(lhratio)
  }
  this.jump.prob <- pmin(1, exp(lhratio))
#cat(QPARAM,"this.jump.prob=",this.jump.prob,"\n")
  reject <- (runif(len) > this.jump.prob)
#cat(QPARAM,"reject=",reject,"\n")
  if (any(reject)) PARAM[reject] <<- old.value[reject]
#cat(QPARAM,"=",PARAM,"\n===========================\n")
}

# ========================================================================
# adapt.PSMetropolis  - 
# ========================================================================
#

.tpl.adapt.PSMetropolis <- function ()
{
  if (!code.Sigma.scale.specified) {
    Sigma.scale <<- scaleAdapt(PARAM, avg.p.jump, Sigma.scale, old.avg.p.jumps, old.scales)
ifdebug(QPARAM, "ADAPT: avg.p.jump=",avg.p.jump, "->", "new scale ", Sigma.scale)
##ifdebug(QPARAM, "PJUMP: old.avg.p.jumps",old.avg.p.jumps)
  }
}


# end PSMetropolis.R

# ========================================================================
# class PMetropolis -  Parallel-Scalar Metropolis SamplingScheme
# ========================================================================
# byCol = FALSE (default) : the independent vectors are in rows.
# byCol = TRUE            : the independent vectors are in columns.
#.

setClass('PMetropolis',
  representation(byCol='logical'),
  contains='PSMetropolis'
)

# NAME:
#  PMetropolis-class - Class definition of "PMetropolis" (Umacs package)
# DESCRIPTION:
#  This class is used only internally, in Umacs.
# DETAILS:
#  Users need only use the function [[PMetropolis]] inside a [[Sampler]]
#  function call,
#  which outputs a [PMetropolis] object and is directly processed by [[Sampler]].
# SLOTS:
#  Refer to the class [PSMetropolis] ([[PSMetropolis-class]])
# 



# ========================================================================
# PMetropolis  -  create new PMetropolis SamplingScheme
# ========================================================================

## DEBUG: make it so that we must refer to proposals, adaptation routines
## by a token and not by actual name.

PMetropolis <- function(
   logpost=NULL,
   init,
   proposal=NULL,
   adapt.scale.function='scaleAdapt.PMetropolis',
   scale.jump=NA,
   class.='PMetropolis', 
   byCol=FALSE
)
{
  if (is.null(proposal)) {
    proposal <- .tpl.proposal.PMetropolis
  }
  snips <- snippetlist(
    code.logpost      = logpost,
    tpl.proposal      = proposal,
    scaleAdapt        = as.language(adapt.scale.function),
    exists.code.logpost = !is.null(logpost),
    CODEINIT   = init,
    code.Sigma.scale  = scale.jump,
    code.Sigma.scale.specified  = !is.na(scale.jump),
    BYCOL=byCol
  )
  new(class., snippets=snips)
}

# NAME:
#   PMetropolis - Generate an PMetropolis object for the Umacs Sampler function
#
# DESCRIPTION:
#  Generates an object including the information necessary 
#  to generate a ``Parallel Metropolis" updating step for a parameter,
#  within the main loop
#  of an iterative sampler function.
#
#  The sampler is built using the Umacs function [[Sampler]];
#  and the arguments ([update] and [init])
#  are embedded in the sampler function. 
#
# ARGUMENTS:
#  ~ logpost : An R function calculating the value (a vector!) of the unnormalized log-posterior function given all other arguments
#  ~   init : An R function that returns a (random) starting point (a vector) for a Markov chain for the parameter
#  ~   proposal : (not used now)
#  ~   adapt.scale.function : name of the function used to adapt the proposal kernel (usually, just ignore and use the default)
#  ~   scale.jump : The scale of the proposal kernel matrix, if adaptation is not to be used
#  ~   class. : (for internal use only; ignore)
#  ~   byCol : TRUE if the parameter is a matrix consisting of columns of independent vectors; the default is FALSE: the parameter is a matrix consisting of rows of independent vectors
#
# DETAILS:
#  The `PMetropolis' is short for `Parallel Metropolis':
#  the parameter to be updated is a __matrix__ consisting of 
#  __rows of independently updateable vectors__.
#  That is, each row of the matrix can be updated independently (``in parallel'')
#  using a (vector) Metropolis updating step.
#  (Alternatively the independent vectors can be in the columns of the matrix,
#  if the argument [byCol] is set to TRUE.)
#
#  In other words, you would use [PMetropolis]
#  if conditional on other parameters in the model, 
#  you have independent (exchangeable) vector-valued parameters
#  that you wish to update using the Metropolis algorithm.
#
#  If instead you have conditionally independent __scalar-valued__ parameters,
#  use \code{PSMetropolis} instead.  
#
#  An alternative to using [PMetropolis] is to define 
#  each row as a separate parameter, but this would be cumbersome and 
#  require lots of repetitive coding, and the code would not be
#  optimized.  
#
#  [PMetropolis] is to be used only within the [[Sampler]] function call.
#
#  The arguments of the update and init functions are ignored.
#  The body of the function can contain references to any parameters
#  that are defined in the \code{Sampler} function call, or to any 
#  variables in an enclosing environment (such as the Global Environment).
#
#  The name of the parameter that is being updated is __not__ specified here,
#  but only within the [[Sampler]] function call.
#
# VALUE:
#  An object of class [PMetropolis],
#  to be further processed by the Umacs function [[Sampler]].


# ========================================================================
# templates.PMetropolis - 
# ========================================================================

setMethod('templates', signature('PMetropolis'),
function(obj)
{
  old.templates <- callNextMethod()
  PMetropolis.templates <- snippetlist(
    tpl.proposal     = obj@snippets$tpl.proposal,
    tpl.reject       = .tpl.reject.PMetropolis,
    tpl.init.Sigma   = .tpl.init.Sigma.PMetropolis,
    tpl.adapt        = .tpl.adapt.PMetropolis,
    tpl.adaptation   = .tpl.adaptation.PMetropolis
  )
  c(old.templates, PMetropolis.templates)
})


# ========================================================================
# template .tpl.init.Sigma.PMetropolis - 
# ========================================================================

.tpl.init.Sigma.PMetropolis <- function ()
{
  dimension <- dim(PARAM)
  if (is.null(dimension)) {
    # DEBUG: testing...
    warning("PMetropolis requires a matrix parameter.")
    dimension <- c(1,1)
  }
  if (BYCOL) dimension <- rev(dimension)
  vec <- dimension[1] # Number of rows (independent vectors).
  len <- dimension[2] # Number of columns (scalars in the vector).
  Sigma.scale  <- if (code.Sigma.scale.specified) rep(code.Sigma.scale, vec) else rep(1, vec)
  if (length(Sigma.scale)!=vec) Sigma.scale <- rep(Sigma.scale, length.out=vec)
  Sigma.shape <- as.list(rep(NA, vec))
  Sigma.jump  <- as.list(rep(NA, vec))
  Sigma.jump.eigen  <- as.list(rep(NA, vec))
  for (i in 1:vec) {
    Sigma.shape[[i]] <- diag(1, len)
    Sigma.jump[[i]] <- Sigma.shape[[i]]*(Sigma.scale[i]^2)
    Sigma.jump.eigen[[i]] <- .compute.eigen(Sigma.jump[[i]])
  }
}

# ========================================================================
# template .tpl.adaptation.PMetropolis - 
# ========================================================================

.tpl.adaptation.PMetropolis <- function ()
{
  # Stick together, row by row
  jumps <<- if (BYCOL) cbind(jumps, t(PARAM)) else cbind(jumps, PARAM)
  # this.jump.prob is a vector of length n (=number of independent vectors)
  if (adapt.now) {
    # Keep old avg.p.jumps stored. Each row i contains avg.p.jump's for vector i.
    if (BYCOL) PARAM <<- t(PARAM)
    adapt()
    if (BYCOL) PARAM <<- t(PARAM)
  }
}

# ========================================================================
# .tpl.proposal.PMetropolis
# ========================================================================

# mvrnorml is a special version that accepts a list for Sigma.jump.eigen.

.tpl.proposal.normal.PMetropolis <- function () mvrnorml(PARAM, Sigma.jump.eigen, BYCOL)

.tpl.proposal.PMetropolis <- .tpl.proposal.normal.PMetropolis


# ========================================================================
# .tpl.reject.PMetropolis
# ========================================================================

.tpl.reject.PMetropolis <- function ()
{
  logpost.old <- .logpost()
  old.value <- PARAM
  PARAM <<- proposal()
  logpost.new <- .logpost()
  # Logpost must return a vector with the same number of rows as PARAM has.
  lhratio <- (logpost.new - logpost.old)
  this.jump.prob <- pmin(1, exp(lhratio))
  # 'reject' will contain TRUE in element i if a row i is rejected.
  reject <- (runif(len) > this.jump.prob)
  # Replace the rejected rows if necessary.
  if (any(reject)) PARAM[reject,] <<- old.value[reject,] 
}


# ========================================================================
# .tpl.adapt.PMetropolis  - 
# ========================================================================
#

.tpl.adapt.PMetropolis <- function () # NEW
{
  if (!code.Sigma.scale.specified) {
    Sigma.scale <<- scaleAdapt(PARAM, avg.p.jump, Sigma.scale, old.avg.p.jumps, old.scales)
ifdebug(QPARAM, " ADAPT: avg.p.jump=",avg.p.jump[i], "->", "new scale ", Sigma.scale[i],"\n")
  }
  # a has dimension (n.parameters, n.jumps, indep.vectors)
  a <- array(t(jumps), c(dimension[2], n.jumps, dimension[1]))
  # a has dimension (n.jumps, n.parameters, indep.vectors)
  a <- aperm(a, c(2,1,3))
  for (i in 1:vec) {
##ifdebug("[",i,"]  PJUMP: old.avg.p.jumps",old.avg.p.jumps[i,],"\n")
    ## Now adapt the covariance matrix.
    Sigma.emp <- cov(a[,,i]) # each row of this matrix contains a realized jump
    d <- det(Sigma.emp)
    if (.about.zero(d)) {
ifdebug("Empirical sigma")
if (debug) print(Sigma.emp)
      Sigma.emp <- Sigma.emp + diag(length(diag(Sigma.emp)))*1e-6
      d <- det(Sigma.emp)
    }
    n.adapted <- nrow(old.avg.p.jumps)
    Sigma.emp <- Sigma.emp*exp(-log(d)/nrow(Sigma.emp))
    Sigma.shape[[i]] <<- (n.adapted*Sigma.shape[[i]] + Sigma.emp)/(n.adapted+1)
    d <- det(Sigma.shape[[i]])
    if (.about.zero(d)) {
       stop("DEBUG: Fatal error: Shape matrix has zero determinant.")
    }
    Sigma.shape[[i]] <- Sigma.shape[[i]]*exp(-log(d)/nrow(Sigma.shape))
    Sigma.jump[[i]] <<- (Sigma.scale[i]^2)*Sigma.shape[[i]]
    Sigma.jump.eigen[[i]] <<- .compute.eigen(Sigma.jump[[i]])
ifdebug("  Sigma.scale=", Sigma.scale[i], "; Sigma.shape follows:\n")
if (debug) print(Sigma.shape[[i]])
  }
ifdebug("----")
}




# ========================================================================
# scaleAdapt.PMetropolis  - default metropolis adaptation for vectors
# ========================================================================

scaleAdapt.PMetropolis <- function(x, this.p.jump, this.scale, old.p.jumps, old.scales, max.multiply=100)
{
  # Find an estimate of the optimal jumping scale
  # given the current scale and avg.p.jump
  # Use the following crude relation:
  # p = exp(-b*scale)
  # from where b can be estimated crudely by b = -log(p)/scale
  # if we are given a pair (p,scale).
  # If p.opt is the optimal scale, scale.opt = -log(p.opt)/b.
  # After we get (p,scale), estimate b and compute
  # scale.opt = -log(p.opt)/b = (log(p.opt)/log(p))*scale.
  #
  # I tried Metropolis on a simple normal mean estimation with fixed variance=1
  # and noticed that p = exp(-b*sqrt(scale)) is much closer.
  # The following code will compute a linear regression on
  # -log(p) ~ sqrt(scale) without an intercept, obtaining an estimate of b.
  # 
  xlen <- ncol(x)
  opt.ps <- c(0.441, 0.352, 0.316, 0.279, 0.275, 0.266, 0.261, 0.255, 0.261, 0.267, 0.234)
  log.opt.p <- -log(opt.ps[min(xlen, length(opt.ps))])
  new.scale <- this.scale
  p0 <- (this.p.jump==0)
  p1 <- (this.p.jump==1)
  p01 <- (!(p0 | p1))
  if (any(p0)) new.scale[ p0 ] <- this.scale[ p0 ]/max.multiply
  if (any(p1)) new.scale[ p1 ] <- this.scale[ p1 ]*max.multiply
  if (any(p01)) {
    new.scale[p01] <- this.scale[p01]*(log.opt.p/log(this.p.jump[p01]))^2
    new.scale[p01] <- pmin(max.multiply, pmax(1/max.multiply, new.scale[p01]))
  }
  new.scale
}


# end PMetropolis



# ========================================================================
# mcts  -  mcts class
# ========================================================================

setClass('mcts', 
  representation(
    chains = 'list',
    n.burnin='numeric',
    n.iter='numeric',
    n.sims='numeric',
    save.all='logical',
    col.names = 'character',
    summary='matrix'
  )
)

# NAME:
#   mcts-class - Class `mcts' (``Markov Chain Time Series'')
# DESCRIPTION:
#   mcts = "Markov Chain Time Series"; a wrapper for holding 
#   matrix of simulation of MCMC chains along with relevant information.
#   Umacs returns simulations in the form of [mcts] objects.
#  objects: Objects can be created using [mcts(...)]
# SLOTS:
#   ~ chains: 
#   ~ n.burnin: 
#   ~ n.chains: 
#   ~ n.iter: 
#   ~ n.sims: 
#   ~ col.names:
#   ~ save.all:
# NOTE:
#   [mcts] objects can be coerced into random variable (rv) objects using
#   [as.rv]. See package `rv' for details.
#



# ========================================================================
# .inspect  -  
# ========================================================================



# ========================================================================
# mcts  -  new mcts object
# ========================================================================


mcts <- function (chains,
    n.burnin=0,
    n.chains=numeric(0),
    n.iter=numeric(0),
    n.sims=n.iter,
    col.names=character(0),
    save.all=TRUE
  )
{
  obj <- new('mcts', 
    chains=chains,
    col.names =col.names,
    n.iter=n.iter,
    n.burnin=n.burnin,
    n.sims=n.sims,
    save.all=save.all
  )
  obj@summary <- .summarize.mcts(obj)
  obj
}


# NAME:
#   mcts - Create a New mcts Object
# DESCRIPTION:
#   A wrapper for new('mcts', ...)
# 
# ARGUMENTS: 
# ~ chains: 
# ~ n.burnin: 
# ~ n.chains: 
# ~ n.iter: 
# ~ n.sims: 
# ~ col.names:
# ~ save.all:
#
# DETAILS:
#
# VALUE: 
#   An object of the class [mcts] ([[mcts-class]]).
#

### ========================================================================
### chains.mcts  -  access chains of an mcts object
### ========================================================================

##setMethod('chains', signature('mcts'),
##function (obj)
##{
##  obj@chains
##})

# ========================================================================
# as.matrix.mcts  -  mcts chains as matrix
# ========================================================================

as.matrix.mcts <- function (x)
{
  chains <- x@chains
  n.chains <- length(chains)
  par.names <- dimnames(chains[[1]])[[2]]
  dims <- dim(chains[[1]])
  n.params <- dims[2]
  #
  Ls <- sapply(chains, function (x) dim(x)[1])
  if (!all(Ls==Ls[1])) {
    warning("All chains are not of the same length.")
  }
  L <- max(Ls)
  #
  # Set up the matrix.
  mat <- array(NA, c(L, n.chains, n.params))
  for (chain in 1:n.chains) {
    ch <- chains[[chain]]
    if (nrow(ch)<L) {
      repeat {  ## What a kludge...
        ch <- rbind(ch,ch)
        if (nrow(ch)>=L) break
      }
      ch <- ch[1:L,]
   }
    mat[,chain,] <- ch
  }
  if (length(par.names)<n.params) par.names <- NULL
  # Delete rows that are inside the burn-in period.
  if (x@save.all) {
    L.save <- (1:L > x@n.burnin)
    mat <- mat[L.save,,,drop=FALSE]
  }
  L <- dim(mat)[1]
  n.sims <- x@n.sims
  n.saved <- dim(mat)[1]
  # 
  if (n.sims>0 && n.sims<n.saved) {
    n.thin <- floor(n.saved/n.sims)
    if (n.thin>1) {
      L.thin <- (1:L %% n.thin == 0)
      mat <- mat[L.thin,,,drop=FALSE]
    }
    n.saved <- dim(mat)[1]
    if (n.sims<n.saved) {
      w <- sample(1:n.saved, n.sims)
      mat <- mat[w,,,drop=FALSE]
    }
  }
  dimnames(mat) <- list(NULL, NULL, par.names)
  mat
}

# ========================================================================
# c.mcts  -  mcts chains as matrix
# ========================================================================

c.mcts <- function (..., recursive=FALSE)
{
  stop("c(...) not yet implemented")
}

# ========================================================================
# monitor.mcts  -  convergence diagnostics etc.
# ========================================================================
# This calls A. Gelman's monitor function, see monitor.R in this package.

monitor.mcts <- function (obj, trans = NULL, keep.all = TRUE, Rupper.keep = FALSE) 
{
  .monitor(as.matrix(obj), trans=trans, keep.all=keep.all, Rupper.keep=Rupper.keep)
}

# ========================================================================
# summary.mcts  -  
# ========================================================================
#

summary.mcts <-function(object, ...)
{
  cat("Object of type 'mcts'\n")
  ##.monitor.mcts(object, keep.all=TRUE, ...)
  object@summary
}

.summarize <- function (vec) {
  # We need sd(as.vector(vec)) since vec may be an array!
  .ms <- c(mean(vec), if (length(vec)==1) 0 else sd(as.vector(vec)))
  names(.ms) <- c('mean', 'sd')
  .q <- quantile(vec,c(0.025,0.25,0.5,0.75,0.975))
  c(.ms, .q)
}

.summarize.mcts <- function (object, ...)
{
  mat <- as.matrix(object)
  .summat <- t(apply(mat, 3, .summarize))
  n.sims <- prod(dim(mat)[1:2])
  cbind(.summat, mcmcdiag(object), n.sims)
}

# ========================================================================
# mcmcdiag  -  mcmc diagnostics for a mcts object
# ========================================================================

setMethod('mcmcdiag', signature('mcts'),
function(obj, trans=NULL, Rupper=FALSE)
{
  mat <- as.matrix(obj)
  n.params <- dim(mat)[3]
  for (i in 1:n.params)
  {
    ai <- mat[,,i]
    if (any(is.na(ai))) {
      cat("Parameter ",i," has NAs\n") # DEBUG!!!
    }
    if (is.null(trans) || trans[i]=='log') {
      if (all(ai>0)) {
        # double-check also in the case of user-defined trans.
        mat[,,i] <- log(ai)
      }
    } else if (trans[i]=='logit')
        mat[,,i] <- logit(ai)
  }
  t(apply(mat, 3, mcmcdiag, Rupper=Rupper))
})


setMethod('mcmcdiag', signature('matrix'),
function(obj, n.chains=dim(x)[2], Rupper=TRUE)
{
  #
  # From A.Gelman's R2WinBUGS package.
  #
  # obj : a vector of length n*n.chains or an n times n.chains matrix
  #       with the n.chains time series concatenated one after another.
  #
  x <- obj
  if (is.null(dim(x))) {
    l.x <- length(x)
    n <- l.x %/% n.chains # rows
    if (l.x %% n.chains != 0) # In the rare case someone gives nonbalanced data
      x <- x[1:(n*n.chains)]
    dim(x) <- c(n,n.chains)
  }  # otherwise, x is an n times n.chains matrix
  #
  n <- nrow(x)
  m <- ncol(x)
  xdot <- apply(x, 2, mean)
  muhat <- mean(xdot)
  s2 <- apply(x, 2, var)
  s2[is.na(s2)] <- 0 # If we have just one iteration...
  W <- mean(s2)
  if ((W > 1e-08) && (m > 1)) { # m = n.chains
    B <- n * var(xdot)
    varW <- var(s2)/m
    varB <- B^2 * 2/(m - 1)
    covWB <- (n/m) * (cov(s2, xdot^2) - 2 * muhat * cov(s2, xdot))
    sig2hat <- ((n - 1) * W + B)/n
    postvar <- sig2hat + B/(m * n)
    varpostvar <- max(0, (((n - 1)^2) * varW + (1 + 1/m)^2 * 
      varB + 2 * (n - 1) * (1 + 1/m) * covWB)/n^2)
    post.df <- min(2 * (postvar^2/varpostvar), 1000)
    confshrink.range <- postvar/W
    if (Rupper) {
      varlo.df <- 2 * (W^2/varW)
      confshrink.range <- c(confshrink.range, (n - 1)/n + 
        (1 + 1/m) * (1/n) * (B/W) * qf(0.975, m - 1, varlo.df))
    }
    confshrink.range <- sqrt(confshrink.range * (post.df + 3)/(post.df + 1))
    n.eff <- m * n * min(sig2hat/B, 1)
    #
    n.eff <- round(n.eff, min(0, 1 - floor(log10(n.eff)))) # rounded here...
    #
    output <- c(confshrink.range, as.integer(n.eff))
  } else {
    # Rhat = 1, n.eff = 1.
    output <- c(rep(1, Rupper + 1), 1)
  }
  names(output) <- c('Rhat', if(Rupper) 'Rupper' else NULL, 'n.eff')
  output
})

# ========================================================================
# print.mcts  -  print method for mcts objects
# ========================================================================
#

print.mcts <- function(x, ...)
{
  print(summary.mcts(x), ...)
}

setMethod('show', signature('mcts'),
  function(object) {
    print(object, digits=2)
})

#
# ========================================================================
# as.rv.mcts  -  build a random vector from the mcts object
# ========================================================================
#

as.rv.mcts <- function(obj)
{
# NAME:
#   as.rv.mcts - Coerce an `mcts' object to an `rv' object
# DESCRIPTION:
#   Coerce an mcts object (esp. those made with Umacs samplers) to an rv object (those compatible with the package `rv')
# ARGUMENT:
#  ~ obj : An mcts object to coerce
# DETAILS:
#   Class `rv' is a simulation-based random variable class. 
#   The simulations stored inside [obj] will be copied into the components of an
#   `rv' object.
# VALUE:
#   An object of class [rv].
# NOTE:
#   Requires package `rv'.
# KEYWORD: manip
#
  require('rv')
  chain <- obj@chains
  n.chains <- length(chain)
  n.sims <- getnsims()
  obj@n.sims <- n.sims
  sims <- as.matrix(obj)
  mcmcattr <- mcmcdiag(obj)
  x <- rvsims(sims, n.sims=n.sims) # rvsims is in package 'rv'
  a <- apply(mcmcattr, 2, as.list)
  rvattr(x) <- a
  x
}




# ========================================================================
# [.mcts  -- subsetting
# ========================================================================
#

"[.mcts" <- function(x, i, ...)
{
  # !!! DOESN'T WORK YET.
  stop("[.mcts Doesn't work yet")
  m <- x$chain
  for (chain in 1:length(m)) {
    x$chain[[chain]] <- m[[chain]][,i,drop=FALSE]
  }
  x$par.names <- x$par.names[i]
  x$par.lengths <- x$par.lengths[i]
  x
}



# ========================================================================
# as.bugs.mcts
# ========================================================================

as.bugs.mcts <- function (x)
{
# NAME:
#   as.bugs.mcts - Coerce an `mcts' object to a `bugs' object
# DESCRIPTION:
#   Coerce an mcts object (esp. those made with Umacs samplers) to an rv object (those compatible with the package `rv')
# ARGUMENT:
#  ~ x : An mcts object to coerce
# DETAILS:
#   Class `bugs' is a simulation-based random variable class. 
#   The simulations stored inside [obj] will be copied into the components of an
#   `rv' object.
# VALUE:
#   An object of class [rv].
# NOTE:
#   Requires package `rv'.
# SEEALSO:
#   Package [R2WinBUGS].
# KEYWORD: manip
#

  require(R2WinBUGS)
  DIC <- TRUE
  sims.array <- as.matrix(x) # Will do thinning for us.
  if (length(parameter.names <- x@col.names)<1) {
    parameter.names <- dimnames(x@chains[[1]])[[2]]
  }
  parameters.to.save <- unique(sapply(strsplit(parameter.names, "\\["), "[", 1))
  d <- dim(sims.array)
  n.burnin     <- x@n.burnin
  n.keep       <- d[1]
  n.chains     <- d[2]
  n.parameters <- d[3]
  n.sims       <- n.keep*n.chains
  n.iter       <- x@n.iter
  n.thin       <- 1
  #
  sims <- matrix(NA, n.sims, n.parameters)
  root.long <- character(n.parameters)
  indexes.long <- vector(n.parameters, mode = "list")
  for (i in 1:n.parameters) {
    temp <- R2WinBUGS:::decode.parameter.name(parameter.names[i])
    root.long[i] <- temp$root
    indexes.long[[i]] <- temp$indexes
  }
  n.roots <- length(parameters.to.save)
  left.bracket.short <- as.vector(regexpr("[[]", parameters.to.save))
  right.bracket.short <- as.vector(regexpr("[]]", parameters.to.save))
  root.short <- ifelse(left.bracket.short == -1, parameters.to.save, 
      substring(parameters.to.save, 1, left.bracket.short - 
          1))
  dimension.short <- rep(0, n.roots)
  indexes.short <- vector(n.roots, mode = "list")
  n.indexes.short <- vector(n.roots, mode = "list")
  long.short <- vector(n.roots, mode = "list")
  length.short <- numeric(n.roots)
  for (j in 1:n.roots) {
      long.short[[j]] <- (1:n.parameters)[root.long == root.short[j]]
      length.short[j] <- length(long.short[[j]])
      if (length.short[j] == 0) 
          stop(paste("parameter", root.short[[j]], "is not in the model"))
      else if (length.short[j] > 1) {
          dimension.short[j] <- length(indexes.long[[long.short[[j]][1]]])
          n.indexes.short[[j]] <- numeric(dimension.short[j])
          for (k in 1:dimension.short[j]) n.indexes.short[[j]][k] <- length(unique(unlist(lapply(indexes.long[long.short[[j]]], 
              .subset, k))))
          length.short[j] <- prod(n.indexes.short[[j]])
          if (length(long.short[[j]]) != length.short[j]) 
              stop(paste("error in parameter", root.short[[j]], 
                "in parameters.to.save"))
          indexes.short[[j]] <- as.list(numeric(length.short[j]))
          for (k in 1:length.short[j]) indexes.short[[j]][[k]] <- indexes.long[[long.short[[j]][k]]]
      }
  }
  rank.long <- unlist(long.short)
  for (k in 1:n.parameters) {
    sims[,k] <- as.vector(sims.array[,,k])
  }
  dimnames(sims) <- list(NULL, parameter.names)
  summary <- R2WinBUGS:::monitor(sims.array, n.chains, keep.all = TRUE)
  last.values <- as.list(numeric(n.chains))
  for (i in 1:n.chains) {
    n.roots.0 <- if (DIC) 
        n.roots - 1
    else n.roots
    last.values[[i]] <- as.list(numeric(n.roots.0))
    names(last.values[[i]]) <- root.short[1:n.roots.0]
    for (j in 1:n.roots.0) {
        if (dimension.short[j] <= 1) {
            last.values[[i]][[j]] <- sims.array[n.keep, i, 
               long.short[[j]]]
            names(last.values[[i]][[j]]) <- NULL
        }
        else last.values[[i]][[j]] <- aperm(array(sims.array[n.keep, 
           i, long.short[[j]]], rev(n.indexes.short[[j]])), 
            dimension.short[j]:1)
    }
  }
  sims <- sims[sample(n.sims), ]
  sims.list <- summary.mean <- summary.sd <- summary.median <- vector(n.roots, 
      mode = "list")
  names(sims.list) <- names(summary.mean) <- names(summary.sd) <- names(summary.median) <- root.short
  for (j in 1:n.roots) {
    if (length.short[j] == 1) {
        sims.list[[j]] <- sims[, long.short[[j]]]
        summary.mean[[j]] <- summary[long.short[[j]], "mean"]
        summary.sd[[j]] <- summary[long.short[[j]], "sd"]
        summary.median[[j]] <- summary[long.short[[j]], "50%"]
    }
    else {
        temp2 <- dimension.short[j]:1
        sims.list[[j]] <- aperm(array(sims[, long.short[[j]]], 
            c(n.sims, rev(n.indexes.short[[j]]))), c(1, (dimension.short[j] + 
            1):2))
        summary.mean[[j]] <- aperm(array(summary[long.short[[j]], 
            "mean"], rev(n.indexes.short[[j]])), temp2)
        summary.sd[[j]] <- aperm(array(summary[long.short[[j]], 
            "sd"], rev(n.indexes.short[[j]])), temp2)
        summary.median[[j]] <- aperm(array(summary[long.short[[j]], 
            "50%"], rev(n.indexes.short[[j]])), temp2)
    }
  }
  summary <- summary[rank.long, ]
  all <- list(n.chains = n.chains, n.iter = n.iter, n.burnin = n.burnin, 
        n.thin = n.thin, n.keep = n.keep, n.sims = n.sims, sims.array = sims.array[, 
            , rank.long, drop = FALSE], sims.list = sims.list, 
        sims.matrix = sims[, rank.long], summary = summary, mean = summary.mean, 
        sd = summary.sd, median = summary.median, root.short = root.short, 
        long.short = long.short, dimension.short = dimension.short, 
        indexes.short = indexes.short, last.values = last.values, is.DIC=DIC)
    if (DIC) {
        deviance <- all$sims.array[, , dim(sims.array)[3], drop = FALSE]
        dim(deviance) <- dim(deviance)[1:2]
        pD <- numeric(n.chains)
        DIC <- numeric(n.chains)
        for (i in 1:n.chains) {
            pD[i] <- var(deviance[, i])/2
            DIC[i] <- mean(deviance[, i]) + pD[i]
        }
        all <- c(all, list(pD = mean(pD), DIC = mean(DIC)))
    }
  class(all) <- "bugs"
  return(all)
}


# end mcts

