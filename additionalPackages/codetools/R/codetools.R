## WARNING:
## This code is a complete hack, may or may not work, etc..
## Use your own risk.  You have been warned.

##
## Environment utilities
##

.BaseEnv <- if (exists("baseenv")) baseenv() else NULL
.EmptyEnv <- if (exists("emptyenv")) emptyenv() else NULL

is.emptyenv <- function(e) identical(e, .EmptyEnv)
is.baseenv <- function(e) identical(e, .BaseEnv)

mkHash <- function() new.env(hash = TRUE, parent = .EmptyEnv)

##
## Code walker
##

walkCode <- function(e, w = makeCodeWalker()) {
    if (typeof(e) == "language") {
        if (typeof(e[[1]]) %in% c("symbol", "character")) {
            h <- w$handler(as.character(e[[1]]), w)
            if (! is.null(h)) h(e, w)
            else w$call(e, w)
        }
        else w$call(e, w)
    }
    else w$leaf(e, w)
}
makeCodeWalker <- function(...,
                           handler = function (v, w) NULL,
                           call = function(e, w)
                               for (ee in as.list(e))
                                   if (! missing(ee)) walkCode(ee, w),
                           leaf = function(e, w) print(e))
     list(handler = handler, call = call, leaf = leaf, ...)

##
## Call tree display
##

showTree <- function(e, write = cat) {
    w <- makeCodeWalker(call = showTreeCall, leaf = showTreeLeaf,
                        write = write)
    walkCode(e, w)
    w$write("\n")
}
showTreeCall <- function(e, w) {
    w$write("(")
    walkCode(e[[1]], w)
    for (a in as.list(e[-1])) {
        w$write(" ")
        if (missing(a))
            w$write("<Missing>")
        else
            walkCode(a, w)
    }
    w$write(")")
}
showTreeLeaf <- function(e, w) {
    if (typeof(e) == "symbol") {
        if (e == "(") w$write("\"(\"")
        else if (e == "{") w$write("\"{\"")
        else w$write(e)
    }
    else w$write(deparse(e))
}

##
## Call with current continuation
##

if (! exists("callCC"))
callCC <- function(fun) {
    value <- NULL
    delayedAssign("throw", return(value))
    fun(function(v) { value <<- v; throw })
}

##
## Constant folding
##

makeConstantFolder <- function(...,
                               leaf = foldLeaf,
                               handler = function(v, w)
                                   if (w$foldable(v, w)) foldCall,
                               call = function(e, w) exitFolder(e, w),
                               exit = function(e, w)
                                  stop0(paste("not a foldable expression:",
                                        deparse(e, width.cutoff = 500))),
                               isLocal = function(v, w) FALSE,
                               foldable = isFoldable,
                               isConstant = isConstantValue,
                               signal = function(e, msg, w) warning0(msg))
     list(handler = handler, call = call, exit = exit, leaf = leaf,
          isLocal = isLocal, foldable = isFoldable,
          isConstant = isConstant, signal = signal, ...)

exitFolder <- function(e, w) {
    w$exit(e, w)
    stop0("constant folding cannot continue")
}
constantFold <- function(e, env = NULL, fail = NULL) {
    job <- function(exit) {
        isLocal <- function(v, w) as.character(v) %in% env
        doExit <- function(e, w) exit(fail)
        w <- makeConstantFolder(isLocal = isLocal, exit = doExit)
        walkCode(e, w)
    }
    callCC(job)
}
constantFoldEnv <- function(e, env = .GlobalEnv, fail = NULL) {
    isLocal <- function(v, w) {
        vname <- as.character(v)
        while (! identical(env, .GlobalEnv)) {
            if (exists(vname, env, inherits = FALSE))
                return(TRUE)
            env <- parent.env(env)
        }
        FALSE
    }
    job <- function(exit) {
        doExit <- function(e, w) exit(fail)
        w <- makeConstantFolder(isLocal = isLocal, exit = doExit)
        walkCode(e, w)
    }
    tryCatch(callCC(job), error = function(e) fail)
}
isConstantValue <- function(v, w)
    is.null(v) ||
    (is.null(attributes(v)) && is.atomic(v)) ||
    (is.list(v) && (identical(v, .Platform) || identical(v, .Machine)))
isFoldable <- function(v, w)
    ((typeof(v) == "symbol" || typeof(v) == "character") &&
     as.character(v) %in% foldFuns && ! w$isLocal(v, w))

foldFuns <- c("+", "-", "*", "/", "^", "(",
              ">", ">=", "==", "!=", "<", "<=", "||", "&&", "!",
              "|", "&", "%%",
              "sqrt", "log", "exp",
              "c", "as.integer", "vector", "integer","numeric","character",
              "rep",
              ":",
              "cos", "sin", "tan", "acos", "asin", "atan", "atan2",
              "is.R", "$", "[", "[[")
constNames <- c("pi", "T", "F", ".Platform", ".Machine")

foldLeaf <- function(e, w) {
    if (is.name(e) && as.character(e) %in% constNames && ! w$isLocal(e, w))
        e <- get(as.character(e), envir = .BaseEnv)
    if (! w$isConstant(e)) exitFolder(e, w)
    e
}
foldCall <- function(e, w) {
    fname <- as.character(e[[1]])
    if (fname == "$") {
        args <- list(walkCode(e[[2]], w), e[[3]])
        foldable <- w$isConstant(args[[1]], w)
    }
    else {
        args <- lapply(e[-1], function(e) walkCode(e, w))
        foldable <- all(sapply(args, w$isConstant, w))
    }
    if (foldable) {
        msg <- try({ v <- do.call(fname, args); NULL }, silent = TRUE)
        if (! is.null(msg)) {
            w$signal(e, msg, w)
            exitFolder(e, w)
        }
        else if (w$isConstant(v, w)) v
        else exitFolder(e, w)
    }
    else exitFolder(e, w)
}

##
## Finding local variables
##

makeLocalsCollector <- function(...,
                                leaf = function (e, w) character(0),
                                handler = getCollectLocalsHandler,
                                isLocal = function(v, w) FALSE,
                                exit = function(e, msg, w) stop0(msg),
                                collect = function(v, e, w) print(v))
    makeCodeWalker(leaf = leaf, handler = handler, collect = collect,
                   isLocal = isLocal)
collectLocals <- function(e, collect) {
    w <- makeLocalsCollector(collect = collect)
    walkCode(e, w)
}
getCollectLocalsHandler <- function(v, w) {
    switch(v,
           "=" =,
           "<-" = collectLocalsAssignHandler,
           "for" = collectLocalsForHandler,
           "function" =,
           "~" = function(e, w) character(0),
           "local" = if (! w$isLocal(v, w))
               collectLocalsLocalHandler,
           "expression" =,
           "Quote" =,
           # **** could add handler for bquote here that looks at the .()'s
           "quote" = if (! w$isLocal(v, w))
               function(e, w) character(0),
           "delayedAssign" =,
           "assign" = function(e, w)
               if (length(e) == 3 && is.character(e[[2]]) &&
                   length(e[[2]]) == 1) {
                   w$collect(e[[2]], e, w)
                   walkCode(e[[3]], w)
               }
               else for (a in dropMissings(e[-1])) walkCode(a, w))
}
dropMissings <- function(x) {
    lx <- as.list(x)
    ix <- rep(TRUE, length(x))
    for (i in seq_along(ix)) {
        a <- lx[[i]]
        if (missing(a)) ix[i] <- FALSE
    }
    lx[ix]
}
collectLocalsAssignHandler <- function(e, w) {
    w$collect(getAssignedVar(e), e, w)
    walkCode(e[[2]], w)
    walkCode(e[[3]], w)
}

collectLocalsForHandler <- function(e, w) {
    signal <- function(msg) w$exit(e, msg, w)
    w$collect(as.character(checkSymOrString(e[[2]], signal)), e, w)
    walkCode(e[[3]], w)
    walkCode(e[[4]], w)
}

checkSymOrString <- function(e, signal = stop) {
    type <- typeof(e)
    if (type == "symbol" || type == "character") e
    else signal("not a symbol or string")
}
collectLocalsLocalHandler <- function(e, w) {
    if (length(e) == 2) # no explicit env
        character(0)
    else for (a in dropMissings(e[-1])) walkCode(a, w)
}
findLocalsList <- function(elist, envir = .BaseEnv) {
    localStopFuns <- c("expression", "quote", "Quote", "local")
    if (is.character(envir))
        locals <- envir
    else
        locals <- localStopFuns[! sapply(localStopFuns,isBaseVar, envir)]
    specialSyntaxFuns <- c("~", "<-", "=", "for", "function")
    sf <- unique(c(locals, localStopFuns))
    nsf <- length(sf)
    collect <- function(v, e, w) assign(v, TRUE, envir = env)
    isLocal <- function(v, w) as.character(v) %in% sf
    w <- makeLocalsCollector(collect = collect, isLocal = isLocal)
    repeat {
        env <- mkHash()
        for (e in elist) walkCode(e, w)
        isloc <- sapply(sf, exists, envir = env, inherits = FALSE)
        last.nsf <- nsf
        sf <- unique(c(locals, sf[isloc]))
        nsf <- length(sf)
        if (last.nsf == nsf) {
            vals <- ls(env, all.names = TRUE)
            rdsf <- vals %in% specialSyntaxFuns
            if (any(rdsf))
                warning0(paste("local assignments to syntactic functions:",
                               vals[rdsf]))
            return(vals)
        }
    }
}

findLocals <- function(e, envir = .BaseEnv)
    findLocalsList(list(e), envir)
findOwnerEnv <- function(v, env, stop = NA, default = NA) {
    while (! identical(env, stop))
        if (exists(v, envir = env, inherits = FALSE))
            return(env)
        else if (is.emptyenv(env))
            return(default)
        else env <- parent.env(env)
    default
}
isBaseVar <- function (v, env) {
    e <- findOwnerEnv(v, env)
    is.baseenv(e) || identical(e, .BaseNamespaceEnv) || v == "@<-"
}
findFuncLocals <- function(formals, body)
   findLocalsList(c(list(body), dropMissings(formals)))

##
## Assignment handling
##

getAssignedVar <- function(e) {
    v <- e[[2]]
    if (missing(v))
        stop0(paste("bad assignment:", pasteExpr(e)))
    else if (typeof(v) %in% c("symbol", "character"))
        as.character(v)
    else {
        while (typeof(v) == "language") {
            if (length(v) < 2)
                stop0(paste("bad assignment:", pasteExpr(e)))
            v <- v[[2]]
            if (missing(v))
                stop0(paste("bad assignment:", pasteExpr(e)))
        }
        if (typeof(v) != "symbol")
            stop0(paste("bad assignment:", pasteExpr(e)))
        as.character(v)
    }
}

evalseq <- function(e) {
    if (typeof(e) == "language") {
        v <- evalseq(e[[2]])
        e[[2]] <- as.name("*tmp*")
        c(v, list(e))
    }
    else list(e)
}
apdef <- function(e) {
    v <- NULL
    tmp <- as.name("*tmp*")
    tmpv <- as.name("*tmpv*")
    while (typeof(e) == "language") {
        ef <- e
        ef[[1]] <- makeAssgnFcn(e[[1]])
        if (typeof(ef[[2]]) == "language")
            ef[[2]] <- tmp
        ef$value <- tmpv
        v <- c(v, list(ef))
        e <- e[[2]]
    }
    v
}
makeAssgnFcn <- function(fun) {
    if (typeof(fun) == "symbol")
        as.name(paste0(as.character(fun), "<-"))
    else {
        if (getRversion() >= "2.13.0" &&
            typeof(fun) == "language" && typeof(fun[[1]]) == "symbol" &&
            as.character(fun[[1]]) %in% c("::", ":::") &&
            length(fun) == 3 && typeof(fun[[3]]) == "symbol") {
            fun[[3]] <- as.name(paste0(as.character(fun[[3]]), "<-"))
            fun
        }
        else
            stop(sQuote(deparse(fun)),
                 " is not a valid function in complex assignments")
   }
}
flattenAssignment <- function(e) {
    if (typeof(e) == "language")
        list(evalseq(e[[2]]), apdef(e))
    else list(NULL, NULL)
}

##
## Collecting usage information
##

makeUsageCollector <- function(fun, ..., name = NULL,
                               enterLocal = doNothing,
                               enterGlobal = doNothing,
                               enterInternal = doNothing,
                               startCollectLocals = doNothing,
                               finishCollectLocals = doNothing,
                               warn = warning0,
                               signal = signalUsageIssue) {
    if (typeof(fun) == "closure")
        env <- environment(fun)
    else
        env <- .GlobalEnv
    makeCodeWalker(..., name = name,
                   enterLocal = enterLocal,
                   enterGlobal = enterGlobal,
                   enterInternal = enterInternal,
                   startCollectLocals = startCollectLocals,
                   finishCollectLocals = finishCollectLocals,
                   warn = warn,
                   signal = signal,
                   leaf = collectUsageLeaf,
                   call = collectUsageCall,
                   handler = getCollectUsageHandler,
                   globalenv = env,
                   env = env,
                   name = NULL,
                   srcfile = NULL,
                   frow = NULL,
                   lrow = NULL,
                   isLocal = collectUsageIsLocal)
}

collectUsage <- function(fun, name = "<anonymous>", ...) {
    w <- makeUsageCollector(fun, ...)
    collectUsageFun(name, formals(fun), body(fun), w)
}
collectUsageLeaf  <- function(v, w) {
    if (typeof(v) == "symbol") {
        vn <- as.character(v)
        if (v == "...")
            w$signal("... may be used in an incorrect context", w)
        else if (isDDSym(v)) {
            if (w$isLocal("...", w))
                w$enterLocal("variable", "...", v, w)
            else
                w$signal(paste(v, "may be used in an incorrect context"), w)
        }
        else if (w$isLocal(vn, w))
            w$enterLocal("variable", vn, v, w)
        else if (! vn %in% c("*tmp*", "*tmpv*"))
            w$enterGlobal("variable", vn, v, w)
    }
}

collectUsageArgs <- function(e, w) {
    for (a in dropMissings(e[-1]))
        if (typeof(a) == "symbol" && a == "...") {
            if (w$isLocal("...", w))
                w$enterLocal("variable", "...", a, w)
            else
                w$signal(paste(a, "may be used in an incorrect context:",
                               pasteExpr(e)), w)
        }
        else walkCode(a, w)
}

collectUsageCall <- function(e, w) {
    if (typeof(e[[1]]) %in% c("symbol", "character")) {
        fn <- as.character(e[[1]])
        if (w$isLocal(fn, w))
            w$enterLocal("function", fn, e, w)
        else w$enterGlobal("function", fn, e, w)
    }
    else walkCode(e[[1]], w)
    collectUsageArgs(e, w)
}

collectUsageFun <- function(name, formals, body, w) {
    w$name <- c(w$name, name)
    parnames <- names(formals)
    locals <- findFuncLocals(formals, body)
    w$env <- new.env(hash = TRUE, parent = w$env)
    for (n in c(parnames, locals))
        assign(n, TRUE, w$env)
    w$startCollectLocals(parnames, locals, w)
    for (a in dropMissings(formals)) walkCode(a, w)
    walkCode(body, w)
    w$finishCollectLocals(w)
}

signalUsageIssue <- function(m, w) {
    if (!is.null(w$frow) && !is.na(w$frow)) {
        fname <- w$srcfile
        if (w$frow == w$lrow)
            loc <- paste(" (", fname, ":",  w$frow, ")", sep = "")
        else loc <- paste(" (", fname, ":", w$frow, "-", w$lrow, ")", sep = "")
    }
    else loc <- NULL

    w$warn(paste(paste(w$name, collapse = " : "), ": ", m, loc, "\n", sep = ""))
}

# **** is this the right handling of ..n things?
# **** signal (possible) error if used in wrong context?
# **** also need error for ... when not present
# **** maybe better done in leaf?
collectUsageIsLocal <- function(v, w) {
    if (isDDSym(v)) v <- "..."
    ! is.baseenv(findOwnerEnv(v, w$env, stop = w$globalenv,
                              default = .BaseEnv))
}

doNothing <- function(...) NULL

##
## Usage collectors for some standard functions
##

collectUsageHandlers <- mkHash()

# 'where' is ignored for now
addCollectUsageHandler <- function(v, where, fun)
    assign(v, fun, envir = collectUsageHandlers)

getCollectUsageHandler <- function(v, w)
    if (exists(v, envir = collectUsageHandlers, inherits = FALSE) &&
        (isBaseVar(v, w$env) ||
         isStatsVar(v, w$env) || isUtilsVar(v, w$env) || # **** for now
         v == "Quote" ))  # **** yet another glorious hack!!!
        get(v, envir = collectUsageHandlers)
##**** this is (yet another) temporary hack
isStatsVar <- function(v, env) {
    e <- findOwnerEnv(v, env)
    if (! identical(e, NA) &&
        exists(v, envir = e, inherits = FALSE, mode = "function")) {
        f <- get(v, envir = e, inherits = FALSE, mode = "function")
        identical(environment(f), getNamespace("stats"))
    }
    else FALSE
}
isUtilsVar <- function(v, env) {
    e <- findOwnerEnv(v, env)
    if (! identical(e, NA) &&
        exists(v, envir = e, inherits = FALSE, mode = "function")) {
        f <- get(v, envir = e, inherits = FALSE, mode = "function")
        identical(environment(f), getNamespace("utils"))
    }
    else FALSE
}

isSimpleFunDef <- function(e, w)
    typeof(e[[2]]) != "language" && typeof(e[[3]]) == "language" &&
    typeof(e[[3]][[1]]) %in% c("symbol", "character") &&
    e[[3]][[1]] == "function" && isBaseVar("function", w$env)

isClosureFunDef <- function(e, w)
    typeof(e[[2]]) != "language" && typeof(e[[3]]) == "closure"

checkDotsAssignVar <- function(v, w) {
    if (v == "...") {
        w$signal("... may be used in an incorrect context", w)
        FALSE
    }
    else if (isDDSym(v)) {
        w$signal(paste(v, "may be used in an incorrect context"), w)
        FALSE
    }
    else TRUE
}

#**** proceeds even if "..." or "..1", etc--is that right?
local({
    h <- function(e, w) {
        w$enterGlobal("function", as.character(e[[1]]), e, w)
        v <- getAssignedVar(e)
        checkDotsAssignVar(v, w)
        w$enterLocal("<-", v, e, w)
        if (isSimpleFunDef(e, w))
            collectUsageFun(v, e[[3]][[2]], e[[3]][[3]], w)
        else if (isClosureFunDef(e, w)) { ## to handle inlined S4 methods
            fun <- e[[3]]
            w$globalenv <- environment(fun)
            w$env = environment(fun)
            collectUsageFun(v, formals(fun), body(fun), w)
        }            
        else {
            if (typeof(e[[2]]) == "language") {
                fa <- flattenAssignment(e[[2]])
                for (a in fa)
                    for (b in a) walkCode(b, w)
            }
            walkCode(e[[3]], w)
        }
    }
    addCollectUsageHandler("<-", "base", h)
    addCollectUsageHandler("=", "base", h)
})

#**** would be better to use match.call in most of these
#**** proceeds even if "..." or "..1", etc--is that right?
addCollectUsageHandler("<<-", "base", function(e, w) {
    w$enterGlobal("function", "<<-", e, w)
    v <- getAssignedVar(e)
    checkDotsAssignVar(v, w)
    if (w$isLocal(v, w))
        w$enterLocal("<<-", v, e, w)
    else w$enterGlobal("<<-", v, e, w)
    if (typeof(e[[2]]) == "language") {
        fa <- flattenAssignment(e[[2]])
        for (a in fa)
            for (b in a) walkCode(b, w)
    }
    walkCode(e[[3]], w)
})

addCollectUsageHandler("for", "base", function(e, w) {
    w$enterGlobal("function", "for", e, w)
    v <- as.character(e[[2]])
    w$enterLocal("for", v, e, w)
    walkCode(e[[3]], w)
    walkCode(e[[4]], w)
})

addCollectUsageHandler("{", "base", function(e, w) {
    w$enterGlobal("function", "{", e, w)
    w$srcfile <- attr(e, "srcfile")$filename
    
    if (length(e)>1){
        for ( i in 2 : length(e)){      
            if ( !is.null(attr(e, "srcref")[[i]])){
                w$frow <- attr(e, "srcref")[[i]][[1]]
                w$lrow <- attr(e, "srcref")[[i]][[3]]
            }
            walkCode(e[[i]], w)
        }
    }
})

#**** is this the right way to handle :: and ::: ??
#**** maybe record package/name space?
local({
    h <- function(e, w)
        w$enterGlobal("function", as.character(e[[1]]), e, w)
    addCollectUsageHandler("~", "base", h)
    addCollectUsageHandler("quote", "base", h)
    addCollectUsageHandler("Quote", "methods", h)
    addCollectUsageHandler("expression", "base", h)
    addCollectUsageHandler("::", "base", h)
    addCollectUsageHandler(":::", "base", h)
})

#**** add counter to anonymous functions to distinguish??
addCollectUsageHandler("function", "base", function(e, w)
    collectUsageFun("<anonymous>", e[[2]], e[[3]], w))

addCollectUsageHandler("local", "base", function(e, w) {
    w$enterGlobal("function", "local", e, w)
    if (length(e) == 2)
        collectUsageFun("<local>", NULL, e[[2]], w)
    else collectUsageArgs(e, w)
})

addCollectUsageHandler("assign", "base", function(e, w) {
    w$enterGlobal("function", "assign", e, w)
    if (length(e) == 3 && is.character(e[[2]]) && length(e[[2]]) == 1) {
        w$enterLocal("<-", e[[2]], e, w)
        walkCode(e[[3]], w)
    }
    else collectUsageArgs(e, w)
})

addCollectUsageHandler("with", "base", function(e, w) {
    w$enterGlobal("function", "with", e, w)
    if (identical(w$skipWith, TRUE))
        walkCode(e[[2]], w)
    else collectUsageArgs(e, w)
})

local({
    h <- function(e, w) {
        w$enterGlobal("function", as.character(e[[1]]), e, w)
        walkCode(e[[2]], w)
    }
    addCollectUsageHandler("$", "base", h)
    addCollectUsageHandler("@", "base", h)
})

local({
    h <- function(e, w) {
        w$enterGlobal("function", as.character(e[[1]]), e, w)
        walkCode(e[[2]], w)
        walkCode(e[[4]], w)
    }
    addCollectUsageHandler("$<-", "base", h)
    addCollectUsageHandler("@<-", "base", h)
})

addCollectUsageHandler(".Internal", "base", function(e, w) {
    w$enterGlobal("function", ".Internal", e, w)
    if (length(e) != 2)
        w$signal(paste("wrong number of arguments to '.Internal':",
                       pasteExpr(e)), w)
    else if (typeof(e[[2]]) == "language") {
        w$enterInternal(e[[2]][[1]], e[[2]], w)
        collectUsageArgs(e[[2]], w)
    }
    else w$signal(paste("bad argument to '.Internal':", pasteExpr(e[[2]])), w)
})

addCollectUsageHandler("substitute", "base", function(e, w) {
    w$enterGlobal("function", "substitute", e, w)
    if (length(e) > 3)
        w$signal("wrong number of arguments to 'substitute'", w)
    if (length(e) == 3) {
        a <- e[[3]]
        if (! missing(a)) walkCode(a, w)
    }
})

addCollectUsageHandler("bquote", "base", function(e, w) {
    w$enterGlobal("function", "bquote", e, w)
    if (length(e) > 3)
        w$signal("wrong number of arguments to 'bquote'", w)
    if (length(e) == 3) {
        a <- e[[3]]
        if (! missing(a)) walkCode(a, w)
    }
})
addCollectUsageHandler("library", "base", function(e, w) {
    w$enterGlobal("function", "library", e, w)
    if (length(e) > 2)
        for(a in dropMissings(e[-(1:2)])) walkCode(a, w)
})
addCollectUsageHandler("require", "base", function(e, w) {
    w$enterGlobal("function", "require", e, w)
    if (length(e) > 2)
        for(a in dropMissings(e[-(1:2)])) walkCode(a, w)
})
addCollectUsageHandler("data", "utils", function(e, w) {
    w$enterGlobal("function", "data", e, w)
})
mkLinkHandler <- function(family, okLinks) {
    function(e, w) {
        w$enterGlobal("function", family, e, w)
        if (length(e) >= 2) {
            if (is.character(e[[2]])) {
                if (! (e[[2]] %in% okLinks))
                    w$signal(paste("link", sQuote(e[[2]]), "not available for",
                                   sQuote(family)), w)
            }
            else if (! is.name(e[[2]]) || ! as.character(e[[2]]) %in% okLinks)
                walkCode(e[[2]], w)
        }
    }
}
addCollectUsageHandler("detach", "base", function(e, w) {
    w$enterGlobal("function", "detach", e, w)
    if (length(e) > 2)
        for(a in dropMissings(e[-(1:2)])) walkCode(a, w)
})
addCollectUsageHandler("binomial", "stats",
                       mkLinkHandler("binomial",
                                     c("logit", "probit", "cloglog",
                                       "cauchit", "log")))
addCollectUsageHandler("gaussian", "stats",
                       mkLinkHandler("gaussian",
                                     c("inverse", "log", "identity")))
addCollectUsageHandler("Gamma", "stats",
                       mkLinkHandler("Gamma",
                                     c("inverse", "log", "identity")))
addCollectUsageHandler("poisson", "stats",
                       mkLinkHandler("poisson",
                                     c("log", "identity", "sqrt")))
addCollectUsageHandler("quasibinomial", "stats",
                       mkLinkHandler("quasibinomial",
                                     c("logit", "probit", "cloglog",
                                       "cauchit", "log")))
addCollectUsageHandler("quasipoisson", "stats",
                       mkLinkHandler("quasipoisson",
                                     c("log", "identity", "sqrt")))
addCollectUsageHandler("quasi", "stats", function(e, w) {
    w$enterGlobal("function", "quasi", e, w)
    # **** don't look at arguments for now.  Need to use match.call
    # **** to get this right and trap errors. Later ...
})

addCollectUsageHandler("if", "base", function(e, w) {
    w$enterGlobal("function", "if", e, w)
    test <- constantFoldEnv(e[[2]], w$env)
    if (is.logical(test) && length(test) == 1 && ! is.na(test)) {
        walkCode(e[[2]], w)
        if (test) walkCode(e[[3]], w)
        else if (length(e) > 3) walkCode(e[[4]], w)
    }
    else collectUsageArgs(e, w)
})

##
## Finding global variables
##

findGlobals <- function(fun, merge = TRUE) {
    vars <- mkHash()
    funs <- mkHash()
    enter  <- function(type, v, e, w)
        if (type == "function")
            assign(v, TRUE, funs)
        else assign(v, TRUE, vars)
    collectUsage(fun, enterGlobal = enter)
    fnames <- ls(funs, all.names = TRUE)
    vnames <- ls(vars, all.names = TRUE)
    if (merge)
        sort(unique(c(vnames, fnames)))
    else list(functions = fnames, variables = vnames)
}


##
## Checking function and variable usage
##

checkUsageStartLocals <- function(parnames, locals, w) {
    env <- w$env
    nplocals <- locals[! locals %in% parnames]
    attr(env, "checkUsageFrame") <- env # for sanity check
    mkentry <- function(parameter) {
        entry <- mkHash()
        assign("parameter", parameter, envir = entry)
        assign("assigns", 0, envir = entry)
        assign("varuses", 0, envir = entry)
        assign("funuses", 0, envir = entry)
        assign("funforms", NULL, envir = entry)
        assign("loopvars", 0, envir = entry)
        assign("srcinfo", NULL, envir = entry)
        entry
    }
    for (v in parnames) assign(v, mkentry(TRUE), envir = env)
    for (v in nplocals) assign(v, mkentry(FALSE), envir = env)
}

getLocalUsageEntry <- function(vn, w) {
    env <- findOwnerEnv(vn, w$env, stop = w$globalenv, default = .BaseEnv)
    if (is.baseenv(env)) stop("no local variable entry")
    if (! identical(env, attr(env, "checkUsageFrame")))
        stop("sanity check on local usage frame failed")
    entry <- get(vn, envir = env, inherits = FALSE)
    if (! is.environment(entry)) stop("bad local variable entry")
    entry
}

getLocalUsageValue <- function(vn, which, w)
    get(which, getLocalUsageEntry(vn, w), inherits = FALSE)

setLocalUsageValue <- function(vn, which, value, w)
    assign(which, value, envir = getLocalUsageEntry(vn, w))

incLocalUsageValue <- function(vn, which, w) {
    entry <- getLocalUsageEntry(vn, w)
    value <- get(which, entry, inherits = FALSE)
    assign(which, value + 1, entry)
}

incLocalSrcInfo <- function(vn, w) {
    entry <- getLocalUsageEntry(vn, w)
    value <- get("srcinfo", entry, inherits = FALSE)
    new <- list(srcfile = if (is.null(w$srcfile)) NA_character_ else w$srcfile,
                frow = if (is.null(w$frow)) NA_integer_ else w$frow,
                lrow = if (is.null(w$lrow)) NA_integer_ else w$lrow)
    new <- as.data.frame(new, stringsAsFactors = FALSE)
    if (is.null(value))
        value <- new
    else
        value <- rbind(value, new)
    assign("srcinfo", value, entry)
}

addLocalFunDef <- function(vn, e, w) {
    entry <- getLocalUsageEntry(vn, w)
    value <- get("funforms", entry, inherits = FALSE)
    assign("funforms", c(value, list(e[[3]][[2]])), entry)
}

checkUsageEnterLocal <- function(type, n, e, w) {
    if (type %in% c("<-", "<<-") && isSimpleFunDef(e, w))
        type <- "fundef"
    switch(type,
        "<-" =,
        "<<-" = incLocalUsageValue(n, "assigns", w),
        "variable" = incLocalUsageValue(n, "varuses", w),
        "function" = incLocalUsageValue(n, "funuses", w),
        "for" = incLocalUsageValue(n, "loopvars", w),
        "fundef" = addLocalFunDef(n, e, w))
    incLocalSrcInfo(n,w)
}

suppressVar <- function(n, suppress) {
    if (is.logical(suppress)) {
        if (suppress) TRUE else FALSE
    }
    else n %in% suppress
}

#**** need test code
#**** merge warnings?
checkUsageFinishLocals <- function(w) {
    vars <- ls(w$env, all.names = TRUE)
    for (v in vars) {
        if (! suppressVar(v, w$suppressLocal)) {
            parameter <- getLocalUsageValue(v, "parameter", w)
            assigns <- getLocalUsageValue(v, "assigns", w)
            varuses <- getLocalUsageValue(v, "varuses", w)
            funuses <- getLocalUsageValue(v, "funuses", w)
            loopvars <- getLocalUsageValue(v, "loopvars", w)
            funforms <- getLocalUsageValue(v, "funforms", w)
            uses <- max(varuses, funuses, loopvars)

            srcinfo <- getLocalUsageValue(v, "srcinfo", w)
            w$srcfile <- srcinfo[1,"srcfile"]
            w$frow <- srcinfo[1,"frow"]
            w$lrow <- srcinfo[1,"lrow"]

            if (parameter) {
                if (! suppressVar(v, w$suppressParamAssigns) && assigns > 0)
                    w$signal(paste("parameter", sQuote(v),
                                   "changed by assignment"), w)
                else if (! suppressVar(v, w$suppressParamUnused) &&
                         uses == 0 && v != "...")
                    w$signal(paste("parameter", sQuote(v), "may not be used"),
                             w)
            }
            else {
                if (uses == 0) {
                    if (! suppressVar(v, w$suppressLocalUnused))
                        w$signal(paste("local variable", sQuote(v),
                                       "assigned but may not be used"), w)
                }
                else if (funuses > 0 && is.null(funforms)) {
                    if (! suppressVar(v, w$suppressNoLocalFun))
                        w$signal(paste("local variable", sQuote(v),
                                       "used as function with no apparent",
                                       "local function definition"), w)
                }
            }
            if (! suppressVar(v, w$suppressFundefMismatch) &&
                length(funforms) > 1) {
                first <- funforms[[1]]
                nfirst <- names(first)
                for (d in funforms[-1])
                    if (! identical(first, d) ||
                        ! identical(nfirst, names(d))) {
                        w$signal(paste("multiple local function",
                                       "definitions for", sQuote(v),
                                       "with different formal arguments"), w)
                        break
                    }
            }
        }
    }
}

#**** warn if non-function used as variable (most likely get false positives)
#**** merge warnings?
checkUsageEnterGlobal <- function(type, n, e, w) {
    if (type == "function") {
        if (exists(n, envir = w$globalenv, mode = "function")) {
            # **** better call check here
            def <- get(n, envir = w$globalenv, mode = "function")
            if (typeof(def) == "closure")
                checkCall(def, e, function(m) w$signal(m, w))
            else {
                isBuiltin <- typeof(def) == "builtin"
                checkPrimopCall(n, e, isBuiltin, function(m) w$signal(m, w))
            }
        }
        else if (! suppressVar(n, w$suppressUndefined))
            w$signal(paste("no visible global function definition for",
                           sQuote(n)), w)
    }
    else if (type == "variable") {
        if (! exists(n, w$globalenv) && ! suppressVar(n, w$suppressUndefined))
            w$signal(paste("no visible binding for global variable",
                           sQuote(n)), w)
    }
    else if (type == "<<-") {
        if (! exists(n, w$globalenv))
            w$signal(paste("no visible binding for '<<-' assignment to",
                           sQuote(n)), w)
    }
}

dfltSuppressUndefined <- c(".Generic", ".Method", ".Class",
    ".split.valid.screens", ".split.cur.screen", ".split.saved.pars",
    ".split.screens", ".split.par.list", "last.dump")

#**** merge undefined variable warnings per top level function (at least)
#**** allow complete suppress or by name for all??
checkUsage <- function(fun, name = "<anonymous>",
                       report = cat,
                       all = FALSE,
                       suppressLocal = FALSE,
                       suppressParamAssigns = ! all,
                       suppressParamUnused = !all,
                       suppressFundefMismatch = FALSE,
                       suppressLocalUnused = FALSE,
                       suppressNoLocalFun = ! all,
                       skipWith = FALSE,
                       suppressUndefined = dfltSuppressUndefined,
                       suppressPartialMatchArgs = TRUE) {
    if (is.null(getOption("warnPartialMatchArgs")))
        options(warnPartialMatchArgs = FALSE)
    if (! suppressPartialMatchArgs) {
        oldOpts <- options(warnPartialMatchArgs = TRUE)
        on.exit(options(oldOpts))
    }
    tryCatch(collectUsage(fun, name = name,
                          warn = report,
                          suppressLocal = suppressLocal,
                          suppressParamAssigns = suppressParamAssigns,
                          suppressParamUnused = suppressParamUnused,
                          suppressFundefMismatch = suppressFundefMismatch,
                          suppressLocalUnused = suppressLocalUnused,
                          suppressNoLocalFun = suppressNoLocalFun,
                          skipWith = skipWith,
                          enterGlobal = checkUsageEnterGlobal,
                          enterLocal = checkUsageEnterLocal,
                          startCollectLocals = checkUsageStartLocals,
                          finishCollectLocals = checkUsageFinishLocals,
                          suppressUndefined = suppressUndefined,
                          suppressPartialMatchArgs = suppressPartialMatchArgs),
             error = function(e) {
                         report(paste0(name, ": Error while checking: ",
                                       conditionMessage(e), "\n"))
                     })
    invisible(NULL)         
}

checkUsageEnv <- function(env, ...) {
    for (n in ls(env, all.names=TRUE)) {
        v <- get(n, envir = env)
        if (typeof(v)=="closure")
            checkUsage(v, name = n, ...)
    }
}
checkUsagePackage <- function(pack, ...) {
    pname <- paste("package", pack, sep = ":")
    if (! pname %in% search())
        stop("package must be loaded")
    if (pack %in% loadedNamespaces())
        checkUsageEnv(getNamespace(pack), ...)
    else checkUsageEnv(as.environment(pname), ...)
}
#++++ check against internal arg count?

primopArgCounts <- mkHash()

anyMissing <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (missing(a)) return(TRUE) #**** better test?
    }
    return(FALSE)
}

noMissingAllowed <- c("c")

checkPrimopCall <- function(fn, e, isBuiltin, signal = warning0) {
    if (anyMissing(e[-1])) {
        if (isBuiltin || fn %in% noMissingAllowed)
            signal(paste("missing arguments not allowed in calls to",
                         sQuote(fn)))
    }                   
    if (exists(".GenericArgsEnv") && exists(fn, get(".GenericArgsEnv"))) {
        def <- get(fn, envir = get(".GenericArgsEnv"))
        checkCall(def, e, signal)
    }
    else if (exists(".ArgsEnv") && exists(fn, get(".ArgsEnv"))) {
        def <- get(fn, envir = get(".ArgsEnv"))
        checkCall(def, e, signal)
    }
    else if (exists(fn, envir = primopArgCounts, inherits = FALSE)) {
        argc <- get(fn, envir = primopArgCounts)
        if (! any(argc == (length(e) - 1))) {
            signal(paste("wrong number of arguments to", sQuote(fn)))
            FALSE
        }
        else TRUE
    }
    else TRUE
}

local({
    zeroArgPrims <- c("break", "browser", "gc.time", "globalenv",
                      "interactive", "nargs", "next", "proc.time")
    for (fn in zeroArgPrims) assign(fn, 0, envir = primopArgCounts)

    zeroOrOneArgPrims <- c("invisible")
    for (fn in zeroOrOneArgPrims) assign(fn, 0:1, envir = primopArgCounts)

    oneArgPrims <- c("!", "(", "abs", "sqrt", "cos", "sin", "tan", "acos",
                     "asin", "atan", "Re", "Im", "Mod", "Arg", "Conj",
                     "cosh", "sinh", "tanh", "acosh", "asinh", "atanh",
                     "sign", "length", "repeat", ".Primitive",
                     "class", "oldClass", "standardGeneric", "unclass",
                     "ceiling", "floor", "trunc",
                     "is.array", "is.atomic", "is.call", "is.character",
                     "is.complex", "is.double",
                     "is.environment", "is.expression", "is.finite",
                     "is.function", "is.infinite", "is.integer",
                     "is.language", "is.list", "is.loaded", "is.logical",
                     "is.matrix", "is.na", "is.name", "is.nan", "is.null",
                     "is.numeric", "is.object", "is.pairlist", "is.real",
                     "is.recursive", "is.single", "is.symbol",
                     "debug", "undebug", "as.character", "as.call",
                     "as.environment", "attributes", "cumsum", "cumprod",
                     "cummax", "cummin", "dim", "dimnames", "exp", "missing",
                     "pos.to.env", ".primTrace", ".primUntrace",
                     "symbol.C", "symbol.For")
    for (fn in oneArgPrims) assign(fn, 1, envir = primopArgCounts)

    oneOrTwoArgPrims <- c("+", "-")
    for (fn in oneOrTwoArgPrims) assign(fn, 1:2, envir = primopArgCounts)

    twoArgPrims <- c("*", "/", "%%", "^", "<", "<=", "==", ">", ">=",
                     "|", "||", ":", "!=", "&", "&&", "%/%", "%*%",
                     "while", "attr", "attributes<-", "class<-",
                     "oldClass<-", "dim<-", "dimnames<-", "environment<-",
                     "length<-", "reg.finalizer")
    for (fn in twoArgPrims) assign(fn, 2, envir = primopArgCounts)

    assign("on.exit", 0:2, primopArgCounts)
})

matchName <- function(name, list)
    if (match(as.character(name), list, 0)) TRUE else FALSE

findVar <- function(e, env) matchName(e, env)

      matchCall <- function(def, call, ...) {
    ## the ... machinations are needed to prevent match.call from signaling
    ## an error when the call contains a ... argument, and to work with
    ## versions of match.call that do or do not have the envir argument
    ## added for R 3.2.0
    fun <- function(...) 
        match.call(def, call, FALSE)
    fun()
}

checkCall <- function(def, call, signal = warning0) {
    testMatch <- function() 
        ## withCallingHandlers is used to capture partial argument
        ## matching warnings if enabled.
        withCallingHandlers(matchCall(def, call),
            warning = function(w) {
                msg <- conditionMessage(w)
                signal(paste("warning in ",
                             deparse(call, width.cutoff = 500),
                             ": ", msg, sep=""))
                invokeRestart("muffleWarning")
            })
    msg <- tryCatch({testMatch(); NULL},
                    error = function(e) conditionMessage(e))
    if (! is.null(msg)) {
        emsg <- paste("possible error in ", deparse(call, width.cutoff = 500),
                      ": ", msg, sep="")
        if (! is.null(signal)) signal(emsg)
        FALSE
    }
    else TRUE
}

##
## Various utilities
##

warning0 <- function(msg) warning(msg, call.=FALSE)
stop0 <- function(msg) stop(msg, call.=FALSE)
pasteExpr <- function(e, prefix = "\n    ") {
    de <- deparse(e)
    if (length(de) == 1) sQuote(de)
    else paste(prefix, deparse(e), collapse="")
}
dotsOrMissing <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (missing(a)) return(TRUE) #**** better test?
        if (typeof(a) == "symbol" && a == "...") return(TRUE)
    }
    return(FALSE)
}

anyDots <- function(args) {
    for (i in 1:length(args)) {
        a <-args[[i]]
        if (! missing(a) && typeof(a) == "symbol" && a == "...")
            return(TRUE)
    }
    return(FALSE)
}

isDDSym <- function(name) {
    (is.symbol(name) || is.character(name)) &&
    length(grep("^\\.\\.[[:digit:]]+$", as.character(name))) != 0
}
