##' @include misc.R
NULL

##' method for stopping getWidget
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method getWidget RGtkObject
## @export getWidget RGtkObject
getWidget.RGtkObject <- function(obj) obj

##' S3 method for stopping getBlock
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method getBlock RGtkObject
## @export getBlock RGtkObject
getBlock.RGtkObject <- function(obj) obj




##  Font method for gtk object
## 
##  @export
##  @rdname font
## "font<-.RGtkObject" <- function(obj, value) {
##   ## set fonts from value
##   ## value might be a vector, we use a list -- from .fixFontMessUp
##   if(!is.list(value)) 
##     value <- sapply(value, identical, simplify=FALSE)
    
##   string <- ""

                   
##   ## do family, weight, style
##   for(i in c("family", "weight", "style")) {
##     if(!is.null(value[[i]])) {
##       x <- .font.styles[[i]]
##       ind <- charmatch(value[[i]], x)
##       if(!is.na(ind)) {
##         string <- paste(string, x[ind[1]], sep=" ")
##         if(i == "family")
##           string <- paste(string,",", sep="")
##       }
##     }
##   }
  
##   ## size can be integer or name -- relative to 12pt
                   
##   if(!is.null(value$size)) {
##     ## is it numeric or character?
##     warn <- getOption("warn"); options(warn=2) # hack to avoid warning -- we want an error here
##     out <- try(as.integer(value[['size']]), silent=TRUE)
##     options(warn=warn)
##     if(!inherits(out, "try-error"))
##       string <- Paste(string," ",out)
##     else if (!is.na(ind <- charmatch(value[['size']], names(fontSizes)))) # fuzzy match?
##       string <- Paste(string, " ", paste(ceiling(12*fontSizes[ind[1]]),"px", sep=""))
##   }
##   string <- gsub(",$","",string) # strip , if present
  
##   if(string != "") {
##     fontDescr = pangoFontDescriptionFromString(string)
##     obj$ModifyFont(fontDescr)
##   }
  
##   ## colors
##   if(!is.null(value$color))
##     obj$modifyFg(GtkStateType[1], value[['color']])
  
## }




## mouse click processing

##' Return TRUE if first mouse click
##'
##' To be called from key-press|release-event
##' @param e event for mouse press
##' @return TRUE or FALSE
isFirstMouseClick <- function(e) {
  if(!is(e, "GdkEvent"))
    stop("Must pass in an event")
  e$getButton() == 1
}

##' Return TRUE/FALSE if right mouse click
##'
##' To be called from key-press|release-event
##' @param e event for mouse press
##' @return TRUE or FALSE
isRightMouseClick <- function(e) {
  if(!is(e, "GdkEvent"))
    stop("Must pass in an event")
  
  e$GetButton() == 3 ||
  (is_MacOSX() && e$GetState() == GdkModifierType['control-mask'] && e$GetButton() == 1) 
}


## for drag and drop we define some global values

## parameters
TARGET.TYPE.TEXT   <- 80L                # our enumeration
TARGET.TYPE.OBJECT <- 81L                  
widgetTargetTypes <- 
  list(text = gtkTargetEntry("text/plain", 0, 
         TARGET.TYPE.TEXT),
       object = gtkTargetEntry("text/plain", 0, 
         TARGET.TYPE.OBJECT))

## an environment to store objects and times when dragging and dropping
.dnd.env <- new.env()
.dnd.env[['last_time']] <- 0

###
##################################################
## Map between R class an dGObject class
## right way, but was not working...
## RtoGObjectConversion <- function(x) UseMethod("RtoGObjectConversion")
## RtoGObjectConversion.default <- function(x) "gchararray"
## RtoGObjectConversion.factor <- function(x) "gchararray"
## RtoGObjectConversion.integer <- function(x) "gint"
## RtoGObjectConversion.numeric <- function(x) "double"
## RtoGObjectConversion.RGtkObject <- function(x) "GObject"
## RtoGObjectConversion.logical <- function(x) "gboolean"

RtoGObjectConversion <- function(x) {
  if(is(x, "factor"))
    "gchararray"
  else if(is(x, "integer"))
    "gint"
  else if(is(x, "numeric"))
    "gdouble"
  else if(is(x, "logical"))
    "gboolean"
  else
    "gchararray"
}




