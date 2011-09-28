##' @include misc.R
NULL

##' method for stopping getWidget
##'
##' @export
##' @rdname gWidgetsRGtk2-undocumented
getWidget.RGtkObject <- function(obj) obj

##' S3 method for stopping getBlock
##'
##' @export
##' @rdname gWidgetsRGtk2-undocumented
getBlock.RGtkObject <- function(obj) obj




## set alignment
#Sets the alignment of the child. This property has no effect unless the child is a GtkMisc or a GtkAligment.
# xalign : the horizontal position of the child, 0.0 is left aligned, 1.0 is right aligned
# yalign : the vertical position of the child, 0.0 is top aligned, 1.0 is bottom aligned

setXYalign <- function(child, childWidget, anchor) {
  if(is(child,"GtkMisc") || is(child,"GtkAlignment")) {
    child['xalign'] <- anchor[1]
    child['yalign'] <- anchor[2]
  } else if(!is.null(childWidget)) {
    if(is(childWidget,"GtkMisc") || is(childWidget,"GtkAlignment")) {
      childWidget['xalign'] <- anchor[1]
      childWidget['yalign'] <- anchor[2]
    }
  }
}



##' Font method for gtk object
##'
##' @export
##' @rdname font
"font<-.RGtkObject" <- function(obj, value) {
  ## set fonts from value
  ## value might be a vector, we use a list -- from .fixFontMessUp
  if(!is.list(value)) 
    value <- sapply(value, identical, simplify=FALSE)
    
  string <- ""

                   
  ## do family, weight, style
  for(i in c("family", "weight", "style")) {
    if(!is.null(value[[i]])) {
      x <- .font.styles[[i]]
      ind <- charmatch(value[[i]], x)
      if(!is.na(ind)) {
        string <- paste(string, x[ind[1]], sep=" ")
        if(i == "family")
          string <- paste(string,",", sep="")
      }
    }
  }
  
  ## size can be integer or name -- relative to 12pt
                   
  if(!is.null(value$size)) {
    ## is it numeric or character?
    warn <- getOption("warn"); options(warn=2) # hack to avoid warning -- we want an error here
    out <- try(as.integer(value[['size']]), silent=TRUE)
    options(warn=warn)
    if(!inherits(out, "try-error"))
      string <- Paste(string," ",out)
    else if (!is.na(ind <- charmatch(value[['size']], names(fontSizes)))) # fuzzy match?
      string <- Paste(string, " ", paste(ceiling(12*fontSizes[ind[1]]),"px", sep=""))
  }
  string <- gsub(",$","",string) # strip , if present
  
  if(string != "") {
    fontDescr = pangoFontDescriptionFromString(string)
    obj$ModifyFont(fontDescr)
  }
  
  ## colors
  if(!is.null(value$color))
    obj$modifyFg(GtkStateType[1], value[['color']])
  
}

