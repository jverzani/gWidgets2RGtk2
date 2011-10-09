##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.glayout.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                             homogeneous = FALSE, spacing = 10,
                                             container = NULL, ... ) {
  GLayout$new(toolkit=toolkit, homogeneous=homogeneous, spacing=spacing, container = container, ...)
}


## layout class
GLayout <- setRefClass("GLayout",
                       contains="GContainer",
                       fields=list(
                         child_positions="list"
                         ),
                       methods=list(
                         initialize=function(toolkit=NULL,
                           homogeneous = FALSE, spacing = 10,
                           container = NULL, ... 
                           ) {
                           
                           widget <<- gtkTableNew(homogeneous = homogeneous)
                           ## homogeneous spacing
                           widget$SetRowSpacings(spacing)
                           widget$SetColSpacings(spacing)
                           
                           initFields(block=widget,
                                      child_positions=list()
                                      )
                           
                           add_to_parent(container, .self, ...)

                           callSuper(toolkit)
                         },
                         get_dim=function(...) {
                           "current size of table"
                           c(nrow=widget$getNrows(), ncol=widget$getNcols())
                         },
                         get_items = function(i, j, ..., drop=TRUE) {
                           ind <- sapply(child_positions, function(comp) {
                             i[1] %in% comp$x && j[1] %in% comp$y
                           })
                           if(any(ind))
                             return(child_positions[ind][[1]]$child) # first
                           else
                             NA
                         },
                         set_items = function(value, i, j, expand=FALSE, fill=FALSE, anchor=NULL) {
                           "Main method to add children"

                           if(missing(j)) {
                             cat(gettext("glayout: [ needs to have a column specified."))
                             return()
                           }

                           if(missing(i))
                             i <- get_dim()[1] + 1
                           
                           if(is.character(value)) {
                             value <- glabel(value, toolkit=toolkit)
                           }

                           expand <- getWithDefault(expand, getWithDefault(child$default_expand, FALSE))
                           fill <- getWithDefault(fill, getWithDefault(child$default_fill, FALSE))
                           
                           ## widgets
                           child <- getBlock(value)
                           
                           
                           if(!is.null(anchor)) {       # put in [0,1]^2
                             anchor <- (anchor+1)/2      # [0,1]
                             anchor[2] <- 1 - anchor[2]     # flip yalign
                           }
                        
                           
                           ## we do things differently if there is a gtkAlignment for a block
                           if(is(child, "GtkAlignment")) {
                             if(expand && (fill == TRUE || fill =="both" || fill == "x")) {
                               child['xscale'] <- 1
                             }

                             if(expand && (fill== TRUE || fill == "both" || fill == "y")) {
                               child['yscale'] <- 1
                             }
                             
                             if(expand && fill == "") {
                               child['xscale'] <- child['yscale'] <- 1
                             }
                             
                             if(!is.null(anchor)) {
                               child['xalign'] <- anchor[1]
                               child['yalign'] <- anchor[2]
                             }
                           } else {
                             ## in gtkstuff 
                             setXYalign(child, getBlock(value), anchor)
                           }
                           
                           ## resize table widget if needed
                           d <- get_dim()
                           nr <- max(i); nc <- max(j)
                           if( nr > d[1] || nc > d[2])
                             widget$Resize(max(max(i), nr), max(max(j), nc))

                           xopts <- yopts <- "fill"
                           
                           if(expand) {
                             if(is.null(fill) || (is.logical(fill) && fill) || (is.character(fill) && fill = "both")) {
                               xopts <- yopts <- c("fill","expand","shrink")
                             } else if(is.character(fill) && fill == "x") {
                               xopts <-  c("fill","expand","shrink")
                             } else if(is.character(fill) && fill == "y") {
                               yopts <-  c("fill","expand","shrink")
                             }
                           }  
                           
                           widget$Attach(child,
                                         min(j)-1, max(j), min(i)-1, max(i),
                                         xoptions=xopts, yoptions=yopts)

                             
                           ## Internal bookkeeping, add to lists
                           if(is(value, "GComponent"))
                             value$set_parent(.self)
                           children <<- c(children, value)
                           ## store for [ method
                           l <- child_positions
                           l[[as.character(length(l) + 1)]] <- list(x=i, y=j, child=value)
                           child_positions <<- l
                         }
                         ))

