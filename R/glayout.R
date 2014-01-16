##' @include GContainer.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::glayout
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .glayout guiWidgetsToolkitRGtk2
##' @S3method .glayout guiWidgetsToolkitRGtk2
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
                           ## make matrix, then extract
                           d <- get_dim()
                           m <- matrix(nrow=d[1], ncol=d[2])
                           for(index in seq_along(child_positions)) {
                             item <- child_positions[[index]] 
                             for(ii in item$x)
                               for(jj in item$y) {
                                 m[ii,jj] <- index
                               }
                           }
                           widgets <- sapply(as.vector(m), function(ii) {
                             if(is.na(ii))
                               NA
                             else
                               child_positions[[ii]]$child
                           })
                           widgets <- matrix(widgets, ncol=d[2])
                           out <- widgets[i,j, drop=drop]
                           if(length(out) == 1 && drop)
                             out <- out[[1]]
                           out
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

                           if(expand) {
                             set_child_align(child, getWidget(value), anchor)
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
                           } 
                           
                           ## resize table widget if needed
                           d <- get_dim()
                           nr <- max(i); nc <- max(j)
                           if( nr > d[1] || nc > d[2])
                             widget$Resize(max(max(i), nr), max(max(j), nc))

                           ## fill options
                           xopts <- yopts <- "fill"
                           if(expand) {
                             if(is.null(fill) ||
                                (is.character(fill) && fill == "both")
                               ) {
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
                         },
                         remove_child=function(child) {
                           if(!is(child, "GComponent"))
                             return()
                           ## we call destroy method on child -- not being reused
                           ## remove from child_positions
                           child_positions <<- Filter(Negate(function(i) {
                             i$child$widget == child$widget
                           }), child_positions)
                           children <<- Filter(function(i) !identical(i, child), children)
                           getBlock(child)$destroy()
                         }
                         ))

