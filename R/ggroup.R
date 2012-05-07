##' @include GContainer.R
NULL

##' toolkit constructor for ggroup
##'
##' @inheritParams gWidgets2::ggroup
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .ggroup guiWidgetsToolkitRGtk2
##' @S3method .ggroup guiWidgetsToolkitRGtk2
.ggroup.guiWidgetsToolkitRGtk2 <- function(toolkit, horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ...) {
  GGroup$new(toolkit, horizontal, spacing=spacing, use.scrollwindow=use.scrollwindow, container, ...)
}

## TODO XXX raise on drag motion

GGroupBase <- setRefClass("GGroupBase",
                      contains="GContainer",
                      fields=list(
                        horizontal="logical"
                        ),
                          ## Make a widget, for subclassing

                          methods=list(
                            make_widget=function(...) {},
                        
                        ## Main add method
                        add_child = function(child, expand, fill, anchor, ...) {
                        "Add child to box container. Child can be RGtk2object or GComponent. We use expand=TRUE, fill=TRUE as a default for containers, and expand=FALSE, fill=FALSE, as the default for widgets. These will usually need tweeking. The properties default_expand and default_fill allow for this."
                        toolkit_child <- getBlock(child)
                        
                        theArgs <- list(...) ## padding (around each)
                        
                        ## anchor puts in one of 9 spots
                        ## if expand=FALSE, fill meaningless
                        ## if expand=TRUE, then
                        ## * anchor=NULL, fill=[TRUE (both), "x", or "y"] to fill in dirction x, y or both
                        
                        
                        ## get expand, anchor, fill
                        expand <- getWithDefault(expand, getWithDefault(child$default_expand, FALSE))

                        ## anchor
                        if(!is.null(theArgs$align))
                          theArgs$anchor <- theArgs$align
                        anchor <- getWithDefault(anchor, NULL)
                        
                        if(!is.null(anchor)) {       # put in [0,1]^2
                          anchor <- (anchor+1)/2      # [0,1]
                          anchor[2] <- 1 - anchor[2]     # flip yalign
                          set_child_align(toolkit_child, getBlock(child), anchor)
                        }

                        
                        ## fill one of NULL, TRUE, FALSE, "", both, "x", "y"
                        if(expand) {
                          fill <- getWithDefault(fill, getWithDefault(child$default_fill, ifelse(is.null(anchor),"both", "")))
                          fill <- set_child_fill(toolkit_child, fill, horizontal)
                        }

                        padding <- getWithDefault(theArgs$padding, 0L)
                        
                        ## all done
                        widget$packStart(toolkit_child, expand=expand, fill=fill, padding=padding)
                        child_bookkeeping(child)
                      },
                        
                        
                        ## Remove a child from list. Can be added back in, if not garbage collected
                        remove_child = function(child) {
                          "remove child from box container"
                          children <<- Filter(function(x) !identical(x, child), children) # remove from list
                          child$set_parent(NULL) # adjust child widget property
                          widget$remove(getBlock(child)) # GUI removal happens last
                        },
                        add_spring=function() {
                          widget$PackStart(gtkHBoxNew(),TRUE,TRUE,0)
                        },
                        add_space=function(value) {
                          box <- gtkHBox()
                          if(horizontal)
                            box$setSizeRequest(value, -1L)
                          else
                            box$setSizeRequest(-1L, value)
                          widget$PackStart(gtkHBoxNew(),FALSE, FALSE,0)
                        },
                        ## [ for returning children
                        get_items = function(i, j, ..., drop=TRUE) {
                          "Return children"
                          out <- children[i]
                          if(drop && length(out) == 1)
                            out[[1]]
                          else
                            out
                        },

                        ## svalue spacing (not borderWidth
                        get_value=function(...) {
                          widget$getSpacing()
                        },
                            set_value=function(value, ...) {
                              widget$setSpacing(as.numeric(value)[1])
                            },
                            set_borderwidth=function(value, ...) {
                              "Sets borderwidth -- space around frame of container. not spacing between children"
                              widget$setBorderWidth(as.numeric(value)[1])
                            },


                        ## size
                        get_size=function() {
                          getBlock(widget)$sizeRequest()
                        },
                        set_size=function(value) {
                          tmp <- getBlock(widget) # size of block, if scrolled window
                          value <- as.integer(value)
                          tmp$setSizeRequest(value[1], value[2])
                        }


                        
                        ))

                              

## base class for box containers. 
GGroup <- setRefClass("GGroup",
                      contains="GGroupBase",
                      methods=list(
                        ## main intialize method
                        initialize=function(toolkit=NULL,
                          horizontal=TRUE, spacing=5,
                          use.scrollwindow=FALSE,
                          container=NULL, ...) {
                          
                          horizontal <<- horizontal
                          ## To be able to subclass we define widget in separate method
                          if(is(widget, "uninitializedField")) 
                            make_widget(use.scrollwindow, spacing)
                          
                          add_to_parent(container, .self, ...)
                          
                          callSuper(toolkit)
                        },
                        make_widget = function(use.scrollwindow, spacing) {
                               if(horizontal)
                                 widget <<- gtkHBox(homogeneous=FALSE, spacing=spacing)
                               else
                                 widget <<- gtkVBox(homogeneous=FALSE, spacing=spacing)

                               set_value(spacing)
                               
                               if(use.scrollwindow) {
                                 block <<- gtkScrolledWindowNew()
                                 block$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                                 block$AddWithViewport(widget)
                               } else {
                                 block <<- widget
                               }
                             }            
                        ))
