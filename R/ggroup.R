##' @include GContainer.R
NULL

##' toolkit constructor for ggroup
##'
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.ggroup.guiWidgetsToolkitRGtk2 <- function(toolkit, horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ...) {
  GGroup$new(toolkit, horizontal, spacing=spacing, use.scrollwindow=use.scrollwindow, container, ...)
}

## TODO XXX raise on drag motion

##' base class for box containers. 
GGroup <- setRefClass("GGroup",
                      contains="GContainer",
                      
                      
                      fields=list(
                        horizontal="logical"
                        ),
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

                        ## Make a widget, for subclassing
                        make_widget = function(use.scrollwindow, spacing) {
                               if(horizontal)
                                 widget <<- gtkHBox(homogeneous=FALSE, spacing=spacing)
                               else
                                 widget <<- gtkVBox(homogeneous=FALSE, spacing=spacing)
                               widget$SetBorderWidth(0L)
                               
                               if(use.scrollwindow) {
                                 block <<- gtkScrolledWindowNew()
                                 block$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                                 block$AddWithViewport(widget)
                               } else {
                                 block <<- widget
                               }
                             },

                        
                        ## Main add method
                        add_child = function(child, expand, fill, anchor, ...) {
                          "Add child to box container. Child can be RGtk2object or GComponent. We use expand=TRUE, fill=TRUE as a default for containers, and expand=FALSE, fill=FALSE, as the default for widgets. These will usually need tweeking"
                          toolkit_child <- getBlock(child)
                          
                          theArgs <- list(...) ## padding (around each)

                          ## get expand, anchor, fill
                          expand <- getWithDefault(expand, getWithDefault(default_expand, FALSE))
                          fill <- getWithDefault(fill, getWithDefault(default_fill, FALSE))
                          if(!is.null(theArgs$align))
                            theArgs$anchor <- theArgs$align
                          anchor <- getWithDefault(anchor, NULL)
                          
                          
                          if(!is.null(anchor)) {       # put in [0,1]^2
                            anchor <- (anchor+1)/2      # [0,1]
                            anchor[2] <- 1 - anchor[2]     # flip yalign
                          }

                          ## need to map values of expand, fill, anchor into values for
                          ## expand, fill and alignment, 
                          ## We do things differently if there is a gtkAlignment for a block
                          if(is(toolkit_child, "GtkAlignment")) {
                            if(expand && (fill =="both" || fill == "x")) {
                              toolkit_child['xscale'] <- 1
                            }
                            if(expand && (fill == "both" || fill == "y")) {
                              toolkit_child['yscale'] <- 1
                            }
                            
                            if(expand && fill == "") {
                              toolkit_child['xscale'] <- toolkit_child['yscale'] <- 1
                            }
                            
                            
                            if(!is.null(anchor)) {
                              toolkit_child['xalign'] <- anchor[1]
                              toolkit_child['yalign'] <- anchor[2]
                            }
                            fill <- TRUE
                            padding <- 0
                          } else {
                            ## anchor argument
                            if(!is.null(anchor))
                              setXYalign(toolkit_child, getWidget(child), anchor)
                            
                            ## padding
                            padding <- getWithDefault(theArgs$padding, 0L)
                            
                            if(!is.null(fill)) {
                              if(fill == "both") {
                                fill <- TRUE
                              } else {
                                if(fill == "x" && horizontal)
                                  fill <- TRUE
                                     else if(fill == "y" && !horizontal)
                                       fill <- TRUE
                              }
                            }
                          }
                          ## all done
                          widget$packStart(toolkit_child, expand=expand, fill=fill, padding=padding)
                          child_bookkeeping(child)
                        },


                        ## Remove a child from list. Can be added back in, if not garbage collected
                        remove_child = function(child) {
                          "remove child from box container"
                          children <<- Filter(function(x) !identical(x, child), children)
                          child$set_parent(NULL)
                          widget$remove(getBlock(child))
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

                        ## svalue (borderwidth, spacing -- which is it...)
                        get_value=function() {
                          widget$getBorderWidth()
                        },
                        set_value=function(value) {
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

                              
