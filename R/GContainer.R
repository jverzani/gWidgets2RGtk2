##' @include GComponent.R
NULL

##' GContainer is the base class for container objects. The main
##' method is \code{add_child}, but there is also book-keepingn code
##' to keep track of the child components of the container
##' @rdname gWidgets2RGtk2-package
GContainer <- setRefClass("GContainer",
                          contains="GComponentObservable",
                          fields=list(
                            children="list"
                            ),
                          methods=list(
                            add_child = function(child, expand, fill, anchor, ...) {
                              "Add child to parent, do internal book keeping"
                            },
                            child_bookkeeping=function(child) {
                              "Update parent property of child and children property of parent container"
                              if(is(child, "GComponent"))
                                child$set_parent(.self)
                              children <<- c(children, child)
                            },
                            set_child_align=function(child, alt_child, anchor) {
                              "Set child alignment, if a GtkMisc or GtkAlignment object"
                              ## xalign : the horizontal position of the child, 0.0 is left aligned, 1.0 is right aligned
                              ## yalign : the vertical position of the child, 0.0 is top aligned, 1.0 is bottom aligned
                              ## depends on many things.
                              is_candidate <- function(child) is(child,"GtkMisc") || is(child,"GtkAlignment")
                              if(!(is_candidate(child)))
                                child <- alt_child
                              if(is_candidate(child)) {
                                child['xalign'] <- anchor[1]
                                child['yalign'] <- anchor[2]
                              } 
                            },
                            set_child_fill=function(child, fill, horizontal=TRUE) {
                              "Fill can be NULL, TRUE, FALSE, '', 'both', 'x', 'y'..."
                              if(is.null(fill))
                                fill <- FALSE
                              if(is.logical(fill))
                                return(fill)

                              if(is(child, "GtkAlignment")) {
                                if(fill == "both" || fill == "x") {
                                  child['xscale'] <- 1
                                }
                                if(fill == "both" || fill == "y") {
                                  child['yscale'] <- 1
                                }
                                fill <- TRUE
                              }

                              ## else, turn character into logical
                              if(is.character(fill)) {
                                if(fill == "both")
                                  fill <- TRUE
                                else if(fill == "x" && horizontal)
                                  fill <- TRUE
                                else if(fill == "y" && !horizontal)
                                  fill <- TRUE
                                else
                                  fill <- FALSE
                              }
                              return(fill)
                            }
                          ))

                              
