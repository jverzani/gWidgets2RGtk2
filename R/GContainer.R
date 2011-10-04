##' @include GComponent.R
NULL

##' Base class for container objects
GContainer <- setRefClass("GContainer",
                          contains="GComponent",
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
                            }
                          ))

                              
