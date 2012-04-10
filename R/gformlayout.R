##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::gformlayout
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gformlayout guiWidgetsToolkitRGtk2
##' @S3method .gformlayout guiWidgetsToolkitRGtk2
.gformlayout.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                             align="left",
                                             spacing=5,
                                             container = NULL, ... ) {
  GFormLayout$new(toolkit,
             align,
             spacing,
             container=container ,...)
}


## a form layout -- really just a table
GFormLayout <- setRefClass("GFormLayout",
                           contains="GContainer",
                           fields=list(
                             align="character",
                             spacing="numeric",
                             lyt="ANY"
                             ),
                           methods=list(
                             initialize=function(toolkit=NULL,
                               align="left", spacing=5,
                               container=NULL,
                               ...) {
                               
                               initFields(align=align,
                                          spacing=spacing
                                          )
                               make_widget()
                               
                               add_to_parent(container, .self)
                               callSuper(toolkit, ...)
                             },
                             make_widget=function() {
                               widget <<- gtkTableNew(homogeneous=FALSE)
                               widget$setColSpacings(spacing)
                               block <<- widget
                             },
                             finalize=function() {
                               ## some cases one needs to call finalize to write table (gWidgetsWWW2)
                             },
                             add_child=function(child, label="", ...) {
                               add_row(label, child, ...)
                             },
                             add_row=function(label, child, ...) {
                               "Add a row at end"
                               row <- no_rows()
                               
                               child_widget <- getBlock(child)

                               ## implement alignment and 
                               ifelse(align == "left", "", "") # fill style

                               opts <- c("fill", "expand", "shrink")
                               widget$attach(gtkLabelNew(label),
                                             0, 1, row, row + 1,
                                             xoptions=opts, yoptions="fill"
                                             )
                               widget$attach(child_widget,
                                             1, 2, row, row + 1,
                                             xoptions=opts, yoptions="fill"
                                             )
                               ## bookkeeping
                               if(is(child, "GComponent"))
                                 child$set_parent(.self)

                               nms <- names(children)
                               children <<- c(children, child)
                               names(children) <<- c(nms, label)
                             },
                             get_value=function(...) sapply(children, svalue, simplify=FALSE),
                             no_rows=function() widget$getNrows()
                             ))
                             
