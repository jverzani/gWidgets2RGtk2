##' @include GWidget.R
NULL

## sapcing is in need of fixing

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
                               set_spacings(spacing)
                               block <<- widget
                             },
                             finalize=function() {
                               ## some cases one needs to call finalize to write table (gWidgetsWWW2)
                             },
                             set_spacings=function(row, col=row) {
                               "Method to adjust row and column space"
                               widget$setRowSpacings(row)
                               widget$setColSpacings(col)
                             },
                             add_child=function(child,  expand=NULL, fill=NULL, anchor=NULL, ..., label="") {
                               add_row(label, child, expand, fill, anchor, ...)
                             },
                             add_row=function(label, child, expand=NULL, fill=NULL, anchor=NULL, ...) {
                               "Add a row at end"
                               row <- no_rows()
                               
                               child_widget <- getBlock(child)

                               ## implement alignment and
                               align_amt <- switch(align,
                                                   "default"=1,
                                                   "right"=1,
                                                   "left"=0,
                                                   1)
                             
                               #print(list(expand, fill, anchor))
                               
                               xopts <- yopts <- c("shrink")
                               if(is.null(expand) || expand)
                                 xopts <- yopts <- c("expand", "shrink")
                               
                               if(is.null(fill)) fill <- TRUE
                               if(is.logical(fill) && fill)
                                 xopts <- yopts <- c("expand", "shrink", "fill")
                               if(is.character(fill)) {
                                 if(fill == "both") {
                                   xopts <- yopts <- c("expand", "shrink", "fill")
                                 } else if(fill == "x") {
                                   xopts <- c("expand", "shrink", "fill")
                                 } else if(fill == "y") {
                                   yopts <- c("expand", "shrink", "fill")
                                 }
                               }

                               ## 0,0 = top, 0.5 center. We center, as
                               ## otherwise widgets in boxes look
                               ## funny, as the boxes have some
                               ## padding.
                               
                               l <- gtkLabelNew(label); l$setAlignment(align_amt, 0.5)
                               widget$attach(l,
                                             0, 1, row, row + 1,
                                             xoptions=c("expand", "fill"),
                                             yoptions="fill"
                                             )

                               ## child alignment ...
                               if(!is.na(match("xalign", names(child_widget))))
                                 child_widget['xalign'] <- ifelse(is.null(anchor),
                                                                  0,
                                                                  (1 + anchor[1])/1)

                               widget$attach(child_widget,
                                             1, 2, row, row + 1,
                                             xoptions=xopts, yoptions=yopts
                                             )
                               ## bookkeeping
                               if(is(child, "GComponent"))
                                 child$set_parent(.self)

                               nms <- names(children)
                               children <<- c(children, child)
                               names(children) <<- c(nms, label)
                             },
                             get_value=function(...) sapply(children, svalue, simplify=FALSE),
                              set_value=function(value, ...) {
                               "value a named list matching children"
                               value <- as.list(value)
                               nms <- Filter(function(i) !is.na(match(i, names(children))), names(value))
                               sapply(nms, function(nm) {
                                 obj <- children[[nm]]
                                 svalue(obj) <- value[[nm]]
                               })
                             },
                             no_rows=function() widget$getNrows(),
                             ## Hacky way to set fonts
                             get_labels=function() {
                               children <- Map(function(x) x$getWidget(), widget$getChildren())
                               labels <- Filter(function(x) is(x, "GtkLabel"), children)
                               names(labels) <- sapply(labels, function(x) x$getText())
                               labels
                             },
                             set_font=function(label_value, value) {
                               "set font for a label which is specified by its value"
                               labels <- get_labels()
                               set_rgtk2_font(labels[[label_value]], value)
                             }
                             ))
                             
