##' @include ggroup.R
NULL

##' gframe constructor
##'
##' @inheritParams gWidgets2::gframe
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gframe guiWidgetsToolkitRGtk2
##' @S3method .gframe guiWidgetsToolkitRGtk2
.gframe.guiWidgetsToolkitRGtk2 <- function(toolkit, text, markup, pos, horizontal=TRUE, container=NULL, ...) {
  GFrame$new(toolkit, text, markup, pos, horizontal, container, ...)
}

## base class for gframe
GFrame <- setRefClass("GFrame",
                      contains="GGroup",
                      fields=list(
                        markup="logical"
                        ),
                      methods=list(
                        initialize=function(toolkit=NULL, text="", markup=FALSE, pos=3, horizontal=TRUE, container=NULL, ...) {

                          horizontal <<- horizontal

                          if(is(widget, "uninitializedField"))
                            make_widget(text, markup, pos)
                          
                          add_to_parent(container, .self, ...)
                          
                          callSuper(toolkit, horizontal=horizontal, ...)
                        },
                        make_widget = function(text, markup, pos) {
                          if(horizontal)
                            widget <<- gtkHBox()
                          else
                            widget <<- gtkVBox()

                          markup <<- markup
                          block <<- gtkFrameNew()
                          block$add(widget)
                          
                          block$SetLabelAlign(pos,0.5)
                          label <- gtkLabelNew()
                          block$setLabelWidget(label)
                          set_names(text)
                        },
                        get_names=function(...) {
                          label <- block$getLabelWidget()
                          label$getLabel()
                        },
                        set_names=function(value, ...) {
                          label <- block$getLabelWidget()
                          if(markup)
                            label$setMarkup(value)
                          else
                            label$setLabel(value)
                        }
                        ))
                        
