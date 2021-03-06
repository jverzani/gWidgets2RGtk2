##' @include ggroup.R
NULL

##' gframe constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gframe guiWidgetsToolkitRGtk2
## @export .gframe guiWidgetsToolkitRGtk2
.gframe.guiWidgetsToolkitRGtk2 <- function(toolkit, text, markup, pos, horizontal=TRUE, spacing=5,container=NULL, ...) {
  GFrame$new(toolkit, text, markup, pos, horizontal, spacing, container, ...)
}

## base class for gframe
GFrame <- setRefClass("GFrame",
                      contains="GGroupBase",
                      fields=list(
                        markup="logical",
                        spacing="numeric"
                        ),
                      methods=list(
                        initialize=function(toolkit=NULL, text="", markup=FALSE, pos=0, horizontal=TRUE, spacing=5, container=NULL,
                          ...) {

                          horizontal <<- horizontal
                          spacing <<- spacing
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
                          block$SetLabelAlign(pos,0.5)
                          label <- gtkLabelNew()
                          block$setLabelWidget(label)
                          set_names(text)
                          block$add(widget)
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
                        
