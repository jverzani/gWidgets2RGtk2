##' @include ggroup.R
NULL

##' gframe constructor
##'
##' @inheritParams gWidgets2::gframe
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gframe guiWidgetsToolkitRGtk2
##' @S3method .gframe guiWidgetsToolkitRGtk2
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
                        initialize=function(toolkit=NULL, text="", markup=FALSE, pos=0, horizontal=TRUE, spacing=5, container=NULL,use.scrollwindow=FALSE, ...) {

                          horizontal <<- horizontal
                          spacing <<- spacing
                          make_widget(text, markup, pos, use.scrollwindow)
                          
                          add_to_parent(container, .self, ...)
                          
                          callSuper(toolkit, horizontal=horizontal, ...)
                        },
                        make_widget = function(text, markup, pos, use.scrollwindow) {
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
                          
                          if(use.scrollwindow) {
                            sw <- gtkScrolledWindowNew()
                            sw$SetPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                            sw$AddWithViewport(widget)
                            block$add(sw)
                          } else {
                            block$add(widget)
                          }
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
                        
