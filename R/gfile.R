##' @include GWidget.R
NULL

##' Toolkit implementation
##'
##' @inheritParams gWidgets2::gfile
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gfile guiWidgetsToolkitRGtk2
##' @S3method .gfile guiWidgetsToolkitRGtk2
.gfile.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                          text = "",
                                          type = c("open","save","selectdir"),
                                          initial.filename = NULL,
                                          filter = list(),
                                          multi=FALSE,
                                          ...) {
  ## make dialog, return character class object (character(0) if no selectino)
  type <- match.arg(type)
  avail_types <- c(
                   "open"="open",
                   "save"="save",
                   "selectdir"="select-folder",
                   "createdir"="create-folder"
                   )
            
  actiontype <- GtkFileChooserAction[avail_types[type]]
  button_with_id = list(
    "ok"= c("gtk-ok",GtkResponseType["ok"]),
    "cancel" = c("gtk-cancel",GtkResponseType["cancel"])
    )
            
  which_buttons <- switch(type,
                          "save"=c("ok","cancel"),
                          "open"=c("ok","cancel"),
                          "selectdir"=c("ok","cancel")
                          )

  filechooser <- gtkFileChooserDialogNew(title=text, action=actiontype)
  filechooser$setSelectMultiple(multi)
            
  for(i in which_buttons) 
    filechooser$AddButton(button_with_id[[i]][1],button_with_id[[i]][2])
            
  ## Add fileter
  if(length(filter) && type %in% c("open","save")) {
    for(i in names(filter)) {
      filefilter <- gtkFileFilterNew()
      filefilter$SetName(i)
      if(!is.null(filter[[i]]$patterns)) {
        for(pattern in filter[[i]]$patterns)
          filefilter$AddPattern(pattern)
      }
      if(!is.null(filter[[i]]$mime.types)) {
        for(mime.type in filter[[i]]$mime.types)
          filefilter$AddMimeType(mime.type)
      }
      filechooser$AddFilter(filefilter)
    }
  }
            
  if(!is.null(initial.filename)) {
    if(type == "open") {
      filechooser$SetFilename(paste(getwd(),initial.filename, sep=.Platform$file.sep))
    } else if(type == "save") {
      filechooser$setCurrentFolder(getwd())
      filechooser$setCurrentName(initial.filename)
    }
  }
  
  ## this makes it modal
  response <- filechooser$Run()
  file <- unlist(filechooser$GetFilenames())

  if(response == GtkResponseType["cancel"]) {
    ## just close
    filechooser$Destroy()
    return(character(0))
  } else if(response == GtkResponseType["ok"]) {
    filechooser$Destroy()
    return(file)
  } else {
    filechooser$Destroy()
    return(character(0))
  }
}
                                          

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gfilebrowse guiWidgetsToolkitRGtk2
##' @S3method .gfilebrowse guiWidgetsToolkitRGtk2
.gfilebrowse.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                 text = "",
                                                 type = c("open","save","selectdir"),
                                                 initial.filename = NULL,
                                                 filter = list(),
                                                 quote=TRUE,
                                                 handler=NULL,
                                                 action=NULL,
                                                 container = NULL,
                                                 ... ) {
  GFileBrowse$new(toolkit,
            text=text, type=type, initial.filename=initial.filename,
            filter=filter, quote=quote, handler=handler, action=action, container=container, ...)
}


## XXX
GFileBrowse <- setRefClass("GFileBrowse",
                           contains="GWidgetWithItems",
                           fields=list(
                             button="ANY"
                             ),
                           methods=list(
                              initialize=function(
                                toolkit=NULL,
                                text = "",
                                type = c("open", "save", "selectdir"),
                                initial.filename = NULL,
                                filter = list(),
                                quote=TRUE,
                                handler=NULL,
                                action=NULL,
                                container = NULL,
                                ... ) {
                                
                                block <<- gtkHBox()
                                widget <<- gtkEntry()
                                widget$setText(text)
                                button <<- gtkButtonNewFromStock("gtk-open") # gtk-file isn't working

                                block$packStart(widget, expand=TRUE, fill=TRUE)
                                block$packStart(button)
                                gSignalConnect(button, "clicked", f=function(...) {
                                 ret <- gfile(text=text, type=type, initial.filename=initial.filename,filter=filter, toolkit=toolkit)
                                 if(length(ret))
                                   set_value(ret)
                                })

                                initFields(change_signal="activate")

                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                              get_value=function( ...) {
                                x <- widget$getText()
                                Encoding(x) <- "UTF-8"
                                x
                              },
                              set_value=function(value, ...) {
                                ## should we check file.exists?
                                widget$setText(value)
                                invisible(notify_observers(signal=change_signal))
                              },
                              add_handler_changed=function(handler, action=NULL, ...) {
                                add_handler(change_signal, handler, action=action, ...)
                              }
                              ))

