##' @include GWidget.R
NULL

##' Toolkit implementation
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gfile guiWidgetsToolkitRGtk2
## @export .gfile guiWidgetsToolkitRGtk2
.gfile.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                          text = "",
                                          type = c("open","save","selectdir"),
                                          initial.filename = NULL,
                                          initial.dir = getwd(),
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

  ## Add filter
  if(length(filter) && type %in% c("open","save")) {
    if(is.character(filter)) {
      ## make alist
      filter <- sapply(names(filter), function(nm) {
        list(patterns=paste("*.", filter[nm], sep=""))
      }, simplify=FALSE)
    }



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

  ## this works *exactly* the same as the previous ifelseifelseifelse
  #switch(type,
  #       "open" = ,
  #       "save" = {
  #         if(!is.null(initial.filename))
  #           filechooser$SetFilename(initial.filename)
  #         filechooser$setCurrentFolder(initial.dir)
  #       },
  #       "selectdir" = {
  #         filechooser$setCurrentFolder(initial.dir)
  #       })

  ## but I think this does the same thing??
  if (type %in% c("open", "save") && !is.null(initial.filename))
    filechooser$setCurrentName(basename(initial.filename))
  filechooser$setCurrentFolder(initial.dir)

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
## @export .gfilebrowse guiWidgetsToolkitRGtk2
.gfilebrowse.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                                 text = "",
                                                 type = c("open","save","selectdir"),
                                                 initial.filename = NULL,
                                                 initial.dir = getwd(),
                                                 filter = list(),
                                                 quote=TRUE,
                                                 handler=NULL,
                                                 action=NULL,
                                                 container = NULL,
                                                 ... ) {
  GFileBrowse$new(toolkit,
            text=text, type=type, initial.filename=initial.filename, initial.dir = initial.dir,
            filter=filter, quote=quote, handler=handler, action=action, container=container, ...)
}


## XXX
GFileBrowse <- setRefClass("GFileBrowse",
                           contains="GWidgetWithItems",
                           fields=list(
                             button="ANY",
                             initial.text="character"
                             ),
                           methods=list(
                              initialize=function(
                                toolkit=NULL,
                                text = "",
                                type = c("open", "save", "selectdir"),
                                initial.filename = NULL,
                                initial.dir = getwd(),
                                filter = list(),
                                quote=TRUE,
                                handler=NULL,
                                action=NULL,
                                container = NULL,
                                ... ) {

                                block <<- gtkHBox()
                                widget <<- gtkEntry()
                                initial.text <<- text
                                widget$setText(text)
                                button <<- gtkButtonNewFromStock("gtk-open") # gtk-file isn't working

                                block$packStart(widget, expand=TRUE, fill=TRUE)
                                block$packStart(button)
                                gSignalConnect(button, "clicked", f=function(...) {
                                 ret <- gfile(text=text, type=type,
                                              initial.filename=initial.filename, initial.dir = initial.dir,
                                              filter=filter, toolkit=toolkit)
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
                                if (x == initial.text)
                                  return(character(0))
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
