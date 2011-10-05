##' @include GWidget.R
NULL

##' toolkit implementation for gmessage
##'
##' @param toolkit 
##' @param msg 
##' @param title 
##' @param icon 
##' @param parent 
##' @param ... 
##' @return NULL
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.gmessage.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                             msg,
                                             title = "message",
                                             icon = c("info","warning","error","question"),
                                             parent=NULL,
                                             ...
                                             ) {
  
  icon <- match.arg(icon)
  icon <- paste("GTK_MESSAGE_",toupper(match.arg(icon)), sep="")
  
  button <- "GTK_BUTTONS_OK"
  
  ## parent
  if(!is.null(parent)) {
    parent <- getBlock(parent)
    if(!is(parent,"GtkWindow"))
      parent <- parent$GetWindow()
    if(!is(parent,"GtkWindow"))
      parent <- NULL          # give up
  }
  
  
  ## use message dialog for Gtk
  dlg <- gtkMessageDialogNew(
                             parent = parent,
                             flags = 0,
                             buttons = button,
                             type=icon,
                             msg[1]
                             )
  
  if(length(message) > 1)
    dlg['secondary-text'] <- paste(msg[-1], collapse = "\n")
  
  dlg$SetTitle(title)
  dlg$GrabFocus()
  dlg$GetWindow()$Raise()
  dlg$setDefaultResponse(GtkResponseType["ok"])
  
  ## run in modal mode
  response <- dlg$Run()
  dlg$destroy()
  return(NULL)
}

##' toolkit implementation for gconfirm
##'
##' @param toolkit 
##' @param msg 
##' @param title 
##' @param icon 
##' @param parent 
##' @param ... 
##' @return logical
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.gconfirm.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                              msg,
                                              title = "Confirm",
                                              icon = c("info", "warning", "error", "question"),
                                              parent=NULL,
                                              ...
                                              ) {
            
  if(missing(icon))
    icon <- "question"
  icon <- match.arg(icon)
  icon <- paste("GTK_MESSAGE_",toupper(match.arg(icon)), sep="")
  buttons <- "GTK_BUTTONS_OK_CANCEL"
            
  ## parent
  if(!is.null(parent)) {
    parent <- getBlock(parent)
    if(!is(parent,"GtkWindow"))
      parent <- parent$GetWindow()
    if(!is(parent,"GtkWindow"))
      parent <- NULL          # give up
  }
              
  

  dlg <- gtkMessageDialogNew(
                             parent = parent,
                             flags = 0,
                             buttons = buttons,
                             type=icon,
                             msg[1]
                             )
  if(length(msg) > 1)
    dlg['secondary-text'] <- paste(msg[-1], collapse = "\n")
  
  dlg$SetTitle(title)            
  dlg$GrabFocus()
  dlg$GetWindow()$Raise()
  dlg$setDefaultResponse(GtkResponseType["ok"]) # fails -- need to use gtkDialog directly
  
  ## add callback to close
  close.handler = function(h,...) h$obj$Destroy()
            
  ## run in modal mode
  response = dlg$Run()
  if (response == GtkResponseType["close"] ||
      response == GtkResponseType["delete-event"] ||
      response == GtkResponseType["cancel"]) {
    dlg$Destroy()
    invisible(FALSE)
  } else if(response == GtkResponseType["ok"]) {
    dlg$Destroy()
    invisible(TRUE)
  } else {
    ## not sure what this is
    invisible(FALSE)
  }
}

##' toolkit implmentation of ginput
##'
##' @param toolkit 
##' @param msg 
##' @param text 
##' @param title 
##' @param icon 
##' @param parent 
##' @param ... 
##' @return character
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.ginput.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                           msg,
                                           text="",
                                           title = "Input",
                                           icon = c("info", "warning", "error", "question"),
                                           parent=NULL,                   
                                           ...
                                           ) {
  
  icon <- paste("GTK_MESSAGE_",toupper(match.arg(icon)), sep="")
  
  ## parent
  if(!is.null(parent)) {
    parent <- getBlock(parent)
    if(!is(parent,"GtkWindow"))
      parent <- parent$GetWindow()
    if(!is(parent,"GtkWindow"))
      parent <- NULL          # give up
  }
  
  
  ## use message dialog for Gtk
  dlg <- gtkMessageDialogNew(
                             parent = parent,
                             flags = 0,
                             buttons = "GTK_BUTTONS_OK_CANCEL",
                             type=icon,
                             msg[1]
                             )
  if(length(msg) > 1)
    dlg['secondary-text'] <- paste(msg[-1], collapse = "\n")
  dlg$SetTitle(title)
  dlg$setDefaultResponse(GtkResponseType["ok"])
            
  dlg$GrabFocus()
  dlg$GetWindow()$Raise()

  entry <- gtkEntry()
  dlg$GetVbox()$PackStart(entry) 

  ## set as default
  entry['can-default'] <- TRUE
  entry$grabFocus()
  entry$grabDefault()
            
  ## run in modal mode
  response = dlg$Run()
  if(response == GtkResponseType["cancel"] ||
     response == GtkResponseType["close"] ||
     response == GtkResponseType["delete-event"]) {
    dlg$Destroy()
    invisible(character(0))
  } else if(response == GtkResponseType["ok"]) {
    val <- entry$getText()
    dlg$Destroy()
    return(val)
  } else {
    dlg$Destroy()
    invisible(character(0))
  }
}         

##' toolkit implementation
##'
##' @param toolkit 
##' @param title 
##' @param parent 
##' @param do.buttons 
##' @param handler 
##' @param action 
##' @param ... 
##' @return logical
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.gbasicdialog.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                                 title = "Dialog",
                                                 parent=NULL,
                                                 do.buttons=TRUE,
                                                 handler = NULL,
                                                 action = NULL,
                                                 ...
                                                 ) {
            
  obj <- GBasicDialog$new(toolkit,
                          title=title, parent=parent, do.buttons=do.buttons,
                          handler=handler, action=action, 
                          ...)
  obj
}


## class for basic dialog
GBasicDialog <- setRefClass("GBasicDialog",
                    contains="GContainer",
                    fields=list(
                      handler="ANY",
                      action="ANY"
                      ),
                    methods=list(
                      initialize=function(toolkit=NULL,
                        title = "Dialog",
                        parent=NULL,
                        do.buttons=TRUE,
                        handler = NULL,
                        action = NULL,
                        ...) {
                        
                        widget <<- gtkHBox() 
                        
                        ## parent
                        .parent <- parent
                        if(!is.null(.parent)) {
                          .parent <- getBlock(.parent)
                          if(!is(.parent,"GtkWindow"))
                            .parent <- .parent$GetWindow()
                          if(!is(.parent,"GtkWindow"))
                            .parent <- NULL          # give up
                        } else {
                          .parent <- gtkWindowNew(show=FALSE)
                        }
            

                        buttons <- c("ok", "cancel")

                        ## can override, though not included here
                        buttonMap <- function(name) {
                          if(name == "ok")
                            list("gtk-ok", GtkResponseType["ok"])
                          else if(name =="yes")
                            list("gtk-yes", GtkResponseType["ok"])
                          else if(name == "cancel")
                            list("gtk-cancel", GtkResponseType["cancel"])
                          else if(name == "close")
                            list("gtk-close", GtkResponseType["close"])
                          else if(name =="no")
                            list("gtk-no", GtkResponseType["cancel"])
                          else
                            list("gtk-yes", GtkResponseType["ok"])
                        }
                        
                        l <- list(title=title, parent=.parent, flags=c("modal"), show=FALSE)
                        for(i in buttons) {
                          m <- buttonMap(i)
                          l[[length(l) + 1]] <- m[[1]]
                          l[[length(l) + 1]] <- m[[2]]
                        }
                        
                        ## do buttons?
                        if(do.buttons) {
                          dlg <- do.call("gtkDialog", l)
                        } else {
                          dlg <- gtkDialogNew(show=FALSE)
                          ## hide separator and button box. Uses internals -- bad idea if widget changes
                          sapply(dlg$getChildren()[[1]]$getChildren(), gtkWidgetHide)
                        }
                        dlg$setTransientFor(.parent)
                        
                        dlg$SetTitle(title)
                        dlg$setDefaultResponse(GtkResponseType["ok"])
                        dlg$getVbox()$PackStart(widget)
                        dlg$grabFocus()
                        
                                
                        initFields(block=dlg,
                                   handler=handler,
                                   action=action
                                   )

                        callSuper(toolkit)
                      },
                      add_child=function(child, ...) {
                        widget$packStart(getBlock(child), expand=TRUE, fill=TRUE)
                        child_bookkeeping(child)
                      },
                      dispose=function() {
                        block$destroy()
                      },
                      set_visible=function(...) {
                        block$show()
                        response <- block$run()

                        h <- list(obj=.self, action=action)
                        if(response == GtkResponseType["cancel"] ||
                           response == GtkResponseType["close"] ||
                           response == GtkResponseType["delete-event"]) {
                          ## cancel action
                          ret <- FALSE
                        } else if(response == GtkResponseType["ok"]) {
                          if(!is.null(handler))
                            handler(h)
                          ret <- TRUE              # was widget, but TRUE now
                        } else if(response == GtkResponseType["delete-event"]) {
                          ## window manager close
                          ret <- FALSE
                        } else if(response == GtkResponseType["none"]) {
                          ## dispose() call
                          ret <- FALSE
                        } else {
                          ret <- FALSE
                        }
                        block$Destroy()
                        return(invisible(ret))
                      }
                      ))


##' toolkit implementation of galert
##'
##' @param toolkit 
##' @param msg 
##' @param title 
##' @param delay 
##' @param parent 
##' @param ... 
##' @return NULL
##' @export
##' @rdname gWidgetsRGtk2-undocumented
.galert.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                            msg,
                                            title = "message",
                                            delay = 3,
                                            parent=NULL,
                                            ...
                                            ) {

            ## insert an info bar here?
            if(is(parent, "gWindow")) {
              parent$set_infobar(msg)
            } else {
              ## make a transient dialog window
              w <- gtkWindow(show=FALSE)
              w$setTitle(title)
              w$setDefaultSize(300, 150)
              evb <- gtkEventBox()
              evb$SetVisibleWindow(FALSE)
              evb$AddEvents(GdkEventMask["all-events-mask"])
              l <- gtkLabel(msg)
              evb$add(l); w$add(evb)

              if(!is.null(parent)) {
                parent <- getBlock(parent)
                if(!is(parent,"GtkWindow"))
                  parent <- parent$GetWindow()
                if(!is(parent,"GtkWindow"))
                  parent <- NULL          # give up
              }
              if(!is.null(parent))
                w$setTransientFor(parent)

              f <- function(...) {
                timer$stop_timer()
                if(!is(w, "<invalid>"))
                  w$destroy()
                FALSE
              }
              gSignalConnect(evb, "motion-notify-event", f=f)

              w$show()

              timer <- gtimer(delay*1000, FUN=f, one.shot=TRUE)
            }
          }
 
