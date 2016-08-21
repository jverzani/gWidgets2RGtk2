##' @include GWidget.R
NULL

## Base class for dialogs
##
## Override modify_widget, ok_response, cancel_response and get_buttons
GDialog <- setRefClass("GDialog",
                      contains="GContainer",
                      methods=list(
                        initialize=function(toolkit=NULL, msg="", title="", icon="info", parent=NULL, ...) {
                          
                          icon <- paste("GTK_MESSAGE_",toupper(icon), sep="")
    
                          ## parent is for placement, not tracking widget heirarchy
                          .parent <- parent
                          if(!is.null(.parent)) {
                            if(is(.parent, "GWindow"))
                              .parent <- .parent
                            .parent <- getBlock(.parent)
                            if(!is(.parent,"GtkWindow"))
                              .parent <- .parent$GetWindow()
                            if(!is(.parent,"GtkWindow"))
                              .parent <- NULL          # give up
                          }
  
                            
                          ## use message dialog for Gtk
                          widget <<- gtkMessageDialogNew(
                                                         parent = .parent,
                                                         flags = 0,
                                                         buttons = get_buttons(),
                                                         type=icon,
                                                         msg[1]
                                                         )
                          ## odd, should have show=FALSE above, but doesn't work. Not sure why. Error is:
                          ## > dlg = GDialog$new(NULL, msg="asdf")
                          ## Error in .Object$initialize(...) : attempt to apply non-function
                          ## > traceback()
                          ## 5: .Object$initialize(...)
                          ## 4: initialize(value, ...)
                          ## 3: initialize(value, ...)
                          ## 2: methods::new(def, ...)
                          ## 1: GDialog$new(NULL, msg = "asdf")

                          widget$hide()
                          
                          if(length(msg) > 1)
                            widget["secondary-text"] <<- paste(msg[-1], collapse = "\n")

                          modify_widget()

                          
                          widget$SetTitle(title)
                          widget$GrabFocus()
                          widget$GetWindow()$Raise()
                          widget$setDefaultResponse(GtkResponseType["ok"])

                          initFields(block=widget)
                          callSuper(toolkit)
                        },
                        get_buttons=function() {
                          "Return string indicating buttons, cf GtkButtonsType"
                          "GTK_BUTTONS_CLOSE"
                        },
                        ok_response=function() {
                          "Response for ok button"
                          NULL
                        },
                        cancel_response=function() {
                          "Response for cancel button"
                          NULL
                        },
                        modify_widget=function() {
                          "Modify widget, eg. add input area"
                        },
                        run=function() {
                          "Run dialog in modal mode"
                            ## run in modal mode
                          widget$showAll()
                          response = widget$Run()
                          
                          if (response == GtkResponseType["close"] ||
                              response == GtkResponseType["delete-event"] ||
                              response == GtkResponseType["cancel"]) {
                            ret <- cancel_response()
                            widget$Destroy()
                          } else if(response == GtkResponseType["ok"]) {
                            ret <- ok_response()
                            widget$Destroy()
                          } else {
                            ret <- cancel_response()
                            widget$Destroy()
                          }
                          invisible(ret)
                        }
                        ))



##' toolkit implementation for gmessage
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gmessage guiWidgetsToolkitRGtk2
## @export .gmessage guiWidgetsToolkitRGtk2
.gmessage.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                             msg,
                                             title = "message",
                                             icon = c("info","warning","error","question"),
                                             parent=NULL,
                                             ...
                                             ) {

  dlg <- GMessage$new(toolkit, msg, title, icon, parent, ...)
  dlg$run()
}

## subclass for message dialog
##
## @param ... passed to constructor
GMessage <- setRefClass("GMessage", contains="GDialog")



##' toolkit implementation for gconfirm
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gconfirm guiWidgetsToolkitRGtk2
## @export .gconfirm guiWidgetsToolkitRGtk2
.gconfirm.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                              msg,
                                              title = "Confirm",
                                              icon = c("info","warning","error","question"),
                                              parent=NULL,
                                              ...
                                              ) {
  dlg <- GConfirm$new(toolkit, msg, title, icon, parent, ...)
  dlg$run()

}

## class for confirmation dialog
GConfirm <- setRefClass("GConfirm",
                        contains="GDialog",
                        methods=list(
                          ok_response=function() TRUE,
                          cancel_response=function() FALSE,
                          get_buttons=function() "GTK_BUTTONS_OK_CANCEL"
                          ))
                        


##' toolkit implmentation of ginput
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .ginput guiWidgetsToolkitRGtk2
## @export .ginput guiWidgetsToolkitRGtk2
.ginput.guiWidgetsToolkitRGtk2 <- function(toolkit,
                                           msg,
                                           text="",
                                           title = "Input",
                                           icon = c("info","warning","error","question"),
                                           parent=NULL,                   
                                           ...
                                           ) {
  dlg <- GInput$new(toolkit, msg, title, icon, parent, ...)
  dlg$set_text(text)
  dlg$run()
}         


GInput <- setRefClass("GInput",
                      contains="GDialog",
                      fields=list(
                        entry="ANY"
                        ),
                      methods=list(
                        get_buttons=function() "GTK_BUTTONS_OK_CANCEL",
                        ok_response=function() entry$getText(),
                        cancel_response=function() character(0),
                        modify_widget=function() {
                          ## define entry
                          entry <<- gtkEntry()
                          widget$GetVbox()$PackStart(entry) 
                          gSignalConnect(entry, "activate", function(...) {
                            widget$response(GtkResponseType["ok"])
                          })
                        },
                        set_text=function(txt) {
                          entry$setText(txt)
                          entry$selectRegion(0L, -1L)
                          entry$grabFocus()
                        }
                        ))

##' toolkit implementation
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gbasicdialog guiWidgetsToolkitRGtk2
## @export .gbasicdialog guiWidgetsToolkitRGtk2
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
                        dlg$present()
                        dlg$setKeepAbove(TRUE)
                        
                        
                                
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
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .galert guiWidgetsToolkitRGtk2
## @export .galert guiWidgetsToolkitRGtk2
.galert.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                            msg,
                                            title = "message",
                                            delay = 3,
                                            parent=NULL,
                                            ...
                                            ) {

            ## insert an info bar here?
            if(is(parent, "GWindow")) {
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
#              gSignalConnect(evb, "motion-notify-event", f=f)

              motion_notify_cb <- function(...) {
                  if(timer$started)
                      timer$stop_timer()
                  FALSE
              }
              gSignalConnect(evb, "motion-notify-event", f=motion_notify_cb)
              leave_notify_cb <- function(...) {
                  timer$set_interval(2*1000)
                  timer$start_timer()
                  FALSE
              }
              gSignalConnect(evb, "leave-notify-event", f=leave_notify_cb)
              
              w$show()

              timer <- gtimer(delay*1000, FUN=f, one.shot=TRUE, toolkit=toolkit)
            }
          }
 
