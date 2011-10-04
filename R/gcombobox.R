##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gcombobox.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                               items, selected = 1, editable = FALSE, coerce.with = NULL,
                                               handler = NULL,action = NULL, container = NULL, ... ) {

  if(editable)
    GComboBoxWithEntry$new(toolkit,
                           items, selected = selected, coerce.with = coerce.with,
                           handler = handler,action = action, container = container, ...)
  else
    GComboBoxNoEntry$new(toolkit,
                         items, selected = selected, coerce.with = coerce.with,
                         handler = handler,action = action, container = container, ...)
  
}


## We create two subclasses of this to handle editable and non-editable situation. These methods end up being in common for both.
GComboBox <- setRefClass("GComboBox",
                         contains="GWidget",
                         methods=list(
                           get_index = function(...) {
                             widget$getActive() + 1L
                           },
                           set_index = function(value,...) {
                             value <- min(max(-1, as.integer(value)), get_length())
                             widget$setActive(value - 1L)
                           },
                           ## ,
                           ## add_handler_changed=function(handler, action=NULL, ...) {
                           ##   add_handler_clicked(handler, action=NULL, ...)
                           ## },
                           add_handler_clicked = function(handler, action=NULL, ...) {
                             add_handler("changed", handler, action=action, ...)
                           },
                           check_windows = function(items) {
                             "Hack to make width under windows work better"
                             if(.Platform$OS == "windows") {
                               if(dim(items)[1] > 0) {
                                 colChars <- max(sapply(items[,1,drop=TRUE],nchar))
                                 if(colChars < 3)
                                   widget$setWidthRequest(15*(4 + colChars))
                               }
                             }
                           }
                           ))

## combobox without entry can have icons, use rGtkDataFrame 
GComboBoxNoEntry <- setRefClass("GComboBoxNoEntry",
                                contains="GComboBox",
                                methods=list(
                                  initialize=function(toolkit=NULL,
                                    items,
                                    selected = 1, # use 0 for blank
                                    coerce.with = NULL,
                                    handler, action, container, ...) {

                                    if(ncol(items) >=2)
                                      items$icons <- sapply(items$icons, getStockIconByName)
                                    
                                    store <- rGtkDataFrame(items)
                                    ## drop down list, not combo
                                    widget <<- gtkComboBoxNewWithModel(store)
                                    cellrenderer <- gtkCellRendererTextNew()
                                    widget$PackStart(cellrenderer, expand=TRUE)
                                    widget$AddAttribute(cellrenderer,"text", 0)
                                    ## icons
                                    if(ncol(items) >= 2) {
                                      cellrenderer <- gtkCellRendererPixbufNew()
                                      widget$PackStart(cellrenderer, expand=FALSE)
                                      widget$AddAttribute(cellrenderer, "stock-id", 1)
                                      
                                      if(ncol(items) >= 3) {
                                        message("tooltips are not implemented")
                                      }
                                    }
                                    
                                    widget$show()
                                    widget$setActive(selected - 1L)
                                    
                                    check_windows(items)
                                    
                                    initFields(block=widget,
                                               coerce_with=coerce.with,
                                               change_signal="changed",
                                               ..blocked=0
                                               )
                                    
                                    add_to_parent(container, .self, ...)
                                    
                                    handler_id <<- add_handler_changed(handler, action)
                                    
                                    callSuper(toolkit)
                                  },
                                  get_value=function( ...) {
                                    widget$getModel()[get_index(), 1]
                                  },
                                  set_value=function(value, ...) {
                                    ind <- pmatch(value, get_items(drop=TRUE))
                                    if(!is.na(ind))
                                      set_index(ind)
                                    else
                                      message("No match for ", value)
                                  },
                                  get_items = function(i, j, ..., drop=TRUE) {
                                    store <- widget$getModel()
                                    if(drop)
                                      store[,1, drop=TRUE]
                                    else
                                      store[,]
                                  },
                                  set_items = function(value, i, j, ...) {
                                    "Set items. Indexing is ignored"
                                    items <- gWidgets2:::.make_gcombobox_items(value)
                                    store <- rGtkDataFrame(items)
                                    if(ncol(store) != ncol(widget$getModel()))
                                      stop("Must keep same number of columns when replacing values")
                                    widget$setModel(store)
                                    set_index(0L)
                                  },
                                  get_length = function(...) {
                                    nrow(widget$getModel())
                                  }
                                  ))

## The editable code is *different* from the non-editable code, as the
## gtkComboBoxNewWithEntry method isn't there yet. Instead we need to use
## a convenience function and manipulate the values with that.
## This method is deprecated as of 2.24, but that isn't what I have installed
GComboBoxWithEntry <- setRefClass("GComboBoxWithEntry",
                                contains="GComboBox",
                                  fields=list(
                                    poss_items="ANY"
                                    ),
                                  methods=list(
                                    initialize=function(toolkit=NULL,
                                      items,
                                      selected = 1, # use 0 for blank
                                      coerce.with = NULL,
                                      handler, action, container, ...) {
                                      
                                      poss_items <<- items[,1, drop=TRUE]

                                      widget <<- gtkComboBoxEntryNewText()
                                      sapply(poss_items, gtkComboBoxAppendText, object=widget)

                                      widget$show()
                                      widget$setActive(selected - 1L)

                                      ## set size if really small under windows
                                      check_windows(items)

                                      initFields(block=widget,
                                                 coerce_with=coerce.with,
                                                 change_signal="changed"
                                                 )
                                      
                                      add_to_parent(container, .self, ...)

                                      handler_id <<- add_handler_changed(handler, action)

                                      callSuper(toolkit)
                                    },
                                    get_value=function( ...) {
                                      widget$getChild()$getText()
                                    },
                                    set_value=function(value, ...) {
                                      widget$getChild()$setText(value)
                                    },
                                    get_items = function(i, j, ..., drop=TRUE) {
                                      poss_items
                                    },
                                    set_items = function(value, i, j, ...) {
                                      "Set items. Indexing is ignored"
                                      ## remove, then append
                                      sapply(rev(seq_len(get_length())), function(i) widget$removeText(i - 1L))
                                      items <- value[,1, drop=TRUE]
                                      sapply(items, gtkComboBoxAppendText, object=widget)
                                      poss_items <<- items
                                      block_handlers()
                                      set_value("")
                                      unblock_handlers()
                                    },
                                    get_length = function(...) {
                                      widget$getModel()$iterNChildren(NULL)
                                    },
                                    add_handler_edited = function(handler, action=NULL, ...) {
                                      "For editing -- need a better name XXX"
                                      gSignalConnect(widget$getChild(), "activate", f=function(h, ...) {
                                        handler(h, ...)
                                      },  data=list(obj=obj, action=action,...),
                                                     user.data.first = TRUE)
                                    },
                                    add_handler_keystroke=function(handler, action=NULL, ...) {
                                      gSignalConnect(widget$getChild(), "keystroke", keyrelease_decorator(handler),
                                                     data=list(obj=obj, action=action,...),
                                                     user.data.first = TRUE)
                                    }
                                    
                                    ))


## ##' exported Subclass for users to subclass
## ##'
## ##' @exportClass GComboBoxRGtk2
## GComboBoxRGtk2 <- setRefClass("GComboBoxRGtk2",
##                                contains="GComboBox")
