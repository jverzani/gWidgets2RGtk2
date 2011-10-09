##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @inheritParams gWidgets2::.gtable
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gtable.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                         items,
                                         multiple = FALSE,
                                         chosen.col = 1,
                                         icon.col = NULL,
                                         tooltip.col=NULL,
                                         handler = NULL, action = NULL,
                                         container = NULL, ... ) {
  GTable$new(toolkit,
           items=items,
           multiple=multiple,
           chosen.col=chosen.col,
           icon.col = icon.col,
           tooltip.col = tooltip.col,
           handler=handler,
             action=action,
           container=container ,...)
}


make_treeview_column <- function(x, col_no, editable=FALSE, model=NULL) UseMethod("make_treeview_column")
make_treeview_column.default <- function(x, col_no, editable=FALSE, model=NULL) {
  ## Return a tree view column instance to render x, located in 0-based col in model
  cellrenderer <- gtkCellRendererText()
  view_col <- gtkTreeViewColumnNew()
  view_col$PackStart(cellrenderer, TRUE)
  view_col$AddAttribute(cellrenderer, "text", col_no)
  view_col
}

## XXX
GTable <- setRefClass("GTable",
                      contains="GWidget",
                      fields=list(
                        items="ANY",
                        chosen_col="integer",
                        icon_col="integerOrNULL",
                        tooltip_col="integerOrNULL"
                        ),
                      methods=list(
                              initialize=function(toolkit=NULL,
                                items="data.frame",
                                multiple = FALSE,
                                chosen.col = 1,
                                icon.col = NULL,
                                tooltip.col=NULL,
                                handler = NULL, action = NULL,
                                container = NULL, ... ) {

                                
                                widget <<- gtkTreeViewNew()
                                
                                block <<- gtkScrolledWindowNew()
                                block$setPolicy("GTK_POLICY_AUTOMATIC","GTK_POLICY_AUTOMATIC")
                                block$add(widget)
                                
                                if(multiple)
                                  set_selection_mode("multiple")
                              
                                
                                if(missing(items) || ncol(items) > 1)
                                  widget$SetRulesHint(TRUE)
                                widget$SetEnableSearch(TRUE)


                                ## we want column index, not name
                                if(is.character(icon.col))
                                  icon.col <- match(icon.col, names(items))
                                if(is.numeric(icon.col))
                                  icon.col <- as.integer(icon.col)
                                
                                if(is.character(tooltip.col))
                                  tooltip.col <- as.integer(match(tooltip.col, names(items)))
                                if(is.numeric(tooltip.col))
                                  tooltip.col <- as.integer(tooltip.col)

                                
                                initFields(chosen_col=as.integer(chosen.col),
                                           icon_col = icon.col,
                                           tooltip_col=tooltip.col,
                                           change_signal="row-activated"
                                           )


                                set_items(items)
                                
                                add_to_parent(container, .self, ...)

                                handler_id <<- add_handler_changed(handler, action)

                                callSuper(toolkit)
                              },
                        clear_columns=function() {
                          "Clear out old treeview columns in preparation of new"
                          sapply(rev(widget$getColumns()), widget$removeColumn)
                        },
                        make_columns=function() {
                          "Make new columns, watching out for icons, tooltips, visible"
                          if(!is.null(icon_col)) {
                            widget$InsertColumn(make_icon_column(),0L)
                          }
                          if(!is.null(tooltip_col)) {
                            ## use column tooltip_col - 1L for a tooltip
                            x <- seq_along(items);
                            if(!is.null(tooltip_col)) {
                              widget$setTooltipColumn(tooltip_col - 1L)
                            }
                          }
                          ## now add columns, one by one
                          DF <- get_model()
                          sapply(get_valid_columns(), function(col) {
                            treeview_col <- make_treeview_column(DF[,col], col - 1L)
                            widget$insertColumn(treeview_col, pos = -1) # at end
                          })
                        },
                        make_icon_column=function() {
                          "Make column for icons"
                          cellrenderer <- gtkCellRendererPixbufNew()
                          view.col <- gtkTreeViewColumnNew()
                          view.col$PackStart(cellrenderer, TRUE)
                          view.col$AddAttribute(cellrenderer, "stock-id", icon_col - 1L)
                          view.col
                        },
                        set_selection_mode=function(mode=c("none","single","browse", "multiple", "extended")) {
                          "Set the selection mode"
                          sel_model <- widget$getSelection()
                          sel_model$setMode(GtkSelectionMode[match.arg(mode)])
                        },
                        not_these=function() {
                          "Helper. Remove these indices due to icon_col, ..."
                          x <- unlist(list(icon_col, tooltip_col))
                          if(is.null(x))
                            x <- integer(0)
                          x
                        },
                        get_valid_columns=function() {
                          "get column indices less those for icons, tooltips, visible"
                          DF <- get_model()
                          if(!is(DF, "RGtkDataFrame"))
                            return(NULL)
                          j <- seq_len(dim(DF)[2] - 1L) # last col is ..visible
                          setdiff(j, not_these())
                        },
                        
                        get_selected=function() {
                          "Get selected indices or numeric(0)"
                          sel_model <- widget$getSelection()
                          x <- sapply(sel_model$getSelectedRows()$retval, gtkTreePathToString)
                          if(is.null(x))
                            x <- integer(0)
                          else
                            x <- as.numeric(x) + 1L
                          x
                        },
                        set_selected=function(ind) {
                          "Set selected rows by index"
                          old_ind <- get_selected()
                          sel_model = widget$getSelection()
                          block_handlers()
                          sel_model$unselectAll()
                          lapply(ind, function(i) sel_model$selectPath(gtkTreePathNewFromString(i)))
                          unblock_handlers()
                          if ((length(ind) != length(old_ind)) ||
                              any(ind != old_ind))
                            invoke_change_handler()
                        },
                        get_model=function() {
                          "get rGtkDataFrame model"
                          m <- widget$getModel()
                          if(is(m, "GtkTreeModelFilter"))
                            m$getModel() # get past filter
                          else
                            NULL
                        },

                        ## implement basic methods
                        get_value=function(drop=TRUE, ...) {
                          "Get selected values by value (or character(0))"
                          vals <- get_items()[get_selected(), ]
                        if(getWithDefault(drop, TRUE))
                          vals[, chosen_col, drop=TRUE]
                        else
                          vals
                      },
                      set_value=function(value, ...) {
                        "Set selected values by vector matching chosen.col"
                        ind <- match(value, get_value(drop=TRUE))
                        set_index(ind)
                      },
                      get_index = function(...) {
                        "Get index of selected rows or integer(0)"
                        get_selected()
                      },
                      set_index = function(value,...) {
                        "set selected values in value"
                        set_selected(as.integer(value))
                      },
                      get_items = function(i, j, ..., drop=TRUE) {
                        DF <- get_model()[]
                        ## we drop out some stuff
                        DF[, get_valid_columns()][i,j]
                      },
                      set_items = function(value, i, j, ...) {
                        if(missing(i) && missing(j)) {
                          ## set a new data frame model
                          ## we shove in ..visible for the last column q
                          if(!is(value, "data.frame"))
                            value <- as.data.frame(value)
                          ## icons
                          if(!is.null(icon_col)) 
                            value[[icon_col]] <-  getStockIconByName(value[[icon_col]])
                          ## visible column
                          items <<- cbind(value, ..visible=rep(TRUE, nrow(value)))
                          model <- rGtkDataFrame(items)
                          filter <- model$filter()
                          filter$setVisibleColumn(ncol(items) -1L) # last column
                          clear_columns()
                          widget$setModel(filter)
                          make_columns()
                          set_names(names(value)[get_valid_columns()])
                        } else {
                          df_model <- get_model()
                          df_model[i,j] <- value ## hope case matches
                        }
                      },
                      ## data store methods
                      get_length=function() {
                        get_dim()[2]
                      },
                      get_dim=function() {
                        "Return dim of view (not data frame which may have extra information)"
                        c(rows=dim(get_model())[1], columns=length(get_valid_columns()))
                      },
                      get_names=function() {
                        sapply(widget$getColumns(), gtkTreeViewColumnGetTitle)
                      },
                      set_names =function(value) {
                        ## check length
                        print(get_dim())
                        print(value)
                        m <- get_dim()[2]
                        if(length(value) != m)
                          return()
                        ## adjust for icons
                        if(!is.null(icon_col))
                          value <- c("", value)
                        mapply(gtkTreeViewColumnSetTitle, widget$getColumns(), value)
                      },
                        get_visible=function() {
                          ## return last column in DF
                          DF <- get_model()
                          DF[, ncol(DF), drop=TRUE]
                        },
                        set_visible=function(value, ...) {
                          DF <- get_model()
                          value <- rep(value, length.out=nrow(DF))
                          DF[,ncol(DF)] <- value
                        },
                        ## Handlers
                        add_handler_changed=function(handler, action=NULL, ...) {
                          ## double click to activate
                          add_handler_double_clicked(handler, action=action, ...)
                        },
                        add_handler_clicked=function(handler, action, ...) {
                          ## selection changed
                          if(is_handler(handler)) {
                            o <- gWidgets2:::observer(.self, handler, action)
                            invisible(add_observer(o, change_signal))
                            gSignalConnect(widget$getSelection(), "changed", function(self, ...) {
                              self$notify_observers(signal=change_signal, ...)
                            }, data=.self, user.data.first=TRUE)
                          }
                        },
                        add_handler_double_clicked=function(handler, action, ...) {
                          add_handler("row-activated", handler, action=action, ...)
                        },
                        ##
                        hide_names=function(value) {
                          "Toggle visibility of header"
                          if(value) {
                            ## hide header
                          } else {
                            ## show header
                          }
                        }

                        ))

