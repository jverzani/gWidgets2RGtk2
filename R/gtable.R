##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .gtable guiWidgetsToolkitRGtk2
## @export .gtable guiWidgetsToolkitRGtk2
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


## helper to make treeview columns based on type of data
## Need S3 methods defined outside of reference class method, not sure why
make_treeview_column <- function(x, col_no, self) UseMethod("make_treeview_column")
make_treeview_column.default <- function(x, col_no, self) {
  ## Return a tree view column instance to render x, located in 0-based col in model
  cellrenderer <- gtkCellRendererText()
  view_col <- gtkTreeViewColumnNew()
  view_col$setResizable(TRUE)
  view_col$PackStart(cellrenderer, TRUE)
  view_col$AddAttribute(cellrenderer, "text", col_no)

  ## we override built in label
  event_box <- gtkEventBox()
  event_box$SetVisibleWindow(FALSE)
  label <- gtkLabel()
##  event_box$addEvents('all-events-mask')
  event_box$add(label)
  event_box$setAboveChild(TRUE)         # gets events to box

  view_col$setWidget(event_box)
  view_col
}

## need to format value for Dates
make_treeview_column.Date <- function(x, col_no, self) {
  ## Return a tree view column instance to render x, located in 0-based col in model
  cellrenderer <- gtkCellRendererText()
  view_col <- gtkTreeViewColumnNew()
  view_col$setResizable(TRUE)
  view_col$PackStart(cellrenderer, TRUE)
  view_col$AddAttribute(cellrenderer, "text", col_no)

  view_col$setCellDataFunc(cellrenderer, function(vc, cr, model, iter, ...) {
    ## set cell value by formatting
    col <- self$find_col_no(vc)
    row <- as.numeric(model$getPath(iter)$toString()) + 1L
    model <- self$get_model()
    val <- model[row, col]
    cr["text"] <- format(val)
  })
  ## we override built in label
  event_box <- gtkEventBox()
  event_box$SetVisibleWindow(FALSE)
  label <- gtkLabel()
##  event_box$addEvents('all-events-mask')
  event_box$add(label)
  event_box$setAboveChild(TRUE)         # gets events to box

  view_col$setWidget(event_box)
  view_col
}
make_treeview_column.POSIXt <- make_treeview_column.Date

##' Class for gtable widget
##'
##' This GTable class for RGtk2 implements a few additional reference
##' methods: \code{hide_names} to hide the header names;
##' \code{remove_popup_menu} to remove the popup menu;
##' \code{add_popup} to add a popup menu
##' @rdname gWidgets2RGtk2-package
GTable <- setRefClass("GTable",
                      contains="GWidget",
                      fields=list(
                        items="ANY",
                        chosen_col="integer",
                        icon_col="IntegerOrNULL",
                        tooltip_col="IntegerOrNULL"
                        ),
                      methods=list(
                              initialize=function(toolkit=NULL,
                                items=NULL,
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


                                if(missing(items) || length(items) > 1)
                                  widget$SetRulesHint(TRUE)
                                widget$SetEnableSearch(TRUE)
                                widget$setSearchColumn(1 + is.null(icon.col) - 1L)

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
                                           change_signal="changed",
                                           default_expand=TRUE,
                                           default_fill=TRUE,
                                           toolkit=toolkit # needed here for gmenu call later
                                           )


                                set_items(items)

                                add_to_parent(container, .self, ...)

                                ## hack in click events here
                                click_decorator <- function(FUN) {
                                  force(FUN)
                                  f <- function(self, w, e, ...) {
                                    if(e$getButton() == 1) {
                                      if(as.numeric(e$getType()) == GdkEventType['button-press']) {
                                        self$notify_observers(signal="button-press-event")
                                      }
                                      if(as.numeric(e$getType()) == GdkEventType['2button-press']) {
                                        self$notify_observers(signal="2button-press-event")
                                      }
                                    }
                                    FALSE
                                  }
                                  f
                                }
                                connect_to_toolkit_signal("button-press-event", click_decorator)


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
                            treeview_col <- make_treeview_column(DF[,col], col - 1L, .self)
                            widget$insertColumn(treeview_col, pos = -1) # at end
                          })
                          add_popup()
                        },
                        make_icon_column=function() {
                          "Make column for icons"
                          cellrenderer <- gtkCellRendererPixbufNew()
                          view.col <- gtkTreeViewColumnNew()
                          view.col$PackStart(cellrenderer, TRUE)
                          view.col$AddAttribute(cellrenderer, "stock-id", icon_col - 1L)
                          event_box <- gtkEventBox() # need this for consistency
                          label <- gtkLabel()
                          event_box$add(label)
                          view.col$setWidget(event_box)
                          view.col
                        },
                        default_popup_menu=function(col_index) {
                          "Provide default popup menu (passed to gmenu(..., popup=TRUE))"
                          actions <- list(sort_increasing=
                                          gaction("Sort (increasing)", handler=function(h, ...) {
                                            DF <- get_model()
                                            ind <- order(DF[,col_index], decreasing=FALSE)
                                            DF$setFrame(DF[][ind,])
                                          }),
                                          sort_decreasing=
                                          gaction("Sort (decreasing)", handler=function(h, ...) {
                                            DF <- get_model()
                                            ind <- order(DF[,col_index], decreasing=TRUE)
                                            DF$setFrame(DF[][ind,])
                                          }),
                                          gseparator(),
                                          gaction("Rename column", handler=function(h,...) {
                                            cur_nms <- get_names()
                                            out <- ginput("Rename column", text=cur_nms[col_index], parent=widget)
                                            if(nchar(out)) {
                                              cur_nms[col_index] <- out
                                              set_names(cur_nms)
                                            }
                                          })
                                          )
                          actions
                        },
                        add_popup_menu=function(menulist) {
                          f <- function(...) menulist
                          add_popup(f)
                        },
                        ## perhaps needs optimization, loops over all columns so n^2 stuff here.
                        find_col_no = function(view.col) {
                          ind <- which(sapply(widget$getColumns(), function(i) identical(i, view.col)))
                          ind - !is.null(icon_col)
                        },
                        add_popup=function(menu_fun=NULL) {
                          "Add a popup menu to the columns. Function should generate list of actions, ..."
                          if(is.null(menu_fun))
                            menu_fun <- .self$default_popup_menu


                          sapply(get_view_columns(), function(view.col) {
                            view.col$setClickable(TRUE)
                            col_no <- find_col_no(view.col)
                            popup <- gmenu(menu_fun(col_no), popup=TRUE, toolkit=toolkit)
                            event_box <- view.col$getWidget()
                            ## This is a *real* hack, as the following doesn't
                            ## work when I use event_box. NOt sure why
                            ## not, seems like the right combination of
                            ## arguments is given
                            btn <- event_box$getParent()$getParent()$getParent()
                            id <- gSignalConnect(btn, "button-press-event", f=function(w, e, ...) {
                              if(e$button == 1 && e$type == GdkEventType['button-press']) {
                                popup$widget$popup(button=e$button, activate.time=e$time)
                              }
                              FALSE
                            })
                          btn$setData("popup_id", id)
                          })
                        },
                        remove_popup_menu=function() {
                          "remove popup menu from column headers"

                          sapply(get_view_columns(), function(view.col) {
                            view.col$setClickable(FALSE)
                            btn <- view.col$getWidget()$getParent()$getParent()$getParent()
                            if(!is.null(id <- btn$getData("popup_id")))
                              gSignalHandlerDisconnect(btn, id)
                          })
                        },
                        set_selection_mode=function(mode=c("none","single","browse", "multiple", "extended")) {
                          "Helper: Set the selection mode"
                          sel_model <- widget$getSelection()
                          sel_model$setMode(GtkSelectionMode[match.arg(mode)])
                        },
                        not_these=function() {
                          "Helper: Remove these indices due to icon_col, ..."
                          x <- unlist(list(icon_col, tooltip_col))
                          if(is.null(x))
                            x <- integer(0)
                          x
                        },
                        get_valid_columns=function() {
                          "Helper: get column indices less those for icons, tooltips, visible"
                          DF <- get_model()
                          if(!is(DF, "RGtkDataFrame"))
                            return(NULL)
                          j <- seq_len(dim(DF)[2] - 1L) # last col is ..visible
                          setdiff(j, not_these())
                        },
                        get_view_columns=function() {
                          "Helper: get non-icon columns to iterate over"
                          columns <- widget$getColumns()
                          if(!is.null(icon_col))
                            columns <- columns[-1]
                          columns
                        },
                        get_selected=function() {
                          "Get selected indices or numeric(0)"
                          sel_model <- widget$getSelection()
                          x <- sapply(sel_model$getSelectedRows()$retval, gtkTreePathToString)
                          if(is.null(x))
                            return(integer(0))
                          x <- as.numeric(x) + 1L # hide, deleted

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
                          "Helper: get rGtkDataFrame model, which is filtered"
                          m <- widget$getModel()
                          if(is(m, "GtkTreeModelFilter"))
                            m$getModel() # get past filter
                          else
                            NULL
                        },

                        ## implement basic methods
                        get_value=function(drop=TRUE, ...) {
                          "Get selected values by value (or character(0))"

                          idx <- get_selected()
                          idx <- which(get_visible())[idx]


                          vals <- get_items(drop=FALSE)[idx, , drop=FALSE]
                          if(getWithDefault(drop, TRUE))
                            vals[, chosen_col, drop=TRUE]
                          else
                            vals
                        },
                        set_value=function(value, ...) {
                          "Set selected values by vector matching chosen.col, unless an integer"
                          block_handlers()
                          vals <- get_items(drop=TRUE)
                          if(is.numeric(value) && !is.numeric(vals))
                            ind <- value
                          else
                            ind <- match(value, vals)
                          ind <- ind[!is.na(ind)]
                          if(length(ind) == 0)
                            return() ## no match
                          set_index(ind)
                          unblock_handlers()
                        },
                        get_index = function(...) {
                          "Get index of selected rows or integer(0)"
                          idx <- get_selected()
                          idx <- which(get_visible())[idx]
                        },
                        set_index = function(value,...) {
                          "set selected values in value. integer(0) or 0L clears selection"
                          if(length(value) == 0 || (length(value) == 1L && value < 1))
                            widget$getSelection()$unselectAll() # clear selection if not >= 1
                          else {
                            ## selected wants actual for filtered
                            value <- match(value, which(get_visible()))
                            value <- value[!is.na(value)]
                            set_selected(as.integer(value) - 1L)
                          }
                        },
                        get_items = function(i, j, ..., drop=TRUE) {
                          DF <- get_model()[]
                          if(!is.data.frame(DF) && is.list(DF))
                            DF <- as.data.frame(DF, stringsAsFactors=FALSE)

                          DF <- DF[, get_valid_columns(), drop=FALSE]
                          names(DF) <- get_names()
                          ## we possibly drop out some stuff
                          DF[i,j, drop=getWithDefault(drop, TRUE)]
                        },
                        set_items = function(value, i, j, ...) {
                          block_handlers()
                          on.exit(unblock_handlers())
                          if(missing(i) && missing(j)) {
                            ## set a new data frame model
                            ## we shove in ..visible for the last column q
                            if(!is(value, "data.frame")) {
                              if (is.vector(value))
                                value <- data.frame(Values=value, stringsAsFactors=FALSE)
                              else if(is.matrix(value))
                                value <- data.frame(value, stringsAsFactors=FALSE)
                            }
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
                        set_focus = function(value) {
                          "If we can focus, do so, then raise"
                          if(value) {
                            widget$grabFocus()
                            widget$getWindow()$raise()
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
                          sapply(get_view_columns(), function(col) {
                            label <- col$getWidget()$getChild()
                            label$getLabel()
                          })
                        },
                        set_names =function(value) {
                          ## check length
                          m <- get_dim()[2]
                          if(length(value) != m)
                            return()

                          f <- function(col, nm) {
                            label <- col$getWidget()$getChild()
                            label$setLabel(nm)
                          }
                          mapply(f, get_view_columns(), value)
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
                        set_size=function(value, ...) {
                          "set size also has possibility of column widths"
                          if(is.list(value)) {
                            col_widths <- value$column.widths
                            value$column.widths <- NULL
                            set_column_widths(col_widths)
                            value <- c(width=value$width, height=value$height) # make vector, not list
                          }
                          callSuper(value, ...)
                        },
                        set_column_widths=function(value) {
                          if(length(value) == get_dim()[2]) {
                            cols <-get_view_columns()
                            mapply(gtkTreeViewColumnSetMinWidth, cols, value)
                          }
                        },
                        ## Handlers
                        add_handler_changed=function(handler, action=NULL, ...) {
                          add_handler("row-activated", handler, action, ...)
                        },
                        add_handler_selection_changed=function(handler, action=NULL, ...) {
                          ## selection changed
                          if(is_handler(handler)) {
                            o <- gWidgets2:::observer(.self, handler, action)
                            invisible(add_observer(o, change_signal))
                            gSignalConnect(widget$getSelection(), "changed", function(self, ...) {
                              self$notify_observers(signal=change_signal, ...)
                            }, data=.self, user.data.first=TRUE)
                          }
                        },
                        add_handler_clicked=function(handler, action, ...) {
                          if(!is_handler(handler))
                            return()
                          o <- gWidgets2:::observer(.self, handler, action)
                          invisible(add_observer(o, "button-press-event"))
                        },
                        add_handler_double_clicked=function(handler, action, ...) {
                          ## There is an oddity here. When using row-activated it does as desired unless
                          ## we also have addHandlerChanged called in which case this is always called. So
                          ## we bypass and do button and mouse events as they arise
                          ## add_handler("row-activated", handler, action=action, ...)

                           if(!is_handler(handler))
                             return()
                           o <- gWidgets2:::observer(.self, handler, action)
                           invisible(add_observer(o, "2button-press-event"))
                        },
                        ##
                        hide_names=function(value) {
                          "adjust visibility of header"
                          widget$setHeadersVisible(!as.logical(value))
                        }

                        ))
