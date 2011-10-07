##' @include GWidget.R
NULL

##' Toolkit constructor
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
.gmenu.guiWidgetsToolkitRGtk2 <-  function(toolkit,
                                           menu.list=list(),
                                           popup=FALSE,
                                           container = NULL,
                                           ... ) {
  if(popup)
    GMenuPopup$new(toolkit, menu.list=menu.list, container = container, ...)
  else
    GMenuBar$new(toolkit, menu.list=menu.list, container = container, ...)
}


## XXX
GMenuBar <- setRefClass("GMenuBar",
                     contains="GWidget",
                     fields=list(
                       menu_list="list"
                       ),
                     methods=list(
                       initialize=function(toolkit=NULL,
                         menu.list=list(),
                         container=NULL, ...) {

                         widget <<- gtkMenuBarNew()
                         initFields(block=widget)

                         menu_list <<- list()
                         set_value(menu.list)
                         
                         add_to_parent(container, .self, ...)
                         
                         callSuper(toolkit)
                       },
                       ## add items
                       add_menu_items=function(sub_menu, items) {
                         sapply(items, function(item) {
                           ## do dispatch based on class
                           if(is(item, "list")) {
                             ## get name by looking up and matching
                             add_submenu(sub_menu, item, nm=names(Filter(function(x) identical(x, item), items)))
                           } else if(is(item, "GAction")) {
                             add_gaction_menuitem(sub_menu, item)
                           } else if(is(item, "GSeparator")) {
                             add_gseparator_menuitem(sub_menu, item)
                           } else if(is(item, "GRadio")) {
                             add_radio_menuitem(sub_menu, item)
                           } else if(is(item, "GCheckbox")) {
                             add_checkbutton_menuitem(sub_menu, item)
                           } else {
                             add_widget_menuitem(sub_menu, item)
                           }
                         })
                         sub_menu$show()
                       },
                       add_submenu=function(sub_menu, items, nm) {
                         item <- gtkMenuItem(nm)
                         sub_menu$append(item)
                         sub_menu <- gtkMenu()
                         add_menu_items(sub_menu, items)
                         item$setSubmenu(sub_menu)
                       },
                       add_gaction_menuitem=function(sub_menu, item) {
                         action <- item$widget
                         item <- gtkImageMenuItem("")
                         if("always-show-image" %in% names(item))
                           item['always-show-image'] <- TRUE
                         sub_menu$append(item)
                         item$setRelatedAction(action)
                       },
                       add_gseparator_menuitem=function(sub_menu, item) {
                         item <- gtkSeparatorMenuItem()
                         sub_menu$append(item)
                       },
                       add_radio_menuitem=function(sub_menu, item) {
                         rb <- item
                         cur <- svalue(rb, index=TRUE)
                         nms <- rb[]
                         ## do this the *hard* way using check menu items, simple way failed
                         ## as I didn't know how to start gtkRadioMenuItem group off.
                         rbs <- sapply(nms, function(i) {
                           item <- gtkCheckMenuItemNewWithLabel(i)
                           item$setDrawAsRadio(TRUE)
                           sub_menu$append(item)
                           item
                         })
                         rbs[[cur]]$setActive(TRUE)
                         sapply(rbs, gSignalConnect, signal="toggled", f=function(w, ...) {
                           if(w$getActive()) {
                             ind <- sapply(rbs, identical, w)
                             sapply(which(!ind), function(i) rbs[[i]]$setActive(FALSE))
                             svalue(rb, index=TRUE) <- which(ind)
                           }
                         })
                       },
                       add_checkbutton_menuitem=function(sub_menu, item) {
                         cb <- item
                         item <- gtkCheckMenuItemNewWithLabel(cb[1])
                         item$setActive(svalue(cb))
                         gSignalConnect(item, "toggled", function(w, ...) {
                           cur <- w$getActive()
                           svalue(cb) <- cur
                         })
                         sub_menu$append(item)
                       },
                       add_widget_menuitem=function(sub_menu, item) {
                         "Add an arbitrary widget, though likely not a good thing to do."
                         mitem <- gtkMenuItemNew()
                         mitem$add(getBlock(item))
                         sub_menu$append(mitem)
                       },
                       clear_menubar=function() {
                         "Clear out menu items"
                         sapply(rev(widget$getChildren()), widget$remove)
                         widget$hide()
                       },
                       ##
                       get_value=function( ...) {
                         menu_list
                       },
                       set_value=function(value, ...) {
                         clear_menubar()
                         menu_list <<- value
                         add_menu_items(widget, value)
                         widget$show()
                       },
                       append_value=function(items) {
                         "Append to menu list"
                         menu_list <<- merge(menu_list, items)
                         add_menu_items(widget, items)
                       }
                       ))


##' Popup class
GMenuPopup <- setRefClass("GMenuPopup",
                            contains="GMenuBar",
                            methods=list(
                              initialize=function(toolkit=NULL,
                                menu.list=list(),
                                container=NULL,
                                ...) {
                                ## could tighten up by callSuper
                                widget <<- gtkMenuBarNew()
                                initFields(block=widget)
                                menu_list <<- menu.list
                                add_menu_items(widget, menu.list)
                                add_to_parent(container, .self, ...)
                                callSuper(toolkit)
                              }
                              ))
