##' @include misc.R
NULL

##' add stock icons
##'
##' @export
##' @inheritParams gWidgets2::addStockIcons
##' @rdname gWidgets2RGtk2-undocumented
##' @method .addStockIcons guiWidgetsToolkitRGtk2
##' @S3method .addStockIcons guiWidgetsToolkitRGtk2
.addStockIcons.guiWidgetsToolkitRGtk2 <- function(toolkit, iconNames, iconFiles,... ) {
  .GWidgetsRGtk2Icons$add_to_gtk_stock_icons(iconNames, iconFiles)
}

##' Returns list of stock ids
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
##' @method .getStockIcons guiWidgetsToolkitRGtk2
##' @S3method .getStockIcons guiWidgetsToolkitRGtk2
.getStockIcons.guiWidgetsToolkitRGtk2 <- function(toolkit, ...) {
  lst <- gtkStockListIds()
  sapply(unlist(lst), identity, simplify=FALSE)
}

##' return stock id
##'
##' @param name name of icon
##' @export
##' @usage \method{.getStockIconByName}{guiWidgetsToolkitRGtk2}(toolkit,name, ...)
##' @rdname gWidgets2RGtk2-undocumented
##' @method .getStockIconByName guiWidgetsToolkitRGtk2
##' @S3method .getStockIconByName guiWidgetsToolkitRGtk2
.getStockIconByName.guiWidgetsToolkitRGtk2 <- function(toolkit, name, ...) {
  icons <- getStockIcons(toolkit)

  sapply(name, function(icon) {
    tmp <- icons[[icon, exact=TRUE]]
    if(is.null(tmp))
      tmp <- icons[[sprintf("gtk-%s", icon)]]
    if(is.null(tmp))
      tmp <- icons[[sprintf("gw-%s", icon)]]
    if(is.null(tmp))
      tmp <- ""
    tmp
  })
}


##' helper function
##'
##' @export
##' @rdname gWidgets2RGtk2-undocumented
addToGtkStockIcons <- function(iconNames, iconFiles) {

  iconfactory <- gtkIconFactoryNew()
  for(i in seq_along(iconNames)) {
    iconsource <- gtkIconSourceNew()
    iconsource$SetFilename(iconFiles[i])
    
    iconset <- gtkIconSetNew()
    iconset$AddSource(iconsource)
    
    stockName <- paste("gw-", iconNames[i], sep="")
    
    iconfactory$Add(stockName, iconset)
    
    items <- list(test=list(stockName, iconNames[i],"","",""))
    gtkStockAdd(items)
  }
  
  iconfactory$AddDefault()
  invisible(TRUE)
}

GWidgetsRGtk2Icons <- setRefClass("GWidgetsRGtk2Icons",
                                  contains="GWidgets2Icons",
                                  methods=list(
                                    update_icons=function() {
                                      callSuper() # makes icons in icons
                                      add_to_gtk_stock_icons(names(icons), icons)
                                    },
                                    add_to_gtk_stock_icons = function(iconNames, iconFiles) {
                                      iconfactory <- gtkIconFactoryNew()
                                      for(i in seq_along(iconNames)) {
                                        iconsource <- gtkIconSourceNew()
                                        iconsource$SetFilename(iconFiles[i])
                                        
                                        iconset <- gtkIconSetNew()
                                        iconset$AddSource(iconsource)
                                        
                                        stockName <- paste("gw-", iconNames[i], sep="")
                                        
                                        iconfactory$Add(stockName, iconset)
                                        
                                        items <- list(test=list(stockName, iconNames[i],"","",""))
                                        gtkStockAdd(items)
                                      }
                                      
                                      iconfactory$AddDefault()
                                      invisible(TRUE)
                                    }
                                    
                                    ))

.GWidgetsRGtk2Icons <- GWidgetsRGtk2Icons$new()

load_gwidget_icons <- function() {
  ## add the icons
  ## we use xpm icons gimp can convert
  iconFileNames <- list.files(system.file("images", package="gWidgets2"), full.names=TRUE)
  iconFileNames <- Filter(function(x) grepl("\\.gif$", x), iconFileNames)
  iconNames <- basename(iconFileNames)
  iconNames <- gsub("\\.gif$","",iconNames)
  .GWidgetsRGtk2Icons$add_to_gtk_stock_icons(iconNames, iconFileNames)
}

##################################################

##' return stock id from object
##'
##' @param obj R object to get icon from
##' @export
##' @usage \method{.stockIconFromObject}{guiWidgetsToolkitRGtk2}(toolkit,obj, ...)
##' @rdname gWidgets2RGtk2-undocumented
##' @method .stockIconFromObject guiWidgetsToolkitRGtk2
##' @S3method .stockIconFromObject guiWidgetsToolkitRGtk2
.stockIconFromObject.guiWidgetsToolkitRGtk2 <- function(toolkit, obj, ...) {
  icon_for_object <- function(x) UseMethod("icon_for_object")
  icon_for_object.default <- function(x) "gw-symbol_dot"
  icon_for_object.numeric <- function(x) "gtk-numeric"
  icon_for_object.numeric <- function(x) "gw-numeric"
  icon_for_object.factor <- function(x) "gw-factor"
  icon_for_object.character <- function(x) "gw-character"
  icon_for_object.function <- function(x) "fw-function"
  icon_for_object.data.frame <- function(x) "gw-dataframe"
  
  icon_for_object(obj)
}
