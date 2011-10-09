##' @include misc.R
NULL

##' add stock icons
##'
##' @export
##' @rdname gWidgets-undocumented
.addStockIcons.guiWidgetsToolkitRGtk2 <- function(toolkit, iconNames, iconFiles,... ) {
  .GWidgetsRGtk2Icons$add_to_gtk_stock_icons(iconNames, iconFiles)
}

##' Returns list of stock ids
##'
##' @export
##' @rdname gWidgets-undocumented
.getStockIcons.guiWidgetsToolkitRGtk2 <- function(toolkit, ...) {
  lst <- gtkStockListIds()
  sapply(unlist(lst), identity, simplify=FALSE)
}

##' return stock id
##'
##' @export
##' @rdname gWidgets-undocumented
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
##' @rdname gWidgets-undocumented
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
                                 
