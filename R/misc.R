## miscellaneous functions

##' toolkit class for RGtk2
##'
##' @importClassesFrom gWidgets2 guiWidgetsToolkit
##' @export
setClass("guiWidgetsToolkitRGtk2",
         contains="guiWidgetsToolkit")



## Pango Scale for converting between name and numeric value
PangoScale <- c(
               "xx-large"= PANGO_SCALE_XX_LARGE,
               "x-large" = PANGO_SCALE_X_LARGE,
               "large"   = PANGO_SCALE_LARGE,
               "medium"  = PANGO_SCALE_MEDIUM,
               "small"   = PANGO_SCALE_SMALL,
               "x-small" = PANGO_SCALE_X_SMALL,
               "xx-small" = PANGO_SCALE_XX_SMALL
               )

