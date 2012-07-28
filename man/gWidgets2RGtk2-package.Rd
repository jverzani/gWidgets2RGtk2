\docType{package}
\name{gWidgets2RGtk2-package}
\alias{GButton}
\alias{GComponent}
\alias{GContainer}
\alias{GDf}
\alias{GLabel}
\alias{GProgressBar}
\alias{GTable}
\alias{GTimer}
\alias{GWidget}
\alias{GWidgetWithItems}
\alias{gWidgets2RGtk2-package}
\title{gWidgets2Rtk2}
\description{
  The \pkg{gWidgets2} package provides a programming
  interface for making graphical user interfaces within R.
  The package is a rewrite of the \pkg{gWidgets} package,
  introducing a few external changes but a significant
  number of internal ones. The package relies on one of
  several underlying toolkit packages providing access to
  the graphical libraries. This package provides the
  implementation to interface with the underlying
  \pkg{RGtk2} package.

  GComponent is a parent class for both GContainer and
  GWidget and inherits its primary interface from
  gWidgets2::BasicToolkitInterface.

  GContainer is the base class for container objects. The
  main method is \code{add_child}, but there is also
  book-keepingn code to keep track of the child components
  of the container

  GWidget is the Base class for widget objects

  GWidgetWithItems is Base class for selection widgets
  based on a set of items. The key here is the handlers
  apply to each item, but the handler is assigned to the
  class member.

  For RGtk2, the GButton class has the extra reference
  method \code{set_border}. The \code{border} argument has
  been deprecated.

  This GTable class for RGtk2 implements a few additional
  reference methods: \code{hide_names} to hide the header
  names; \code{remove_popup_menu} to remove the popup menu;
  \code{add_popup} to add a popup menu

  For \code{RGtk2}, a GDf object has several methods
  defined for it that are toolkit specific, but may be
  useful. For example, the columns may be made editable or
  non editable (\code{block_editable_column} and
  \code{unblock_editable_column}) (accessed through
  \code{editable<-}); the headers can be hidden/shown
  through the method \code{hide_names(boolean)}; the
  rownames can be hidden/shown through themethod
  \code{hide_row_names(boolean)}; the popup menus for the
  headers can be removed (\code{remove_popup_menu}) and
  customized (\code{add_popup}); similarly the cell popup
  can be (\code{remove_cell_popup} and
  \code{add_cell_popup}).

  The label class for RGtk2 has the extra method
  \code{set_angle} to set the angle for the text. A value
  of 90 will rotate ccw to read bottom to top

  The main reference methods \code{GTimer} are
  \code{start_timer} and \code{stop_timer}

  For RGtk2, the Gprogressbar class has the extra reference
  method \code{set_border}. The \code{border} argument has
  been deprecated.
}
\author{
  John Verzani \email{jverzani@gmail.com}

  Maintainer: John Verzani \email{jverzani@gmail.com}
}
\keyword{package}

