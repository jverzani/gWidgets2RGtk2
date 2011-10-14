\name{.gbutton.guiWidgetsToolkitRGtk2}
\alias{.gaction.guiWidgetsToolkitRGtk2}
\alias{.gbutton.guiWidgetsToolkitRGtk2}
\alias{.gcalendar.guiWidgetsToolkitRGtk2}
\alias{.gcheckbox.guiWidgetsToolkitRGtk2}
\alias{.gcheckboxgroup.guiWidgetsToolkitRGtk2}
\alias{.gcombobox.guiWidgetsToolkitRGtk2}
\alias{.gedit.guiWidgetsToolkitRGtk2}
\alias{.gexpandgroup.guiWidgetsToolkitRGtk2}
\alias{.gfile.guiWidgetsToolkitRGtk2}
\alias{.gfilebrowse.guiWidgetsToolkitRGtk2}
\alias{.gframe.guiWidgetsToolkitRGtk2}
\alias{.ggraphics.guiWidgetsToolkitRGtk2}
\alias{.gimage.guiWidgetsToolkitRGtk2}
\alias{.glabel.guiWidgetsToolkitRGtk2}
\alias{.glayout.guiWidgetsToolkitRGtk2}
\alias{.gmenu.guiWidgetsToolkitRGtk2}
\alias{.gnotebook.guiWidgetsToolkitRGtk2}
\alias{.gpanedgroup.guiWidgetsToolkitRGtk2}
\alias{.gradio.guiWidgetsToolkitRGtk2}
\alias{.gseparator.guiWidgetsToolkitRGtk2}
\alias{.gslider.guiWidgetsToolkitRGtk2}
\alias{.gspinbutton.guiWidgetsToolkitRGtk2}
\alias{.gstackwidget.guiWidgetsToolkitRGtk2}
\alias{.gstatusbar.guiWidgetsToolkitRGtk2}
\alias{.gtable.guiWidgetsToolkitRGtk2}
\alias{.gtext.guiWidgetsToolkitRGtk2}
\alias{.gtimer.guiWidgetsToolkitRGtk2}
\alias{.gtoolbar.guiWidgetsToolkitRGtk2}
\alias{.gtree.guiWidgetsToolkitRGtk2}
\title{Toolkit button constructor}
\usage{
  .gbutton.guiWidgetsToolkitRGtk2(toolkit, text, handler,
  action, container, ...)

  .gframe.guiWidgetsToolkitRGtk2(toolkit, text, markup,
  pos, horizontal = TRUE, container = NULL, ...)

  .glabel.guiWidgetsToolkitRGtk2(toolkit, text = "", markup
  = FALSE, editable = FALSE, handler = NULL, action = NULL,
  container = NULL, ...)

  .gtimer.guiWidgetsToolkitRGtk2(toolkit, ms, FUN, data =
  NULL, one.shot = FALSE, start = TRUE)

  .gedit.guiWidgetsToolkitRGtk2(toolkit, text = "", width =
  25, coerce.with = NULL, initial.msg = initial.msg,
  handler = NULL, action = NULL, container = NULL, ...)

  .gcheckbox.guiWidgetsToolkitRGtk2(toolkit, text, checked
  = FALSE, use.togglebutton = FALSE, handler = NULL, action
  = NULL, container = NULL, ...)

  .gcheckboxgroup.guiWidgetsToolkitRGtk2(toolkit = NULL,
  items, checked = FALSE, horizontal = FALSE, use.table =
  FALSE, handler = NULL, action = NULL, container = NULL,
  ...)

  .gradio.guiWidgetsToolkitRGtk2(toolkit, items, selected =
  1, horizontal = FALSE, handler = NULL, action = NULL,
  container = NULL, ...)

  .gnotebook.guiWidgetsToolkitRGtk2(toolkit, tab.pos = 3,
  container = NULL, ...)

  .gslider.guiWidgetsToolkitRGtk2(toolkit, from = 0, to =
  100, by = 1, value = from, horizontal = TRUE, handler =
  NULL, action = NULL, container = NULL, ...)

  .gspinbutton.guiWidgetsToolkitRGtk2(toolkit, from = 0, to
  = 10, by = 1, value = from, digits = 0, handler = NULL,
  action = NULL, container = NULL, ...)

  .gexpandgroup.guiWidgetsToolkitRGtk2(toolkit, text,
  markup, horizontal = TRUE, handler = NULL, action = NULL,
  container = NULL, ...)

  .gstackwidget.guiWidgetsToolkitRGtk2(toolkit, container =
  NULL, ...)

  .glayout.guiWidgetsToolkitRGtk2(toolkit, homogeneous =
  FALSE, spacing = 10, container = NULL, ...)

  .gpanedgroup.guiWidgetsToolkitRGtk2(toolkit, horizontal =
  TRUE, container = NULL, ...)

  .gseparator.guiWidgetsToolkitRGtk2(toolkit, horizontal =
  TRUE, container = NULL, ...)

  .gtext.guiWidgetsToolkitRGtk2(toolkit, text = NULL, width
  = NULL, height = 300, font.attr = NULL, wrap = TRUE,
  handler = NULL, action = NULL, container = NULL, ...)

  .gcombobox.guiWidgetsToolkitRGtk2(toolkit, items,
  selected = 1, editable = FALSE, coerce.with = NULL,
  handler = NULL, action = NULL, container = NULL, ...)

  .gaction.guiWidgetsToolkitRGtk2(toolkit, label, tooltip =
  NULL, icon = NULL, key.accel = NULL, handler = NULL,
  action = NULL, parent = NULL, ...)

  .gcalendar.guiWidgetsToolkitRGtk2(toolkit, text = "",
  format = "\%Y-\%m-\%d", handler = NULL, action = NULL,
  container = NULL, ...)

  .gfile.guiWidgetsToolkitRGtk2(toolkit, text = "", type =
  c("open", "save", "selectdir"), initial.filename = NULL,
  filter = list(), multi = FALSE, ...)

  .gfilebrowse.guiWidgetsToolkitRGtk2(toolkit, text = "",
  type = c("open", "save", "selectdir"), initial.filename =
  NULL, filter = list(), quote = TRUE, handler = NULL,
  action = NULL, container = NULL, ...)

  .gstatusbar.guiWidgetsToolkitRGtk2(toolkit, text = "",
  container = NULL, ...)

  .ggraphics.guiWidgetsToolkitRGtk2(toolkit, width = dpi *
  6, height = dpi * 6, dpi = 75, ps = 12, handler = NULL,
  action = NULL, container = NULL, ...)

  .gimage.guiWidgetsToolkitRGtk2(toolkit, filename = "",
  dirname = "", stock.id = NULL, size = "", handler = NULL,
  action = NULL, container = NULL, ...)

  .gtoolbar.guiWidgetsToolkitRGtk2(toolkit, toolbar.list =
  list(), style = c("both", "icons", "text", "both-horiz"),
  container = NULL, ...)

  .gmenu.guiWidgetsToolkitRGtk2(toolkit, menu.list =
  list(), popup = FALSE, container = NULL, ...)

  .gtable.guiWidgetsToolkitRGtk2(toolkit, items, multiple =
  FALSE, chosen.col = 1, icon.col = NULL, tooltip.col =
  NULL, handler = NULL, action = NULL, container = NULL,
  ...)

  .gtree.guiWidgetsToolkitRGtk2(toolkit, offspring = NULL,
  offspring.data = NULL, chosen.col = 1, offspring.col = 2,
  icon.col = NULL, tooltip.col = NULL, multiple = FALSE,
  handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

  \item{text}{label text. If text matches a stock icon
  name, that is used as well}

  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgetsWWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations.}

  \item{markup}{logical. If toolkit supports markup, this
  indicates it will be used}

  \item{value}{integer amount of spacing}

  \item{menu.list}{A list defining the menu structure.
  Named sub lists determine the submenu titles and
  structure. The list may have components of class:
  \code{GAction}, mapped to a button; \code{GSeparator},
  mapped to a horizontal separator; \code{GRadio}, mapped
  to linked buttons; or \code{GCheckbox}, mapped to a
  checkbox button.}

  \item{popup}{logical. If true, make a popup window to be
  added through a handler call}

  \item{items}{data.frame specifies items for selection.
  May be a vector, matrix or data frame}

  \item{multiple}{logical allow multiple selectino}

  \item{chosen.col}{which value from the row is returned by
  selection}

  \item{icon.col}{NULL or integer. If latter, specifies
  column containing stock icon}

  \item{tooltip.col}{NULL or integer. If latter, specifies
  column containing tooltip}

  \item{offspring}{function. A function passed values
  \code{path} and \code{data}, the latter from
  \code{offspring.data}. The path is the current position
  of the parent item using the named keys from the chosen
  column.}

  \item{offspring.data}{}

  \item{offspring.col}{integer or column name. Points to
  column containing logical values indicating if a row has
  offspring.}
}
\description{
  Toolkit button constructor

  gframe constructor

  Toolkit label constructor

  S3 method for gtimer

  Toolkit gedit constructor

  Toolkit XXX constructor

  Toolkit XXX constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit XXX constructor

  toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  toolkit implementation

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit implementation

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor

  Toolkit constructor
}

