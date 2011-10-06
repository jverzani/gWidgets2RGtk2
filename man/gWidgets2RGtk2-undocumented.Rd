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
\alias{.gnotebook.guiWidgetsToolkitRGtk2}
\alias{.gpanedgroup.guiWidgetsToolkitRGtk2}
\alias{.gradio.guiWidgetsToolkitRGtk2}
\alias{.gseparator.guiWidgetsToolkitRGtk2}
\alias{.gslider.guiWidgetsToolkitRGtk2}
\alias{.gspinbutton.guiWidgetsToolkitRGtk2}
\alias{.gstackwidget.guiWidgetsToolkitRGtk2}
\alias{.gstatusbar.guiWidgetsToolkitRGtk2}
\alias{.gtext.guiWidgetsToolkitRGtk2}
\alias{.gtimer.guiWidgetsToolkitRGtk2}
\alias{.gtoolbar.guiWidgetsToolkitRGtk2}
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
}

