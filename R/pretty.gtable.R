
padding <- function() grid::unit.c(grid::unit(2, "mm"), grid::unit(2, "mm"))

table.theme <- function(fs) {
    gridExtra::ttheme_default(
    core=list(
      fg_params=list(fontsize=fs, just="left"),
      padding=padding()
    ),
    rowhead=list(
      fg_params=list(fontsize=fs, fontface="bold", just="left"),
      padding=padding()
    ),
    colhead=list(
      fg_params=list(fontsize=fs, fontface="bold", just="left"),
      padding=padding()
    )
  )
}

set.row.border <- function(obj, row, color) {
  # row + 1 because of header
  gtable::gtable_add_grob(obj, grobs=grid::rectGrob(gp=grid::gpar(fill=color, lwd=2, col=color, alpha=0.5)), t=(row+1.02), b=(row+1.98), l=1.02, r=(ncol(obj)+1))
}

#' @export
pretty.gtable <- function(data, options) {
    if (!("data.frame" %in% class(data)) & !("data.table" %in% class(data))) {
      stop(paste("provided data should be data.frame or data.table not", class(data)))
    }
    a.gt <- gridExtra::tableGrob(data, theme=table.theme(16), rows=NULL)
    for(row in options$rows) {
        set.row.border(a.gt, options$rows$row)
    }
    a.gt
}
