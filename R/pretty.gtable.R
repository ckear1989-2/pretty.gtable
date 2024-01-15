
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

accepted_outf <- c(
  "jpg",
  "jpeg"
)

check_arg_outf <- function(outf) {
  if(!is.null(outf)) {
    file_ext_match <- function(x) {
      grepl(x, tools::file_ext(outf), fixed=TRUE)
    }
    if(!any(unlist(lapply(accepted_outf, file_ext_match)))) {
      stop(paste("file extension must be in (", paste(accepted_outf, collapse=", "), ") not", tools::file_ext(outf)))
    }
  }
  NULL
}

print_object <- function(a.gt, outf) {
  NULL
}

#' @export
pretty_gtable <- function(data, options=NULL, outf=NULL) {
  check_arg_outf(outf)
  if (!("data.frame" %in% class(data)) & !("data.table" %in% class(data))) {
    stop(paste("provided data should be data.frame or data.table not", class(data)))
  }
  a.gt <- gridExtra::tableGrob(data, theme=table.theme(16), rows=NULL)
  for(row in options$rows) {
    set.row.border(a.gt, options$rows$row)
  }
  if(!is.null(outf)) {
    print_object(a.gt, outf)
  }
  a.gt
}
