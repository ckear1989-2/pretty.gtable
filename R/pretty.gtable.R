padding <- function() grid::unit.c(grid::unit(2, "mm"), grid::unit(2, "mm"))

#' @import gridExtra
table.theme <- function(fs) {
  gridExtra::ttheme_default(
    core = list(
      fg_params = list(fontsize = fs, just = "left"),
      padding = padding()
    ),
    rowhead = list(
      fg_params = list(fontsize = fs, fontface = "bold", just = "left"),
      padding = padding()
    ),
    colhead = list(
      fg_params = list(fontsize = fs, fontface = "bold", just = "left"),
      padding = padding()
    )
  )
}

find_cell <- function(table, row, col, name = "core-fg") {
  l <- table$layout
  which(l$t == row & l$l == col & l$name == name)
}

#' @import gtable
set.row.border <- function(obj, row, color) {
  # row + 1 because of header
  gtable::gtable_add_grob(obj, grobs = grid::rectGrob(gp = grid::gpar(fill = color, lwd = 2, col = color, alpha = 0.5)), t = (row + 1.02), b = (row + 1.98), l = 1.02, r = (ncol(obj) + 1))
}

accepted_outf <- c(
  "jpg",
  "jpeg",
  "pdf",
  "png"
)

check_arg_outf <- function(outf) {
  if (!is.null(outf)) {
    if (!any(tools::file_ext(outf) %in% accepted_outf)) {
      stop(paste("file extension must be in (", paste(accepted_outf, collapse = ", "), ") not", tools::file_ext(outf)))
    }
  }
  NULL
}

print_object <- function(a.gt, options, outf) {
  outfx <- tools::file_ext(outf)
  if (is.null(options$width)) {
    width <- 480
  } else {
    width <- options$width
  }
  if (is.null(options$height)) {
    height <- 480
  } else {
    height <- options$height
  }
  if (outfx %in% c("jpg", "jpeg")) {
    grDevices::jpeg(outf, width = width, height = height)
  } else if (outfx == "png") {
    grDevices::png(outf, width = width, height = height)
  } else if (outfx == "pdf") {
    if (is.null(options$width)) {
      width <- 7
    } else {
      width <- options$width
    }
    if (is.null(options$height)) {
      height <- 7
    } else {
      height <- options$height
    }
    grDevices::pdf(outf, width = width, height = height)
  } else {
    stop(paste("file extension", outfx, "not supported."))
  }
  gridExtra::grid.arrange(a.gt)
  grDevices::dev.off()
  NULL
}

#' @importFrom grid gpar
colorise.tableGrob <- function(obj, dt, cols, fs = 12) {
  # make sure there is a color for every row
  cols <- rep(cols, nrow(dt))[1:(nrow(dt) + 1)]
  # set all font sizes
  for (x in 1:(ncol(dt) + 1)) {
    for (y in 1:(nrow(dt) + 1)) {
      for (fg in c("colhead-fg", "rowhead-fg", "core-fg")) {
        ind <- find_cell(obj, y, x, fg)
        if (!length(ind) > 0) {
          next
        } else {
          obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize = fs, just = "left")
        }
      }
    }
  }
  # bold first row
  for (x in 1:(ncol(dt) + 1)) {
    for (fg in c("core-fg", "colhead-fg")) {
      ind <- find_cell(obj, 1, x, fg)
      if (!length(ind) > 0) {
        next
      } else {
        obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize = fs, fontface = "bold", just = "left")
      }
    }
  }
  # bold first column
  for (x in 1:(nrow(dt) + 1)) {
    for (fg in c("core-fg", "rowhead-fg")) {
      ind <- find_cell(obj, x, 1, fg)
      if (!length(ind) > 0) {
        next
      } else {
        obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fontsize = fs, fontface = "bold", just = "left")
      }
    }
  }
  # alternate colors
  for (x in 1:(ncol(dt) + 1)) {
    for (y in 1:(nrow(dt) + 1)) {
      for (bg in c("colhead-bg", "rowhead-bg", "core-bg")) {
        ind <- find_cell(obj, y, x, bg)
        if (!length(ind) > 0) {
          next
        } else {
          fill <- cols[y]
        }
        obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fill = fill, col = "white", just = "left")
      }
    }
  }
  obj
}

colorise.tableGrob.cols <- function(obj, dt, col1, col2) {
  # alternate colors
  for (x in 1:(ncol(dt) + 1)) {
    for (y in 1:(nrow(dt) + 1)) {
      for (bg in c("colhead-bg", "rowhead-bg", "core-bg")) {
        ind <- find_cell(obj, y, x, bg)
        if (!length(ind) > 0) {
          next
        } else {
          if ((x %% 2) == 0) {
            fill <- col1
          } else {
            fill <- col2
          }
          obj$grobs[ind][[1]][["gp"]] <- grid::gpar(fill = fill, col = "white", just = "left")
        }
      }
    }
  }
  obj
}

#' @export
pretty_gtable <- function(data, options = NULL, outf = NULL, truncate = NULL) {
  check_arg_outf(outf)
  if (!("data.frame" %in% class(data)) & !("data.table" %in% class(data))) {
    stop(paste("provided data should be data.frame or data.table not", class(data)))
  }
  datac <- data.frame(data)
  if (!is.null(truncate)) {
    tryCatch(
      {
        if (nrow(datac) < length(truncate)) {
          warning("attempting to truncate data with incompatible indexing")
        } else if (is.character(truncate)) {
          if (!all(truncate %in% rownames(datac))) {
            warning("attempting to truncate data with incompatible indexing")
          } else {
            datac <- datac[truncate, ]
            row_truncate <- (which(rownames(datac) == truncate))
            options$rows <- options$rows[row_truncate]
          }
        } else {
          datac <- datac[truncate, ]
          options$rows <- options$rows[truncate]
        }
      },
      error = function(cond) {
        warning("attempting to truncate data with incompatible indexing")
        message("Here's the original error message:")
        message(conditionMessage(cond))
        datac
      }
    )
  }
  if (!is.null(options$fs)) {
    a.gt <- gridExtra::tableGrob(datac, theme = table.theme(options$fs), rows = options$rows, cols = options$cols)
  } else {
    a.gt <- gridExtra::tableGrob(datac, theme = table.theme(16), rows = options$rows, cols = options$cols)
  }
  if (!is.null(options$rowcs)) {
    if (length(options$rowcs) == 2) {
      a.gt <- colorise.tableGrob(a.gt, datac, options$rowcs, options$fs)
    }
  }
  if (!is.null(options$colcs)) {
    if (length(options$colcs) == 2) {
      a.gt <- colorise.tableGrob.cols(a.gt, datac, options$colcs[1], options$colcs[2])
    }
  }
  all_bg_options <- (!is.null(options$bg_fill) & !is.null(options$bg_color) & !is.null(options$bg_alpha) & !is.null(options$bg_linewidth))
  if (all_bg_options) {
    a.gt <- grid::grobTree(
      grid::rectGrob(gp = grid::gpar(fill = options$bg_fill, lwd = options$bg_linewidth, col = options$bg_color, alpha = options$bg_alpha)),
      a.gt
    )
  }
  if (!is.null(options$title)) {
    title.p.obj <- grid::textGrob(
      label = options$title,
      gp = grid::gpar(fontsize = options$fs, fontface = "bold", fill = "black", col = "black", alpha = 1),
      x = 0.5,
      y = 0.92,
    )
    a.gt <- grid::grobTree(title.p.obj, a.gt)
  }
  if (!is.null(outf)) {
    print_object(a.gt, options, outf)
  }
  a.gt
}
