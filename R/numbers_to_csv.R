#' Export tables from Apple Numbers (.numbers) file to CSV
#'
#' This function reads an Apple Numbers (.numbers) file, extracts tables from each sheet,
#' and exports them as CSV files, mirroring the Python script using `numbers_parser`.
#'
#' @param in_path Path to the .numbers file.
#' @param out_dir Output directory to write CSV files.
#' @param create_dir If TRUE, creates the output directory if it doesn't exist.
#' @param na_rep Representation for missing values in output CSVs (default: "").
#' @param python_env Path to Python executable or Conda environment name. If NULL, uses default Python.
#' @param verbose If TRUE, prints progress and file write messages.
#' @param progress If TRUE and `progress` package is available, shows a progress bar.
#' @return Invisibly returns a character vector of written file paths.
#' @export
#' @examples
#' \dontrun{
#'   numbers_to_csv(
#'     in_path = "example.numbers",
#'     out_dir = "output",
#'     create_dir = TRUE,
#'     na_rep = "",
#'     python_env = NULL,
#'     verbose = TRUE,
#'     progress = TRUE
#'   )
#' }
#'

numbers_to_csv <- function(in_path,
                           out_dir,
                           create_dir = TRUE,
                           na_rep = "",
                           python_env = NULL,
                           verbose = TRUE,
                           progress = TRUE) {
  .require_py_modules(c("numbers_parser", "pandas"))
  # Input validation
  if (!is.character(in_path) || length(in_path) != 1) {
    stop("in_path must be a single character string")
  }
  if (!is.character(out_dir) || length(out_dir) != 1) {
    stop("out_dir must be a single character string")
  }
  if (!grepl("\\.numbers$", tolower(in_path))) {
    warning("in_path does not have a .numbers extension; ensure it is a valid Numbers file")
  }
  in_path <- normalizePath(in_path, mustWork = TRUE)
  out_dir <- normalizePath(out_dir, mustWork = FALSE)
  if (!dir.exists(out_dir)) {
    if (isTRUE(create_dir)) {
      dir.create(out_dir, recursive = TRUE)
      if (!dir.exists(out_dir)) stop("Failed to create output directory: ", out_dir)
    } else {
      stop("Output directory does not exist: ", out_dir)
    }
  }
  if (!is.character(na_rep) || length(na_rep) != 1) {
    stop("na_rep must be a single character string")
  }

  # Configure Python environment
  if (!is.null(python_env)) {
    if (grepl("^conda:", python_env)) {
      reticulate::use_condaenv(gsub("^conda:", "", python_env), required = TRUE)
    } else {
      reticulate::use_python(python_env, required = TRUE)
    }
  } else if (!reticulate::py_available(initialize = TRUE)) {
    stop("No Python environment available. Specify python_env or configure reticulate.")
  }

  # Import Python modules
  np <- tryCatch(
    reticulate::import("numbers_parser", delay_load = TRUE),
    error = function(e) stop("Failed to import numbers_parser: ", conditionMessage(e))
  )
  pd <- tryCatch(
    reticulate::import("pandas", delay_load = TRUE),
    error = function(e) stop("Failed to import pandas: ", conditionMessage(e))
  )
  pathlib <- tryCatch(
    reticulate::import("pathlib", delay_load = TRUE),
    error = function(e) stop("Failed to import pathlib: ", conditionMessage(e))
  )

  # Debugging: Inspect Python environment
  if (verbose) {
    message("Python version: ", reticulate::py_config()$version)
    message("numbers_parser version: ", tryCatch(np$`__version__`, error = function(e) "unknown"))
  }

  # Python code to process sheets, tables, and debugging
  python_code <- sprintf("
from numbers_parser import Document
import pandas as pd
from pathlib import Path

in_path = Path('%s')
out_dir = Path('%s')
out_dir.mkdir(parents=True, exist_ok=True)
written = []
doc = Document(in_path)

# Debugging: Sheets info
sheets = list(doc.sheets)
sheets_info = [(s.name, len(list(s.tables))) for s in sheets]
print('Sheets info:', sheets_info)

# Count total tables for progress
total_steps = sum(len(list(s.tables)) for s in sheets)

# Process sheets and tables
for si, sheet in enumerate(sheets, start=1):
    sheet_name = str(sheet.name) if sheet.name else 'untitled'
    sheet_name = sheet_name.replace('/', '_').replace('\\\\', '_').replace(':', '_').replace('*', '_').replace('?', '_').replace('\"', '_').replace('<', '_').replace('>', '_').replace('|', '_')
    tables = list(sheet.tables)
    for ti, table in enumerate(tables, start=1):
        table_name = str(table.name) if table.name else 'untitled'
        table_name = table_name.replace('/', '_').replace('\\\\', '_').replace(':', '_').replace('*', '_').replace('?', '_').replace('\"', '_').replace('<', '_').replace('>', '_').replace('|', '_')
        data = table.rows(values_only=True)
        if not data:
            print(f'Skipping empty table S{si}T{ti}')
            continue
        cols = [str(c) if c is not None else f'V{j+1}' for j, c in enumerate(data[0])]
        df = pd.DataFrame(data[1:], columns=cols)
        out_file = out_dir / f'{in_path.stem}__S{si}_{sheet_name}__T{ti}_{table_name}.csv'
        df.to_csv(out_file, index=False, na_rep='%s')
        print(f'Wrote: {out_file}')
        written.append(str(out_file))
", in_path, out_dir, na_rep)

  # Execute Python code
  result <- tryCatch(
    reticulate::py_run_string(python_code, convert = TRUE),
    error = function(e) {
      message("Python error details: ", reticulate::py_last_error()$message)
      stop("Failed to process sheets and tables: ", conditionMessage(e))
    }
  )

  # Retrieve written files and total steps
  written <- tryCatch(
    reticulate::py_to_r(result$written),
    error = function(e) {
      warning("Failed to retrieve written files: ", conditionMessage(e))
      character()
    }
  )
  total_steps <- tryCatch(
    reticulate::py_to_r(result$total_steps),
    error = function(e) {
      warning("Failed to retrieve total steps: ", conditionMessage(e))
      0
    }
  )

  # Initialize progress bar
  if (progress && requireNamespace("progress", quietly = TRUE) && total_steps > 0) {
    pb <- progress::progress_bar$new(
      format = "Processing [:bar] :percent (:current/:total)",
      total = total_steps,
      clear = FALSE
    )
    pb$tick(total_steps) # Complete progress bar since processing is done
  }

  # Log written files
  log_message <- if (verbose) message else function(...) invisible(NULL)
  for (file in written) {
    log_message("Wrote: ", file)
  }

  invisible(written)
}

# Replace illegal filename characters
.sanitize_filename <- function(x) {
  x <- as.character(if (is.null(x)) "" else x)
  x <- gsub("[/\\\\:*?\"<>|]+", "_", x, perl = TRUE)
  if (!Encoding(x) %in% c("UTF-8", "unknown")) x <- enc2utf8(x)
  if (nchar(x) == 0) x <- "untitled"
  substr(x, 1, 120)
}
