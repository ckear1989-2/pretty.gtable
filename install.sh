#! /bin/bash

# Rscript -e "devtools::load_all()"
# Rscript -e "devtools::build_manual()"
# Rscript -e "devtools::check(document=TRUE, manual=TRUE, force_suggests=TRUE, run_dont_test=FALSE)"
# Rscript -e "devtools::document()"
Rscript -e "devtools::install()"
# Rscript -e "testthat::test_file(\"tests/testthat/test_input.R\", package=\"pretty.gtable\")"
# Rscript -e "testthat::test_file(\"tests/testthat/test_output.R\", package=\"pretty.gtable\")"
# Rscript -e "testthat::test_file(\"tests/testthat/test_truncation.R\", package=\"pretty.gtable\")"
# Rscript -e "testthat::test_file(\"tests/testthat/test_row_color.R\", package=\"pretty.gtable\")"
# Rscript -e "testthat::test_file(\"tests/testthat/test_column_color.R\", package=\"pretty.gtable\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_background.R\", package=\"pretty.gtable\")"
# Rscript -e "devtools::test()"
