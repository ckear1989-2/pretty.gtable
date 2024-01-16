#! /bin/bash

# Rscript -e "devtools::load_all()"
# Rscript -e "devtools::test()"
# Rscript -e "devtools::document()"
# Rscript -e "devtools::build_manual()"
# Rscript -e "devtools::check(document=TRUE, manual=TRUE, force_suggests=TRUE, run_dont_test=FALSE)"
Rscript -e "devtools::install()"
Rscript -e "testthat::test_file(\"tests/testthat/test_input.R\", package=\"pretty.gtable\")"
Rscript -e "testthat::test_file(\"tests/testthat/test_output.R\", package=\"pretty.gtable\")"
Rscript -e "testthat::test_package(\"pretty.gtable\")"

