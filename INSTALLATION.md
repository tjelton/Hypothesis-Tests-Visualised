The steps to make the Shinylive version are as follows:

1. Start an R session and set your working directory to be the folder of the respository - using setwd().

2. If there is a folder at inst/app/R, delete it (back it up first in case you have anything you want to keep).

3. Run the following to create a folder at the correct location: dir.create("inst/app/R", recursive = TRUE, showWarnings = FALSE)

4. Run: file.copy(list.files("R", full.names = TRUE, recursive = TRUE), "inst/app/R", overwrite = TRUE, recursive = TRUE)

5. Run (you must have the shinylive package installed): shinylive::export(appdir = "inst/app", destdir = "site")