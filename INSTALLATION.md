The steps to make the Shinylive version are as follows:

1. Start an R session and set your working directory to be the folder of the respository - using setwd().

2. If there is a folder at inst/app/R, delete it (back it up first in case you have anything you want to keep).

3. Run the following to create a folder at the correct location: dir.create("inst/app/R", recursive = TRUE, showWarnings = FALSE)

4. Run: file.copy(list.files("R", full.names = TRUE, recursive = TRUE), "inst/app/R", overwrite = TRUE, recursive = TRUE)

5. Run (you must have the shinylive package installed): shinylive::export(appdir = "inst/app", destdir = "site")

To update files on Githhub:

1. Before switching to the "gh-pages" branch, move the site folder someone outside of the repo's folder (such as the desktop).

2. Delete /site files in the "gh-pages" branch.

3. Move the new files in from the /site folder you moved somewhere.

4. Run the following:

git add app.json edit index.html shinylive shinylive-sw.js
git commit -m "Updated app"
git push
