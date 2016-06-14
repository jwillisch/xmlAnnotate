## ------------------------------------------------------------------------
library(xmlAnnotate)

## ------------------------------------------------------------------------
folder <- system.file("extdata", "fomc", package = "xmlAnnotate")
dir(folder)

## ------------------------------------------------------------------------
f <- file.path(folder, "2004_03_2-1.xml")
f
ftags <- get_tagset(f)

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(ftags)

## ----eval=FALSE----------------------------------------------------------
#  ftags <- get_tagset(f, nodes=c('hedge'))

## ------------------------------------------------------------------------
ftags2 <- get_tagset(f, nodes=c('hedge', 'note'))

## ---- echo=FALSE, results='asis'-----------------------------------------
knitr::kable(ftags2)

## ------------------------------------------------------------------------
fftags <- get_tagsets(folder, nodes=c('hedge', 'note'))

