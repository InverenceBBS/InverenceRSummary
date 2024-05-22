.onAttach <- function(libname, pkgname){
  options(ExtSummary.showRMSE=TRUE)
  options(ExtSummary.showQtle="ALL")
  options(ExtSummary.nQtle0=2)
  options(ExtSummary.nQtlem=3)
  options(ExtSummary.nQtle1=2)
  options(ExtSummary.showPoints=TRUE)
  options(ExtSummary.showRMSE=TRUE)
  options(ExtSummary.showRelativeStats=TRUE)
  options(ExtSummary.digits=3)
  options(ExtSummary.max_discrete_levels=20)
}
