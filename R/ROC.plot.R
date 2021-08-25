#' Draw ROC (Receiver operating characteristic) Curve using ggplot2.
#'
#' @param rocList One or a list of pROC::roc object.
#' @return A ggplot instance.
#' @import pROC ggplot2
#' @export
#'
ROC.plot <- function(rocList){
  requireNamespace("ggplot2")
  requireNamespace("pROC")
  if(class(rocList)=="roc") rocList = list(AUC = rocList)
  gg = rocList
  aucs = unlist(lapply(gg, function(x){ x$auc }))
  names(gg) = paste0(names(gg), " = ", round(aucs, 3))
  gg = gg[order(-aucs)]
  p = pROC::ggroc(gg)
  p = p + geom_abline(slope = 1, intercept = 1, color = "gray30", linetype = "dashed")
  p = p + theme_bw(base_size = 14)
  p = p + labs(color = "auROC")
  p = p + theme(legend.position = c(0.995, 0.01),
                legend.justification = c("right", "bottom"),
                legend.title.align = 0.5)
  p = p + labs(x = "Specificity", y = "Sensitivity")
  return(p)
}
