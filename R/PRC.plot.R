#' Draw Precision-Recall Curve using ggplot2.
#'
#' @param prcList One or a list of PRROC::pr.curve object.
#' @return A ggplot instance.
#' @import ggplot2 PRROC
#' @export
#'
PRC.plot <- function(prcList){
  requireNamespace("ggplot2")
  requireNamespace("PRROC")
  if(class(prcList)=="PRROC") prcList = list(AUC = prcList)
  gg = data.frame()
  for(i in 1:length(prcList)){
    tmpCurve = as.data.frame(prcList[[i]]$curve)
    colnames(tmpCurve) = c("Recall", "Precision", "Threshold")
    tmpCurve = tmpCurve[nrow(tmpCurve):1, ]
    tmpCurve$Model = paste0(names(prcList)[i], " = ", round(prcList[[i]][[2]], 3))
    gg = rbind(gg, tmpCurve)
  }
  ord = order(-as.numeric(gsub(".* ", "", unique(gg$Model))))
  gg$Model = factor(gg$Model, levels = unique(gg$Model)[ord])
  p = ggplot(gg, aes_string(x="Recall", y="Precision", color="Model", group = "Model"))
  p = p + geom_line()
  p = p + theme_bw(base_size = 14)
  p = p + labs(color = "auPRC")
  p = p + theme(legend.position = c(0.01, 0.005),
                legend.justification = c("left", "bottom"),
                legend.title.align = 0.5)
  p = p + ylim(0,1)
  p
}
