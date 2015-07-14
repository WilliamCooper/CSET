### plot 3: plot all temperatures, one plot
RPlot3 <- function (data) { 
  ## needs ATH1, ATH2, AT_A
  par(oma=c(1.1,0,0,0))
  ylb <- expression (paste ("temperature  ATy  [", degree, "C]"))
  plotWAC (data[, c("Time", "ATH2", "ATH1", "ATF1", "AT_A")],
           ylab=ylb, lty=c(1,1,1,2), lwd=c(2,1.5,1,2,1))
  #        lwd=c(2,1.5,1,2), legend.position=NA) # pulling legend out of plotWAC to increase font size
  # legend('bottomright',c("ATH2", "ATH1", "AT_A"),col=c("blue","darkgreen","red"),text.col=c("blue","darkgreen","red"),lty=c(1,1,2),lwd=c(2,1.5,1))
  title(sprintf("Means H1-H2: %.2f; F1-H2: %.2f; _A-H2: %.2f", 
                mean (data$ATH1-data$ATH2, na.rm=TRUE), 
                mean (data$ATF1-data$ATH2, na.rm=TRUE),
                mean (data$AT_A-data$ATH2, na.rm=TRUE)), cex.main=0.8)
  AddFooter ()
}
