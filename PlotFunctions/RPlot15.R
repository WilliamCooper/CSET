### plot 15: CN, FSSP, CDP, F300, CONCP, CONC1DC_LWOO
RPlot15 <- function(data) {
  ## needs CONCN, CONCD_LWOI, CONCP_RWOOP, CONC1DC_LWOO,
  ##       CONCU, CONCU100, CONCU500, USMPFLW, USHFLW, FCNC, XICNC, PFLWC
  layout(matrix(1:2, ncol = 1), widths = 1, heights = c(5,6))
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  # remove zeroes for log plot:
  data$CONCN[!is.na(data$CONCN) & (data$CONCN <= 0)] <- NA
  data$CONCU_RWOOU[!is.na(data$CONCU_RWOOU) & (data$CONCU_RWOOU <= 0)] <- NA
  data$CONCU100_RWOOU[!is.na(data$CONCU100_RWOOU) & (data$CONCU100_RWOOU <= 0)] <- NA
  data$CONCU500_RWOOU[!is.na(data$CONCU500_RWOOU) & (data$CONCU500_RWOOU <= 0)] <- NA
  data$CONCD_LWOI[!is.na(data$CONCD_LWOI) & (data$CONCD_LWOI <= 0)] <- NA
  data$CONCP_RWOOP[!is.na(data$CONCP_RWOOP) & (data$CONCP_RWOOP <= 0)] <- NA
  data$CONC1DC_LWOO[!is.na(data$CONC1DC_LWOO) & (data$CONC1DC_LWOO <= 0)] <- NA
  plotWAC (data[, c("Time", "CONCN", "CONCP_RWOOP", "CONCU_RWOOU", "CONCU100_RWOOU")], 
           logxy='y', ylim=c(1,1.e5), 
           ylab=expression (paste ("CONCy [cm"^"-3"*"]")))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "CONCD_LWOI", "CONC1DC_LWOO")], 
           logxy='y', ylim=c(0.001,1e4), ylab=expression(paste("CONCy [cm"^"-3"*"]")))
  AddFooter ()
  op <- par (mar=c(2,4,1,1)+0.1)
  data$USHF <- data$USHFLW_RWOOU/10
  plotWAC (data[, c("Time", "USMPFLW_RWOOU", "USHF", "FCNC", "XICNC",
                    "PFLWC_RWOOP")], ylab="flows", legend.position='topright',
           ylim=c(0,2.5))
  hline (0.82, 'blue'); hline (1, 'darkgreen'); hline(0.5, 'red'); hline (1.5, 'red')
  legend ("topleft", legend=c("dashed red: limits for FCNC, XICNC, PFLWC", 
          "dashed blue-green: expected values for corresponding flows"), text.col=c('red', 'blue'), cex=0.55)
  title ("USHF is USHFLW_RWOOU/10", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "UREF_RWOOU", "USCAT_RWOOU", "PREF_RWOOP")],
           ylab="laser V", legend.position='topright', ylim=c(0,10))
  hline (2.05, 'blue'); hline (1.95, 'darkgreen'); hline(6, 'red'); hline (9.95, 'red')
  title ("dashed-blue: lower limit for UREF; dashed-green: upper limit for USCAT", cex.main=0.65)
  AddFooter ()
}

