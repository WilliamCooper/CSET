### plot 16: DBAR (mean diameters) and PLWC (liquid water content)
# do 1-min smoothing; otherwise, too noisy
RPlot16 <- function (data) {
  ## needs DBARD_LWOI, DBARP_RWOOP, DBAR1DC_LWOO, DBARU_RWOOU
  ## PLWCD_LWOI, PLWC, PLWCC, PLWC1DC_LWOO
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  # DBAR:
  op <- par (mar=c(2,4,1,1)+0.1,oma=c(1.1,0,0,0))
  DF <- data[, c("Time", "DBARD_LWOI")]
  DF$DBARD_LWOI <- SmoothInterp(data$DBARD_LWOI)
  plotWAC (DF, ylim=c(0,30), ylab="DBAR", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  #DF <- data[, c("Time", "DBAR3_RPO", "DBARP_RWOOP")]
  DF <- data[, c("Time", "DBARP_RWOOP", "DBARU_RWOOU")]
  #DF$DBAR3_RPO <- SmoothInterp(data$DBAR3_RPO)
  DF$DBARP_RWOOP <- SmoothInterp(data$DBARP_RWOOP)
  DF$DBARU_RWOOU <- SmoothInterp(data$DBARU_RWOOU)
  plotWAC (DF, ylim=c(0,2), ylab="DBARP and DBARU", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "DBAR1DC_LWOO")]
  DF$DBAR1DC_LWOO <- SmoothInterp(data$DBAR1DC_LWOO)
  plotWAC (DF)
  title ("1-min filter", cex.main=0.75)
  AddFooter ()
  # PLWC:
  op <- par (mar=c(2,4,1,1)+0.1)
  DF <- data[, c("Time", "PLWCD_LWOI", "PLWCC")]
  DF$PLWCD_LWOI <- SmoothInterp(data$PLWCD_LWOI)
  DF$PLWCC <- SmoothInterp(data$PLWCC)
  plotWAC (DF, ylim=c(0,1), ylab="PLWCy", legend.position="topright")
  title ("1-min filter", cex.main=0.75)
  plotWAC (data[, c("Time", "PLWC", "RICE")], ylim=c(0,25), ylab="PLWC (Watts)")
  hline (10); hline (15)
  op <- par (mar=c(5,4,1,1)+0.1)
  DF <- data[, c("Time", "PLWC1DC_LWOO")]
  DF$PLWC1DC_LWOO <- SmoothInterp(data$PLWC1DC_LWOO)
  plotWAC (DF, ylim=c(0,1))
  title ("1-min filter", cex.main=0.75)
  AddFooter ()
  
  # CDP housekeeping
  layout(matrix(1:3, ncol = 1), widths = 1, heights = c(5,5,6))
  op <- par (mar=c(2,4,1,1)+0.1)
  data$DOFACC <- data$TCNTD_LWOI / (data$TCNTD_LWOI + data$REJDOF_LWOI)
  data$DOFACC <- SmoothInterp (data$DOFACC)
  plotWAC (data[, c("Time", "DOFACC")], ylab="DOF acceptance fraction")
  hline (0.2, 'red')
  data$AVT <- SmoothInterp (data$AVGTRNS_LWOI)
  plotWAC (data[, c("Time", "AVT")], ylim=c(0, 2))
  op <- par (mar=c(5,4,1,1)+0.1)
  plotWAC (data[, c("Time", "CDPLSRP_LWOI")], ylab="CDP laser power", ylim=c(0,4))
  AddFooter ()
}

