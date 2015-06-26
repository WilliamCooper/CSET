### plot 4: plot differences, individual pairs of temperatures
RPlot4 <- function (data) { 
  ## needs ATH1, ATH2, AT_A
  op <- par (mar=c(5,5,2,4)+0.1,oma=c(1.1,0,0,0))
  labelled=F
  layout(matrix(1:4, nrow=2, ncol = 2), widths = c(5,5), heights = c(5,6))
  # ATH1,2 section
  # protection against all-bad:
  if (any(!is.na(data$ATH1) & !is.na(data$ATH2))) {
    ylb <- expression (paste ("ATH2  [", degree, "C]"))
    plot (DF <- data[, c("ATH1", "ATH2")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
    plot (data$ATH1, data$ATH2 - data$ATH1, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("ATH2 - ATH1  [", degree, "C]")),4,3,col='red',cex=0.8)
#    DF$ATH2 <- (data$ATH2 - data$ATH1)*5
#    points (DF, col='red', type='l', lwd=2)
#    lines (c(-70,30), c(5,5), col='red', lty=2)
#    lines (c(-70,30), c(-5,-5), col='red', lty=2)
# this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
#    legend ("bottomright", legend=c("red: y=(ATH2-ATH1)*5", 
#                                    "dashed lines: +/-1C error bands"), 
#            box.col='red', text.col='red', cex=0.5)
    fm <- lm(ATH2~ATH1, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("ATH2=%.3f(ATH1)%.3f\nmean diff ATH2-ATH1=%.2f +/- %.2f", 
                    coef[2], coef[1], mean (data$ATH2-data$ATH1, na.rm=TRUE),
                    sd(data$ATH2-data$ATH1, na.rm=TRUE))
    } else {
      t <- sprintf ("ATH2=%.3f(ATH1)+%.3f\nmean diff ATH2-ATH1=%.2f +/-%.2f", 
                    coef[2], coef[1], mean (data$ATH2-data$ATH1, na.rm=TRUE),
                    sd(data$ATH2-data$ATH1, na.rm=TRUE))
    }
    title(t, cex.main=0.75)
    AddFooter ()
  }
  # AT_A section:
  if (any(!is.na(data$AT_A) & !is.na(data$ATH2))) {
    ylb <- expression (paste ("AT_A  [", degree, "C]"))
    plot (DF <- data[, c("ATH2", "AT_A")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
#### this is plotting ATH2-AT_A but reporting in the title AT_A-ATH2, should pick either and make consistent
    plot (data$ATH2, data$ATH2 - data$AT_A, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("ATH2 - AT_A  [", degree, "C]")),4,3,col='red',cex=0.8)
#    DF$AT_A <- (data$ATH2 - data$AT_A)*5
#    points (DF, col='red', type='l', lwd=2)
#    lines (c(-70,30), c(5,5), col='red', lty=2)
##    lines (c(-70,30), c(-5,-5), col='red', lty=2)
# this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
#    legend ("topleft", legend=c("red: y=(ATH2-AT_A)*5", 
#                                "dashed lines: +/-1C error bands"), 
#            box.col='red', text.col='red', cex=0.5)
    fm <- lm(AT_A~ATH2, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("AT_A=%.3f(ATH2)%.3f\n mean diff AT_A-ATH2%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$AT_A-data$ATH2, na.rm=TRUE),
                    sd(data$AT_A-data$ATH2, na.rm=TRUE))
    } else {
      t <- sprintf ("AT_A=%.3f(ATH2)+%.3f\n mean diff AT_A-ATH2%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$AT_A-data$ATH2, na.rm=TRUE), 
                    sd(data$AT_A-data$ATH2, na.rm=TRUE))
    }
    title(t, cex.main=0.8)
    if(!labelled) AddFooter (); labelled=T
  }
}

