### plot 4: plot differences, individual pairs of temperatures
RPlot4 <- function (data) { 
  ## needs ATHR1, ATHR2, AT_A
  op <- par (mar=c(5,5,2,4)+0.1,oma=c(1.1,0,0,0))
  labelled=F
  layout(matrix(1:4, nrow=2, ncol = 2), widths = c(5,5), heights = c(5,6))
  # ATHR1,2 section
  # protection against all-bad:
  if (any(!is.na(data$ATHR1) & !is.na(data$ATHR2))) {
    ylb <- expression (paste ("ATHR2  [", degree, "C]"))
    plot (DF <- data[, c("ATHR1", "ATHR2")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
    plot (data$ATHR1, data$ATHR2 - data$ATHR1, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("ATHR2 - ATHR1  [", degree, "C]")),4,3,col='red',cex=0.8)
#    DF$ATHR2 <- (data$ATHR2 - data$ATHR1)*5
#    points (DF, col='red', type='l', lwd=2)
#    lines (c(-70,30), c(5,5), col='red', lty=2)
#    lines (c(-70,30), c(-5,-5), col='red', lty=2)
# this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
#    legend ("bottomright", legend=c("red: y=(ATHR2-ATHR1)*5", 
#                                    "dashed lines: +/-1C error bands"), 
#            box.col='red', text.col='red', cex=0.5)
    fm <- lm(ATHR2~ATHR1, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("ATHR2=%.3f(ATHR1)%.3f\nmean diff ATHR2-ATHR1=%.2f +/- %.2f", 
                    coef[2], coef[1], mean (data$ATHR2-data$ATHR1, na.rm=TRUE),
                    sd(data$ATHR2-data$ATHR1, na.rm=TRUE))
    } else {
      t <- sprintf ("ATHR2=%.3f(ATHR1)+%.3f\nmean diff ATHR2-ATHR1=%.2f +/-%.2f", 
                    coef[2], coef[1], mean (data$ATHR2-data$ATHR1, na.rm=TRUE),
                    sd(data$ATHR2-data$ATHR1, na.rm=TRUE))
    }
    title(t, cex.main=0.75)
    AddFooter ()
  }
  # AT_A section:
  if (any(!is.na(data$AT_A) & !is.na(data$ATHR2))) {
    ylb <- expression (paste ("AT_A  [", degree, "C]"))
    plot (DF <- data[, c("ATHR2", "AT_A")], pch=20,ylab=ylb)
    lines (c(-70.,30.), c(-69.,31.), col="darkorange", lwd=2, lty=2)
    lines (c(-70.,30.), c(-71.,29.), col="darkorange", lwd=2, lty=2)
    par(new=T)
#### this is plotting ATHR2-AT_A but reporting in the title AT_A-ATHR2, should pick either and make consistent
    plot (data$ATHR2, data$ATHR2 - data$AT_A, type='l', col='red',axes=F,xlab='',ylab='',ylim=c(-2,2))
    axis (4,col='red',col.axis='red')
    abline(h=c(-1,1),col='red',lty=2)
    mtext(expression (paste ("ATHR2 - AT_A  [", degree, "C]")),4,3,col='red',cex=0.8)
#    DF$AT_A <- (data$ATHR2 - data$AT_A)*5
#    points (DF, col='red', type='l', lwd=2)
#    lines (c(-70,30), c(5,5), col='red', lty=2)
##    lines (c(-70,30), c(-5,-5), col='red', lty=2)
# this info somewhat redundant with legend now, also cex=0.5 was too small to read in pngs but 1.0 too big for plot
#    legend ("topleft", legend=c("red: y=(ATHR2-AT_A)*5", 
#                                "dashed lines: +/-1C error bands"), 
#            box.col='red', text.col='red', cex=0.5)
    fm <- lm(AT_A~ATHR2, data=data)
    coef <- coefficients (fm)
    if (coef[1] < 0.) {
      t <- sprintf ("AT_A=%.3f(ATHR2)%.3f\n mean diff AT_A-ATHR2%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$AT_A-data$ATHR2, na.rm=TRUE),
                    sd(data$AT_A-data$ATHR2, na.rm=TRUE))
    } else {
      t <- sprintf ("AT_A=%.3f(ATHR2)+%.3f\n mean diff AT_A-ATHR2%.2f +/-%.2f", 
                    coef[2], coef[1], mean(data$AT_A-data$ATHR2, na.rm=TRUE), 
                    sd(data$AT_A-data$ATHR2, na.rm=TRUE))
    }
    title(t, cex.main=0.8)
    if(!labelled) AddFooter (); labelled=T
  }
}

