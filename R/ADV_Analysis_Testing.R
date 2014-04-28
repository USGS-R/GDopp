rm(list=ls(all=TRUE))

data.dir <- '/Users/jread/Documents/R/velocimeterK/supporting data/'
file.nm <- "ICACOS04.dat"
file.loc <- file.path(data.dir,file.nm)
library(oce)
##################################################
data=read.table(file.loc)
str(data)
################################################################################
#Signal to Noise Ratio time series
plot(data$V9, col="blue", type="l", ylab="") 
lines(data$V10, col="red", type='l')
lines(data$V11, col="green", type='l')
mtext(side=2, line=2, quote(Signal/Noise~(dB)))
abline(h=15, lty=2)
legend("topright", c("X", "Y", "Z"), col=c("blue", "red", "green"), pch=16)
##############################################################################
#Pulse Correlation Time Series
plot(data$V12,col="blue", type="l", ylab="") 
lines(data$V13, col="red", type='l')
lines(data$V14, col="green", type='l')
abline(h=70)
mtext(side=2, line=2.5, quote(Correlation~("%")))
legend("topright", c("X", "Y", "Z"), col=c("blue", "red", "green"), pch=16)
#########################################
#Velocity Time Series
plot(data$V5, type="l", col="green", ylim=c(-1,1), ylab="", lwd=0.1)
lines(data$V4, type="l", col="red", lwd=0.1)
lines(data$V3, type="l", col="blue", lwd=0.1)
abline(h=mean(data$V4), lty=2)
legend("topright", c("X", "Y", "Z"), col=c("blue", "red", "green"), pch=16)
mtext(side=2, line=2.5, quote(Velocity~(m~s^-1)))
###############################################################################
#frequency spectra 
Fs = 32 #frequency (Hz)
d=density(data$V5)
plot(d, type="p")
xts = ts(data$V5, frequency=Fs)
w = pwelch(xts)
str(w)
wavenum=2*pi*w$spec/mean(data$V4)
plot(log10(w$freq), log10(wavenum), type="l", ylab="", xlab="", col="green", lwd=1.5)
mtext(side=1, line=2.5, quote(log~Frequency~(Hz)))
mtext(side=2, line=2.5, quote(log~Wavenumber~(m^2~sec^-3)))
abline(v=log10(0.5), lty=2)
abline(v=log10(8), lty=2)
text(log(1.4), log(1), "Inertial Subrange")
