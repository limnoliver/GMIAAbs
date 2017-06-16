
pdf("AbsQuickLook.pdf")
par(mfrow=c(4,3), oma = c(1,1,1,1), mar = c(2,2,0,0))
for (i in 1:(length(FinalAbsDf)-7)){
  plotAbs(FinalAbsDf, WaveCol = "Wavelength", absCol = names(FinalAbsDf[i]))
  legend("topright", paste(names(FinalAbsDf[i])), bty = "n", col = "red")
}
dev.off()


