wd <- getwd()
filedata <- read.csv(paste0(wd,"/Failure_1.csv"))
fftinput <- data.frame(cbind(0:(nrow(filedata)-1),filedata$TEMP_OUTER_BEARING))

fftoutput <- fft(fftinput$X2)

fftinput2 = fftinput[489:nrow(fftinput),]

fftoutput2 <- fft(t(fftinput2$X2))
fftoutput2
