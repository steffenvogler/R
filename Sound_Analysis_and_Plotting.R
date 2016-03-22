# from http://samcarcagno.altervista.org/blog/basic-sound-processing-r/ AND http://www.r-bloggers.com/intro-to-sound-analysis-with-r/

library(tuneR)

#Functions

sound_dist <- function(duration, samplingrate) {
  
  #Speed of sound is 1125 ft/sec
  
  return((duration/samplingrate)*1125/2)
  
}

sound_data <- function(dataset, threshold, samplingrate) {
  
  dataset <- snap@left
  
  threshold = 4000
  
  samplingrate = 44100
  
  data <- data.frame()
  
  max = 0
  
  maxindex = 0
  
  for (i in 1:length(dataset)) {
    
    if (dataset[i] > max) {
      
      max = dataset[i]
      
      maxindex = i
      
      data <- data.frame()
      
    }
    
    if (abs(dataset[i]) > threshold) {
      
      data <- rbind(data, c(i,dataset[i], sound_dist(i - maxindex, samplingrate)))
      
    }
    
  }
  
  colnames(data) <- c("x", "y", "dist")
  
  return(data)
  
}

#Analysis

#snap <- readWave("Data/snap.wav")
snap<- readWave("/Users/Vogler/Desktop/Clap(1).wav")

print(snap)
str(snap)

play(snap)

plot(snap@left[30700:31500], type = "l", main = "Snap",
     
     xlab = "Time", ylab = "Frequency")

data <- sound_data(snap@left, 4000, 44100)

plot(data[,3], data[,2], type = "l", main = "Snap",
     
     xlab = "Dist", ylab = "Frequency")


s1 <- snap@left
s1 <- s1 / 2^(snap@bit -1)
# fill in length of list = 277440
timeArray <- (0:(277440-1)) / snap@samp.rate
timeArray <- timeArray * 1000 #scale to milliseconds
plot(timeArray, s1, type='l', col='black', xlab='Time (ms)', ylab='Amplitude') 

n <- length(s1)
p <- fft(s1)

nUniquePts <- ceiling((n+1)/2)
p <- p[1:nUniquePts] #select just the first half since the second half 
# is a mirror image of the first
p <- abs(p)  #take the absolute value, or the magnitude 

p <- p / n #scale by the number of points so that
# the magnitude does not depend on the length 
# of the signal or on its sampling frequency  
p <- p^2  # square it to get the power 

# multiply by two (see technical document for details)
# odd nfft excludes Nyquist point
if (n %% 2 > 0){
  p[2:length(p)] <- p[2:length(p)]*2 # we've got odd number of points fft
} else {
  p[2: (length(p) -1)] <- p[2: (length(p) -1)]*2 # we've got even number of points fft
}

freqArray <- (0:(nUniquePts-1)) * (snap@samp.rate / n) #  create the frequency array 

plot(freqArray/1000, 10*log10(p), type='l', col='black', xlab='Frequency (kHz)', ylab='Power (dB)')
