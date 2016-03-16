library(hexbin)
x <- rnorm(100000)
y <- rnorm(100000)
(bin <- hexbin(x, y))

## nicer colour palette
cols <- colorRampPalette(c("darkorchid4","darkblue","green","yellow", "red") )
plot(hexbin(x, y + x*(x+1)/4), main = "Example" ,
     colorcut = seq(0,1,length.out=24),
     colramp = function(n) cols(24) ,
     legend = 1 )

##########another example
# Color housekeeping
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(32)

# Create normally distributed data for plotting
x <- rnorm(mean=1.5, 5000)
y <- rnorm(mean=1.6, 5000)
df <- data.frame(x,y)

# Plot
plot(df, pch=16, col='black', cex=0.5)
##### OPTION 1: hexbin from package 'hexbin' #######
library(hexbin)
# Create hexbin object and plot
h <- hexbin(df)
plot(h)
plot(h, colramp=rf)
# hexbinplot function allows greater flexibility
hexbinplot(y~x, data=df, colramp=rf)
# Setting max and mins
hexbinplot(y~x, data=df, colramp=rf, mincnt=2, maxcnt=60)
# Scaling of legend - must provide both trans and inv functions
h1 <- hist(df$x, breaks=25, plot=F)
h2 <- hist(df$y, breaks=25, plot=F)
top <- max(h1$counts, h2$counts)
k <- kde2d(df$x, df$y, n=25)

oldpar <- par()
par(mar=c(3,3,1,1))
layout(matrix(c(2,0,1,3),2,2,byrow=T),c(3,1), c(1,3))
#hexbinplot(y~x, data=df, colramp=rf, trans=log, inv=exp)
image(k, col=r) #plot the image
par(mar=c(0,2,1,0))
barplot(h1$counts, axes=F, ylim=c(0, top), space=0, col='red')
par(mar=c(2,0,0.5,1))
barplot(h2$counts, axes=F, xlim=c(0, top), space=0, col='red', horiz=T)