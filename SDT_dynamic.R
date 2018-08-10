#mSDT function

### things to do:
#make list of parameters at the top of the function, with short var names, to avoid repetition of definitions through out function
#place text for foil likelihood on two line instead of one
#decide what added functionality woul dbe handy
#test savinf high resolution plot, at the moment = very coarse == note varible nr_n, might have to amped up for high res
#fully test with extreme parameters
#


SDT <- function(criterion, signal_mean, noise_mean, input_sigma = 1, likelihood_line_col = 'gray31', col1= rgb(70/255, 130/255, 180/255, .85), col2= rgb(135/255, 206/255, 235/255, 0.35), criterion_col = "pink3", line_col = "black") {

  #initiate empty plot
  plot.new()
  #set margines:
  par(mar=c(5,5,3,7), xpd = T) #bottom, left, top and right margins, xpd = sets clipping to 'off' so that lables of likelihoods are plotted and not clipped
  
  nr_n = 500
  #plot(a)
  curve(dnorm(x, noise_mean, input_sigma), n= nr_n, xlim=c(0, signal_mean+input_sigma + 3), ylim=c(0, dnorm(0, sd = input_sigma)+.1),
        xlab="Decision variable", ylab="Probability", frame=FALSE)
  legend("topright", legend=c("target", "foil"), lty=c(2, 1))
  
  segments(0,0, 10,0, col=likelihood_line_col)  
  curve(dnorm(x, signal_mean, input_sigma), lty=2, add=T)
  
  #shading:
  z_n <- seq(from=criterion, to=pi+noise_mean, length=50)
  z_n_y <- seq(from=criterion-noise_mean, to=pi, length=50)
  
  polygon(x=c(criterion, z_n, pi+noise_mean),
          y=c(0, dnorm(z_n_y, sd=input_sigma), 0),
          col=col1, border=NA)
  
  z_sn <- seq(from=criterion, to=2*pi+signal_mean, length=100)
  polygon(x=c(criterion, z_sn, signal_mean+2*pi),
          y=c(0, dnorm(z_sn, signal_mean, input_sigma), 0),
          col=col2, border=NA)
  
  #c
  lines(x=c(criterion, criterion), y=c(0, dnorm(0, sd = input_sigma)+0.08), col=criterion_col, lwd=2)
  text(criterion, dnorm(0, sd = input_sigma) + 0.1,  "c")
  #d
  d_line_height = dnorm(0, sd = input_sigma)
  segments(noise_mean, d_line_height + 0.02, signal_mean, d_line_height + 0.02) #horizontal line
  segments(noise_mean, 0, noise_mean, d_line_height+0.02, lty=3) #vertical line at foil mean
  segments(signal_mean, 0, signal_mean, d_line_height+0.02, lty=3) #vertical line at target mean
  text((signal_mean+noise_mean)/2, d_line_height+0.04, "d'")
  
  
  #get foil likelihood & corss-over point:
  foil_like= dnorm(criterion - noise_mean, sd = input_sigma)
  target_like = dnorm(criterion - signal_mean, sd = input_sigma)
  
  #draw the foil likelihood line
  scalar = .1
  location_foil_like = signal_mean + input_sigma + 6
  segments(criterion,foil_like, location_foil_like, foil_like, lty=3) 
  segments(location_foil_like,0, location_foil_like,foil_like, col = line_col) #actual foil likelihood line
  text(location_foil_like + .5,foil_like-foil_like/3,"foil likelihood", srt = 90) #srt is to turn the text vertical 
  segments(location_foil_like- scalar, foil_like, location_foil_like+ scalar, foil_like, col = line_col)
  segments(location_foil_like- scalar, 0, location_foil_like+ scalar, 0, col = line_col) #the end bits
  
  #draw the target likelihood line
  location_target_like = signal_mean + input_sigma + 4
  segments(criterion,target_like, location_target_like, target_like, lty=3)
  segments(location_target_like,0,location_target_like,target_like, col = line_col) #actual target likelihood line
  text(location_target_like + .5,target_like-target_like/3,"target likelihood", srt = 90)
  segments(location_target_like - scalar, target_like, location_target_like+scalar, target_like, col = line_col)
  segments(location_target_like - scalar, 0, location_target_like+scalar, 0, col = line_col) #the end bits
  
}

criterion = 4.2
signal_mean = 6
noise_mean = 4
input_sigma = 1 #default at 1
#color arguments defaulted at blue, red, black

SDT(criterion, signal_mean, noise_mean, input_sigma)

####documentation:
# 
# SDT plotting function
# 
# Description
# 
# plots two standard distributions with criterion, d prime and likelyhoods for signal and noise_mean
# 
# Usage
# 
# SDT(criterion, signal_mean, noise_mean, input_sigma, = 1)
# 
# Arguments
# 
# criterion single number, integer or float
# signal_mean single number, integer or float
# noise_mean single number, integer or float
# input_sigma single number, integer or float
#
#color arguments:
# likelihood_line_col = 'gray31', 
# col1= rgb(70/255, 130/255, 180/255, .85)
# col2= rgb(135/255, 206/255, 235/255, 0.35)
# criterion_col = "pink3"
# line_col = "black"
# 
# Details
# 
# criterion, signal_mean, noise_mean are necessary, sigma (standard deviation) is set at 1.
# colors are at the moment hard coded in the function, criterion usually lies between signal and noise mean.
# function only works for positive values of signal and noise, noiseis to be expected to be smaller then the signal, 
# function plots from zero up to signal_mean + 4
# 
# source
# 
# reference the orignal online source
# 
# Examples
# 1##
# SDT(1.5, 1, 2)
# 
# 2##
# c = 1
# sm = -1
# nm = 2
# sigma 5
# SDT(c, sm , nm, sigma)

