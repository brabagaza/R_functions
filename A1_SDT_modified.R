#make a function
input_c = 4.2
signal_mean =  6
input_sigma_sn= .8
foil_mean = 4

SDT <- function(input_c, signal_mean, input_sigma_sn, foil_mean) {

  #initiate empty plot
  plot.new()
  #set margines:
  par(mar=c(5,5,3,7), xpd = T) #bottom, left, top and right margins, xpd = sets clipping to 'off' so that lables of likelyhoods are plotted and not clipped
  
  nr_n = 50
  #plot(a)
  curve(dnorm(x, foil_mean, input_sigma_sn), n= nr_n, xlim=c(0, 10), ylim=c(0, .55),
        xlab="Decision variable", ylab="Probability", frame=FALSE)
  legend("topright", legend=c("signal", "foil"), lty=c(2, 1))
  
  segments(0,0, 10,0, col="gray31")  
  curve(dnorm(x, signal_mean, input_sigma_sn), lty=2, add=T)
  
  #shading:
  z_n <- seq(from=input_c, to=pi+foil_mean, length=50)
  z_n_y <- seq(from=input_c-foil_mean, to=pi, length=50)
  
  polygon(x=c(input_c, z_n, pi+foil_mean),
          y=c(0, dnorm(z_n_y, sd=input_sigma_sn), 0),
          col=rgb(70/255, 130/255, 180/255, .85), border=NA)
  
  z_sn <- seq(from=input_c, to=2*pi+signal_mean, length=100)
  polygon(x=c(input_c, z_sn, signal_mean+2*pi),
          y=c(0, dnorm(z_sn, signal_mean, input_sigma_sn), 0),
          col=rgb(135/255, 206/255, 235/255, 0.35), border=NA)
  
  #c
  lines(x=c(input_c, input_c), y=c(0, .5), col="pink3", lwd=2)
  text(input_c, 0.52, "c")
  #d
  d_line_height = dnorm(0, sd = input_sigma_sn)
  segments(foil_mean, d_line_height, signal_mean, d_line_height)
  segments(foil_mean, 0, foil_mean, d_line_height+0.2, lty=3)
  segments(signal_mean, 0, signal_mean, d_line_height+0.2, lty=3)
  text((signal_mean+foil_mean)/2, d_line_height+0.4, "d'")
  
  
  #getting foil likelyhood, corssoverpoint:
  foil_like= dnorm(input_c - foil_mean, sd = input_sigma_sn)
  signal_like = dnorm(input_c - signal_mean, sd = input_sigma_sn)
  
  ####Van
  #draw the foil likelihood line
  scalar = .1
  location_foil_like = 12
  segments(input_c,foil_like, location_foil_like, foil_like, col = "blue")
  segments(location_foil_like,0, location_foil_like,0.39, col = "black")
  text(location_foil_like + .5,foil_like-foil_like/3,"foil likelihood", srt = 90) #srt is to turn the text vertical 
  segments(location_foil_like- scalar, foil_like, location_foil_like+ scalar, foil_like, col = 'black')
  segments(location_foil_like- scalar, 0, location_foil_like+ scalar, 0, col = 'black')
  
  #draw the signal likelihood line
  location_signal_like = 11
  segments(input_c,signal_like, location_signal_like, signal_like, col = "blue")
  segments(location_signal_like,0,location_signal_like,signal_like, col = "black")
  text(location_signal_like + .5,signal_like-signal_like/3,"signal likelihood", srt = 90)
  segments(location_signal_like - scalar, signal_like, location_signal_like+scalar, signal_like, col = 'black')
  segments(location_signal_like - scalar, 0, location_signal_like+scalar, 0, col = 'black')
  
}

input_c = 4.2
signal_mean =  6
input_sigma_sn= .8
foil_mean = 4

SDT_func(input_c, signal_mean,input_sigma_sn,foil_mean)
