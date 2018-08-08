#make a function

SDT <- function(input_c, target_mean, input_sigma, foil_mean) {

  #initiate empty plot
  plot.new()
  #set margines:
  par(mar=c(5,5,3,7), xpd = T) #bottom, left, top and right margins, xpd = sets clipping to 'off' so that lables of likelihoods are plotted and not clipped
  
  nr_n = 50
  #plot(a)
  curve(dnorm(x, foil_mean, input_sigma), n= nr_n, xlim=c(0, 10), ylim=c(0, .55),
        xlab="Decision variable", ylab="Probability", frame=FALSE)
  legend("topright", legend=c("target", "foil"), lty=c(2, 1))
  
  segments(0,0, 10,0, col="gray31")  
  curve(dnorm(x, target_mean, input_sigma), lty=2, add=T)
  
  #shading:
  z_n <- seq(from=input_c, to=pi+foil_mean, length=50)
  z_n_y <- seq(from=input_c-foil_mean, to=pi, length=50)
  
  polygon(x=c(input_c, z_n, pi+foil_mean),
          y=c(0, dnorm(z_n_y, sd=input_sigma), 0),
          col=rgb(70/255, 130/255, 180/255, .85), border=NA)
  
  z_sn <- seq(from=input_c, to=2*pi+target_mean, length=100)
  polygon(x=c(input_c, z_sn, target_mean+2*pi),
          y=c(0, dnorm(z_sn, target_mean, input_sigma), 0),
          col=rgb(135/255, 206/255, 235/255, 0.35), border=NA)
  
  #c
  lines(x=c(input_c, input_c), y=c(0, .5), col="pink3", lwd=2)
  text(input_c, 0.52, "c")
  #d
  d_line_height = dnorm(0, sd = input_sigma)
  segments(foil_mean, d_line_height + 0.02, target_mean, d_line_height + 0.02) #horizontal line
  segments(foil_mean, 0, foil_mean, d_line_height+0.02, lty=3) #vertical line at foil mean
  segments(target_mean, 0, target_mean, d_line_height+0.02, lty=3) #vertical line at target mean
  text((target_mean+foil_mean)/2, d_line_height+0.04, "d'")
  
  
  #get foil likelihood & corss-over point:
  foil_like= dnorm(input_c - foil_mean, sd = input_sigma)
  target_like = dnorm(input_c - target_mean, sd = input_sigma)
  
  #draw the foil likelihood line
  scalar = .1
  location_foil_like = 12
  segments(input_c,foil_like, location_foil_like, foil_like, lty=3) 
  segments(location_foil_like,0, location_foil_like,foil_like, col = "black") #actual foil likelihood line
  text(location_foil_like + .5,foil_like-foil_like/3,"foil likelihood", srt = 90) #srt is to turn the text vertical 
  segments(location_foil_like- scalar, foil_like, location_foil_like+ scalar, foil_like, col = 'black')
  segments(location_foil_like- scalar, 0, location_foil_like+ scalar, 0, col = 'black') #the end bits
  
  #draw the target likelihood line
  location_target_like = 11
  segments(input_c,target_like, location_target_like, target_like, lty=3)
  segments(location_target_like,0,location_target_like,target_like, col = "black") #actual target likelihood line
  text(location_target_like + .5,target_like-target_like/3,"target likelihood", srt = 90)
  segments(location_target_like - scalar, target_like, location_target_like+scalar, target_like, col = 'black')
  segments(location_target_like - scalar, 0, location_target_like+scalar, 0, col = 'black') #the end bits
  
}
input_c = 4.2
target_mean = 6
input_sigma = 1
foil_mean = 4

SDT(input_c, target_mean, input_sigma, foil_mean)
  
