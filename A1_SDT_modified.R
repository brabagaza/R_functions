#Van testing if Git works

input_c = 4.2
input_d =  6
input_sigma_sn= 1
foil_mean = 4

#set margines:
par(mar=c(5,5,3,7), xpd = NA) #bottom, left, top and right margins, xpd = sets clipping to 'off' so that lables of likelyhoods are plotted and not clipped

curve(dnorm(x, foil_mean, 1), xlim=c(0, 10), ylim=c(0, .55),
      xlab="Decision variable", ylab="Probability", frame=FALSE)
      legend("topright", legend=c("signal", "foil"), lty=c(2, 1))
      abline(h=0, col="gray31")  
      curve(dnorm(x, input_d, input_sigma_sn), lty=2, add=T)

#cover line to get rid of black line underneath
abline(h = 0,col= 'white')

#
z_n <- seq(from=input_c, to=pi+foil_mean, length=50)
z_n_y <- seq(from=input_c-foil_mean, to=pi, length=50)

polygon(x=c(input_c, z_n, pi+foil_mean),
        y=c(0, dnorm(z_n_y), 0),
        col=rgb(70/255, 130/255, 180/255, .85), border=NA)

z_sn <- seq(from=input_c, to=2*pi+input_d, length=100)
polygon(x=c(input_c, z_sn, input_d+2*pi),
        y=c(0, dnorm(z_sn, input_d, input_sigma_sn), 0),
        col=rgb(135/255, 206/255, 235/255, 0.35), border=NA)

#c
lines(x=c(input_c, input_c), y=c(0, .5), col="pink3", lwd=2)
text(input_c, 0.52, "c")
#d
segments(foil_mean, 0.42, input_d, 0.42)
segments(foil_mean, 0, foil_mean, 0.43, lty=3)
segments(input_d, 0, input_d, 0.43, lty=3)
text((input_d+foil_mean)/2, 0.44, "d'")


####Van
#draw the foil likelihood line
scalar = .05
foil_like = 0.39
location_foil_like = 12
segments(input_c,foil_like, location_foil_like, foil_like, col = "blue")
segments(location_foil_like,0, location_foil_like,0.39, col = "black")
text(location_foil_like - .4,foil_like-foil_like/3,"foil likelihood", srt = 90) #srt is to turn the text vertical 
segments(location_foil_like- scalar, foil_like, location_foil_like+ scalar, foil_like, col = 'black', lwd = 4)
segments(location_foil_like- scalar, 0, location_foil_like+ scalar, 0, col = 'black', lwd = 4)

#draw the signal likelihood line
signal_like = 0.07
location_signal_like = 11
segments(input_c,signal_like, location_signal_like, signal_like, col = "blue")
segments(location_signal_like,0,location_signal_like,signal_like, col = "black")
text(location_signal_like - .4,signal_like-signal_like/3,"foil likelihood", srt = 90)
segments(location_signal_like - scalar, signal_like, location_signal_like+scalar, signal_like, col = 'black', lwd = 4)
segments(location_signal_like - scalar, 0, location_signal_like+scalar, 0, col = 'black', lwd = 1)
