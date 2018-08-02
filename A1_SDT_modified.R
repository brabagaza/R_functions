input_c = 4.2
input_d =  6
input_sigma_sn= 1
foil_mean = 4
curve(dnorm(x, foil_mean, 1), xlim=c(0, 10), ylim=c(0, .55),
      xlab="Decision variable", ylab="Probability")
legend("topright", legend=c("signal", "foil"), lty=c(2, 1))
abline(h=0, col="gray31")  
curve(dnorm(x, input_d, input_sigma_sn), lty=2, add=T)

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



#adding the likelyhood lines
likely = .35
segments(foil_mean+3, likely, input_d+2, likely,  lty=3)
#segments(foil_mean+3, 0, foil_mean+3, 0.42,  lty=3, col = 'red')
segments(input_d+2, 0, input_d+2, likely,  col = 'red')

#small end line to top the line off with:
scalar = .05
segments(input_d+2 - scalar, likely, input_d+2+scalar, likely, col = 'red', lwd = 4)
segments(input_d+2 - scalar, 0, input_d+2+scalar, 0, col = 'red', lwd = 4)



#############################
#c= 2
#z_n_y <- seq(from=c, to=pi, length=50)
#x = seq(1, 50)
#y = dnorm(z_n_y)
#plot(x,y)







