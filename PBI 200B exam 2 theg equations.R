#PBI 200B, Midterm 2 (Dr. Theg portion)
#all code below written (poorly) by Isaiah L Mohr

greyskull<-"By the power of Greyskull!"
#oxygen evolution under high light intensity
s0=0.25   #initial proportion of Mn in s0
s1=0.75   #initial proportion of Mn in s1
s2=0.0    #other states proportion = 0% (unstable or s4)
s3=0.0
s4=0.0
O2=0.0    #oxygen evolution (initial is, of course, 0)


#equations below
#s4 <- s4+ 0.85*s3 + 0.05*s2 - 1.0*s4
#s3 <- s3+ 0.85*s2 + 0.05*s1 - 0.9*s3
#s2 <- s2+ 0.85*s1 + 0.05*s0 - 0.9*s2
#s1 <- s1 + 0.85*s0 + 0.05*s3 - 0.9*s1
#s0 <- s0 + 1.0*s4 - 0.9*s0

#O2 <- 1*s4 + 0.05*s3

#variables for use in for() loop below
s4rec<-NULL
s3rec<-NULL
s2rec<-NULL
s1rec<-NULL
s0rec<-NULL
O2rec<-NULL


#for loop, runs the code 25 steps, recording changes in each step
flashes<-seq(1,25,1)
for(i in flashes){
  s4 <- s4+ 0.85*s3 + 0.05*s2 - 1.0*s4
  s3 <- s3+ 0.85*s2 + 0.05*s1 - 0.9*s3
  s2 <- s2+ 0.85*s1 + 0.05*s0 - 0.9*s2
  s1 <- s1 + 0.85*s0 + 0.05*s3 - 0.9*s1
  s0 <- s0 + 1.0*s4 - 0.9*s0
  
  O2 <- 1*s4 + 0.05*s3
  
  s4rec[i+1]<-s4
  s3rec[i+1]<-s3
  s2rec[i+1]<-s2
  s1rec[i+1]<-s1
  s0rec[i+1]<-s0
  O2rec[i+1]<-O2
  
}

#readout of the recorded O2 evolution at each step
O2rec

#graphing them:
greyskull
par(mfrow=c(1,3))
plot(O2rec, type="o", col="orange")
title(main="Oxygen Evolution under high intensity light", col.main="red", font.main=4)



######################
######################


#Oxygen evolution under 1/2 light intensity
s0l=0.25
s1l=0.75
s2l=0.0
s3l=0.0
s4l=0.0
O2l=0.0

#assumption of equations is 1/2 number of photons means 1/2 probabilities of change in state due to light

#s4.low <- s4l + 0.425*s3l + 0.025*s2l - 1.0*s4l
#s3.low <- s3l + 0.425*s2l + 0.025*s1l - 0.45*s3l
#s2.low <- s2l + 0.425*s1l + 0.025*s0l - 0.45*s2l
#s1.low <- s1l + 0.425*s0l + 0.025*s3l - 0.45*s1l
#s0.low <- s0l + 1.0*s4l - 0.45*s0l

#O2l <- 1*s4l + 0.025*s3l

s4rec.low<-NULL
s3rec.low<-NULL
s2rec.low<-NULL
s1rec.low<-NULL
s0rec.low<-NULL
O2rec.low<-NULL


flashes<-seq(1,25,1)				#1 to 25, in single steps
for(i in flashes){

  s4l <- s4l + 0.425*s3l + 0.025*s2l - 1.0*s4l
  s3l <- s3l + 0.425*s2l + 0.025*s1l - 0.45*s3l
  s2l <- s2l + 0.425*s1l + 0.025*s0l - 0.45*s2l
  s1l <- s1l + 0.425*s0l + 0.025*s3l - 0.45*s1l
  s0l <- s0l + 1.0*s4l - 0.45*s0l
  
  O2l <- 1*s4l + 0.025*s3l
  
  s4rec.low[i+1]<-s4l
  s3rec.low[i+1]<-s3l
  s2rec.low[i+1]<-s2l
  s1rec.low[i+1]<-s1l
  s0rec.low[i+1]<-s0l
  O2rec.low[i+1]<-O2l
}

O2rec.low

greyskull
plot(O2rec.low, type="o", col="purple")
title(main="Oxygen Evolution under half intensity light", col.main="blue", font.main=4)



####################
####################




#Oxygen evolution with 2 flashes and relaxation back; so...should begin with a larger s1 state value...
s0f=0.25
s1f=0.75
s2f=0.0
s3f=0.0
s4f=0.0
O2f=0.0


#s4f <- s4f + 0.85*s3f + 0.05*s2f - 1.0*s4f
#s3f <- s3f + 0.85*s2f + 0.05*s1f - 0.9*s3f
#s2f <- s2f + 0.85*s1f + 0.05*s0f - 0.9*s2f
#s1f <- s1f + 0.85*s0f + 0.05*s3f - 0.9*s1f
#s0f <- s0f + 1.0*s4f - 0.9*s0f

#O2f <- 1*s4f + 0.05*s3f

s4frec<-NULL
s3frec<-NULL
s2frec<-NULL
s1frec<-NULL
s0frec<-NULL
O2frec<-NULL


#running initial cycle for 2 steps (representing first 2 flashes before the 2 minutes of darkness)
flashes<-seq(1,2,1)				#1 to 25, in single steps
for(i in flashes){
  s4f <- s4f + 0.85*s3f + 0.05*s2f - 1.0*s4f
  s3f <- s3f + 0.85*s2f + 0.05*s1f - 0.9*s3f
  s2f <- s2f + 0.85*s1f + 0.05*s0f - 0.9*s2f
  s1f <- s1f + 0.85*s0f + 0.05*s3f - 0.9*s1f
  s0f <- s0f + 1.0*s4f - 0.9*s0f
  
  O2f <- 1*s4f + 0.05*s3f
  
  s4frec[i+1]<-s4f
  s3frec[i+1]<-s3f
  s2frec[i+1]<-s2f
  s1frec[i+1]<-s1f
  s0frec[i+1]<-s0f
  O2frec[i+1]<-O2f
  
}

#now take that and transfer everything to s0 and s1 - the devolution of s4-->s0, of s1&s2-->s1

s0g = s0f + s4f          #the new s0 proportion
s1g = s1f + s2f + s3f    #the new s1 proportion
s2g = 0
s3g = 0
s4g = 0

s0grec = NULL
s1grec = NULL
s2grec = NULL
s3grec = NULL
s4grec = NULL
O2grec = NULL

#and re-running it for 25 steps of light
flashes<-seq(1,25,1)
for(i in flashes){
  s4g <- s4g + 0.85*s3g + 0.05*s2g - 1.0*s4g
  s3g <- s3g + 0.85*s2g + 0.05*s1g - 0.9*s3g
  s2g <- s2g + 0.85*s1g + 0.05*s0g - 0.9*s2g
  s1g <- s1g + 0.85*s0g + 0.05*s3g - 0.9*s1g
  s0g <- s0g + 1.0*s4g - 0.9*s0g
  
  O2g <- 1*s4g + 0.05*s3g
  
  s4grec[i+1]<-s4g
  s3grec[i+1]<-s3g
  s2grec[i+1]<-s2g
  s1grec[i+1]<-s1g
  s0grec[i+1]<-s0g
  O2grec[i+1]<-O2g
  
}

plot(O2grec, type="o", col="brown")
title(main="Oxygen Evolution after initial flashes", col.main="green", font.main=4)


#and final plots are received!
#at half intensity, O2 evolution stabilizes at ~0.12 copared to 0.25 and does so very early (by about flash 15 \
#compared to no signs of a steady-state yet in high intensity), while priming the Mn in a s1 state increases the \
#aount of O2 evolved initially but it seems to stabilize at about the same point as in high-intensity light.