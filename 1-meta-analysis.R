#installing packages 

install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")

#2 The funnel plot 

#2.1 Simulating data
#the funnel plot is an incredibly useful way of visualising effect size data
#this plot can tell us a lot about the underlying processes and biases that generated the data
#observing a funnel plot under the scenario where the dataset of different sizes were all generated with identical parameters and analysed with an lm
#aiming to simulate a dataset with a specified intercept and slope
#use rnorm to generate random normal predictor and residual; specify n, mean, sd

slope<-0.25
intercept<-0
predictor<-rnorm(n=100,mean=10,sd=10)
response<-intercept+slope*predictor+rnorm(n=100,mean=0,sd=40)
plot(predictor,response)
