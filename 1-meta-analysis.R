#installing packages 

install.packages("metafor",dependencies=TRUE,repos="https://cloud.r-project.org")

#2 The funnel plot 

#2.1 Simulating data
#the funnel plot is an incredibly useful way of visualising effect size data
#this plot can tell us a lot about the underlying processes and biases that generated the data
#observing a funnel plot under the scenario where the dataset of different sizes were all generated with identical parameters and analysed with an lm
#aiming to simulate a dataset with a specified intercept and slope
#use rnorm to generate random normal predictor and residual; specify n, mean, sd

slope<--0.25
intercept<-0
predictor<-rnorm(n=100,mean=10,sd=10)
response<-intercept+slope*predictor+rnorm(n=100,mean=0,sd=40)
plot(predictor,response)

#question:run a simple lm with the data generated; how does the slope estimate compare with the slope generated
model<-lm(response~predictor)
summary(model)

#2.2 Simulating many datsets
#use the above simulation approach in a for loop to generate lots of datasets differing in size
#store the outputs in a matrix called store: the sample size, the slope, the se of the slope, and the P-value for the slope

store<-matrix(nrow=200,ncol=4)
#We need to create somewhere to store our data

for(x in 1:200){
  #we will simulate 200 different datasets 
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))+3
  #we're using this code to select sample sizes at random from a log normal distribution, so that small sample sizes are common and large sample sizes are rare. And n is always > 3.                  
  
  
  predictor<-rnorm(n=samplesize,mean=10,sd=10)
  response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
  #predictor and response are just as we used before, except now n is given by samplesize rather than n = 100
  
  model<-lm(response~predictor)
  #the same linear model as we ran before
  
  store[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4])
  #here we extract the model outputs we want and store them in our store matrix
  
  
}
store<-as.data.frame(store)
names(store)<-c("n","slope","standard.error","p.value")

#2.3 Producing and interpreting a funnel plot
#next step is to visualise the output on a funnel plot; scatterplot where the x-axis is the effect size and y-axis is the inverse of the sampling variance (sample size or se)
#below plot two funnel plots, one using sampling size and the other using precision (1/se)

par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Slope",ylab="Sample size")
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)")

#we can colour the slope estimates that are significant (P<0.05) red
#and indicate the slope that we used for the simulation with a vertical dashed line

sigslope<-which(store$p.value<0.05)
par(mfrow=c(1,2))
plot(store$slope,store$n,xlab="Slope",ylab="Sample size")
points(store$slope[sigslope],store$n[sigslope],pch=16,col="red")
abline(v=slope,lty=2)
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)")
points(store$slope[sigslope],(1/store$standard.error[sigslope]),pch=16,col="red")
abline(v=slope,lty=2)

#3 A basic meta analysis 
#3.1 Estimating mean effect size ignoring sampling variance
#often the aim of a meta-analysis is to estimate the mean effect size
#if we imagine that our 200 simulated slope estimates gave come from 200 different studies then the most straightfoward way of finding out mean effect size is to use an lm

model2<-lm(slope~1,data=store)
#The ~1 tells the model to fit an intercept only.
summary(model2)

#question: what is wring with the analysis above?
#ignores sampling variance, and the fact that some slopes are estimated much more accurately than others 

#3.2 estimating the mean effect size using metafor 
#metafor is an r package that has lots of functionality and flexibility for running meta-analyses 
#main r function is rma 

library(metafor)

?rma

meta<-rma(yi=slope,sei=standard.error,data=store)
meta

#summary of the main output:
  #the mean estimate and the standard error of the mean and 95% ci
  #tau^2 - an estimate of the variance in true effect sizes among studies 
  #i^2 statistic - tells us what proportion of the total variance (sampling variance + among study heterogeneity in effect size) is due to among study heterogeneity in effect size
  #test for heterogeneity - this provides a test of whether there is significant heterogeneity in effect sizes 
#should see from the model outputs that the estimate is close to the slope value we started with and the lack of heterogeneity in slopes is consistent with what we predicted 

#we can also see some of metafor's built in plotting functions to generate a funnel and forest plot

funnel(meta)

forest(meta,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra")

#4 A meta-analysis with moderators and random terms 
#4.1 simulating a new meta-dataset 

#this time we will generate datasets such that the slope estimates (effect sizes) vary as a function of another variable
#let’s imagine that the slopes we are generating correspond to the effect of temperature on phenology (days/degree C), and that this slope becomes more negative with latitude
#to achieve this we’ll adjust the simulation loop we used earlier, adding a new step to it where we generate a latitudinal relationship between temperature and the slope
#the code we’ll use to generate the latitudinal gradient is immediately below
#we’ve made it so that latitude predicts slope (ie. species from further north advance timings more in response to temperature), but some residual variation in slope remains
#you could think of this residual variation as being due to populations having slightly different slopes

latitude<-runif(100,0,90)
#we will randomly sample a latitude from 0,90 degree North
slope<-0+latitude*-0.1+rnorm(100,0,3)
plot(latitude,slope)

#next step we’ll add this to the code we had before
#add one extra thing, and that’s a random effect, such that slopes vary among 20 species
#slopes 1-10 will be for species 1, slopes 11-20 species 2 and so on

store2<-matrix(nrow=200,ncol=7)
#We need to create somewhere to store our data. We'll call this one store2 to distinguish it from the previous one. This time we also want to save the latitude and species that the slope estimate comes from. We will aslo save a unique ID for each observation - we can use this later to include a residual random effect

species<-rep(1:20,each=10)
specieseffect<-rep(rnorm(20,0,2),each=10)
#we will use this to generate our 20 species random effects

for(x in 1:200){
  #we will simulate 200 different datasets 
  
  latitude<-runif(1,0,90)
  
  slope<-0+specieseffect[x]+latitude*-0.1+rnorm(1,0,3)
  
  samplesize<-ceiling(exp(rnorm(1,4.5,1.5)))
  #we're using this code to select sample sizes at random from a log normal distribution, so that small sample sizes are common and large sample sizes are rare                    
  
  if(samplesize>3){
    #we included this if function so that we don't run an analyses on datasets that are too small
    predictor<-rnorm(n=samplesize,mean=10,sd=10)
    response<-intercept+predictor*slope+rnorm(n=samplesize,0,40)
    
    model<-lm(response~predictor)
    #the same linear model as we ran before
    
    store2[x,]<-c(samplesize,summary(model)$coefficients[2,1:2],summary(model)$coefficients[2,4],latitude,species[x],x)
    #here we extract the model outputs we want and store them in our store matrix
    
    
  }}
store2<-as.data.frame(store2)
names(store2)<-c("n","slope","standard.error","p.value","latitude","species","ID")

#4.2 funnel plot and simple meta-analysis 

plot(store2$slope,(1/store2$standard.error),xlab="Slope",ylab="Precision, (1/se)")

meta2<-rma(yi=slope,sei=standard.error,data=store2)
funnel(meta2)


#question: why doesn't the slope estimate funnel in much this time?
  #there is substantial heterogeneity in the slope, as is also reflected in the I^2 value and the test for heterogeneity

#4.3 a meta analysis and controlling for latitude 
#a meta-analysis model can be run in the same way as an lm or lmm and we can include fixed and random effects
#common to see fixed effects referred to as moderators, so all we need to add to the rma function is the model formula to include latitude as a covariate 

meta3<-rma(yi=slope,sei=standard.error,mods=~latitude,data=store2)
meta3
funnel(meta3)

#look at latitude, slope and tau value
#should be similar to latitudinal slope we simulated and the residual se in slopes that we simulated 
#we see after controlling for the effect of latitude, there is still some heterogeneity in slopes, but this is much less than in the meta2 model
#tau estimate will still be a bit elevated as we haven't taken into account the variance among species given by our species level random effects 

#4.4 A meta-analysis with random terms 
#next add a random term to our meta analysis 
#shift to using the rma.mv function and can add a random term by adding the argument random=~1|yourterm
#included the slope variance as the square of the standard error
#included an observation level random effect (to estimate the residual variance)

store2$se2<-store2$standard.error^2
store3<-store2[-which(is.na(store2$slope)==TRUE),]
#this function won't run with NAs, so we remove those rows
meta4<-rma.mv(yi=slope,V=se2,mods=~latitude,random=~1|species/ID,data=store3)
meta4

#meta4 we can see that the variance estimated among species (sigma^2.1) is similar to the variance we simulated (simulated value = 2^2 = 4)
#can also see that the residual variance (sigma^2.2) is similar to what we simulated (simulated value = 3^2 = 9)
#overall our meta-analysis has done a good job of recovering (estimating) the parameters used to generate the data

#5. Confronting a real dataset
birdbroods<-read.csv("~/Desktop/MSc EEB/WD//Meta-analysis/birdbroods.csv",sep=",",header=TRUE)

#plot the data using a funnel plot and run a meta analysis that includes two random terms 

plot(birdbroods$slope,(1/birdbroods$slope.SE),xlab="Slope",ylab="Precision, (1/se)")

birdbroods$se2<-birdbroods$slope.SE^2
meta5<-rma.mv(yi=slope,V=se2,random=~1|Species/id.pop,data=birdbroods)
meta5
funnel(meta5)
forest(meta5,cex.lab=0.8,cex.axis=0.8,addfit=TRUE,shade="zebra",order="obs")

#question: has the brood size of the average bird species declined?
  #no; small estimate and non-significant pval
#question: is more of the variation in slope estimates distributed among or within species?
  #
#question: is trend in brood size more positive for populations in protected areas? 
meta6<-rma.mv(yi=slope,V=se2,mods=~protected.area,random=~1|Species/id.pop,data=birdbroods)
meta6
  #no, more negative 
#question: if the information was available, what other terms do you think it would be worth including as random effects?
  #age, body size, first clutch? 

#6. Publication bias 
#return to the first dataset that we simulated (store)
#go through each row of data in turn and make it so the probability of being published is:
  #significant, probability published = 1
  #non-significant and <= 30 observations, probability published = 0.25
  #non-significant and >30 observations, probability published = 0.75
#we will then generate the before and after publication funnel plots 

store<-store[is.na(store$slope)==FALSE,]
store$publish<-0
#
store$publish[store$p.value<=0.05]<-1
largesamplesize<-intersect(which(store$p.value>0.05),which(store$n>30))
retainlarge<-largesamplesize[as.logical(rbinom(length(largesamplesize),prob=0.75,size=1))]
store$publish[retainlarge]<-1
smallsamplesize<-intersect(which(store$p.value>0.05),which(store$n<=30))
retainsmall<-smallsamplesize[as.logical(rbinom(length(smallsamplesize),prob=0.25,size=1))]

store$publish[retainsmall]<-1

par(mfrow=c(1,2))
plot(store$slope,(1/store$standard.error),xlab="Slope",ylab="Precision, (1/se)",main="Before")
plot(store$slope[store$publish==1],(1/store$standard.error[store$publish==1]),xlab="Slope",ylab="Precision, (1/se)",main="After")

#we see what an extreme case of publication bias looks like - a scarcity of low precision results that are close to the null
#various tests can be conducted to test for publication bias, one of the most informative is to visualise the funnel plot
#apply a test of whether final plot is symmetric

regtest(x=slope, sei=standard.error, data=store,
        model="rma", predictor="sei", ret.fit=FALSE)





