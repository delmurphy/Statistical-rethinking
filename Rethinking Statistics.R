#Richard McElreath's Rethinking Statistics course on Bayesian Stats

#Lecture 1
#install RStan (see http://mc-stan.org/)
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE, lib = "C:/personal_R_lib")
# #Install R McElreath's rethinking package
# install.packages(c("coda", "mvtnorm", "devtools", "loo"), lib = "C:/personal_R_lib")
# library(devtools, lib.loc = "C:/personal_R_lib")
# devtools::install_github("rmcelreath/rethinking", ref="Experimental", lib = "C:/personal_R_lib")
#library(V8, lib.loc = "C:/personal_R_lib")
#library(rethinking, lib.loc = "C:/personal_R_lib")
library(rethinking)
#2nd edition book draft http://xcelab.net/rm/sr2/
#password: tempest

###########################?#############################################################################
#########################################################################################################
##Chapter 2
############################################################?############################################
#########################################################################################################

#Grid Approximation

#What's the probability distribution look like for observing 6 counts of water out ?f 9 total globe tosses 
#if the proportion of the globe that is actually covered by water is set to different values of prob?

p_grid <- seq(0, 1, length.out = 100)
prior <- rep(1, 100)
likelihood <- dbinom(6, size = 9, p_grid)
unstd.posterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior)
plot(posterior~p_grid)

#playing with the priors:
prior<-ifelse(p_grid < 0.5, 0, 1)
prior<-exp(-5*abs(p_grid-0.5))

#Quadratic approximation

#Use rethinking::quap() to estimate the mode of the poste?ior distribution and the curvature near the peak
#in order to compute a quadratic approximation (Gaussian approximation) of the entire distribution

globe.qa <- quap(
  alist(
    W ~ dbinom(W+L, p), #binomial likelihood
    p ~ dunif(0,1) #uniform prior
  ),
  data = list(W=6, L=3)
)

#display summary of quadratic approximation:
precis(globe.qa)
#   mean   sd 5.5% 94.5%
# p 0.67 0.16 0.42  0.92


#Monte Carlo Markov Chain (MCMC)
#an example of the Metropolis algorithm to show that estimating the posteri?r distribution by sampling from the posterior distribution
#results in a similar estimate to the analytical posterior distribution
n_samples <- 1000
p <- rep( NA , n_samples )
p[1] <- 0.5
W <- 6
L <- 3
for ( i in 2:n_samples ) {
  p_new <- rnorm( 1 , p[i-1] , 0.1 )
  if ( p_new < 0 ) p_new <- abs( p_new )
  if ( p_new > 1 ) p_new <- 2 - p_new
  q0 <- dbinom( W , W+L , p[i-1] )
  q1 <- dbinom( W , W+L , p_new )
  p[i] <- ifelse( runif(1) < q1/q0 , p_new , p[i-1] )
}
#now plot the distrribution of p (samples f?om the posterior) to the analytical solution for the posterior
dens( p , xlim=c(0,1) )
curve( dbeta( x , W+1 , L+1 ) , lty=2 , add=TRUE )


#Chapter 2 Practice:
#Medium
#2M1. Recall the globe tossing model from the chapter. Compute and plot the grid approx?mate
# posterior distribution for each of the following sets of observations. In each case, assume a uniform
# prior for p.
# (1) W, W, W
# (2) W, W, W, L
# (3) L, W, W, L, W, W, W

p_grid<-seq(0,1,length.out = 100)
prior<-rep(1,100)
#(1)
likelihood<-dbinom(3, size = 3, p_grid)
unstd.posterior<-likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot(posterior~p_grid)
#(2)
likelihood<-dbinom(3, size = 4, p_grid)
unstd.posterior<-likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
pl?t(posterior~p_grid)
#(3)
likelihood<-dbinom(5, size = 7, p_grid)
unstd.posterior<-likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot(posterior~p_grid)

# 2M2. Now assume a prior for p that is equal to zero when p < 0.5 and is a positive ?onstant when
# p ??? 0.5. Again compute and plot the grid approximate posterior distribution for each of the sets of
# observations in the problem just above.
p_grid<-seq(0,1, length.out = 100)
prior<-ifelse(p_grid<0.5,0,1)
#(1)
likelihood<-dbinom(3, size = 3, p_grid)
unstd.p?sterior <- likelihood*prior
posterior <- unstd.posterior/sum(unstd.posterior)
plot(posterior~p_grid)
#(2)
likelihood<-dbinom(3, size = 4, p_grid)
unstd.posterior<-likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot(posterior~p_grid)
#(3)
?ikelihood<-dbinom(5, size = 7, p_grid)
unstd.posterior<-likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
plot(posterior~p_grid)

# 2M3. Suppose there are two globes, one for Earth and one for Mars. The Earth globe is 70% covered
# in water.?The Mars globe is 100% land. Further suppose that one of these globes-you don't know
# which-was tossed in the air and produced a "land" observation. Assume that each globe was equally
# likely to be tossed. Show that the posterior probability that the globe was the Earth, conditional on
# seeing "land" (Pr(Earth|land)), is 0.23
p_grid<-c(0, 0.7) #proportion of mars that is water == 0, prop earth water == 0.7
prior<-c(1,1) #equally likely prior probability that the globe was earth or mars
likelihood<-dbinom(0, size = 1, p_grid)
unstd.posterior <- likelihood*prior
posterior<-unstd.posterior/sum(unstd.posterior)
post.earth <- posterior[2]

# 2M4. Suppose you have a deck with only three cards. Each card has two sides, and each side is either
# black or white. One?card has two black sides. The second card has one black and one white side. The
# third card has two white sides. Now suppose all three cards are placed in a bag and shuffled. Someone
# reaches into the bag and pulls out a card and places it flat on a tabl?. A black side is shown facing up,
# but you don't know the color of the side facing down. Show that the probability that the other side is
# also black is 2/3. Use the counting method (Section 2 of the chapter) to approach this problem. This
# means count?ng up the ways that each card could produce the observed data (a black side facing up
# on the table)
c1<-2
c2<-1
c3<-0
probc1<-c1/sum(c1,c2,c3)


# 2M5. Now suppose there are four cards: B/B, B/W, W/W, and another B/B. Again suppose a card is
# drawn from?the bag and a black side appears face up. Again calculate the probability that the other
# side is black. (using counting method - Bayesian updating)
c1<-2*2
c2<-1*1
c3<-0*1
probc1<-c1/sum(c1,c2,c3) #0.8

# 2M6. Imagine that black ink is heavy, and so card? with black sides are heavier than cards with white
# sides. As a result, it's less likely that a card with black sides is pulled from the bag. So again assume
# there are three cards: B/B, B/W, and W/W. After experimenting a number of times, you conclude ?hat
# for every way to pull the B/B card from the bag, there are 2 ways to pull the B/W card and 3 ways to
# pull the W/W card. Again suppose that a card is pulled and a black side appears face up. Show that
# the probability the other side is black is now?0.5. Use the counting method, as before.
c1<-2*1
c2<-1*2
c3<-0*3
probc1<-c1/sum(c1,c2,c3) #0.5

# 2M7. Assume again the original card problem, with a single card showing a black side face up. Before
# looking at the other side, we draw another card from th? bag and lay it face up on the table. The face
# that is shown on the new card is white. Show that the probability that the first card, the one showing
# a black side, has black on its other side is now 0.75. Use the counting method, if you can. Hint: Trea?
# this like the sequence of globe tosses, counting all the ways to see each observation, for each possible
# first card.
#if c1 was B/B:
#  2 ways to get 1st ob, 3 ways to get 2nd ob (w/w =2 + w/b = 1)
#if c1 was B/W:
# 1 way to get 1st ob, 2 ways to get ?nd ob (w/w)
#if c1 was W/W:
#  0 ways to get obs1, 1 ways to get 2nd ob (w/b)
bb<-2*3
bw<-1*2
ww<-0*1
bb/sum(bb,bw,ww) #0.75

# 2H1. Suppose there are two species of panda bear. Both are equally common in the wild and live
# in the same places. They look e?actly alike and eat the same food, and there is yet no genetic assay
# capable of telling them apart. They differ however in their family sizes. Species A gives birth to twins
# 10% of the time, otherwise birthing a single infant. Species B births twins 20? of the time, otherwise
# birthing singleton infants. Assume these numbers are known with certainty, from many years of field
# research.
# Now suppose you are managing a captive panda breeding program. You have a new female panda
# of unknown species, and?she has just given birth to twins. What is the probability that her next birth
# will also be twins?
p_grid<-c(0.1, 0.2)
prior<-c(1,1)
likelihood<-dbinom(1,size = 1, p_grid)
unstd.posterior<-prior*likelihood
posterior<-unstd.posterior/sum(unstd.posterior) #this becomes prior for next question: what's the likelihood of having twins next?
#result
sum(posterior*likelihood) #0.167


# 2H2. Recall all the facts from the problem above. Now compute the probability that the panda we
# have is from species A, assumin? we have observed only the first birth and that it was twins.
p_grid<-c(0.1, 0.2)
prior<-c(1,1)
likelihood<-dbinom(1, size = 1, p_grid) #likelihood of twins - in this case, same as c(0.1, 0.2)
unstd.posterior<-prior*likelihood
posterior<-unstd.posterior/su?(unstd.posterior) #0.33

# 2H3. Continuing on from the previous problem, suppose the same panda mother has a second birth
# and that it is not twins, but a singleton infant. Compute the posterior probability that this panda is
# species A.
#Bayesian updati?g:
prior<-posterior
likelihood<-c(1-0.1, 1-0.2) #likelihood of singleton - in this case equivalent to dbinom(1, 1, 1-p_grid)
unstd.posterior<-prior*likelihood
posterior<-unstd.posterior/sum(unstd.posterior) #0.36

# 2H4. A common boast of Bayesian statisti?ians is that Bayesian inference makes it easy to use all of
# the data, even if the data are of different types.
# So suppose now that a veterinarian comes along who has a new genetic test that she claims can
# identify the species of our mother panda. But?the test, like all tests, is imperfect. This is the information you have about the test:
#   . The probability it correctly identifies a species A panda is 0.8.
#   . The probability it correctly identifies a species B panda is 0.65.
# The vet administers ?he test to your panda and tells you that the test is positive for species A. First
# ignore your previous information from the births and compute the posterior probability that your
# panda is species A. Then redo your calculation, now using birth data as ?ell.
#Calculation based on genetic test:
prior<-c(1,1) #before test result, equally likely that the panda is from either species
likelihood<-c(0.8,1-0.65) #knowledge about the certainty of result of the test assuming it is truly species A
posterior<- prior?likelihood
posterior<-posterior/sum(posterior) #0.696
#Update caluclation based on births (twins then singleton)
prior<-posterior
likelihood<-c(0.1*(1-0.1),0.2*(1-0.2)) #prob of twins*prob of not twins [likelihood of two events (p(twins_step1 & single_step?|species=X))]
posterior<-prior*likelihood
posterior<-posterior/sum(posterior) #0.56


####################################################################################################################################################

#Chapter 3 practice:
#Sample from the posterior distribution for the globe tossing example:
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size=1e4 , replace=TRUE )

#Use the values in samples to answer the questions that follow.
#3E1. How much posterior probability lies below p = 0.2
sum(samples<0.2)/1e4 #same as length(samples[samples>0.2]/1e4) and mean(samples<0.2) #4e-04 or 0.0004
#3E2. How much posterior probability lies above p = 0.8?
mean(samples>0.8) #0.1116
#3E3. How much posterior probability lies between p = 0.2 and p = 0.8
mean(samples>0.2 & samples<0.8) #0.888
#3E4. 20% of the posterior probability lies below which value of p?
quantile(samples, 0.2) #0.52
#3E5. 20% of the posterior probability lies above which value of p?
quantile(samples, 0.8) #0.76
#3E6. Which values of p contain the narrowest interval equal to 66% of the posterior probability?
rethinking::HPDI(samples, prob=0.66) #Highest Posterior Density Interval 0.51 - 0.77
#plot this:
d<-as.data.frame(samples)
p1<-ggplot(data = d, aes(x = samples)) +
  geom_density()
p2<-ggplot_build(p1)$data[[1]]
p1 + geom_area(data = subset(p2,x>0.51 & x<0.77), aes(x = x, y = y), fill = "blue")
#3E7. which values of p containt 66% of the posterior probability, assuming equal posterior probability
# both below and above the interval?
quantile(samples, prob = c(0.165, 0.835)) #0.499 - 0.772
rethinking::PI(samples, prob = 0.66) #0.502 - 0.77
#plot this:
p3<-ggplot_build(p1)$data[[1]]
p1 + geom_area(data = subset(p3, x>0.5 & x<0.77), aes(x = x, y = y), fill = "blue")

#Medium
#3M1. Suppose the globe tossing data had turned out to be 8 water in 15 tosses. Construct the posterior
# distribution, using grid approximation. Use the same flat prior as before.
pgrid<-seq(0,1, length.out = 1000)
prior<-rep(1,1000)
likelihood<-dbinom(8, 15, prob = pgrid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
plot(posterior, type = "l")
#3M2. Draw 10,000 samples from the grid approximation from above. 
# Then use the samples to calculate the 90% HPDI for p. (and plot it)
samples<-sample(pgrid, size = 1e4, replace = TRUE, prob = posterior)
rethinking::HPDI(samples, prob = 0.9) #0.329 - 0.717
p1<-ggplot(data = as.data.frame(samples), aes(x = samples)) + geom_density()
p2<-ggplot_build(p1)$data[[1]]
p1 + geom_area(data = subset(p2, x>0.3293 & x<0.7167), aes(x = x, y = y), fill = "blue")
#3M3. Construct a posterior predictive check for this model and data. 
# This means simulate the distribution of samples, averaging over the posterior uncertainty in p. 
# What is the probability of observing 8 water in 15 tosses?
dummydata<-rbinom(1e4, size = 15, prob = samples)
ggplot(data = as.data.frame(dummydata), aes(x = dummydata)) + geom_bar()
mean(dummydata==8) #probability of observing 8 water in 15 tosses = 0.148
#3M4. Using the posterior distribution constructed from the new (8/15) data, 
# now calculate the probability of observing 6 water in 9 tosses.
dummydata<-rbinom(1e4, size = 9, prob = samples)
ggplot(data = as.data.frame(dummydata), aes(x = dummydata)) + geom_bar()
mean(dummydata==6) #probability of observing 6 water in 9 tosses = 0.1745
#3M5. Start over at 3M1, but now use a prior that is zero below p = 0.5 and a constant above p = 0.5.
# This corresponds to prior information that a majority of the Earth's surface is water. Repeat each
# problem above and compare the inferences. What difference does the better prior make? If it helps,
# compare inferences (using both priors) to the true value p = 0.7.
## First, generate the new posterior
pgrid<-seq(0,1,length.out = 1000)
prior<-as.numeric(p_grid>0.5)
plot(prior, type = "l", x = pgrid)
likelihood<-dbinom(8,15,prob = pgrid)
posterior2<-likelihood*prior
posterior2<-posterior2/sum(posterior2)
plot(posterior2, type = "l")
## now draw 10,000 samples from the posterior and calculate the 90% HPDI for p.
samples<-sample(pgrid, 1e4, replace = TRUE, prob = posterior2)
rethinking::HPDI(samples, prob = 0.9) #0.5005005 0.7097097 
p1<-ggplot(data = as.data.frame(samples), aes(x = samples)) + geom_density() + xlim(c(0,1))
p2<-ggplot_build(p1)$data[[1]]
p1 + geom_area(data = subset(p2, x>0.5005 & x<0.70971), aes(x = x, y = y), fill = "blue")
## next construct posterior predictive check for the model and data 
## (i.e. simulate the distribution of samples, averaging over the posterior uncertainty in p.
## what is the probability of observing 8 water in 15 tosses?)
dummydata<-rbinom(1e4, size = 15, prob = samples)
rethinking::simplehist(dummydata)
mean(dummydata==8) #probability of observing 8 water in 15 tosses = 0.1564
## next, using the posterior distribution constructed from the 8/15 data,
## calculate the probability of observing 6 water in 9 tosses
dummydata<-rbinom(1e4, size = 9, prob = samples)
rethinking::simplehist(dummydata)
mean(dummydata==6) # probability of observing 6 water in 9 tosses = 0.2325
## finally, compare inferences using both priors to the true value p = 0.7:
## (what would the probability of observing 8/15 or 6/9 be under prob = 0.7?)
dbinom(8, size = 15, prob = 0.7) #0.081 (cf posterior1 = 0.148; posterior 2 = 0.1564)
rethinking::simplehist(rbinom(1e4, 15, prob = 0.7))
dbinom(6, size = 9, prob = 0.7) #0.267 (cf posterior1 = 0.1745; posterior 2 = 0.2325)
rethinking::simplehist(rbinom(1e4, 9, prob = 0.7))
## what's the likelihood that p = 0.7 given the two posteriors?
posterior[pgrid > 0.68 & pgrid < 0.72]
#both posteriors include the true value in the 90% HPDI, although the HDPI for posterior 1 covers a wider range of values of p.

#Hard
# Introduction. The practice problems here all use the data below. These data indicate the gender
# (male=1, female=0) of officially reported first and second born children in 100 two-child families.
library(rethinking)
data(homeworkch3)
birth1
birth2
# So for example, the first family in the data reported a boy (1) and then a girl (0). The second family
# reported a girl (0) and then a boy (1). The third family reported two girls.
# Use these vectors as data. So for example to compute the total number of boys born across all of these
# births, you could use:
sum(birth1,birth2)  #111 boys out of 200 births
# 3H1. Using grid approximation, compute the posterior distribution for the probability of a birth
# being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?
pgrid<-seq(0,1, length.out = 1000)
prior<-rep(1,1000)
likelihood<-dbinom(111, size = 200, prob = pgrid)
posterior = prior*likelihood
posterior<-posterior/sum(posterior)
plot(posterior, x=pgrid, type = "l")
pgrid[which.max(posterior)] # parameter value which maximises the posterior prob = 0.55
# 3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. 
# Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals.
samples<-sample(pgrid, 1e4, replace = TRUE, prob = posterior)
HPDI(samples, prob = 0.5) #0.5265265 0.5725726
HPDI(samples, prob = 0.89) #0.4964965 0.6066066
HPDI(samples, prob = 0.97) #0.4774775 0.627627
p1<-ggplot(data = as.data.frame(samples), aes(x = samples)) + geom_density()
p2<-ggplot_build(p1)$data[[1]]
p1 + geom_area(data = subset(p2, x>0.4774775 & x<0.627627), aes(x = x, y = y), fill = "green") +
  geom_area(data = subset(p2, x>0.4964965 & x<0.6066066), aes(x = x, y = y), fill = "blue") +
  geom_area(data = subset(p2, x>0.5265265 & x<0.5725726), aes(x = x, y = y), fill = "red")
# 3H3. Use rbinom to simulate 10,000 replicates of 200 births. You should end up with 10,000 numbers, 
# each one a count of boys out of 200 births. Compare the distribution of predicted numbers
# of boys to the actual count in the data (111 boys out of 200 births). There are many good ways to
# visualize the simulations, but the dens command (part of the rethinking package) is probably the
# easiest way in this case. Does it look like the model fits the data well? That is, does the distribution
# of predictions include the actual observation as a central, likely outcome?
dummydata<-rbinom(1e4, size = 200, prob = samples)
rethinking::dens(dummydata)
abline(v = 111)
#yes - the simulated distribution indicates that the observed value is highly likely.
# 3H4. Now compare 10,000 counts of boys from 100 simulated first borns only to the number of boys
# in the first births, birth1. How does the model look in this light?
n_boys_1<-sum(birth1)
dummydata<-rbinom(1e4, size = 100, prob = samples)
rethinking::dens(dummydata)
abline(v = n_boys_1)  
mean(dummydata)#55.4431
median(dummydata)#55
#the model overpredicts the most likely number of boys in 100 first births
# 3H5. The model assumes that sex of first and second births are independent. To check this assumption, focus now on second births that followed female first borns. 
# Compare 10,000 simulated counts of boys to only those second births that followed girls. To do this correctly, you need to count the number of first borns who were 
# girls and simulate that many births, 10,000 times. Compare the counts of boys in your simulations to the actual observed count of boys following girls. How does the
# model look in this light? Any guesses what is going on in these data?
n_girls_1<-100-sum(birth1)
n_boys_after_girls<-sum(birth2[birth1==0])  #39
dummydata<-rbinom(1e4, size = n_girls_1, prob = samples)
dens(dummydata)
abline(v = n_boys_after_girls)
mean(dummydata) #27.1391
median(dummydata) #27
#the most likely model predicted number of boys as a second birth following the birth of a girl is much lower (27) than the observed value (39)

####################################################################################################################################################

#Chapter 4 practice:

#Medium
# 4M1. For the model definition below, simulate observed y values from the prior (not the posterior).
# yi ??? Normal(??, ??)
# ?? ??? Normal(0, 10)
# ?? ??? Exponential(1)
sim_mu<-rnorm(1e4, 0, 10)
sim_sigma<-rexp(1e4, 1)
sim_y<-rnorm(sim_mu, sim_sigma)
dens(sim_y)
# 4M2. Translate the model just above into a quap formula.
flist <- alist(
    y ~ dnorm(mu, sigma),
    mu ~ dnorm(0,10),
    sigma ~ dexp(1)
  )
# 4M3. Translate the quap model formula below into a mathematical model definition.
# flist <- alist(
#   y ~ dnorm( mu , sigma ),
#   mu <- a + b*x,
#   a ~ dnorm( 0 , 10 ),
#   b ~ dunif( 0 , 1 ),
#   sigma ~ dexp( 1 )
# )
### ans:
# yi ~ Normal(mu, sigma)
# mu = a + b*x
# a ~ Normal(0, 10)
# b ~ Uniform(0, 1)
# sigma ~ Exponential(1)
# 4M4. A sample of students is measured for height each year for 3 years. After the third year, you want
# to fit a linear regression predicting height using year as a predictor. Write down the mathematical
# model definition for this regression, using any variable names and priors you choose. Be prepared to
# defend your choice of priors.
### ans:
# Hi ~ Normal(mu_i, sigma)
# mu_i = a + b*(year_i - mean_year)
# a ~ Normal(178, 20)
# b ~ Log-Normal(0,1)
# sigma ~ Uniform(0,50)
# 4M5. Now suppose I remind you that every student got taller each year. Does this information lead
# you to change your choice of priors? How?
## ans:
# No, as this assumption was already built into the priors (using a log-normal distribution for b forces the slope to be positive)
# 4M6. Now suppose I tell you that the variance among heights for students of the same age is never
# more than 64cm. How does this lead you to revise your priors?
### ans:
# sigma = sqrt(variance), thus we can update our prior for sigma as:
# sigma ~ Uniform(0, 8)

#Hard
# 4H1. The weights listed below were recorded in the !Kung census, but heights were not recorded for
# these individuals. Provide predicted heights and 89% intervals for each of these individuals. That is,
# fill in the table below, using model-based predictions.
# Individual  weight  expected height   89% interval
# 1           46.95             156.4    148.3-164.5
# 2           43.72             153.5    145.3-161.6
# 3           64.78             172.5    164.2-180.5
# 4           32.59             143.4    135.2-151.7
# 5           54.63             163.2    155.4-171.4
#first, load the data and create the model:
d<-Howell1
plot(height~weight, data = d, xlim = c(30, 66))
#The data include children and adults, so the relationship between weight and height is not linear.
#For the purposes of this question, all weights seem to be for adults, so let's use only the adult data and apply a linear model.
d<-d[d$age>=18,]
xbar<-mean(d$weight)
m4h1<-quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight-xbar),
    a ~ dnorm(160, 10),
    b ~ dlnorm(0, 1),
    sigma ~ dunif(0,50)
  ),
  data = d
)
precis(m4h1)
#now simulate heights for people of weights given in the table
weights<-c(46.95,
           43.72,
           64.78,
           32.59,
           54.63)
#simulate heights:
sim.heights<- sim(m4h1, data = list(weight = weights), n = 1e4)
#summarise posterior predictions:
mean.heights<-apply(sim.heights, 2, mean)
PI.heights<-apply(sim.heights, 2, PI, 0.89)
#plot these points on top of the rest of the data
points(weights, mean.heights, col = "red", pch = 15)
segments(weights, PI.heights[1,], weights, PI.heights[2,], col = "red")

# 4H2. Select out all the rows in the Howell1 data with ages below 18 years of age. If you do it right,
# you should end up with a new data frame with 192 rows in it.
# (a) Fit a linear regression to these data, using quap. Present and interpret the estimates. For
# every 10 units of increase in weight, how much taller does the model predict a child gets?
d<-Howell1 %>% filter(age<18)
plot(height~weight, data = d)
xbar<-mean(d$weight)
m4h2<-quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(weight-xbar),
    a ~ dnorm(115, 10),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)
  ),
  data = d
)
precis(m4h2)
#the model predicts that for every 10 units increase in weight, a child should get 27.2cm taller (on average)

# (b) Plot the raw data, with height on the vertical axis and weight on the horizontal axis. Superimpose
# the MAP regression line and 89% interval for the mean. Also superimpose the 89% interval
# for predicted heights.
#plot estimates (mean relationship, confidence interval on mean, prediction interval)
#calculate mean mu and 89% confidence interval on mu and add to plot
weight.seq<-seq(0,50, by = 1)
mu<-link(m4h2, data = data.frame(weight = weight.seq))
mu.mean<-apply(mu, 2, mean)
mu.PI<-apply(mu, 2, PI, 0.89)
lines(weight.seq, mu.mean)
shade(mu.PI, weight.seq)
#calculate prediction interval and add to plot
sim.heights<-sim(m4h2, data = list(weight = weight.seq))
sim.PI<-apply(sim.heights, 2, PI, 0.89)
shade(sim.PI, weight.seq)
# For every 10 units of increase in weight, how much taller does the model predict a child gets?
precis(m4h2)
# (c) What aspects of the model fit concern you? Describe the kinds of assumptions you would
# change, if any, to improve the model. You don't have to write any new code. Just explain what the
# model appears to be doing a bad job of, and what you hypothesize would be a better model.
### ans:
# The model does a poor job of calculating the mean relationship particularly at low and high values
# of weight. The relationship does not appear to be well represented by a straight line. A curved line,
# using a polynomial model, or b-splines, would be a better fit to the data.

# 4H3. Suppose a colleague of yours, who works on allometry, glances at the practice problems just
# above. Your colleague exclaims, "That's silly. Everyone knows that it's only the logarithm of body
# weight that scales with height!" Let's take your colleague's advice and see what happens.
# (a) Model the relationship between height (cm) and the natural logarithm of weight (log-kg). Use
# the entire Howell1 data frame, all 544 rows, adults and non-adults. Fit this model, using quadratic
# approximation:
# hi ??? Normal(??i, ??)
# ??i = ?? + ?? log(wi)
# ?? ??? Normal(178, 20)
# ?? ??? Log ??? Normal(0, 1)
# ?? ??? Uniform(0, 50)
# where hi is the height of individual i and wi is the weight (in kg) of individual i. The function for
# computing a natural log in R is just log. Can you interpret the resulting estimates?
d<-Howell1
xbar<-mean(d$weight)
m4h3<-quap(
  alist(
    height ~ dnorm(mu, sigma),
    mu <- a + b*(log(weight)),
    a ~ dnorm(178, 20),
    b ~ dlnorm(0,1),
    sigma ~ dunif(0,50)
  ),
  data = d
)
precis(m4h3)
# interpretation is more difficult here as (1) I haven't centred weight and (2) I've transformed the 
# weight variable to log(weight), meaning that alpha is not the mean height in the dataset, but the
# mean height of someone whose log(weight) is 0 (i.e. whose weight is 1kg).
# (b) Begin with this plot:
plot( height ~ weight , data=Howell1 ,
      col=col.alpha(rangi2,0.4) )
# Then use samples from the quadratic approximate posterior of the model in (a) to superimpose on
# the plot: (1) the predicted mean height as a function of weight, (2) the 97% interval for the mean, and
# (3) the 97% interval for predicted heights.
weight.seq<-seq(1,65, by = 1)
#(1)
mu<-link(m4h3, data = data.frame(weight = weight.seq))
mu.mean<-apply(mu, 2, mean)
lines(weight.seq, mu.mean)
#(2)
mu.PI<-apply(mu, 2, PI, 0.97)
shade(mu.PI, weight.seq)
#(3)
sim.heights<-sim(m4h3, data = data.frame(weight = weight.seq))
sim.PI<-apply(sim.heights, 2, PI, 0.97)
shade(sim.PI, weight.seq)

# 4H4. Plot the prior predictive distribution for the polynomial regression model in the chapter. You
# can modify the code that plots the linear regression prior predictive distribution. Can you modify the
# prior distributions of ??, ??1, and ??2 so that the prior predictions stay within the biologically reasonable
# outcome space? That is to say: Do not try to fit the data by hand. But do try to keep the curves
# consistent with what you know about height and weight, before seeing these exact data.
#here's the model:
d$weight_s <- ( d$weight - mean(d$weight) )/sd(d$weight)
d$weight_s2 <- d$weight_s^2
m4.5 <- quap(
  alist(
    height ~ dnorm( mu , sigma ) ,
    mu <- a + b1*weight_s + b2*weight_s2 ,
    a ~ dnorm( 178 , 20 ) ,
    b1 ~ dlnorm( 0 , 1 ) ,
    b2 ~ dnorm( 0 , 1 ) ,
    sigma ~ dunif( 0 , 50 )
  ) ,
  data=d )
#prior predictive distributions:
N<-100
a.samp<-rnorm(N, 178, 20)
b1.samp<-rlnorm(N, 0, 1)
b2.samp<-rnorm(N, 0, 1)
#plot:
plot(height~weight, data = d, type = "n", ylim = c(55, 250))
for(i in 1:N){
  curve(a.samp[i]+b1.samp[i]*((x-mean(x))/sd(x)) + b2.samp[i]*((x-mean(x))/sd(x))^2,
  from = min(d$weight), to = max(d$weight), add = T)
}
#modify the prior distributions to keep the prior predictions in a reasonable outcome space:
a.samp<-rnorm(N, 160, 20) #this allows a range of likely values for mean adult height (120-200 cm)
b1.samp<-rlnorm(N, 0, 1) #
#b2.samp<-rlnorm(N, 0, 1)
b2.samp<-rnorm(N, 0, 1)
plot(height~weight, data = d, type = "n", ylim = c(55, 250))
for(i in 1:N){
  curve(a.samp[i]+b1.samp[i]*((x-mean(x))/sd(x)) + b2.samp[i]*((x-mean(x))/sd(x))^2,
        from = min(d$weight), to = max(d$weight), add = T)
}
#I think the idea behind this exercise is to show (1) that it is difficult to set realistic priors for
# polynomial parameters and (2) that we don't want to set a positive constraint on b2 (unlike b1) 
# because this tends to result in a u-shaped curve



####################################################################################################################################################

#Chapter 5 practice:

## Easy
# 5E2. Write down a multiple regression to evaluate the claim: 
# Animal diversity is linearly related to latitude, but only after controlling for plant diversity. 
# You just need to write down the model definition.
#ans
# Animal diversity = A
# Latitude = L
# Plant diversity = P
# A_mu = a + Bl*L + Bp*P

# 5E3. Write down a multiple regression to evaluate the claim: Neither amount of funding nor size
# of laboratory is by itself a good predictor of time to PhD degree; but together these variables are both
# positively associated with time to degree. Write down the model definition and indicate which side of
# zero each slope parameter should be on
#ans
# T_mu = a + Bf*F + Bl*L
# The more funding, the longer the time to submission, so Bf > 0
# The larger the lab, presumably the more experience the PI has, so more timely submission
# so Bl < 0
# (but the two parameters will be correlated as larger labs will have more funding?)
# test this answer:
f<-rnorm(100, 0, 1)
l<-rnorm(100, f, 1)
t<-rnorm(100, f-l, 1)
d<-data.frame(f, l, t)
m5E3<-quap(
  alist(t~dnorm(mu, sigma),
        mu <- a + Bf*f + Bl*l,
        a ~ dnorm(0,0.1),
        Bf ~ dnorm(0,1),
        Bl ~ dnorm(0,1),
        sigma ~ dexp(1)
        ), data = d
)
precis(m5E3)

#Medium

# 5M1. Invent your own example of a spurious correlation. An outcome variable should be correlated
# with both predictor variables. But when both predictors are entered in the same model, the correlation
# between the outcome and one of the predictors should mostly vanish (or at least be greatly reduced)
p1<-rnorm(1e3)
p2<-rnorm(1e3, p1)
out<-rnorm(1e3, p1)
d<-data.frame(p1, p2, out)
m5M1a<-quap(
  alist(
    out~dnorm(mu, sigma),
    mu <- a + B1*p1,
    a ~ dnorm(0, 1),
    B1 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)
m5M1b<-quap(
  alist(
    out~dnorm(mu, sigma),
    mu <- a + B2*p2,
    a ~ dnorm(0, 1),
    B2 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)
m5M1c<-quap(
  alist(
    out~dnorm(mu, sigma),
    mu <- a + B1*p1 + B2*p2,
    a ~ dnorm(0, 1),
    B1 ~ dnorm(0, 1),
    B2 ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d
)
pairs(d)
coeftab_plot(coeftab(m5M1a, m5M1b, m5M1c), pars = c("B1", "B2"))

# 5M2. Invent your own example of a masked relationship. An outcome variable should be correlated
# with both predictor variables, but in opposite directions. And the two predictor variables should be
# correlated with one another
p1<-rnorm(1e3)
p2<-rnorm(1e3, p1)
out<-rnorm(1e3, p1-p2)
d<-data.frame(p1, p2, out)
m5M2a<-quap(
  alist(
    out~dnorm(mu, sigma),
    mu <- a + B1*p1,
    a ~ dnorm(0, 1),
    B1 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)
m5M2b<-quap(
  alist(
    out~dnorm(mu, sigma),
    mu <- a + B2*p2,
    a ~ dnorm(0, 1),
    B2 ~ dnorm(0, 1),
    sigma ~ dexp(1)
  ), data = d
)
m5M2c<-quap(
  alist(
    out~dnorm(mu, sigma),
    mu <- a + B1*p1 + B2*p2,
    a ~ dnorm(0, 1),
    B1 ~ dnorm(0, 1),
    B2 ~ dnorm(0,1),
    sigma ~ dexp(1)
  ), data = d
)
pairs(d)
coeftab_plot(coeftab(m5M2a, m5M2b, m5M2c), pars = c("B1", "B2"))

# 5M4. In the divorce data, States with high numbers of Mormons (members of The Church of Jesus
# Christ of Latter-day Saints, LDS) have much lower divorce rates than the regression models expected.
# Find a list of LDS population by State and use those numbers as a predictor variable, 
# predicting divorce rate using marriage rate, median age at marriage, and percent LDS population 
# (possibly standardized). You may want to consider transformations of the raw percent LDS variable
#ans
# First, use rvest to webscrape the data
library(rvest)
website <- read_html("https://www.worldatlas.com/articles/mormon-population-by-state.html")
tab <- website %>%
  html_nodes(css = "table") %>%
  .[[1]] %>%
  html_table()
str(tab)
#tidy data
names(tab)[2]<-"State"
tab<-tab %>% mutate(mormon.pc = as.numeric(substr(`Percentage of Mormon Residents`, 
                                                  1,nchar(`Percentage of Mormon Residents`)-1))) %>%
  dplyr::select(State, mormon.pc)
#read in divorce data and add mormon data to it
data("WaffleDivorce")
d<-WaffleDivorce
d<-d %>% left_join(tab, by = c("Location" = "State"))
#mormon.pc has a very skewed distribution - try using log(mormon.pc) before standardising
simplehist(d$mormon.pc)
#standardise variables:
d<-d %>% mutate(A = scale(MedianAgeMarriage), M = scale(Marriage), D = scale(Divorce), 
                lds = scale(log(mormon.pc)))
#model: predict divorce rate using mariage rate, median age at mariage and LDS proportion
m5M4 <- quap(
  alist(
    D ~ dnorm(mu, sigma),
    mu <- a + bM*M + bA*A + bLDS*lds,
    a ~ dnorm(0, 0.1),
    bM ~ dnorm(0, 0.5),
    bA ~ dnorm(0, 0.5),
    bLDS ~ dnorm(0, 0.5),
    sigma ~ dexp(1)
  ), data = d
)
precis(m5M4)
#to complete the analysis, let's plot predictions v observations, and counterfactuals
#predictions v observations:
mu<-link(m5M4)
mu_mean<-apply(mu, 2, mean)
mu_PI<-apply(mu, 2, PI)
plot(mu_mean ~ d$D, col = rangi2, xlab = "observed divorce rate", ylab = "predicted divorce rate")
abline(a = 0, b = 1, lty = 2)
for(i in 1:nrow(d)) lines(rep(d$D[i],2), mu_PI[,i], 
                          col = rangi2)
identify(x = d$D, y = mu_mean, labels = d$Loc)
# Idaho is still predicted as having a higher divorce rate than observed.
# also not great predictions for states with very high divorce rates
#counterfactuals:
#effect of age while holding lds and marriage rate constant (at mean)
aseq <- seq( from=min(d$A)-0.15 , to=max(d$A)+0.15 , length.out=nrow(d) )
mu <- link( m5M4 , data=data.frame( A=aseq , M=0, lds=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(d$A) , ylim=range(d$D), main = "counterfactual plot for the effect of age",
      xlab = "(std) divorce rate", ylab = "(std) median age at marriage")
lines( aseq , mu_mean , lwd=2 )
shade( mu_PI , aseq )
sim.div<-sim(m5M4, data = data.frame( A=aseq , M=0, lds=0 ))
sim.PI<-apply(sim.div, 2, PI, 0.97)
shade(sim.PI, aseq)
#effect of marriage rate while holding lds and age constant (at mean)
mseq <- seq( from=min(d$M)-0.15 , to=max(d$M)+0.15 , length.out=nrow(d) )
mu <- link( m5M4 , data=data.frame( A=0 , M=mseq, lds=0 ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(d$M) , ylim=range(d$D), main = "counterfactual plot for the effect of marriage rate",
      xlab = "(std) divorce rate", ylab = "(std) marriage rate")
lines( mseq , mu_mean , lwd=2 )
shade( mu_PI , mseq )
sim.div<-sim(m5M4, data = data.frame( A=0 , M=mseq, lds=0 ))
sim.PI<-apply(sim.div, 2, PI, 0.97)
shade(sim.PI, mseq)
#effect of lds while holding marriage rate and age constant (at mean)
lseq <- seq( from=min(d$lds)-0.15 , to=max(d$lds)+0.15 , length.out=nrow(d) )
mu <- link( m5M4 , data=data.frame( A=0 , M=0, lds=lseq ) )
mu_mean <- apply(mu,2,mean)
mu_PI <- apply(mu,2,PI)
plot( NULL , xlim=range(d$lds) , ylim=range(d$D), main = "counterfactual plot for the effect of LDS proportion",
      xlab = "(std) divorce rate", ylab = "(std) proportion of LDS in population")
lines( lseq , mu_mean , lwd=2 )
shade( mu_PI , lseq )
sim.div<-sim(m5M4, data = data.frame( A=0 , M=0, lds=lseq ))
sim.PI<-apply(sim.div, 2, PI, 0.97)
shade(sim.PI, lseq)

# 5M5. One way to reason through multiple causation hypotheses is to imagine detailed mechanisms
# through which predictor variables may influence outcomes. For example, it is sometimes argued 
# that the price of gasoline (predictor variable) is positively associated with lower obesity rates
# (outcome variable). However, there are at least two important mechanisms by which the price of 
# gas could reduce obesity. First, it could lead to less driving and therefore more exercise. 
# Second, it could lead to less driving, which leads to less eating out, which leads to less 
# consumption of huge restaurant meals. Can you outline one or more multiple regressions that 
# address these two mechanisms? Assume you can have any predictor data you need.
#ans
# mechanism 1: gasprice -> driving rate -> exercise -> obesity
# mechanism 2: gasprice -> driving rate -> restaurant use -> obesity
# is exercise rate or restaurant use a better predictor?
# a model could include mean gasprice per region and both number of restaruants per capita
# and some measure of walking/exercising (from a survey?) as predictors