---
title: "Primer on occupancy analysis"
author: Michael E. Colvin
date: 29 June 2016
output:
  html_document:
    theme: united
    highlight: tango
---

# The process

Suppose you are out in the field at a specific location (note-occupancy makes inference
about the site being occupied or not) and you repeated sample that site for a critter.
It is likely you do not perfectly detect that critter if it is there, but you can 
detect it with some probability.  Lets assume you can perfectly detect the critter then the
detection history would be 1111 if you went out on 4 occasions.  If you have imperfect detection,
let's define detection probability as $p$, then there is some probability you might miss 
detecting the critter even if it is there.  Now that brings up a the foundation of occupancy
analysis, you can miss a critter for 2 reasons:  1) that critter was not there to begin
with (i.e., was not occupying the site) or 2) the critter was there (i.e., was occupying the site) 
but you did not detect it.  These 2 sources of 0s in a detection history can either be a true 
negative (reason 1 above) or a false negative (reason 2 above).  

## Probability of a site being occupied

In this context, occupancy as we observe it, is a 0 or 1. However as we think about occupancy or 
try to model occupancy it is done as a probability.  Lets define the probability of a site being 
occupied as $\psi$.  Let's assume that we can detect our critter perfectly and $p = 1$.
Now, if we have a $\psi = 0.35$, the probability of site not being occupied is $1=\psi$ or 1-0.35 = 0.65.
Recall that occupancy is site or habitat specific and therefore the true site occupancy status (0 or 1)
and therfore occupancy for 10 sites might be 111000000 for 10 sites given a $\psi = 0.35$.  We can actually 
simulate this easily in r. Suppose there are potential 3000 sites to sample in a large tract of land (i.e.,
the sampling domain).  But we can only sample 35 of those sites due to budget limitations.  Lets simulate
this to see how things play out  


```r
psi<- 0.35 # set occupancy probability to 0.35
samp_domain<- 3000
site_status<- rbinom(samp_domain,1,psi)
table(site_status) # should be close to psi*3000 and (1-psi)*3000
```

```
## site_status
##    0    1 
## 1911 1089
```

```r
mean(site_status)# should be close to psi
```

```
## [1] 0.363
```
For clarity lets make the status into a data.frame and number each site.


```r
sites<- data.frame(id=c(1:samp_domain), occupied=site_status)
# lets look at the first 10 rows of the data.frame
head(sites,10)
```

```
##    id occupied
## 1   1        1
## 2   2        1
## 3   3        0
## 4   4        1
## 5   5        0
## 6   6        0
## 7   7        0
## 8   8        0
## 9   9        0
## 10 10        0
```

Now that we know what our sites are for occupancy status we can sample 35 of them
to estimate occupancy!  Remember we are assuming that $p=1$ for now!   


```r
# the sample function takes a random sample 
# of 35 sites without replacement
my_sample<- sample(sites$id, 35,replace=FALSE)
my_srs<- sites[which(sites$id %in% my_sample),]# get the samples 
# look at our sites
my_srs
```

```
##        id occupied
## 69     69        1
## 89     89        1
## 120   120        0
## 289   289        0
## 316   316        0
## 328   328        1
## 487   487        0
## 495   495        1
## 555   555        0
## 583   583        0
## 611   611        1
## 612   612        1
## 715   715        0
## 733   733        1
## 738   738        0
## 872   872        0
## 886   886        1
## 969   969        1
## 1088 1088        1
## 1214 1214        1
## 1291 1291        0
## 1375 1375        0
## 1433 1433        1
## 1458 1458        1
## 1503 1503        0
## 1508 1508        0
## 1824 1824        1
## 1984 1984        0
## 2028 2028        0
## 2151 2151        1
## 2168 2168        1
## 2169 2169        0
## 2363 2363        0
## 2536 2536        0
## 2875 2875        1
```
Now we can do just as we did before, but now we are estimating occupancy.


```r
table(my_srs$status) # should be close to psi*35 and (1-psi)*35
```

```
## < table of extent 0 >
```

```r
mean(my_srs$status)# should be close to psi
```

```
## Warning in mean.default(my_srs$status): argument is not numeric or logical:
## returning NA
```

```
## [1] NA
```
Recall that $p = 1$.  So if we went out 
