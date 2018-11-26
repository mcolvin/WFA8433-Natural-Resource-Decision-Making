## ----echo=FALSE, out.width="65%"-----------------------------------------
include_graphics("media/banner-05.jpg")

## ----echo=FALSE, out.width="45%"-----------------------------------------
include_graphics("media/cl07-chuck-norris.png")

## ----echo=FALSE, out.width="45%"-----------------------------------------
include_graphics("media/Rgui.png") 

## ----echo=FALSE, out.width="45%"-----------------------------------------
include_graphics("media/figure1.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure2.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure3.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure4.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure5.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure6.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure7.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure8.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure9.png") 

## ------------------------------------------------------------------------
fish.wt = 10
fish.length = 150
fish.condition = fish.length/fish.wt
fish.condition
ls()

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure10.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/figure11.png") 

## ------------------------------------------------------------------------
# This program calculates the goofy Peterson and Colvin condition factor
fish.wt = 10
fish.length = 150
## here's the formula..., pure genius 
fish.condition = fish.length/fish.wt
fish.condition
ls()

## ------------------------------------------------------------------------
# This program calculates the goofy Peterson and Colvin condition factor
fish.wt = 10
fish.length = 150
## here's the formula..., pure genius 
fish.condition = fish.length/fish.wt
#fish.condition
ls()

## ------------------------------------------------------------------------
setwd("F:/lab1/RRRR") ## NOTICE USE OF FORWARD SLASH
# This program calculates the goofy Peterson and Colvin condition factor
fish.wt = 10
fish.length = 150
## here's the formula..., pure genius 
fish.condition = fish.length/fish.wt
fish.condition
ls()

## ------------------------------------------------------------------------
setwd("F:\\lab1\\RRRR")## NOTICE USE OF DOUBLE BACKSLASH
# This program calculates the goofy Peterson and Colvin condition factor
fish.wt = 10
fish.length = 150
## here's the formula..., pure genius 
fish.condition = fish.length/fish.wt
fish.condition
ls()


## ------------------------------------------------------------------------
setwd("F:\\lab1\\RRRR")
# This program calculates the goofy Peterson and Colvin condition factor
fish.wt = 10
fish.length = 150
## here's the formula..., pure genius 
fish.condition = fish.length/fish.wt
fish.condition
ls()
### Here's where we save everything
save.image(file = "firstRclass.Rdata")


## ------------------------------------------------------------------------
### Here's where we save just 2 objects
save(file = "firstRclass.Rdata", list = c("fish.condition","fish.wt"))

## ------------------------------------------------------------------------
load("firstRclass.Rdata")


## ------------------------------------------------------------------------
load("./firstRclass.Rdata")

## ------------------------------------------------------------------------
# Load objects from existing file, full path edition
load("F:\\lab1\\RRRR\\firstRclass.Rdata")


## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/rstudio1.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/rstudio2.png") 

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/rstudio3.png") 

## ------------------------------------------------------------------------
# using the = operator
Z= 10
# or using the assignment “<-“ operator
Z <- 10

## ------------------------------------------------------------------------
# create vector
a_vect <- c(1,2,3,4,5)
# print out vector
a_vect

## ------------------------------------------------------------------------
# divide all the values in the vector by 10
new_vect <- a_vect/10
# print out vector
new_vect

## ------------------------------------------------------------------------
# print out 4th element
new_vect[4]

## assign 4th element to another object, fourth
fourth = new_vect[4]


## ------------------------------------------------------------------------
# print elements 3, 4, and 5 in new.vect
new_vect[3:5]

## ------------------------------------------------------------------------
## create vector
biggy = c(10:20)
#print out vector
biggy

## ------------------------------------------------------------------------
# create a vector with a sequence of values from 1.25 to 8.75 by increments of 0.25
wow <- seq(1.25, 8.75, by = 0.25)

# create a vector with a sequence of 13 evenly spaced values between 9 and 14
double_wow <- seq(9, 14, length = 13)

# create a vector of 13 elements with the same value 41
double_dog_wow <- rep(41,13)

# create a vector consisting of two sequences of 1,2,3,4 
triple_dog_wow <- rep(1:4, 2)


## ------------------------------------------------------------------------
# create a vector with a sequence of values from 1 to 10 by increments of 0.5
nuts <- seq(1,10, by = 0.5)

# select the odd numbered elements of nuts and put them into a vector wing.nuts
wing_nuts = nuts[c(1,3,4,5,7,9,11,13,15,17,19)]

## ------------------------------------------------------------------------
# select the odd numbered elements of nuts using seq and put them into a vector wing.nuts
wing_nuts = nuts[seq(1,19,2)]

## ------------------------------------------------------------------------
### create the vector the hard way
vect = c(1,2,3,4,5,6,7,8,9,10,11,12)

### create the vector the easy way
vect = c(1:12) 


## ------------------------------------------------------------------------
### create the vector the easy way
vect <- c(1:12) 
## create the 4 by 3 matrix using the values in vect
the_matrix = matrix(vect, nrow = 4)
## create the 4 by 3 matrix using the values in vect
the_matrix = matrix(vect, ncol = 3)

## ------------------------------------------------------------------------
## create the 4 by 3 matrix using the values in vect
the_matrix = matrix(vect, ncol = 3, byrow = TRUE)
## print out the matrix
the_matrix

## ------------------------------------------------------------------------
### print out value in row 2, column 3 in jims.matrix
the_matrix[2,3]

## print out the values in rows 2 and 3 in the first column
the_matrix[2:3,1]

## print out the values in rows 1 and 3 in the second column
the_matrix[c(1,3),2]

## print out all the rows for columns 1 and 3, notice blank for row
the_matrix[,c(1,3)]
## print out all the columns for row 2, notice blank for column
the_matrix[2,]

## ------------------------------------------------------------------------
## change the value in row 2, column 1 to -99
the_matrix[2,1] <- -99
## print out matrix
the_matrix

## change the values in rows 1 and 4, column 3 to missing, remember NA is missing
the_matrix[c(1,4),3] <- NA
## print out matrix
the_matrix

## change the values in row 4, column 2 to the sum of values in row 3, columns 1 and 2
the_matrix[4,2] <- the_matrix[3,1] + the_matrix[3,2]
## print out matrix
the_matrix

## ------------------------------------------------------------------------
## create species assign "dog"
species<- 'dog'
#print out 
species

## ------------------------------------------------------------------------
## create species.vect and assign pet names
species_vect <- c("dog","cat","hamster")
#print out 
species_vect

## ------------------------------------------------------------------------
# create vector alphabet the hard way
alphabet <- c("a","b","c","d","e","f","g")
#print it out
alphabet

# create vector alphabet the easy way
alphabet <- letters[1:10]
#print it out
alphabet



## ------------------------------------------------------------------------
## create pet.vect and assign pet names
pet_vect = c("dog","cat","hamster","goldfish","mouse","bird")
## create a matrix
pet_matrix = matrix(pet_vect, ncol = 3, byrow = TRUE)
## print it out
pet_matrix



## ------------------------------------------------------------------------
## print out the values in row 1 column 1 and 3 
pet_matrix[1,c(1,3)]

## print out all the rows for columns 1
pet_matrix[,1]

## ------------------------------------------------------------------------
# the hard way
trial<- c("a","b","c","d",1,2,3,4,5,6,7,8)
#print it out
trial

#easier way
trial<- c(letters[1:4],1:8)
#print it out
trial

## ------------------------------------------------------------------------
## create mixed up matrix 
trial_n_error <-matrix(trial,ncol = 3, byrow = FALSE)
# print it out
trial_n_error

## ----eval=FALSE----------------------------------------------------------
## ## add columns 2 and 3 on trial.n.error matrix
## trial_n_error[,2] + trial_n_error[,3]
## 

## ------------------------------------------------------------------------
## is trial.n.error a numeric matrix
is.numeric(trial_n_error)

## is trial.n.error a character matrix
is.character(trial_n_error)

## ------------------------------------------------------------------------
# create new object
fix<- trial_n_error[,2:3]

# what is this type of object
class(fix)

# is fix a matrix, notice another is function-- of course!
is.matrix(fix)

## what are the type of variables in fix
typeof(fix)

## what are the attributes of fix, yes another function 
attributes(fix)

## ------------------------------------------------------------------------
# coerce fix to become numeric
fixed <- as.numeric(fix)
#print it out
fixed

# what is this type of object-- numeric
class(fixed)

# is fixed a matrix, -- oh no!
is.matrix(fixed)

## what are the type of variables in fixed-- yes numeric!
is.numeric(fixed)


## ------------------------------------------------------------------------
fixed <- as.numeric(fix)
#print it out
fixed <- matrix(fixed,ncol = 2, byrow = FALSE)
fixed

# what is this type of object-- matrix
class(fixed)

## ------------------------------------------------------------------------
#add column 1 and 2 of fixed
fixed[,1] + fixed[,2]

#create a third column in fixed by adding column 1 and 2 of fixed
new_val <- fixed[,1] + fixed[,2]

#print it out
new_val

## ------------------------------------------------------------------------
# create vector curious by coercing trial.n.error
curious<-as.numeric(trial_n_error)
## print it out
curious

## ------------------------------------------------------------------------
## are there missing values in curious
is.na(curious)

## ------------------------------------------------------------------------
#replace missing elements in curious with zero
curious[is.na(curious)] <- 0
#print it out
curious


## ------------------------------------------------------------------------
#replace zero elements in curious with -999
curious[curious== 0] <- -999
#print it out
curious


## ------------------------------------------------------------------------
## create numeric vector num.vect with values 1:15
num_vect = c(1:15)
## print it out
num_vect

# create character vector char.vect by coercing num.vect 
char_vect= as.character(num_vect)
## print it out
char_vect

## ------------------------------------------------------------------------
## print out matrix as a reminder
trial_n_error
#create my.dater data frame by coercing trial.n.error
my_dater<- as.data.frame(trial_n_error)
# print it out
my_dater

#what classs of object
class(my_dater)

# is my.dater a data frame-- yes!
is.data.frame(my_dater)

# is my.dater a matrix -- no!
is.matrix(my_dater)



## ------------------------------------------------------------------------
#change column names in my.dater to pixie, dixie, and bud
colnames(my_dater) = c("pixie", "dixie", "bud")
#print it out
my_dater

## ------------------------------------------------------------------------
#print out the second column dixie in my.data
my_dater$dixie

#we can also refer to individual columns in a data frame using brackets
#print out the second column dixie in my.data
my_dater[,2]

## ------------------------------------------------------------------------
#try to add dixie and bud columns in my.dater
my_dater$dixie + my_dater$bud


## ------------------------------------------------------------------------
# whatclass is dixie
class(my_dater$dixie)

#is dixie numeric- no!
is.numeric(my_dater$dixie)

#is dixie a factor- yes!
is.factor(my_dater$dixie)


## ------------------------------------------------------------------------
# first print out bud
my_dater$bud

# try to coerce bud to a numeric value
as.numeric(my_dater$bud)


## ------------------------------------------------------------------------
# print out bud
my_dater$bud

# now coerce as character then to coerce bud to a numeric value
as.numeric(as.character(my_dater$bud))

## ------------------------------------------------------------------------
# now coerce as character then to coerce bud to a numeric value
my_dater$bud <- as.numeric(as.character(my_dater$bud))

# whats the class-- numeric!
class(my_dater$bud)

# is it numeric--yes!
is.numeric(my_dater$bud)

# now coerce as character then to coerce dixie to a numeric value
my_dater$dixie <- as.numeric(as.character(my_dater$dixie))

# whats the class--numeric!
class(my_dater$dixie)

# is it numeric--yes!
is.numeric(my_dater$dixie)

## ------------------------------------------------------------------------
# create a numeric vector with a sequence of values from 1.25 to 8.75 by increments of 0.25 and print
num_vct <- seq(1.25, 8.75, by = 0.25)
num_vct

# create 3 by 4 numeric matrix with a sequence of values from 1 to 6.5 by 0.5 and print
num_mtrx <- matrix(seq(1, 6.5, by = 0.5), ncol = 4, byrow = FALSE)
num_mtrx

# create a character vector with a through z and print
char_vct <- letters[1:10]
char_vct

# create 2 by 2 numeric with peoples names and print
char_mtrx <- matrix(c("bill", "mary","joe","brenda"), ncol = 2, byrow = FALSE)
char_mtrx

## create a list that contains all of these objectives 
big_list <- list(num_vct,char_mtrx,num_mtrx,char_vct)

## create a name for each object within the big.list and print
names(big_list) <- c("vect_numbrs", "names", "numb_matrx","letters")
big_list


## ------------------------------------------------------------------------
## what class is this object
class(big_list)

# what type of object
typeof(big_list)




## ------------------------------------------------------------------------
## new very important function for lists
str(big_list)

## ------------------------------------------------------------------------
# to access the ‘names’ object in big.list
big_list$names

# to access the names object the 2nd one in big.list
big_list[2]


## ------------------------------------------------------------------------
# what is the class of object created using $ in big.list
class(big_list$names)

# what is the class of object created using [] syntax in big.list
class(big_list[2])

## ------------------------------------------------------------------------
# First create a 4 row by 5 column matrix
MTX <- matrix(c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4), ncol = 5, byrow = T)

# Print it out
MTX 

## ------------------------------------------------------------------------
t(MTX)

## ------------------------------------------------------------------------
A <- 0.5

# multiply the matrix by a scalar
MTX*A

## ------------------------------------------------------------------------
MTX*10

## ------------------------------------------------------------------------
V = c(10,1,0.1,0.01)
MTX*V

## ------------------------------------------------------------------------
IDENT = matrix(c(1,0,0,0,1,0,0,0,1), ncol= 3)
IDENT

## ------------------------------------------------------------------------
V.new = c(1,2,3)
IDENT %*% V.new

## ------------------------------------------------------------------------
IDENT * V.new

## ------------------------------------------------------------------------
c = matrix(c(2,3,4,5,6,7), ncol = 2)
z = c(10,15)
c %*% z

## ------------------------------------------------------------------------
## check with a little R code
c * z

## ----echo=FALSE, out.width="95%"-----------------------------------------
include_graphics("media/cl07-Picture1.png")

## ------------------------------------------------------------------------
# per capita fecundity juvenile
Fj = 1.1
# per capita fecundity adult
Fa = 2.1

#survival age 0
S0 = 0.25
#survival juvenile
Sj = 0.35
#survival adult
Sa = 0.75

## ------------------------------------------------------------------------
trans_mtrx = matrix(c(0,Fj,Fa,S0,0,0,0,Sj,Sa), 
    ncol = 3, byrow = T)
#print it out
trans_mtrx

## ------------------------------------------------------------------------
#Number of animals in each age class
N0 = 100
Nj = 50
Na = 200

Nt = c(N0,Nj,Na)

## ------------------------------------------------------------------------
trans_mtrx %*% Nt

## ------------------------------------------------------------------------
# number of years to simulate
nyears<- 20
# Create a matrix to hold the output
output<- matrix(0, nrow=3, ncol=nyears)
# set the initial population
output[,1]<- Nt
for(year in 2:nyears)
  {
  output[,year]<- trans_mtrx %*% output[,year-1]# forecast from previous year  
  }
output

## ------------------------------------------------------------------------
#eigen analysis of population transition matrix 
eigen(trans_mtrx)$values[1]

## stable age distribution
eigen(trans_mtrx)$vectors[,1]/sum(eigen(trans_mtrx)$vectors[,1])

