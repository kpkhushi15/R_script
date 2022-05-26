## Some tricky operations on vectors and data frames:

# (A)  Vectors
#------------------
### Create vectors of numbers and strings

numvec = c(22,35,12,56,75,47,67,54,67,47,89,NA, NA,122,34,56,47,34,65,45,89,47)

strvec = c("ATG", "TAA","TGA","TTG", "TAC","GCA", "ATG", "ACC", "ATG", "BCC")


## take a subset based on condition on individual numbers
as = subset(numvec, (numvec > 40) & (numvec < 68) )
print(as)

## count the number of "ATG" in the vector
print( length( subset(strvec, strvec == "ATG") ) )

## locate the position(s) of "ATG"string in the vector -- "which" command
vlocation = which(strvec == "ATG")  ## vlocation is a vector with positions of
                                    ## "ATG" string elements in strvec
print(vlocation)

## locate the position(s) of a number in a vector.  'NA' are handled.
nloc = which(numvec==47)
print(nloc)

## Create a vector that repeats a vector
ssv = c(10,20,30,40,50)
ssvrep = rep(ssv,4)  ## repeats ssv 4 times to create a long vevtor
print(ssvrep)

## split a string at a location of charater
seq1 = "ATGTGAGATACAGATATATAGTACAGAAAGTACAATAGAATATATAAGTAGACAGATAG"
## split above string at "TATATA". We get a list of pieces, since 
## there are two "TATATA" in the string
slis = strsplit(seq1,"TATATA")
print(slis)
print(slis[[1]][1])
print(slis[[1]][2])
print(slis[[1]][3])

## split a string into a vector of characters
seq = "ATGCGTACGGGATACAGATAGATGACAG"
sv = strsplit(seq, "")[[1]]  ## sv is now a string of characters in svec
                             ## It is split at "", which means split every 
                             ##    character
print(sv)

##------------------------------------------------------------------------
# (B)  Built in constants in R:  (all return vectors)

## There is an internal vector of english alphabets as characters. 
## it is called "letters"
print(letters)          ## useful to name 21 aminoacods with single letters
                        ##  if you want!!

print(LETTERS)  ## capital letters


## month names abbreviated to 3 lettrs
print(month.abb)

## month names full
print(month.name)

## value of constant pi
print(pi)   ## defalt upto 6 digits fraction
print(pi, digits=22)  ## 12 digits in fraction!

##----------------------------------------------------------------------


## (C) Important operations on data frames:


## we create a data frame with repeat row names

ns = c("gene1","gene3", "gene2","gene4","gene5")

names = c( rep(ns, 5), rep("gene1",4), rep("gene5", 6))

## create vectors which atr Gaussian deviates, rounded to a digit.
sample1 = rnorm(length(names), mean=112, sd=10)
sample1 = round(sample1, digits=1)
sample2 = rnorm(length(names), mean=130, sd=12)
sample2 = round(sample2, digits=1)
sample3 = rnorm(length(names), mean=90, sd=7)
sample3 = round(sample3, digits=1)
sample4 = rnorm(length(names), mean=139, sd=14)
sample4 = round(sample4, digits=1)
sample5 = rnorm(length(names), mean=100, sd=9)
sample5 = round(sample5, digits=1)

## put them into data frame
data = data.frame(names, sample1, sample2, sample3, sample4, sample5)


### create a subset of gene1
data_gene1 = subset(data, data$names == "gene1")
print(data_gene1)

## create a subset with gene1 and gene4
data_gene14 = subset(data, (data$names=="gene1") | (data$names=="gene4"))
print(data_gene14)


## select specicif rows as a frame
rowf = data[c(2,5,8,14), ] ## select rows 2,5,8 and 14 into a frame "rowf"
print(rowf)

## In data, gene1, gene2 rows are jubled. Can we put it in order?
## we can use "order" function. It orders a vector elements and returns the
## number of ordered elements in the original velctor. Give this as row numbers

data_ordered = data[ order(data$names), ]
print(data_ordered)

### sum the sample1 value for gene1 using aggregate command
df1 = aggregate(data$sample1, by=list(Category=data$names), FUN=sum)
print(df1)


### Aggregation operations.
#-----------------------------
### Aggregated sum of multiple columns -- columns 2 to 6 
df2 = aggregate(data[, 2:6], by=list(Category=data$names), FUN=sum)
print(df2)

### aggregate to find mean for multiple columns
df3 = aggregate(data[, 2:6], by=list(Category=data$names), FUN=sum)
print(df3)

## aggregate to apply a user defined function on aggregates.
## example : along each column, take a square root of expressions for a gene1
##              and sum. do this for all genes separately.
myfunc = function(x){mean(sqrt(x))}
df4 = aggregate(data[, 2:6], by=list(Category=data$names), FUN=myfunc)
print(df4)




### To apply an operation row wise or column wise on a matrix
## MARGIN=1 is row wise, MARGIN=2 is columnwise

## perform a row wise sum and return the row wise sum vector
vec = apply(as.matrix(data[,2:6]), MARGIN=1, sum)
print(vec)

## apply sum columwise
vec = apply(as.matrix(data[,2:6]), MARGIN=2, sum)
print(vec)

#apply a function along each row

## given a vector x, this function squares each element, sums them and takes a square root. 
fun = function(x){sqrt(sum(x^2))}

## apply the above function to each row of a matrix. 
## data should be converted to matrix.
## Each rowof matrix is a vector. It is given to function and reault is computed. Returns a vector.
vec = apply(as.matrix(data[,2:6]), MARGIN=1, FUN=fun)
print(vec)















