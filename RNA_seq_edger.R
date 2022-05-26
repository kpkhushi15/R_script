###### RNA_seq analysis 

#### load packages
library(edgeR)
library(limma)
library(Glimma)
library(gplots)
library(org.Mm.eg.db)
library(RColorBrewer)

###installation
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("Glimma")
source("https://bioconductor.org/biocLite.R")
biocLite("Glimma")
#source("https://bioconductor.org/biocLite.R")
biocLite("org.Mm.eg.db")

# Read the data into R
seqdata <- read.delim("GSE60450_Lactation-GenewiseCounts.txt", stringsAsFactors = FALSE)
# Read the sample information into R
sampleinfo <- read.delim("SampleInfo.txt")
head(seqdata)
sampleinfo
dim(seqdata)
colnames(seqdata)

#Format the data
## Remove first two columns from seqdata
countdata=seqdata[,-(1:2)]

## Store EntrezGeneID as rownames
rownames(countdata)=seqdata[,1]
head(countdata)

# using substr, you extract the characters starting at position 1 and stopping at position 7 of the colnames
colnames(countdata)=substr(colnames(countdata),1,7)
head(countdata)

##
table(colnames(countdata)==sampleinfo$SampleName)

##Filtering to remove lowly expressed genes
# Obtain CPMs
mycpm=cpm(countdata)
mycpm

### Which values in myCPM are greater than 0.5?
thresh=mycpm>0.5
thresh

# Summary of how many TRUEs there are in each row
# There are 11433 genes that have TRUEs in all 12 samples.
table(rowSums(thresh))
rowSums(thresh)

# we would like to keep genes that have at least 2 TRUES in each row of thresh
keep <- rowSums(thresh) >= 2
keep
# Subset the rows of countdata to keep the more highly expressed genes
counts.keep <- countdata[keep,]
counts.keep
summary(keep)

## Let's have a look and see whether our threshold of 0.5 does indeed correspond to a count of about 10-15
# We will look at the first sample
mycpm
countdata
plot(mycpm[,1],countdata[,1])

## Let us limit the x and y-axis so we can actually look to see what is happening at the smaller counts
plot(mycpm[,1],countdata[,1],ylim=c(0,50),xlim=c(0,3))
# Add a vertical line at 0.5 CPM
abline(v=0.5)


#Challenge
#Plot the counts-per-million versus counts for the second sample.
#Add a vertical line at 0.5 and a horizontal line at 10.
#Add the lines again, colouring them blue

plot(mycpm[,2],countdata[,2],ylim=c(0,50),xlim=c(0,3))
abline(v=0.5,h=10,col="blue")

#create a DGEList object.object used by edgeR to store count data
y <- DGEList(counts.keep)
# have a look at y
y
colSums(counts.keep)
# See what slots are stored in y
names(y)

# Library size information is stored in the samples slot
y$samples


#Quality control
#Library sizes and distribution plots
y$samples$lib.size


#We can also plot the library sizes as a barplot to see whether there are any major discrepancies between the samples more easily.

colnames(y)
rownames(y)


# The names argument tells the barplot to use the sample names on the x-axis
# The las argument rotates the axis names
barplot(y$samples$lib.size,names=colnames(y),las=2)
title("Barplot of the library size")


# Get log2 counts per million
logcounts=cpm(y,log=TRUE)
# Check distributions of samples using boxplots
boxplot(logcounts,xlab="",ylab="Log2 counts per million",las=2)
# Let's add a blue horizontal line that corresponds to the median logCPM
abline(h=median(logcounts),col="blue")
title("Boxplots of logCPMs (unnormalised)")


# multidimensional caling plot
plotMDS(y)
