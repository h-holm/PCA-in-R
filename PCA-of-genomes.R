# Grab the required packages. Install first if needed.
listOfPackages <- c("ggplot2")
for (i in listOfPackages){
  if(! i %in% installed.packages()){
    install.packages(i, dependencies = TRUE)
  }
  require(i)
}

# Create made-up data.
data.matrix <- matrix(nrow=100, ncol=10)

# Let wt represent wild type (i.e. actual, everyday samples).
# Let ko represent knock-out samples (i.e. samples with knocked out genes).
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep=""),
  paste("ko", 1:5, sep=""))

# Name samples gene1, gene2 etc.
rownames(data.matrix) <- paste("gene", 1:100, sep="")

# Give fake read counts to the genes.
# Poisson ditribution instead of
for (i in 1:100) {
  # Since we're using made-up data, we can use the Poisson Distribution
  # instead of the negative Binomial Distribution for simplicity.
  wt.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  ko.values <- rpois(5, lambda=sample(x=10:1000, size=1))
  
  data.matrix[i,] <- c(wt.values, ko.values)
}

head(data.matrix)
dim(data.matrix)
