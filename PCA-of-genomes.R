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

# Samples are columns, genes are rows. Transpose to make data compatible with prcomp().
# head(data.matrix)
dim(data.matrix)
data.matrix <- t(data.matrix)
# head(data.matrix)
dim(data.matrix)

# Now we can test run PCA on our data.
pca <- prcomp(data.matrix, scale=TRUE)

# Test plot using the two first principal components (out of 10 total).
# The 1st account for the most variation, 2nd PC for second most etc.
plot(pca$x[,1], pca$x[,2])

# From the plot we could easily identify two clusters, each on opposite sides of the 1st PC's axis (x-axis).
# print the variation of PC 1:
pca.var <- (pca$sdev)^2

# Calculate percentage of variation for current PC.
pca.var.per <- round(pca.var/sum(pca.var)*100, 1)


