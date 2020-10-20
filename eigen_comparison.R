set.seed(1337)

# Grab the required packages. Install first if needed.
list_of_packages <- c("ggplot2", "conflicted")

# Extract not installed packages
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(not_installed)) install.packages(not_installed)

# Create made-up data.
data_matrix <- matrix(nrow = 100, ncol = 10)

# Let wt represent wild type (i.e. actual, everyday samples).
# Let ko represent knock-out samples (i.e. samples with knocked out genes).
colnames(data_matrix) <- c(
  paste("wt", 1:5, sep = ""),
  paste("ko", 1:5, sep = "")
)

# Name samples gene1, gene2 etc.
rownames(data_matrix) <- paste("gene", 1:100, sep = "")

# Give fake read counts to the genes.
# Poisson ditribution instead of
for (i in 1:100) {
  # Since we're using made-up data, we can use the Poisson Distribution
  # instead of the negative Binomial Distribution for simplicity.
  wt.values <- rpois(5, lambda = sample(x = 10:1000, size = 1))
  ko.values <- rpois(5, lambda = sample(x = 10:1000, size = 1))
  
  data_matrix[i, ] <- c(wt.values, ko.values)
}

# Samples are columns, genes are rows. Transpose to make data compatible with prcomp().
data_matrix <- t(data_matrix)

# Compare PCA results with results from using eigen()
# eigen() returns vectors - eigenvectors (vectors with loading scores in this case)
#                           pcs = sum(loading scores * values for sample)
#                 values  - eigenvalues
cov_matrix <- cov(scale(data_matrix, center = TRUE))
dim(cov_matrix)

# We saw that the covariance matrix is symmetric. Hence, we can tell eigen()
# to work only on the lower triangle by specifying symmetric = TRUE.
eigen_res <- eigen(cov_matrix, symmetric = TRUE)
dim(eigen_res$vectors)
head(eigen_res$vectors[, 1:2])