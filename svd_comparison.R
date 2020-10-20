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

# Compare PCA results with results from using Singular Value Decomposition with svd()
# svd() returns v - the rotation (matrix of eigenvectors / loading scores)
#               u - sum(rotation * original data) but compressed to unit vector
#               d - similar to sdev of prcomp() but not scaled by sample size in an unbiazed way (i.e. 1/(n-1)).
#               prcomp() uses sdev = sqrt(var) = sqrt(ss(fit)/(n-1)), where ss = Sum of Squared Distances
#               svd()    uses d    = sqrt(ss(fit))

svd_decomp <- svd(scale(data_matrix, center = TRUE))

# Calculate the PCs.
svd_df <- data.frame(
  Sample = colnames(data_matrix),
  X = (svd_decomp$u[, 1] * svd_decomp$d[1]),
  Y = (svd_decomp$u[, 2] * svd_decomp$d[2])
)
head(svd_df)

# Alternatively, compute the PCs with the eigenvectors and original data.
dim(svd_decomp$v)
dim(data_matrix)

svd_PCs <- svd_decomp$v %*% scale(data_matrix, center = TRUE)

# Investigate first two principal components.
# svd_PCs[, 1:2]

svd_ds_of_f <- ncol(data_matrix) - 1
svd_var <- (svd_decomp$d^2) / svd_ds_of_f
svd_var_prc <- round(svd_var / sum(svd_var) * 100, 1)

ggplot2::ggplot(data = svd_df, ggplot2::aes(x = X, y = Y, label = Sample)) +
  ggplot2::geom_text() +
  ggplot2::xlab(paste("PC1 - ", svd_var_prc[1], "%", sep = "")) +
  ggplot2::ylab(paste("PC2 - ", svd_var_prc[2], "%", sep = "")) +
  ggplot2::theme_bw() +
  ggplot2::ggtitle("svd(scale(data_matrix, center=TRUE)")
