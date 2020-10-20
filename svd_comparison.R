# Grab the required packages. Install first if needed.
list_of_packages <- c("ggplot2", "conflicted")

# Extract not installed packages
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(not_installed)) install.packages(not_installed)

source("PCA-of-genomes.R")

# Compare PCA results with results from using Singular Value Decomposition with svd()
# svd() returns v - the rotation (matrix of eigenvectors / loading scores)
#               u - sum(rotation * original data) but compressed to unit vector
#               d - similar to sdev of prcomp() but not scaled by sample size in an unbiazed way (i.e. 1/(n-1)).
#               prcomp() uses sdev = sqrt(var) = sqrt(ss(fit)/(n-1)), where ss = Sum of Squared Distances
#               svd()    uses d    = sqrt(ss(fit))

svd_decomp <- svd(scale(data_matrix, center=TRUE))

# Calculate the PCs.
svd_df <- data.frame(Sample=colnames(data_matrix),
                       X=(svd_decomp$u[,1] * svd_decomp$d[1]),
                       Y=(svd_decomp$u[,2] * svd_decomp$d[2]))
head(svd_df)

