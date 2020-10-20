# Grab the required packages. Install first if needed.
list_of_packages <- c("ggplot2", "conflicted")

# Extract not installed packages
not_installed <- list_of_packages[!(list_of_packages %in% installed.packages()[, "Package"])]
if (length(not_installed)) install.packages(not_installed)

# Create made-up data.
data.matrix <- matrix(nrow = 100, ncol = 10)

# Let wt represent wild type (i.e. actual, everyday samples).
# Let ko represent knock-out samples (i.e. samples with knocked out genes).
colnames(data.matrix) <- c(
  paste("wt", 1:5, sep = ""),
  paste("ko", 1:5, sep = "")
)

# Name samples gene1, gene2 etc.
rownames(data.matrix) <- paste("gene", 1:100, sep = "")

# Give fake read counts to the genes.
# Poisson ditribution instead of
for (i in 1:100) {
  # Since we're using made-up data, we can use the Poisson Distribution
  # instead of the negative Binomial Distribution for simplicity.
  wt.values <- rpois(5, lambda = sample(x = 10:1000, size = 1))
  ko.values <- rpois(5, lambda = sample(x = 10:1000, size = 1))

  data.matrix[i, ] <- c(wt.values, ko.values)
}

# Samples are columns, genes are rows. Transpose to make data compatible with prcomp().
# head(data.matrix)
dim(data.matrix)
data.matrix <- t(data.matrix)
# head(data.matrix)
dim(data.matrix)

# Now we can test run PCA on our data.
pca <- prcomp(data.matrix, scale = TRUE)

# Test plot using the two first principal components (out of 10 total).
# The 1st account for the most variation, 2nd PC for second most etc.
plot(pca$x[, 1], pca$x[, 2])

# From the plot we could easily identify two clusters, each on opposite sides of the 1st PC's axis (x-axis).
# print the variation of PC 1:
pca.var <- (pca$sdev)^2

# Calculate percentage of variation for current PC.
pca.var.per <- round(pca.var / sum(pca.var) * 100, 1)

# From the plot, we saw that PC1 accounts for almost all variation in the data:
barplot(pca.var.per, main = "Scree Plot", xlab = "Principal Component", ylab = "Percent Variation")

# If we want to investigate X/Y-coordinates for PCs 1 and 2, put them in dataframe.
pca.data <- data.frame(
  Sample = rownames(pca$x),
  X = pca$x[, 1],
  Y = pca$x[, 2]
)
pca.data

# Plot PCs 1 and 2 using ggplot this time. Note variations on axes.
ggplot(data = pca.data, aes(x = X, y = Y, label = Sample)) +
  geom_text() +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep = "")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep = "")) +
  theme_bw() + # Make background white.
  ggtitle("My PCA")

# Check loading scores to determine which genes have the largest effect on where samples are plotted.
# Focus on PC1 since it has the by far highest variation.
loading_scores <- pca$rotation[, 1]

# Genes pushing samples to the left have large negative values and vice versa.
gene_scores <- abs(loading_scores)

gene_score_ranked <- sort(gene_scores, decreasing = TRUE)
top_10_genes <- names(gene_score_ranked[1:10])
top_10_genes

# If we want to see the signs:
pca$rotation[top_10_genes, 1]
