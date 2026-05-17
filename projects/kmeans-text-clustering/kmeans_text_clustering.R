setwd("~/Desktop/spring 26/Data Matters /Project 2")

# K-means text clustering of the sentiment dataset.
# The data file has no header: column 1 is sentiment, column 2 is the sentence.
# Normalize legacy carriage-return line endings before CSV parsing.
raw_text <- rawToChar(readBin("all-data.csv", what = "raw", n = file.info("all-data.csv")$size))
csv_text <- iconv(raw_text, from = "latin1", to = "UTF-8", sub = "")
csv_text <- gsub("\r\n?", "\n", csv_text)

data <- read.csv(
  text = csv_text,
  header = FALSE,
  col.names = c("sentiment", "text"),
  stringsAsFactors = FALSE,
  quote = "\""
)

data <- data[complete.cases(data), ]
data$sentiment <- factor(data$sentiment, levels = c("negative", "neutral", "positive"))
data$text <- as.character(data$text)

# Use the group's smaller working dataset size without changing the original CSV.
# This keeps the 65 documents roughly proportional to the original sentiment mix.
full_data_count <- nrow(data)
target_sample_size <- 65
set.seed(26)
sample_counts <- round(prop.table(table(data$sentiment)) * target_sample_size)
sample_counts[which.max(sample_counts)] <- sample_counts[which.max(sample_counts)] +
  (target_sample_size - sum(sample_counts))
sample_rows <- unlist(lapply(names(sample_counts), function(sentiment_label) {
  available_rows <- which(data$sentiment == sentiment_label)
  sample(available_rows, sample_counts[sentiment_label])
}))
data <- data[sort(sample_rows), ]
row.names(data) <- NULL

tokenize <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z ]", " ", x)
  words <- unlist(strsplit(x, "\\s+"))
  words[nchar(words) > 3]
}

stop_words <- c(
  "the", "and", "for", "that", "with", "from", "this", "was", "are", "its",
  "has", "have", "had", "will", "would", "said", "were", "but", "not", "his",
  "her", "their", "they", "you", "your", "our", "out", "into", "about", "than",
  "then", "also", "been", "more", "over", "under", "after", "before", "between",
  "during", "while", "which", "who", "what", "when", "where", "why", "how",
  "can", "could", "should", "may", "might", "per", "pct", "eur", "mln", "mn",
  "million", "billion", "company", "companies", "finnish", "finland",
  "according", "announced", "reported", "reports", "statement", "today",
  "yesterday", "monday", "tuesday", "wednesday", "thursday", "friday",
  "saturday", "sunday", "helsinki", "thomson", "financial", "newsroom",
  "keywords", "oyj", "plc", "corp", "corporation", "group", "ltd", "inc",
  "share", "shares", "stock", "exchange", "market", "markets", "business",
  "services", "operations", "sales", "profit", "operating", "result",
  "results", "revenue", "turnover", "period", "quarter", "year", "years",
  "month", "months", "total", "totaled", "totalled", "rose", "fell",
  "increased", "decreased", "growth", "decline", "compared", "corresponding",
  "expected", "expects", "including", "excluding", "first", "second", "third",
  "fourth", "value", "price", "prices", "percent", "number", "based", "unit",
  "other", "euro", "euros", "down", "release", "long", "term", "short",
  "same", "made", "make", "part", "well", "held", "used", "using", "still",
  "since", "however", "mainly", "approximately", "currently", "some"
)

tokens <- lapply(data$text, function(sentence) {
  setdiff(tokenize(sentence), stop_words)
})

term_counts <- sort(table(unlist(tokens)), decreasing = TRUE)
minimum_term_count <- ifelse(nrow(data) <= 100, 2, 5)
term_counts <- term_counts[term_counts >= minimum_term_count]
top_terms <- names(term_counts)[seq_len(min(300, length(term_counts)))]

doc_term <- matrix(
  0,
  nrow = nrow(data),
  ncol = length(top_terms),
  dimnames = list(NULL, top_terms)
)

for (i in seq_along(tokens)) {
  counts <- table(tokens[[i]])
  matches <- intersect(names(counts), top_terms)
  doc_term[i, matches] <- as.numeric(counts[matches])
}

# TF-IDF weights make the clustering about text features, not just word frequency.
document_frequency <- colSums(doc_term > 0)
idf <- log(nrow(doc_term) / (1 + document_frequency))
tf_idf <- sweep(doc_term, 2, idf, "*")

set.seed(26)
kmeans_result <- kmeans(tf_idf, centers = 3, nstart = 25)
data$cluster <- factor(kmeans_result$cluster)

cluster_sizes <- as.data.frame(table(data$cluster))
names(cluster_sizes) <- c("cluster", "documents")

sentiment_cluster_table <- table(data$sentiment, data$cluster)
sentiment_cluster_percent <- prop.table(sentiment_cluster_table, margin = 2) * 100

write.csv(cluster_sizes, "kmeans_cluster_sizes.csv", row.names = FALSE)
write.csv(as.data.frame(sentiment_cluster_table), "kmeans_sentiment_cluster_table.csv", row.names = FALSE)

cat("\nK-means cluster sizes (K = 3):\n")
cat("Working dataset:", nrow(data), "documents sampled from", full_data_count, "total documents.\n")
print(cluster_sizes)

cat("\nSentiment by K-means cluster:\n")
print(sentiment_cluster_table)

top_terms_by_cluster <- lapply(levels(data$cluster), function(cluster_id) {
  cluster_rows <- data$cluster == cluster_id
  scores <- colMeans(tf_idf[cluster_rows, , drop = FALSE])
  head(names(sort(scores, decreasing = TRUE)), 10)
})
names(top_terms_by_cluster) <- paste("Cluster", levels(data$cluster))

top_word_scores_by_cluster <- lapply(levels(data$cluster), function(cluster_id) {
  cluster_rows <- data$cluster == cluster_id
  scores <- colMeans(tf_idf[cluster_rows, , drop = FALSE])
  head(sort(scores, decreasing = TRUE), 5)
})
names(top_word_scores_by_cluster) <- paste("Cluster", levels(data$cluster))

cat("\nTop text features by cluster:\n")
print(top_terms_by_cluster)

slide_summary <- data.frame(
  cluster = paste("Cluster", levels(data$cluster)),
  documents = as.integer(cluster_sizes$documents),
  dominant_sentiment = apply(sentiment_cluster_table, 2, function(x) names(which.max(x))),
  top_words = sapply(top_terms_by_cluster, function(words) paste(head(words, 5), collapse = ", ")),
  row.names = NULL
)
write.csv(slide_summary, "kmeans_slide_summary.csv", row.names = FALSE)

top_words_table <- do.call(rbind, lapply(names(top_word_scores_by_cluster), function(cluster_name) {
  scores <- top_word_scores_by_cluster[[cluster_name]]
  data.frame(
    cluster = cluster_name,
    word = names(scores),
    importance = as.numeric(scores),
    row.names = NULL
  )
}))
write.csv(top_words_table, "kmeans_cluster_top_words.csv", row.names = FALSE)

plot_kmeans_sentiment_summary <- function() {
  sentiment_colors <- c(
    negative = "#0072B2",
    neutral = "#E69F00",
    positive = "#CC79A7"
  )
  cluster_labels <- paste0(
    "Cluster ", levels(data$cluster),
    "\n(n=", format(cluster_sizes$documents, big.mark = ","), ")"
  )

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mar = c(6.5, 6, 5.5, 9), xpd = NA, las = 1)

  bar_positions <- barplot(
    sentiment_cluster_percent,
    col = sentiment_colors[rownames(sentiment_cluster_percent)],
    names.arg = cluster_labels,
    ylim = c(0, 100),
    ylab = "Percent of documents in each cluster",
    main = "How Sentiment Appears Within K-means Text Clusters",
    border = "#333333",
    cex.names = 0.95,
    cex.axis = 0.95,
    cex.lab = 1.05,
    cex.main = 1.2
  )

  mtext(
    paste("K-means used TF-IDF word features with K = 3; working dataset n =", nrow(data)),
    side = 3,
    line = 0.6,
    cex = 0.9,
    col = "#555555"
  )

  for (cluster_index in seq_len(ncol(sentiment_cluster_percent))) {
    cumulative <- 0
    for (sentiment_index in seq_len(nrow(sentiment_cluster_percent))) {
      value <- sentiment_cluster_percent[sentiment_index, cluster_index]
      if (value >= 8) {
        text(
          x = bar_positions[cluster_index],
          y = cumulative + value / 2,
          labels = paste0(round(value), "%"),
          cex = 0.9,
          col = "white",
          font = 2
        )
      }
      cumulative <- cumulative + value
    }
  }

  legend(
    "right",
    inset = c(-0.19, 0),
    legend = c("Negative", "Neutral", "Positive"),
    fill = sentiment_colors,
    title = "Sentiment",
    bty = "n",
    cex = 1
  )
}

plot_kmeans_top_words <- function() {
  cluster_colors <- c("#0072B2", "#E69F00", "#009E73")

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par))
  par(mfrow = c(1, 3), mar = c(5, 6.5, 5, 1.5), las = 1)

  for (cluster_index in seq_along(top_word_scores_by_cluster)) {
    scores <- rev(top_word_scores_by_cluster[[cluster_index]])
    barplot(
      scores,
      horiz = TRUE,
      col = cluster_colors[cluster_index],
      border = "#333333",
      main = paste0(names(top_word_scores_by_cluster)[cluster_index], "\nTop Words"),
      xlab = "Average TF-IDF importance",
      cex.names = 0.9,
      cex.axis = 0.85,
      cex.lab = 0.9,
      cex.main = 1
    )
  }

  mtext(
    "Words with the highest average TF-IDF score inside each K-means cluster",
    outer = TRUE,
    line = -1,
    cex = 1
  )
}

png("kmeans_sentiment_alignment.png", width = 1400, height = 900, res = 130)
plot_kmeans_sentiment_summary()
dev.off()

png("kmeans_cluster_top_words.png", width = 1500, height = 700, res = 130)
plot_kmeans_top_words()
dev.off()

# A 2D PCA view helps visualize how the K-means clusters separate in text space.
pca <- prcomp(tf_idf, center = TRUE, scale. = FALSE)
pca_points <- data.frame(
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  sentiment = data$sentiment,
  cluster = data$cluster
)

png("kmeans_text_clusters_pca.png", width = 1100, height = 750)
cluster_colors <- c("#0072B2", "#E69F00", "#009E73")
point_colors <- adjustcolor(cluster_colors, alpha.f = 0.45)
sentiment_shapes <- c(17, 16, 15)
plot(
  pca_points$PC1,
  pca_points$PC2,
  col = point_colors[pca_points$cluster],
  pch = sentiment_shapes[pca_points$sentiment],
  cex = 0.75,
  main = "K-means Clusters of Documents Using TF-IDF Text Features",
  xlab = "Principal Component 1",
  ylab = "Principal Component 2"
)
legend(
  "topright",
  legend = paste("Cluster", levels(data$cluster)),
  col = cluster_colors,
  pch = 16,
  title = "K-means cluster"
)
legend(
  "bottomright",
  legend = levels(data$sentiment),
  pch = sentiment_shapes,
  title = "Sentiment"
)
dev.off()

# Show the page 7 chart once, at the end, so it is the visible graph in RStudio's Plots pane.
plot_kmeans_sentiment_summary() 

plot_kmeans_top_words()
