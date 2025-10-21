# Install packages if you haven't already
# install.packages(c("caret", "naivebayes"))

# Load tidyverse for convenient data manipulation

library(tidyverse)
library(caret)
library(naivebayes)
library(ggplot2)
library(viridis)  # for colorblind-friendly palettes
library(rpart)
library(rpart.plot)
library(data.tree)
library(visNetwork)
library(randomForest)
library(scales)

# Read my CSV (adjust path)
# hanag_wdy1xo1\OneDrive\Documentos\archive
setwd("C:/Users/hanag_wdy1xo1/OneDrive/Documentos/archive")
df_2 = read.csv("social_media_engagement1.csv", stringsAsFactors = FALSE)

# Convert columns to factors
df_2$post_type <- as.factor(df_2$post_type)
df_2$sentiment_score <- as.factor(df_2$sentiment_score)

# Explore the data
table(df_2$post_type)
table(df_2$sentiment_score)

# Train Naive Bayes model on all rows
model <- naive_bayes(sentiment_score ~ post_type, data = df_2)

# Predict on the same dataset
pred <- predict(model, newdata = df_2)

# Compare predictions vs actual
table(pred, df_2$sentiment_score)

new_posts <- data.frame(post_type = c("image", "video", "carousel"))
pred_new <- predict(model, newdata = new_posts)
pred_new

# Count posts by post_type and sentiment
summary_df_2 <- df_2 %>%
  group_by(post_type, sentiment_score) %>%
  summarise(count = n(), .groups = "drop")

summary_df_2

# Create a Bar Plot of the sentiment scores by post type
ggplot(summary_df_2, aes(x = post_type, y = count, fill = sentiment_score)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_viridis(discrete = TRUE, option = "D") +  # colorblind-friendly
  labs(
    title = "Sentiment Distribution by Post Type",
    subtitle = "Number of positive, neutral, and negative posts per type",
    x = "Post Type",
    y = "Number of Posts",
    fill = "Sentiment",
    caption = "Data includes posts with types: image, video, carousel, etc."
  ) +
  theme_minimal(base_size = 14) +  # bigger text for readability
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black")
  )

# Calculate percentages
summary_df_2_by_percentage <- summary_df_2 %>%
  group_by(post_type) %>%
  mutate(percent = count / sum(count) * 100)

# Plot
ggplot(summary_df_2_by_percentage, aes(x = post_type, y = percent, fill = sentiment_score)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_viridis(discrete = TRUE, option = "D") +  # colorblind-friendly
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 4, color = "white") +  # show percentage inside bars
  labs(
    title = "Sentiment Distribution by Post Type (Percentage)",
    subtitle = "Shows the proportion of positive, neutral, and negative posts per post type",
    x = "Post Type",
    y = "Percentage of Posts",
    fill = "Sentiment",
    caption = "Data includes post types like image, video, carousel, etc."
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic"),
    axis.text = element_text(color = "black")
  )

summary_df_2_by_percentage %>%
  arrange(post_type, sentiment_score) %>%
  select(Post_Type = post_type, Sentiment = sentiment_score, Count = count, Percent = percent)

# Train the decision tree
tree <- rpart(sentiment_score ~ post_type, data = df_2, method = "class")

# Predict probabilities for all posts
pred_probs <- predict(tree, type = "prob")  # matrix: rows = posts, columns = classes

# Add post_type info
pred_df <- cbind(df_2["post_type"], pred_probs)

# Summarize by post_type (each leaf corresponds to a post_type)
leaf_summary <- pred_df %>%
  group_by(post_type) %>%
  summarise(
    n = n(),
    predicted = names(which.max(colMeans(across(positive:negative)))),  # majority predicted
    prob_positive = round(mean(positive), 2),
    prob_neutral = round(mean(neutral), 2),
    prob_negative = round(mean(negative), 2)
  ) %>%
  mutate(
    summary_text = paste0(
      "Post type = ", post_type, 
      ": Predicted sentiment = ", predicted,
      ", n = ", n,
      ", P(positive) = ", prob_positive,
      ", P(neutral) = ", prob_neutral,
      ", P(negative) = ", prob_negative
    )
  )

# Show textual summaries
leaf_summary$summary_text

library(rpart)
library(rpart.plot)
library(dplyr)

# Convert categorical columns to factors
df_2$post_type <- as.factor(df_2$post_type)
df_2$platform <- as.factor(df_2$platform)
df_2$sentiment_score <- as.factor(df_2$sentiment_score)

# Train the tree with multiple predictors
tree_multi <- rpart(sentiment_score ~ post_type + platform + likes + comments + shares,
                    data = df_2,
                    method = "class",
                    control = rpart.control(minsplit = 5, cp = 0.01))

# Plot the tree with counts + predicted class + probabilities
rpart.plot(tree_multi,
           main = "Decision Tree for Sentiment Prediction (Multi-Predictor)",
           type = 4,          # draw separate boxes for each node
           extra = 104,       # predicted class + n + class probabilities
           shadow.col = "gray",
           box.palette = "Blues",
           nn = TRUE)         # show node numbers

# Generate accessible textual summaries per leaf
pred_probs <- predict(tree_multi, type = "prob")   # probabilities per post
pred_df <- cbind(df_2[c("post_type", "platform")], pred_probs)

leaf_summary_1 <- pred_df %>%
  group_by(post_type, platform) %>%
  summarise(
    n = n(),
    predicted = names(which.max(colMeans(across(positive:negative)))),  # majority predicted
    prob_positive = round(mean(positive), 2),
    prob_neutral  = round(mean(neutral), 2),
    prob_negative = round(mean(negative), 2)
  ) %>%
  mutate(
    summary_text = paste0(
      "Post type = ", post_type, ", Platform = ", platform, 
      ": Predicted sentiment = ", predicted,
      ", n = ", n,
      ", P(positive) = ", prob_positive,
      ", P(neutral) = ", prob_neutral,
      ", P(negative) = ", prob_negative
    )
  )

leaf_summary_1$summary_text

# Train tree
tree_multi <- rpart(sentiment_score ~ post_type + platform + likes + comments + shares,
                    data = df_2,
                    method = "class",
                    control = rpart.control(minsplit = 5, cp = 0.01))

# Each observation's node number
df_2$node <- tree_multi$where

# Get predicted probabilities for each observation
pred_probs <- predict(tree_multi, type = "prob")

# Combine with node
pred_df <- cbind(df_2, pred_probs)

# Aggregate per node to get counts + mean probabilities
node_summary <- pred_df %>%
  group_by(node) %>%
  summarise(
    n = n(),
    predicted = names(which.max(colMeans(across(positive:negative)))),
    prob_positive = round(mean(positive), 2),
    prob_neutral  = round(mean(neutral), 2),
    prob_negative = round(mean(negative), 2)
  ) %>%
  mutate(
    label = paste0(
      "Predicted: ", predicted,
      "\nn = ", n,
      "\nP(positive)=", prob_positive,
      ", P(neutral)=", prob_neutral,
      ", P(negative)=", prob_negative
    )
  )

node_summary

# Make sure factors are set
df_2$post_type <- as.factor(df_2$post_type)
df_2$platform <- as.factor(df_2$platform)
df_2$sentiment_score <- as.factor(df_2$sentiment_score)

# Predict likes using other features
set.seed(123)
rf_model <- randomForest(likes ~ post_type + platform + sentiment_score + comments + shares,
                         data = df_2,
                         importance = TRUE)

# Variable importance
importance(rf_model)
varImpPlot(rf_model)

# Predict likes for new data
new_posts <- data.frame(
  post_type = factor(c("image","video"), levels=levels(df_2$post_type)),
  platform = factor(c("Facebook","Twitter"), levels=levels(df_2$platform)),
  sentiment_score = factor(c("positive","negative"), levels=levels(df_2$sentiment_score)),
  comments = c(100,50),
  shares = c(20,30)
)
predict(rf_model, new_posts)

# Engagement by Post Type and Sentiment
ggplot(df_2, aes(x=post_type, y=likes, fill=sentiment_score)) +
  geom_boxplot() +
  scale_fill_viridis(discrete=TRUE, option="D") +
  labs(title="Likes by Post Type and Sentiment",
       x="Post Type",
       y="Likes",
       fill="Sentiment") +
  theme_minimal(base_size = 14)

# Correlation Between Engagement Metrics
library(GGally)
ggpairs(df_2[, c("likes", "comments", "shares")])

# Sentiment Distribution by Platform
# Compute proportions manually
df_plot <- df_2 %>%
  group_by(platform, sentiment_score) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(platform) %>%
  mutate(perc = n / sum(n),
         label = paste0(round(perc * 100, 1), "%"))

# Plot with percentage labels
ggplot(df_plot, aes(x = platform, y = perc, fill = sentiment_score)) +
  geom_bar(stat = "identity", position = "fill") +
  geom_text(aes(label = label),
            position = position_fill(vjust = 0.5),
            color = "white", size = 4.5, fontface = "bold") +
  scale_y_continuous(labels = percent_format()) +
  scale_fill_viridis(discrete = TRUE, option = "D") +
  labs(title = "Proportion of Sentiment by Platform",
       x = "Platform", y = "Percentage of Posts", fill = "Sentiment") +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold", hjust = 0.5)
  )

# Clustering – Posts by Engagement
library(plotly)
set.seed(123)

# Scale engagement metrics
engagement <- scale(df_2[, c("likes","comments","shares")])

# K-means clustering with 3 clusters
km <- kmeans(engagement, centers = 3, nstart = 25)
df_2$cluster <- as.factor(km$cluster)

# Static ggplot
p <- ggplot(df_2, aes(x = likes, y = shares, color = cluster, text = paste(
  "<b>Platform:</b>", platform,
  "<br><b>Post Type:</b>", post_type,
  "<br><b>Likes:</b>", likes,
  "<br><b>Comments:</b>", comments,
  "<br><b>Shares:</b>", shares,
  "<br><b>Cluster:</b>", cluster
))) +
  geom_point(size = 3, alpha = 0.8) +
  labs(
    title = "Clustering Posts by Engagement",
    x = "Likes", y = "Shares", color = "Cluster"
  ) +
  theme_minimal(base_size = 14)

# Convert to interactive plotly
interactive_plot <- ggplotly(p, tooltip = "text")

interactive_plot