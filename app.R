# =========================
# app.R — Social Media Engagement Dashboard
# =========================

# Packages
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(lubridate)
library(tidytext)
library(topicmodels)
library(tm)
library(SnowballC)
library(DT)
library(viridis)
library(scales)
library(stringr)

# -------------------------
# Load Data
# -------------------------
# Adjust path if needed
# setwd("C:/Users/hanag_wdy1xo1/OneDrive/Documentos/archive")
df_2 <- read.csv("social_media_engagement1.csv", stringsAsFactors = FALSE)

# Parse datetime (expects m/d/Y H:M)
# If your format differs, tweak mdy_hm() accordingly
df_2$post_time <- suppressWarnings(lubridate::mdy_hm(df_2$post_time))
# Derive date & month for aggregations
df_2$post_date  <- as.Date(df_2$post_time)
df_2$post_month <- floor_date(df_2$post_date, "month")

# Factors
df_2$post_type        <- as.factor(df_2$post_type)
df_2$platform         <- as.factor(df_2$platform)
df_2$sentiment_score  <- as.factor(df_2$sentiment_score)

# -------------------------
# Helpers
# -------------------------
# Predefined time-window start date
get_start_date <- function(choice, today = Sys.Date()) {
  switch(choice,
         "All Time"      = as.Date("1900-01-01"),
         "Last 30 Days"  = today - 30,
         "Last 90 Days"  = today - 90,
         "Year to Date"  = as.Date(sprintf("%s-01-01", year(today))),
         as.Date("1900-01-01"))
}

# Best-guess text column(s) for LDA (customize if needed)
# If you actually have a text column with captions/posts, put its name here.
text_cols <- c("caption", "text", "body", "content", "message")

find_text_column <- function(df, candidates = text_cols) {
  candidates[candidates %in% names(df)][1]
}

# Build LDA objects safely; returns NULL if no text column / not enough text
build_lda <- function(data, k_topics = 5, text_col = NULL) {
  if (is.null(text_col) || !(text_col %in% names(data))) return(NULL)
  if (all(is.na(data[[text_col]])) || sum(nchar(trimws(data[[text_col]])) > 0) == 0) return(NULL)
  
  # Minimal cleaning & tokenization
  text_data <- data %>%
    select(post_id, !!sym(text_col)) %>%
    mutate(!!sym(text_col) := as.character(!!sym(text_col))) %>%
    filter(!is.na(!!sym(text_col))) %>%
    mutate(!!sym(text_col) := str_squish(!!sym(text_col)))
  
  if (nrow(text_data) == 0) return(NULL)
  
  tidy_text <- tryCatch(
    {
      text_data %>%
        unnest_tokens(word, !!sym(text_col)) %>%
        anti_join(stop_words, by = "word") %>%
        mutate(word = wordStem(word)) %>%
        filter(nchar(word) > 2)
    },
    error = function(e) NULL
  )
  if (is.null(tidy_text) || nrow(tidy_text) == 0) return(NULL)
  
  # DTM
  dtm <- tidy_text %>%
    count(post_id, word) %>%
    cast_dtm(document = post_id, term = word, value = n)
  
  if (nrow(dtm) < k_topics || ncol(dtm) < 5) return(NULL) # not enough data
  
  # Fit LDA
  lda_model <- LDA(dtm, k = k_topics, control = list(seed = 123))
  list(
    model  = lda_model,
    topics = tidy(lda_model, matrix = "beta")
  )
}

# -------------------------
# UI
# -------------------------
ui <- fluidPage(
  titlePanel("Social Media Sentiment & Engagement Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      # Platform filter
      selectInput("platform", "Platform:", choices = c("All", levels(df_2$platform)), selected = "All"),
      # Time filter presets
      selectInput("time_filter", "Time Window:",
                  choices = c("All Time", "Last 30 Days", "Last 90 Days", "Year to Date"),
                  selected = "All Time"),
      # LDA controls
      sliderInput("lda_topics", "Number of LDA Topics:", min = 2, max = 10, value = 5, step = 1),
      # Cluster color toggle
      checkboxInput("color_by_sentiment", "Color clusters by sentiment (instead of cluster id)", TRUE),
      width = 3
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Engagement Over Time", plotlyOutput("engagementPlot", height = "480px")),
        tabPanel("Topic Modeling",
                 br(),
                 uiOutput("ldaInfo"),
                 plotlyOutput("ldaPlot", height = "520px"),
                 DTOutput("ldaTable")),
        tabPanel("Cluster Analysis",
                 plotlyOutput("clusterPlot", height = "520px"),
                 br(),
                 h4("Cluster Summary"),
                 DTOutput("clusterSummary")),
        tabPanel("Sentiment Summary", DTOutput("sentimentTable"))
      ),
      width = 9
    )
  )
)

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {
  
  # Reactive: filter by platform + time window
  filtered_data <- reactive({
    req(df_2$post_date)
    # Apply platform filter
    dat <- if (input$platform == "All") df_2 else df_2 %>% filter(platform == input$platform)
    
    # Apply time filter
    start_date <- get_start_date(input$time_filter, today = Sys.Date())
    dat %>% filter(post_date >= start_date)
  })
  
  # 1) Engagement Over Time (likes, comments, shares)
  output$engagementPlot <- renderPlotly({
    dat <- filtered_data()
    
    # Guard: if empty after filter
    if (nrow(dat) == 0) {
      return(ggplotly(ggplot() + theme_void() + ggtitle("No data in selected filters")))
    }
    
    series <- dat %>%
      group_by(post_month) %>%
      summarise(
        avg_likes    = mean(likes, na.rm = TRUE),
        avg_comments = mean(comments, na.rm = TRUE),
        avg_shares   = mean(shares, na.rm = TRUE),
        .groups = "drop"
      )
    
    p <- ggplot(series, aes(x = post_month)) +
      geom_line(aes(y = avg_likes,    color = "Likes"),    linewidth = 1.2) +
      geom_line(aes(y = avg_comments, color = "Comments"), linewidth = 1.2) +
      geom_line(aes(y = avg_shares,   color = "Shares"),   linewidth = 1.2) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      scale_x_date(date_labels = "%b %Y") +
      labs(title = paste0("Average Engagement Over Time — ", input$platform, " (", input$time_filter, ")"),
           x = "Month", y = "Average Value", color = "Metric") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = c("x", "y", "colour"))
  })
  
  # 2) LDA Topic Modeling — auto-detect text column
  output$ldaInfo <- renderUI({
    dat <- filtered_data()
    txt_col <- find_text_column(dat)
    
    if (is.null(txt_col)) {
      HTML("<b>No text column found for topic modeling.</b> 
           Add a column (e.g., <code>caption</code>) with post text, 
           or update <code>text_cols</code> in the app to point to your text field.")
    } else {
      HTML(paste0("<b>Using text column:</b> <code>", txt_col, "</code>"))
    }
  })
  
  lda_results <- reactive({
    dat <- filtered_data()
    txt_col <- find_text_column(dat)
    build_lda(dat, k_topics = input$lda_topics, text_col = txt_col)
  })
  
  output$ldaPlot <- renderPlotly({
    res <- lda_results()
    if (is.null(res)) {
      return(ggplotly(ggplot() + theme_void() + ggtitle("LDA not available — no usable text column")))
    }
    
    top_terms <- res$topics %>%
      group_by(topic) %>%
      slice_max(beta, n = 10, with_ties = FALSE) %>%
      arrange(topic, desc(beta)) %>%
      ungroup()
    
    p <- ggplot(top_terms, aes(x = reorder(term, beta), y = beta, fill = factor(topic))) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~ topic, scales = "free") +
      coord_flip() +
      scale_fill_viridis(discrete = TRUE) +
      labs(title = paste0("Top Words per Topic (k = ", input$lda_topics, ")"),
           x = "Word", y = "β (importance)") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
    ggplotly(p, tooltip = c("x", "y"))
  })
  
  output$ldaTable <- renderDT({
    res <- lda_results()
    if (is.null(res)) {
      return(DT::datatable(data.frame(Message = "LDA not available — no usable text column."),
                           options = list(dom = 't')))
    }
    top_terms <- res$topics %>%
      group_by(topic) %>%
      slice_max(beta, n = 10, with_ties = FALSE) %>%
      summarise(Top_Terms = paste(term, collapse = ", "), .groups = "drop")
    datatable(top_terms, rownames = FALSE, options = list(pageLength = 5))
  })
  
  # 3) Cluster Analysis (K-means on scaled engagement)
  output$clusterPlot <- renderPlotly({
    dat <- filtered_data()
    if (nrow(dat) == 0) {
      return(ggplotly(ggplot() + theme_void() + ggtitle("No data in selected filters")))
    }
    
    engage <- dat[, c("likes", "comments", "shares")]
    if (any(!sapply(engage, is.numeric))) {
      return(ggplotly(ggplot() + theme_void() + ggtitle("Engagement fields must be numeric")))
    }
    
    set.seed(123)
    km <- kmeans(scale(engage), centers = 3, nstart = 25)
    plot_data <- dat %>%
      mutate(cluster = factor(km$cluster),
             legend_color = if (isTRUE(input$color_by_sentiment)) sentiment_score else cluster)
    
    p <- ggplot(plot_data,
                aes(x = likes, y = shares, color = legend_color,
                    text = paste0(
                      "<b>Platform:</b> ", platform,
                      "<br><b>Post Type:</b> ", post_type,
                      "<br><b>Sentiment:</b> ", sentiment_score,
                      "<br><b>Likes:</b> ", likes,
                      "<br><b>Comments:</b> ", comments,
                      "<br><b>Shares:</b> ", shares,
                      "<br><b>Cluster:</b> ", cluster
                    ))) +
      geom_point(size = 3, alpha = 0.85) +
      scale_color_viridis(discrete = TRUE, option = "D") +
      labs(title = "Clustering Posts by Engagement",
           x = "Likes", y = "Shares",
           color = if (isTRUE(input$color_by_sentiment)) "Sentiment" else "Cluster") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold"))
    
    ggplotly(p, tooltip = "text")
  })
  
  # Cluster Summary Table (averages + sentiment mix)
  output$clusterSummary <- renderDT({
    dat <- filtered_data()
    if (nrow(dat) == 0) {
      return(DT::datatable(data.frame(Message = "No data in selected filters"), options = list(dom = 't')))
    }
    
    km <- kmeans(scale(dat[, c("likes", "comments", "shares")]), centers = 3, nstart = 25)
    dat2 <- dat %>% mutate(cluster = factor(km$cluster))
    
    # Averages per cluster
    agg <- dat2 %>%
      group_by(cluster) %>%
      summarise(
        n = n(),
        avg_likes    = round(mean(likes, na.rm = TRUE), 1),
        avg_comments = round(mean(comments, na.rm = TRUE), 1),
        avg_shares   = round(mean(shares, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    # Sentiment distribution per cluster
    sent <- dat2 %>%
      group_by(cluster, sentiment_score) %>%
      summarise(cnt = n(), .groups = "drop") %>%
      group_by(cluster) %>%
      mutate(pct = round(cnt / sum(cnt) * 100, 1)) %>%
      tidyr::pivot_wider(names_from = sentiment_score, values_from = pct, values_fill = 0)
    
    out <- agg %>% left_join(sent, by = "cluster")
    datatable(out, rownames = FALSE, options = list(pageLength = 5))
  })
  
  # 4) Sentiment Summary table (counts + percentages within current filter)
  output$sentimentTable <- renderDT({
    dat <- filtered_data()
    if (nrow(dat) == 0) {
      return(DT::datatable(data.frame(Message = "No data in selected filters"), options = list(dom = 't')))
    }
    
    total_n <- nrow(dat)
    sumtab <- dat %>%
      group_by(platform, post_type, sentiment_score) %>%
      summarise(count = n(), .groups = "drop") %>%
      group_by(platform, post_type) %>%
      mutate(percent_within_type = round(100 * count / sum(count), 1)) %>%
      ungroup() %>%
      arrange(platform, post_type, desc(count))
    
    datatable(sumtab, rownames = FALSE, options = list(pageLength = 10))
  })
  
}

# -------------------------
# Run the app
# -------------------------
shinyApp(ui, server)
