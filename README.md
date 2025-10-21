# 🧠 Social Media Sentiment & Engagement Analysis  
**Exploring how sentiment, engagement, and platform dynamics shape online conversations**

[![Made with R](https://img.shields.io/badge/Made%20with-R-276DC3?logo=r&logoColor=white)](https://www.r-project.org/)
[![Shiny Dashboard](https://img.shields.io/badge/Shiny-Interactive%20Dashboard-75AADB?logo=r)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Contributions Welcome](https://img.shields.io/badge/Contributions-Welcome-brightgreen.svg)](https://github.com/hgbidon/social-media-engagement-dashboard/issues)
[![Open in RStudio](https://img.shields.io/badge/Open%20in-RStudio-blue?logo=rstudio)](https://posit.co/download/rstudio/)

---

## 📊 Overview
This project builds an **end-to-end analytics workflow in R** to explore how people engage with content across social media platforms.  

It combines **sentiment classification**, **engagement clustering**, and **interactive data visualization** to uncover patterns in audience reactions and post performance.  

The project demonstrates an integrated approach to **data wrangling, supervised and unsupervised modeling, and dashboard visualization** — emphasizing **accessibility** and **interpretability** in analysis.

---

## 🧩 Key Highlights
- Trained multiple models — **Naive Bayes**, **Decision Trees**, and **Random Forests** — to classify post sentiment.  
- Performed **K-Means clustering** on engagement metrics (likes, comments, shares) to identify audience behavior groups.  
- Created **interactive dashboards** using **Plotly** and **visNetwork** for dynamic exploration of engagement and decision paths.  
- Visualized explainable insights such as **feature importance**, **sentiment proportions**, and **cluster relationships** using **ggplot2**.  
- Designed visualizations with **colorblind-accessible palettes** for inclusivity.

---

## ⚙️ Tech Stack

| Category | Tools & Libraries |
|-----------|------------------|
| **Language** | R |
| **Core Packages** | `tidyverse` · `ggplot2` · `plotly` · `rpart` · `randomForest` · `dplyr` · `caret` |
| **Modeling** | Naive Bayes · Decision Trees · Random Forest · K-Means |
| **Visualization** | ggplot2 · Plotly · visNetwork · viridis |
| **Utilities** | scales · data.tree · DT |
| **Environment** | RStudio / Posit IDE |

---

## 🚀 How to Run Locally

### 1️⃣ Clone the repository:
```bash
git clone https://github.com/hgbidon/social-media-engagement-dashboard.git
cd social-media-engagement-dashboard
