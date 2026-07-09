#Student Stress Clustering Analysis
#This script performs exploratory data analysis, PCA, outlier removal,
#model-based clustering, supervised classification, and regression-based mixture modeling
#on a student lifestyle dataset.

rm(list = ls())

#------------------------------------------------------------
#1. Load packages
#------------------------------------------------------------

library(tidyverse)
library(GGally)
library(gridExtra)
library(heatmaply)
library(mclust)
library(caret)
library(Rmixmod)
library(corrplot)
library(flexmix)

#------------------------------------------------------------
#2. Load data
#------------------------------------------------------------

#If the dataset is stored in a local data folder, use:
student_lifestyle_dataset <- read.csv("data/student_lifestyle_dataset.csv")

data_raw <- student_lifestyle_dataset

#------------------------------------------------------------
#3. Exploratory data analysis
#------------------------------------------------------------

#Check missing values
sum(is.na(data_raw))

#Inspect dataset structure
str(data_raw)

#Keep numeric variables and remove Student_ID
numeric_data <- data_raw %>%
  select(where(is.numeric)) %>%
  na.omit()

numeric_data <- numeric_data[, -1]

#Rename variables for readability
numeric_data <- numeric_data %>%
  rename(
    Study_Hours = Study_Hours_Per_Day,
    Physical_Hours = Physical_Activity_Hours_Per_Day,
    Social_Hours = Social_Hours_Per_Day,
    Extracurricular_Hours = Extracurricular_Hours_Per_Day,
    Sleep_Hours = Sleep_Hours_Per_Day
  ) %>%
  select(
    Study_Hours,
    Physical_Hours,
    Social_Hours,
    Extracurricular_Hours,
    Sleep_Hours,
    GPA
  ) %>%
  na.omit()

label_data <- data_raw$Stress_Level

#------------------------------------------------------------
#4. Stress level distribution
#------------------------------------------------------------

barplot_stress <- ggplot(data_raw, aes(x = as.factor(Stress_Level), fill = as.factor(Stress_Level))) +
  geom_bar() +
  labs(
    title = "Distribution of Stress Levels among Students",
    subtitle = "Absolute Frequency Representation",
    x = "Stress Level",
    y = "Absolute Frequency",
    fill = "Stress Level"
  ) +
  theme_minimal()

print(barplot_stress)

#------------------------------------------------------------
#5. Principal Component Analysis
#------------------------------------------------------------

pca <- princomp(numeric_data, cor = TRUE)

print(pca)

explained_variance_first_4 <- sum((pca$sdev[1:4])^2) / length(pca$sdev)
print(explained_variance_first_4)

top_variables <- apply(
  abs(pca$loadings[, 1:4]),
  2,
  function(x) names(numeric_data)[which.max(x)]
)

print(top_variables)

screeplot(pca, type = "lines", main = "Scree Plot")

component_labels <- apply(
  abs(pca$loadings[, 1:6]),
  2,
  function(x) names(numeric_data)[which.max(x)]
)

text(
  x = 1:length(pca$sdev),
  y = pca$sdev^2,
  labels = component_labels,
  pos = 4.5,
  cex = 0.6,
  col = "red3"
)

#------------------------------------------------------------
#6. Histograms, scatter plots, and boxplots
#------------------------------------------------------------

pca_data <- numeric_data %>%
  select(
    Study_Hours,
    Physical_Hours,
    Social_Hours,
    Extracurricular_Hours
  ) %>%
  na.omit()

hist1 <- ggplot(pca_data, aes(x = Study_Hours, fill = label_data)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(
    title = "Study Hours Distribution",
    x = "Study Hours",
    y = "Frequency",
    fill = "Stress Level"
  ) +
  theme_minimal()

hist2 <- ggplot(pca_data, aes(x = Physical_Hours, fill = label_data)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(
    title = "Physical Activity Hours Distribution",
    x = "Physical Activity Hours",
    y = "Frequency",
    fill = "Stress Level"
  ) +
  theme_minimal()

hist3 <- ggplot(pca_data, aes(x = Social_Hours, fill = label_data)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(
    title = "Social Hours Distribution",
    x = "Social Hours",
    y = "Frequency",
    fill = "Stress Level"
  ) +
  theme_minimal()

hist4 <- ggplot(pca_data, aes(x = Extracurricular_Hours, fill = label_data)) +
  geom_histogram(binwidth = 1, color = "black", alpha = 0.7) +
  labs(
    title = "Extracurricular Hours Distribution",
    x = "Extracurricular Hours",
    y = "Frequency",
    fill = "Stress Level"
  ) +
  theme_minimal()

grid.arrange(hist1, hist2, hist3, hist4, nrow = 2)

scatter1 <- ggplot(pca_data, aes(x = Study_Hours, y = Physical_Hours, color = label_data)) +
  geom_point() +
  labs(title = "Study Hours vs Physical Activity Hours", color = "Stress Level") +
  theme_minimal()

scatter2 <- ggplot(pca_data, aes(x = Study_Hours, y = Social_Hours, color = label_data)) +
  geom_point() +
  labs(title = "Study Hours vs Social Hours", color = "Stress Level") +
  theme_minimal()

scatter3 <- ggplot(pca_data, aes(x = Study_Hours, y = Extracurricular_Hours, color = label_data)) +
  geom_point() +
  labs(title = "Study Hours vs Extracurricular Hours", color = "Stress Level") +
  theme_minimal()

scatter4 <- ggplot(pca_data, aes(x = Physical_Hours, y = Social_Hours, color = label_data)) +
  geom_point() +
  labs(title = "Physical Activity Hours vs Social Hours", color = "Stress Level") +
  theme_minimal()

grid.arrange(scatter1, scatter2, scatter3, scatter4, nrow = 2)

boxplot1 <- ggplot(pca_data, aes(x = label_data, y = Study_Hours, fill = label_data)) +
  geom_boxplot() +
  labs(title = "Boxplot of Study Hours", x = "Stress Level", fill = "Stress Level") +
  theme_minimal()

boxplot2 <- ggplot(pca_data, aes(x = label_data, y = Physical_Hours, fill = label_data)) +
  geom_boxplot() +
  labs(title = "Boxplot of Physical Activity Hours", x = "Stress Level", fill = "Stress Level") +
  theme_minimal()

boxplot3 <- ggplot(pca_data, aes(x = label_data, y = Social_Hours, fill = label_data)) +
  geom_boxplot() +
  labs(title = "Boxplot of Social Hours", x = "Stress Level", fill = "Stress Level") +
  theme_minimal()

boxplot4 <- ggplot(pca_data, aes(x = label_data, y = Extracurricular_Hours, fill = label_data)) +
  geom_boxplot() +
  labs(title = "Boxplot of Extracurricular Hours", x = "Stress Level", fill = "Stress Level") +
  theme_minimal()

grid.arrange(boxplot1, boxplot2, boxplot3, boxplot4, nrow = 2)

#------------------------------------------------------------
#7. Outlier detection and removal
#------------------------------------------------------------

find_outliers_iqr <- function(data, column) {
  Q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  Q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  
  IQR_value <- Q3 - Q1
  
  lower_bound <- Q1 - 1.5 * IQR_value
  upper_bound <- Q3 + 1.5 * IQR_value
  
  outliers <- data[data[[column]] < lower_bound | data[[column]] > upper_bound, ]
  
  return(outliers)
}

high_stress_data <- subset(data_raw, Stress_Level == "High")

outliers_study <- find_outliers_iqr(
  high_stress_data,
  "Study_Hours_Per_Day"
)

outliers_activity <- find_outliers_iqr(
  high_stress_data,
  "Physical_Activity_Hours_Per_Day"
)

outliers_combined <- unique(rbind(outliers_study, outliers_activity))

data_cleaned <- data_raw[
  !data_raw$Student_ID %in% outliers_combined$Student_ID,
]

#Scatter plot without outliers
ggplot(
  data_cleaned,
  aes(
    x = Study_Hours_Per_Day,
    y = Physical_Activity_Hours_Per_Day,
    color = Stress_Level
  )
) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Classification into Three Stress Levels",
    subtitle = "Without outliers",
    x = "Study Hours",
    y = "Physical Activity Hours"
  )

#Scatter plot with outliers
ggplot(
  data_raw,
  aes(
    x = Study_Hours_Per_Day,
    y = Physical_Activity_Hours_Per_Day,
    color = Stress_Level
  )
) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Classification into Three Stress Levels",
    subtitle = "With outliers",
    x = "Study Hours",
    y = "Physical Activity Hours"
  )

#------------------------------------------------------------
#8. PCA after outlier removal
#------------------------------------------------------------

numeric_data_cleaned <- data_cleaned %>%
  select(where(is.numeric)) %>%
  na.omit()

numeric_data_cleaned <- numeric_data_cleaned[, -1]

pca_cleaned <- princomp(numeric_data_cleaned, cor = TRUE)

print(pca_cleaned)

explained_variance_first_3 <- sum((pca_cleaned$sdev[1:3])^2) / length(pca_cleaned$sdev)
print(explained_variance_first_3)

screeplot(pca_cleaned, type = "lines", main = "Scree Plot after Outlier Removal")

component_labels_cleaned <- apply(
  abs(pca_cleaned$loadings[, 1:6]),
  2,
  function(x) names(numeric_data_cleaned)[which.max(x)]
)

text(
  x = 1:length(pca_cleaned$sdev),
  y = pca_cleaned$sdev^2,
  labels = component_labels_cleaned,
  pos = 4.5,
  cex = 0.6,
  col = "red3"
)

#------------------------------------------------------------
#9. Prepare cleaned dataset
#------------------------------------------------------------

data_model <- data_cleaned %>%
  rename(
    Study_Hours = Study_Hours_Per_Day,
    Physical_Hours = Physical_Activity_Hours_Per_Day,
    Social_Hours = Social_Hours_Per_Day,
    Extracurricular_Hours = Extracurricular_Hours_Per_Day,
    Sleep_Hours = Sleep_Hours_Per_Day
  ) %>%
  select(
    Study_Hours,
    Physical_Hours,
    Sleep_Hours,
    Extracurricular_Hours,
    Stress_Level,
    GPA
  ) %>%
  na.omit()

boxplot_study <- ggplot(data_model, aes(x = Stress_Level, y = Study_Hours, fill = Stress_Level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Study Hours", fill = "Stress Level") +
  theme_minimal()

boxplot_physical <- ggplot(data_model, aes(x = Stress_Level, y = Physical_Hours, fill = Stress_Level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Physical Activity Hours", fill = "Stress Level") +
  theme_minimal()

boxplot_sleep <- ggplot(data_model, aes(x = Stress_Level, y = Sleep_Hours, fill = Stress_Level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Sleep Hours", fill = "Stress Level") +
  theme_minimal()

boxplot_extra <- ggplot(data_model, aes(x = Stress_Level, y = Extracurricular_Hours, fill = Stress_Level)) +
  geom_boxplot() +
  labs(title = "Boxplot of Extracurricular Hours", fill = "Stress Level") +
  theme_minimal()

grid.arrange(
  boxplot_study,
  boxplot_physical,
  boxplot_sleep,
  boxplot_extra,
  nrow = 2
)

#------------------------------------------------------------
#10. Scatter plot matrix and correlation heatmap
#------------------------------------------------------------

pca_data$Label <- as.factor(label_data)

scatter_matrix <- ggpairs(
  pca_data,
  aes(color = Label, alpha = 0.5)
)

print(scatter_matrix)

correlation_data <- data_model %>%
  select(-Stress_Level)

cor_matrix <- cor(correlation_data)

heatmaply_cor(
  cor_matrix,
  dendrogram = "none",
  main = "Correlation Heatmap",
  column_text_angle = 0,
  fontsize_row = 8,
  fontsize_col = 8,
  margins = c(60, 60, 60, NA),
  cellnote = cor_matrix,
  draw_cellnote = TRUE,
  cellnote_textposition = "middle center",
  cellnote_size = 7,
  cellnote_color = "black",
  colorbar_thickness = 15,
  colors = colorRampPalette(c("white", "palegreen2", "palegreen4")),
  key.title = "Values from -1 to 1",
  k_col = 2,
  k_row = 2,
  grid_color = "black"
)

#------------------------------------------------------------
#11. Model-based clustering with Mclust
#------------------------------------------------------------

clustering_data <- data_model %>%
  select(where(is.numeric)) %>%
  na.omit()

icl_model <- mclustICL(clustering_data)
summary(icl_model)

clustering_model <- Mclust(clustering_data, G = 3)
summary(clustering_model)

clustering_data$New_Stress_Level <- as.factor(clustering_model$classification)

cluster_order <- aggregate(
  clustering_data$Study_Hours,
  by = list(clustering_data$New_Stress_Level),
  mean
)

colnames(cluster_order) <- c("Cluster", "Mean_Study_Hours")
cluster_order <- cluster_order[order(cluster_order$Mean_Study_Hours), ]

data_model$New_Stress_Level <- factor(
  clustering_data$New_Stress_Level,
  levels = cluster_order$Cluster,
  labels = c("Low", "Moderate", "High")
)

true_labels <- factor(
  data_model$Stress_Level,
  levels = c("Low", "Moderate", "High")
)

cluster_labels <- data_model$New_Stress_Level

confusion_matrix_mclust <- confusionMatrix(cluster_labels, true_labels)

print(confusion_matrix_mclust)

confusion_df_mclust <- data.frame(confusion_matrix_mclust$table)
confusion_df_mclust$Reference <- factor(
  confusion_df_mclust$Reference,
  levels = rev(levels(confusion_df_mclust$Reference))
)

ggplot(confusion_df_mclust, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "black", fill = "white") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  labs(
    title = "Confusion Matrix: Mclust Classification",
    x = "Prediction",
    y = "True Class",
    fill = "Occurrences"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

classification_error_mclust <- classError(cluster_labels, true_labels)
print(classification_error_mclust)

#Original classification
ggplot(data_model, aes(x = Study_Hours, y = Sleep_Hours, color = Stress_Level)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Original Stress Classification",
    x = "Study Hours",
    y = "Sleep Hours"
  )

#Mclust classification
ggplot(data_model, aes(x = Study_Hours, y = Sleep_Hours, color = New_Stress_Level)) +
  geom_point(size = 2, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Mclust Classification",
    x = "Study Hours",
    y = "Sleep Hours"
  )

plot(clustering_model, what = "BIC")

#------------------------------------------------------------
#12. Supervised classification with Rmixmod
#------------------------------------------------------------

stress_data <- clustering_data[, c(
  "Sleep_Hours",
  "Study_Hours",
  "Physical_Hours",
  "Extracurricular_Hours"
)]

stress_class <- as.integer(
  factor(
    data_model$Stress_Level,
    levels = c("Low", "Moderate", "High")
  )
)

set.seed(123)

total_rows <- nrow(stress_data)

train_indices <- sample(
  1:total_rows,
  size = floor(0.8 * total_rows),
  replace = FALSE
)

test_indices <- setdiff(1:total_rows, train_indices)

train_data <- stress_data[train_indices, ]
test_data <- stress_data[test_indices, ]

train_labels <- stress_class[train_indices]
test_labels <- stress_class[test_indices]

cat("Training set size:", nrow(train_data), "\n")
cat("Test set size:", nrow(test_data), "\n")

candidate_models <- c(
  "Gaussian_pk_L_I",
  "Gaussian_pk_Lk_I",
  "Gaussian_pk_L_B",
  "Gaussian_pk_Lk_B",
  "Gaussian_pk_L_Bk",
  "Gaussian_pk_Lk_Bk",
  "Gaussian_pk_L_C",
  "Gaussian_pk_Lk_C",
  "Gaussian_pk_L_D_Ak_D",
  "Gaussian_pk_Lk_D_Ak_D",
  "Gaussian_pk_L_Dk_A_Dk",
  "Gaussian_pk_Lk_Dk_A_Dk",
  "Gaussian_pk_L_Ck",
  "Gaussian_pk_Lk_Ck"
)

mixmod_result <- mixmodLearn(
  train_data,
  train_labels,
  models = mixmodGaussianModel(listModels = candidate_models),
  criterion = c("CV", "BIC")
)

summary(mixmod_result)
print(mixmod_result)

available_models <- mixmodGaussianModel()@listModels

CV <- rep(NA, length(available_models))
BIC_values <- rep(NA, length(available_models))
model_names <- rep(NA, length(available_models))

for (i in seq_along(available_models)) {
  current_model <- available_models[i]
  
  model_fit <- mixmodLearn(
    train_data,
    train_labels,
    models = mixmodGaussianModel(listModels = current_model),
    criterion = c("CV", "BIC")
  )
  
  CV[i] <- model_fit@results[[1]]@criterionValue[1]
  BIC_values[i] <- model_fit@results[[1]]@criterionValue[2]
  model_names[i] <- current_model
}

model_results <- data.frame(
  Model = model_names,
  CV = CV,
  BIC = BIC_values
)

model_results <- arrange(model_results, CV)

print(model_results)

par(mfrow = c(2, 1))

plot(
  BIC_values,
  type = "b",
  xlab = "",
  xaxt = "n",
  col = 2,
  main = "BIC across Rmixmod Models"
)

axis(
  1,
  at = 1:length(model_names),
  labels = substr(model_names, 10, 30),
  cex.axis = 0.8,
  las = 2
)

abline(v = which.min(BIC_values), col = 1, lty = 2)

plot(
  CV,
  type = "b",
  xlab = "",
  xaxt = "n",
  col = 3,
  main = "CV across Rmixmod Models"
)

axis(
  1,
  at = 1:length(model_names),
  labels = substr(model_names, 10, 30),
  cex.axis = 0.8,
  las = 2
)

abline(v = which.min(CV), col = 1, lty = 2)

par(mfrow = c(1, 1))

prediction <- mixmodPredict(
  data = test_data,
  classificationRule = mixmod_result["bestResult"]
)

predicted_labels <- as.factor(prediction@partition)

confusion_matrix_test <- confusionMatrix(
  predicted_labels,
  as.factor(test_labels)
)

print(confusion_matrix_test)

confusion_df_test <- data.frame(confusion_matrix_test$table)
confusion_df_test$Reference <- factor(
  confusion_df_test$Reference,
  levels = rev(levels(confusion_df_test$Reference))
)

ggplot(confusion_df_test, aes(x = Prediction, y = Reference, fill = Freq)) +
  geom_tile(color = "black", fill = "white") +
  geom_text(aes(label = Freq), color = "black", size = 6) +
  labs(
    title = "Confusion Matrix: Test Set Classification",
    x = "Prediction",
    y = "True Class",
    fill = "Occurrences"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16, face = "bold"))

#------------------------------------------------------------
#13. General correlation heatmap
#------------------------------------------------------------

correlation_data_general <- data_cleaned %>%
  rename(
    Study_Hours = Study_Hours_Per_Day,
    Physical_Hours = Physical_Activity_Hours_Per_Day,
    Social_Hours = Social_Hours_Per_Day,
    Extracurricular_Hours = Extracurricular_Hours_Per_Day,
    Sleep_Hours = Sleep_Hours_Per_Day
  ) %>%
  select(
    Study_Hours,
    Physical_Hours,
    Social_Hours,
    Extracurricular_Hours,
    Sleep_Hours,
    GPA
  ) %>%
  na.omit()

cor_matrix_general <- cor(correlation_data_general)

heatmaply_cor(
  cor_matrix_general,
  dendrogram = "none",
  main = "General Correlation Heatmap",
  column_text_angle = 30,
  fontsize_row = 8,
  fontsize_col = 8,
  margins = c(60, 60, 60, NA),
  cellnote = cor_matrix_general,
  draw_cellnote = TRUE,
  cellnote_textposition = "middle center",
  cellnote_size = 7,
  cellnote_color = "black",
  colorbar_thickness = 15,
  colors = colorRampPalette(c("white", "salmon1", "salmon3")),
  k_col = 2,
  k_row = 2,
  grid_color = "black"
)

#------------------------------------------------------------
#14. Flexmix regression analysis
#------------------------------------------------------------

set.seed(123)

regression_data <- data_cleaned %>%
  select(
    Study_Hours_Per_Day,
    Extracurricular_Hours_Per_Day,
    Sleep_Hours_Per_Day,
    GPA,
    Social_Hours_Per_Day,
    Physical_Activity_Hours_Per_Day
  ) %>%
  na.omit()

cor_matrix_regression <- cor(regression_data)

corrplot(
  cor_matrix_regression,
  method = "color",
  type = "upper",
  order = "hclust",
  addCoef.col = "black",
  col = colorRampPalette(c("blue", "white", "red"))(200),
  tl.col = "black",
  tl.srt = 45
)

#The correlation analysis suggests that GPA is positively associated
#with study hours and negatively associated with physical activity hours.
#Other variables, such as sleep, social, and extracurricular hours,
#appear to have a more marginal relationship with GPA.

data_scaled <- data_cleaned

data_scaled$Study_Hours_Per_Day <- scale(data_cleaned$Study_Hours_Per_Day)
data_scaled$Physical_Activity_Hours_Per_Day <- scale(data_cleaned$Physical_Activity_Hours_Per_Day)
data_scaled$GPA <- scale(data_cleaned$GPA)
data_scaled$Stress_Level <- as.factor(data_cleaned$Stress_Level)

step_model <- stepFlexmix(
  GPA ~ Study_Hours_Per_Day + Physical_Activity_Hours_Per_Day + Stress_Level,
  data = data_scaled,
  k = 2:6,
  model = FLXMRglm(family = "gaussian"),
  concomitant = FLXPmultinom(~ Stress_Level),
  nrep = 10,
  control = list(verbose = 2, iter.max = 1000)
)

par(mfrow = c(1, 2))

plot(
  2:6,
  BIC(step_model),
  type = "b",
  ylab = "BIC",
  xlab = "Number of Clusters",
  main = "BIC for All Models"
)

points(
  which.min(BIC(step_model)) + 1,
  min(BIC(step_model)),
  col = "red",
  pch = 20
)

plot(
  2:6,
  ICL(step_model),
  type = "b",
  ylab = "ICL",
  xlab = "Number of Clusters",
  main = "ICL for All Models"
)

points(
  which.min(ICL(step_model)) + 1,
  min(ICL(step_model)),
  col = "red",
  pch = 20
)

par(mfrow = c(1, 1))

converged <- sapply(step_model@models, function(m) m@converged)

bic_converged <- BIC(step_model)[converged]
icl_converged <- ICL(step_model)[converged]

best_k_bic <- as.numeric(names(bic_converged)[which.min(bic_converged)])
best_k_icl <- as.numeric(names(icl_converged)[which.min(icl_converged)])

par(mfrow = c(1, 2))

plot(
  as.numeric(names(bic_converged)),
  bic_converged,
  type = "b",
  ylab = "BIC",
  xlab = "Number of Clusters",
  main = "BIC for Convergent Models"
)

points(
  best_k_bic,
  min(bic_converged),
  col = "blue",
  pch = 20
)

plot(
  as.numeric(names(icl_converged)),
  icl_converged,
  type = "b",
  ylab = "ICL",
  xlab = "Number of Clusters",
  main = "ICL for Convergent Models"
)

points(
  best_k_icl,
  min(icl_converged),
  col = "blue",
  pch = 20
)

par(mfrow = c(1, 1))

get_best_model <- function(model, converged) {
  if (any(converged)) {
    converged_indices <- which(converged)
    best_index <- converged_indices[which.min(ICL(model)[converged])]
    
    return(getModel(model, which = best_index))
  } else {
    return(NULL)
  }
}

best_model <- get_best_model(step_model, converged)

print(best_model)

data_scaled$Cluster <- as.factor(clusters(best_model))

gpa_vs_study <- ggplot(
  data_scaled,
  aes(
    x = Study_Hours_Per_Day,
    y = GPA,
    color = Cluster
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(
    method = "lm",
    aes(group = Cluster, color = Cluster),
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    title = "Relationship between GPA and Study Hours by Cluster",
    x = "Study Hours per Day",
    y = "GPA",
    color = "Cluster"
  ) +
  theme_minimal()

print(gpa_vs_study)

gpa_vs_physical <- ggplot(
  data_scaled,
  aes(
    x = Physical_Activity_Hours_Per_Day,
    y = GPA,
    color = Cluster
  )
) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(
    method = "lm",
    aes(group = Cluster, color = Cluster),
    se = TRUE,
    linewidth = 1
  ) +
  labs(
    title = "Relationship between GPA and Physical Activity Hours by Cluster",
    x = "Physical Activity Hours per Day",
    y = "GPA",
    color = "Cluster"
  ) +
  theme_minimal()

print(gpa_vs_physical)

ggplot(data_scaled, aes(x = as.factor(Cluster), fill = as.factor(Stress_Level))) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Stress Levels across Clusters",
    x = "Assigned Cluster",
    y = "Count",
    fill = "Stress Level"
  ) +
  theme_minimal()

predictions <- fitted(best_model)

if (ncol(predictions) >= 2) {
  data_scaled$predicted_GPA <- ifelse(
    data_scaled$Cluster == 1,
    predictions[, 1],
    predictions[, 2]
  )
  
  ggplot(data_scaled, aes(x = as.factor(Stress_Level), y = predicted_GPA, fill = as.factor(Cluster))) +
    geom_boxplot(alpha = 0.6) +
    labs(
      title = "Effect of Stress on Predicted GPA across Clusters",
      x = "Stress Level",
      y = "Predicted GPA",
      fill = "Assigned Cluster"
    ) +
    theme_minimal()
}
