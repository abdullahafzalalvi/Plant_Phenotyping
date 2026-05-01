####PHASE 4: Model Training, Evaluation & Results

library(randomForest)
library(caret)
library(ggplot2)
library(dplyr)
library(viridis)
library(scales)

cat("ackages loaded\n\n")
####LOAD PHASE 3 ENVIRONMENT
ROOT <- "C:/Users/Hp Computer/Desktop/Plant_Phenotyping_Project"

PROC_DIR    <- file.path(ROOT, "data", "processed")
FIGURES_DIR <- file.path(ROOT, "figures")
OUTPUTS_DIR <- file.path(ROOT, "outputs")
MODELS_DIR  <- file.path(ROOT, "models")

load(file.path(PROC_DIR, "phase3_environment.RData"))
cat(sprintf("Loaded features_df: %d rows x %d columns\n\n",
            nrow(features_df), ncol(features_df)))
####____
cat("preparing data for modeling:\n")
set.seed(42)
model_df <- features_df %>%
  select(-class) %>%
  mutate(label = factor(label, levels = c("healthy", "diseased")))


# Remove any rows with NA or infinite values

model_df <- model_df[complete.cases(model_df), ]
model_df <- model_df[apply(model_df[, -1], 1,
                           function(x) all(is.finite(x))), ]

cat(sprintf("  Clean rows for modelling: %d\n", nrow(model_df)))
cat(sprintf("  Healthy  : %d\n", sum(model_df$label == "healthy")))
cat(sprintf("  Diseased : %d\n\n", sum(model_df$label == "diseased")))
train_idx  <- createDataPartition(model_df$label,
                                  p = 0.80, list = FALSE)
train_data <- model_df[train_idx, ]
test_data  <- model_df[-train_idx, ]

X_train <- train_data %>% select(-label)
y_train <- train_data$label
X_test  <- test_data %>% select(-label)
y_test  <- test_data$label

cat(sprintf("  Training set : %d samples\n", nrow(train_data)))
cat(sprintf("  Testing set  : %d samples\n\n", nrow(test_data)))
####TRAIN RANDOM FOREST MODEL
cat("training Random Forest model:\n")
cat("   ntree = 500 | importance = TRUE\n")
cat("   Please wait ~2-5 minutes...\n\n")
rf_model <- randomForest(
  x          = X_train,
  y          = y_train,
  ntree      = 500,
  importance = TRUE,
  proximity  = FALSE
)
cat("Model training complete!\n\n")
print(rf_model)
####Save model----
model_path <- file.path(MODELS_DIR, "random_forest_model.rds")
saveRDS(rf_model, model_path)
cat(sprintf("model saved → models/random_forest_model.rds\n\n"))

#### EVALUATE ON TEST SET
cat("Evaluating on test set:\n\n")
predictions  <- predict(rf_model, newdata = X_test)
conf_matrix  <- confusionMatrix(predictions, y_test,
                                positive = "diseased")
accuracy    <- round(conf_matrix$overall["Accuracy"]        * 100, 2)
sensitivity <- round(conf_matrix$byClass["Sensitivity"]     * 100, 2)
specificity <- round(conf_matrix$byClass["Specificity"]     * 100, 2)
precision   <- round(conf_matrix$byClass["Precision"]       * 100, 2)
f1_score    <- round(conf_matrix$byClass["F1"], 4)
oob_error   <- round(min(rf_model$err.rate[, "OOB"]) * 100, 2)

cat(" Key performance metrics \n\n")
cat(sprintf("  Accuracy        : %.2f%%\n", accuracy))
cat(sprintf("  Sensitivity     : %.2f%%\n", sensitivity))
cat(sprintf("  Specificity     : %.2f%%\n", specificity))
cat(sprintf("  Precision       : %.2f%%\n", precision))
cat(sprintf("  F1-Score        : %.4f\n",   f1_score))
cat(sprintf("  OOB Error       : %.2f%%\n\n", oob_error))
#### FIGURE 8 — CONFUSION MATRIX

cat(" Generating figures \n\n")
theme_plant <- theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )
cm_df <- as.data.frame(conf_matrix$table)
names(cm_df) <- c("Predicted", "Reference", "Count")
cm_df <- cm_df %>%
  mutate(
    pct   = round(Count / sum(Count) * 100, 1),
    label = paste0(Count, "\n(", pct, "%)")
  )
p8 <- ggplot(cm_df,
             aes(x = Predicted, y = Reference, fill = Count)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = label),
            size = 7, fontface = "bold", color = "white") +
  scale_fill_gradient(low = "#74add1", high = "#1a3a6b") +
  labs(
    title    = "Figure 8 — Confusion Matrix",
    subtitle = sprintf(
      "Accuracy: %.2f%% | F1-Score: %.4f | Sensitivity: %.2f%% | n = %d test images",
      accuracy, f1_score, sensitivity, nrow(test_data)),
    x = "Predicted class",
    y = "True class"
  ) +
  theme_plant +
  theme(legend.position = "none",
        axis.text = element_text(size = 13, face = "bold"))
ggsave(file.path(FIGURES_DIR, "fig08_confusion_matrix.png"),
       p8, width = 6, height = 5, dpi = 300)
print(p8)
cat("  fig08_confusion_matrix.png\n")

#### FIGURE 9 — FEATURE IMPORTANCE

imp_df <- as.data.frame(importance(rf_model))
imp_df$Feature <- rownames(imp_df)
imp_df <- imp_df %>%
  arrange(desc(MeanDecreaseGini)) %>%
  head(15) %>%
  mutate(Feature = reorder(Feature, MeanDecreaseGini))
p9 <- ggplot(imp_df,
             aes(x = Feature, y = MeanDecreaseGini,
                 fill = MeanDecreaseGini)) +
  geom_col(color = "white") +
  coord_flip() +
  scale_fill_viridis(option = "D", direction = -1) +
  labs(
    title    = "Figure 9 — Top 15 Feature Importances",
    subtitle = "Mean Decrease Gini — higher value = more discriminative feature",
    x = "Feature",
    y = "Mean Decrease Gini"
  ) +
  theme_plant +
  theme(legend.position = "none")
ggsave(file.path(FIGURES_DIR, "fig09_feature_importance.png"),
       p9, width = 8, height = 6, dpi = 300)
print(p9)
cat("  fig09_feature_importance.png\n")
#### FIGURE 10 — OOB ERROR CURVE
oob_df <- data.frame(
  trees = seq_len(nrow(rf_model$err.rate)),
  oob   = rf_model$err.rate[, "OOB"] * 100
)
p10 <- ggplot(oob_df, aes(x = trees, y = oob)) +
  geom_line(color = "#1a3a6b", linewidth = 0.8) +
  geom_hline(yintercept = min(oob_df$oob),
             linetype = "dashed",
             color = "#CC2936", linewidth = 0.8) +
  annotate("text",
           x     = max(oob_df$trees) * 0.65,
           y     = min(oob_df$oob) + 0.5,
           label = sprintf("Min OOB = %.2f%%", min(oob_df$oob)),
           color = "#CC2936", fontface = "bold", size = 4) +
  labs(
    title    = "Figure 10 — OOB Error Rate Convergence",
    subtitle = "Model stabilises after ~200 trees — well converged at 500 trees",
    x = "Number of trees",
    y = "OOB error rate (%)"
  ) +
  theme_plant
ggsave(file.path(FIGURES_DIR, "fig10_oob_error_curve.png"),
       p10, width = 8, height = 4.5, dpi = 300)
print(p10)
ggsave(file.path(FIGURES_DIR, "fig10_oob_error_curve.png"),
       p10, width = 8, height = 4.5, dpi = 300)
cat("  fig10_oob_error_curve.png\n")
####SAVE RESULTS SUMMARY CSV
results_summary <- data.frame(
  Metric = c(
    "Total images", "Training images", "Test images",
    "Accuracy (%)", "Sensitivity (%)", "Specificity (%)",
    "Precision (%)", "F1-Score", "OOB Error (%)"
  ),
  Value = c(
    nrow(model_df), nrow(train_data), nrow(test_data),
    accuracy, sensitivity, specificity,
    precision, f1_score, oob_error
  )
)
results_path <- file.path(OUTPUTS_DIR, "model_results_summary.csv")
write.csv(results_summary, results_path, row.names = FALSE)
cat(sprintf("\n  esults summary saved → outputs/model_results_summary.csv\n"))
cat("\n Final results summary \n\n")
print(results_summary)

#### FINAL PROJECT SUMMARY
cat(sprintf("  Model          : Random Forest (ntree = 500)\n"))
cat(sprintf("  Accuracy       : %.2f%%\n",  accuracy))
cat(sprintf("  F1-Score       : %.4f\n",    f1_score))
cat(sprintf("  OOB Error      : %.2f%%\n",  oob_error))
cat(sprintf("  Figures saved  : 10 total (fig01 to fig10)\n"))
cat(sprintf("  Model saved    : models/random_forest_model.rds\n"))
cat(sprintf("  Results saved  : outputs/model_results_summary.csv\n"))
cat("ALHAMDULILLAH PROJECT COMPLETED\n")
