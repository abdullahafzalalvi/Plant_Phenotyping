####Feature Extraction & Exploratory Data Analysis
####Load packages 
library(magick)
library(EBImage)
library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)
library(viridis)
library(scales)
library(RColorBrewer)

#Loading Phase 2 Environment
ROOT     <- "C:/Users/Hp Computer/Desktop/Plant_Phenotyping_Project"
PROC_DIR <- file.path(ROOT, "data", "processed")
FIGURES_DIR <- file.path(ROOT, "figures")
OUTPUTS_DIR <- file.path(ROOT, "outputs")

load(file.path(PROC_DIR, "phase2_environment.RData"))

cat(sprintf("added %d image paths from Phase 2\n\n", nrow(all_images)))

#### FEATURE EXTRACTION FUNCTION

extract_features <- function(image_path, img_size = 128) {
  
  tryCatch({
    
    img_magick <- image_read(image_path) %>%
      image_resize(paste0(img_size, "x", img_size, "!")) %>%
      image_convert(colorspace = "sRGB")
    raw   <- as.integer(image_data(img_magick, channels = "rgb"))
    total <- length(raw) / 3
    
    r_ch  <- raw[seq(1, length(raw), 3)] / 255
    g_ch  <- raw[seq(2, length(raw), 3)] / 255
    b_ch  <- raw[seq(3, length(raw), 3)] / 255
    mean_r <- mean(r_ch);  mean_g <- mean(g_ch);  mean_b <- mean(b_ch)
    sd_r   <- sd(r_ch);    sd_g   <- sd(g_ch);    sd_b   <- sd(b_ch)
    green_red_ratio  <- mean_g / (mean_r + 1e-6)
    green_blue_ratio <- mean_g / (mean_b + 1e-6)
    exg              <- 2 * mean_g - mean_r - mean_b  
    
    
    leaf_mask <- (g_ch > r_ch * 0.85) & (g_ch > b_ch * 0.85)
    leaf_area <- sum(leaf_mask) / total
    img_eb <- Image(
      array(c(r_ch, g_ch, b_ch), dim = c(img_size, img_size, 3)),
      colormode = "Color"
    )
    img_gray     <- channel(img_eb, "gray")
    lesion_mask  <- img_gray < 0.38
    lesion_label <- bwlabel(lesion_mask)
    lesion_props <- computeFeatures.shape(lesion_label)
    if (!is.null(lesion_props) && nrow(lesion_props) > 0) {
      valid        <- lesion_props[lesion_props[, "s.area"] >= 10, , drop = FALSE]
      lesion_count     <- nrow(valid)
      mean_lesion_size <- if (lesion_count > 0) mean(valid[, "s.area"]) else 0
      max_lesion_size  <- if (lesion_count > 0) max(valid[, "s.area"])  else 0
    } else {
      lesion_count <- 0; mean_lesion_size <- 0; max_lesion_size <- 0
    }
    
    texture_var <- var(as.numeric(img_gray))
    brks  <- c(0, 0.25, 0.5, 0.75, 1.0)
    r_h   <- hist(r_ch, breaks = brks, plot = FALSE)$counts / total
    g_h   <- hist(g_ch, breaks = brks, plot = FALSE)$counts / total
    b_h   <- hist(b_ch, breaks = brks, plot = FALSE)$counts / total    
    c(
      mean_r = mean_r, mean_g = mean_g, mean_b = mean_b,
      sd_r   = sd_r,   sd_g   = sd_g,   sd_b   = sd_b,
      green_red_ratio  = green_red_ratio,
      green_blue_ratio = green_blue_ratio,
      exg              = exg,
      leaf_area        = leaf_area,
      lesion_count     = lesion_count,
      mean_lesion_size = mean_lesion_size,
      max_lesion_size  = max_lesion_size,
      texture_var      = texture_var,
      r_h1 = r_h[1], r_h2 = r_h[2], r_h3 = r_h[3], r_h4 = r_h[4],
      g_h1 = g_h[1], g_h2 = g_h[2], g_h3 = g_h[3], g_h4 = g_h[4],
      b_h1 = b_h[1], b_h2 = b_h[2], b_h3 = b_h[3], b_h4 = b_h[4]
    )
  }, error = function(e) {
    NULL  
  })
}    

#### RUN EXTRACTION ON ALL IMAGES


cat("Running feature extraction\n")
cat(sprintf("   Total images to process: %d\n", nrow(all_images)))
cat("   Expected time: 20–40 minutes\n")
cat("   Progress shown every 100 images\n\n")
feature_list <- vector("list", nrow(all_images))
failed       <- 0
start_time   <- Sys.time()

for (i in seq_len(nrow(all_images))) {
  
  feat <- extract_features(all_images$path[i])
  
  if (is.null(feat)) {
    failed <- failed + 1
  } else {
    feature_list[[i]] <- feat
  }
  if (i %% 100 == 0 || i == nrow(all_images)) {
    elapsed   <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    remaining <- if (i > 0) (elapsed / i) * (nrow(all_images) - i) else 0
    cat(sprintf("   [%4d / %4d]  %.1f min elapsed | ~%.1f min remaining | failed: %d\n",
                i, nrow(all_images), elapsed, remaining, failed))
  }
}

valid_idx <- !sapply(feature_list, is.null)
feat_matrix <- do.call(rbind, lapply(feature_list[valid_idx], function(x) {
  as.data.frame(t(x))
}))
rownames(feat_matrix) <- NULL
features_df <- data.frame(
  label = all_images$label[valid_idx],
  class = all_images$class[valid_idx],
  feat_matrix,
  stringsAsFactors = FALSE
)

features_df$label <- factor(features_df$label,
                            levels = c("healthy", "diseased"))
cat(sprintf("✅ features_df shape: %d rows x %d columns\n",
            nrow(features_df), ncol(features_df)))
cat("   First 5 column names:", paste(names(features_df)[1:5], collapse = ", "), "\n")
cat("   Last 5 column names:",  paste(tail(names(features_df), 5), collapse = ", "), "\n")
cat(sprintf("\n✅ Extraction complete: %d succeeded | %d failed\n\n",
            nrow(features_df), failed))


feat_csv <- file.path(PROC_DIR, "extracted_features.csv")
write.csv(features_df, feat_csv, row.names = FALSE)
cat(sprintf("eatures saved → data/processed/extracted_features.csv\n\n"))


####EDA PLOTS
cat("Generating EDA plots\n")
CLASS_COLORS <- c("healthy" = "#2E8B57", "diseased" = "#CC2936")
theme_plant <- theme_bw(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(color = "gray40", size = 10),
    axis.title       = element_text(face = "bold"),
    legend.position  = "bottom",
    panel.grid.minor = element_blank()
  )
rgb_long <- features_df %>%
  select(label, mean_r, mean_g, mean_b) %>%
  pivot_longer(cols = c(mean_r, mean_g, mean_b),
               names_to  = "channel",
               values_to = "intensity") %>%
  mutate(channel = recode(channel,
                          "mean_r" = "Red channel",
                          "mean_g" = "Green channel",
                          "mean_b" = "Blue channel"))
p2 <- ggplot(rgb_long, aes(x = intensity, fill = label)) +
  geom_density(alpha = 0.65, color = NA) +
  facet_wrap(~ channel, ncol = 3) +
  scale_fill_manual(values = CLASS_COLORS, name = "Class") +
  labs(
    title    = "Figure 2 — RGB Channel Intensity Distribution",
    subtitle = "Healthy leaves show higher green channel; diseased show elevated red",
    x = "Mean intensity (0–1)", y = "Density"
  ) + theme_plant

ggsave(file.path(FIGURES_DIR, "fig02_rgb_distribution.png"),
       p2, width = 10, height = 4.5, dpi = 300)
cat("fig02_rgb_distribution.png saved\n")

#### Figure 3: Lesion Count Histogram

p3 <- ggplot(features_df, aes(x = lesion_count + 1, fill = label)) +
  geom_histogram(bins = 35, alpha = 0.75,
                 position = "identity", color = "white") +
  scale_x_log10(labels = label_comma()) +
  scale_fill_manual(values = CLASS_COLORS, name = "Class") +
  labs(
    title    = "Figure 3 — Lesion Count Distribution",
    subtitle = "Diseased leaves contain significantly more lesion regions",
    x = "Lesion count (log scale)", y = "Number of images"
  ) + theme_plant

ggsave(file.path(FIGURES_DIR, "fig03_lesion_count.png"),
       p3, width = 7, height = 4.5, dpi = 300)
print(p3)

cat(" fig03_lesion_count.png saved\n")

####Figure 4: ExG vs Texture Variance-----
p4 <- ggplot(features_df,
             aes(x = exg, y = texture_var, color = label)) +
  geom_point(alpha = 0.35, size = 1.2) +
  geom_smooth(method = "lm", se = TRUE, linewidth = 1.2) +
  scale_color_manual(values = CLASS_COLORS, name = "Class") +
  labs(
    title    = "Figure 4 — Excess Green Index vs. Texture Variance",
    subtitle = "Healthy leaves cluster at high ExG; diseased at high texture variance",
    x = "Excess Green Index (ExG = 2G − R − B)",
    y = "Texture variance (grayscale)"
  ) + theme_plant

ggsave(file.path(FIGURES_DIR, "fig04_exg_vs_texture.png"),
       p4, width = 7, height = 5, dpi = 300)
print(p4)
cat("fig04_exg_vs_texture.png saved\n")
####Figure 5: Boxplot — Green-Red Ratio--------

p5 <- ggplot(features_df,
             aes(x = label, y = green_red_ratio, fill = label)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8,
               outlier.alpha = 0.4, width = 0.5) +
  scale_fill_manual(values = CLASS_COLORS) +
  labs(
    title    = "Figure 5 — Green-Red Ratio by Class",
    subtitle = "Key discriminating feature: healthy leaves have higher G/R ratio",
    x = "Class", y = "Green / Red ratio"
  ) +
  theme_plant +
  theme(legend.position = "none")

ggsave(file.path(FIGURES_DIR, "fig05_green_red_ratio.png"),
       p5, width = 5, height = 5, dpi = 300)
print(p5)
cat("ig05_green_red_ratio.png saved\n")
#### Figure 6: Correlation Heatmap----

key_feats <- features_df %>%
  select(mean_r, mean_g, mean_b, exg,
         leaf_area, lesion_count,
         mean_lesion_size, texture_var,
         green_red_ratio) %>%
  rename(
    Red            = mean_r,
    Green          = mean_g,
    Blue           = mean_b,
    ExG            = exg,
    `Leaf area`    = leaf_area,
    `Lesion count` = lesion_count,
    `Lesion size`  = mean_lesion_size,
    `Texture var`  = texture_var,
    `G/R ratio`    = green_red_ratio
  )
cor_mat <- cor(key_feats, use = "pairwise.complete.obs")
cor_df  <- as.data.frame(as.table(cor_mat))
names(cor_df) <- c("Var1", "Var2", "Correlation")

p6 <- ggplot(cor_df, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", linewidth = 0.6) +
  geom_text(aes(label = sprintf("%.2f", Correlation)),
            size = 3, fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#1a9850",
    midpoint = 0, limits = c(-1, 1), name = "r"
  ) +
  labs(
    title    = "Figure 6 — Feature Correlation Matrix",
    subtitle = "Pearson r among 9 key extracted features",
    x = "", y = ""
  ) +
  theme_plant +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(FIGURES_DIR, "fig06_correlation_heatmap.png"),
       p6, width = 8, height = 7, dpi = 300)
print(p6)
cat("ig06_correlation_heatmap.png saved\n")

####Figure 7: Mean RGB per Disease Class----


class_rgb <- features_df %>%
  mutate(class_short = gsub("Tomato___", "", class)) %>%
  group_by(class_short, label) %>%
  summarise(R = mean(mean_r),
            G = mean(mean_g),
            B = mean(mean_b),
            .groups = "drop") %>%
  pivot_longer(cols = c(R, G, B),
               names_to = "channel", values_to = "value")

p7 <- ggplot(class_rgb,
             aes(x = reorder(class_short, value),
                 y = value, fill = channel)) +
  geom_col(position = "dodge", width = 0.7) +
  coord_flip() +
  scale_fill_manual(
    values = c(R = "#E74C3C", G = "#2ECC71", B = "#3498DB"),
    name = "Channel") +
  labs(
    title    = "Figure 7 — Mean RGB Values per Disease Class",
    subtitle = "Shows how different diseases alter leaf color composition",
    x = "Tomato class", y = "Mean channel intensity (0–1)"
  ) + theme_plant

ggsave(file.path(FIGURES_DIR, "fig07_rgb_per_class.png"),
       p7, width = 10, height = 6, dpi = 300)
print(p7)
cat("fig07_rgb_per_class.png saved\n")

write.csv(features_df,
          file.path(PROC_DIR, "extracted_features.csv"),
          row.names = FALSE)
cat("✅ extracted_features.csv saved\n")

save(
  ROOT, PROC_DIR, FIGURES_DIR, OUTPUTS_DIR, MODELS_DIR,
  all_images, features_df,
  file = file.path(PROC_DIR, "phase3_environment.RData")
)
cat("✅ Phase 4 environment saved\n")


