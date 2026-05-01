#### Data Loading & Path Verification
#### scripts/script_02_data_loading.R
library(magick)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

cat("Packages loaded\n\n")

ROOT      <- "C:/Users/Hp Computer/Desktop/Plant_Phenotyping_Project"

RAW_DIR     <- file.path(ROOT, "data", "raw")
PROC_DIR    <- file.path(ROOT, "data", "processed")
FIGURES_DIR <- file.path(ROOT, "figures")
OUTPUTS_DIR <- file.path(ROOT, "outputs")
MODELS_DIR  <- file.path(ROOT, "models")
SCRIPTS_DIR <- file.path(ROOT, "scripts")

dir.create(PROC_DIR,    showWarnings = FALSE, recursive = TRUE)
dir.create(FIGURES_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUTPUTS_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(MODELS_DIR,  showWarnings = FALSE, recursive = TRUE)

cat("ll project paths set\n\n")

#### ERIFY RAW DATA FOLDERS
cat(" Checking raw image folders ──\n\n")

TOMATO_FOLDERS <- c(
  "Tomato___healthy",
  "Tomato___Bacterial_spot",
  "Tomato___Early_blight",
  "Tomato___Late_blight",
  "Tomato___Leaf_Mold",
  "Tomato___Septoria_leaf_spot",
  "Tomato___Spider_mites Two-spotted_spider_mite",
  "Tomato___Target_Spot",
  "Tomato___Tomato_mosaic_virus",
  "Tomato___Tomato_Yellow_Leaf_Curl_Virus"
)

folder_summary <- data.frame(
  folder    = character(),
  label     = character(),
  n_images  = integer(),
  exists    = logical(),
  stringsAsFactors = FALSE
)
for (folder in TOMATO_FOLDERS) {
  
  full_path <- file.path(RAW_DIR, folder)
  folder_exists <- dir.exists(full_path)
  if (folder_exists) {
    imgs <- list.files(
      full_path,
      pattern    = "\\.(jpg|JPG|jpeg|JPEG|png|PNG)$",
      full.names = FALSE
    )
    n <- length(imgs)
  } else {
    n <- 0
  }
  # Assign binary label

  
  lbl <- ifelse(folder == "Tomato___healthy", "healthy", "diseased")
  
  folder_summary <- rbind(folder_summary, data.frame(
    folder   = folder,
    label    = lbl,
    n_images = n,
    exists   = folder_exists,
    stringsAsFactors = FALSE
  )) 
  status <- ifelse(folder_exists, "✅", "❌ NOT FOUND")
  cat(sprintf("  %s  %-50s  %d images\n", status, folder, n))
}
  cat("\n")

  #### DATASET SUMMARY

cat("Dataset Summary:\n")  
total_images   <- sum(folder_summary$n_images)
healthy_images <- sum(folder_summary$n_images[folder_summary$label == "healthy"])
diseased_images<- sum(folder_summary$n_images[folder_summary$label == "diseased"])
cat(sprintf("  Total images found : %d\n",   total_images))
cat(sprintf("  Healthy images     : %d\n",   healthy_images))
cat(sprintf("  Diseased images    : %d\n",   diseased_images))
cat(sprintf("  Disease folders    : %d\n",   sum(folder_summary$label == "diseased")))
cat(sprintf("  Missing folders    : %d\n\n", sum(!folder_summary$exists)))

#### Collecting images

cat("Collecting image paths and labels...\n")
N_HEALTHY_MAX  <- 1500
N_DISEASE_MAX  <- 200
set.seed(42)

all_images <- data.frame(
  path  = character(),
  label = character(),
  class = character(),
  stringsAsFactors = FALSE
)

for (i in seq_len(nrow(folder_summary))) {
  
  row <- folder_summary[i, ]
  
  if (!row$exists || row$n_images == 0) next
  
  full_path <- file.path(RAW_DIR, row$folder)
  imgs <- list.files(full_path,
                     pattern   = "\\.(jpg|JPG|jpeg|JPEG|png|PNG)$",
                     full.names = TRUE)
  cap <- ifelse(row$label == "healthy", N_HEALTHY_MAX, N_DISEASE_MAX)
  sampled <- sample(imgs, min(cap, length(imgs)))
  
  chunk <- data.frame(
    path  = sampled,
    label = row$label,
    class = row$folder,
    stringsAsFactors = FALSE
  )
  all_images <- rbind(all_images, chunk)
  cat(sprintf("  Collected %4d images from: %s\n", nrow(chunk), row$folder))
}

all_images <- all_images[sample(nrow(all_images)), ]
all_images$label <- factor(all_images$label, levels = c("healthy", "diseased"))
rownames(all_images) <- NULL

cat(sprintf("\n  Total collected : %d images\n", nrow(all_images)))
cat(sprintf("  Healthy         : %d\n", sum(all_images$label == "healthy")))
cat(sprintf("  Diseased        : %d\n", sum(all_images$label == "diseased")))

##### TEST — LOAD ONE IMAGE & SHOW INFO
cat("\nTesting image loading:\n")

test_path <- all_images$path[1]
cat(sprintf("  Loading test image: %s\n", basename(test_path)))
test_img  <- image_read(test_path)
test_info <- image_info(test_img)


cat(sprintf("  ✅ Format     : %s\n",       test_info$format))
cat(sprintf("  ✅ Dimensions : %d x %d px\n", test_info$width, test_info$height))
cat(sprintf("  ✅ File size  : %s\n",       test_info$filesize))
cat(sprintf("  ✅ Label      : %s\n",       as.character(all_images$label[1])))



####  SAVE IMAGE PATH TABLE
save_path <- file.path(PROC_DIR, "image_paths.csv")
write.csv(all_images, save_path, row.names = FALSE)


cat(sprintf("\n  ✅ Image path table saved to:\n"))
cat(sprintf("     %s\n", save_path))
#### CLASS DISTRIBUTION PLOT

cat("\nPlotting class distribution:\n")

class_counts <- all_images %>%
  group_by(class, label) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(class_short = gsub("Tomato___", "", class))
CLASS_COLORS <- c("healthy" = "#2E8B57", "diseased" = "#CC2936")

p_dist <- ggplot(class_counts,
                 aes(x = reorder(class_short, n),
                     y = n,
                     fill = label)) +
  geom_col(width = 0.7, color = "white") +
  geom_text(aes(label = n),
            hjust = -0.2, size = 3.5, fontface = "bold") +
  coord_flip() +
  scale_fill_manual(values = CLASS_COLORS, name = "Class") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  labs(
    title    = "Figure 1 — Image Count per Tomato Class",
    subtitle = "PlantVillage Dataset | Binary: Healthy vs. Diseased",
    x        = "Tomato disease class",
    y        = "Number of images sampled"
  ) +
  theme_bw(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(color = "gray40", size = 10),
    axis.title    = element_text(face = "bold"),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )
print(p_dist)
fig1_path <- file.path(FIGURES_DIR, "fig01_class_distribution.png")
ggsave(fig1_path, p_dist, width = 9, height = 5.5, dpi = 300)

cat(sprintf(" igure 1 saved: figures/fig01_class_distribution.png\n"))

#### Saving Paths 
save(
  ROOT, RAW_DIR, PROC_DIR, FIGURES_DIR,
  OUTPUTS_DIR, MODELS_DIR, all_images,
  file = file.path(PROC_DIR, "phase2_environment.RData")
)

cat(" phase 2 complete\n")
