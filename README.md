# Plant_Phenotyping
Plant_Phenotyping_Project
# **Plant Phenotyping Image Analysis in R**

## **Project Overview**

This project presents a complete plant phenotyping and disease detection workflow developed in **R** using image processing and machine learning techniques. The system analyzes tomato leaf images from the PlantVillage dataset and classifies samples into two categories:

* **Healthy**  
* **Diseased**

The project demonstrates how computational biology, image analysis, and data science can be integrated to support modern agriculture and plant disease diagnostics.

---

## **Project Objectives**

The major objectives of this project were:

* To automatically read plant leaf image folders in R  
* To preprocess and organize image datasets  
* To extract useful phenotypic features from tomato leaves  
* To classify leaf health status using machine learning  
* To evaluate model performance using statistical metrics  
* To create a professional and reproducible R-based workflow

---

## **Dataset Information**

### **Dataset Used**

**PlantVillage Dataset**

### **Crop Selected**

**Tomato**

### **Classes Used**

* `Tomato___healthy` → Healthy  
* All diseased tomato classes → Diseased

### **Image Specifications**

* File Format: JPG / JPEG  
* Color Space: RGB  
* Background: Uniform plain background  
* Resolution: Standardized leaf image samples  
* Orientation: Single centered leaf

---

## **Technologies Used**

### **Programming Language**

* R

### **Development Environment**

* RStudio

### **Main R Packages**

* EBImage  
* magick  
* tidyverse  
* caret  
* randomForest  
* ggplot2  
* fs

---

## **Project Folder Structure**

Plant\_Phenotyping\_Project/  
│── README.md  
│── run\_project.R  
├── data/  
│   ├── raw/  
│   └── processed/  
├── scripts/  
├── figures/  
├── outputs/  
└── models/

---

## **Methodology**

### **1\. Data Import**

The image dataset was automatically loaded from the project folders using R file handling functions.

### **2\. Label Assignment**

Folder names were converted into binary labels:

* Healthy  
* Diseased

### **3\. Image Feature Extraction**

The following image-based biological features were extracted:

* Leaf Area  
* Mean Red Intensity  
* Mean Green Intensity  
* Mean Blue Intensity  
* Lesion / Spot Count  
* Dark Region Ratio  
* Color Variation Statistics

### **4\. Data Preprocessing**

* Missing value handling  
* Feature dataset construction  
* Train-test split  
* Binary class preparation

### **5\. Machine Learning Classification**

A machine learning model was trained to classify healthy and diseased tomato leaves based on extracted features.

### **6\. Performance Evaluation**

The model was evaluated using:

* Accuracy  
* Confusion Matrix  
* Precision  
* Recall  
* Feature Importance

---

## **Results**

The developed system successfully classified tomato leaf health status with strong predictive performance.

### **Important Findings**

* Lesion count strongly indicated disease presence  
* Green channel intensity was important for healthy leaf detection  
* Dark spot ratio improved disease recognition  
* Feature-based machine learning performed efficiently

---

## **Output Files**

### **Processed Data**

* `features_dataset.csv`  
* `metadata.csv`

### **Figures**

* `confusion_matrix.png`  
* `feature_importance.png`

### **Model Files**

* `random_forest_model.rds`

### **Additional Outputs**

* Prediction results  
* Performance summaries

---

## **How to Run the Project**

Open the project in **RStudio** and run:

source("run\_project.R")

This executes the complete workflow including data loading, feature extraction, model training, and evaluation.

---

## **Scientific Importance**

This project demonstrates the application of biotechnology and computational tools in:

* Early plant disease diagnosis  
* Precision agriculture  
* Smart farming systems  
* Crop health monitoring  
* Automated phenotyping platforms

---

## **Future Improvements**

Potential future enhancements include:

* Deep learning using CNN models  
* Multi-class disease classification  
* Mobile disease detection application  
* Real-time greenhouse monitoring  
* Cross-species disease prediction

---

## **Citation**

Hughes, D. P., & Salathé, M. (2015).  
*An open access repository of images on plant health to enable the development of mobile disease diagnostics.*

---

## **Authors**

* Muzzamil Hussain  
* Abdullah Afzal Alvi  
* Sadia Noureen

---

## **Submission Status**

Completed and ready for academic submission.

