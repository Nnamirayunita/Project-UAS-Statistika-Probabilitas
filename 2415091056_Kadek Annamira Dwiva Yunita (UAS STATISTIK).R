# Nama  : [Kadek Annamira Dwiva Yunita]
# NIM   : [2415091056]
# Kelas : [1 DPS]

# 1. Instal dan Muat Paket yang Diperlukan
install.packages("readxl")  # Untuk membaca file Excel
install.packages("car")     # Untuk uji asumsi regresi
install.packages("ggplot2") # Untuk visualisasi
install.packages("dplyr")   # Untuk manipulasi data
install.packages("lmtest")  # Untuk uji homoskedastisitas

library(readxl)
library(car)
library(ggplot2)
library(dplyr)
library(lmtest)

# 2. Memuat Data
file_path <- "C:/UNDIKSHA ADEK/Data Performa Siswa 2.xlsx"  # Ganti dengan path file Anda
data <- read_excel(file_path)

# 3. Membersihkan Data
# Menghapus kolom yang tidak relevan dan mengubah nama kolom
cleaned_data <- data %>%
  select(-`Nama Siswa`) %>%  # Kolom ini dihapus karena tidak digunakan
  rename(
    Kehadiran = `Kehadiran dalam kelas (%)`,
    Kepahaman = `Tingkat Kepahaman dalam Pembelajaran`,
    Kenyamanan = `Tingkat kenyaman dalam kelas`,
    Jam_Tidur = `Rata-rata jam tidur / hari`,
    Kepercayaan = `Tingkat Kepercayaan Diri`,
    Kejelasan = `Kejelasan arah karir (%)`
  )

# Menghapus baris dengan nilai kosong (NA)
cleaned_data <- na.omit(cleaned_data)

# Pastikan semua kolom numerik terkonversi dengan benar
cleaned_data <- cleaned_data %>%
  mutate(
    Kehadiran = as.numeric(Kehadiran),
    Kepahaman = as.numeric(Kepahaman),
    Kenyamanan = as.numeric(Kenyamanan),
    Jam_Tidur = as.numeric(Jam_Tidur),
    Kepercayaan = as.numeric(Kepercayaan),
    Kejelasan = as.numeric(Kejelasan)
  )

# Verifikasi keberadaan kolom yang relevan
if (!all(c("Kepahaman", "Kehadiran", "Kepercayaan", "Jam_Tidur", "Kejelasan") %in% colnames(cleaned_data))) {
  stop("Kolom yang diperlukan untuk analisis tidak ditemukan dalam cleaned_data. Periksa proses pembersihan dan nama kolom.")
}

# 4. Membuat Model Regresi Linear Berganda
model <- lm(Kepahaman ~ Kehadiran + Kepercayaan + Jam_Tidur + Kejelasan, data = cleaned_data)

# Tampilkan hasil ringkasan model
cat("\n### Ringkasan Model Regresi ###\n")
print(summary(model))

# 5. Uji Asumsi
cat("\n### Uji Asumsi ###\n")

## a. Uji Normalitas Residual
shapiro_test <- shapiro.test(residuals(model))
cat("Uji Normalitas (Shapiro-Wilk):\n")
print(shapiro_test)

## b. Uji Homoskedastisitas
ncv_test <- ncvTest(model)
cat("\nUji Homoskedastisitas (Breusch-Pagan):\n")
print(ncv_test)

## c. Uji Multikolinearitas
vif_values <- vif(model)
cat("\nUji Multikolinearitas (VIF):\n")
print(vif_values)

## d. Uji Linearitas (Visualisasi Komponen + Residual)
cat("\nPlot Komponen + Residual (Linearitas):\n")
cr_plots <- crPlots(model)

# 6. Visualisasi
cat("\n### Visualisasi ###\n")

## a. Plot Residual vs Fitted
plot(model, which = 1, main = "Residuals vs Fitted Values")

## b. Histogram Residuals
hist(residuals(model), main = "Histogram Residuals", xlab = "Residuals", col = "lightblue", border = "black")

## c. Scatterplot Matrix untuk Variabel
pairs(~ Kepahaman + Kehadiran + Kepercayaan + Jam_Tidur + Kejelasan, 
      data = cleaned_data, main = "Scatterplot Matrix")

# 7. Interpretasi
cat("\n### Interpretasi ###\n")
cat("1. Perhatikan nilai p-value untuk setiap variabel dalam summary(model) untuk mengevaluasi signifikansi.\n")
cat("2. Evaluasi hasil uji asumsi untuk memastikan model valid:\n")
cat("   - Normalitas: P-value Shapiro-Wilk > 0.05 menunjukkan residual normal.\n")
cat("   - Homoskedastisitas: P-value Breusch-Pagan > 0.05 menunjukkan varians residual homogen.\n")
cat("   - Multikolinearitas: Semua nilai VIF < 10 menunjukkan tidak ada masalah multikolinearitas.\n")
cat("   - Linearitas: Plot komponen + residual seharusnya menunjukkan hubungan linear.\n")

