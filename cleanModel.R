# Load library
options(warn = -1)
library(xgboost)
library(caret)
library(mltools)
library(ggplot2)
library(data.table)

# Load data
data_properti <- read.table("Copy of Merge Data Properti - Sheet1.csv", header = TRUE, sep = ",")

# Fungsi preprocessing
replace_spaces <- function(column) {
  if (is.character(column)) return(gsub(" ", "_", column)) else return(column)
}

convert_to_upper <- function(column) {
  if (is.character(column)) return(toupper(column)) else return(column)
}

# Terapkan preprocessing
data_properti <- as.data.frame(lapply(data_properti, function(column) {
  column <- replace_spaces(column)
  column <- convert_to_upper(column)
  return(column)
}))

# Hapus kolom yang tidak diperlukan
data_properti <- subset(data_properti, select = -c(No, Lokasi, Kecamatan))

# Konversi kolom numerik
numeric_columns <- c("Harga", "Luas.Tanah", "Luas.Bangunan", "Lebar.Jalan",
                     "Jumlah.Lantai", "Jumlah.KT", "Jumlah.KM", "Daya.Listrik")
data_properti[numeric_columns] <- lapply(data_properti[numeric_columns], function(col) {
  as.numeric(gsub("[^0-9]", "", col))
})

# Konversi kolom kategorik menjadi faktor
categorical_columns <- c("Tipe.Hunian", "Kondisi", "Kepemilikan", "Garasi", "Rooftop",
                         "Sumber.Air", "Tipe.Furnish")
data_properti[categorical_columns] <- lapply(data_properti[categorical_columns], as.factor)

# One-hot encoding
data_properti <- one_hot(as.data.table(data_properti))

# Hapus kolom redundan
if ("Tipe.Hunian_APARTEMENTT" %in% colnames(data_properti)) {
  data_properti$Tipe.Hunian_APARTEMENT <- data_properti$Tipe.Hunian_APARTEMENT +
    data_properti$Tipe.Hunian_APARTEMENTT
  data_properti <- subset(data_properti, select = -Tipe.Hunian_APARTEMENTT)
}

# Hapus NA
data_properti <- na.omit(data_properti)

# Split data menjadi train dan test
set.seed(123)
indices <- sample(1:nrow(data_properti), 0.9 * nrow(data_properti))
train_data <- data_properti[indices, ]
test_data <- data_properti[-indices, ]

# Variabel target dan fitur
y_col <- "Harga"
X_cols <- setdiff(names(data_properti), y_col)

# Konversi data untuk XGBoost
X_train <- as.matrix(train_data[, ..X_cols])
y_train <- train_data[[y_col]]
X_test <- as.matrix(test_data[, ..X_cols])
y_test <- test_data[[y_col]]

# Simulasi data tambahan (test2)
set.seed(456)
test_data2 <- data_properti[sample(1:nrow(data_properti), 30), ]
X_test2 <- as.matrix(test_data2[, ..X_cols])
y_test2 <- test_data2[[y_col]]

# Ensemble XGBoost menggunakan loop
params_list <- list(
  list(objective = "reg:squarederror", max_depth = 8, eta = 0.1, eval_metric = "rmse"),
  list(objective = "reg:squarederror", max_depth = 10, eta = 0.05, eval_metric = "rmse"),
  list(objective = "reg:squarederror", max_depth = 12, eta = 0.01, eval_metric = "rmse")
)

# Buat model menggunakan loop
predictions_train <- matrix(0, nrow = nrow(X_train), ncol = length(params_list))
predictions_test <- matrix(0, nrow = nrow(X_test), ncol = length(params_list))
predictions_test2 <- matrix(0, nrow = nrow(X_test2), ncol = length(params_list))

for (i in seq_along(params_list)) {
  params <- params_list[[i]]
  dtrain <- xgb.DMatrix(data = X_train, label = y_train)
  model <- xgb.train(params = params, data = dtrain, nrounds = 100, verbose = 0)
  
  # Simpan prediksi
  predictions_train[, i] <- predict(model, X_train)
  predictions_test[, i] <- predict(model, X_test)
  predictions_test2[, i] <- predict(model, X_test2)
}

# Hitung rata-rata ensemble
ensemble_train <- rowMeans(predictions_train)
ensemble_test <- rowMeans(predictions_test)
ensemble_test2 <- rowMeans(predictions_test2)

# Evaluasi model
rmse_train <- sqrt(mean((ensemble_train - y_train)^2))
rmse_test <- sqrt(mean((ensemble_test - y_test)^2))
rmse_test2 <- sqrt(mean((ensemble_test2 - y_test2)^2))

cat("RMSE Train:", rmse_train, "\n")
cat("RMSE Test:", rmse_test, "\n")
cat("RMSE Test2:", rmse_test2, "\n")

# Visualisasi hasil prediksi vs aktual
comparison_train <- data.frame(Actual = y_train, Predicted = ensemble_train)
comparison_test <- data.frame(Actual = y_test, Predicted = ensemble_test)
comparison_test2 <- data.frame(Actual = y_test2, Predicted = ensemble_test2)

plot_train <- ggplot(comparison_train, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Prediksi vs Aktual (Train)", x = "Aktual", y = "Prediksi") +
  theme_minimal()

plot_test <- ggplot(comparison_test, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Prediksi vs Aktual (Test)", x = "Aktual", y = "Prediksi") +
  theme_minimal()

plot_test2 <- ggplot(comparison_test2, aes(x = Actual, y = Predicted)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Prediksi vs Aktual (Test2)", x = "Aktual", y = "Prediksi") +
  theme_minimal()

# Simpan plot
ggsave("train_comparison_plot.png", plot = plot_train, width = 10, height = 6)
ggsave("test_comparison_plot.png", plot = plot_test, width = 10, height = 6)
ggsave("test2_comparison_plot.png", plot = plot_test2, width = 10, height = 6)

print(plot_train)
print(plot_test)
print(plot_test2)
