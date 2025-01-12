options(warn=-1)
library(xgboost)
library(caret)
library(mltools)
library(ggplot2)
library(data.table)

# Input data dari Merge Data Properti - Sheet1
data_properti <- read.table("Copy of Merge Data Properti - Sheet1.csv", header = TRUE, sep=",")

# Cek tipe data
str(data_properti)

# Mendefinisikan fungsi untuk mengganti spasi dengan '_'
replace_spaces <- function(column) {
  if (is.character(column)) {
    return(gsub(" ", "_", column))
  } else {
    return(column)
  }
}

# Mendefinisikan fungsi untuk mengubah huruf besar jika kolom berisi teks
convert_to_upper <- function(column) {
  if (is.character(column)) {
    return(toupper(column))
  } else {
    return(column)
  }
}

# Menggunakan apply untuk menerapkan fungsi pada setiap kolom
data_properti <- as.data.frame(lapply(data_properti, convert_to_upper))
data_properti <- as.data.frame(lapply(data_properti, replace_spaces))

# Praproses (Drop kolom yang tidak diperlukan)
data_properti <- subset(data_properti, select = -No)
data_properti <- subset(data_properti, select = -Lokasi)
data_properti <- subset(data_properti, select = -Kecamatan)
data_properti$Harga <- as.numeric(gsub("[^0-9]", "", data_properti$Harga))
data_properti$Luas.Tanah <- as.numeric(data_properti$Luas.Tanah)
data_properti$Luas.Bangunan <- as.numeric(data_properti$Luas.Bangunan)
data_properti$Lebar.Jalan <- as.numeric(data_properti$Lebar.Jalan)
data_properti$Jumlah.Lantai <- as.numeric(data_properti$Jumlah.Lantai)
data_properti$Jumlah.KT <- as.numeric(data_properti$Jumlah.KT)
data_properti$Jumlah.KM <- as.numeric(data_properti$Jumlah.KM)
data_properti$Daya.Listrik <- as.numeric(data_properti$Daya.Listrik)
data_properti$Tipe.Hunian <- as.factor(data_properti$Tipe.Hunian)
data_properti$Kondisi <- as.factor(data_properti$Kondisi)
data_properti$Kepemilikan <- as.factor(data_properti$Kepemilikan)
data_properti$Garasi <- as.factor(data_properti$Garasi)
data_properti$Rooftop <- as.factor(data_properti$Rooftop)
data_properti$Sumber.Air <- as.factor(data_properti$Sumber.Air)
data_properti$Tipe.Furnish <- as.factor(data_properti$Tipe.Furnish)
data_properti <- one_hot(as.data.table(data_properti))
data_properti_encoded <- na.omit(data_properti)
data_properti_encoded$Tipe.Hunian_APARTEMENT <- data_properti_encoded$Tipe.Hunian_APARTEMENT + data_properti_encoded$Tipe.Hunian_APARTEMENTT
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Hunian_APARTEMENTT)
data_properti_encoded$Rooftop_TIDAK <- data_properti_encoded$Rooftop_TIDAK + data_properti_encoded$Rooftop_TIDAK_
data_properti_encoded <- subset(data_properti_encoded, select = -Rooftop_TIDAK_)
data_properti_encoded$Garasi_TIDAK <- data_properti_encoded$Garasi_TIDAK + data_properti_encoded$Garasi_
data_properti_encoded <- subset(data_properti_encoded, select = -Garasi_)
data_properti_encoded$Sumber.Air_PDAM <- data_properti_encoded$Sumber.Air_PAM + data_properti_encoded$Sumber.Air_PAM_TANDOM_TANAM + data_properti_encoded$Sumber.Air_PDAM_MANDIRI + data_properti_encoded$`Sumber.Air_PAM_/_PDAM`
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_PAM_TANDOM_TANAM`)
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_PAM_/_PDAM`)
data_properti_encoded <- subset(data_properti_encoded, select = -Sumber.Air_PDAM_MANDIRI)
# data_properti_encoded <- subset(data_properti_encoded, select = -Sumber.Air_PAM)
data_properti_encoded$Sumber.Air_SUMUR <- data_properti_encoded$Sumber.Air_SUMUR + data_properti_encoded$Sumber.Air_SUMUR_BOR + data_properti_encoded$Sumber.Air_SUMUR_POMPA + data_properti_encoded$Sumber.Air_SUMUR_RESAPAN + data_properti_encoded$Sumber.Air_JET_PUMP
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_SUMUR_BOR`)
data_properti_encoded <- subset(data_properti_encoded, select = -Sumber.Air_JET_PUMP)
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_SUMUR_POMPA`)
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_SUMUR_RESAPAN`)
data_properti_encoded$Sumber.Air_KOMBINASI <- data_properti_encoded$Sumber.Air_KOMBINASI + data_properti_encoded$`Sumber.Air_SUMUR_&_PDAM`
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_SUMUR_&_PDAM`)
data_properti_encoded$Tipe.Furnish_FURNISHED <- data_properti_encoded$Tipe.Furnish_FURNISHED + data_properti_encoded$Tipe.Furnish_FURNISH
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Furnish_FURNISH)
data_properti_encoded$Tipe.Furnish_FURNISHED <- data_properti_encoded$Tipe.Furnish_FURNISHED + data_properti_encoded$Tipe.Furnish_FULL_FURNISH + data_properti_encoded$Tipe.Furnish_FULL_FURNISHED
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Furnish_FULL_FURNISH)
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Furnish_FULL_FURNISHED)
data_properti_encoded$Tipe.Furnish_NON_FURNISHED <- data_properti_encoded$Tipe.Furnish_NON_FURNISHED + data_properti_encoded$Tipe.Furnish_NON_FURNISH
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Furnish_NON_FURNISH)
data_properti_encoded$Tipe.Furnish_SEMI_FURNISHED <- data_properti_encoded$Tipe.Furnish_SEMI_FURNISHED + data_properti_encoded$Tipe.Furnish_SEMI_FURNISH + data_properti_encoded$Tipe.Furnish_SEBAGIAN
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Furnish_SEMI_FURNISH)
data_properti_encoded <- subset(data_properti_encoded, select = -Tipe.Furnish_SEBAGIAN)
data_properti_encoded$Kepemilikan_SHM <- data_properti_encoded$`Kepemilikan_SHM_&_PBG` + data_properti_encoded$Kepemilikan_SHM + data_properti_encoded$Kepemilikan_SMH
data_properti_encoded <- subset(data_properti_encoded, select = -`Kepemilikan_SHM_&_PBG`)
data_properti_encoded <- subset(data_properti_encoded, select = -Kepemilikan_SMH)
data_properti_encoded$Sumber.Air_KOMBINASI <- data_properti_encoded$`Sumber.Air_PDAM_&_PAM` + data_properti_encoded$Sumber.Air_KOMBINASI
data_properti_encoded <- subset(data_properti_encoded, select = -`Sumber.Air_PDAM_&_PAM`)
# str(data_properti_encoded)
# Pilih kolom yang akan menjadi variabel dependen (y) dan independen (X)
set.seed(123)
indices <- sample(1:nrow(data_properti_encoded), 0.9 * nrow(data_properti_encoded))
y_col <- "Harga"
X_cols <- setdiff(names(data_properti_encoded), y_col)

# Pisahkan variabel dependen dan independen untuk train set dan test set
train_data <- data_properti_encoded[indices, ]
test_data <- data_properti_encoded[-indices, ]

# Model Pertama
X_train <- data.table(train_data[, ..X_cols])
y_train <- train_data[, ..y_col]
dtrain <- xgb.DMatrix(as.matrix(X_train), label = as.matrix(y_train))
params <- list(
  objective = "reg:squarederror",
  max_depth = 8,
  eta = 0.9,#0.0076015,#76 ,#9,
  nthread = 2,
  eval_metric = "rmse"
)

# Model Kedua
X_train <- data.table(train_data[, ..X_cols])
y_train <- train_data[, ..y_col]
dtrain <- xgb.DMatrix(as.matrix(X_train), label = as.matrix(y_train))
params2 <- list(
  objective = "reg:squarederror",
  max_depth = 32,
  eta = 0.006015,#428,
  nthread = 59,
  eval_metric = "rmse"
)

# Model Ketiga
dtrain <- xgb.DMatrix(as.matrix(X_train), label = as.matrix(y_train))
params3 <- list(
  objective = "reg:squarederror",
  max_depth = 32,
  eta = 0.006015,#76,
  nthread = 59,
  eval_metric = "rmse"
)

# Model Keempat
dtrain <- xgb.DMatrix(as.matrix(X_train), label = as.matrix(y_train))
params4 <- list(
  objective = "reg:squarederror",
  max_depth = 32,
  eta = 0.006515,#76,
  nthread = 59,
  eval_metric = "rmse"
)

# Gabungkan Prediksi dari Kedua Model
X_test <- data.table(test_data[, ..X_cols])
y_test <- test_data[, ..y_col]

# kfold
# --------------------------------------------------------------------------------
# Lakukan k-fold cross-validation dengan k = 5
set.seed(123)
folds <- createFolds(train_data$Harga, k = 10, list = TRUE, returnTrain = FALSE)

# Inisialisasi vektor untuk menyimpan hasil evaluasi
rmse_values <- numeric(length(folds))

# Iterasi melalui setiap lipatan
for (i in seq_along(folds)) {
  test_indices <- unlist(folds[i])
  train_indices <- setdiff(seq_along(train_data$Harga), test_indices)
  X_train_fold <- data.table(train_data[train_indices, ..X_cols])
  y_train_fold <- train_data[train_indices, ..y_col]
  X_test_fold <- data.table(train_data[test_indices, ..X_cols])
  y_test_fold <- train_data[test_indices, ..y_col]
  
  # Model XGBoost
  dtrain_fold <- xgb.DMatrix(as.matrix(X_train_fold), label = as.matrix(y_train_fold))
  xg_model_fold2 <- xgb.train(data = dtrain_fold, params = params, nrounds = 2, verbose = 10)
  xg_model_fold1 <- xgb.train(data = dtrain_fold, params = params, nrounds = 4, verbose = 10)
  xg_model_fold3 <- xgb.train(data = dtrain_fold, params = params, nrounds = 3, verbose = 10)
  xg_model_fold4 <- xgb.train(data = dtrain_fold, params = params, nrounds = 5, verbose = 10)
  xg_model_fold5 <- xgb.train(data = dtrain_fold, params = params, nrounds = 4, verbose = 10)
  xg_model_fold6 <- xgb.train(data = dtrain_fold, params = params, nrounds = 3, verbose = 10)
  xg_model_fold7 <- xgb.train(data = dtrain_fold, params = params, nrounds = 2, verbose = 10)
  xg_model_fold8 <- xgb.train(data = dtrain_fold, params = params, nrounds = 3, verbose = 10)
  xg_model_fold9 <- xgb.train(data = dtrain_fold, params = params, nrounds = 5, verbose = 10)
  xg_model_fold10 <- xgb.train(data = dtrain_fold, params = params, nrounds = 2, verbose = 10)
  
  # Prediksi menggunakan model
  predictions_fold1 <- predict(xg_model_fold1, as.matrix(X_test_fold))
  predictions_fold2 <- predict(xg_model_fold2, as.matrix(X_test_fold))
  predictions_fold3 <- predict(xg_model_fold3, as.matrix(X_test_fold))
  predictions_fold4 <- predict(xg_model_fold4, as.matrix(X_test_fold))
  predictions_fold5 <- predict(xg_model_fold5, as.matrix(X_test_fold))
  predictions_fold6 <- predict(xg_model_fold6, as.matrix(X_test_fold))
  predictions_fold7 <- predict(xg_model_fold7, as.matrix(X_test_fold))
  predictions_fold8 <- predict(xg_model_fold8, as.matrix(X_test_fold))
  predictions_fold9 <- predict(xg_model_fold9, as.matrix(X_test_fold))
  predictions_fold10 <- predict(xg_model_fold10, as.matrix(X_test_fold))
  
  # Evaluasi model (misalnya, RMSE untuk regresi)
  ensemble_predictions <- (predictions_fold1 + predictions_fold2 + predictions_fold3 + predictions_fold4 +
                             predictions_fold5 + predictions_fold6 + predictions_fold7 + predictions_fold8 +
                             predictions_fold9 + predictions_fold10) / 10
  rmse_values[i] <- sqrt(mean((ensemble_predictions - as.matrix(y_test_fold))^2))
}

# Cetak hasil RMSE tiap iterasi
cat("Hasil RMSE tiap iterasi:\n")
print(rmse_values)

predictions1 <- predict(xg_model_fold1, as.matrix(X_train))
predictions2 <- predict(xg_model_fold2, as.matrix(X_train))
predictions3 <- predict(xg_model_fold3, as.matrix(X_train))
predictions4 <- predict(xg_model_fold4, as.matrix(X_train))
predictions5 <- predict(xg_model_fold5, as.matrix(X_train))
predictions6 <- predict(xg_model_fold6, as.matrix(X_train))
predictions7 <- predict(xg_model_fold7, as.matrix(X_train))
predictions8 <- predict(xg_model_fold8, as.matrix(X_train))
predictions9<- predict(xg_model_fold9, as.matrix(X_train))
predictions10 <- predict(xg_model_fold10, as.matrix(X_train))
ensemble_predictions <- (predictions1 + predictions2 + predictions3 + predictions4 + predictions5 +
                           predictions6 + predictions7 + predictions8 + predictions9 + predictions10) / 10
RMSE(as.matrix(y_train), ensemble_predictions)

# Gabungkan data actual dan prediksi
comparison_data <- data.frame(Actual = as.matrix(y_train), Predicted = ensemble_predictions)

# Plot garis data train
plot_train <- ggplot(comparison_data, aes(x = seq_along(Harga), y = Harga, color = "Actual")) +
  geom_line() +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Perbandingan Harga Rumah Sebenarnya dan Hasil Prediksi Data Train",
       x = "Harga",
       y = "Prediksi") +
  scale_color_manual(values = c("blue", "red"), name = "Legend", labels = c("Actual", "Predicted"))

ggsave("train_plot.png", plot = plot_train, width = 10, height = 6)

predictions1 <- predict(xg_model_fold1, as.matrix(X_test))
predictions2 <- predict(xg_model_fold2, as.matrix(X_test))
predictions3 <- predict(xg_model_fold3, as.matrix(X_test))
predictions4 <- predict(xg_model_fold4, as.matrix(X_test))
predictions5 <- predict(xg_model_fold5, as.matrix(X_test))
predictions6 <- predict(xg_model_fold6, as.matrix(X_test))
predictions7 <- predict(xg_model_fold7, as.matrix(X_test))
predictions8 <- predict(xg_model_fold8, as.matrix(X_test))
predictions9 <- predict(xg_model_fold9, as.matrix(X_test))
predictions10 <- predict(xg_model_fold10, as.matrix(X_test))
ensemble_predictions2 <- (predictions1 + predictions2 + predictions3 + predictions4 + predictions5 +
                            predictions6 + predictions7 + predictions8 + predictions9 + predictions10) / 10
RMSE(as.matrix(y_test), ensemble_predictions2)

# Data asli
actual_values <- as.matrix(y_test)

# Prediksi dari model
predicted_values <- ensemble_predictions2

# Gabungkan data actual dan prediksi
comparison_data <- data.frame(Actual = actual_values, Predicted = predicted_values)

# Plot garis data test
plot_test <- ggplot(comparison_data, aes(x = seq_along(Harga), y = Harga, color = "Actual")) +
  geom_line() +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Perbandingan Harga Rumah Sebenarnya dan Hasil Prediksi Data Test",
       x = "Harga",
       y = "Prediksi") +
  scale_color_manual(values = c("blue", "red"), name = "Legend", labels = c("Actual", "Predicted"))

ggsave("test_plot.png", plot = plot_test, width = 10, height = 6)

# Input data testing baru
data_test_baru <- read.table("Data Testing Properti.csv", header = TRUE, sep=",")

# Mendefinisikan fungsi untuk mengganti spasi dengan '_'
replace_spaces <- function(column) {
  if (is.character(column)) {
    return(gsub(" ", "_", column))
  } else {
    return(column)
  }
}

# Mendefinisikan fungsi untuk mengubah huruf besar jika kolom berisi teks
convert_to_upper <- function(column) {
  if (is.character(column)) {
    return(toupper(column))
  } else {
    return(column)
  }
}

# Menggunakan apply untuk menerapkan fungsi pada setiap kolom
data_test_baru <- as.data.frame(lapply(data_test_baru, convert_to_upper))
data_test_baru <- as.data.frame(lapply(data_test_baru, replace_spaces))

# Praproses (Drop kolom yang tidak diperlukan)
data_test_baru <- subset(data_test_baru, select = -No)
data_test_baru <- subset(data_test_baru, select = -Lokasi)
data_test_baru <- subset(data_test_baru, select = -Kecamatan)
data_test_baru$Harga <- as.numeric(gsub("[^0-9]", "", data_test_baru$Real.Harga))
data_test_baru$Luas.Tanah <- as.numeric(data_test_baru$Luas.Tanah..dalam.satuan.m..)
data_test_baru$Luas.Bangunan <- as.numeric(data_test_baru$Luas.Bangunan..dalam.satuan.m..)
data_test_baru$Lebar.Jalan <- as.numeric(data_test_baru$Lebar.Jalan..dalam.satuan.m.)
data_test_baru$Jumlah.Lantai <- as.numeric(data_test_baru$Jumlah.Lantai)
data_test_baru$Jumlah.KT <- as.numeric(data_test_baru$Jumlah.KT)
data_test_baru$Jumlah.KM <- as.numeric(data_test_baru$Jumlah.KM)
data_test_baru$Daya.Listrik <- as.numeric(data_test_baru$Daya.Listrik..dalam.satuan.watt.)
data_test_baru$Tipe.Hunian <- as.factor(data_test_baru$Tipe.Hunian)
data_test_baru$Kondisi <- as.factor(data_test_baru$Kondisi)
data_test_baru$Kepemilikan <- as.factor(data_test_baru$Kepemilikan)
data_test_baru$Garasi <- as.factor(data_test_baru$Garasi)
data_test_baru$Rooftop <- as.factor(data_test_baru$Rooftop)
data_test_baru$Sumber.Air <- as.factor(data_test_baru$Sumber.Air)
data_test_baru$Tipe.Furnish <- as.factor(data_test_baru$Tipe.Furnish)
data_test_baru <- one_hot(as.data.table(data_test_baru))
data_test_baru<- subset(data_test_baru,select=-PREDIKSI)
data_test_baru<- subset(data_test_baru,select=-Real.Harga)
data_test_baru<- subset(data_test_baru,select=-Luas.Bangunan..dalam.satuan.m..)
data_test_baru<- subset(data_test_baru,select=-Daya.Listrik..dalam.satuan.watt.)
data_test_baru<- subset(data_test_baru,select=-Luas.Tanah..dalam.satuan.m..)
data_test_baru<- subset(data_test_baru,select=-Lebar.Jalan..dalam.satuan.m.)
data_test_baru$Rooftop_TIDAK <- data_test_baru$Rooftop_TIDAK + data_test_baru$Rooftop_TIDAK_ + data_test_baru$'Rooftop_-'
data_test_baru <- subset(data_test_baru, select = -Rooftop_TIDAK_)
data_test_baru <- subset(data_test_baru, select = -`Rooftop_-`)
data_test_baru$Sumber.Air_TIDAK <- data_test_baru$'Sumber.Air_-'
data_test_baru <- subset(data_test_baru, select = -`Sumber.Air_-`)
data_test_baru$Tipe.Furnish_NON_FURNISHED <- data_test_baru$Tipe.Furnish_NON_FURNISHED + data_test_baru$Tipe.Furnish_NON_FURNISH + data_test_baru$'Tipe.Furnish_-' + data_test_baru$Tipe.Furnish_UNFURNISHED
data_test_baru <- subset(data_test_baru, select = -Tipe.Furnish_NON_FURNISH)
data_test_baru <- subset(data_test_baru, select = -`Tipe.Furnish_-`)
data_test_baru <- subset(data_test_baru, select = -Tipe.Furnish_UNFURNISHED)
data_test_baru$Tipe.Furnish_SEMI_FURNISHED <- data_test_baru$Tipe.Furnish_SEMI_FURNISHED + data_test_baru$Tipe.Furnish_SEBAGIAN
data_test_baru <- subset(data_test_baru, select = -Tipe.Furnish_SEBAGIAN)
data_test_baru$Kepemilikan_SHGB <- data_test_baru$Kepemilikan_SHGB + data_test_baru$Kepemilikan_SHGD
data_test_baru <- subset(data_test_baru, select = -Kepemilikan_SHGD)
data_test_baru$Sumber.Air_KOMBINASI <- data_test_baru$Sumber.Air_KOMBINASI + data_test_baru$`Sumber.Air_SUMUR_&_PDAM`
data_test_baru <- subset(data_test_baru, select = -`Sumber.Air_SUMUR_&_PDAM`)
data_test_baru$Sumber.Air_SUMUR <- data_test_baru$Sumber.Air_SUMUR + data_test_baru$`Sumber.Air_SUMUR_BOR`
data_test_baru <- subset(data_test_baru, select = -Sumber.Air_SUMUR_BOR)
# data_test_baru$Sumber.Air_PDAM <- data_test_baru$Sumber.Air_PDAM + data_test_baru$`Sumber.Air_PAM`
# data_test_baru <- subset(data_test_baru, select = -Sumber.Air_PAM)
data_test_baru$Kepemilikan_AJB <- 0
data_test_baru$Kepemilikan_HGB <- 0
data_test_baru$Kepemilikan_PPJB <- 0
data_test_baru$Kepemilikan_STRATA <- 0
# str(data_test_baru)
# str(data_properti_encoded)
# datao <- data_properti_encoded$Sumber.Air_PDAM + data_properti_encoded$Sumber.Air_PAM
# datao
y_col1 <- "Harga"
X_cols1 <- setdiff(names(data_test_baru), y_col1)
test_data2 <- data_test_baru
# Gabungkan Prediksi dari Kedua Model
X_test2<- data.table(test_data2[, ..X_cols1])
y_test2 <- test_data2[, ..y_col1]

# Periksa atribut feature_names pada model
feature_names_model <- xg_model_fold1$feature_names
feature_names_model2 <- xg_model_fold2$feature_names
feature_names_model3 <- xg_model_fold3$feature_names

# Sesuaikan nama kolom pada data test dengan feature_names_model3
if (!identical(names(X_test2), feature_names_model3)) {
  colnames(X_test2) <- feature_names_model3
}

# str(xg_model_fold1)
predictions1 <- predict(xg_model_fold1, as.matrix(X_test2))
predictions2 <- predict(xg_model_fold2, as.matrix(X_test2))
predictions3 <- predict(xg_model_fold3, as.matrix(X_test2))

ensemble_predictions2 <- (predictions1 + predictions2+ predictions3) / 3
RMSE(as.matrix(y_test2), ensemble_predictions2)
# str(xg_model_fold1)
# predictions1 <- predict(xg_model_fold1, as.matrix(X_test2))
# predictions2 <- predict(xg_model_fold2, as.matrix(X_test2))
# predictions3 <- predict(xg_model_fold3, as.matrix(X_test2))
# predictions4 <- predict(xg_model_fold4, as.matrix(X_test2))
# predictions5 <- predict(xg_model_fold5, as.matrix(X_test2))
# predictions6 <- predict(xg_model_fold6, as.matrix(X_test2))
# predictions7 <- predict(xg_model_fold7, as.matrix(X_test2))
# predictions8 <- predict(xg_model_fold8, as.matrix(X_test2))
# predictions9 <- predict(xg_model_fold9, as.matrix(X_test2))
# predictions10 <- predict(xg_model_fold10, as.matrix(X_test2))
# ensemble_predictions2 <- (predictions1 + predictions2 + predictions3 )/ 10
# RMSE(as.matrix(y_test2), ensemble_predictions2)
# Menyiapkan vektor untuk menyimpan prediksi dari masing-masing model
# predictions <- vector("list", length = 10)

# Menghasilkan prediksi dari masing-masing model
# for (i in 1:10) {
#   predictions[[i]] <- predict(get(paste0("xg_model_fold", i)), as.matrix(X_test2))
# }

# Menghitung ensemble predictions sebagai rata-rata dari prediksi semua model
# ensemble_predictions <- rowMeans(do.call(cbind, predictions))
# ensemble_predictions
# Menghitung RMSE
# RMSE(as.matrix(y_test2), ensemble_predictions)

# Data asli
actual_values <- as.matrix(y_test2)

# Prediksi dari model
predicted_values <- ensemble_predictions2
ensemble_predictions2
# Gabungkan data actual dan prediksi
comparison_data <- data.frame(Actual = actual_values, Predicted = predicted_values)

# Plot garis data test
plot_test2 <- ggplot(comparison_data, aes(x = seq_along(Harga), y = Harga, color = "Actual")) +
  geom_line() +
  geom_line(aes(y = Predicted, color = "Predicted")) +
  labs(title = "Perbandingan Harga Rumah Sebenarnya dan Hasil Prediksi Data Test Hidden",
       x = "Harga",
       y = "Prediksi") +
  scale_color_manual(values = c("blue", "red"), name = "Legend", labels = c("Actual", "Predicted"))

ggsave("test2_plot.png", plot = plot_test2, width = 10, height = 6)

# Input data testing baru
data_test_baru <- read.table("Data Testing Properti.csv", header = TRUE, sep=",")

# Mendefinisikan fungsi untuk mengganti spasi dengan '_'
replace_spaces <- function(column) {
  if (is.character(column)) {
    return(gsub(" ", "_", column))
  } else {
    return(column)
  }
}

# Mendefinisikan fungsi untuk mengubah huruf besar jika kolom berisi teks
convert_to_upper <- function(column) {
  if (is.character(column)) {
    return(toupper(column))
  } else {
    return(column)
  }
}
