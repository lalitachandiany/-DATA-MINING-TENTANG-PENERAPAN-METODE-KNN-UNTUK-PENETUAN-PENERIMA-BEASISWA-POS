library(class)
library(data.table)
library(ggplot2)
library(plyr)
options(scipen=999)  # turn-off scientific notation like 1e+48

#baca dataset
data.beasiswa <- read.csv("Dataset.csv", header=T)
new.data.beasiswa <- data.beasiswa[,c(3,4,5,6)]
beasiswa <- data.table(new.data.beasiswa)
View(beasiswa)


#knn function
prediksi_beasiswa <- cbind(beasiswa$IPK, beasiswa$Gaji_Orang_Tua, beasiswa$Jumlah_Tanggungan)
head(prediksi_beasiswa)

keterangan <- beasiswa$Hasil
head(keterangan)

databaru <- cbind(2.73, 3500000, 5)
databaru

prediksi_beasiswa.knn <- knn(prediksi_beasiswa, databaru, keterangan, k=3, prob = T)
prediksi_beasiswa.knn


#Visualisasi
plot.beasiswa = data.frame(prediksi_beasiswa, Keputusan = beasiswa$Hasil)

plot.beasiswa1 = data.frame(x = plot.beasiswa$X1,
                            y = plot.beasiswa$X2,
                            Keputusan = plot.beasiswa$Keputusan)

find_hull = function(beasiswa) beasiswa[chull(beasiswa$X1, beasiswa$X2), ]
boundary = ddply(plot.beasiswa1, .variables = "Keputusan", .fun = find_hull)


ggplot(plot.beasiswa, aes(X1, X2, color = Keputusan, fill = Keputusan)) + 
  geom_point(size = 5) + 
  annotate("point", x = 2.73, y = 3500000, size = 5, colour="black") +
  labs(subtitle="Klasifikasi mahasiswa penerima beasiswa", 
  y="Gaji Orang Tua", 
  x="IPK", 
  title="Penerima Beasiswa PT POS INDONESIA", 
  caption = "Source: Bagian Kemahasiswaan")
  geom_polygon(data = boundary, aes(x,y), alpha = 0.5)