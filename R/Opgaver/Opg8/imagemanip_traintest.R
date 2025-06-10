library(imager)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(patchwork)

#### Data retrieval ####
image_files <- list.files("R/Opgaver/Opg8", pattern = "\\.jpg$", full.names = TRUE)

images <- lapply(image_files, load.image)
# 0 = not
# 1 = is

# plot images
par(mfrow = c(2, 5), mar = c(1, 1, 2, 1)) 
for (i in 1:10) {
  plot(images[[i]], main = i)
  print(paste0("image number: ",i))
  cat(dim(images[[i]]),"\n")
}
# all images are the same length

#### Label images ####
labels <- c(0,0,0,0,0,1,1,1,1,1) 
labels_df <- data.frame(
  file = basename(image_files),
  label =   c(0,0,0,0,0,1,1,1,1,1)
)
dev.off()
names(images) <- labels

# total stats
image_stats <- lapply(images, function(img) {
  data.frame(
    mean = mean(img),
    sd = sd(img)
  )
})
images_df <- do.call(rbind, image_stats)
images_df$label <- labels
images_df




# grouped by cc 
image_stats_grouped <- lapply(images, function(img) {
  img_df <- as.data.frame(img)
  img_df$value <- round(img_df$value*244,0)
  img_df %>%
    group_by(cc) %>%  # channel code: 1 = R, 2 = G, 3 = B
    summarise(
      mean = mean(value),
      sd = sd(value)
    )
})
images_df_grouped <- do.call(rbind, image_stats_grouped)
images_df_grouped$label <- rep(labels, each = 3)
# lets see only for red
red_df <- images_df_grouped %>% 
  filter(cc == "1") %>% 
  group_by(label) %>% 
  summarise(sd = mean(sd),
            mean = mean(mean))
images_df_grouped %>%
  filter(cc == 1) %>%
  ggplot(aes(x = factor(label), y = mean)) +
  geom_boxplot()
# more red intensity within the pics with the flag


#### plot an image ####
image <- images[[9]]
dim(image)
plot(image)
plot(grayscale(image))

# you can resize images: 
dim(image)
# making the image half the size
img_small <- resize(image, size_x = dim(image)[1]/2, size_y = dim(image)[2]/2)
plot(img_small)

##### plotting from the df #####
# make into a df
image_df <- as.data.frame(images[[9]]) %>% 
  mutate(value = round(value * 255,0))
colMeans(image_df)

# Image color distribution
image_df <- mutate(image_df, channel = factor(cc,labels = c("R","G","B")))
image_df %>% 
  ggplot(aes(value, col=channel)) + 
  geom_histogram(bins=30) +
  facet_wrap(~channel)
# shows that for expecially blue, its either VERY blue pixels, or not very blue at all
image_df %>% group_by(channel) %>% summarise(total = sum(value)) # mosty blue in the pic


image_df %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value))
# not quite, mainly also due to y being upside down

image_df %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value)) +
  scale_y_reverse() +
  scale_fill_continuous(low = "black", high = "white") # bit of a manual grayscale

# to get color, we need to plot each channel seperately
image_df %>% ggplot(aes(x,y)) + 
  geom_raster(aes(fill=value)) +
  facet_wrap(~cc) +
  scale_y_reverse()
# we need to make the df in a wide format
as.data.frame(image, wide="c") %>% head

# get rgb value
df_rgb <- as.data.frame(image, wide="c") %>% mutate(rgb.val = rgb(c.1,c.2,c.3))
head(df_rgb,3)

df_rgb %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill = rgb.val)) +
  scale_fill_identity() +
  scale_y_reverse()
# :)

# not if we wanted to flip the df upright
df_rgb_flip <- df_rgb %>% 
  mutate(tmp_x = x,
         x = y,
         y = tmp_x) %>% 
  select(-tmp_x)
df_rgb_flip %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill = rgb.val)) +
  scale_fill_identity() +
  scale_y_reverse()










#### Train / Test ####
train_idx <- 2:9
test_idx <- c(1,10)

train_images <- image_files[train_idx]
train_labels <- labels[train_idx]

test_images <- image_files[test_idx]
test_labels <- labels[test_idx]

