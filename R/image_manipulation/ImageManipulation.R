library(imager)
library(dplyr)
library(ggplot2)
library(scales)
library(tidyverse)
library(patchwork)

# Load images
parrots <- load.example("parrots")
class(parrots)
parrots # see info

parrots_df <- as.data.frame(parrots) 
parrots_df$value <- round(parrots_df$value*244,0) # make into actual RGB values



# Plot
plot(parrots)
  # Manuelle see object in it

dim(parrots)


# images work like a regular array, so regular arithmetic operations work
mean(parrots)
sd(parrots)

colMeans(parrots_df)

layout(t(1:3)) # show all 3 plots under
plot(parrots, rescale = F)
plot(parrots/2) # looks the same as above, due to imager automatically rescaling
plot(parrots/2, rescale = F)
layout(1)

#  Gray scale
parrots_gray <- grayscale(parrots)
parrots_gray # colour channels changed to 1
plot(parrots_gray)

parrots_gray_df <- as.data.frame(parrots_gray) 
parrots_gray_df$value <- round(parrots_gray_df$value*244,0) # make into actual RGB values


# Plots ----
grayscale(parrots) %>% hist(main="Pixel values")
R(parrots) %>% hist(main="red channel values")

parrots_df <- mutate(parrots_df, channel = factor(cc,labels = c("R","G","B")))
parrots_df %>% 
  ggplot(aes(value, col=channel)) + 
  geom_histogram(bins=30) +
  facet_wrap(~channel)
parrots_df %>% group_by(channel) %>% summarise(total = sum(value)) # mosty red in the pic


# edge detection -----
parrots.g <- grayscale(parrots)
gr <- imgradient(parrots.g,"xy")
gr
plot(gr, layout = "row")

imgradient(parrots.g,"xy") %>% enorm %>% plot()



# plotting from the df -----
parrots_df %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value))
  # not quite, mainly also due to y being upside down

parrots_df %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value)) +
  scale_y_reverse() +
  scale_fill_continuous(low = "black", high = "white") # bit of a manual grayscale

# 2 get color, we need to plot each channel seperately
parrots_df %>% ggplot(aes(x,y)) + 
  geom_raster(aes(fill=value)) +
  facet_wrap(~cc) +
  scale_y_reverse()
# we need to make the df in a wide format
as.data.frame(parrots, wide="c") %>% head

  # get rgb value
df_rgb <- as.data.frame(parrots, wide="c") %>% mutate(rgb.val = rgb(c.1,c.2,c.3))
head(df_rgb,3)

df_rgb %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill = rgb.val)) +
  scale_fill_identity() +
  scale_y_reverse()
# :)


# Split image every 2nd row
pdf_2 <- parrots_df[seq(2, nrow(parrots_df), by = 2), ]
pdf_1 <- parrots_df %>% filter(!x %in% pdf_2$x)

pdf_2 <- pdf_2 %>% 
  mutate(
    x = dense_rank(x),
    y = dense_rank(y)
  )
pdf_1 <- pdf_1 %>% 
  mutate(
    x = dense_rank(x),
    y = dense_rank(y)
  )

pdf_1 %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value)) +
  scale_y_reverse() +
  scale_fill_continuous(low = "black", high = "white") 
pdf_2 %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value)) +
  scale_y_reverse() +
  scale_fill_continuous(low = "black", high = "white")

pdf_2_w <- pdf_2 %>% 
  mutate(value = value/255) %>% 
  pivot_wider(names_from = cc) 
colnames(pdf_2_w) <- c("x","y","R","G","B")

pdf_2_w <- pdf_2_w %>% 
  mutate(rgb.val = rgb(R,G,B))

pdf_2_w %>% ggplot(aes(x,y)) + 
  geom_raster(aes(fill=rbg.val)) + 
  scale_fill_identity() +
  scale_y_reverse()

# Split function ----
split_df <- function(df, n) {
  # row index for modulo splitting
  df <- df %>% mutate(row_index = row_number())
  
  # list of split daraframes
  splits <- lapply(0:(n-1), function(i) {
    df_part <- df %>% 
      filter((row_index - 1) %% n == i) %>% 
      select(-row_index) %>% 
      mutate(
        x = dense_rank(x),
        y = dense_rank(y)
      )
    
    # covert to wide with RGB
    df_wide <- df_part %>% 
      mutate( value = value / 255) %>% 
      pivot_wider(names_from = cc, values_from = value)
    
    colnames(df_wide) <- c("x","y", "R", "G", "B")
    
    df_wide <- df_wide %>% 
      mutate(rgb.val = rgb(R, G, B))
    
    list(
      raw = df_part,
      wide = df_wide
    )
  })
  
  # name splits
  names(splits) <- paste0("part_", 1:n)
  
  return(splits)
}

split_df(parrots_df, 3)
splits <- split_df(parrots_df, 3)

df_1 <- splits$part_1$wide
df_2 <- splits$part_2$wide
df_3 <- splits$part_3$wide

df_1 %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill = rgb.val)) +
  scale_fill_identity() +
  scale_y_reverse()


rgb_plot <- function(df) {
 original_name <- deparse(substitute(df))
 
  p <- df %>% ggplot(aes(x,y)) +
    geom_raster(aes(fill = rgb.val)) +
    scale_fill_identity() +
    scale_y_reverse() +
    labs(title = original_name)
  
  assign(paste0(original_name, "_plot"), p, envir = .GlobalEnv)
  
  return(p)
}

rgb_plot(df_3)

# combine plots via patchwork
df_1_plot + df_2_plot + df_3_plot


# to do
# grayscale from df maybe? 
# colmeans / sd to see difference in 2 close to identical pics
# make an image df back into imager format


# own image -----
image <- load.image("Data/berserk_vol1.jpg")
plot(image)

df <- as.data.frame(image)
# cut out table
df_ss <- df %>% filter(x>950 & x<3700 & y>600 & y<2500) %>% 
  mutate(
    x = dense_rank(x), # reorder x, so it starts at 1
    y = dense_rank(y)
  )
df_ss %>% as.cimg %>% plot()

# flip it
df_ss_flip <- df_ss %>% 
  rename(tmp = x) %>% # make a tmp for x
  mutate(
    x = y,
    y = tmp) %>% 
  select(-tmp)
df_ss_flip %>% as.cimg %>% plot()
# its mirrored
df_ss_flip$x <- rev(df_ss_flip$x)
df_ss_flip %>% as.cimg %>% plot()

berserk <- df_ss_flip %>% as.cimg

berserk.g <- grayscale(berserk)
gr <- imgradient(berserk.g,"xy")
gr
plot(gr, layout = "row")

imgradient(berserk.g,"xy") %>% enorm %>% plot()


df_ss_flip %>% ggplot(aes(x,y)) +
  geom_raster(aes(fill=value)) +
  scale_y_reverse() +
  scale_fill_continuous(low = "black", high = "white") # bit of a manual grayscale


