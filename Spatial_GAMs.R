library(mgcv)
library(fields)
library(viridis)
library(ggplot2)

# Load in data and predictions
GM_dat <- readRDS("France_GM.rds")
grid_dat <- readRDS("France_grid.rds")

col.vec <- viridis(100, option = 'magma')
col.seq <- seq(from = 0, to = max(GM_dat$PM25), length = length(col.vec) + 1)
col.labels <- numeric(nrow(GM_dat))
for (i in 1:length(col.labels)){
  col.labels[i] <- col.vec[which(col.seq > GM_dat$PM25[i])[1] - 1]
}

par(mar = c(4,4,2,2))
plot(GM_dat$Longitude, GM_dat$Latitude, pch = 19, col = col.labels, cex = 2,
     xlab = 'Lon', ylab = 'Lat')
map('world', add = TRUE)
legend(y = 46.5, x = -4.7, legend = seq(0,20,by = 5), col = col.vec[c(1,25,50,75,100)], pch = 19)

GM_dat$logPOP <- log(GM_dat$POP + 1)
grid_dat$logPOP <- log(grid_dat$POP_2016 + 1)
grid_dat$POP <- grid_dat$POP_2016

#### Longitude only ####
mod_sp <- gam(PM25 ~ s(Longitude, bs = 'cr'), data = GM_dat)
plot(mod_sp)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")


#### Latitude only ####
mod_sp <- gam(PM25 ~ s(Latitude, bs = 'cr'), data = GM_dat)
plot(mod_sp)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")



#### Latitude only, more knots ####
mod_sp <- gam(PM25 ~ s(Latitude, bs = 'cr', k = 50), data = GM_dat)
plot(mod_sp)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")



#### Lon+lat ####
mod_sp <- gam(PM25 ~ s(Longitude, bs = 'cr') + s(Latitude, bs = 'cr'), data = GM_dat)

par(mfrow=c(1,2))
plot(mod_sp)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")


#### Spatial ####
mod_sp <- gam(PM25 ~ s(Longitude, Latitude), data = GM_dat)
plot(mod_sp)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")




#### Spatial ####
mod_sp <- gam(PM25 ~ s(Longitude, Latitude) + s(ELEVATION, bs = 'cr'), data = GM_dat)

par(mfrow=c(1,2))
plot(mod_sp)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")



#### Gamma ####
mod_sp <- gam(PM25 ~ s(Longitude, Latitude) + s(ELEVATION, bs = 'cr'), family = Gamma, data = GM_dat)
preds <- predict(mod_sp, grid_dat, type = 'response')
summary(preds)

ggplot(data.frame(Lon = grid_dat$Longitude,
                  Lat = grid_dat$Latitude,
                  PM25 = preds), aes(x = Lon, y = Lat, fill = PM25, col = PM25)) +
  geom_point(size = 2.1, shape = 15) +
  scale_fill_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  scale_color_viridis(limits = c(0,max(GM_dat$PM25)), option = 'magma') +
  geom_point(data = data.frame(Lon = GM_dat$Longitude,
                               Lat = GM_dat$Latitude,
                               PM25 = GM_dat$PM25), aes(fill = PM25), shape = 21, size = 4, col = 1) +
  guides(fill="none")


# Always good to check...
gam.check(mod_sp)






