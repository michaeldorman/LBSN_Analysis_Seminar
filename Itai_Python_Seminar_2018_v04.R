library(rtweet)

dat = parse_stream("stream_boston.json")
dat = as.data.frame(dat)

dim(dat)

vars = c(
  "user_id", 
  "screen_name", 
  "created_at", 
  "text", 
  "lang", 
  "geo_coords"
  )

dat = dat[, vars]

head(dat)

class(dat$created_at)

dat$created_at[1]

dat$created_at = format(
  dat$created_at,
  tz = "US/Eastern"
)
dat$created_at = as.POSIXct(
  dat$created_at,
  tz = "US/Eastern"
)

dat$created_at[1]

min(dat$created_at)
max(dat$created_at)

dat$geo_coords[1:20]

dat$lon = sapply(dat$geo_coords, "[", 2)
dat$lon[1:20]

dat$lat = sapply(dat$geo_coords, "[", 1)
dat$lat[1:20]

dat$geo_coords = NULL

head(dat, 20)

geo = complete.cases(dat[, c("lon", "lat")])
geo[1:20]

dat = dat[geo, ]

dim(dat)

library(sf)

pnt = st_as_sf(
  x = dat, 
  coords = c("lon", "lat"), 
  crs = 4326
)

pnt

p = c(-72.21437, 41.19034, -69.64939, 43.30924)
p1 = st_point(c(p[1], p[2]))
p2 = st_point(c(p[1], p[4]))
p3 = st_point(c(p[3], p[4]))
p4 = st_point(c(p[3], p[2]))
b = c(p1, p2, p3, p4)
b = st_convex_hull(b)

plot(st_geometry(pnt))
plot(b, add = TRUE, border = "red")

pnt = pnt[b, ]

plot(st_geometry(pnt))
plot(b, border = "red", add = TRUE)

borders = st_read("borders.shp")

opar = par()
plot(borders[, "NAME_1"], border = "grey", key.size = lcm(5))
plot(b, add = TRUE, border = "red")
plot(st_geometry(pnt), add = TRUE)
par(opar)

pnt$hour = format(pnt$created_at, "%m-%d %H")
pnt$hour

tab = table(pnt$hour)
tab

barplot(tab, ylab = "Tweets", las = 3)

pnt = st_join(pnt, borders)

pnt1 = st_set_geometry(
  pnt[, c("hour", "NAME_2")], 
  NULL
)

head(pnt1)

tab = table(pnt1)
tab

tab1 = tab
colnames(tab1) = seq(0, 1, 1/(ncol(tab)-1))
rownames(tab1) = seq(0, 1, 1/(nrow(tab)-1))
tab1 = as.data.frame(tab1, stringsAsFactors = FALSE)
tab1$hour = as.numeric(tab1$hour)
tab1$NAME_2 = as.numeric(tab1$NAME_2)
tab1$Freq[tab1$Freq == 0] = NA

image(tab, axes = FALSE)
axis(
  2, at = seq(0, 1, 1/(ncol(tab)-1)),
  labels = colnames(tab),
  las = 2, lwd = 0, lwd.ticks = 1, cex.axis = 0.75
)
axis(
  1, at = seq(0, 1, 1/(nrow(tab)-1)),
  labels = rownames(tab),
  las = 1, lwd = 0, lwd.ticks = 1, cex.axis = 0.75
)
text(tab1$hour, tab1$NAME_2, tab1$Freq, cex = 0.75)

path = split(pnt, pnt$user_id)

n = sapply(path, nrow)
barplot(table(n), xlab = "Tweets", ylab = "Number of users")

path = path[n > 1]
path = lapply(path, st_combine)
path = lapply(path, st_cast, "LINESTRING")
path = do.call(c, path)

opar = par()
plot(borders[, "NAME_1"], border = "grey", key.size = lcm(5))
plot(b, add = TRUE, border = "red")
plot(st_geometry(path), add = TRUE)
par(opar)

## python get_followers.py -s MichaelDorman84 -d 2

## python twitter_network.py

friends = read.csv("network.csv", sep = "\t", header = FALSE)

head(friends)
dim(friends)

friends = friends[friends$V2 %in% friends$V1, ]

dim(friends)

locations = read.csv(
  "locations.csv", 
  stringsAsFactors = FALSE
)

head(locations)

library(mapsapi)

key = readLines("~/Dropbox/BGU/key")
gc = mp_geocode(locations$location[1:5], key = key)
gcp = mp_get_points(gc)

# library(mapsapi)
# key = readLines("~/Dropbox/BGU/key")
# gc = mp_geocode(locations$location, key = key)
# gcp = mp_get_points(gc)
# st_write(gcp, "gcp.shp")

gcp = st_read("gcp.shp")

library(rworldmap)

world = getMap(resolution = "li")
world = st_as_sf(world)

plot(st_geometry(world), border = "grey")
plot(st_geometry(gcp), add = TRUE, col = "red")

gcp = st_join(gcp, world)
locations$NAME = gcp$NAME

head(locations)

friends$from = 
  locations$NAME[match(friends$V1, locations$name)]
friends$to = 
  locations$NAME[match(friends$V2, locations$name)]

friends = friends[, c("from", "to")]

friends = friends[complete.cases(friends), ]

head(friends)

library(igraph)

g = graph_from_data_frame(friends)

E(g)$weight = 1
g = simplify(graph = g, remove.loops = FALSE, edge.attr.comb = "sum")

g

is.directed(g)
is.weighted(g)
vcount(g)
ecount(g)

V(g)[1:5]
V(g)$name[1:5]

E(g)[1:5]
E(g)$weight[1:5]

edge_density(g)
count_components(g)
diameter(g)

ew = E(g)$weight
l = structure(c(-12.914331834664, -3.20946890558559, 15.4079654533961, 
12.303623802916, 9.21958478771939, -12.6530433606339, 0.703803604878003, 
6.79489837945612, -6.55707051796123, -10.2935250573191, 18.2638111564049, 
28.0713039242138, -5.2114501999283, 10.4972716408154, 6.3310926959767, 
-27.1001911450631, -13.3725942662633, -7.28516906447345, -26.0508067516085, 
-27.8577443234432, 7.68953430493844, -3.55987023974154, -15.8891453906384, 
8.12752863091133, -18.3779833793809, -7.09235400730142, 12.4011749694883, 
-5.18350548748148, -12.4775870968728, -17.7904187773816, -3.41414052716002, 
5.15139851973654, -31.6256988695004, -28.4609030773732, 21.5817808730989, 
7.5026958846952, 20.2890220020182, 34.4660817756094, -16.9945305779494, 
37.1685058291664), .Dim = c(20L, 2L))

plot(
  g, 
  layout = l, 
  edge.arrow.size = 0.5,
  edge.color = rgb(1, 0, 0, scales::rescale(ew, c(0.25, 1))),
  edge.curved = 0.1,
  edge.width = scales::rescale(ew, c(0.1, 10))
)

d = degree(g, mode = "total")
d

plot(
  g, 
  layout = l, 
  edge.arrow.size = 0.5,
  edge.color = rgb(1, 0, 0, scales::rescale(log(ew), c(0.25, 1))),
  edge.curved = 0.1,
  edge.width = scales::rescale(log(ew), c(0.1, 5)),
  vertex.size = d
)

cfg = cluster_optimal(g)

plot(cfg, g, layout = l, edge.arrow.size = 0.5, edge.width = 0.5)

library(qdap)

text = c(
  "I'm happy", "I'm sad...",
  "I'm very happy!", "I'm not happy"
)
polarity(text)$all[, -(1:2)]

pnt1 = pnt[pnt$lang == "en", ]
pol = polarity(pnt1$text)
pol = pol$all
pnt1$score = pol$polarity
pnt1$pos = 
  sapply(pol$pos.words, paste, collapse = "|")
pnt1$neg = 
  sapply(pol$neg.words, paste, collapse = "|")

tmp = pnt1[order(pnt1$score, decreasing = TRUE)[1:4], ]
tmp$text = tmp$text %>% substr(1, 10) %>% paste0("...")
head(tmp[, c("text", "pos", "neg")]) %>% st_set_geometry(NULL)

tmp = pnt1[order(pnt1$score)[1:4], ]
tmp$text = tmp$text %>% substr(1, 10) %>% paste0("...")
head(tmp[, c("text", "pos", "neg")]) %>% st_set_geometry(NULL)

plot(pnt1[, "score"], pal = RColorBrewer::brewer.pal(10, "RdBu"), pch = 19)

