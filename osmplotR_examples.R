require(osmplotr)

bbox <- get_bbox (c(-0.15,51.5,-0.10,51.52))
dat_B <- extract_osm_objects (key='building', bbox=bbox)
map <- osm_basemap (bbox=bbox, bg='gray20')
map <- add_osm_objects (map, dat_B, col='gray40')
print (map)

dat_H <- extract_osm_objects (key='highway', bbox=bbox)
dat_P <- extract_osm_objects (key='park', bbox=bbox)
dat_G <- extract_osm_objects (key='landuse', value='grass', bbox=bbox)
map <- osm_basemap (bbox=bbox, bg='gray20')
map <- add_osm_objects (map, dat_B, col='gray40')
map <- add_osm_objects (map, dat_H, col='gray80')
map <- add_osm_objects (map, dat_P, col='darkseagreen')
map <- add_osm_objects (map, dat_G, col='darkseagreen1')
print_osm_map (map)

map <- osm_basemap (bbox=bbox, bg='gray20')
map <- add_osm_groups (map, dat_B, groups=pts, cols='orange', bg='gray40')
map <- add_osm_objects (map, london$dat_P, col='darkseagreen1')
map <- add_osm_groups (map, london$dat_P, groups=pts, cols='darkseagreen1',
                       bg='darkseagreen', boundary=0)
print_osm_map (map)

set.seed (2)
ngroups <- 12
x <- bbox [1,1] + runif (ngroups) * diff (bbox [1,])
y <- bbox [2,1] + runif (ngroups) * diff (bbox [2,])
groups <- cbind (x, y)
groups <- apply (groups, 1, function (i) 
  sp::SpatialPoints (matrix (i, nrow=1, ncol=2)))

# These highways extend beyond the previous, smaller bbox
bbox_big <- get_bbox (c(-0.15,51.5,-0.10,51.52))
highways <- c ('Davies.St', 'Berkeley.Sq', 'Berkeley.St', 'Piccadilly',
               'Regent.St', 'Oxford.St')
highways1 <- connect_highways (highways=highways, bbox=bbox_big)
highways <- c ('Regent.St', 'Oxford.St', 'Shaftesbury')
highways2 <- connect_highways (highways=highways, bbox=bbox_big)
highways <- c ('Piccadilly', 'Shaftesbury.Ave', 'Charing.Cross.R',
               'Saint.Martin', 'Trafalgar.Sq', 'Cockspur.St',
               'Pall.Mall', 'St.James')
highways3 <- connect_highways (highways=highways, bbox=bbox_big)
highways <- c ('Charing.Cross', 'Duncannon.St', 'Strand', 'Aldwych',
               'Kingsway', 'High.Holborn', 'Shaftesbury.Ave')
highways4 <- connect_highways (highways=highways, bbox=bbox_big)
highways <- c ('Kingsway', 'Holborn', 'Farringdon.St', 'Strand',
               'Fleet.St', 'Aldwych')
highways5 <- connect_highways (highways=highways, bbox=bbox_big)
groups <- list (highways1, highways2, highways3, highways4, highways5)

ap <- osm_basemap (bbox=bbox, bg='gray20')
library (wesanderson)
cols <- wes_palette ('Darjeeling', 5) 
map <- add_osm_groups (map, dat_B, groups=groups, boundary=1,
                       cols=cols, bg='gray40', colmat=FALSE)
map <- add_osm_groups (map, dat_H, groups=groups, boundary=0,
                       cols=cols, bg='gray70', colmat=FALSE)
print_osm_map (map)
