## Sunday 22nd Oct 2017

9:36 Found Math.Geometry.Grid, appears to work fine for unbounded hex grids.

How to do collisions? _Think_ you need to interpolate and do collision detection?
If you do it instantaneous, two atoms could switch places. But can't just check target destination, otherwise can't have side by side atoms following each other. Maybe could do something drawing lines/arcs and checking for intersection? Based on red square highlighting, suggests OM interpolates.

11:00 Basic grabber rotation sketched out.

(later that day)

1:00 playing around with GLUT for a UI, seems more useful than figuring out ascii hexes

1:30 got a game loop with timer going. GLUT probably won't scale long term but gets us going for now.

2:30 rendering basic shapes for grabbers, reagents and products

## Saturday 4th Nov 2017

Grabbers can pick single element lattices up and move them now.

Next steps

* Products consume lattices when covered
* Reagents produce lattices if not blocked
* Two grabbers can't move the same lattice
* Multi-element lattices (with rotation, and grabbing at different spots)
