Sunday 22nd Oct 2017

9:36 Found Math.Geometry.Grid, appears to work fine for unbounded hex grids.

How to do collisions? _Think_ you need to interpolate and do collision detection?
If you do it instantaneous, two atoms could switch places. But can't just check target destination, otherwise can't have side by side atoms following each other. Maybe could do something drawing lines/arcs and checking for intersection? Based on red square highlighting, suggests OM interpolates.

11:00 Basic grabber rotation sketched out.

(later that day)

1:00 playing around with GLUT for a UI, seems more useful than figuring out ascii hexes
