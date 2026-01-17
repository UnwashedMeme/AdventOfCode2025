### original grid

Points make a perimeter

```
..............    ..............
.......#...#..    .......#XXX#..
..............    .......X...X..
..#....#......    ..#XXXX#...X..
..............    ..X........X..
..#......#....    ..#XXXXXX#.X..
..............    .........X.X..
.........#.#..    .........#X#..
..............    ..............
```

The goal is to find a pair of points that make the opposite corners of the
largest rectangle that lies inside this perimeter.

### BAD

Consider the rectangle made by the stars

```
..............
.......#...#..
..............
..*....G......
..............
..#......B....
..............
.........*.#..
..............
```

Doesn't work because B[ad] is inside the rectangle
G[ood] is fine because it is on the border


### Best rectangle

```
..............    ..............
.......#...#..    .......#XXX#..
..............    .......XXXXX..
..*....G......    ..OOOOOOOOXX..
..............    ..OOOOOOOOXX..
..#......*....    ..OOOOOOOOXX..
..............    .........XXX..
.........#.#..    .........#X#..
..............    ..............
```

Again G[ood] doesn't matter, while it is on the edge of our rectangle it goes
away from it.

```f#
let checkForGreen (points: seq<Point>) pair =
    let ((x1,y1), (x2,y2)) = pair
    let minx, miny = min x1 x2, min y1 y2
    let maxx, maxy = max x1 x2, max y1 y2
    let easyskip (x, y) =
        x < minx || maxx < x ||
        y < miny || maxy < y

    let between a b t = 
        (a < t && t < b) || (b < t && t < a)
    let includes a b t = 
        (a <= t && t <= b) || (b <= t && t <= a)

    let badpoint (x,y) =
        (between x1 x2 x && between y1 y2 y) || 
        (between x1 x2 x  && includes y1 y2 y) ||
        (includes x1 x2 x  && between y1 y2 y)

    points |> Seq.exists badpoint
```