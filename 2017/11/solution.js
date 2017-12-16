const fs = require('fs')
let inp = fs.readFileSync("./day11input").toString('utf-8').trim().split(","),
    dirs = {'n': [-1,1,0], 'ne': [0,1,-1], 'se': [1,0,-1], 's': [1,-1,0], 'sw': [0,-1,1], 'nw': [-1,0,1]},
    coords = [0,0,0],
    max = -Infinity,
    distance = (x => x.map(Math.abs).reduce((a,b) => a > b ? a : b))

for (let d of inp) {
  coords = coords.map( (x,i) => x + dirs[d][i] )
  max = Math.max(max, distance(coords))
}
console.log(distance(coords), max);
