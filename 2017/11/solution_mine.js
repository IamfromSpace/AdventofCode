const fs = require('fs');
const inp = fs.readFileSync("./day11input").toString('utf-8').trim().split(",");

const easyWay = list => {
    const countSteps = ([ x, y, z ]) =>
      (Math.abs(x) + Math.abs(y) + Math.abs(z)) / 2;

    return list.reduce(
      ([ [x, y, z], prevMax ], dir) => {
        let nextPoint;
        switch (dir) {
          case "n":
            nextPoint = [ x, y + 1, z - 1 ]
            break;
          case "nw":
            nextPoint = [ x - 1, y + 1, z ]
            break;
          case "sw":
            nextPoint = [ x - 1, y, z + 1 ]
            break;
          case "s":
            nextPoint = [ x, y - 1, z + 1 ]
            break;
          case "se":
            nextPoint = [ x + 1, y - 1, z ]
            break;
          case "ne":
            nextPoint = [ x + 1, y, z - 1 ]
            break;
          default:
            throw new Error("pattern match failed!");
        }
        return [ nextPoint, Math.max((countSteps(nextPoint)), prevMax) ];
      },
      [[0,0,0],0]
    );
}

console.log(easyWay(inp));
