i = []
with open("input.txt") as f:
  i = f.read().split(",")

dmap = {
  "n": (0,1),
  "s": (0,-1),
  "ne": (.5,.5),
  "se": (.5,-.5),
  "nw": (-.5,.5),
  "sw": (-.5,-.5)
}

x,y = 0,0 #x,y coordinates for tracking how far we've moved 
m = []
for d in i:
  x += dmap[d][0]
  y += dmap[d][1]
  m.append(abs(x)+abs(y))

print(abs(x)+abs(y)) #Part 1
print(max(m)) # Part 2
