import numpy as np
import operator
import re

# ops = { "+": operator.add, "-": operator.sub, "*": operator.mul}

ROCK = "#"
AIR = "."
SAND = "o"

def read_data(fp: str = "./day14/test.txt"):
    with open(fp) as f:
        data = f.read().splitlines()
    formations = []
    for n in data:
        rockSplit = n.split("->")
        for i in range(0, len(rockSplit) - 1):
          p1, p2 = rockSplit[i].split(","), rockSplit[i + 1].split(",")
          x1, y1 = int(p1[0]), int(p1[1])
          x2, y2 = int(p2[0]), int(p2[1])
          points = pointsOnLine(x1, y1, x2, y2)
          formations.append(points)
    return formations

def pointsOnLine(x1, y1, x2, y2):
  x_range = range(min(x1, x2), max(x1, x2) + 1)
  y_range = range(min(y1, y2), max(y1, y2) + 1)
  return[(x, y) for x in x_range for y in y_range]

def chunker(seq, size):
    return (seq[pos:pos + size] for pos in range(0, len(seq), size))

def createMap(rocks):
  cave = np.full((318, 1000), '.')
  bottom_depth = 0
  for rock in rocks:
      for (x, y) in rock:
            cave[y, x] = ROCK
            bottom_depth = max(y, bottom_depth)
  return cave, bottom_depth


def part1(input, bottom_depth):
  sandCount = 0
  caveMap = input.copy()
  while True:
    sandCount += 1
    r, c = 0, 500
    while True:
      if r > bottom_depth:
        return sandCount - 1
      elif caveMap[r + 1, c] == AIR:
        r += 1 
      elif caveMap[r + 1, c - 1] == AIR:
        r += 1
        c -= 1
      elif caveMap[r + 1, c + 1] == AIR:
        r += 1
        c += 1
      else:
        caveMap[r, c] = SAND
        break
      
def part2(input, bottom_depth):
  sandCount = 0
  caveMap = input.copy()
  floor_depth = bottom_depth + 2
  while True:
    sandCount += 1
    r, c = 0, 500
    while True:
      if r + 1 >= floor_depth:
        caveMap[r, c] = SAND
        break
      elif caveMap[1, 499] == caveMap[1, 500] == caveMap[1, 501] == SAND:
        return sandCount
      elif caveMap[r + 1, c] == AIR:
        r += 1 
      elif caveMap[r + 1, c - 1] == AIR:
        r += 1
        c -= 1
      elif caveMap[r + 1, c + 1] == AIR:
        r += 1
        c += 1
      else:
        caveMap[r, c] = SAND
        break
          
if __name__ == "__main__":
    rocks = read_data("./day14/input.txt")
    caveMap = createMap(rocks)
    print(part1(*caveMap))
    print(part2(*caveMap))


