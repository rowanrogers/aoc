import numpy as np
import operator
import re

# ops = { "+": operator.add, "-": operator.sub, "*": operator.mul}

ROCK = "#"

def read_data(fp: str = "./day14/test.txt"):
    with open(fp) as f:
        data = f.read().splitlines()
    formations = []
    for n in data:
        formation = n.split("->")
        formation = [[x.split(",")[0].replace(" ", ""),x.split(",")[1].replace(" ", "")] for x in formation]
        formation = [list(map(int, x)) for x in formation]
        formations.append(formation)
    return formations


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
  TRUE

if __name__ == "__main__":
    rocks = read_data()
    print(rocks)
    
    caveMap = createMap(rocks)
    print(caveMap[0])
    # for m in allMonkeys:
    #     m.assignPartner(allMonkeys)
    #     monkeymod *= m.divisor
    # 
    # round2 = True
    # i = 1
    # while i <= 10000:
    #     for m in allMonkeys:
    #         m.processItems()
    #     i += 1
    # 
    # monkey_stuff = sorted([x.inspectCounter for x in allMonkeys], reverse=True)
    # print(f"score: {monkey_stuff[0] * monkey_stuff[1]}")
    # print(allMonkeys[0].operation)



