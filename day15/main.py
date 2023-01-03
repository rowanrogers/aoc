import numpy as np
import operator
import re
from collections import namedtuple

def manDist(point1, point2):
  manD = abs(point1[0] - point2[0]) + abs(point1[1] - point2[1])
  return manD

# ops = { "+": operator.add, "-": operator.sub, "*": operator.mul}

def read_data(fp: str = "./day15/test.txt"):
    with open(fp) as f:
        data = f.read().splitlines()
    sensor = namedtuple('sensors', ['sens', 'dist'])
    allSensors = []
    for n in data:
        beac = re.findall("\\-?[0-9]+", n)
        x1, y1, x2, y2 = [int(i) for i in beac]
        
        allSensors.append(sensor([x1,y1], abs(x2 - x1) + abs(y2 - y1)))
        
    return allSensors

def testRow(rn, sensors):
  coveredSpace = np.empty(0, dtype = np.int8)
  for s in sensors:
    dToRn = abs(s.sens[1] - rn)
    if dToRn < s.dist:
      xr = s.dist - dToRn 
      coveredSpace = np.append(coveredSpace, range(s.sens[0] - xr, s.sens[0] + xr + 1))
  return np.unique(coveredSpace)

# find circumference points
def findCircPoints(sensors, minCo, maxCo):
  g = set()
  
  for s in sensors:

    for n in range(-(s.dist + 1), s.dist + 2):
      xpos = s.sens[0] + n
      if minCo <= xpos <= maxCo:
        y1 = s.sens[1] + (s.dist + 1 - abs(n))
        if minCo <= y1 <= maxCo:
          g.add((xpos, y1))
        
        y2 = s.sens[1] - (s.dist + 1 - abs(n))
        if minCo <= y2 <= maxCo:
          g.add((xpos, y2))
    
  return sorted(g)
  
# test a set of points against the sensors,
#if outside of distance for all sensors then finished
def testPoints(sensors, points):

  for p in points:
    h = []
    for s in sensors:
      if manDist(p, s.sens) > s.dist:
        h.append(True)
      
    if(len(h) == len(sensors)):
      return(p)
      
  
  return "E"  
        

if __name__ == "__main__":
    allSensors = read_data("./day15/input.txt")
    testRow(11, allSensors)
    #validPoints = findCircPoints(allSensors,0 ,20)
    #print(validPoints)
    #testPoints(allSensors, validPoints)
    

    validPoints = findCircPoints(allSensors, 0, 4000000)
    print(testPoints(allSensors, validPoints))

