import numpy
import operator
import re

ops = { "+": operator.add, "-": operator.sub, "*": operator.mul}

def read_data(fp: str = "./day11/input.txt"):
    with open(fp) as f:
        data = f.read().splitlines()
    monkeys = []
    monkeymod = 1
    for item in chunker(data, 7):
        dic1 = data_to_dict(item)
        monkeys.append(Monkey(dic1))
        monkeymod *= int(dic1["divisor"])
    return monkeys

def data_to_dict(item):
    output = dict()
    output["id"] = item[0][-2]
    heldItems = item[1].split(":")[1].split(",")
    heldItems = [int(x.replace(" ", "")) for x in heldItems]
    output["heldItems"] = heldItems
    output["operation"] = ops[re.findall(r'[\+\*\-]+', item[2])[0]]
    if(item[2].count("old") == 2):
        output["opValue"] = "square"
    else:
        output["opValue"] = int(re.findall(r'\d+', item[2])[0])

    output["divisor"] = int(re.findall(r'\d+', item[3])[0])
    output["trueMonkey"] = int(re.findall(r'\d+', item[4])[0])
    output["falseMonkey"] = int(re.findall(r'\d+', item[5])[0])

    return(output)

def chunker(seq, size):
    return (seq[pos:pos + size] for pos in range(0, len(seq), size))

class Monkey:
    def __init__(monk, dict):
        monk.id = dict["id"]
        monk.heldItems = dict["heldItems"]
        monk.operation = dict["operation"]
        monk.opValue = dict["opValue"]
        monk.divisor = dict["divisor"]
        monk.trueMonkey = dict["trueMonkey"]
        monk.falseMonkey = dict["falseMonkey"]
        monk.inspectCounter = 0

    def processItems(monk):
        for item in monk.heldItems:
            # apply operation
            if monk.opValue == "square":
                item = monk.operation(item, item)
            else:
                item = monk.operation(item, monk.opValue)
            if round2:
                item = item % monkeymod
            else:
                item = item // 3
            # now check condition
            if item % monk.divisor == 0:
                monk.trueMonkey.receiveItem(item)
            else:
                monk.falseMonkey.receiveItem(item)
            monk.inspectCounter += 1
        #remove all items
        monk.heldItems = []



    def assignPartner(monk, all_monkeys):
        monk.trueMonkey = all_monkeys[monk.trueMonkey]
        monk.falseMonkey = all_monkeys[monk.falseMonkey]

    def receiveItem(monk, item):
        monk.heldItems.append(item)

if __name__ == "__main__":
    allMonkeys = read_data()
    monkeymod = 1

    for m in allMonkeys:
        m.assignPartner(allMonkeys)
        monkeymod *= m.divisor

    round2 = True
    i = 1
    while i <= 10000:
        for m in allMonkeys:
            m.processItems()
        i += 1

    monkey_stuff = sorted([x.inspectCounter for x in allMonkeys], reverse=True)
    print(f"score: {monkey_stuff[0] * monkey_stuff[1]}")
    print(allMonkeys[0].operation)



