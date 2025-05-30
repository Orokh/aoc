result = 0

left = []
right = []

with open("input.txt", "r") as file:
    for line in file:
        values = line.split()
        left.append(int(values[0]))
        right.append(int(values[1]))

left.sort()
right.sort()

for a, b in zip(left, right):
    result += abs(a - b)

print(f"part1: {result}")


nb_occurs = dict()

for val in set(right):
    nb_occurs[val] = right.count(val)

result = 0

for a in left:
    if a in nb_occurs:
        result += a * nb_occurs[a]

print(f"part2: {result}")
