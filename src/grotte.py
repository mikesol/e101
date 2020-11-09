import random

a = [random.random() for x in range(40)]
a = [sum(a[:x], 0.0) for x in range(len(a))]
for x in a:
  print("Tuple %f %s," % (x, "gesture"+str(random.randint(0,13))))