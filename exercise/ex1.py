import numpy as np

amin = float(input("Input the start value: ") or 1)
amax = float(input("Input the end value: ") or 100)

sum = 0
for i in (np.arange(amin, amax+1, 1)):
    sum += i

print(f"Summation from {amin} to {amax} = {sum}")