from curses import doupdate
import numpy as np
import warnings
warnings.filterwarnings("ignore")

double = 0
print(f"Precision: {'Double' if double else 'Single'}")
under = np.double(1.0) if double else np.single(1.0)
over = np.double(1.0) if double else np.single(1.0)
factor = np.double(2.0) if double else np.single(2.0)

N = 0
while True:
    N += 1
    under /= factor
    over *= factor
    if under/factor == 0 or over*factor == float('inf'):
        print(f"N, under, over = {N} {under} {over}")
    if under == 0 and over == float('inf'):
        break