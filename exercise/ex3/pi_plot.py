import pandas as pd
import matplotlib.pyplot as plt

df = pd.read_csv("pi_error.dat", sep="   ")

plt.loglog(df["N"], df["Rel_err"])
plt.xlabel("Number of iterations")
plt.ylabel("Relative error")
plt.title("Calculate for $\pi$")
plt.savefig("Pi_logN_logErr")
