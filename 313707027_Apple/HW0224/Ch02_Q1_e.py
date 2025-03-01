import numpy as np
import matplotlib.pyplot as plt

x = np.array([3, 2, 1, -1, 0])
y = np.array([4, 2, 3, 1, 0])


x_mean = np.mean(x)
y_mean = np.mean(y)


b2 = np.sum((x - x_mean) * (y - y_mean)) / np.sum((x - x_mean) ** 2)
b1 = y_mean - b2 * x_mean


y_pred = b1 + b2 * x


plt.figure(figsize=(6, 6))
plt.scatter(x, y, color='black', label='Data Points')  
plt.plot(x, y_pred, color='red', label=f'Regression Line: $y = {b1:.2f} + {b2:.2f}x$')  


for i in range(len(x)):
    plt.text(x[i] + 0.1, y[i], f'({x[i]},{y[i]})', fontsize=10, verticalalignment='bottom')


plt.scatter(x_mean, y_mean, color='blue', s=100, label='Mean Point')
plt.text(x_mean + 0.1, y_mean, f'({x_mean:.0f},{y_mean:.0f})', fontsize=10, color='blue')


plt.xlabel("x")
plt.ylabel("y")
plt.title("Scatter Plot and Regression Line")
plt.axhline(0, color='black', linewidth=0.5)
plt.axvline(0, color='black', linewidth=0.5)
plt.legend()
plt.show()
