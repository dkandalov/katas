from sklearn import datasets, neighbors, linear_model
import numpy as np
import pylab as pl


# from http://docs.scipy.org/doc/numpy/reference/arrays.indexing.html
# x = np.array([[1, 2], [3, 4], [5, 6]])
# print(x[[0, 1, 2], [0, 1, 0]])  # [1 4 5]


digits = datasets.load_digits()

train_size = len(digits.data) * 0.9
test_size = len(digits.data) - train_size

X_train = digits.data[:train_size]
y_train = digits.target[:train_size]
X_test = digits.data[train_size:]
y_test = digits.target[train_size:]
print(y_test)

c = neighbors.KNeighborsClassifier()
c.fit(X_train, y_train)
y_test_actual = c.predict(X_test)
print(y_test_actual)

c = linear_model.LogisticRegression()
c.fit(X_train, y_train)
y_test_actual = map(int, map(round, c.predict(X_test)))
print(y_test_actual)


# print(type(digits.data))    # <type 'numpy.ndarray'>
# print(type(digits.images))  # <type 'numpy.ndarray'>
# print(type(digits.target))  # <type 'numpy.ndarray'>
# ---
# print(digits.data.size)    # 115008
# print(digits.images.size)  # 115008
# print(digits.target.size)  # 1797
# ---
# print(len(digits.data))    # 1797
# print(len(digits.images))  # 1797
# print(len(digits.target))  # 1797


# for it in digits.data[:]:
#     print(it.size)

# pl.gray()
# pl.matshow(digits.images[0])
# pl.show()
