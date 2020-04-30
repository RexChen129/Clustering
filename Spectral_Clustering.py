import csv
import math
import numpy as np
import scipy as sp
from scipy.linalg import eigh
from numpy.linalg import norm
import matplotlib.pyplot as plt
from sklearn.neighbors import NearestNeighbors

with open('/Users/macbookpro/Desktop/IE529_Comp2/Dataset_1/clustering.csv', 'r') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    x = list(reader)
    data_1 = np.array(x).astype("float")

with open('/Users/macbookpro/Desktop/IE529_Comp2/Dataset_2/ShapedData.csv', 'r') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    x = list(reader)
    data_2 = np.array(x).astype("float")

# Gaussian similarity function
# For example, one can choose σ in the order of the mean distance of a point to its k-th nearest neighbor,
# where k is chosen similarly as above (e.g., k ∼ log(n) + 1 ). like σ=5
def adj_generate(X, gamma):
    n = X.shape[0]
    a = np.zeros([n,n])
    for i in range(n):
        for j in range(n):
            a[i,j] = math.exp(-math.pow(norm((X[i]-X[j]),2), 2) * gamma)
    return a

# k-nearest neighborhood structure
# k -> log(n) -> 3 or 4
def adj_generate_KNN(X, k):
    n = X.shape[0]
    a = np.zeros([n,n])
    neigh = NearestNeighbors(n_neighbors=k)
    neigh.fit(X)
    idx = neigh.kneighbors(X,  n_neighbors=k, return_distance=False)
    for i in range(n):
        for j in range(n):
            if j in idx[i]:
                a[i,j] = 1
    return a

def diag_generate(a):
    n = a.shape[0]
    d = np.sum(a, axis = 1)
    return np.diagflat(d)

# Randomly initialize centroids
def centroids_init(matrix, K):
    index = np.random.randint(low=0, high=len(matrix[:,0]), size=K)
    centroids = matrix[index]
    return centroids


def find_closest_centroids(matrix, centroids):
    # Set m
    m = centroids.shape[0]

    # initialize distance matrix
    distance = np.zeros((matrix.shape[0], m))

    for i in range(matrix.shape[0]):
        for j in range(m):
            #             distance[i][j] = math.sqrt((centroids[j][0] - matrix[i][0])**2 + (centroids[j][1] - matrix[i][1])**2)
            distance[i, j] = math.pow(norm((matrix[i, :] - centroids[j, :]), 2), 0.5)
    idx = np.argmin(distance, axis=1)
    #     new_centroids = centroids[idx]
    return idx

def compute_centroids(matrix, idx, K):
    temp = []
    for j in range(matrix.shape[1]):
        a = matrix[np.where(idx == 0)][:,j].sum()
        b = len(matrix[np.where(idx == 0)])
        temp.append(a/b)
    centroids = np.array([temp])
    for i in range(1, K):
        index = matrix[np.where(idx == i)]
        temp = []
        for i in range(matrix.shape[1]):
            temp.append(index[:,i].sum()/len(index))
        centroids = np.concatenate((centroids, np.array([temp])))
    return centroids

def compute_distortion(matrix, idx, centroids, K):
    distance = []
    for i in range(K):
        group = matrix[np.where(idx == i)]
        for j in range(group.shape[0]):
            distance.append(math.pow(norm((centroids[i,:]-group[j,:]),2), 2))
    distortion = 0
    for i in distance:
        distortion += i
    return distortion

X = data_2
K = 6
gamma = 1

# Adjacency matrix
A = adj_generate(X, gamma)
# degree-diagonal matrix
D = diag_generate(A)


# Unnormalized clustering, Lapalacian matrix
L = D - A

# # Normalized Lapalacian matrix
# def normalize_adj(A, D):
#     d_inv_sqrt = np.power(D, -0.5)
#     d_inv_sqrt[np.isinf(d_inv_sqrt)] = 0.
#     L = sp.eye(D.shape[0]) - d_inv_sqrt.dot(A.dot(d_inv_sqrt))
#     return L
# L = normalize_adj(A, D)

eigValue, U = np.asarray(eigh(L, eigvals=(1,K)))

# parameters initialization
max_iter = 300
val = [0.001, 10**(-5)] # val = 10**-5

centroids = centroids_init(U, K)

for _ in range(max_iter):
    idx = find_closest_centroids(U, centroids)
    new_centroids = compute_centroids(U, idx, K)
    d = compute_distortion(U, idx, centroids, K)
    d_new = compute_distortion(U, idx, new_centroids, K)
    if abs(d_new - d) < val[1]:
        break
    else:
        centroids = new_centroids
        new_centroids = compute_centroids(U, idx, K)

plt.plot(K_list, inertia, 'g--')
K_list = K_list[::-1]
inertia = inertia[::-1]
plt.scatter(K_list, inertia, c='r', marker = 'D')
plt.savefig('D_K.svg',format='svg')
plt.show()