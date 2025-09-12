import gurobipy as gp
from gurobipy import GRB
import numpy as np

###START DECOMPOSITION ORIENTED NON-NEGATIVE GARROTE ESTIMATOR###

#This is a simple least-squares estimator
def LSE(X,Y):
    #X: An NxM matrix numpy matrix representing the model matrix
    #Y: An Nx1 numpy array representing the responses.

    return np.linalg.inv(X.T @ X)@(X.T)@Y

def construct_R(X,Y,const = 10**(-6)):
    #X: An NxM numpy matrix representing the model matrix
    #Y: An Nx1 numpy array representing the responses.
    #const: A small constant to ensure numerical stability of the decompostion algorithm later on.
    
    N = len(X)
    M = len(X.T)
    
    lse = LSE(X,Y)
    #print(lse)

    R = np.zeros((N,M))

    for n in range(N):
        for m in range(M):
            R[n,m] = X[n,m]*lse[m] + const

    return R,lse

#THIS IS THE MODEL WE WILL USE! IT DOES NOT RELY ON PARAMETER TUNINING OR COORDINATE EXCHANGE!
def decomp_orthog(F,R,D,K,t=50,theta_ub = 20.0,focus = 0,outputflag = 0):
    #F - This is a JxN numpy matrix of responses where F_jn is the nth response of objective function j.
    #R - This is a list with J entries, each entry is a NxM numpy matrix where R[j]_nm is X_nm*Beta_jm. We will typically assume a response surface model for X.
    #D - This is a list of lists of lists. There are J second layer lists, and within each second layer list there are M lists. List D[j][m] holds information on which predictors should be included in the function j if predictor m in function j is included. For example in the strong heredity case, For function 1 if predictor 1 and predictor 2 are x_1 and x_2, and predictor 3 is x_1*x_2, predictor 4 is x_1^2 and predictor 5 is x_2^2, Then D[0][0] = [], D[0][1] = [], D[0][2] = [0,1], D[0][3] = [0], D[0][4] = [1]
    #K - This is the number of clusters.
    #theta_ub: This value is an arbitrary upper bound for the theta values. We add this because Gurobi gave a warning about theta's being large when involved in product terms.

    J = len(F)
    N = len(R[0])
    M = len(R[0].T)

    model = gp.Model("decomp_orthog")
    model.setParam('Timelimit', t)
    model.setParam('MIPFocus', focus)
    model.setParam('OutputFlag', outputflag)

    Theta = model.addMVar((J,M),lb=0.0, ub=theta_ub, vtype=GRB.CONTINUOUS, name="Theta")
    Z = model.addMVar((J,K), vtype=GRB.BINARY, name = "Z")

    spec_indices = [(j_1,j_2) for j_1 in range(J) for j_2 in range(j_1 + 1,J)]
    Nu = model.addVars(spec_indices, lb = 0.0, ub = theta_ub**2, vtype = GRB.CONTINUOUS, name = "Nu") 
    Xi = model.addVars(spec_indices, vtype = GRB.BINARY, name = "Xi")

    model.setObjective(gp.quicksum([(F[j,n] - gp.quicksum([R[j][n,m]*Theta[j,m] for m in range(M)]))*(F[j,n] - gp.quicksum([R[j][n,m]*Theta[j,m] for m in range(M)])) for j in range(J) for n in range(N)]),GRB.MINIMIZE)

    #Clusters are non-empty constraint
    for k in range(K):
        model.addConstr(gp.quicksum([Z[j,k] for j in range(J)]) >= 1)

    #Every function is assigned to only one cluster
    for j in range(J):
        model.addConstr(gp.quicksum([Z[j,k] for k in range(K)]) == 1)
    
    #Strong heredity constraints
    for j in range(J):
        for m in range(M):
            for d in D[j][m]:
                model.addConstr(Theta[j,m] <= Theta[j,d])

    #Constraints encoding whether f_{j_1} and f_{j_2} are in the same cluster
    for j_1 in range(J):
        for j_2 in range(j_1 + 1, J):
            model.addConstr(Xi[j_1,j_2] == 1 - gp.quicksum([Z[j_1,k]*Z[j_2,k] for k in range(K)]))

    #Constraints encoding orthogonality between f_{j_1} and f_{j_2}.
    for j_1 in range(J):
        for j_2 in range(j_1 + 1, J):
            model.addConstr(Nu[j_1,j_2] == gp.quicksum([Theta[j_1,m]*Theta[j_2,m] for m in range(M)]))

    #Constraints enforcing orthgonality between f_{j_1} and f_{j_2} if they are in different clusters.
    for j_1 in range(J):
        for j_2 in range(j_1 + 1, J):
            model.addConstr(Xi[j_1,j_2]*Nu[j_1,j_2] == 0)
    

    model.optimize()

    return [Theta.X,Z.X,model.objVal]
    