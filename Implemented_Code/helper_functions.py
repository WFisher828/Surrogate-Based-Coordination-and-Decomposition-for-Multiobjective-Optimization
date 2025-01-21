import gurobipy as gp
from gurobipy import GRB
import numpy as np


############################################################################
### START FUNCTIONS RELATED TO BIPARTITE SPECTRAL GRAPH DECOMPOSITION   ####
############################################################################

#This function calculates the sensitivity score of each variable in a model, assuming the model
#is a quadratic effects model and the underlying distribution of the variables are all
#i.i.d unif(0,1).
#We assume the input model has the form:
#     mu(x) = beta_0 + beta_1 x_1 + beta_2 (x_1)**2 + ... + beta_{2j-1} x_j + beta_{2j} x_j**2 + ... + beta_{2m-1} x_m + beta_{2m} x_m**2
def sensitivity_quad_unif(betas, tolerance = 10**(-9)):
    #betas: This is a list of betas [beta_1,beta_2,...,beta_{2m-1},beta_{2m}]. Note that we do not include the intercept term as it is not associated with
    #any variables.
    #tolerance: This is a cutoff value for what we consider to be numberical 0. May need to be adjusted depending on the application.

    num_vars = int(len(betas)/2)
    #This list will hold the values Var(beta_{2j-1}x_j + beta_{2j} x_j**2)
    variability = []
    
    for i in range(num_vars):
        beta_lin = betas[2*i]
        beta_quad = betas[2*i + 1]
        if abs(beta_quad) > tolerance:
            var_x_i = (beta_quad**2)*( ((beta_lin/(2*beta_quad) + 1)**5 - (beta_lin/(2*beta_quad))**5)/5 - (( (beta_lin/(2*beta_quad) + 1)**3 -(beta_lin/(2*beta_quad))**3)/3)**2 )
        else:
            var_x_i = (beta_lin**2)/12
        variability.append(var_x_i)

    sum_of_var = sum(variability)
    sensitivity_score = [variability[j]/sum_of_var for j in range(num_vars)]

    return sensitivity_score

#This function constructs the edge set of the biclustered bipartite graph, E_C.
def construct_E_C(C_f,C_x):
    #C_f: A KxJ array of True/False values where C_f[k][j] is True if surrogate function f_j is in cluster k. This should be attained from
    #a spectral coclustering fit.
    #C_x: A KxI array of True/False values where C_x[k][i] is True if input variable x_i is in cluster k. This should be attained from
    #a spectral coclustering fit.

    E_C = []

    I = len(C_x[0])
    J = len(C_f[0])
    K = len(C_f)
    

    for k in range(K):
        for j in range(J):
            for i in range(I):
                if C_f[k][j] and C_x[k][i]:
                    E_C.append([i,j])

    return E_C

    

#This function allows one to re-add edges into a coclustered bipartite graph
#in such a manner so that the minimum amount of edges and cluster crossings are
#added while recovering a user-set threshold for the minimum sensitivity score (sum of sensitivity scores of input variables for function f_j)
#needed within each surrogate function f_j.
#See shared document for more details.
def sens_recovery(S,deltas,C_f,C_x,E_C,lam):
    #S: This is a JxI array (list of lists) of sensitivity scores. S[j][i] is the sensitivity of surrogate function f_j to input variable x_i.
    #deltas: This is a Jx1 array of sensitivity thresholds for each surrogate function f_j.
    #C_f: A KxJ array of True/False values where C_f[k][j] is True if surrogate function f_j is in cluster k.
    #C_x: A KxI array of True/False values where C_x[k][i] is True if input variable x_i is in cluster k.
    #E_C: This is a list of lists, where the inner lists are 2x1 ([i,j]) which represent an edge between a surrogate function f_j and input variable x_i, if
    #such an edge exists in the initial bipartite graph coclustering.
    #lam: This is a parameter which controls how much the adding of a new edge is penalized. (NEED TO INVESTIGATE lam's properties further, i.e. are solutions
    #sensitive to the value of lam?
    
    I = len(S[0])
    J = len(deltas)
    K = len(C_x)

    model = gp.Model("recover")
    alpha_indices = [(k_1,k_2) for k_1 in range(K) for k_2 in range(k_1 + 1,K)]
    alpha = model.addVars(alpha_indices,vtype=GRB.BINARY, name = "alpha")
    Z = model.addMVar((J,I), vtype = GRB.BINARY, name = "Z")
    
    model.setObjective(sum([alpha[k_1,k_2] for k_1 in range(K) for k_2 in range(k_1 + 1,K)]) + lam*sum([(1.0-S[j][i])*Z[j,i] for j in range(J) for i in range(I)]),
                      GRB.MINIMIZE)

    for j in range(J):
        model.addConstr(sum([S[j][i]*Z[j,i] for i in range(I)]) >= deltas[j])

    for edge in E_C:
        model.addConstr(Z[edge[1],edge[0]] == 1)

    for k_1 in range(K):
        for k_2 in range(k_1 + 1, K):
            for j in range(J):
                for i in range(I):
                    if C_f[k_1][j] and C_x[k_2][i]:
                        model.addConstr(alpha[k_1,k_2] >= Z[j,i])
                    if C_f[k_2][j] and C_x[k_1][i]:
                        model.addConstr(alpha[k_1,k_2] >= Z[j,i])

    model.optimize()

    alpha_sol = [[] for k in range(K-1)]
    for k_1 in range(K):
        for k_2 in range(k_1 + 1, K):
            alpha_sol[k_1].append(alpha[k_1,k_2].X)
    
    Z_sol = Z.X

    return[Z_sol,alpha_sol]
            

    


#########################################################################
###   START DECOMPOSITION ORIENTED PENALIZED REGRESSION FUNCTIONS     ###
#########################################################################

#This function calculates the parameters of a set of linear surrogate models through a penalized regression procedure that promotes clustering
#of variables and surrogate models into blocks for multi-objective decomposition. This particular function is the Decomposition Oriented Ridge-Type Penalized Regression.
#We assume each of J surrogate models has the form:
#f_j(x) = h(x)*beta_j + eps.
#That is, there is a common deterministic component h(x) (we call this the master model, which is a functional Mx1 vector) and the difference between models is that we have different parameters beta_j.
#We assume that N input-output pairs have been gathered on each of the J objective functions as training data (each of the N inputs is evaluated at all J of the objective functions).
#We further assume that the number of clusters, K, is given by the user.
def decomp_ridge_regress(F,H,Z,gamma,lam,kappa,t = 50):
    #F - This is a JxN numpy matrix of responses where F_jn is the nth response of objective function j.
    #H - This is a NxM numpy matrix where H_nm is the mth component of h(x) evaluated at the nth input data x_n.
    #Z - This is a JxK numpy matrix where Z_jk is equal to 1 if objective function j is in cluster K, and 0 otherwise. Note that the rows of this matrix
    #should sum to 1. Ideally, this matrix should also satisfy that clusters should have roughly equal size. So we suggest that the sum of the entries in each
    #column should sum to a value between floor(J/K) and ceil(J/K). We will enforce this later.
    #gamma - This is a parameter which controls the importance of the L2 loss function. Should be greater than 0.
    #lam - This is a parameter which controls the importance of the ridge penalty term. Should be greater than 0.
    #kappa - This is a parameter which control the importance of the orthogonality penalty term. Should be greater than 0.
    #t - amount of time which should be spent for solving the problem.

    J = len(F)
    N = len(H)
    K = len(Z.T)
    M = len(H.T)

    model = gp.Model("decomp_ridge")
    model.setParam('Timelimit', t)
    Beta = model.addMVar((J,M),lb=-GRB.INFINITY, ub=GRB.INFINITY, vtype=GRB.CONTINUOUS, name="Beta")
    W = model.addMVar((J,M),lb=0.0, ub=GRB.INFINITY, vtype=GRB.CONTINUOUS, name="W")

    model.setObjective( gamma*sum([(F[j,n] - sum([H[n,m]*Beta[j,m] for m in range(M)]))*(F[j,n] - sum([H[n,m]*Beta[j,m] for m in range(M)])) for j in range(J) for n in range(N)]) +
                      lam*sum([W[j,m] for j in range(J) for m in range(M)]) + 
                      kappa*sum([ (1 - sum([Z[j,k]*Z[j_2,k] for k in range(K)])) * sum([W[j,m]*W[j_2,m] for m in range(M)])
                                 for j in range(J) for j_2 in range(j+1,J)]),GRB.MINIMIZE)
    for j in range(J):
        for m in range(M):
            model.addConstr(Beta[j,m]*Beta[j,m] - W[j,m] == 0.0 )

    model.optimize()

    Beta_sol = Beta.X
    W_sol = W.X

    return [Beta_sol,W_sol]

#This is the objective function for the decomposition oriented lasso-type penalized regression.
def decomp_lasso_objective(Beta,F,H,Z,gamma,lam,kappa):
    #Beta - This is a JxM numpy matrix of parameters where Beta_jm is the coefficient of the mth component of h(x) for objective function j.
    #F - This is a JxN numpy matrix of responses where F_jn is the nth response of objective function j.
    #H - This is a NxM numpy matrix where H_nm is the mth component of h(x) evaluated at the nth input data x_n.
    #Z - This is a JxK numpy matrix where Z_jk is equal to 1 if objective function j is in cluster K, and 0 otherwise. Note that the rows of this matrix
    #should sum to 1. Ideally, this matrix should also satisfy that clusters should have roughly equal size. So we suggest that the sum of the entries in each
    #column should sum to a value between floor(J/K) and ceil(J/K). We will enforce this later.
    #gamma - This is a parameter which controls the importance of the L2 loss function. Should be greater than 0.
    #lam - This is a parameter which controls the importance of the ridge penalty term. Should be greater than 0.
    #kappa - This is a parameter which control the importance of the orthogonality penalty term. Should be greater than 0.

    J = len(F)
    N = len(H)
    K = len(Z.T)
    M = len(H.T)
            
    objective_val = gamma*sum([(F[j,n] - sum([Beta[j,m]*H[n,m] for m in range(M)]))**2 for j in range(J) for n in range(N)]) + lam*sum([abs(Beta[j,m]) for j in range(J) for m in range(M)]) + kappa*sum([ (1 - sum([Z[j,k]*Z[j_2,k] for k in range(K)])) * sum([abs(Beta[j,m])*abs(Beta[j_2,m]) for m in range(M)]) for j in range(J) for j_2 in range(j+1,J)])

    return objective_val

#This function is used to optimize the decomp_lasso objective function above with respect to one coefficient Beta[j_var,m_var] assuming all other coefficients are held constant. The decomp_lasso objective function is differentiable everywhere except for 0, and thus we only need to search for optimal solutions in three regions: Beta[j_var,m_var]<0, Beta[j_var,m_var]=0, Beta[j_var,m_var]>0.
def decomp_lasso_one_coord_opt(Beta,F,H,Z,gamma,lam,kappa,j_var,m_var):
    #Beta - This is a JxM numpy matrix of parameters where Beta_jm is the coefficient of the mth component of h(x) for objective function j.
    #F - This is a JxN numpy matrix of responses where F_jn is the nth response of objective function j.
    #H - This is a NxM numpy matrix where H_nm is the mth component of h(x) evaluated at the nth input data x_n.
    #Z - This is a JxK numpy matrix where Z_jk is equal to 1 if objective function j is in cluster K, and 0 otherwise. Note that the rows of this matrix
    #should sum to 1. Ideally, this matrix should also satisfy that clusters should have roughly equal size. So we suggest that the sum of the entries in each
    #column should sum to a value between floor(J/K) and ceil(J/K). We will enforce this later.
    #gamma - This is a parameter which controls the importance of the L2 loss function. Should be greater than 0.
    #lam - This is a parameter which controls the importance of the ridge penalty term. Should be greater than 0.
    #kappa - This is a parameter which control the importance of the orthogonality penalty term. Should be greater than 0.
    #j_var,m_var - These are indices denoting the objective function (j_var) and component of h(x) (m_var) for which we are optimizing the corresponding 
    #coefficient Beta[j_var,m_var].

    Beta_temp = Beta.copy()

    J = len(F)
    N = len(H)
    K = len(Z.T)
    M = len(H.T)

    #This is all components of the decomp_lasso objective function which have nothing to do with Beta[j_var,m_var]
    C = gamma*sum([(F[j,n] - sum([Beta_temp[j,m]*H[n,m] for m in range(M)]))**2 for j in range(J) for n in range(N) if j != j_var]) + lam*sum([abs(Beta_temp[j,m]) for j in range(J) for m in range(M) if (j != j_var or m != m_var)]) + kappa*sum([(1-sum([Z[j,k]*Z[j_2,k] for k in range(K)]))*sum([abs(Beta_temp[j,m])*abs(Beta_temp[j_2,m]) for m in range(M)]) for j in range(J) for j_2 in range(j+1,J) if (j != j_var and j_2 != j_var)])
    
    #Case 1: Beta[j_var,m_var]=0
    case_1_sol = 0.0
    Beta_temp[j_var,m_var] = 0.0
    case_1_obj_val = decomp_lasso_objective(Beta_temp,F,H,Z,gamma,lam,kappa)
    print(case_1_obj_val)

    #Case 2: Beta[j_var,m_var]<0
    case_2_sol = (-C + lam + kappa*sum([(1-sum([Z[j,k]*Z[j_var,k] for k in range(K)]))*abs(Beta_temp[j,m_var]) for j in range(J) if j != j_var]) - gamma*sum([2.0*H[n,m_var]*F[j_var,n] for n in range(N)]) + gamma*sum([2*H[n,m_var]*Beta[j_var,m]*H[n,m] for n in range(N) for m in range(M) if m != m_var]))/(2*gamma*sum([H[n,m_var]**2 for n in range(N)]))
    print(case_2_sol)
    
    Beta_temp[j_var,m_var] = (-C + lam + kappa*sum([(1-sum([Z[j,k]*Z[j_var,k] for k in range(K)]))*abs(Beta_temp[j,m_var]) for j in range(J) if j != j_var]) - gamma*sum([2.0*H[n,m_var]*F[j_var,n] for n in range(N)]) + gamma*sum([2*H[n,m_var]*Beta[j_var,m]*H[n,m] for n in range(N) for m in range(M) if m != m_var]))/(2*gamma*sum([H[n,m_var]**2 for n in range(N)]))

    case_2_obj_val = decomp_lasso_objective(Beta_temp,F,H,Z,gamma,lam,kappa)
    print(case_2_obj_val)

    #Case 3: Beta[j_var,m_var]>0
    case_3_sol = (-C - lam - kappa*sum([(1-sum([Z[j,k]*Z[j_var,k] for k in range(K)]))*abs(Beta_temp[j,m_var]) for j in range(J) if j != j_var]) + gamma*sum([2.0*H[n,m_var]*F[j_var,n] for n in range(N)]) - gamma*sum([2*H[n,m_var]*Beta[j_var,m]*H[n,m] for n in range(N) for m in range(M) if m != m_var]))/(2*gamma*sum([H[n,m_var]**2 for n in range(N)]))
    print(case_3_sol)
    
    Beta_temp[j_var,m_var] = (-C - lam - kappa*sum([(1-sum([Z[j,k]*Z[j_var,k] for k in range(K)]))*abs(Beta_temp[j,m_var]) for j in range(J) if j != j_var]) + gamma*sum([2.0*H[n,m_var]*F[j_var,n] for n in range(N)]) - gamma*sum([2*H[n,m_var]*Beta[j_var,m]*H[n,m] for n in range(N) for m in range(M) if m != m_var]))/(2*gamma*sum([H[n,m_var]**2 for n in range(N)]))

    case_3_obj_val = decomp_lasso_objective(Beta_temp,F,H,Z,gamma,lam,kappa)
    print(case_3_obj_val)

    arg_min = np.argmin(np.array([case_1_obj_val, case_2_obj_val, case_3_obj_val]))

    if arg_min == 0:
        sol = case_1_sol
    if arg_min == 1:
        sol = case_2_sol
    if arg_min == 2:
        sol = case_3_sol

    return sol


###START DECOMPOSITION ORIENTED NON-NEGATIVE GARROTE ESTIMATOR###

#This is a simple least-squares estimator
def LSE(X,Y):
    #X: An NxM matrix numpy matrix representing the model matrix
    #Y: An Nx1 numpy array representing the responses.

    return np.linalg.inv(X.T @ X)@(X.T)@Y

def construct_R(X,Y):
    #X: An NxM numpy matrix representing the model matrix
    #Y: An Nx1 numpy array representing the responses.
    
    N = len(X)
    M = len(X.T)
    
    lse = LSE(X,Y)
    #print(lse)

    R = np.zeros((N,M))

    for n in range(N):
        for m in range(M):
            R[n,m] = X[n,m]*lse[m]

    return R,lse

def decomp_non_neg_garrote(F,R,Z,D,gamma,lam,kappa,t = 50):
    #F - This is a JxN numpy matrix of responses where F_jn is the nth response of objective function j.
    #R - This is a list with J entries, each entry is a NxM numpy matrix where R[j]_nm is X_nm*Beta_jm. We will typically assume a response surface model for X.
    #Z - This is a JxK numpy matrix where Z_jk is equal to 1 if objective function j is in cluster K, and 0 otherwise. Note that the rows of this matrix
    #should sum to 1. Ideally, this matrix should also satisfy that clusters should have roughly equal size. So we suggest that the sum of the entries in each
    #column should sum to a value between floor(J/K) and ceil(J/K). We will enforce this later.
    #D - This is a list of lists of lists. There are J second layer lists, and within each second layer list there are M lists. List D[j][m] holds information on which predictors should be included in the function j if predictor m in function j is included. For example in the strong heredity case, For function 1 if predictor 1 and predictor 2 are x_1 and x_2, and predictor 3 is x_1*x_2, predictor 4 is x_1^2 and predictor 5 is x_2^2, Then D[0][0] = [], D[0][1] = [], D[0][2] = [0,1], D[0][3] = [0], D[0][4] = [1]
    #gamma - This is a parameter which controls the importance of the L2 loss function. Should be greater than 0.
    #lam - This is a parameter which controls the importance of the ridge penalty term. Should be greater than 0.
    #kappa - This is a parameter which control the importance of the orthogonality penalty term. Should be greater than 0.
    #t - amount of time which should be spent for solving the problem.

    
    J = len(F)
    N = len(R[0])
    K = len(Z.T)
    M = len(R[0].T)

    model = gp.Model("decomp_garrote")
    model.setParam('Timelimit', t)
    Theta = model.addMVar((J,M),lb=0.0, ub=GRB.INFINITY, vtype=GRB.CONTINUOUS, name="Theta")

    model.setObjective( gamma*sum([(F[j,n] - sum([R[j][n,m]*Theta[j,m] for m in range(M)]))*(F[j,n] - sum([R[j][n,m]*Theta[j,m] for m in range(M)])) for j in range(J) for n in range(N)]) + lam*sum([Theta[j,m] for j in range(J) for m in range(M)]) + kappa*sum([ (1 - sum([Z[j,k]*Z[j_2,k] for k in range(K)])) * sum([Theta[j,m]*Theta[j_2,m] for m in range(M)]) for j in range(J) for j_2 in range(j+1,J)]),GRB.MINIMIZE)

    for j in range(J):
        for m in range(M):
            for d in D[j][m]:
                model.addConstr(Theta[j,m] <= Theta[j,d])

    model.optimize()

    return [Theta.X,model.objVal]

#THIS IS THE MODEL WE WILL USE! IT DOES NOT RELY ON PARAMETER TUNINING OR COORDINATE EXCHANGE!
def decomp_orthog(F,R,D,K,t=50,theta_ub = 20.0,focus = 0):
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
    