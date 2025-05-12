library(lpSolveAPI)

# DECISION VARIABLES

# x_{i,j} number of cluents supplied from provider i in region j
# i in {p,e,s}
#j in {1,2,3,4,5}

#OBJECTIVE FUNCTION

# 6.5 * x_{p,1} + 7 * x_{p,2} + 8.25 * x_{p,3} 
# + 7.5 * x_{e,1} + 8 * x_{e,2} + 7.25 * x_{e,3} + 7.75 * x_{e,4} + 7.5 * x_{e,5}
#+ 6.75 * x_{s,3} + 7 * x_{s,4} + 6.75 * x_{s,5}



#$$ minimize (6.5,7,8.25,7.5,8,7.25,7.75, 7.5,6.75,7,6.75) (y_{1} y{2} y_{3} y_{4} y_{5} y_{6} y_{7} y_{8} y_{9})^t

edges <- data.frame(index_i=  c("p","p","p",  "e","e","e","e","e",  "s","s","s" ),
                     index_j =c( 1,  2,  3,    1,  2,  3,  4,  5,    3,  4,  5),
                     coeff = c (6.5,7, 8.25, 7.5, 8, 7.25,7.75,7.5, 6.75,7,6.75))

edges

model <- make.lp(0,11)

name.lp(model, "Problem 2")

lp.control(model, sense="min")

set.objfn(model, edges$coeff)

# CONSTRAINTS

## SUPPLY CONSTRAINTS

add.constraint(model,
               xt = c(1,1,1),
               type = "<=",
               rhs = 60000,
               indices = c(1,2,3)) # Supply from PH

add.constraint(model,
               xt = c(1,1,1,1,1),
               type = "<=",
               rhs = 70000,
               indices = c(4,5,6,7,8)) # Supply from E

add.constraint(model,
               xt = c(1,1,1),
               type = "<=",
               rhs = 40000,
               indices = c(9,10,11)) # Supply from S

## DEMAND CONSTRAINTS

add.constraint(model,
               xt = c(1,1),
               type = "=",
               rhs = 30000,
               indices = c(1,4)) # Demand region 1

add.constraint(model,
               xt = c(1,1),
               type = "=",
               rhs = 40000,
               indices = c(2,5)) # Demand region 2

add.constraint(model,
               xt = c(1,1,1),
               type = "=",
               rhs = 25000,
               indices = c(3,6,9)) # Demand region 3

add.constraint(model,
               xt = c(1,1),
               type = "=",
               rhs = 35000,
               indices = c(7,10)) # Demand region 4

add.constraint(model,
               xt = c(1,1),
               type = "=",
               rhs = 33000,
               indices = c(8,11)) # Demand region 5


set.type (model, cbind(edges$index_i,edges$index_j), "integer")

set.bounds(model,c(rep(0,11)))

#SOLUTION

solve(model)

get.objective(model)

v <- get.variables(model); v


res<-cbind(edges, v)[c("index_i", "index_j", "v")]
res[res$v !='0',]
