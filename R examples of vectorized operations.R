# examples of vectorization and functions in R
# can take the place of 'for-loops'


#--------------------------------------------------------
# example 1
# simple vector operations
#--------------------------------------------------------

x1.in = c(11, 10, 15, 4, 9, 16, 17, 5, 18, 1, 6, 14, 7, 13, 2, 20, 12, 19, 8, 3)
  # a randomized order of 1:20
  # could use something else, like rnorm(20,50,3)


x1.out = 5*x1.in

cbind(x1.in, x1.out)


### here is the for-loop equivalent for comparison 

x1b.out = vector()

for (i in 1:length(x1.in)){
  x1b.out[i] = 5*x1.in[i]
}



#--------------------------------------------------------
# example 2
# simple vector operations with a defined function
#--------------------------------------------------------

fn1 = function(z){
  5*z   # could be a more elaborate function 
        # that returns a more complicated structure
}


x2.out = fn1(x1.in)   # this applies the function like a 'zipper' along the length of x1.in
                      # no for-loop needed

cbind(x1.in, x2.out)  # look at the input and output side-by-side




#--------------------------------------------------------
# example 3
# vectorized operation applied to columns of a matrix
#--------------------------------------------------------

x3.in = matrix(rnorm(15, 50, 2),nrow = 5, ncol = 3)
print(x3.in)


# take the mean of each column using the 'apply' functionality 
# (instead of using a for-loop)

x3.out = apply(x3.in, MARGIN = 2, FUN = mean)

print(x3.out)



#--------------------------------------------------------
# example 4
# vectorized operation applied to columns of a matrix
# using a more complicated function
#--------------------------------------------------------

# use x3.in

fn4 = function(z){
  return(cbind(mean(z),sd(z)))
}

x4.out = apply(x3.in, MARGIN = 2, FUN = fn4)
print(x4.out)




##### END CODE #####
