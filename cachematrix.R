##Calculating an Inverse of a matrix is a costly operation considering the real-time use-cases.
##The 2 functions below will result in caching an Inverse of a matrix,So that
##When similar Matrix is supplied for Inverse Calculation,Cached Value can be retrieved which saves time.

###################################################################
#Object Type: Function (Return Type is "LIST")
#Name: makeCacheMatrix (Formal Argument is a "SQUARE MATRIX")
#Purpose: Create a Special Matrix which is a list to 
#         1."Reset" the Matrix
#         2."get" the Matrix
#         3."set" the value of the Inverse of Matrix (Caching Step)
#         4."get" the value of the Inverse of Matrix
#Created Date:23-Apr-2014
#Modified Date:24-Apr-2014
###################################################################

makeCacheMatrix<-function(x=matrix()){
        inv<-NULL
        
        #set function below will reset the matrix with the newly
        #supplied matrix and also Inverse variable
        set<-function(y){
                x<<-y
                inv<<-NULL
        }
        
        get<-function(){
                x
        }
        
        #set_inverse function below will Cache the Inverse of Input matrix
        set_inverse<-function(inv_matrix){
                inv<<-inv_matrix
        }
        
        get_inverse<-function(){
                inv
        }

        #Return a list of 4 functions defined above
        list(set=set,get=get,set_inverse=set_inverse,get_inverse=get_inverse)
}


#####################################################################
#Object Type: Function (Return Type is "MATRIX")
#Name: cacheSolve (Formal Argument is a "LIST"
#      of Special Matrix resulted from function "makeCacheMatrix")
#Purpose: 1.Return the Inverse of Matrix supplied if it 
#           is available in Cache/Global_Env
#         2.If Inverse of Supplied Matrix is not cached then calculate
#           the Inverse and Cache the Value to ease the next Inverse
#           Operation if matrix supplied is same
#Created Date:23-Apr-2014
#Modified Date:24-Apr-2014
#####################################################################


cacheSolve <- function(x, ...) {
        #get the Inverse 
        inv<-x$get_inverse()

        ## Return a matrix that is the inverse of 'x' if found in Cache        
        if(!is.null(inv)) {
                message("Getting Cached Inverse Matrix")
                return(inv)
        }
        
        #Calling get function to get the new Matrix data
        matrix_data<-x$get()
        
        #Below expression is to Calculate Inverse of a matrix using R function "SOLVE"        
        inv<-solve(matrix_data)
        
        #For the newly supplied Matrix Cache the Inverse
        x$set_inverse(inv)
        
        #Final Output: Return Inverse
        inv
}
