# Memory Test -----

library(R6)

# Define the MeshGrid class using R6
MeshGrid <- R6Class("MeshGrid",
                    public = list(
                      # Constructor
                      initialize = function() {
                        # Initialize any variables if necessary
                      },
                      
                      # Method for 2D meshgrid
                      meshgrid_2d = function(x, y = NULL) {
                        if (is.null(y)) {
                          y <- x
                        }
                        
                        if (length(x) == 0 || length(y) == 0) {
                          xx <- matrix(0, nrow = 0, ncol = 0)
                          yy <- matrix(0, nrow = 0, ncol = 0)
                        } else {
                          xx <- matrix(rep(x, each = length(y)), nrow = length(y), byrow = TRUE)
                          yy <- matrix(rep(y, length(x)), nrow = length(y), byrow = FALSE)
                        }
                        list(xx = xx, yy = yy)
                      },
                      
                      # Method for 3D meshgrid
                      meshgrid_3d = function(x, y = NULL, z = NULL) {
                        if (is.null(y)) {
                          y <- x
                          z <- x
                        }
                        
                        if (length(x) == 0 || length(y) == 0 || length(z) == 0) {
                          xx <- array(0, dim = c(0, 0, 0))
                          yy <- array(0, dim = c(0, 0, 0))
                          zz <- array(0, dim = c(0, 0, 0))
                        } else {
                          xx <- array(rep(x, each = length(y) * length(z)), dim = c(length(y), length(x), length(z)))
                          yy <- array(rep(y, times = length(x) * length(z)), dim = c(length(y), length(x), length(z)))
                          zz <- array(rep(z, each = length(x) * length(y)), dim = c(length(y), length(x), length(z)))
                        }
                        list(xx = xx, yy = yy, zz = zz)
                      }
                    )
)

# Example usage for 2D:
mesh <- MeshGrid$new()
result_2d <- mesh$meshgrid_2d(seq(-2, 2, 0.2), seq(-4, 4, 0.4))
xx_2d <- result_2d$xx
yy_2d <- result_2d$yy

# Example usage for 3D:
result_3d <- mesh$meshgrid_3d(seq(-2, 2, 0.2), seq(-4, 4, 0.4), seq(-1, 1, 0.2))
xx_3d <- result_3d$xx
yy_3d <- result_3d$yy
zz_3d <- result_3d$zz



# Test data for R
set.seed(123)  # For reproducibility
x <- seq(-10, 10, length.out = 1000)
y <- seq(-5, 5, length.out = 500)
z <- seq(-2, 2, length.out = 200)


library(pryr)  # For memory measurement

# Create the MeshGrid class (assuming it is already defined as per the previous translation)
mesh <- MeshGrid$new()

# Measure initial memory usage
initial_mem <- mem_used()

# Run the 3D meshgrid function with test data
result_2d <- mesh$meshgrid_2d(x, y)

# Measure final memory usage
final_mem <- mem_used()

# Calculate the memory difference
memory_used1 <- final_mem - initial_mem
cat("Memory used by R6 meshgrid function (2D):", memory_used1, "bytes\n")

### CPP test -----

library(Rcpp)

# Compile the C++ function
Rcpp::sourceCpp("C_functions//meshgrid_2d_C.cpp")


library(pryr)

# Test data
set.seed(123)
x <- seq(-10, 10, length.out = 1000)
y <- seq(-5, 5, length.out = 500)

# Measure initial memory usage
initial_mem <- mem_used()

# Call the C++ meshgrid function
result <- meshgrid_2d_C(x, y)

# Measure final memory usage
final_mem <- mem_used()

# Calculate memory difference
memory_used2 <- final_mem - initial_mem
cat("Memory used by C++ meshgrid function (2D):", memory_used2, "bytes\n")


### bigmemory test -----

library(bigmemory)

MeshgridBig <- R6::R6Class("MeshgridBig",
                           public = list(
                             x = NULL,
                             y = NULL,
                             X = NULL,
                             Y = NULL,
                             
                             initialize = function(x, y, file_path_x = "X.bin", file_path_y = "Y.bin") {
                               self$x <- x
                               self$y <- y
                               
                               # Define the dimensions of the meshgrid
                               nx <- length(x)
                               ny <- length(y)
                               
                               # Create big matrices to store the result (on disk)
                               self$X <- bigmemory::big.matrix(nrow = ny, ncol = nx, type = "double", backingfile = file_path_x)
                               self$Y <- bigmemory::big.matrix(nrow = ny, ncol = nx, type = "double", backingfile = file_path_y)
                               
                               # Populate the big matrices
                               self$create_meshgrid()
                             },
                             
                             create_meshgrid = function() {
                               nx <- length(self$x)
                               ny <- length(self$y)
                               
                               # Fill the X matrix
                               for (i in 1:nx) {
                                 self$X[, i] <- self$x[i]
                               }
                               
                               # Fill the Y matrix
                               for (i in 1:ny) {
                                 self$Y[i, ] <- self$y[i]
                               }
                             }
                           )
)
# Test data
x <- seq(-10, 10, length.out = 1000)
y <- seq(-5, 5, length.out = 500)

# Measure initial memory usage
initial_mem <- mem_used()

# Create an instance of the MeshgridBig class
mesh <- MeshgridBig$new(x, y)

# Access the resulting big matrices
X_big <- mesh$X
Y_big <- mesh$Y

# Measure final memory usage
final_mem <- mem_used()

# Calculate memory difference
memory_used3 <- final_mem - initial_mem

# Check memory usage with pryr
library(pryr)
cat("Memory used after creating big.matrix meshgrid:", memory_used3, "\n")


### cpp function ----

# Load the Rcpp package
library(Rcpp)

# Define meshgrid_2d function using cppFunction
cppFunction('
Rcpp::List meshgrid_2d_cpp(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  int nx = x.size();
  int ny = y.size();
  
  // Create matrices to store the results
  Rcpp::NumericMatrix X(ny, nx);
  Rcpp::NumericMatrix Y(ny, nx);
  
  // Fill in the X matrix (rows are copies of x)
  for (int i = 0; i < ny; ++i) {
    for (int j = 0; j < nx; ++j) {
      X(i, j) = x[j];
    }
  }
  
  // Fill in the Y matrix (columns are copies of y)
  for (int i = 0; i < ny; ++i) {
    for (int j = 0; j < nx; ++j) {
      Y(i, j) = y[i];
    }
  }
  
  return Rcpp::List::create(Rcpp::Named("X") = X, Rcpp::Named("Y") = Y);
}
')

# Test data
x <- seq(-10, 10, length.out = 10000)
y <- seq(-5, 5, length.out = 5000)

# Measure initial memory usage
initial_mem <- mem_used()

result <- meshgrid_2d_cpp(x, y)
X <- result$X
Y <- result$Y

# Measure final memory usage
final_mem <- mem_used()

# Calculate memory difference
memory_used4 <- final_mem - initial_mem

# Check memory usage with pryr
library(pryr)
cat("Memory used after creating cpp meshgrid:", memory_used4, "\n")



### sourcecpp ----


# Load the Rcpp package
library(Rcpp)

# Define the meshgrid_2d function using sourceCpp
sourceCpp(code = '
#include <Rcpp.h>

// [[Rcpp::export]]
Rcpp::List meshgrid_2d_Rcpp(Rcpp::NumericVector x, Rcpp::NumericVector y) {
  int nx = x.size();
  int ny = y.size();
  
  // Create matrices to store the results
  Rcpp::NumericMatrix X(ny, nx);
  Rcpp::NumericMatrix Y(ny, nx);
  
  // Fill in the X matrix (rows are copies of x)
  for (int i = 0; i < ny; ++i) {
    for (int j = 0; j < nx; ++j) {
      X(i, j) = x[j];
    }
  }
  
  // Fill in the Y matrix (columns are copies of y)
  for (int i = 0; i < ny; ++i) {
    for (int j = 0; j < nx; ++j) {
      Y(i, j) = y[i];
    }
  }
  
  return Rcpp::List::create(Rcpp::Named("X") = X, Rcpp::Named("Y") = Y);
}
')

# Test data
x <- seq(-10, 10, length.out = 10000)
y <- seq(-5, 5, length.out = 500)

# Measure initial memory usage
initial_mem <- mem_used()

result <- meshgrid_2d_Rcpp(x, y)
X <- result$X
Y <- result$Y

# Measure final memory usage
final_mem <- mem_used()

# Calculate memory difference
memory_used5 <- final_mem - initial_mem

# Check memory usage with pryr
library(pryr)
cat("Memory used after creating sourcecpp meshgrid:", memory_used5, "\n")


memory_used1 -> memory_used1_R6
memory_used1_R6

memory_used2 -> memory_used2_CPP_HR
memory_used2_CPP_HR

memory_used3 -> memory_used3_Big
memory_used3_Big

memory_used4 -> memory_used4_CPP
memory_used4_CPP

memory_used5 -> memory_used5_SourceCPP
memory_used5_SourceCPP


## FIle matrix -----

# Install filematrix package if not installed
# install.packages("filematrix")

library(filematrix)

meshgrid_2d_filematrix <- function(x, y, backingfile = "meshgrid_matrix") {
  nx <- length(x)
  ny <- length(y)
  
  # Create filematrix with double the number of columns to store X and Y grids
  fm <- fm.create(backingfile, nrow = ny, ncol = nx * 2, type = "double")
  
  # Fill the Y part (columns 1 to nx) with the repeated values of y
  for (i in 1:nx) {
    fm[, i] <- y # Assign y to each column in the Y part
  }
  
  # Fill the X part (columns (nx + 1) to 2*nx) with the repeated values of x
  for (i in 1:ny) {
    fm[i, (nx + 1):(2 * nx)] <- x # Assign x to each row in the X part
  }
  
  return(fm)
}

fm_grid <- meshgrid_2d_filematrix(x, y)

# View the created filematrix
print(fm_grid[])


# Benchmark memory and execution time -----

library(bench)

result5 <- mark(meshgrid_2d_Rcpp(x, y), iterations = 10)

result4 <- mark(meshgrid_2d_cpp(x, y), iterations = 10)

result2 <- mark(meshgrid_2d_C(x, y), iterations = 10)


print(result5)
print(result4)
print(result2)


# Install peakRAM package if not installed
# install.packages("peakRAM")

library(peakRAM)

# Measure the peak memory usage of meshgrid_2d_filematrix
peakRAM({
  fm_grid <- meshgrid_2d_filematrix(x, y)
})

# Install pryr package if not installed
# install.packages("pryr")

library(pryr)

# Measure memory change
mem_change({
  fm_grid <- meshgrid_2d_filematrix(x, y)
})

mem_change({
  fm_grid <- meshgrid_2d_Rcpp(x, y)
})

mem_change({
  fm_grid <- meshgrid_2d_cpp(x, y)
})

mem_change({
  fm_grid <- meshgrid_2d_C(x, y)
})

# Install profvis package if not installed
# install.packages("profvis")

library(profvis)

# Profile the memory and time usage
profvis({
  fm_grid <- meshgrid_2d_filematrix(x, y)
})

profvis({
  fm_grid <- meshgrid_2d_Rcpp(x, y)
})

profvis({
  fm_grid <- meshgrid_2d_cpp(x, y)
})

profvis({
  fm_grid <- meshgrid_2d_C(x, y)
})

#Biggrid <- MeshgridBig$new(x, y)
#fm_grid <- meshgrid_2d_filematrix(x, y)
Rcpp_grid <- meshgrid_2d_Rcpp(x, y)
CPP_grid <- meshgrid_2d_cpp(x, y)
C_grid <- meshgrid_2d_C(x, y)
result_2d <- mesh$meshgrid_2d(x, y)