
# transformacion box cox
inversePower = function(x, lambda=1) {
  out = (lambda*x+1)^(1/lambda)
  return(out)
}






#  ------------------------------------------------------------------------




# estas funciones deben pasar a SISESATools

# Sinuosidad1 <- function(tetha, distanciaEmision){
#   s <- mean(sin(tetha), na.rm = TRUE)
#   c <- mean(cos(tetha), na.rm = TRUE)
#   p <- mean(distanciaEmision, na.rm = TRUE)
#   b <- sd(distanciaEmision, na.rm = TRUE)
#   
#   out <- 2*(p*((1-(c^2)-(s^2))/((1-c^2)+s^2)+b^2))^(-0.5) 
#   
#   return(out)
# }
# 
# 
# Sinuosidad2 <- function(tetha, distancia_emision){
#   
#   s <- mean(sin(tetha), na.rm = TRUE)
#   c <- mean(cos(tetha), na.rm = TRUE)
#   p <- mean(distancia_emision, na.rm = TRUE)
#   b <- sd(distancia_emision, na.rm = TRUE)
#   
#   out <- 2*(p*((1+c)/(1-c))+b^2)^-0.5
#   return(out)
# }
# 
# Sinuosidad3 <- function(x){
#   
#   out <- sum(x, na.rm = TRUE)/length(x)  
#   return(out)
# }

# 
# equation_angle <- function(x,y){
#   dot.prod <- x%*%y
#   norm.x <- norm(x,type="2")
#   norm.y <- norm(y,type="2")
#   theta <- suppressWarnings(acos(dot.prod / (norm.x * norm.y))*180/pi)
#   return(theta)
# }
# 
# angle <- function(x,y){
#   
#   vec_angle <- NULL
#   for(z in 2:(length(x)-1)){
#     #print(z)
#     A <- matrix(NA,nrow = 2)
#     B <- matrix(NA,nrow = 2)
#     X1 <- as.matrix(x[c(z-1,z)])
#     Y1 <- as.matrix(y[c(z-1,z)])
#     X2 <- as.matrix(x[c(z,z+1)])
#     Y2 <- as.matrix(y[c(z,z+1)])
#     A[1] <- X1[1]-X1[2]
#     A[2] <- Y1[1]-Y1[2]
#     B[1] <- X2[2]-X2[1]
#     B[2] <- Y2[2]-Y2[1]
#     out <- equation_angle(t(A),B)
#     vec_angle <- rbind(vec_angle,out)
#   }
#   vec_angle <- as.numeric(vec_angle)
#   return(vec_angle)
# }


