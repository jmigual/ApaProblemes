
v1 <- matrix(c(4,2,2,3,4,1,4,3,6,4),nrow=2,byrow = TRUE)
(v1)

(mean(v1[1,]))
(mean(v1[2,]))

vm1 <- matrix(c(mean(v1[1,]),mean(v1[2,])))

(vm1)

vx1 <- c(4,2,2,3,4)
vy1 <- c(1,4,3,6,4)

vx2 <- c(9,6,9,8,10)
vy2 <- c(10,8,5,7,8)

mx1 <- mean(vx1)
my1 <- mean(vy1)

mx2 <- mean(vx2)
my2 <- mean(vy2)

(mx1)
(my1)

(mx2)
(my2)

# S_w = SUM(n)[(x_n - m_+)(x_n - m_+)T] + SUM(n)[(x_n - m_m)(x_n - m_-)T]

Sw <- sum((vx1 - mx1))