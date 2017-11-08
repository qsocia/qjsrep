SUBROUTINE enomaly(m, enom, e, i, f1)

!Solves for Eccentric Anomaly using Mean Anomaly
!Inputs should be in 

IMPLICIT NONE
REAL :: m, e, enom, del, f, fp, dele, x, f1, f2, pi
INTEGER :: i

f(x) = x - e*SIN(x) - m
fp(x) = 1 - e*COS(x)

pi = ACOS(-1.)

enom = m

DO i=1, 25, 1
f1 = f(enom)
f2 = fp(enom)

IF(ABS(f1) <= 0.00001) GO TO 500

del = f1 / f2
enom = enom - del

END DO

PRINT *, "Did not converge after 25 iterations"

500 CONTINUE
! PRINT *, enom, i, f1
m = m/(2*pi)
enom = enom/(2*pi)

END SUBROUTINE enomaly
