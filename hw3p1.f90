PROGRAM hw3p1

!Quentin Socia 20170923
!ASTR680 HW3

IMPLICIT NONE
REAL :: m, e, enom, pi, dele, theta, r, a
INTEGER :: n, l, i

pi = ACOS(-1.)
a = 1.

OPEN(11,file='hw3p1.dat')

DO l=1, 4
IF(l==1) e = 0.0
IF(l==2) e = 0.15
IF(l==3) e = 0.5
IF(l==4) e = 0.85

DO n=0, 7, 1
m = FLOAT(n)*pi/4.
CALL enomaly(m, enom, e, i, dele)
CALL tanomaly(theta, e,enom)
r = a*(1-e*COS(enom))
WRITE(11,*) e,m*(180/pi),m,enom*(180/pi),enom,dele,i,theta*(180/pi),theta,r

WRITE(12,*) ABS(m-enom), i

END DO
END DO

STOP 'Done'
END PROGRAM hw3p1

!-------------------------------------------------

SUBROUTINE enomaly(m, enom, e, i, f1)

!Solves for Eccentric Anomaly using Mean Anomaly
!Units in radians

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

END SUBROUTINE enomaly

!-------------------------------------------------

SUBROUTINE tanomaly(theta, e, enom)

!Solves for True Anomaly using Eccentric Anomaly
!Units in radians

IMPLICIT NONE
REAL :: theta, enom, e, a, b, pi

pi = ACOS(-1.)

a = (TAN(enom/2))
b = SQRT((1+e)/(1-e))

theta = 2*ATAN(b*a)

IF(enom>=0. .AND. enom < pi) CONTINUE
IF(enom>=pi .AND. enom < 2*pi) theta = theta + 2*pi

END SUBROUTINE tanomaly
