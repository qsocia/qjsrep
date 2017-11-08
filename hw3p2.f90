PROGRAM hw3p1

!Quentin Socia 20170923
!ASTR680 HW3 part 2

IMPLICIT NONE
REAL :: m, e, enom, pi, dele, theta, r, a, p, t, t0, w, bigw, inc
REAL :: psi, x, y, del
INTEGER :: n, l, i

pi = ACOS(-1.)
p = 59.5 !years
a = 0.880 !arcsec
inc = 165.*pi/180. !degrees to radians
bigw = 30.4*pi/180. !degrees to radians
t0 = 1989.23 !years
e = 0.325
w = 30.4*pi/180. !degrees to radians
del = p/36.

OPEN(11,file='hw3p2.dat')

	DO n=0, 107, 1

	t = t0 + del*FLOAT(n)
	CALL phase(t, p, t0, l, psi)
	m = psi*2*pi
	CALL enomaly(m, enom, e, i, dele)
	CALL tanomaly(theta, e, enom)
	r = a*(1-e*COS(enom))
	x = r*(COS(bigw)*COS(theta+w)-SIN(bigw)*SIN(theta+w)*COS(inc))
	y = r*(SIN(bigw)*COS(theta+w)-COS(bigw)*SIN(theta+w)*COS(inc))
	WRITE(11,*) x, y

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

!--------------------------------------------------

SUBROUTINE phase(t, per, tz, i, psi)

!QSocia 20170913
!This subroutine takes a t_zero, period, and time and outputs the 'i'th
!cycle number and phase between zero and one 

IMPLICIT NONE
REAL :: b
REAL, INTENT(IN) :: per, t, tz 
REAL, INTENT(OUT) :: psi
INTEGER, INTENT(OUT) :: i

b = (t-tz)/per
i = INT(b)
psi = b - FLOAT(i)

IF((t-tz)<0.) psi = psi + 1 !keeps 0<=phase<1
IF((t-tz)>=0.) i = i+1 !This means immediately after tz is cycle 1

END SUBROUTINE phase
