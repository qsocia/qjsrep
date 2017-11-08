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
