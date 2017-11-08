PROGRAM tantest

xpi = ACOS(-1.)

DO i=0, 11, 1
x = xpi*FLOAT(i)/6.

z = TAN(x)
y = ATAN(z) 

IF(x>=0.) THEN
w = y
IF(x < xpi/2.) PRINT *, 1
END IF

IF(x>=xpi/2. .AND. x < xpi) w = y + xpi
IF(x>=xpi .AND. x < 3*xpi/2.) w = y + xpi
IF(x>=3*xpi/2. .AND. x < 2*xpi) w = y + 2*xpi

WRITE(11,*) x, z, y, w
END DO

END PROGRAM tantest
