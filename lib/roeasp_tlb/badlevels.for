      LOGICAL FUNCTION BADLEVELS(NLEVS,LEVELS,TABS,TABF)

C+
C    BADLEVELS
C
C	part of calibration suite.
C	returns .true. if all the VALUE entries in LEVELS (LEVELS(J,*))
C	are wthin the bounds TABS..TABF and .false. otherwise
C
C    Given (arguments)
C	LEVELS(NLEVS,2)	(RA)		known intensity levels
C	TABS,TABF	(R)	      start+finish of observed value range
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      INTEGER NLEVS
      REAL TABS,TABF
      REAL LEVELS(NLEVS,2)
      INTEGER J

      BADLEVELS=.TRUE.
      DO J=1,NLEVS
        IF ((LEVELS(J,1).LT.TABS) .OR. (LEVELS(J,1).GT.TABF)) GOTO 100
      ENDDO
      BADLEVELS=.FALSE.
100   END
