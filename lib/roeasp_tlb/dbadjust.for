      SUBROUTINE DBADJUST (D1,D2,AD1,AD2)

C+
C    DBADJUST
C
C	called from PLOTS.
C	adjusts window+viewport dimensions D1,D2 to AD1,AD2
C	because of bug in args data base, which requires that the
C	larger of the 2 dims. get D=D-F+1 done to it where F is scale factor(>1)
C	note that the centre of the args in WRIM does not need to be changed
C	this all is because STRMS subtracts 1 when it should be doing this
C	if D1=D2 then no change need be made.
C
C    Given (arguments)
C	D1,D2   (I)	dimensions to be adjusted
C
C	Returned (arguments)
C	AD1,AD2 (I)	adjusted dimensions
C
C  D. Tudhope/ROE/Oct 1982
C-

      INTEGER D1,D2,AD1,AD2

      IF (D1.EQ.D2) THEN
        AD1=D1
        AD2=D2
      ELSE
        IF (D1.GT.D2) THEN
          AD2=D2
          AD1=D1+1-NINT(D1/REAL(D2))
        ELSE
          AD1=D1
          AD2=D2+1-NINT(D2/REAL(D1))
        ENDIF
      ENDIF
      END
