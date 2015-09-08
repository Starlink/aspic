      SUBROUTINE COSFITS(NLEVS,LEVELS,TCLEAR,TBLACK,TABS,TABF,
     :                   NTAB,XVALS,TABINT)

C+
C    COSFITS
C
C	called from COSFIT, part of calibration suite.
C	given known LEVELS, calculates COSMOS-type intensity conversion table, TABINT
C	need to go from TABS..TABF range to 1..NTAB index to TABINT
C	For COSMOS data - needs INTENSITY>0
C
C    Given (arguments)
C	LEVELS(NLEV,2)	(RA)		known intensity levels
C	TCLEAR,TBLACK	(I)		cosmos tclear and tblack
C	TABS,TABF	(R)	      start+finish of observed value range
C
C    Returned (arguments)
C	TABINT(NTAB)  (R)	      intensity conversion table
C
C    Work (arguments)
C	XVALS(NTAB)	(RA)		array of x-values to do fit on
C
C    D. TUDHOPE/ROE/Nov 1982
C    J. A. Cooke/UOE/ 1981
C-

      INTEGER NLEVS,NTAB,TCLEAR,TBLACK
      REAL TABS,TABF
      REAL LEVELS(NLEVS,2),TABINT(NTAB),XVALS(NTAB)
      REAL X(50),Y(50),GAMMA,CONST,XVAL
      REAL TABR,TABV
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
      REAL INTS(MAXLEVS),VALS(MAXLEVS)
      INTEGER STAT,I,NACC
      CHARACTER MESSG*64
      LOGICAL BADLEVELS
      INTEGER WKID
      PARAMETER (WKID=1)

      IF (NLEVS.LT.2) THEN
        CALL WRERR('TOOFEW')
      ELSE
      IF (BADLEVELS(NLEVS,LEVELS,TABS,TABF)) THEN
        CALL WRERR('BADLEV')
      ELSE

*     CALCULATE 'X' AND 'Y' FOR FIT
*
      NACC=0
      DO I=1,NLEVS
        IF (LEVELS(I,2).GT.0.0) THEN
          NACC=NACC+1
          Y(NACC)=ALOG10(LEVELS(I,2))
          XVAL=LEVELS(I,1)
          IF(XVAL.LE.REAL(TBLACK)) XVAL=TBLACK+1.
          X(NACC)=ALOG10((REAL(TCLEAR-TBLACK)/(XVAL-REAL(TBLACK)))-1.)
        ELSE
10        FORMAT(' (',F9.2,',',F9.2,') rejected as COSFIT requires
     : INTENSITY>0.0')
          WRITE(MESSG,10) LEVELS(I,1),LEVELS(I,2)
          CALL WRUSER(MESSG,STAT)
        ENDIF
      ENDDO
      IF (NACC.LE.0) THEN
        CALL WRERR('NONE')
      ELSE


      CALL LINFIT(NACC,X,Y,GAMMA,CONST)
 
C*  get arrays vals+ints for drawfit later
        DO I=1,NLEVS
          VALS(I)=LEVELS(I,1)
          INTS(I)=LEVELS(I,2)
        ENDDO
*   get lookup table.....
      TABR=TABF-TABS
      DO I=1,NTAB
         TABV=TABS+((REAL(I-1)/(NTAB-1))*TABR)
         IF (TABV.LE.REAL(TBLACK)) TABV=REAL(TBLACK)+1.
         IF (TABV.GE.REAL(TCLEAR)) TABV=REAL(TCLEAR)-1.
         XVALS(I)=TABV
         TABINT(I)=10.**(GAMMA*ALOG10( (REAL(TCLEAR-TBLACK)/
     :     (TABV-REAL(TBLACK))) - 1.) + CONST)
      ENDDO
 
*   plot the fit.....
        CALL DRAWFIT(NLEVS,VALS,INTS,NTAB,XVALS,TABINT,TABS,TABF,
     :               R1,R2,R3,R4,R5,R6)
 
*
*     INFORM USER
*
      WRITE(MESSG,20)TCLEAR,TBLACK,GAMMA,CONST
 20   FORMAT(' Tclear: ',I4,'   Tblack: ',I3,'   GAMMA: ',F5.2,
     -  '   CONST: ',F5.2)
      CALL WRUSER(MESSG,STAT)
      CALL WRUSER(' ',STAT)
      CALL WRUSER('logI = GAMMA * log( (Tclear - Tblack)/(T - Tblack) -
     -1 ) + C',STAT)
      CALL HIGR_GZEND(WKID)
 
      ENDIF
      ENDIF
      ENDIF
      END
