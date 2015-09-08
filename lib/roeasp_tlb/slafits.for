      SUBROUTINE SLAFITS(NLEVS,LEVELS,SLEVELS,TABS,TABF,
     :                   NTAB,XVALS,TABINT)

C+
C    SLAFITS
C
C	called from SLAFIT, part of calibration suite.
C	given known LEVELS, calculates intensity conversion table, TABINT
C	using a 'slalom' fit devised by John Cooke (UOE).
C	Calls SLALOM to do the work.
C	need to go from TABS..TABF range to 1..NTAB index to TABINT
C
C    Given (arguments)
C	LEVELS(NLEVS,2)	(RA)		known intensity levels
C	TABS,TABF	(R)	      start+finish of observed value range
C
C    Returned (arguments)
C	TABINT(NTAB)  (R)	      intensity conversion table
C
C    Work (arguments)
C	XVALS(NTAB)	(RA)		needed by slalom as array of x-values to do fit on
C	SLEVELS(NLEVS,2)(RA)		work array to hold sorted LEVELS
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      INTEGER NLEVS,NTAB
      REAL TABS,TABF
      REAL LEVELS(NLEVS,2),SLEVELS(NLEVS,2),TABINT(NTAB),XVALS(NTAB)
      REAL TABR,TABV
C*  workstation id for GKS
      INTEGER WKID
      PARAMETER (WKID=1)
C*  arrays duplicating LEVELS, as get overwritten by slalom, and array of weights (all 1)
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
      REAL INTS(MAXLEVS),VALS(MAXLEVS)
      INTEGER WEIGHTS(MAXLEVS)
      INTEGER I,ST
      LOGICAL BADLEVELS
 

      IF (NLEVS.LT.3) THEN
        CALL WRERR('TOOFEW')
      ELSE
      IF (BADLEVELS(NLEVS,LEVELS,TABS,TABF)) THEN
        CALL WRERR('BADLEV')
      ELSE

C*  sort levels into slevels
        CALL SORTLEVS(NLEVS,LEVELS,SLEVELS)
C*  first copy Slevels into vals and ints and set weights to 1
        DO I=1,NLEVS
          VALS(I)=SLEVELS(I,1)
          INTS(I)=SLEVELS(I,2)
          WEIGHTS(I)=1
        ENDDO
C*  get XVALS - values in range TABS..TABF (that map to 1..NTAB) to call slalom on
        TABR=TABF-TABS
        DO I=1,NTAB
           TABV=TABS+((REAL(I-1)/(NTAB-1))*TABR)
           XVALS(I)=TABV
        ENDDO
C*  perform fit and get TABINT
        CALL SLALOM(VALS,INTS,WEIGHTS,NLEVS,XVALS,NTAB,TABINT)
C*  plot the fit
        CALL DRAWFIT(NLEVS,VALS,INTS,NTAB,XVALS,TABINT,TABS,TABF,
     :               R1,R2,R3,R4,R5,R6)
C*  inform user
        CALL WRUSER('fit complete - intensity table stored',ST)
C*  terminate HIGR
        CALL HIGR_GZEND(WKID)

      ENDIF
      ENDIF
      END
