      SUBROUTINE POLFITS(NLEVS,LEVELS,TABS,TABF,NTAB,XVALS,TABINT)

C+
C    POLFITS
C
C	called from POLFIT, part of calibration suite.
C	given known LEVELS, calculates intensity conversion table, TABINT
C	using a polynomial fit.
C	ORDER MUST BE >2 AND <NLEVS
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
C	XVALS(NTAB)	(RA)		array of x-values to do fit on
C
C    D. TUDHOPE/ROE/Nov 1982
C    J. A. Cooke/UOE/ 1981
C-

      INTEGER NLEVS,NTAB
      REAL TABS,TABF
      REAL LEVELS(NLEVS,2),TABINT(NTAB),XVALS(NTAB)
      REAL RMSDEV,TABR,TABV
      REAL*8 COEFS(21),DX,DY
      INTEGER STAT,I,ORDER,LORDER
      CHARACTER MESSG*72,RPLY*4
C*  arrays duplicating LEVELS, for ease in calling NEWPOL and DRAWFIT
C*  screen area dimensions - see comments in drawfit
      REAL MSGXT,TEXTYT,VXT,VYT,MSGY,H
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
      REAL INTS(MAXLEVS),VALS(MAXLEVS)
      LOGICAL BADLEVELS
      INTEGER WKID
      PARAMETER (WKID=1)
 

      IF (NLEVS.LT.3) THEN
        CALL WRERR('TOOFEW')
      ELSE
      IF (BADLEVELS(NLEVS,LEVELS,TABS,TABF)) THEN
        CALL WRERR('BADLEV')
      ELSE

C*  first copy levels into vals and ints, so as can call newpol (ok cos nlevs is small)
        DO I=1,NLEVS
          VALS(I)=LEVELS(I,1)
          INTS(I)=LEVELS(I,2)
        ENDDO

      RPLY='N'
      DO WHILE (RPLY.NE.'Y')
        CALL READI('ORDER','Enter ORDER for fit',2,2,NLEVS-1,ORDER,STAT)
*         perform fit.....
            CALL NEWPOL(1,ORDER,NLEVS,VALS,INTS,LORDER,RMSDEV,COEFS)
 
*         get lookup table.....
            TABR=TABF-TABS
            DO I=1,NTAB
               TABV=TABS+((REAL(I-1)/(NTAB-1))*TABR)
               XVALS(I)=TABV
               DX=TABV
               CALL POLVAL(ORDER,COEFS,DX,DY)
               TABINT(I)=DY
            ENDDO
 
*         plot the fit.....
        CALL DRAWFIT(NLEVS,VALS,INTS,NTAB,XVALS,TABINT,TABS,TABF,
     :               MSGXT,TEXTYT,VXT,VYT,MSGY,H)
 
*         OK ? .....
            CALL TEXTAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,1)
            CALL YESNO('If fit ok,type Y
     : else N (and try another order for fit)','Y',RPLY,STAT)
            IF (RPLY.NE.'Y') THEN
              CALL GKS_CLRWK(WKID)
              CALL HIGR_GZEND(WKID)
            ENDIF
      ENDDO

      CALL TEXTAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,3)
      CALL WRUSER(' intensity = polynomial(pixel value)',STAT)
      WRITE(MESSG,30)ORDER
30    FORMAT(' polynomial order',I2,' coefficients:')
      CALL WRUSER(MESSG,STAT)
      DO I=1,ORDER+1
         WRITE(MESSG,40)COEFS(I)
40       FORMAT(10X,1PD17.9)
         CALL WRUSER(MESSG,STAT)
      ENDDO
      CALL HIGR_GZEND(WKID)
 
      ENDIF
      ENDIF
      END
