      SUBROUTINE SPLFITS(NLEVS,LEVELS,SLEVELS,TABS,TABF,
     :                   NTAB,XVALS,TABINT)

C+
C    SPLFITS
C
C	called from SPLFIT, part of calibration suite.
C	given known LEVELS, calculates intensity conversion table, TABINT
C	using spline fit from NAG routines EO2BAF,EO2BBF.
*     Ideas and inspiration for the spline fitting and knot
*     handling in this program come from S.R. Heathcote's
*     spline package (ROE GEC4082).  However programming
*     responsibility in this package is entirely with the
*     author !
C	need to go from TABS..TABF range to 1..NTAB index to TABINT
C
C    Given (arguments)
C	LEVELS(NLEVS,2)	(RA)		known intensity levels
C	SLEVELS(NLEVS,2)(RA)		work array used by SPLFITS for sorted levels
C	TABS,TABF	(R)	      start+finish of observed value range
C
C    Returned (arguments)
C	TABINT(NTAB)  (R)	      intensity conversion table
C
C    Work (argument)
C	XVALS(NTAB)	(RA)		VALUES for look up table
C
C    D. TUDHOPE/ROE/Nov 1982
C    J. A. Cooke/UOE/ 1981
C-

      INTEGER NLEVS,NTAB
      REAL TABS,TABF
      REAL LEVELS(NLEVS,2),SLEVELS(NLEVS,2),TABINT(NTAB),XVALS(NTAB)
      REAL TABR,TABV,DELI
C*  GKS wkstn id
      INTEGER WKID
      PARAMETER (WKID=1)
      INTEGER MAXLEVS
      PARAMETER (MAXLEVS=50)
      REAL VALS(MAXLEVS),INTS(MAXLEVS)
      REAL*8 KNOTS(MAXLEVS),DVALS(MAXLEVS),DINTS(MAXLEVS),DW(MAXLEVS)
      REAL*8 COEFS(MAXLEVS),WK1(MAXLEVS),WK2(4,MAXLEVS)
      REAL*8 SUMSQ,DX,DY
      INTEGER STAT,I,NKNOTS,NS,NF
      LOGICAL BADLEVELS,FITTED
      CHARACTER MESSG*72,RPLY*4
C*  variables describing graph,message,text areas - see comments for drawfit
      REAL MSGXT,TEXTYT,VXT,VYT,MSGY,H
 
      FITTED=.FALSE.
 
      IF (NLEVS.LT.4) THEN
        CALL WRERR('TOOFEW')
      ELSE
      IF (BADLEVELS(NLEVS,LEVELS,TABS,TABF)) THEN
        CALL WRERR('BADLEV')
      ELSE
 
C*  SORT LEVELS INTO ASCENDING VALUES
        CALL SORTLEVS(NLEVS,LEVELS,SLEVELS)
 
*      generate default knots.....
         CALL SPLGKN(NLEVS,SLEVELS,KNOTS,NKNOTS)
 
         DO I=1,NLEVS
            VALS(I)=SLEVELS(I,1)
            INTS(I)=SLEVELS(I,2)
            DINTS(I)=SLEVELS(I,2)
            DVALS(I)=SLEVELS(I,1)
            DW(I)=1
         ENDDO
 
         DO WHILE (.NOT. FITTED)
 
*         perform fit.....
            STAT=1
            CALL E02BAF(NLEVS,NKNOTS,DVALS,DINTS,DW,KNOTS,
     -        WK1,WK2,COEFS,SUMSQ,STAT)
            IF (STAT.NE.0) THEN
               CALL WRUSER('error in NAG routine E02BAF',STAT)
               STAT=0
            ENDIF
 
*         get lookup table.....
            TABR=TABF-TABS
            NS=((SLEVELS(1,1)-TABS)/TABR)*(NTAB-1)+2.
            NF=((SLEVELS(NLEVS,1)-TABS)/TABR)*(NTAB-1)-2.
            DO I=NS,NF
               TABV=TABS+((REAL(I-1)/(NTAB-1))*TABR)
               DX=TABV
               XVALS(I)=TABV
               STAT=1
               CALL E02BBF(NKNOTS,KNOTS,COEFS,DX,DY,STAT)
               IF (STAT.NE.0) THEN
                  CALL WRUSER('error in NAG routine E02BBF',
     -              STAT)
                  STAT=0
               ENDIF
               TABINT(I)=DY
            ENDDO
*         extrapolate ends.....
            DELI=TABINT(NS)-TABINT(NS+1)
            DO I=NS-1,1,-1
               XVALS(I)=TABS+((REAL(I-1)/(NTAB-1))*TABR)
               TABINT(I)=TABINT(NS)+(NS-I)*DELI
            ENDDO
            DELI=TABINT(NF)-TABINT(NF-1)
            DO I=NF+1,NTAB
               XVALS(I)=TABS+((REAL(I-1)/(NTAB-1))*TABR)
               TABINT(I)=TABINT(NF)+(I-NF)*DELI
            ENDDO
 
*         plot the fit.....
            CALL DRAWFIT(NLEVS,VALS,INTS,NTAB,XVALS,TABINT,TABS,TABF,
     :                    MSGXT,TEXTYT,VXT,VYT,MSGY,H)
 
C*  set windows to graph area and indicate knots by arrows
      CALL GRAFAREA(MSGXT,TEXTYT,VXT,VYT)
      DO I=5,NKNOTS-4
        CALL ARROW(KNOTS(I),0.0)
      ENDDO

*         OK ? .....
            CALL MSGAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,2)
            CALL YESNO('fit OK ? (Y or N)','Y',RPLY,STAT)
            IF (RPLY.EQ.'Y') THEN
               FITTED=.TRUE.
*            inform user.....
               CALL TEXTAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,1)
               CALL WRUSER('fit complete - intensity table stored',
     -           STAT)
            ELSE
*            change knots interactively.....
             CALL SPLCKN(MSGXT,TEXTYT,VXT,VYT,MSGY,H,NLEVS,KNOTS,NKNOTS)
             CALL GKS_CLRWK(WKID)
            ENDIF
 
            CALL HIGR_GZEND(WKID)
         ENDDO
 
      ENDIF
      ENDIF
      END
