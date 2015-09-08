      PROGRAM HISTPLOT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     **********************
C                     *                    *
C                     * Program   HISTPLOT *
C                     *                    *
C                     **********************
C
C
C
C          CALLING SEQUENCE:-
C               HISTPLOT
C
C
C          FUNCTION:-
C               It  produces  a  1-D  plot  from  a  Starlink  image  using
C               descriptor  items HMIN and HMAX, if present, to convert the
C               array subscript into physical units.
C
C
C          USE:-
C               It was written to plot meaningful histograms from the  file
C               created by WRHIST (qv).
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This is  the  Starlink  image
C                                             used  for input. It should be
C                                             1-D.
C
C         DEVICE          GKS_1               This is the  graphics  device
C                                             to  be used. It may be any GKS
C                                             device as specified in SGP 26.
C
C         YRANGE          Min., Max           This  defines  the   vertical
C                                             limits for plotting.
C
C         XRANGE          Min., Max.          This allows selection of part
C                                             of the horizontal range to be
C                                             plotted.
C
C         AXES            Lin,Lin             This  allows  definition   of
C                                             linear or logarithmic axes in
C                                             both X (first)  and  Y.  Note
C                                             that logs and zeros do not go
C                                             well together!
C
C         SIZE                                This is the size of the plot,
C                                             but   is  only  used  if  the
C                                             the selected device does not
C                                             have a cursor.
C
C
C
C
C
C         K F Hartley              RGO                             7-DEC-82
C
C
C--------------------------------------------------------------------------



C
C This is a modified version of the original LINPLOT program
C written by C D PIKE  at RGO.
C
C This version as modified by K.F.Hartley at RGO on 22-8-84 to work
C over the GKS graphics package.
C



C
C   CDP RGO 2/4/81
C
C
      INTEGER PIN,STATUS,AXIS1,NPIX(3)
      REAL SIZE(2)
      CHARACTER*12 DEV
      CHARACTER*80 TEXT
      LOGICAL CURSOR
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C       GET INPUT FRAME 2 TRIES ALLOWED
C
      ITRY = 0
    1 CALL RDIMAG('INPUT',FMT_R,1,AXIS1,I,PIN,STATUS)
      IF(STATUS.NE.ERR_NORMAL)  THEN
         CALL CNPAR('INPUT',STATUS)
         IF(ITRY.EQ.0)  THEN
            ITRY = 1
            GO TO 1
         ENDIF
         CALL EXIT
      END IF
      IF(I.NE.1) THEN
         CALL WRUSER('WARNING - IMAGE IS NOT 1D',STATUS)
      ENDIF
C
C   SELECT DEVICE CHOICE OF TEK,ARGS,VERS,CALCOMP
C
  100 CONTINUE
      DEV='GKS_1'
      CALL RDKEYC('DEVICE',.TRUE.,1,DEV,I,STATUS)
      IF (STATUS.NE.ERR_NORMAL.AND.STATUS.NE.ERR_PARNUL) THEN
         CALL WRERR('HELLDEV')
         CALL CNPAR('DEVICE',STATUS)
         GO TO 100
      END IF
      CALL DEVTRAN(DEV,IDEV,ICONID,STATUS)
      IF (STATUS.NE.0) THEN
         CALL WRUSER('Failed to recognize that device',STATUS)
         CALL CNPAR('DEVICE',STATUS)
         GO TO 100
      END IF
      CALL JBDEV(DEV)
C
C   Device dependent code to get round bug in HP 2648 driver.
C
      IF (IDEV.EQ.12) PRINT *,'+I'
      CALL JBINQ(XMAX,YMAX,CURSOR)
      SIZE(1)=XMAX
      SIZE(2)=YMAX
      IF (.NOT.CURSOR) THEN
        SIZE(1)=22
        SIZE(2)=15
        CALL RDKEYR('SIZE',.TRUE.,2,SIZE,I,ISTAT)
        IF (SIZE(2).EQ.0.0) THEN
          SIZE(2)=SIZE(1)
        ENDIF
      END IF
      XL = SIZE(1)
      YL = SIZE(2)

C
C     Copy the image into an array which can be altered by the
C     program.
C

      CALL GETDYN('ARRAY',FMT_R,AXIS1,IPIN,ISTATUS)
      CALL COPY(%VAL(PIN),%VAL(IPIN),AXIS1)
C
C  Read descriptor items used to scale X-axis.
C
      CALL RDDSCR('INPUT','HMIN',1,TEXT,I,ISTAT)
      IF (ISTAT.NE.0) THEN
         HMIN=1.0
      ELSE
         CALL CTOR(TEXT,HMIN,ISTAT)
      END IF
      CALL RDDSCR('INPUT','HMAX',1,TEXT,I,ISTAT)
      IF (ISTAT.NE.0) THEN
         HMAX=REAL(AXIS1)
      ELSE
         CALL CTOR(TEXT,HMAX,ISTAT)
      END IF
C
C  Call plotting subroutine
C
      CALL PLOTHIST(%VAL(IPIN),AXIS1,XL,YL,HMIN,HMAX)
C
C   Tidy up  and go home
C
      CALL FRDATA(' ',STATUS)
      CALL ENDPLT
      CALL EXIT
      END




      SUBROUTINE PLOTHIST(LINE,NX,XL,YL,HMIN,HMAX)
      REAL LINE(NX)
      REAL AX(2),AY(2)
      CHARACTER LL(2)*3
   10 CONTINUE
         AX(1) = HMIN
         AX(2) = HMAX
C
C   Now pick part (or whole) of X-axis
C
      CALL RDKEYR('XRANGE',.TRUE.,2,AX,I,ISTAT)
C
C   Now convert to array elements.
C
      I1=INT((AX(1)-HMIN)/(HMAX-HMIN)*REAL(NX)+1.0)
      IF (I1.LT.1.OR.I1.GT.NX) I1=1
      I2=INT((AX(2)-HMIN)/(HMAX-HMIN)*REAL(NX))
      IF (I2.LT.1.OR.I2.GT.NX) I2=NX
C
C  RUN THROUGH ARRAY FOR SCALING OF PLOT
C
   20 CONTINUE
      FMAX = LINE(1)
      FMIN = LINE(1)
      DO 100 I=I1,I2
      IF(FMAX.LT.LINE(I)) FMAX = LINE(I)
      IF(FMIN.GT.LINE(I))  FMIN = LINE(I)
  100 CONTINUE
         AY(1) = FMIN
         AY(2) = FMAX*1.5
       NXA=2
       NYA=2

C
C     Ask for the range of the Y axis.
C

      CALL RDKEYR('YRANGE',.TRUE.,2,AY,I,ISTAT)

C
C      Replace any elements in the array which lie outside
C      the range by either the upper or lower limit.
C

      DO 300 I=1,NX
      IF (LINE(I).LT.AY(1)) LINE(I)=AY(1)
      IF (LINE(I).GT.AY(2)) LINE(I)=AY(2)
300   CONTINUE
      LL(1)='LIN'
      LL(2)='LIN'

C
C     Ask for the type of axes.
C

      CALL RDKEYC('AXES',.TRUE.,2,LL,I,ISTAT)
C
C   Now check that if LOG axes are present no problems will
C   arise with the limits chosen
C
      IF (LL(1).EQ.'LOG') THEN
         IF (AX(1).LE.0.0) THEN
            CALL WRUSER('Log axes do not go with these limits',ISTAT)
            CALL CNPAR('XRANGE',ISTAT)
            GO TO 10
         END IF
      END IF
      IF (LL(2).EQ.'LOG') THEN
         IF (AY(1).LE.0.0) THEN
            CALL WRUSER('Log axes do not go with these limits',ISTAT)
            CALL CNPAR('YRANGE',ISTAT)
            GO TO 20
         END IF
      END IF
      IF (LL(1).EQ.'LOG') NXA= -2
      IF (LL(2).EQ.'LOG') NYA= -2
      CALL JBAXES(AX,NXA,XL,'DATA VALUE',10,AY,NYA,YL,'NUMBER',6)
C
C  HAVING DRAWN SIMPLEPLOT AXES NOW PLOT DATA REQUIRED
C
      X=REAL(I1-1)/REAL(NX)*(HMAX-HMIN)+HMIN
      CALL JOIN PT (X,LINE(I1))
      DO 200 I=I1+1,I2
      X=REAL(I-1)/REAL(NX)*(HMAX-HMIN)+HMIN
      CALL JOIN PT (X,LINE(I-1))
      CALL JOIN PT(X,LINE(I))
  200 CONTINUE
      CALL BREAK
      RETURN
      END


      SUBROUTINE COPY(IN,OUT,N)

C
C     This program takes in data from the image and stores it
C     in an array .

      REAL IN(N),OUT(N)
      DO I=1,N
         OUT(I)=IN(I)
      END DO
      END
