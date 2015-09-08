      PROGRAM IAMHISP

C+
C     Program IAMHISP
C
C     part of IAM suite of programs for parameterised information
C     computes intensity histogram of image and plots it to given
C     quadrant of ARGS. If this has not been called before this
C     session (HMAX=HMIN=0), then HISTDEF called to compute default
C     min. and max.  bins for histogram, but these values can be
C     overwritten (elsewhere) by user to a better range.
C
C  Given (program parameters)
C    INPUTIMG  (RA)   Image whose histogram is to be plotted.
C    HMIN      (R)    Zero point of histogram.
C    HMAX      (R)    Value of maximum bin in histogram.
C    REPLY     (CH)   Quadrant histo plotted in (from quad).
C    WORK      (I*2A) Work array used in IAMHISS to clear Args
C                     quadrant.
C
C  Returned (program parameters)
C    HMIND,HMAXD   (R)   Minimum and max. default bins (see below).
C    HMIN,HMAX     (R)   .....  only if this is the first time
C                           IAMHIS has been run and default values
C                           had to be obtained.
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C  A C Davenhall./ROE/13.7.84.
C     {Modified to trap histograms with 0 range, and also
C      inserted CLEARIMs.}
C-

      REAL  HMIN,HMAX
      INTEGER  IXR,IYR,NQUAD
      INTEGER NAXIN(2),NAXWK(2),NPIN,NPWK,ISTATUS,PSTATUS

      ISTATUS=0
      CALL INPICR('INPUTIMG',' ',2,NAXIN,NPIN,ISTATUS)
      CALL READR('HMIN',' ',0.0,0.0,1000000.0,HMIN,ISTATUS)
      CALL READR('HMAX',' ',0.0,0.0,1000000.0,HMAX,ISTATUS)
      CALL QUAD(IXR,IYR,NQUAD)
C*  obtain a work array (I*2) to be used to clear screen in IAMHISSUB
      NAXWK(1)=256
      NAXWK(2)=256
      CALL OUTPICI2('WORK',' ',2,NAXWK,NPWK,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE
        IF ((HMIN.EQ.0.0) .AND. (HMAX.EQ.0.0)) THEN
C*  first time IAMHIS used - get default HMIN,HMAX
          CALL HISTDEF(NAXIN(1),NAXIN(2),%VAL(NPIN),HMIN,HMAX)
          CALL WRITER('HMIN',HMIN,ISTATUS)
          CALL WRITER('HMAX',HMAX,ISTATUS)
          CALL WRITER('HMIND',HMIN,ISTATUS)
          CALL WRITER('HMAXD',HMAX,ISTATUS)
          IF (ISTATUS.NE.0) CALL WRERR('MISWRITE')
        ENDIF
        CALL IAMHISS(NAXIN(1),NAXIN(2),%VAL(NPIN),%VAL(NPWK),
     :                 HMIN,HMAX,IXR,IYR,NQUAD,PSTATUS)
        IF (PSTATUS.NE.0) THEN
          CALL WRUSER ('*** ERROR Histogram has zero range.',ISTATUS)
          CALL WRUSER ('     Check the original data and/or use '/
     :      /'CLRLIM to reset',ISTATUS)
          CALL WRUSER ('     the current histogram limits ready '/
     :      /'for a new plot.',ISTATUS)
        END IF
C*  signal successful termination to .scl cmd. proc.
        CALL WRITEI('SCLSTAT',0,ISTATUS)
      ENDIF
C*
      CALL CLEARIM ('INPUTIMG')
      CALL CLEARIM ('WORK')
C*
      END
