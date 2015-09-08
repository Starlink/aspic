      PROGRAM GETLIMP

C+
C  program GETLIMP
C
C  change the limits of the IAM histogram on the args
C  using the cursor.
C
C  Since have to use the low level cursor routine which returns args coords
C  not pixel, the cursor is just called from this program.
C  To use with the histogram the values returned from the cursor must lie
C  in the range 0..255. The histo may be in any of the quadrants of the args
C  and so the cursor values mod 256 are taken. Actually only the x-coord is used.
C
C  Given (program parameters)
C    HMIN (R)    zero point of histogram
C    HMAX (R)    value of maximum bin in histogram
C
C  Returned (program parameters)
C    HMIN (R)    .....................
C    HMAX (R)    .....................
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      REAL HMIN,HMAX,TMIN,TMAX
C*  variables for cursor call
      INTEGER XMIN,XMAX,Y,IB
      INTEGER NBIN2
C*  actually nbin*2 where nbin is the number of bins in histo
      PARAMETER (NBIN2=256)

      ISTATUS=0
      CALL SRINIT(0,.FALSE.,ISTATUS)
      CALL READR('HMIN',' ',0.0,0.0,1000000.0,HMIN,ISTATUS)
      CALL READR('HMAX',' ',0.0,0.0,1000000.0,HMAX,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE
      IF ((HMIN.EQ.0.0) .AND. (HMAX.EQ.0.0)) THEN
        CALL WRERR('NOTYET')
      ELSE
        CALL WRUSER('MARK MINIMUM OF REQUIRED RANGE',ISTAT)
C*  call low level cursor
        CALL ARGS_CUROP('1','M')
        CALL ARGS_LRDC(IB,XMIN,Y)
        CALL ARGS_CURCL
        CALL WRUSER('MARK MAXIMUM OF REQUIRED RANGE',ISTAT)
        CALL ARGS_CUROP('1','M')
        CALL ARGS_LRDC(IB,XMAX,Y)
        CALL ARGS_CURCL
C*  ensure min and max values are in range 0..255
        XMIN=MOD(XMIN,256)
        XMAX=MOD(XMAX,256)
C*  if min and max wrong way round then switch them
        IF (XMIN.GT.XMAX) THEN
          IB=XMAX
          XMAX=XMIN
          XMIN=IB
        ENDIF
C*  from cursor values, work out histogram settings
        TMIN=HMIN+XMIN*(HMAX-HMIN)/NBIN2
        TMAX=HMIN+XMAX*(HMAX-HMIN)/NBIN2
        HMIN=TMIN
        HMAX=TMAX
        CALL WRITER('HMIN',HMIN,ISTATUS)
        CALL WRITER('HMAX',HMAX,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISWRITE')
        ELSE
C*  tell .SCL command proc. that program terminated successfully
          CALL WRITEI('SCLSTAT',0,ISTATUS)
        ENDIF
      ENDIF
      ENDIF
      END
