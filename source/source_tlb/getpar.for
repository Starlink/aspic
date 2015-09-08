      PROGRAM GETPAR

C+
C  program GETPAR
C    	part of IAM suite
C  change the value of general IAM histogram parameter PAR
C  (which might be SKY or THRLD in practice)
C  using the cursor.
C  this should be used via the command procedures GETSKY,GETTHR.
C
C  Since have to use the low level cursor routine which returns
C  args coords not pixel, the cursor is just called from this program.
C  To use with the histogram the value returned from the cursor must
C  lie in the range 0..255. The histo may be in any of the quadrants
C  of the args and so the cursor value mod 256 is taken. Actually
C  only the x-coord is used.
C
C  Given (program parameters)
C    HMIN (R)    zero point of histogram
C    HMAX (R)    value of maximum bin in histogram
C
C  Returned (program parameters)
C    PAR (R)    histogram parameter like SKY or THRLD
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      REAL HMIN,HMAX,PAR
C*  variables for cursor call
      INTEGER X,Y,IB
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
        CALL WRUSER('PLACE CURSOR ON REQUIRED POINT ON HISTO',ISTAT)
C*  call low level cursor
        CALL ARGS_CUROP('1','G')
        CALL ARGS_LRDC(IB,X,Y)
        CALL ARGS_CURCL
C*  ensure in range 0..255
        X=MOD(X,256)
C*  from cursor value, work out histogram setting
        PAR=HMIN+X*(HMAX-HMIN)/NBIN2
        CALL WRITER('PAR',PAR,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISWRITE')
        ELSE
C*  tell .SCL command proc. that program terminated successfully
          CALL WRITEI('SCLSTAT',0,ISTATUS)
        ENDIF
      ENDIF
      ENDIF
      END
