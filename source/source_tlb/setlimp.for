      PROGRAM SETLIMP

C+
C  program SETLIMP
C
C  part of IAM suite of programs.
C  change HMIN and HMAX, zero point and value of maximum bin in histogram
C  by typing in new values.
C
C  Given (program parameters)
C    HMIND,HMAXD (RA)     default limits of histo
C    HMIN,HMAX   (RA)     current limits of histo
C
C  Returned (program parameters)
C    HMIN (R)    zero point of histogram
C    HMAX (R)    value of maximum bin in histogram
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      REAL  HMIN,HMAX,HMIND,HMAXD
      INTEGER IST,ISTATUS
      CHARACTER MESSG*72
      INTEGER J

      ISTATUS=0
      IST=0
      CALL READR('HMIND',' ',0.0,0.0,1000000.0,HMIND,ISTATUS)
      CALL READR('HMAXD',' ',0.0,0.0,1000000.0,HMAXD,ISTATUS)
      CALL READR('HMIN',' ',0.0,0.0,1000000.0,HMIN,ISTATUS)
      CALL READR('HMAX',' ',0.0,0.0,1000000.0,HMAX,ISTATUS)
      IF (ISTATUS.NE.0) THEN
         CALL WRERR('MISREAD')
      ELSE
        IF ((HMIN.EQ.0.0) .AND. (HMAX.EQ.0.0)) THEN
          CALL WRUSER(' No current limits as IAMHIS not called',IST)
        ELSE
          WRITE(MESSG,10) HMIND,HMAXD
          CALL WRUSER(MESSG,IST)
10        FORMAT(' Default limits of histogram are',2F10.3)
20        FORMAT(' Current limits are',2F10.3)
          WRITE(MESSG,20) HMIN,HMAX
          CALL WRUSER(MESSG,IST)
        ENDIF
        CALL WRUSER(' Enter new values',IST)
        CALL CNPAR('HMIN',IST)
        CALL READR('HMIN','ENTER ZERO POINT',0.0,-1.0E19,1.0E19,
     :             HMIN,ISTATUS)
        CALL CNPAR('HMAX',IST)
        CALL READR('HMAX','ENTER VALUE OF MAX BIN',0.0,-1.0E19,1.0E19,
     :             HMAX,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISREAD')
        ELSE
          IF (HMIN.GT.HMAX) THEN
C*  if min and max wrong way round then switch them
            J=HMAX
            HMAX=HMIN
            HMIN=J
          ENDIF
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
