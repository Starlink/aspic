      PROGRAM SETPERP

C+
C  program SETPERP
C
C  part of IAM suite of programs.
C  set the threshold (THRLD) parameter of histogram as percentage above sky background
C  note that the sky background must have been defined first.
C
C  Given (program parameters)
C    SKY (R)    value of sky background
C    PERCENT (R)    value of THRLD as a percentage of SKY background
C
C  Returned (program parameters)
C    THRLD (R)   threshold cut
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      REAL SKY,THRLD,PERCENT

      ISTATUS=0
      CALL READR('SKY',' ',0.0,-1.0E19,1.0E19,SKY,ISTATUS)
      IF (SKY.EQ.-99999.0) THEN
        CALL WRERR('NOSKY')
      ELSE
C*  ignore cnpar status
        CALL CNPAR('PERCENT',IST)
        CALL READR('PERCENT','GIVE THRLD AS A PERCENTAGE OF SKY',
     :               10.0,0.05,1000.0,PERCENT,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISREAD')
        ELSE
          THRLD=SKY + SKY*PERCENT/100.0
          CALL WRITER('THRLD',THRLD,ISTATUS)
          IF (ISTATUS.NE.0) THEN
            CALL WRERR('MISWRITE')
          ELSE
C*  tell .SCL command proc. that program terminated successfully
            CALL WRITEI('SCLSTAT',0,ISTATUS)
          ENDIF
        ENDIF
      ENDIF
      END
