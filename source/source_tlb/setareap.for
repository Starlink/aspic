      PROGRAM SETAREAP

C+
C  program SETAREAP
C
C  set the IAM area cut
C
C  Returned (program parameters)
C    AREA (I)    area cut
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      INTEGER AREA

      ISTATUS=0
C*  ignore cnpar status
      CALL CNPAR('AREA',IST)
      CALL READI('AREA','GIVE AREA CUT',10,1,10000,AREA,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL WRITEI('AREA',AREA,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISWRITE')
        ELSE
C*  tell .SCL command proc. that program terminated successfully
          CALL WRITEI('SCLSTAT',0,ISTATUS)
        ENDIF
      ENDIF
      END
