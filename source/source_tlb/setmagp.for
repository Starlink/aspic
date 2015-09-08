      PROGRAM SETMAGP

C+
C  program SETMAGP
C
C  set the IAM sky background magnitude in one pixel
C
C  Returned (program parameters)
C    SKYMAG (R)    sky background magnitude in one pixel
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      INTEGER SKYMAG

      ISTATUS=0
C*  ignore cnpar status
      CALL CNPAR('SKYMAG',IST)
      CALL READR('SKYMAG','ENTER SKYMAG IN 1 PIXEL',0.0,-1.0E19,1.0E19,
     :           SKYMAG,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL WRITER('SKYMAG',SKYMAG,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISWRITE')
        ELSE
C*  tell .SCL command proc. that program terminated successfully
          CALL WRITEI('SCLSTAT',0,ISTATUS)
        ENDIF
      ENDIF
      END
