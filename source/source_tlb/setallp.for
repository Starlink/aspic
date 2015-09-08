      PROGRAM SETALLP

C+
C  program SETALLP
C
C	allows IAM parameters to be changed
C	default is old values
C
C  Given (program parameters)
C    AREA (I)    area cut
C    SKY  (R)    sky background
C    THRLD(R)    threshold cut
C    SKYMAG(R)   sky magnitude in one pixel
C
C  Returned (program parameters)
C    AREA (I)    ............
C    SKY  (R)    ............
C    THRLD(R)    ............
C    SKYMAG(R)   ............
C
C  B. D. KELLY/ROE/1981
C  D. Tudhope/ROE/Sept 1982
C-

      INTEGER AREA
      REAL SKY,THRLD,SKYMAG
      INTEGER OLDAREA
      REAL OLDSKY,OLDTHR,OLDMAG

      ISTATUS=0
      CALL READI('AREA',' ',10,1,10000,OLDAREA,ISTATUS)
      CALL READR('SKY',' ',0.0,-1.0E19,1.0E19,OLDSKY,ISTATUS)
      CALL READR('THRLD',' ',0.0,-1.0E19,1.0E19,OLDTHR,ISTATUS)
      CALL READR('SKYMAG',' ',0.0,-1.0E19,1.0E19,OLDMAG,ISTATUS)
      IF (ISTATUS.NE.0) THEN
        CALL WRERR('MISREAD')
      ELSE
        CALL CNPAR('AREA',ISTATUS)
        CALL READI('AREA','ENTER AREA CUT',OLDAREA,1,10000,
     :             AREA,ISTATUS)
        CALL CNPAR('SKY',ISTATUS)
        CALL READR('SKY','ENTER SKY BACKGROUND',OLDSKY,-1.0E19,1.0E19,
     :             SKY,ISTATUS)
        CALL CNPAR('THRLD',ISTATUS)
        CALL READR('THRLD','ENTER THRESHOLD CUT',OLDTHR,-1.0E19,1.0E19,
     :             THRLD,ISTATUS)
        CALL CNPAR('SKYMAG',ISTATUS)
        CALL READR('SKYMAG','ENTER SKYMAG IN 1 PIXEL',OLDMAG,
     :             -1.0E19,1.0E19,SKYMAG,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISREAD')
        ELSE
          CALL WRITEI('AREA',AREA,ISTATUS)
          CALL WRITER('SKY',SKY,ISTATUS)
          CALL WRITER('THRLD',THRLD,ISTATUS)
          CALL WRITER('SKYMAG',SKYMAG,ISTATUS)
          IF (ISTATUS.NE.0) CALL WRERR('MISWRITE')
        ENDIF
      ENDIF
      END
