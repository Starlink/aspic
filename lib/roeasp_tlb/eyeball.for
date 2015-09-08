      SUBROUTINE EYEBALL (XEXT,YEXT,ARRAY)
C+
C     EYEBALL.
C
C     Menu driven subroutine for interactive object removal.
C
C  Given;
C   XEXT   (I)  X size of image.
C   YEXT   (I)  Y  "   "    "  .
C   ARRAY  (RA) Image array.
C
C  Returned;
C   ARRAY  (RA) Image array modified by removal of objects.
C
C  Subroutines called;
C   Interfaces:-  OUTPUT, READC.
C   Args cursor:- RECUR.
C   E2D:-         HEYEBLL, AVER, ZAP, CURCOL, STARZAP.
C
C  A C Davenhall./ROE/                                   1/9/81.
C  A C Davenhall./ROE/  {Modified}                       25/9/82.
C-
      INTEGER XEXT,YEXT
      INTEGER ARRAY(XEXT,YEXT)
C
      INTEGER IOSTAT,CURSTT
      INTEGER IX1,IX2,IY1,IY2
      REAL AVRSKY
      LOGICAL GO_ON
      CHARACTER COMAND*80
      CHARACTER CURCO1*1,CURCO2*1
C
C    Overlay planes for the cursors.
C
      INTEGER PLANE1,PLANE2
      PARAMETER (PLANE1=13)
      PARAMETER (PLANE2=14)
C
C
C    Default colours for the cursors.
C
      CURCO1='Y'
      CURCO2='M'
      CALL OUTPUT ('  ',IOSTAT)
      CALL OUTPUT (' Interactive Object Removal.',IOSTAT)
      CALL OUTPUT ('  ',IOSTAT)
      CALL HEYEBLL
      GO_ON=.TRUE.
C
      DO WHILE (GO_ON)
C
C    Obtain command.
C
        IOSTAT=0
        CALL READC ('COMAND',' Enter command:-',
     :              ' ',' ','~',COMAND,IOSTAT)
C
C    Compute average sky value.
C
        IF (COMAND.EQ.'SKY'.OR.COMAND.EQ.'SK') THEN
          CURSTT=0
          CALL RECUR (PLANE1,CURCO1,PLANE2,CURCO2,IX1,IY1,IX2,IY2,
     :                CURSTT)
          IF (CURSTT.EQ.0) THEN
            CALL AVER (XEXT,YEXT,ARRAY,IX1,IY1,IX2,IY2,AVRSKY)
          ELSE
            CALL OUTPUT (
     :       ' ***ERROR Obtaining coords. from the cursor.',IOSTAT)
          END IF
        END IF
C
C    Mark a patch with the cursor.
C
        IF (COMAND.EQ.'SET'.OR.COMAND.EQ.'S') THEN
          CURSTT=0
          CALL RECUR (PLANE1,CURCO1,PLANE2,CURCO2,IX1,IY1,IX2,IY2,
     :                CURSTT)
        END IF
C
C    Remove object.
C
        IF (COMAND.EQ.'ZAP'.OR.COMAND.EQ.'Z') THEN
          CALL ZAP (ARRAY,XEXT,YEXT,IX1,IY1,IX2,IY2,AVRSKY) 
          CALL OUTPUT (' Object removed.',IOSTAT)
        END IF
C
C   Change the cursor colour.
C
        IF (COMAND.EQ.'CURCOL'.OR.COMAND.EQ.'C') THEN
          CALL CURCOL (CURCO1,CURCO2,CURSTT)
          IF (CURSTT.NE.0) THEN
            CURCO1='Y'
            CURCO2='M'
          END IF
        END IF
C
C    Replace object with polynomial fitted to the periphery.
C
        IF (COMAND.EQ.'REPLACE'.OR.COMAND.EQ.'R') 
     :      CALL STARZAP (PLANE1,CURCO1,PLANE2,CURCO2,XEXT,YEXT,ARRAY)
C
C    List the commands available.
C
        IF (COMAND.EQ.'HELP'.OR.COMAND.EQ.'H')
     :      CALL HEYEBLL
C
C    Terminate this process.
C
        IF (COMAND.EQ.'EXIT'.OR.COMAND.EQ.'E')
     :       GO_ON=.FALSE.
      END DO
C
      END
