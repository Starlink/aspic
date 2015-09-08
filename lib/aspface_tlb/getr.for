      SUBROUTINE GETR(BATCH,NAME,PROMPT,DEFAULT,RMIN,RMAX,OUTPUT,ISTAT)
C+
C   GETR
C
C   Input a real variable from the Starlink environment.
C
C   Given      (arguments)
C   BATCH       =.TRUE. if running in batch, .FALSE. otherwise.
C   NAME        name of parameter required (matches name in
C               STARLINK CON file.
C   PROMPT      prompt string given to user
C   DEFAULT     default value
C   RMIN        minimum value acceptable
C   RMAX        maximum value acceptable
C   ISTAT       input status
C
C   Returned   (arguments)
C   OUTPUT      value returned
C   ISTAT       status return = MAX(input status,output status)
C
C
C   Subroutine calls :
C    WRUSER,RDKEYR,CNPAR  : STARLINK
C
C   B.D.Kelly/ROE/18.9.1981
C-

      CHARACTER*(*) NAME,PROMPT
      REAL DEFAULT,RMIN,RMAX,OUTPUT
      REAL BUFFER
      INTEGER ISTAT,IST1,IST2,ILEN,JDUM
      LOGICAL BATCH


   10 CONTINUE
      BUFFER=DEFAULT
      IF(.NOT.BATCH) CALL WRUSER(PROMPT,IST1)
      CALL RDKEYR(NAME,.TRUE.,1,BUFFER,JDUM,IST2)
      CALL CNPAR(NAME,IST1)
      ISTAT=MAX(ISTAT,IST2-1)

      IF(ISTAT.EQ.0) THEN

        IF((BUFFER.GT.RMAX).OR.(BUFFER.LT.RMIN)) THEN
          IF(.NOT.BATCH) GO TO 10
          ISTAT=1
        ELSE
          OUTPUT=BUFFER
        ENDIF

      ENDIF

      END
