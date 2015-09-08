      SUBROUTINE GETI(BATCH,NAME,PROMPT,DEFAULT,IMIN,IMAX,OUTPUT,ISTAT)
C+
C   GETI
C
C   Input an integer variable from the Starlink environment.
C
C   Given      (arguments)
C   BATCH       =.TRUE. if running in batch, .FALSE. otherwise.
C   NAME        name of parameter required (matches name in
C               STARLINK CON file.
C   PROMPT      prompt string given to user
C   DEFAULT     default value
C   IMIN        minimum value acceptable
C   IMAX        maximum value acceptable
C   ISTAT       input status
C
C   Returned   (arguments)
C   OUTPUT      value returned
C   ISTAT       status return = MAX(input status,output status)
C
C
C   Subroutine calls :
C    WRUSER,RDKEYI,CNPAR  : STARLINK
C
C   B.D.Kelly/ROE/18.9.1981
C-

      CHARACTER*(*) NAME,PROMPT
      INTEGER DEFAULT,IMIN,IMAX,OUTPUT
      INTEGER BUFFER
      INTEGER ISTAT,IST1,IST2,ILEN,JDUM
      LOGICAL BATCH


   10 CONTINUE
      BUFFER=DEFAULT
      IF(.NOT.BATCH) CALL WRUSER(PROMPT,IST1)
      CALL RDKEYI(NAME,.TRUE.,1,BUFFER,JDUM,IST2)
      CALL CNPAR(NAME,IST1)
      ISTAT=MAX(ISTAT,IST2-1)

      IF(ISTAT.EQ.0) THEN

        IF((BUFFER.GT.IMAX).OR.(BUFFER.LT.IMIN)) THEN
          IF(.NOT.BATCH) GO TO 10
          ISTAT=1
        ELSE
          OUTPUT=BUFFER
        ENDIF

      ENDIF

      END
