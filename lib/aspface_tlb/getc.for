      SUBROUTINE GETC(BATCH,NAME,PROMPT,DEFAULT,MINC,MAXC,OUTPUT,ISTAT)
C+
C   GETC
C
C   Input a character variable from the Starlink environment.
C
C   Given      (arguments)
C   BATCH       =.TRUE. if running in batch, .FALSE. otherwise.
C   NAME        name of parameter required (matches name in
C               STARLINK CON file.
C   PROMPT      prompt string given to user
C   DEFAULT     default value
C   MINC        character string of lowest ASCII value acceptable
C   MAXC        character string of highest ASCII value acceptable
C   ISTAT       input status
C
C   Returned   (arguments)
C   OUTPUT      character string returned
C   ISTAT       status return = MAX(input status,output status)
C
C
C   Subroutine calls :
C    WRUSER,RDKEYC,CNPAR  : STARLINK
C
C   B.D.Kelly/ROE/18.9.1981
C-

      CHARACTER*(*) NAME,PROMPT,DEFAULT,MINC,MAXC,OUTPUT
      CHARACTER*80 BUFFER
      INTEGER ISTAT,IST1,IST2,ILEN,JDUM
      LOGICAL BATCH

      ILEN=LEN(OUTPUT)

   10 CONTINUE
      BUFFER=DEFAULT
      IF(.NOT.BATCH) CALL WRUSER(PROMPT,IST1)
      CALL RDKEYC(NAME,.TRUE.,1,BUFFER,JDUM,IST2)
      CALL CNPAR(NAME,IST1)
      ISTAT=MAX(ISTAT,IST2-1)

      IF(ISTAT.EQ.0) THEN

C
C     Check if lower-upper or upper-lower case conversions
C     are needed.
C
        IF((BUFFER.GE.'a').AND.(MAXC.LT.'a')) THEN
          DO JPOS=1,80
            IF(BUFFER(JPOS:JPOS).GE.'a') THEN
              BUFFER(JPOS:JPOS)=CHAR(ICHAR(BUFFER(JPOS:JPOS))-32)
            ENDIF
          ENDDO
        ELSE IF((BUFFER.LT.'a').AND.(MINC.GE.'a')) THEN
          DO JPOS=1,80
            IF(BUFFER(JPOS:JPOS).LT.'a') THEN
              BUFFER(JPOS:JPOS)=CHAR(ICHAR(BUFFER(JPOS:JPOS))+32)
            ENDIF
          ENDDO
        ENDIF

        IF((BUFFER.GT.MAXC).OR.(BUFFER.LT.MINC)) THEN
          IF(.NOT.BATCH) GO TO 10
          ISTAT=1
        ELSE
          OUTPUT=BUFFER(1:ILEN)
        ENDIF

      ENDIF

      END
