      SUBROUTINE READC(NAME,PROMPT,DEFAULT,MINC,MAXC,OUTPUT,ISTAT)
C+
C   READC
C
C   Obtain a character string from the environment.
C
C   ASPIC version
C
C   Given      (arguments)
C   NAME        name of parameter required
C   PROMPT      prompt string given to user
C   DEFAULT     default value
C   MINC        string of lowest ASCII value acceptable
C   MAXC        string of highest ASCII value acceptable
C   ISTAT       input status
C
C   Returned   (arguments)
C   OUTPUT      character string returned
C   ISTAT       status return = MAX(input status,output status)
C
C   Subroutine calls :
C   GETC,BATCH             : ASPFACE/LIB
C
C   B.D.Kelly/ROE/19.9.1981
C   A C Davenhall./ROE/  14/8/84.
C      {Modified to echo values read when running in batch.}
C   A C Davenhall./ROE/  27/8/84.
C      {Modified to include the parameter name in the echo.} 
C-
C   The algorithm is :-
C
C      find whether in batch or not
C
C      do starlink calls
C
C      if(starlink call o.k.) then
C        istat = max(istat,istarlink)
C      else if(default in range) then
C        value = default
C        istat = max(istat,0)
C      else
C        value = minc
C        istat = max(istat,1)
C      endif
C
C      end
C
C
      CHARACTER*(*) NAME,PROMPT,DEFAULT,MINC,MAXC,OUTPUT
      INTEGER IBAT,ISTAR,ISTAT
      LOGICAL BATFL
      CHARACTER  OUTBUF*80
      INTEGER  OUTLEN,NAMLEN
 
      ILEN=LEN(OUTPUT)
      JLEN=LEN(DEFAULT)
C
C   Check whether running in batch mode -
C
      CALL BATCH(BATFL)

      ISTAR=0
      CALL GETC(BATFL,NAME,PROMPT,DEFAULT,MINC,MAXC,OUTPUT,ISTAR)

      IF(ISTAR.EQ.0) THEN
        ISTAT=MAX(ISTAT,ISTAR)
      ELSE IF((DEFAULT.GE.MINC).AND.(DEFAULT.LE.MAXC)) THEN
        OUTPUT=DEFAULT(1:MIN(ILEN,JLEN))
        ISTAT=MAX(ISTAT,0)
      ELSE
        JLEN=LEN(MINC)
        OUTPUT=MINC(1:MIN(ILEN,JLEN))
        ISTAT=MAX(ISTAT,1)
      ENDIF
C
C    Echo value read if working in batch.
C
      IF (BATFL) THEN
         OUTBUF=' '
         CALL CHRLEN (NAME,NAMLEN)
         CALL CHRLEN (OUTPUT,OUTLEN)
         OUTLEN=MAX(OUTLEN,1)
         OUTLEN=MIN(OUTLEN,30)
         WRITE(OUTBUF,2000) NAME(1:NAMLEN),OUTPUT(1:OUTLEN)
 2000    FORMAT(2X,'... value obtained for ',A<NAMLEN>,' = ',
     :     A<OUTLEN>)
         CALL WRUSER (OUTBUF,ISTAR)
      END IF

      END
