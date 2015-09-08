      SUBROUTINE READR(NAME,PROMPT,DEFAULT,RMIN,RMAX,OUTPUT,ISTAT)
C+
C   READR
C
C   Obtain a real number from the environment.
C
C   ASPIC version
C
C   Given      (arguments)
C   NAME        name of parameter required
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
C   Subroutine calls :
C   GETR,BATCH             : ASPFACE/LIB
C
C   B.D.Kelly/ROE/19.9.1981
C   A C Davenhall./ROE/     14/8/84.
C      {Modified to echo value read when running in batch.}
C   A C Davenhall./ROE/     27/8/84.
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
C        value = rmin
C        istat = max(istat,1)
C      endif
C
C      end
C
C
      CHARACTER*(*) NAME,PROMPT
      REAL DEFAULT,RMIN,RMAX,OUTPUT
      INTEGER IBAT,ISTAR,ISTAT
      LOGICAL BATFL
      CHARACTER OUTBUF*80
      INTEGER NAMLEN

C
C   Check whether running in batch mode -
C
      CALL BATCH(BATFL)

      ISTAR=0
      CALL GETR(BATFL,NAME,PROMPT,DEFAULT,RMIN,RMAX,OUTPUT,ISTAR)

      IF(ISTAR.EQ.0) THEN
        ISTAT=MAX(ISTAT,ISTAR)
      ELSE IF((DEFAULT.GE.RMIN).AND.(DEFAULT.LE.RMAX)) THEN
        OUTPUT=DEFAULT
        ISTAT=MAX(ISTAT,0)
      ELSE
        OUTPUT=RMIN
        ISTAT=MAX(ISTAT,1)
      ENDIF
C
C    Echo value read if working in batch.
C
      IF (BATFL) THEN
         OUTBUF=' '
         CALL CHRLEN (NAME,NAMLEN)
         WRITE(OUTBUF,2000) NAME(1:NAMLEN),OUTPUT
 2000    FORMAT(2X,'... value obtained for ',A<NAMLEN>,' = ',1PE14.6)
         CALL WRUSER (OUTBUF,ISTAR)
      END IF

      END
