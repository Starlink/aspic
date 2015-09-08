      SUBROUTINE GETCORR(NAME,MDROP,NUMPOS,IREJP,IST)
C+
C   GETCORR
C
C   Reads flat-field parameters from Starlink image header
C
C   Given      (arguments)
C   NAME        flat-field image parameter name
C   IST         input status
C
C   Returned   (arguments)
C   MDROP       scale-length of Martin filter used
C   NUMPOS      number of pixels rejected
C   IREJP       coordinates of rejected pixels
C   IST         status return = max(input status,output status)
C
C   Subroutine calls :
C   RDDSCR           : STARLINK
C
C   B.D.Kelly/ROE/26.9.1981
C-

      CHARACTER*(*) NAME
      CHARACTER*5 COORDS(2,4096)
      CHARACTER*5 SCALE
      INTEGER MDROP,NUMPOS
      INTEGER IREJP(2,4096)

      CALL RDDSCR('INPUT','FILSCAL',1,SCALE,JDUM,IST1)
      CALL RDDSCR('INPUT','COORDS',8192,COORDS,NVALS,IST2)
      NUMPOS=NVALS/2
      READ(SCALE,'(BN,I5)') MDROP
      DO J=1,NUMPOS
        DO I=1,2
          READ(COORDS(I,J),'(BN,I5)') IREJP(I,J)
        ENDDO
      ENDDO

      IST=MAX(IST,IST1,IST2)

      END
