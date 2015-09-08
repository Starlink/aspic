      SUBROUTINE PUTCORR(NAME,MDROP,NUMPOS,IREJP,IST)
C+
C   PUTCORR
C
C   Writes parameters of flat-field correction to Starlink image header
C
C   Given      (arguments)
C   NAME        image name
C   MDROP       scale length used in Martin filter
C   NUMPOS      number of pixels rejected
C   IREJP       coordinates of rejected pixels
C   IST         input status
C
C   Returned   (arguments)
C   IST         returned status = MAX(input status,output status)
C
C   B.D.Kelly/ROE/28.9.1981
C-

      CHARACTER*(*) NAME
      CHARACTER*5 COORDS(2,4096)
      CHARACTER*5 SCALE
      INTEGER MDROP,IREJP(2,NUMPOS)

C
C   Put values into characters, then write to image header
C
      WRITE(SCALE,'(I5)') MDROP
      DO J=1,NUMPOS
        DO I=1,2
          WRITE(COORDS(I,J),'(I5)') IREJP(I,J)
        ENDDO
      ENDDO

      NVALS=2*NUMPOS
      CALL WRDSCR('OUTPUT','FILSCAL',SCALE,1,IST1)
      CALL WRDSCR('OUTPUT','COORDS',COORDS,NVALS,IST2)
      IST=MAX(IST,IST1,IST2)

      END
