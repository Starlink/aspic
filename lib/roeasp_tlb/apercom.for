      SUBROUTINE APERCOM 
*+
*     APERCOM
*
*     Lists commands for aperture photometer.
*
*     B.D Kelly/ROE/1981.
*     C.Aspin/UOE/ FEB 1984.     {GIVE, POSN added}
*     A C Davenhall/ROE/13.3.84  {WRITE added}
*-
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER(' Aperture photometer',ISTAT)
      CALL WRUSER(' ',ISTAT)
      CALL WRUSER(' SHAPE     Set shape of aperture.',ISTAT)
      CALL WRUSER(' SIZE      Set diameter aperture.',ISTAT)
      CALL WRUSER(' POSN      Set position of aperture.',ISTAT)
      CALL WRUSER(' STAR      Wish to measure a star.',ISTAT)
      CALL WRUSER(' SKY       Wish to measure sky.',ISTAT)
      CALL WRUSER(' MAG       Compute zero-2.5*alog10(star-sky).',
     :       ISTAT)
      CALL WRUSER(' ZERO      Set magnitude zero-point.',ISTAT)
      CALL WRUSER(' WRITE     Writes current measure to a file.',
     :       ISTAT)
      CALL WRUSER(' GIVE      Lists current set-up.',ISTAT)
      CALL WRUSER(' E         Exit.',ISTAT)
      CALL WRUSER(' ',ISTAT)

      END
