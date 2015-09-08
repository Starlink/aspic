      SUBROUTINE HISTS                                                           
C+
C   HISTS
C
C   Calculate histogram of an image, plot on graphics terminal,
C   and compute statistical properties of the image.
C
C   Subroutine calls :
C   INPICR,READR,HIST1  : E2DLIB
C
C   B.D.Kelly/ROE/1981
C   A C Davenhall./ROE/   {modified}                      2/12/82.
C-
      INTEGER NAXIS(2),IXEXT,IYEXT,NPTR
C
      IST=0
C
      CALL INPICR ('INPIC1','Enter name of input image;',
     :             2,NAXIS,NPTR,IST)
      CALL READI ('IXST','Give X-index of leftmost pixel;',
     &           1,1,NAXIS(1),IXST,IST)
      CALL READI ('IXFIN','Give X-index of rightmost pixel;',
     &           NAXIS(1),IXST,NAXIS(1),IXFIN,IST)
      CALL READI ('IYST','Give Y-index of bottom pixel;',
     &           1,1,NAXIS(2),IYST,IST)
      CALL READI ('IYFIN','Give Y-index of top pixel;',
     &           NAXIS(2),IYST,NAXIS(2),IYFIN,IST)
C
      IF(IST.EQ.0) THEN
        CALL HIST1(NAXIS(1),NAXIS(2),%VAL(NPTR),IXST,IXFIN,IYST,IYFIN)
      ELSE
        CALL WRUSER (
     &    '***ERROR Unable to obtain input image successfully.',IST)
      ENDIF
C
      CALL CLEARIM('INPIC1')
C
      END
