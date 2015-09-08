C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   IMANIC *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               IMANIC
C
C
C          FUNCTION:-
C               This enables you to make a new image from  an  area  of  an
C               existing I*2 EDRS type 2D image.
C
C
C          USE:-
C               For getting at bits of images.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The name of the input  image.
C                                             It must be an I*2 image
C
C         XLIMITS         Whole of Input      The limits in X of  the  area
C                                             you want
C
C         YLIMITS         Whole of Input      The limits in Y of  the  area
C                                             you want
C
C         OUTPUT                              The Output image
C
C         TITLE                               The Title to be attached as a
C                                             descriptor   of   the  Output
C                                             image
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         A.J.Penny                RGO                             8-AUG-82
C
C
C--------------------------------------------------------------------------






C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C         PROGRAM IMANIC
C
C
C CALLS
C             EDRS PACKAGE:
C                     GT2DIW,PTDSCR,RDDSCR,
C             STARLINK:
C                     FRDATA,RDKEYC,RDKEYI,WRUSER
C                     RDIMAG,CTOI,CTOR
C             THIS FILE:
C                     TRANS
C
C NOTES
C             USES VAX %VAL FACILITY
C
C
C
C
C -------------------------------------------------------------



      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
C
C
C
      INTEGER NSIZE(2),LIMX(2),LIMY(2)
      LOGICAL*1 VALID
      CHARACTER*72 TXT
      CHARACTER TEXT*72,CVAL*1,TITLE(1)*30
C
C Set continuation flag
C
      VALID = .TRUE.
C
C --------------------------------------------------------------
C
C   Get the image data array
C
      CALL GTIMAG(IPIN,NSIZE,BSCALE,BZERO,INVAL,IERR)
C
C  If cant pick up input frame, set the continuation flag to no
C
      IF (IERR.NE.0) THEN
         VALID = .FALSE.
         CALL WRUSER('NO VALID IMAGE',ISTAT)
      ENDIF
C
C -------------------------------------------------------------------
C
C  Get area of image to be transfered
C
      IF (VALID) THEN
         LIMX(1) = 1
         LIMX(2) = NSIZE(1)
         CALL RDKEYI('XLIMITS',.TRUE.,2,LIMX,NVAL,ISTAT)
         IF ((ISTAT.EQ.4).OR.
     +       (LIMX(1).LT.1.OR.LIMX(1).GT.NSIZE(1)).OR.
     +       (LIMX(2).LT.LIMX(1)).OR.
     +       (LIMX(2).GT.NSIZE(1))) VALID = .FALSE.
         IF (VALID.EQ.FALSE) THEN
            CALL WRUSER('BAD LIMITS',ISTAT)
         ELSE
            LIMY(1) = 1
            LIMY(2) = NSIZE(2)
            CALL RDKEYI('YLIMITS',.TRUE.,2,LIMY,NVAL,ISTATA)
            IF ((ISTATA.EQ.4).OR.
     +          (LIMY(1).LT.1.OR.LIMY(1).GT.NSIZE(2)).OR.
     +          (LIMY(2).LT.LIMY(1)).OR.
     +          (LIMY(2).GT.NSIZE(2))) VALID = .FALSE.
            IF (VALID.EQ.FALSE) THEN
               CALL WRUSER('BAD LIMITS',ISTAT)
            ELSE
               NPIX = LIMX(2) - LIMX(1) + 1
               NLINES = LIMY(2) - LIMY(1) + 1
            ENDIF
         ENDIF
      ENDIF
C
C ----------------------------------------------------------------
C
C  Obtain Output image
C
C
      IF (VALID) THEN
         CALL GT2DIW('OUTPUT',102,.FALSE.,NPIX,NLINES,IPOUT,IERR)
         IF(IERR.EQ.0) THEN
C
C  Get Title
C
            TITLE(1) = 'Output from IMANIC'
            CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C LOAD TITLE AND DESCRIPTORS
C
            CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE(1),
     +                 IERR)
            CALL PTDSCR('OUTPUT',' INVAL','INTEGER',INVAL,RVAL,
     +                  CVAL,IERR)
            CALL PTDSCR('OUTPUT','BSCALE','REAL',IVAL,BSCALE,CVAL,
     +                  IERR)
            CALL PTDSCR('OUTPUT','BZERO','REAL',IVAL,BZERO,CVAL,IERR)
         ELSE
            VALID = .FALSE.
            CALL WRUSER('CANT GET THAT FILE',ISTAT)
         ENDIF
      ENDIF
C
C -------------------------------------------------------------------
C
C  Transfer area
C
      IF (VALID) THEN
         CALL TRANS(%VAL(IPIN),NSIZE(1),NSIZE(2),
     +              %VAL(IPOUT),NPIX,NLINES,
     +              LIMX(1),LIMY(1))
      ENDIF
C
C -------------------------------------------------------------------
C
C  Tidy up and exit
C
      CALL FRDATA(' ',ISTAT)
      CALL EXIT
C
C
C
      END



C
C CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C          *************
C          *           *
C          * S/R TRANS *
C          *           *
C          *************
C
C  This transfers an area of one I*2 image to another
C
C --------------------------------------------------------------
C
C
C
      SUBROUTINE TRANS(IM1,KX1,KY1,IM2,KX2,KY2,KXS,KYS)
C
C
C
      INTEGER*2 IM1(KX1,KY1),IM2(KX2,KY2)
C
C
C
      DO K = 1,KY2
         DO J = 1,KX2
            IM2(J,K) = IM1(J+KXS-1,K+KYS-1)
         ENDDO
      ENDDO
C
C
C
      END



