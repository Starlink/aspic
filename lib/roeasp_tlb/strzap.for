      SUBROUTINE STRZAP
C+
C      STRZAP.
C
C      **** - Only used in ASPIC.
C
C      Subroutine to allow the interactive removal of a star
C     image or other blamish from a 2D "Starlink" image. The
C     object removed is replaced by a 2D polynomial fitted
C     to the background. The perimeter of the object to be
C     removed is defined by a circular cursor.
C
C  Given;  None.
C
C  Returned; None.
C
C  Subroutines called;
C   E2DLIB;     STARZAP, COPY.
C   I/O;        INPICR, OUTPUT, CLEARIM, OUTPICR.
C   Args;       SRINIT.
C
C  A C Davenhall./ROE/                                  22/3/82.
C-
      INTEGER IXEXT,IYEXT,NAXIS(2),INPTR,OUTPTR
      INTEGER ISTIN,ISTOUT,IST
      CHARACTER BUFF*80
      CHARACTER CURCO1*1,CURCO2*1
C
C      Initialise an ARGS.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C      Setup colours for the cursor.
C
      CURCO1='Y'
      CURCO2='M'
C
C      Obtain a pointer to the "Starlink" image containing the
C      starfield.
C
      CALL INPICR ('INPUT',' Enter filename for input image;',
     :              2,NAXIS,INPTR,ISTIN)
C
C      Obtain a pointer to the output image.
C
      CALL OUTPICR ('OUTPUT',' Enter filename for output image;',
     :               2,NAXIS,OUTPTR,ISTOUT)
C
C      If the images have been obtained successfully (return status=0)
C      proceed to allow removal of the object.
C
      IF (ISTIN.EQ.0.AND.ISTOUT.EQ.0) THEN
        IXEXT=NAXIS(1)
        IYEXT=NAXIS(2)
C
C        Copy input image into output image.
C
        CALL COPY (IXEXT,IYEXT,%VAL(INPTR),IXEXT,IYEXT,%VAL(OUTPTR))
C
C        Remove stars.
C
        CALL STARZAP (9,CURCO1,10,CURCO2,IXEXT,IYEXT,%VAL(OUTPTR))
      ELSE
        CALL OUTPUT (
     :  ' Unable to obtain Starlink images satisfactorily.',IST)
        WRITE(BUFF,2000) ISTIN
 2000   FORMAT(1X,'Return status from attempt to obtain input',
     :         ' image  = ',I3)
        CALL OUTPUT (BUFF,IST)
        WRITE(BUFF,2001) ISTOUT
 2001   FORMAT(1X,'Return status from attempt to obtain output',
     :         ' image = ',I3)
        CALL OUTPUT (BUFF,IST)
      END IF
      CALL CLEARIM ('INPUT')
      CALL CLEARIM ('OUTPUT')
      END
