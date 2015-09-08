      SUBROUTINE PECALB
C+
C     PECALB.
C
C     Top level subroutine to drive the routines to perform
C     interactive absolute calibration of a galaxy image
C     using multi-aperture photoelectric photometry.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-           INPLT, ABSCALB.
C   Interfaces:-    INPICR, OUTPUT, CLEARIM.
C   ARGS:-          SRINIT.
C
C  A C Davenhall./ROE/                                        21/7/82.
C-
      INTEGER IFAIL,STATUS,IXEXT,IYEXT,IMPTR,ICST,IRST
      INTEGER NAXIS(2)
      REAL PLTSCL
C
      REAL PARMFIL(50)
      INTEGER PARMSIZ
      PARAMETER (PARMSIZ=50)
C
C
C    Initialise the Args.
C
      IFAIL=0
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C    Attempt to obtain a pointer to the image to be calibrated.
C
      STATUS=0
      CALL INPICR ('INPIC1',
     :  ' Enter filename of the image to be calibrated;',
     :    2,NAXIS,IMPTR,STATUS)
C
C    Proceed if the image pointer has been obtained Ok.
C
      IF (STATUS.EQ.0) THEN
C
C    Obtain the appropraite bits of the "parameter-descriptor"
C    file for this image.
C
        CALL INPLT (ICST,IRST,PLTSCL,IFAIL)
        PARMFIL(1)=FLOAT(ICST)
        PARMFIL(2)=FLOAT(IRST)
        PARMFIL(3)=PLTSCL
C
C    Perform the absolute calibration.
C
        IXEXT=NAXIS(1)
        IYEXT=NAXIS(2)
        CALL ABSCALB (%VAL(IMPTR),IXEXT,IYEXT,PARMFIL,PARMSIZ)
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain image successfully.',IFAIL)
      END IF
C
C    Tidy up the image.
C
      CALL CLEARIM ('INPIC1')
      END
