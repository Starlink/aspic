      SUBROUTINE JMOOTH 
C+
C     JMOOTH.
C
C     Subroutine to perform a 2 dimensional smooth, following
C     the method of Jones et al (1967). The degree of smoothing
C     to which a pixel is subjected depends on the intensity of 
C     the pixel.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Smoothing; JONES.
C   Interfaces; INPICR, OUTPICR, CLEARIM, READR, OUTPUT.
C
C  Reference;
C   Jones et al (1967) Pub. Dept. Astr. Univ. of Texas at Austin,
C               Ser II, Vol.1, No.8.
C
C  Structure;
C   Obtain pointer to input image.
C     "      "     "  output  "  .
C   If pointers obtained successfully
C     Obtain value tor first threshold.
C       "      "    "  second   "     .
C     If thresholds not obtained satisfactorily
C       reset the defaults.
C       print a message.
C     end if
C     Smooth the image.
C     If return status from smoothing not Ok
C       print message
C     end if
C   else
C     print a message saying unable to obtain image pointers.
C   end if
C   clearup images.
C
C  A C Davenhall./ROE/                              27/5/82.
C-
      INTEGER NAXIS(2),INPTR,OUTPTR
      INTEGER STAT1,STAT2,STAT3,STAT4,IXEXT,IYEXT
      REAL THRESH1,THRESH2
C
C      Attempt to obtain a pointer to the input image.
C
      STAT1=0
      CALL INPICR ('INPIC1',
     :  ' Enter the filename for the image to be smoothed;',
     :    2,NAXIS,INPTR,STAT1)
C
C      Attempt to obtain a pointer to the output image.
C
      CALL OUTPICR ('OUTPIC1',
     :  ' Enter filename for the smoothed image;',
     :    2,NAXIS,OUTPTR,STAT1)
C
C      Proceed if the pointers have been successfully obtained.
C
      IF (STAT1.EQ.0) THEN
C
C      Obtain a value for the first threshold.
C      (defaults taken from Jones et al (1967) p63).
C
        STAT2=0
        CALL READR ('THRESH1',
     :    ' Enter upper threshold for 5 pt. smoothing;',
     :      4.0E-2,-5.0E-7,5.0E7,THRESH1,STAT2)
C
C      Obtain a value for the second threshold.
C
        CALL READR ('THRESH2',
     :    ' Enter upper threshold for 3 pt. smoothing;',
     :      1.6E-1,-5.0E7,5.0E7,THRESH2,STAT2)
C
C      If the values have not been obtained properly
C      restore the defaults.
C
        IF (STAT2.NE.0) THEN
          THRESH1=4.0E-2
          THRESH2=1.6E-1
          CALL OUTPUT (
     :     ' ERROR*** Unable to obtain values for the thresholds',STAT3)
          CALL OUTPUT (
     :     '          Defaults restored.',STAT3)
        END IF
C
C      Smooth the image.
C
        CALL OUTPUT (' Please wait. Smoothing in progress.',STAT3)
        IXEXT=NAXIS(1)
        IYEXT=NAXIS(2)
        CALL JONES (THRESH1,THRESH2,IXEXT,IYEXT,%VAL(INPTR),
     :              %VAL(OUTPTR),STAT4)
        IF (STAT4.NE.0) THEN
          CALL OUTPUT (
     :     ' ERROR*** Image too small to be smoothed.',STAT3)
          CALL OUTPUT (
     :     '          Copied without smoothing.',STAT3)
        END IF
      ELSE
C
C      Unable to get pointers to Starlink images.
C
        CALL OUTPUT (
     :   ' ERROR*** Unable to obtain image pointers successfully.',
     :     STAT3)
      END IF
C
C      Clear up the images.
C
      CALL CLEARIM ('INPIC1')
      CALL CLEARIM ('OUTPIC1')
      END
