      SUBROUTINE STAREM
C+
C     STAREM.
C
C     Subroutine for interactive star removal from a galaxy image.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICR, OUTPICR, OUTPUT, CLEARIM.
C   E2D:-         COPY, EYEBALL.
C
C  A C Davenhall./ROE/                                     25/9/82.
C-
      INTEGER AXIS(2),INPTR,OUTPTR
      INTEGER IMSTAT,IOSTAT
      INTEGER XEXT,YEXT
C
C
C    Attempt to obtain a pointer to the input image.
C
      IOSTAT=0
      IMSTAT=0
      CALL INPICR ('IMAGEIN',
     :  ' Enter name of the input file;',
     :    2,AXIS,INPTR,IMSTAT)
C
C    Attempt to obtain a pointer to the output image.
C
      CALL OUTPICR ('IMAGEOUT',
     :  ' Enter name of the output file;',
     :    2,AXIS,OUTPTR,IMSTAT)
C
C    Proceed if the pointers have been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
        XEXT=AXIS(1)
        YEXT=AXIS(2)
C
C    Copy the input file to the output file.
C
        CALL COPY (XEXT,YEXT,%VAL(INPTR),XEXT,YEXT,%VAL(OUTPTR))
C
C    Remove the objects.
C
        CALL EYEBALL (XEXT,YEXT,%VAL(OUTPTR))
      ELSE
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain files successfully.',IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('IMAGEIN')
      CALL CLEARIM ('IMAGEOUT')
      END
