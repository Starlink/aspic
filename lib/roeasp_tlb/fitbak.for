      SUBROUTINE FITBAK
C+
C     FITBAK.
C
C     Subroutine to fit a polynomial to the background of a
C     specified image, avoiding predeterimned avoidance 
C     zones. The polynomial evaluated for every point in the
C     image and normailsed image obtained by dividing
C     the polynomial into the original image are generated
C     as output frames.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:-  INPICR, OUTPICR, READI, READC, OUTPUT,
C                 CLEARIM.
C   E2D:-         CELL, FITBK1.
C
C  Structure:-
C   Attempt to obtain a pointer to the input image
C   If pointer obtained Ok.
C     Attempt to obtain pointer to input exclusion zone file
C     If pointer obtained Ok
C       Attempt to obtain output pointer to polynomial array.
C          "    "    "      "      "     "  normalised image.
C       if pointer obtained Ok
C         ....
C         make fit
C       else
C         print message
C       end if
C     else
C       print message explaining how to set up avoidance boxes.
C     end if
C   else
C     print message
C   end if
C   Tidy up images.
C
C  A C Davenhall./ROE/                                     17/8/82.
C-
      INTEGER IMPTR,IMSTAT,XEXT,YEXT,AXISIZ(2)
C
      INTEGER BOXPTR,BXSTAT,BOXSIZ(2),NAVOID
C
      INTEGER POLPTR,NORPTR,SAPTR,OUTSTT
C
      INTEGER COEFX,COEFY,ICELL
C
      INTEGER IOSTAT
C
      CHARACTER TITLE*(30)
C
C
C
C    Attempt to obtain a pointer to the input image to be 
C    fitted, and proceed if pointer obtained Ok.
C
      IMSTAT=0
      CALL INPICR ('INPIC1',
     :  ' Enter filename for the input image;',
     :    2,AXISIZ,IMPTR,IMSTAT)
      IF (IMSTAT.EQ.0) THEN
C
C    Attempt to obtain a pointer to the exclusion zones files
C    and proceed if successful.
C
        BXSTAT=0
        CALL INPICR ('BOXFIL',
     :   ' Enter name of file holding exclusion zones;',
     :     2,BOXSIZ,BOXPTR,BXSTAT)
        IF (BOXSIZ(2).NE.4) BXSTAT=33
        IF (BXSTAT.EQ.0) THEN
          NAVOID=BOXSIZ(1)
C
C    Attempt to obtain pointer to output images to hold the
C    evaluated polynomial and the normalised image.
C
          XEXT=AXISIZ(1)
          YEXT=AXISIZ(2)
          OUTSTT=0
          CALL OUTPICR ('POLBAK',
     :  ' Enter name of the file to hold the evaluated polynomial;',
     :      2,AXISIZ,POLPTR,OUTSTT)
          CALL OUTPICR ('NORIMG',
     :  ' Enter name of the file to hold the normalised image;',
     :      2,AXISIZ,NORPTR,OUTSTT)
          CALL OUTPICR ('WORK1',' File for scales avoidance boxes.',
     :      2,BOXSIZ,SAPTR,OUTSTT)
C
C    Proceed if these pointers obtained Ok.
C
          IF (OUTSTT.EQ.0) THEN
C
C    Obtain the number of coefficients to be fitted in each 
C    dimension.
C
            CALL READI ('COEFX',' No. of X coefficients;',
     :         4,1,30,COEFX,IOSTAT)
            CALL READI ('COEFY',' No. of Y coefficients;',
     :         COEFX,1,30,COEFY,IOSTAT)
C
C    Obtain the averaging cell side size.
C
            ICELL=32
            CALL CELL (XEXT,YEXT,ICELL)
C
C    Obtain the title.
C
            CALL READC ('TITLE',
     :       ' Enter title to appear on listings etc.',
     :       ' ',' ','~',TITLE,IOSTAT)
C
C    Finally make the fit.
C
            CALL FITBK1 (XEXT,YEXT,%VAL(IMPTR),NAVOID,4,%VAL(BOXPTR),
     :                   COEFX,COEFY,ICELL,TITLE,
     :                   %VAL(SAPTR),%VAL(POLPTR),%VAL(NORPTR))
          ELSE
            CALL OUTPUT (
     : ' ***ERROR Unable to successfully obtain output files.',
     :            IOSTAT)
          END IF
        ELSE
          CALL OUTPUT (
     : ' ***ERROR Unable to obtain file of exclusion zones.',IOSTAT)
          CALL OUTPUT (
     : '          A  suitable file can be generated using ABOXASP.',
     :             IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     : ' ***ERROR Unable to obtain imput image.',IOSTAT)
      END IF
C
C    Tidy up the input images.
C
      CALL CLEARIM ('INPIC1')
      CALL CLEARIM ('BOXFIL')
      CALL CLEARIM ('POLBAK')
      CALL CLEARIM ('NORIMG')
      CALL CLEARIM ('WORK1')
      END
