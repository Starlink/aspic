      SUBROUTINE MULCONS
C+
C     MULCONS.
C
C     Subroutine to produce a contour map of one or mor regions of an
C     image on the Versatec.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- INPICR, OUTPICR, YESNO, READR, OUTPUT, CLEARIM.
C   ROEASP:-     VRCONS.
C
C  Structure:-
C   Attempt to obtain a pointer to the input image.
C   Attempt to obtain a pointer to a work array.
C   If pointer obtained ok
C     Inquire if the plot is to be the entire array or just regions of it.
C     If entire array
C       Set up for contouring the entire array.
C     else
C       inquire for the file holding the coords of the regions
C        to be contoured.
C     end if
C     If (file obtained ok .or. not needed)
C       inquire for the faintest countour
C       inquire for the increment between contours.
C       Plot the contours.
C     else
C       print message
C     end if
C   else
C     print message.
C   end if
C   tidy up the images.
C
C  A C Davenhall./ROE. {Modified from MAGCNT.}            1/2/84.
C-
      IMPLICIT NONE
C
      INTEGER XEXT,YEXT,AXISIZ(2),IMPTR,IMSTAT
      INTEGER IOSTAT
      INTEGER BOXPTR,BXSTAT,BOXSIZ(2),MAXREG,MAXPAR
      PARAMETER (MAXREG=30)
      PARAMETER (MAXPAR=4)
      INTEGER  REGION(MAXREG,MAXPAR),NREG
      REAL SKY,THRESH,INCR
      CHARACTER REPLY*1, TITLE*30
C
C
C    Attempt to obtain pointer to the image to be contoured
C    and a work array of equal size.
C
      IOSTAT=0
      IMSTAT=0
      CALL INPICR ('IMAGE',
     : ' Enter filename of the image to be contoured;',
     :   2,AXISIZ,IMPTR,IMSTAT)
C
C    Proceed if the pointers have been obtained Ok.
C
      IF (IMSTAT.EQ.0) THEN
        XEXT=AXISIZ(1)
        YEXT=AXISIZ(2)
C
C    Inquire if the plot is to be made from the entire image,
C    or a set of regions on it.
C
        CALL YESNO (' Is the entire array to be contoured?',
     :              'N',REPLY,IOSTAT)
        IF (REPLY.EQ.'Y') THEN
C
C    Set up for contouring the entire array.
C
          NREG=1
          REGION(1,1)=1
          REGION(1,2)=1
          REGION(1,3)=XEXT
          REGION(1,4)=YEXT
          BXSTAT=0
        ELSE
C
C    Attempt to obtain a file containing the coords. of the
C    region to be contoured.
C
          BXSTAT=0
          CALL INPICR ('BOXFIL',
     :      ' Enter file of regions to be contoured;',
     :        2,BOXSIZ,BOXPTR,BXSTAT)
C
C    Copy into the internal array if the pointer has been
C    obtained Ok.
C
          IF (BXSTAT.EQ.0) THEN
            NREG=BOXSIZ(1)
            CALL COPY (BOXSIZ(1),BOXSIZ(2),%VAL(BOXPTR),
     :                 MAXREG,MAXPAR,REGION)
          END IF
        END IF
C
C    Proceed if the file has been obtained ok or is not needed.
C
        IF (BXSTAT.EQ.0) THEN
C
C    Obtain values for the contouring threshold and increment from
C    the environment.
C
          CALL READR ('THRESH',
     :     ' Enter faintest contour;',
     :       1.0E2,-1.0E5,1.0E5,THRESH,IOSTAT)
          CALL READR ('INCR',
     :     ' Enter increment between contours;',
     :       1.0E0,1.0E-5,1.0E5,INCR,IOSTAT)
          CALL READC ('TITLE',
     :     ' Enter title to appear on contour map;',
     :     '  ','  ','~',TITLE,IOSTAT)
          CALL OUTPUT (
     :     ' Please wait. Contours being produced now.',IOSTAT)
C
C    Plot the contours.
C
          CALL VRCONS (XEXT,YEXT,%VAL(IMPTR),MAXREG,MAXPAR,REGION,
     :                 NREG,THRESH,INCR,TITLE)
        ELSE
          CALL OUTPUT (
     :      ' Unable to obtain file of regions to be contoured.',
     :        IOSTAT)
          CALL OUTPUT (
     :      ' Such a file can be generated using ABOXASP.',IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     : '***ERROR Unable to obtain input files correctly.',IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('IMAGE')
      CALL CLEARIM ('BOXFIL')
      END
