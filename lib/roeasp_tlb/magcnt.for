      SUBROUTINE MAGCNT
C+
C     MAGCNT.
C
C     Subroutine to produce a contour map of one or regions of an
C     image on the versatec in mag./sq.arcsec. The input image
C     should be held as intensity normalised to a sky of 1.0.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   Interfaces:- INPICR, OUTPICR, YESNO, READR, OUTPUT, CLEARIM.
C   E2D:-        COPY, PSEUMG, VRPLTS.
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
C       inquire for the sky brightness (mag./sq.arcsec)
C       inquire for the faintest countour
C       inquire for the increment between contours.
C       Copy the input array to a work array
C       convert the work array to pseudo-magnitudes (2.5logI)
C       compute the threshold in pseudo-magnitudes corresponding
C        to the absolute threshold input.
C       Plot the contours.
C     else
C       print message
C     end if
C   else
C     print message.
C   end if
C   tidy up the images.
C
C  A C Davenhall./ROE/                                     9/9/82.
C  A C Davenhall./ROE/ {Modified}                          2/3/83.
C-
      INTEGER XEXT,YEXT,AXISIZ(2),IMPTR,WKPTR,IMSTAT
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
      CALL INPICR ('INPIC1',
     : ' Enter filename of the image to be contoured;',
     :   2,AXISIZ,IMPTR,IMSTAT)
      CALL OUTPICR ('WORK',' Work array;',
     :   2,AXISIZ,WKPTR,IMSTAT)
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
C    Inquire for values from the user.
C
          CALL READR ('SKY',
     :     ' Enter the sky brightness (mag./sq.arcsec);',
     :       2.25E1,1.50E1,3.0E1,SKY,IOSTAT)
          CALL READR ('THRESH',
     :     ' Enter faintest contour (mag./sq.arcsec);',
     :       2.40E1,1.50E1,3.00E1,THRESH,IOSTAT)
          CALL READR ('INCR',
     :     ' Enter increment between contours (mag./sq.arcsec);',
     :       1.0E0,1.0E-2,1.0E2,INCR,IOSTAT)
          CALL READC ('TITLE',
     :     ' Enter title to appear on contour map;',
     :     '  ','  ','~',TITLE,IOSTAT)
          CALL OUTPUT (
     :     ' Please wait. Contours being produced now.',IOSTAT)
C
C    Copy the image input into the work array.
C
          CALL COPY (XEXT,YEXT,%VAL(IMPTR),XEXT,YEXT,%VAL(WKPTR))
C
C
C    Convert the work array from intensity normalised to the sky
C    to pseudo-magnitudes (2.5logI).
C
          CALL PSEUMG (XEXT,YEXT,%VAL(WKPTR))
C
C    Plot the contours.
C
          CALL VRPLTS (XEXT,YEXT,%VAL(WKPTR),MAXREG,MAXPAR,REGION,
     :                 NREG,SKY,THRESH,INCR,TITLE)
        ELSE
          CALL OUTPUT (
     :      ' Unable to obtain file of regions to be contoured.',
     :        IOSTAT)
          CALL OUTPUT (
     :      ' Such a file can be generated using ABOXASP.',IOSTAT)
        END IF
      ELSE
        CALL OUTPUT (
     :   ' Unable to obtain input files correctly.',IOSTAT)
      END IF
C
C    Tidy up images.
C
      CALL CLEARIM ('INPIC1')
      CALL CLEARIM ('WORK')
      CALL CLEARIM ('BOXFIL')
      END
