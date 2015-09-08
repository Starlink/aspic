      SUBROUTINE CELL (XEXT,YEXT,ICELL)
C+
C     CELL.
C
C     Subroutine to interogate the user in order to obtain 
C     a value for the averaging cell size to be used in
C     extracting background values from an image array.
C
C  Given;
C   XEXT  (I)  X extent of image from which pts. are to be extracted.
C   YEXT  (I)  Y   "    "    "    "     "    " .  "  "  "     "     .
C   ICELL (I)  Input value for averaging cell. adopted as default.
C
C  Returned;
C   ICELL (I)  Chosen value for cell size.
C
C  A C Davenhall./ROE/                                17/8/82.
C-
      INTEGER XEXT,YEXT,ICELL
C
      INTEGER DCELL,MINL,IOSTAT
      LOGICAL MORE
      CHARACTER REPLY*1,BUFFER*60
C
C
C    Setup the default cell size.
C
      DCELL=ICELL
      MINL=MIN(XEXT,YEXT)
C
      MORE=.TRUE.
      DO WHILE (MORE)
C
C    Obtain new value for the cellsize.
C
        IOSTAT=0
        CALL READI ('ICELL',' Enter required cell side size;',
     :               DCELL,1,MINL,ICELL,IOSTAT)
C
C    Check that the value given for ICELL is an integer
C    submultiple of the X and Y extents of the image
C    to be fitted.
C
        IF (MOD(XEXT,ICELL).NE.0.OR.MOD(YEXT,ICELL).NE.0) THEN
          WRITE(BUFFER,2000)
 2000     FORMAT(1X,'*WARNING*:')
          CALL OUTPUT (BUFFER,IOSTAT)
          WRITE(BUFFER,2001) ICELL
 2001     FORMAT(1X,'Each size of averaging cell is ',I2,' pixels.')
          CALL OUTPUT (BUFFER,IOSTAT)
          WRITE(BUFFER,2002) XEXT,YEXT
 2002     FORMAT(1X,'X extent = ',I4,' Y extent = ',I4)
          CALL OUTPUT (BUFFER,IOSTAT)
          CALL YESNO (' Is this acceptable?','N',REPLY,IOSTAT)
          IF (REPLY.EQ.'N') MORE=.FALSE.
        ELSE
          MORE=.FALSE.
        END IF
      END DO
      END
