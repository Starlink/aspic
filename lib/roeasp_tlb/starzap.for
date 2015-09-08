      SUBROUTINE STARZAP (PLANE1,COL1,PLANE2,COL2,IXEXT,IYEXT,A)
C+
C      STARZAP.
C
C      Subroutine to use the ARGS cursor to define a star image
C      & to remove the image so isolated by fitting a polynomial
C      to the background beyond its periphery.
C
C    Given;
C    PLANE1 (I) Overlay plane for cursor.
C    COL1   (C) Selected colour for labels and cursor prior 
C               to accepting point.
C    PLANE2 (I) Overlay plane for accepted aperture.
C    COL2   (C) Colour of cursor after accepting point.
C    IXEXT  (I) X size of image.
C    IYEXT  (I) Y  "   "    "  .
C    A      (R) Array holding image.
C
C    Returned;
C    A     (R) Array holding image may have been modified by removal of star.
C
C    Subroutines called;
C    I/O - OUTPUT, YESNO.
C    Actually do the work - STAZAP, CRCLCR.
C
C     Structure;
C     Put out instructions to terminal.
C     Put up button labels on the Args.
C     Put up cursor on the Args.
C     If (star selected Ok)
C       Call STAZAP to remove the image.
C     end if.
C
C     A C Davenhall./ROE/                                 21/1/82.
C     A C Davenhall./ROE/   {Modified}                    28/9/82.
C-
      INTEGER PLANE1,PLANE2,IXEXT,IYEXT
      REAL A(IXEXT,IYEXT)
      CHARACTER COL1*1,COL2*2
C
      INTEGER IX1,IY1,IRAD,STAT2
      INTEGER CURSTT,ZAPSTT
      CHARACTER REPLY*1
C
      CALL OUTPUT (' Position cursor over star image.',STAT2)
      CALL OUTPUT (' Adjust size until image is COMPLETELY enclosed.',
     :        STAT2)
      CURSTT=0
      CALL CRCLCR (PLANE1,COL1,PLANE2,COL2,IX,IY,IRAD,CURSTT)
      IF (CURSTT.EQ.0) THEN
        CALL YESNO (' Proceed to remove object?','Y',REPLY,STAT2)
        IF (REPLY.EQ.'Y') THEN
          ZAPSTT=0
          CALL STAZAP (IX,IY,IRAD,IXEXT,IYEXT,A,ZAPSTT)
          IF (ZAPSTT.NE.0) CALL OUTPUT (
     :  '*WARNING* Too many pts. in background. Not all used.',STAT2)
        END IF
      END IF
      END
