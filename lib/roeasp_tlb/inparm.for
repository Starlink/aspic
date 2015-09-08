      SUBROUTINE INPARM (IOBULG,ROBULG,IODISK,ALPHA,STATUS)
C+
C     INPARM.
C
C     Subroutine to obtain from the user the parameters
C     defining a disk galaxy profile following the r**1/4
C     law in the nucleus and the exponential law in the disk.
C
C  Given;
C   None.
C
C  Returned;
C   IOBULG  (R)  Io for r**1/4 law.
C   ROBULG  (R)  Ro  "    "     " .
C   IODISK  (R)  Io for the exponential law.
C   ALPHA   (R)  Alpha for the exponential law.
C   STATUS  (I)  Return status from interface calls,
C                =0 for successful return, otherwise non-zero.
C
C  Subroutine called;
C   Interfaces:-   READR.
C
C  A C Davenhall./ROE/                              2/8/82.
C-
      REAL IOBULG,ROBULG,IODISK,ALPHA
      INTEGER STATUS
C
      REAL MUBULG,MUDISK,SKY
C
      REAL POGSON
      PARAMETER (POGSON=2.5E0)
C
C
C    Obtain the Sky brightness (mag./sq.arcsec).
C
      STATUS=0
      CALL READR ('SKY',
     :  ' Enter the sky brightness (mag./sq.arcsec);',
     :    21.55,10.0,30.0,SKY,STATUS)
C
C    Obtain disk parameters.
C
      CALL READR ('ALPHA',
     :  ' Enter the disk scale length, alpha;',
     :    1.0,0.0,1.0E2,ALPHA,STATUS)
      CALL READR ('MUDISK',
     :  ' Enter the disk scale brightness (mag./sq.arcsec);',
     :    21.65,10.0,30.0,MUDISK,STATUS)
C
C    Obtain bulge parameters.
C
      CALL READR ('ROBULG',
     :  ' Enter the bulge scale length, Ro;',
     :    1.0,0.0,1.0E2,ROBULG,STATUS)
      CALL READR ('MUBULG',
     :  ' Enter the bulge scale brightness (mag./sq.arcsec);',
     :    21.00,10.0,30.0,MUBULG,STATUS)
C
C    Convert the scale brightnesses into intensities above
C    the sky.
C
      IODISK=(1.0E1**((SKY-MUDISK)/POGSON))-1.0E0
      IOBULG=(1.0E1**((SKY-MUBULG)/POGSON))-1.0E0
      END
