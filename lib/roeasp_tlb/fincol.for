      SUBROUTINE FINCOL (COL_L,COL_B)
C+
C      FINCOL.
C
C      Subroutine to generate colours for the lines and
C     background when Fings graphics are used on the ARGS.
C
C  Given;
C   None.
C
C  Returned;
C   COL_L  (IA)  Colour for lines.
C   COL_B  (IA)  Colour for background.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                14/3/82.
C  A C Davenhall./ROE/  {Modified}                    18/3/83.
C-
      INTEGER COL_L(3),COL_B(3)
C
C      Black lines (now irrelevant).
C
      COL_L(1)=255
      COL_L(2)=255
      COL_L(3)=0
C
C      Yellow/white background.
C
      COL_B(1)=255
      COL_B(2)=255
      COL_B(3)=127
      END
