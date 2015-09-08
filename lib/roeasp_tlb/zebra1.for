      SUBROUTINE ZEBRA1 (NUMCON,NCOL,NLEVEL,IDCOL)
*+
*     ZEBRA1.
*
*     Create a black and white pseudo contour colour table.
*
*  Given;
*   NUMCON (I)  No. of contours required.
*   NCOL   (I)  No. of colour guns.
*   NLEVEL (I)  No. of intensity levels for each gun.
*
*  Returned;
*   IDCOL  (IA) Colour table evaluated at each intensity level for 
*               gun.
*
*  Subroutines called;
*   None.
*
*  B.D KELLY/ROE/1981
*  A C Davenhall./ROE/    {Modified}                  26/10/82.
*-
      INTEGER NUMCON,NCOL,NLEVEL
      INTEGER IDCOL(NCOL,NLEVEL)
*
      INTEGER MAXINT,INCR,INDEX
*
*    Width of the pseudo contour.
* 
      INTEGER WIDTH
      PARAMETER (WIDTH=4)
*
*
      DO J=1,NLEVEL
         DO I=1,NCOL
            IDCOL(I,J)=0
         END DO
      END DO
*
      MAXINT=NLEVEL-16
      INCR=MAXINT/NUMCON
*
      DO J=1,MAXINT,INCR
         DO I=1,NCOL
            DO K=1,WIDTH
              INDEX=J+INCR-WIDTH-1+K
              IDCOL(I,INDEX)=255
            END DO
         END DO
      END DO
*
      END
