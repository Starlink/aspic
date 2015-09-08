      SUBROUTINE MAG (SUMSTAR,SUMSKY,ZEROMAG,RMAG,LSTAT)
*+
*     MAG
*
*     Calculates magnitudes for aperture photometer.
*
*  Given            (arguments)
*   SUMSTAR  (R) Total signal within aperture when centred on star.
*   SUMSKY   (R) Total signal within aperture when centred on sky.
*   ZEROMAG  (R) Magnitude zero-point.
*
*  Returned         (arguments)
*   RMAG     (R) Magnitude.
*   LSTAT    (I) Status. = 0 for success, = 1 if unable to
*                calculate the magnitude because the star signal
*                was smaller than the sky signal.
*
*  B.D.Kelly/ROE/1981.
*  A C Davenhall./ROE/13.3.84.  {Modified}
*-
      IMPLICIT NONE

      REAL SUMSTAR,SUMSKY,ZEROMAG,RMAG
      INTEGER LSTAT

      REAL RINT

 
      RINT=SUMSTAR-SUMSKY
      LSTAT=0

      IF (RINT.GT.1.0E-3) THEN
        RMAG=ZEROMAG-(2.5*ALOG10(RINT))
      ELSE
        RMAG=3.0E4
        LSTAT=1
      ENDIF
 
      END
