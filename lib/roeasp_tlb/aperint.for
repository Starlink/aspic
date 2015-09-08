        SUBROUTINE APERINT (XCEN,YCEN,SCALDIA,MAGNIT,MAXAP,NAP,PIXSIZ,
     :                A,IXEXT,IYEXT,LUMIN,SKYBRI)
C+
C         Subroutine to integrate a series of circular apertures in
C         order to find the luminosity in each, & by knowing the
C         measured magnitude in each compute the corresponding
C         sky brightness. A variety of algorithms are used for the
C         integration, increasing sophistication corresponding to 
C         decreasing radius.
C
C         Given;
C         XCEN     R X coord. of centre of image.
C         YCEN     R Y   "    "    "    "    "  .
C         SCALDIA  R Array holding scaled radii of apertures.
C         MAGNIT   R Array holding magnitudes inside apertures.
C         MAXAP    I Size of arrays SCALDIA & MAGNIT etc.
C         NAP      I No. of apertures.
C         PIXSIZ   R Length of a pixel side (arcsec).
C         A        R Array holding image.
C         IXEXT    I X size of image.
C         IYEXT    I Y  "   "    "  .
C
C         Returned;
C         LUMIN    R Array holding luminosity inside each aperture.
C         SKYBRI   R Array holding sky brightness computed for each
C                    aperture.
C
C         Subroutines called;
C         CIRINT1,CIRINT2,OUTPUT,ADDEL3.
C
C         Structure;
C
C         Do for (all apertures).
C           If radius .lt. 15
C             integrate using Dave's routine to give the correct soln.
C           else if radius .ge. 15 & .lt. 30
C             Integrate using the routine of Jones et.al (1967).
C           else if radius .ge. 30
C             integrate simply by adding pixels with radii smaller than
C             the aperture
C           end if
C         end do
C
C         A C Davenhall./ROE/                                18/12/81.
C-
        REAL SCALDIA(MAXAP),MAGNIT(MAXAP),LUMIN(MAXAP),SKYBRI(MAXAP)
        REAL A(IXEXT,IYEXT)
        REAL XCEN,YCEN
        INTEGER MAXAP,NAP,IXEXT,IYEXT
        REAL RADIUS,APERLUM,POGSON,PI
        INTEGER ISTAT
        CHARACTER BUFF*80
        DATA POGSON/2.5E0/, PI/3.1415927E0/
        DO I=1,NAP
          RADIUS=SCALDIA(I)/2.0E0
C
C         For scaled radii .le. 15 pixels use Dave's correct integration
C         routine.
C
          IF (RADIUS.LE.1.5E1) THEN
            CALL ADDEL3 (XCEN,YCEN,RADIUS,RADIUS,0.0E0,A,IXEXT,IYEXT,
     :                   APERLUM)
          END IF
C
C         If the scaled radius is between 15 & 30 pixels use the
C         Jones et.al. (1967) method.
C
          IF (RADIUS.GT.1.5E1.AND.RADIUS.LE.3.0E1) THEN
            CALL CIRINT2 (XCEN,YCEN,RADIUS,A,IXEXT,IYEXT,APERLUM)
          END IF
C
C         For a scaled radius of more than 30 pixels simply add up
C         the results.
C
          IF (RADIUS.GT.3.0E1) THEN
            CALL CIRINT1 (XCEN,YCEN,RADIUS,A,IXEXT,IYEXT,APERLUM)
          END IF
          LUMIN(I)=(APERLUM-(PI*RADIUS*RADIUS))*PIXSIZ*PIXSIZ
          SKYBRI(I)=MAGNIT(I)+(POGSON*ALOG10(LUMIN(I)))
          WRITE (BUFF,2000) I
 2000        FORMAT(1X,'Aperture no. ',I2,' processed.')
          CALL OUTPUT (BUFF,ISTAT)
        END DO
        END
