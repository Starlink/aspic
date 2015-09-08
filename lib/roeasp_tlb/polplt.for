      SUBROUTINE POLPLT(VECSCL,NXT,NYT,TOTAL,NXA,NYA,ANGLE,
     :                  NPLTX,NPLTY,PLTOT,PLANG)
*+
*     POLPLT
*
*     Plots polarization vectors
*
*     Given      (arguments)
*     VECSCL  R   scale factor for vector length
*     NXT     I   X-dimension of total polarization array
*     NYT     I   Y-dimension of total polarization array
*     TOTAL   RA  total polarization array
*     NXA     I   X-dimension of angle array
*     NYA     I   Y-dimension of angle array
*     ANGLE   RA  angle array
*     NPLTX   I   X-dimension of plotting arrays
*     NPLTY   I   Y-dimension of plotting arrays
*     PLTOT   RA  polarization plotting array
*     PLANG   RA  angle plotting array
*  
*     Subroutines called :
*     MOVTO2,LINTO2      : ROEFINGS
*
*     B.D.Kelly/ROE/8.2.1982
*     D.W.T.Baines/ROE/Feb 1983
*
*-

      INTEGER NXT,NYT,NXA,NYA,NPLTX,NPLTY
      REAL TOTAL(NXT,NYT),ANGLE(NXA,NYA)
      REAL PLTOT(NPLTX,NPLTY),PLANG(NPLTX,NPLTY)
      REAL SORTOT(25),SORTANG(25)
      REAL VECSCL
      PARAMETER (PI=3.14159)
*
*     only every NPIX'th pixel will be plotted
*
      IF(MAX(NXT,NYT).GT.256) THEN
         NPIX=8
      ELSE IF(MAX(NXT,NYT).GT.128) THEN
         NPIX=4
      ELSE IF(MAX(NXT,NYT).GT.64) THEN
         NPIX=2
      ELSE
         NPIX=1
      ENDIF
*
*     polarization vectors are to be plotted at NPIX pixel intervals
*     so trasfer every NPIX'th PIXEL into the plotting arrays
*
      DO J = NPIX , NYT , NPIX
         JM = J / NPIX
         DO I = NPIX , NXT , NPIX
            IM = I / NPIX
            PLTOT ( IM , JM ) = TOTAL ( I , J )
            PLANG ( IM , JM ) = ANGLE ( I , J )
         ENDDO
      ENDDO
*
*   Plot vectors
*

      TOTMAX=50.0
      DO J=1,NYT/NPIX
         DO I=1,NXT/NPIX
            YC=(REAL(J*NPIX-NPIX/2)+0.5)
            XC=(REAL(I*NPIX-NPIX/2)+0.5)
            HALF=VECSCL*PLTOT(I,J)/TOTMAX
            XSTART=XC+HALF*COS(PLANG(I,J)-0.5*PI)
            XFIN  =XC-HALF*COS(PLANG(I,J)-0.5*PI)
            YSTART=YC+HALF*SIN(PLANG(I,J)-0.5*PI)
            YFIN  =YC-HALF*SIN(PLANG(I,J)-0.5*PI)

*
*           if the polarization is greater than 0 , plot the vector
*
            IF ( PLTOT ( I,J ) .GT. 0. ) THEN
               CALL MOVTO2(XSTART,YSTART)
               CALL LINTO2(XFIN,YFIN)
            ENDIF
         ENDDO
      ENDDO
      END
