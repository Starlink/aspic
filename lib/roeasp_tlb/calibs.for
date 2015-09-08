      SUBROUTINE CALIBS(NX,NY,ARRIN,TABS,TABF,NTAB,TABINT,ARROUT)
*+
*   CALIBS
*
*   part oif calibration suite
*   called from CALIB
*   applies intensity conversion to an image
*
*   Given      (arguments)
*   NX          X-dimension of image
*   NY          Y-dimension of image
*   ARRIN       input image
*   TABS        minimum value of intensity conversion
*   TABF        maximum value of intensity conversion
*   NTAB       no. of steps in intensity conversion - (length of TABINT)
*   TABINT      intensity look-up table
*
*   Returned   (arguments)
*   ARROUT      output image
*
*   J.A.Cooke/UOE/1981
*   D. Tudhope/ROE/Nov 1982
*-
 
      INTEGER NX,NY,NTAB
      REAL ARRIN(NX,NY),ARROUT(NX,NY),TABINT(NTAB),TABS,TABF
      REAL PIXVAL
      INTEGER IX,IY,TABPOS
 
      DO IY=1,NY
         DO IX=1,NX
            PIXVAL=ARRIN(IX,IY)
C*  convert observed value in range TABS..TABF into range 1..NTAB
            TABPOS=((PIXVAL-TABS)/(TABF-TABS))*(NTAB-1) +1
            IF(TABPOS.LT.1)TABPOS=1
            IF(TABPOS.GT.NTAB)TABPOS=NTAB
            ARROUT(IX,IY)=TABINT(TABPOS)
         ENDDO
      ENDDO
 
      END
