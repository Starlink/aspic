      SUBROUTINE GAUFIT2(IXEXT,IYEXT,RIMAGE,NCAT,NCORD,COORDS,
     :                   MODE,NX,NY,P1,DS1,ARRAY,XMARGE,YMARGE,
     :                   SUMRS,SUM,NSUM)
*+
*    GAUFIT2
*
*    takes the list of (X,Y) positions in COORDS(NCAT,NCORD)
*    as a finding list of stellar images. Each image is fed
*    in turn to GAUDRIVE, which determines astrometric position
*    and photometric parameters.
*
*    Arguments :
*
*    Given     Type     Usage
*    IXEXT      I       image X-dimension
*    IYEXT      I       image Y-dimension
*    RIMAGE     RA      array for current image
*    NCAT       I       No. of catalogue entries for each object
*    NCORD      I       No. of defined coordinates
*    COORDS     IA      Catalogue. X=COORDS(1,J) ; Y=COORDS(2,J)
*    MODE       I       number of parameters being fitted
*    NX         I       X-dimension of fitting area
*    NY         I       Y-dimension of fitting area
*    P1         R       saturation exponent starting/fixed value
*    DS1        R       saturation density starting/fixed value
*    ARRAY      RA      array corresponding to the fitting area
*    XMARGE     RA      X-marginal workspace array
*    YMARGE     RA      Y-marginal workspace array
*    SUMRS      RA      workspace array
*    SUM        RA      workspace array
*    NSUM       IA      workspace array
*
*   Subroutines called :
*   GAUDRIVE           : E2DLIB
*
*   B.D.Kelly/ROE/11.5.1982
*-
 
      INTEGER MODE,NX,NY
      REAL P,DS,P1,DS1
      INTEGER IXEXT,IYEXT,NCAT,NCORD
      REAL ARRAY(NX,NY),COEFF(6)
      REAL RIMAGE(IXEXT,IYEXT),COORDS(NCAT,NCORD)
      REAL XMARGE(NX),YMARGE(NY),SUMRS(NX),SUM(NX)
      INTEGER NSUM(NX)
      CHARACTER*72 OBUF
 
      P=P1
      DS=DS1
*
*   Open text file for listing output
*
      OPEN(UNIT=30,NAME='OBJECTS.LIS',TYPE='NEW',
     :     FORM='FORMATTED')
      IF(MODE.EQ.3) THEN
         WRITE(OBUF,'('' Fixed values'')')
         CALL COPBUF(OBUF,30)
         WRITE(OBUF,'('' Saturation Exponent = '',F10.3)') P
         CALL COPBUF(OBUF,30)
         WRITE(OBUF,'('' Saturation Density  = '',E20.6)') DS
         CALL COPBUF(OBUF,30)
      ELSE IF(MODE.EQ.4) THEN
         WRITE(OBUF,'('' Fixed value'')')
         CALL COPBUF(OBUF,30)
         WRITE(OBUF,'('' Saturation Exponent = '',F10.3)') P
         CALL COPBUF(OBUF,30)
      ELSE
         WRITE(OBUF,'('' Starting values'')')
         CALL COPBUF(OBUF,30)
         WRITE(OBUF,'('' Saturation Exponent = '',F10.3)') P
         CALL COPBUF(OBUF,30)
         WRITE(OBUF,'('' Saturation Density  = '',E20.6)') DS
         CALL COPBUF(OBUF,30)
      ENDIF
      CALL COPBUF(' ',30)
*
*   Copy the areas around the given coordinates into a (NX,NY)
*   work array, and pass this to the gaussian fitting routines.
*
      DO JOBJ=1,NCORD
         NXBOT=INT(COORDS(1,JOBJ))-NX/2
         NYBOT=INT(COORDS(2,JOBJ))-NY/2
         IF((NXBOT.GE.1).AND.(NYBOT.GE.1).AND.
     :     ((NXBOT+NX-1).LE.IXEXT).AND.((NYBOT+NY-1).LE.IYEXT)) THEN
            DO J=1,NY
               DO I=1,NX
                  ARRAY(I,J)=RIMAGE(I-1+NXBOT,J-1+NYBOT)
               ENDDO
            ENDDO
            P=P1
            DS=DS1
            CALL GAUDRIVE(ARRAY,NX,NY,MODE,COEFF,P,DS,
     :                    XO,YO,SX,SY,ITX,ITY,DENMAG,B,SIG,N,M,
     :                    XMARGE,YMARGE,SUMRS,SUM,NSUM)
            XPOS=XO+NXBOT-1.5
            YPOS=YO+NYBOT-1.5
            WRITE(OBUF,'('' Object = '',I5)') JOBJ
            CALL COPBUF(OBUF,30)
            IF(MODE .EQ. 5) THEN
               WRITE(OBUF,'('' Saturation Exponent = '',F10.3)') P
               CALL COPBUF(OBUF,30)
            ENDIF
            IF(MODE .GE. 4) THEN
               WRITE(OBUF,'('' Saturation density =  '',E20.6)') DS
               CALL COPBUF(OBUF,30)
            ENDIF
            WRITE(OBUF,'('' Xpos = '',F10.3,'' +/- '',F10.3)') XPOS,SX
            CALL COPBUF(OBUF,30)
            WRITE(OBUF,'('' Ypos = '',F10.3,'' +/- '',F10.3)') YPOS,SY
            CALL COPBUF(OBUF,30)
            WRITE(OBUF,'('' Magnitude Index     = '',F10.3)') DENMAG
            CALL COPBUF(OBUF,30)
            WRITE(OBUF,'('' Background Density  = '',F10.3)') B
            CALL COPBUF(OBUF,30)
            WRITE(OBUF,'('' 1-sigma image width = '',F10.3)') SIG
            CALL COPBUF(OBUF,30)
            WRITE(OBUF,'('' No. of Iterations for X,Y'',
     :           '' and photometric parameters = '',3I5)') ITX,ITY,M
            CALL COPBUF(OBUF,30)
            CALL COPBUF(' ',30)
         ENDIF
      ENDDO
 
      CLOSE(UNIT=30)
 
      END
