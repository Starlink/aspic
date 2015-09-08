      SUBROUTINE CURFIT2(IXEXT,IYEXT,RIMAGE,IX,IY,
     :                   MODE,NX,NY,P,DS,ARRAY,XMARGE,YMARGE,
     :                   SUMRS,SUM,NSUM)
*+
*   CURFIT2
*
*   fits a gaussian at a specified point.
*
*    Arguments :
*
*    Given     Type     Usage
*    IXEXT      I       image X-dimension
*    IYEXT      I       image Y-dimension
*    RIMAGE     RA      array for current image
*    IX         I       X-coordinate of point
*    IY         I       Y-coordinate of point
*    MODE       I       number of parameters being fitted
*    NX         I       X-dimension of the fitting area
*    NY         I       Y-dimension of the fitting area
*    P          R       saturation exponent starting/fixed value
*    DS         R       saturation density starting/fixed value
*    ARRAY      RA      array corresponding to the fitting area
*    XMARGE     RA      X-marginal workspace array
*    YMARGE     RA      Y-marginal workspace array
*    SUMRS      RA      workspace array
*    SUM        RA      workspace array
*    NSUM       RA      workspace array
*
*    Subroutine called :
*    GAUDRIVE          : E2DASP
*
*	B.D KELLY/ROE/1981
*-
 
      INTEGER MODE,NX,NY
      REAL P,DS
      INTEGER IXEXT,IYEXT
      REAL ARRAY(NX,NY),COEFF(6)
      REAL RIMAGE(IXEXT,IYEXT)
      REAL XMARGE(NX),YMARGE(NY),SUMRS(NX),SUM(NX)
      INTEGER NSUM(NX)
      CHARACTER*80 OBUF
*
*   Open text file for listing output
*
      OPEN(UNIT=30,NAME='OBJECTS.LIS',TYPE='UNKNOWN',
     :     ACCESS='APPEND',FORM='FORMATTED')
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
      NXBOT=IX-NX/2
      NYBOT=IY-NY/2
      IF((NXBOT.GE.1).AND.(NYBOT.GE.1).AND.
     :     ((NXBOT+NX-1).LE.IXEXT).AND.((NYBOT+NY-1).LE.IYEXT)) THEN
         DO J=1,NY
            DO I=1,NX
               ARRAY(I,J)=RIMAGE(I-1+NXBOT,J-1+NYBOT)
            ENDDO
         ENDDO
         CALL GAUDRIVE(ARRAY,NX,NY,MODE,COEFF,P,DS,
     :                 XO,YO,SX,SY,ITX,ITY,DENMAG,B,SIG,N,M,
     :                 XMARGE,YMARGE,SUMRS,SUM,NSUM)
         XPOS=XO+NXBOT-1.5
         YPOS=YO+NYBOT-1.5
*
*   Write the results into a character string, then output the
*   string both to the terminal and to a disk file.
*
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
     :         '' and photometric parameters = '',3I5)') ITX,ITY,M
         CALL COPBUF(OBUF,30)
         CALL COPBUF(' ',30)
*
*     Check for convergence failure, and give warning if necessary.
*
         IF((ITX.EQ.0).OR.(ITX.EQ.9).OR.(ITY.EQ.0).OR.(ITY.EQ.9)
     :      .OR.(M.EQ.0)) THEN
            CALL COPBUF(' ',30)
            CALL COPBUF(' Warning, failure in fit',30)
            CALL COPBUF(' ***********************',30)
            CALL COPBUF(' ',30)
         ENDIF
      ENDIF
 
      CLOSE(UNIT=30)
 
      END
