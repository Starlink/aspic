      SUBROUTINE CURFITXY(IXEXT,IYEXT,RIMAGE,IX,IY,
     :                    NX,NY,ARRAY,XMARGE,YMARGE)
*+
*     CURFITXY
*
*     Fits a gaussian at a specified point.
*
*     Given      (arguments)
*     IXEXT  I    Image X-dimension
*     IYEXT  I    Image Y-dimension
*     RIMAGE RA   Array for current image
*     IX     I    X-coordinate of point
*     IY     I    Y-coordinate of point
*     NX     I    Fitting area X-dimension
*     NY     I    Fitting area Y-dimension
*     ARRAY  RA   Workspace array for fitting area
*     XMARGE RA   Workspace array for X-marginal
*     YMARGE RA   Workspace array for Y-marginal
*
*     CALLS: GAUXY,COPBUF
*
*     B.D.Kelly/ROE/1981
*     D.W.T.Baines/ROE/JAN 1983
*-
      INTEGER NX,NY,IXEXT,IYEXT
      INTEGER ITX,ITY,ISTAT,IX,IY,NXBOT,NYBOT
      REAL ARRAY(NX,NY) , RIMAGE(IXEXT,IYEXT)
      REAL XMARGE(NX) , YMARGE(NY)
      REAL XPOS,YPOS,SX,SY,XO,YO
      CHARACTER*80 OBUFF
*
*     copy the areas around the given coordinates into a (NX,NY)
*     work array, and pass this to the gaussian fitting routines.
*
*     NXBOT,NYBOT is the bottom left corner of the fitting area
*
      NXBOT=IX-NX/2
      NYBOT=IY-NY/2
*
*     check that the fitting area is within the image
*
      IF((NXBOT.GE.1).AND.(NYBOT.GE.1).AND.
     :     ((NXBOT+NX-1).LE.IXEXT).AND.((NYBOT+NY-1).LE.IYEXT)) THEN
*
*        transfer fitting area to the workspace array
*
         DO J=1,NY
            DO I=1,NX
               ARRAY(I,J)=RIMAGE(I-1+NXBOT,J-1+NYBOT)
            ENDDO
         ENDDO
*
*        perform the gaussian fitting
*
         CALL GAUXY(ARRAY,NX,NY,XO,YO,SX,SY,ITX,ITY,
     :              XMARGE,YMARGE)
         XPOS=XO + REAL(NXBOT) - 1.5
         YPOS=YO + REAL(NYBOT) - 1.5
*
*        write the results into a character string, then output the
*        string both to the terminal and to a disk file.
*
         CALL COPBUF(' ',32)
         WRITE(OBUFF,'('' XPOS ='',F10.3,''  + OR -'',F10.3)') XPOS,SX
         CALL COPBUF(OBUFF,32)
         WRITE(OBUFF,'('' YPOS ='',F10.3,''  + OR -'',F10.3)') YPOS,SY
         CALL COPBUF(OBUFF,32)
         WRITE(OBUFF,'('' ITERATIONS FOR X AND Y ='',2I5)')
     :         ITX,ITY
         CALL COPBUF(OBUFF,32)
*
*        check for convergence failure, and give warning if necessary.
*
         IF((ITX.EQ.0).OR.(ITX.EQ.9).OR.(ITY.EQ.0).OR.(ITY.EQ.9))THEN
            CALL COPBUF(' ',32)
            CALL COPBUF(' WARNING, FAILURE IN FIT',32)
            CALL COPBUF(' ***********************',32)
            CALL COPBUF(' ',32)
         ENDIF
      ELSE
*
*        here when the fitting area is not completely within the iamge
*
         CALL WRUSER('FITTING AREA IS NOT COMPLETELY WITHIN THE IMAGE',
     :                ISTAT)
         CALL WRUSER('REDUCE SIZE OF FITTING AREA OR REPOSITION CURSOR',
     :                ISTAT)
      ENDIF
      END
