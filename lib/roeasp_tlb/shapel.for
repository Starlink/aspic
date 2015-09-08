      SUBROUTINE SHAPEL(AXMAJOR,ECCEN,THETA,ICX,ICY)
*+
*   SHAPEL
*
*   Elliptical ARGS cursor. The size, eccentricity and orientation
*   of the ellipse are controlled by the trackerball buttons.
*   Button 1 changes the ellipse parameter controlled by buttons 2 & 3
*   Buttons 2 & 3 respectively increase and decrease the ellipse
*   parameters.
*   Button 4 means take cursor position and return.
*
*   The cursor goes into ARGS plane 8, cursor instructions into
*   plane 9.
*   Uses args data base - so ICX,ICY,AXMAJOR in pixel not args coords
*
*   Given         (arguments)
*   AXMAJOR  R     major axis of ellipse in pixel coords
*   ECCEN    R     eccentricity
*   THETA    R     angle in radians above horizontal measured
*                  anticlockwise
*   ICX      I     centre of ellipse in pixel coords
*   ICY      I     centre of ellipse in pixel coords
*
*   Returned      (arguments)
*   AXMAJOR  R     major axis of ellipse in pixel coords
*   ECCEN    R     eccentricity
*   THETA    R     angle in radians above horizontal measured
*                  anticlockwise
*   ICX      I     centre of ellipse in pixel coords
*   ICY      I     centre of ellipse in pixel coords
*
*   Subroutines called :
*   CURELL,BLABELS,ARGS_PICWRT     : E2DLIB
*   ARGS_USRCUR        :
*   SRINIT,ARGS-NUMIM,ARGS_PTOA,ARGS_ATOP,IDENIM,WRUSER
*
*   B.D.Kelly/ROE/5.1.1981
*   D. Tudhope/ROE/July/1982
*-

      REAL AXMAJOR,ECCEN,THETA
      REAL VECCEN(0:7),VTHETA(0:35)
      INTEGER ICX,ICY
      INTEGER XIN,YIN,XOUT,YOUT,BUTT,FUNC,NDIR
      INTEGER*2 CURSOR(256)
      CHARACTER*12 LABEL1,LABEL2(3),LABEL3(3),LABEL4
*  local vars for axmajor,icx,icy in args not pixel coords
      REAL AXMAJ
      INTEGER IX,IY
*  local vars to help with transforming AXMAJOR
      INTEGER I,IAXM,IAXMA
      INTEGER ID,ISTATUS

      DATA LABEL1/'CHANGE FUNC'/
      DATA LABEL2/'INC SIZE','INC ELLIPT','ANTICLOCK'/
      DATA LABEL3/'DEC SIZE','DEC ELLIPT','CLOCK ROT'/
      DATA LABEL4/'MEASURE'/
      DATA VECCEN/0.0,0.4359,0.6,0.7141,0.8,0.866,0.9165,0.9539/
      DATA VTHETA/0.0000,0.0873,0.1745,0.2618,0.3491,0.4363,
     :            0.5236,0.6109,0.6981,0.7854,0.8727,0.9599,
     :            1.0472,1.1345,1.2217,1.3090,1.3963,1.4835,
     :            1.5708,1.6581,1.7453,1.8326,1.9199,2.0071,
     :            2.0944,2.1817,2.2689,2.3562,2.4435,2.5307,
     :            2.6180,2.7053,2.7925,2.8798,2.9671,3.0543/

      ISTATUS=0
*  initialise args data base
      CALL SRINIT(0,.FALSE.,ISTATUS)
*  to convert initial cursor position and AXMAJOR from pixel coords to args coords,
*   assume initial coords refer to latest image drawn on args.
*  so get id of lastest image.
      CALL ARGS_NUMIM(ID)
*  convert pixel to args coords
      CALL ARGS_PTOA(ID,ICX,ICY,IX,IY,ISTATUS)
*  do same for AXMAJOR, except have to pretend it's a point
      IAXM=NINT(AXMAJOR)
      CALL ARGS_PTOA(ID,ICX+IAXM,ICY,IAXMA,I,ISTATUS)
      AXMAJ=IAXMA-IX

      CALL ARGS_PICWRT
      FUNC=0
      XIN=IX-32
      YIN=IY-32
      BUTT=0
*
*   Determine nearest acceptable values to given eccentricity
*   and orientation.
*
      NECCEN=0
      DIFF=ABS(ECCEN-VECCEN(0))
      DO I=0,7
         IF(DIFF.GE.ABS(ECCEN-VECCEN(I))) THEN
            NECCEN=I
            DIFF=ABS(ECCEN-VECCEN(I))
         ENDIF
      ENDDO

      NTHETA=0
      DIFF=ABS(THETA-VTHETA(0))
      DO I=0,35
         IF(DIFF.GE.ABS(THETA-VTHETA(I))) THEN
            NTHETA=I
            DIFF=ABS(THETA-VTHETA(I))
         ENDIF
      ENDDO
*
*   Interaction with trackerball
*
      CALL BLABELS(9,'W',LABEL1,LABEL2(FUNC+1),LABEL3(FUNC+1),LABEL4)
      DO WHILE(BUTT.NE.4)
         IF(BUTT.EQ.1) THEN
            FUNC=MOD(FUNC+1,3)
            CALL BLABELS(9,'W',LABEL1,LABEL2(FUNC+1),
     :                   LABEL3(FUNC+1),LABEL4)
         ELSE IF((BUTT.EQ.2).OR.(BUTT.EQ.3)) THEN
            IF(BUTT.EQ.2) THEN
               NDIR=1
            ELSE
               NDIR=-1
            ENDIF
            IF(FUNC.EQ.0) THEN
               AXMAJ=AXMAJ+1.0*NDIR
               IF(AXMAJ.GT.64.0) AXMAJ=64.0
            ELSE IF(FUNC.EQ.1) THEN
               NECCEN=MOD(NECCEN+NDIR,8)
               IF(NECCEN.LT.0) NECCEN=NECCEN+8
               ECCEN=VECCEN(NECCEN)
            ELSE IF(FUNC.EQ.2) THEN
               NTHETA=MOD(NTHETA+NDIR,36)
               IF(NTHETA.LT.0) NTHETA=NTHETA+36
               THETA=VTHETA(NTHETA)
            ENDIF
         ENDIF
         CALL CURELL(AXMAJ,ECCEN,THETA,4,64,CURSOR)
         CALL ARGS_USRCUR(8,'W',CURSOR,XIN,YIN,XOUT,YOUT,BUTT)
         XIN=XOUT
         YIN=YOUT
      ENDDO

      IX=XOUT+32
      IY=YOUT+32

*
*  now have returned args (ix,iy) in args coords. to convert to pixel coords,
*   have to find which image they lie on (may not be latest one plotted).
      CALL IDENIM(IX,IY,ID)
*  convert to pixel coords
      CALL ARGS_ATOP(ID,IX,IY,ICX,ICY,ISTATUS)
*  do same for major axis
      IAXMA=NINT(AXMAJ)
      CALL ARGS_ATOP(ID,IX+IAXMA,IY,IAXM,I,ISTATUS)
      AXMAJOR=IAXM-ICX
      IF (ISTATUS.NE.0) THEN
        CALL WRUSER('ERROR IN ACCESSING ARGS DATA BASE',ISTATUS)
      ENDIF

*   Clear ARGS plane containing instruction boxes
*
      CALL ARGS_OVCLR(9)
*
*   Re-enable ARGS picture planes
*
      CALL ARGS_PICWRT

      END
