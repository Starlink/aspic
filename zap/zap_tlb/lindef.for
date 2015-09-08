      SUBROUTINE LINDEF(IDLOC,X1,X2,Y1,Y2,STATUS)
C
C   USE CURSOR TO SELECT TWO POINTS
C
      INTEGER STATUS,PX,PY
      REAL UX(2),UY(2)
C
C   INITIALISE CURSOR AND ENABLE OVERLAYS
C
      CALL ARGS_CUROP ('1234','W')
      CALL ARGS_OVOP (8,'G')
	CALL BLABELS(9,'Y','END-POINT','LOOK AT LINE',
     1'LOOK AT LINE','EXIT')
C
C   IPT IS USED TO COUNT THE POINTS SELECTED
C
      IPT=0
      IDLOC = 0
  998 CONTINUE
      DO WHILE (IPT.LT.2)
          CALL ARGS_RDCUR('IMAG',ID,IB,X,Y)
C
C       IF IMAGE WAS LOCATED THEN STORE POSITION
C
          IF (IB.EQ.0) THEN
              STATUS = 1
              GOTO 999
          ELSE IF (IB.EQ.4) THEN
              STATUS = 2
              GOTO 999
          ELSE IF (ID.GT.0) THEN
              IF (IDLOC.EQ.0) THEN
                  IDLOC = ID
              ENDIF
              IF (ID.EQ.IDLOC) THEN
                  IF (IPT.EQ.0) THEN
                      CALL CROSS (ID,X,Y)
                      CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
                      XU = X
                      YU = Y
                      X1 = PX
                      Y1 = PY
                  ELSE IF (IB.GT.1) THEN
                          UX(1) = XU
                          UX(2) = X
                          UY(1) = YU
                          UY(2) = Y
                          CALL ARGS_CLS(8)
                          CALL ARGS_OVCL(8,.FALSE.)
                          CALL ARGS_OVOP(8,'G')
                          CALL CROSS(ID,XU,YU)
                          CALL ARGS_POLYL(ID,2,UX,UY,STATUS)
                          GO TO 998
                       ELSE
                          CALL ARGS_UTOP (ID,X,Y,PX,PY,STATUS)
                          X2 = PX
                          Y2 = PY
                  ENDIF
                  IPT = IPT + 1
                  CALL ARGS_CURC ('G')
              ENDIF
          ENDIF
      ENDDO
	CALL ARGS_OVCLR(9)
      STATUS = 0
C
C   REMOVE CURSOR
C
999   CALL ARGS_CURCL
      END
C
