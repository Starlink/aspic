	SUBROUTINE CRB_TBXY
 
      INTEGER IDMAX,STATUS,ID,IB,AX,AY
      REAL UX,UY
 
*   Check an object is displayed
      CALL ARGS_NUMIM (IDMAX)
      IF (IDMAX.EQ.0) THEN
          CALL WRERR ('NOIMS')
      ELSE
 
*       Assign and prepare ARGS
          CALL SRINIT (0,.FALSE.,STATUS)
          IF (STATUS.NE.0) THEN
              CALL WRERR('NOARGS')
          ELSE
 
*           Read cursor until valid point is chosen
              CALL ARGS_CUROP ('1234','W')
	CALL BLABELS(9,'Y','EXIT','EXIT','EXIT','EXIT')
              ID = 0
              DO WHILE (ID.EQ.0)
                  CALL ARGS_RDCUR (' ',ID,IB,UX,UY)
              ENDDO
	          WRITE(*,101) UX,UY
101   FORMAT(/' Cursor position: ',2F6.1)
              CALL ARGS_CURCL
 
*           Send 'id' and '(ux,uy)' to the environment
*           (next 5 lines are temporary)
              CALL ARGS_DOPDB ('ARGS_DEVICE',STATUS)
              CALL ARGS_UTOA (ID,UX,UY,AX,AY,STATUS)
              CALL ARGS_CLDB (STATUS)
              CALL WRKEYI ('AX',AX,1,STATUS)
              CALL WRKEYI ('AY',AY,1,STATUS)
              CALL WRKEYI ('ID',ID,1,STATUS)
              CALL WRKEYR ('X',UX,1,STATUS)
              CALL WRKEYR ('Y',UY,1,STATUS)
	CALL ARGS_OVCLR(9)
 
          ENDIF
 
      ENDIF
 
      END
 
C   WRITTEN BY C D PIKE (AND OTHERS) AT RGO ON 23/2/81
C   THIS VERSION BY WFL 1/7/81 TO DEMONSTRATE ARGS DATABASE
C   EXTENSIVELY PRUNED BY WFL TO DEMONSTRATE ARGS OVERLAYS SEP 1981
