	SUBROUTINE CRB_PICK
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
      INTEGER AX(2),AXO(2),STATUS
      INTEGER LIMX(2),LIMY(2)
      INTEGER XY1(2),XY2(2)
C
C   First get the input frame.
C
      CALL RDIMAG('IMAGE',FMT_R,2,AX,I,IPIN,STATUS)
      IF (STATUS.EQ.ERR_NORMAL.AND.I.EQ.2) THEN
C
C      If the image is OK then get the limits
C
         CALL GET2XY(LIMX,LIMY)
C      and write them to the environment.
C
         XY1(1)=LIMX(1)
         XY1(2)=LIMY(1)
         XY2(1)=LIMX(2)
         XY2(2)=LIMY(2)
         CALL WRKEYI('XYOUT1',XY1,2,STATUS)
         CALL WRKEYI('XYOUT2',XY2,2,STATUS)
C
C      Now work out size of output frame.
C
         AXO(1)=LIMX(2)-LIMX(1)+1
         AXO(2)=LIMY(2)-LIMY(1)+1
C
C      Then get the output frame.
C
         CALL WRIMAG('OUTPUT',FMT_R,AXO,2,IPOUT,STATUS)
         IF (STATUS.EQ.ERR_NORMAL) THEN
C
C         If the response was OK then make the copy.
C
            CALL COPYIT(%VAL(IPIN),AX(1),AX(2),LIMX,LIMY,
     :                  AXO(1),AXO(2),%VAL(IPOUT))
C
         END IF
 
      END IF
 
C
C   Now tidy up and exit.
C
      CALL FRDATA(' ',STATUS)
	CALL CNPAR('XYPOS1',JSTAT)
	CALL CNPAR('XYPOS2',JSTAT)
	CALL CNPAR('XYOUT1',JSTAT)
	CALL CNPAR('XYOUT2',JSTAT)
	CALL CNPAR('INPUT',JSTAT)
	CALL CNPAR('OUTPUT',JSTAT)
      END
