      SUBROUTINE CURCOL (CURCO1,CURCO2,STAT)
C+
C     CURCOL.
C
C     Subroutine to select a colour for the cursor, to be
C     used in STARZAP.
C
C  Given;
C   None.
C
C  Returned;
C   CURCO1  (C)  Selected colour for labels & cursor prior to
C                accepting point.
C   CURCO2  (C)  Selected colour for cursor after accepting point.
C   STAT    (I)  Return status.
C                = 0 - Ok return.
C                > 0 - failed to obtain values for CURCO1 & CURCO2.
C
C  Subroutine called;
C   MULREP.
C
C  A C Davenhall./ROE/                                 26/5/82.
C-
      CHARACTER CURCO1*1,CURCO2*2
      INTEGER STAT
C
      CHARACTER REPLY*4
C
C
      STAT=0
C
C      Obtain colour for labels & cursor prior to accepting point.
C
      CALL MULREP (' Select colour for cursor;',
     :             'W,R,G,BLU,Y,C,M,BLAK$',REPLY,STAT)
      IF (REPLY.NE.'BLAK') THEN
        CURCO1=REPLY(1:1)
      ELSE
        CURCO1='Z'
      END IF
C
C      Obtain colour for cursor after accepting point.
C
      CALL MULREP
     : (' Select colour for cursor after accepting point;',
     :  'W,R,G,BLU,Y,C,M,BLAK$',REPLY,STAT)
      IF (REPLY.NE.'BLAK') THEN
        CURCO2=REPLY(1:1)
      ELSE
        CURCO2='Z'
      END IF
      END
