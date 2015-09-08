      SUBROUTINE SPLCKN(MSGXT,TEXTYT,VXT,VYT,MSGY,H,NLEVS,KNOTS,NKNOTS)
C+
C   SPLCKN
C
C     modify knot list interactively for SPLFIT in calibration suite
C	see comments in drawfit for graph,text ans message areas
C
C   Given      (arguments)
C   NLEVS  -  I*4       - number of data points
C   MSGXT,TEXTYT,VXT,VYT  -  R*4	- dimensions of message,text and graph areas
C   MSGY,H		   -  R*4	- current msg position and char height
C
C    given and returned:
C	MSGY    R*4
C       KNOTS    R*8 array - knots (exterior knots undefined)
C       NKNOTS   I*4       - number of knots (includes exterior)
C
C  D. Tudhope/ROE/Mar 1983
C   J.A.Cooke/UOE/27.8.1981
C-
 
      INTEGER NKNOTS
      REAL*8 KNOTS(NKNOTS)
      REAL MSGXT,TEXTYT,VXT,VYT,MSGY,H
      CHARACTER*1 ADE
      INTEGER WKID
      PARAMETER (WKID=1)
      INTEGER I,STAT,J,IKNOTS,MINI
      REAL X,Y,D,MIND
      REAL WKNOTS(20)
 
C   transfer knots to work array.....
      IKNOTS=NKNOTS-8
      DO I=1,IKNOTS
         J=I+4
         WKNOTS(I)=KNOTS(J)
      ENDDO
      CALL TEXTAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,1)
      CALL WRUSER(' Prompts for actions A)dd, D)elete, E)xit',STAT)
      CALL WRUSER('appear in message area. If you type "A" or "D"',STAT)
      CALL WRUSER('then cursor will appear for you indicate place',STAT)
      CALL WRUSER('to add a knot or knot to be deleted.',STAT)
      CALL WRUSER(' Only X-coordinate is relevant.',STAT)
      CALL WRUSER('This process is repeated until E)xit is typed.',STAT)
      ADE='B'
      DO WHILE (ADE.NE.'E')
         CALL MSGAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,2)
         CALL READC('ADE','action?','A','A','E',ADE,STAT)
         IF (ADE.EQ.'A') THEN
C         add knot.....
            IF (IKNOTS.GE.(NLEVS-4)) THEN
               CALL MSGAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,1)
               CALL WRUSER('too many knots',STAT)
            ELSE
               CALL GRAFAREA(MSGXT,TEXTYT,VXT,VYT)
               CALL GKS_RQLC(WKID,1,X,Y)
               IKNOTS=IKNOTS+1
               WKNOTS(IKNOTS)=X
               CALL ARROW(X,0.0)
            ENDIF
         ELSE IF (ADE.EQ.'D') THEN
C         delete knot.....
            IF (IKNOTS.LE.0) THEN
               CALL MSGAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,1)
               CALL WRUSER('no knots left',STAT)
            ELSE
               CALL GRAFAREA(MSGXT,TEXTYT,VXT,VYT)
               CALL GKS_RQLC(WKID,1,X,Y)
C            remove nearest knot from list.....
               MINI=1
               MIND=ABS(WKNOTS(1)-X)
               DO I=2,IKNOTS
                  D=ABS(WKNOTS(I)-X)
                  IF (D.LT.MIND) THEN
                     MIND=D
                     MINI=I
                  ENDIF
               ENDDO
               CALL CROSS(WKNOTS(MINI),0.0)
               DO I=MINI,IKNOTS-1
C*                 not executed if last knot is nearest
                  WKNOTS(I)=WKNOTS(I+1)
               ENDDO
               IKNOTS=IKNOTS-1
            ENDIF
         ENDIF
      ENDDO
      IF (IKNOTS.GT.0) THEN
C            sort knots.....
         DO I=1,IKNOTS
            J=I+4
            KNOTS(J)=WKNOTS(I)
         ENDDO
         NKNOTS=IKNOTS+8
         CALL M01ANF(KNOTS,5,NKNOTS-4,STAT)
      ENDIF
      END
