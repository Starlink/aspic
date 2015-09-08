      SUBROUTINE MSGAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,NY)

C+
C    MSGAREA
C
C	to be used in conjunction with drawfit
C	sets up window and viewport (gks) to message area
C	and moves to next message position
C	all coords in NDC (window=viewport)
C	see also comments for drawfit
C
C    Given (arguments)
C	MSGXT		(R)	x-extent of message area (starts 0)
C	TEXTYT		(R)	y-extent of text area (starts 0)
C	VXT,VYT		(R)	extent of graph area - top right hand corner
C	MSGY		(R)	current position in message area
C	H		(R)	character height
C	NY		(I)	number of lines to move down
C
C    Returned (arguments)
C	MSGY		(R)	updated
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      REAL MSGXT,TEXTYT,VXT,VYT,MSGY,H
      INTEGER NY
      INTEGER WKID
      PARAMETER (WKID=1)

      CALL GKS_SVW(0.0,TEXTYT,MSGXT,VYT)
      CALL GKS_SW (0.0,TEXTYT,MSGXT,VYT)
      CALL MOVEABS(0.0,MSGY)
      CALL GKS_UPDTE(WKID)
      MSGY=MSGY-NY*H
      IF (MSGY.LE.TEXTYT) MSGY=VYT-H
      END
