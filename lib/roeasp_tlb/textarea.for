      SUBROUTINE TEXTAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,NY)

C+
C    TEXTAREA
C
C	to be used in conjunction with drawfit
C	sets up window and viewport (gks) to text area and moves there
C	all coords in NDC
C	see also comments for drawfit
C
C    Given (arguments)
C	MSGXT		(R)	x-extent of message area (starts 0)
C	TEXTYT		(R)	y-extent of text area (starts 0)
C	VXT,VYT		(R)	extent of graph area - top right hand corner
C	MSGY		(R)	current position in message area
C	H		(R)	character height in message area
C	NY		(I)	number of lines to update
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      REAL MSGXT,TEXTYT,VXT,VYT,MSGY,H
      INTEGER NY
      INTEGER WKID
      PARAMETER (WKID=1)

      CALL GKS_SVW(0.0,0.0,VXT,TEXTYT)
      CALL GKS_SW (0.0,0.0,VXT,TEXTYT)
      CALL MOVEABS(0.0,TEXTYT-NY*H)
      CALL GKS_UPDTE(WKID)
      END
