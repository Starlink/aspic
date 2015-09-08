      SUBROUTINE GRAFAREA(MSGXT,TEXTYT,VXT,VYT)

C+
C    GRAFAREA
C
C	to be used in conjunction with drawfit
C	sets up window and viewport (gks) to graph area
C	all coords in NDC
C	see also comments for drawfit
C
C    Given (arguments)
C	MSGXT		(R)	x-extent of message area (starts 0)
C	TEXTYT		(R)	y-extent of text area (starts 0)
C	VXT,VYT		(R)	extent of graph area - top right hand corner
C
C    D. TUDHOPE/ROE/Mar 1983
C-

      REAL MSGXT,TEXTYT,VXT,VYT

      CALL GKS_SVW(MSGXT,TEXTYT,VXT,VYT)
      CALL HIGR_GZRST
      END
