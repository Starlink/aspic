      SUBROUTINE DRAWFIT (NLEVS,VALS,INTS,NTAB,XVALS,TABINT,TABS,TABF,
     :                    MSGXT,TEXTYT,VXT,VYT,MSGY,H)
*+
*
*
*     ---------
*       DRAWFIT
*     ---------
*
*
*	part of calibration suite - plot graph of fit and levels table
*
*  Given (arguments)
*	NLEVS		(I)	number of levels in vals,ints
*	VALS(NLEVS)	(RA)	levels table - VALUES
*	INTS(NLEVS)	(RA)	levels table - INTENSITIES
*	NTAB		(I)	length of look up table (xvals,tabint)
*	XVALS(NTAB)	(RA)	VALUES for entries in TABINT
*	TABINT(NTAB)	(RA)	look up table of INTENSITIES
*	TABS		(R)	minimum value in VALS and XVALS
*	TABF		(R)	maximum .......................
*
*  Returned (arguments)
*	MSGXT		(R)	X-extent of message area (right X boundary)
*	TEXTYT		(R)	Y-extent of text area (upper Y boundary)
*	VXT		(R)	X-extent of viewport (right X boundary)
*	VYT		(R)	Y-extent of viewport (upper Y boundary)
*	MSGY		(R)	current Y position in message area
*	H		(R)	character height for message area
*
*  uses HIGR and GKS to plot a graph of the fit obtained for the levels in levels table
*  plots levels (vals,ints) as a polymarker and then draws a graph of (xvals,tabint) (the lookup table)
*  first has to find out the min and max for the INTENSITIES in levels and tabint
*  this is already given for the values by tabs and tabf
*   screen is divided into 3 areas:
*    graph area where graph is plotted and labelled
*    text area where text info is displayed after graph is plotted
*    message area where short messages are displayed during interaction with graph
*   the subroutines GRAFAREA,TEXTAREA,MSGAREA set window transfns for areas
*   all descriptors of areas are in NDC
*   HIGR sets up its own (unknown) window for grafarea, the windows for the
*   other 2 areas are set equal to the viewports.
*   subroutine SFWV is called to force HIGR to use the workstn viewport (screen)
*   in the msgarea track is kept of next message position, but textarea is
*   basically for when all graphs and interaction is finished (except SPLFIT)
*
*   so screen is divided into G)raph, T)ext, Message areas thus:
*		+---------------+
*		|  :            |
*		| M:       G    |
*		|  :            |
*		|---------------|
*		|       T       |
*		+---------------+
*
*  D. Tudhope. ROE.  March 1983.
*-

      INTEGER NLEVS,NTAB
      REAL VALS(NLEVS),INTS(NLEVS),XVALS(NTAB),TABINT(NTAB),TABS,TABF
      INTEGER WKID,LOCID,CONID,MTYPE,PREC
      PARAMETER(WKID=1,LOCID=1,CONID=0,MTYPE=4,PREC=1)
C*  extents of message, text and whole viewport area
      REAL MSGXT,TEXTYT,VXT,VYT
C*  current Y-position in message area
      REAL MSGY
C*  character height (in world coords)
      REAL H
C*  min and max of INTENSITIES
      REAL TABIS,TABIF
      INTEGER J,TERMINAL
C*  metres-->world scaling factors
      REAL FX,FY

C*  set up constant area boundaries
      MSGXT=0.06
      TEXTYT=0.14

      TABIS=INTS(1)
      TABIF=TABIS
      DO J=1,NLEVS
        IF (INTS(J).LT.TABIS) TABIS=INTS(J)
        IF (INTS(J).GT.TABIF) TABIF=INTS(J)
      ENDDO
      DO J=1,NTAB
        IF (TABINT(J).LT.TABIS) TABIS=TABINT(J)
        IF (TABINT(J).GT.TABIF) TABIF=TABINT(J)
      ENDDO

      CALL READI('TERMINAL','Put terminal in graphics mode (if necesary)
     : and type 1 for ARGS, 2 for TEKTRONIX (or lookalike), 3 for GOC',
     : 2,1,3,TERMINAL,IST)

      CALL HIGR_GZBGN(WKID,CONID,TERMINAL,PREC)
C*  allow full workstation viewport to be used
      CALL SFWV(WKID,VXT,VYT)
C*  work out character height (in world coords) for message area
        CALL GKS_SVW(0.0,TEXTYT,MSGXT,VYT)
        CALL GKS_SW (0.0,TEXTYT,MSGXT,VYT)
        CALL MTOWT(WKID,FX,FY)
        H=0.0075*FY
C*  initialise current message position to top of area
        MSGY=VYT-H
C*  set viewport to graph area
      CALL GKS_SVW(MSGXT,TEXTYT,VXT,VYT)
      CALL HIGR_DRAXES(TABS,TABIS,TABF,TABIF,
     : 'Observed Values#','True Intensities#','Plot of Fit#','#')
C*  set marker size
      CALL GKS_SMK(0.0,H,H,0.0)
      CALL GKS_POLYM(NLEVS,VALS,INTS,MTYPE)
      CALL HIGR_DRLINE(XVALS,TABINT,NTAB)
C*  save HIGR window
      CALL HIGR_GZMEM
C*  turn locator echo on for later
      CALL GKS_SEC(WKID,1,1,.TRUE.)
C*  set viewport to text area and move there
      CALL TEXTAREA(MSGXT,TEXTYT,VXT,VYT,MSGY,H,1)
      END
