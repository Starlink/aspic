      SUBROUTINE CRB_PATCH
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   PATCH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               PATCH  [ NOISE=f ]  [ ORDER=n ]  [ CURSOR=t ]  [ FILE=f
C
C
C          FUNCTION:-
C               It allows the user to replace several circular  patches
C               an image with a fitted noisy piece of synthetic data. It
C               derived from a program, by  W  D  Pence (at  University
C               Sussex).The ARGS cursor and trackerball are used to cont
C               the process.
C
C
C          USE:-
C               It may be used to remove large defects, bright galaxies
C               any other localised unwanted pixels. The result can be v
C               convincing. If  the  defects  are  more  like  strips  t
C               circular patches the program ZAPLIN should be used.
C
C
C
C         USER PARAMETERS:-
C
C         IN                                  The input 2-D image which
C                                             being  displayed on the AR
C
C         OUTPUT                              The  new   image   with
C                                             patched regions.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         NOISE           1                   This is a noise factor  wh
C                                             may  be  between 0 (no noi
C                                             and 1 (noise level calcula
C                                             from the whole image).
C
C         ORDER           3                   This is the the degree of
C                                             2-dimensional  surface  wh
C                                             is to be fitted to an annu
C                                             around the circular patch.
C                                             may  be  0  (constant)  to
C                                             (bi-cubic).
C
C
C         CURSOR          TRUE                This defines if the cusor
C                                             be used to define the patc
C                                             If FALSE then the position
C                                             size of the  patches  will
C                                             read from the file PATCHES
C
C
C         FILE            FALSE               This defines  if  the posi
C                                             and size of  the  patches
C                                             be   written   to   the
C                                             PATCHES.DAT
C
C
C
C
C         USE OF TRACKER-BALL BUTTONS:-
C
C         GREEN 1     Accept this size of patch  and  then  do  a  fit
C                     display  the results. AFTER THIS BUTTON has been u
C                     then GREEN means accept the result  and  move  on
C                     another  location,  whilst  RED  means  revert to
C                     previous state.
C
C         WHITE 2     Decrease the size of the patch.
C
C         WHITE 3     Increase the size of the patch.
C
C         RED   4     Exit from the program.
C
C
C
C
C
C
C         D J King-W D Pence       RGO-U. of Sussex                7-JAN
C
C
C-----------------------------------------------------------------------
 
 
 
	PARAMETER NDIMER=200
	INTEGER DIM(2)
	LOGICAL TRIM,LLOG,CURSOR,FILE
	CHARACTER VALUE*80,COM*2
	REAL VLO,VHI,ZXC,ZYC,ER(NDIMER)
      INCLUDE 'INTERIM(FMTPAR)'
	CALL ARGS_NUMIM(IDMAX)
	IF (IDMAX.EQ.0) THEN
		CALL WRERR('NOIMS')
	ELSE
	CALL SRINIT(0,.FALSE.,JSTAT)
		IF (JSTAT.NE.0) THEN
			CALL WRERR('NOARGS')
		ELSE
			CALL ARGS_RDIM(IMX,IMY,ISX,ISY,I,I,JSTAT)
			IXOR=IMX-(ISX/2)
			IYOR=IMY-(ISY/2)
	CALL ARGS_RDPAR('DISPZOOM',1,VALUE,NVALS,JSTAT)
	CALL ASP_DZTOL('TRIM',VALUE,TRIM,JSTAT)
	CALL ASP_DZTOL('LOG',VALUE,LLOG,JSTAT)
	CALL ASP_DZTOF('PVLO',VALUE,VLO,JSTAT)
	CALL ASP_DZTOF('PVHI',VALUE,VHI,JSTAT)
	CALL ASP_DZTOF('ZXC',VALUE,ZXC,JSTAT)
	IXPOS=NINT(ZXC)
	IF (IXPOS.EQ.0) IXPOS=256
	CALL ASP_DZTOF('ZYC',VALUE,ZYC,JSTAT)
	IYPOS=NINT(ZYC)
	IF (IYPOS.EQ.0) IYPOS=256
	CALL ARGS_WRPAR('DISPZOOM',VALUE,1,JSTAT)
	CALL RDIMAG('IMAGE',FMT_R,2,DIM,IADIM,IPNT,JSTAT)
	CALL WRIMAG('OUTPUT',FMT_R,DIM,2,IOPNT,JSTAT)
      PRINT *,'*** TRACKERBALL INSTRUCTIONS ***'
      PRINT *,' '
      PRINT *,' GREEN - ACCEPT PATCH & DISPLAY PATCHED'
      PRINT *,'   GREEN - OK GO TO NEXT LOCATION'
      PRINT *,'   RED   - REVERT TO PREVIOUS STATE'
      PRINT *,' WHITE - BIGGER'
      PRINT *,' WHITE - SMALLER'
      PRINT *,' RED   - EXIT'
	CALL OUTDATA(%VAL(IPNT),%VAL(IOPNT),DIM(1),DIM(2))
        CALL FRDATA('IMAGE',JSTAT)
        CALL CNPAR('IMAGE',JSTAT)
	ICOL=DIM(1)
	IROW=DIM(2)
C
C     SET UP ARRAY OF RANDOM ERRORS WITH GAUSSIAN DISTRIBUTION
C     FOR APPROXIMATING THE NOISE WHEN INTERPOLATING
C
	SIG=0.0
	CALL RDKEYR('NOISE',.TRUE.,1,SIG,NVALS,JSTAT)
C
C     USE NAG ROUTINE TO GENERATE NORMAL ERRORS, MEAN=0, SIGMA=1
C
      CALL G05CBF(0)
        DO 2 I=1,200
2       ER(I)=G05DDF(0.,SIG)
	NCODE=3
	CALL RDKEYI('ORDER',.TRUE.,1,NCODE,NVALS,JSTAT)
	CURSOR=.TRUE.
	CALL RDKEYL('CURSOR2',.TRUE.,1,CURSOR,NVALS,JSTAT)
	IF (.NOT.CURSOR) OPEN(UNIT=9,STATUS='OLD',FILE='PATCHES.DAT')
	FILE=.FALSE.
	CALL RDKEYL('FILE2',.TRUE.,1,FILE,NVALS,JSTAT)
	IF (FILE) OPEN(UNIT=9,STATUS='NEW',FILE='PATCHES.DAT')
	CALL STAR(%VAL(IOPNT),ICOL,IROW,TRIM,LLOG,VLO,VHI,
     1	IXPOS,IYPOS,IXOR,IYOR,ER,NDIMER,NCODE,CURSOR,FILE)
	CALL FRDATA('OUTPUT',JSTAT)
	CALL CNPAR('OUTPUT',JSTAT)
	ENDIF
	ENDIF
	IF (FILE.OR.CURSOR) CLOSE(UNIT=9)
       CALL CNPAR('OUTPUT',ISTAT)
	END
