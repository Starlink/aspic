C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program  XYFITA *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C              XYFITA [ILEVEL=n] [FITTYPE=m] [NSIGMA=f] [MXREJECT=i]
C
C
C          FUNCTION:-
C               It generates the coefficients for a  linear  transformation
C               between  one  set  of  (x,y)  co-ordinates and another. The
C               option is available to reject selected and aberrant points.
C
C
C          USE:-
C               It is part of the GRASP sub-package.
C
C
C
C         USER PARAMETERS:-
C
C         INPUTA                              The   'A'   list   of   (x,y)
C                                             positions.
C
C         INPUTB                              The   'B'   list   of   (x,y)
C                                             positions.
C
C         REJECT           -1                 The number of the selected
C                                             points not to be used in
C                                             the file INPUTA. This param
C                                             is repeatedly asked for
C                                             until -1 is replied.
C
C
C         TRCOEFFS                            On output this contains the 6
C                                             coefficients   which  specify
C                                             the   most   general   LINEAR
C                                             transformation  from  'A'  to
C                                             'B'.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         ILEVEL          2                   This controls the printing of
C                                             output.    A   value   of   1
C                                             supresses all printed output,
C                                             2  prints  details of the fit
C                                             and 3  prints  positions  and
C                                             alignment   errors   for  all
C                                             points  whether  accepted  or
C                                             rejected.
C
C         FITTYPE         4                   This controls the type of fit
C                                             sought. A value of 1 allows a
C                                             shift  of  origin   only,   2
C                                             allows  a  shift+rotation,  3
C                                             allows  shift  +  rotation  +
C
C
C                                             magnification and 4 allows  a
C                                             completely   general   linear
C                                             transformation.
C
C         NSIGMA          1.0E10              A real number specifying  the
C                                             level   at   which   aberrant
C                                             points are  to  be  rejected.
C                                             The  default is so large that
C                                             no points are rejected.
C
C         MXREJECT                            A  non-zero   integer   which
C                                             specifies   the   number   of
C                                             rejection    iterations    to
C                                             perform.  The  default  is so
C                                             large that all  points  lying
C                                             outside  the NSIGMA threshold
C                                             may be rejected.
C
C
C
C         R Warren-Smith           Durham                         14-JAN-82
C   and   A J PENNY                RGO                            83-1-25
C
C
C--------------------------------------------------------------------------
C
C
* ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO OBTAIN A LEAST-SQUARES LINEAR POSITION TRANSFORMATION
*	BETWEEN 2 SETS OF X,Y POSITIONS
*
*METHOD
*	OBTAIN THE INPUT X,Y POSITION LISTS. ASSIGN WORKSPACE AND
*	EXTRACT THE X,Y POSITIONS AND IDENTIFIERS. OBTAIN PARAMETERS
*	CONTROLLING THE FITTING, THEN CALL FITLST TO OBTAIN THE
*	FIT AND PRINT THE RESULTS.
*
*ARGUMENTS
*	NONE
*
*STARLINK PARAMETERS
*	ILEVEL
*		INTERACTION LEVEL: CONTROLS PRINTING OF RESULTS
*	INPUTA
*		INPUT LIST OF POSITIONS
*	INPUTB
*		SECOND INPUT LIST OF POSITIONS
*	NOSPACE/ERROR/
*		ACCESSED IF DYNAMIC STORAGE SPACE IS NOT AVAILABLE
*	FITTYPE
*		CONTROLS NUMBER OF DEGREES OF FREEDOM IN FIT
*	NSIGMA
*		NUMBER OF STANDARD DEVIATIONS AT WHICH ABERRANT POINTS
*		ARE REJECTED
*	MXREJECT
*		MAX NUMBER OF REJECTION ITERATIONS
*	NOMATCH/ERROR/
*		ACCESSED IF INPUT LISTS HAVE NO POSITIONS WITH
*		MATCHING IDENTIFIERS
*	TRCOEFFS
*		OUTPUT PARAMETER, GIVES 6 COEFFICIENTS DEFINING THE
*		TRANSFORMATION
*
*CALLS
*     THIS FILE:
*             FITLSA
*	EDRS PACKAGE:
*		GETPAR,GTXYLR,TRCOUT
*	STARLINK:
*		GETDYN,WRERR,FRDATA
*
*NOTES
*	USES VAX %VAL FACILITY
*
*WRITTEN BY
*	R.F. WARREN-SMITH AND A.J. PENNY     RGO           83-1-25
*-----------------------------------------------------------------------
C
C
      REAL C(6)
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C OBTAIN FIRST INPUT DATA FRAME 'A'
C
      CALL GTXYLR('INPUTA',.FALSE.,NITEMA,LLENA,IPINA,IERRA)
      IF(IERRA.EQ.0) THEN
C
C OBTAINED SUCCESSFULLY... GET SECOND FRAME 'B'
C
	CALL GTXYLR('INPUTB',.FALSE.,NITEMB,LLENB,IPINB,IERRB)
	IF(IERRB.EQ.0) THEN
C
C SECOND FRAME OK... GET WORKSPACE
C
	  CALL GETDYN('IDA',104,5*LLENA,IPIDA,ISTIDA)
	  CALL GETDYN('IDB',104,5*LLENB,IPIDB,ISTIDB)
	  CALL GETDYN('XA',204,LLENA,IPXA,ISTXA)
	  CALL GETDYN('XB',204,LLENB,IPXB,ISTXB)
	  CALL GETDYN('YA',204,LLENA,IPYA,ISTYA)
	  CALL GETDYN('YB',204,LLENB,IPYB,ISTYB)
	  CALL GETDYN('VALID',104,MIN(LLENA,LLENB),IPVAL,ISTVAL)
C
C IF WORKSPACE WAS NOT AVAILABLE, GIVE MESSAGE AND ABORT
C
	  IF((ISTIDA.NE.0).OR.(ISTIDB.NE.0).OR.(ISTXA.NE.0).OR.
     +    (ISTXB.NE.0).OR.(ISTYA.NE.0).OR.(ISTYB.NE.0).OR.
     +    (ISTVAL.NE.0)) THEN
	    CALL WRERR('NOSPACE')
	    GO TO 99
	  ENDIF
C
C OBTAIN TYPE OF FIT (IFIT), NO. OF SIGMA REJECTION THRESHOLD (GAMMA)
C AND MAX NO. OF REJECTED POINTS (MAXIT)
C
	  IFIT=4
	  CALL GETPAR('FITTYPE','INTEGER',1,1.0,4.0,.TRUE.,IFIT,RVAL,
     +    IERR)
	  GAMMA=1.0E10
	  CALL GETPAR('NSIGMA','REAL',1,0.0,1.0E10,.TRUE.,IVAL,GAMMA,
     +    IERR)
          MAXIT=MIN(LLENA,LLENB)
	  CALL GETPAR('MXREJECT','INTEGER',1,0.0,1.0E7,.TRUE.,MAXIT,
     +    RVAL,IERR)
C
C CALL FITLSA TO PERFORM THE FITTING AND DISPLAY THE RESULTS
C
	  CALL FITLSA(%VAL(IPINA),NITEMA,LLENA,%VAL(IPINB),NITEMB,LLENB,
     +    MAXIT,GAMMA,IFIT,ILEVEL,C,%VAL(IPIDA),%VAL(IPXA),%VAL(IPYA),
     +    %VAL(IPIDB),%VAL(IPXB),%VAL(IPYB),%VAL(IPVAL),IERRF)
C
C IF NO MATCHES ARE FOUND BETWEEN THE TWO SETS OF POSITIONS, GIVE
C MESSAGE AND ABORT
C
	  IF(IERRF.EQ.1) THEN
	    CALL WRERR('NOMATCH')
	    GO TO 99
	  ENDIF
C
C WRITE TRANSFORMATION COEFFICIENTS TO ENVIRONMENT
C
	  CALL TRCOUT('TRCOEFFS',C,6,ISTAT)
	ENDIF
      ENDIF
C
C FREE DATA AREAS AND EXIT
C
   99 CALL FRDATA(' ',ISTAT)
      END



C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R FITLSA *
C      *            *
C      **************
C
C
*+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
*PURPOSE
*	TO CONTROL THE FITTING OF A LINEAR TRANSFORMATION BETWEEN
*	TWO SETS OF X,Y POSITIONS AND TO DISPLAY THE RESULTS
*
*METHOD
*	EXTRACT THE X,Y POSITIONS AND IDENTIFIERS FROM THE INPUT LISTS
*	AND MATCH THEM TOGETHER USING XYMTCH. CALL XYFITR TO PERFORM
*	THE FITTING, THEN DISPLAY A TABLE OF RESULTS.
*
*ARGUMENTS
*	LISTA (IN)
*	INTEGER(NITEMA,LLENA)
*		THE FIRST INPUT LIST OF X,Y POSITIONS
*	NITEMA,LLENA (IN)
*	INTEGER
*		THE DIMENSIONS OF LISTA
*	LISTB (IN)
*	INTEGER(NITEMB,LLENB)
*		THE SECOND INPUT LIST OF X,Y POSITIONS
*	NITEMB,LLENB (IN)
*	INTEGER
*		THE DIMENSIONS OF LISTB
*	MAXIT (IN)
*	INTEGER
*		THE MAXIMUM NUMBER OF REJECTION ITERATIONS TO BE
*		PERFORMED BY THE FITTING ROUTINE XYFITR
*	GAMMA (IN)
*	REAL
*		THE NUMBER OF STANDARD DEVIATIONS AT WHICH ABERRANT
*		POINTS ARE REJECTED IN XYFITR
*	IFIT (IN)
*	INTEGER
*		AN INTEGER DEFINING THE TYPE OF LINEAR TRANSFORMATION
*		CALCULATED BY XYFITR
*	ILEVEL (IN)
*	INTEGER
*		INTERACTION LEVEL, CONTROLLING THE PRINTING OF RESULTS
*	C (OUT)
*	REAL(6)
*		RETURNS THE 6 TRANSFORMATION COEFFICIENTS
*	IDA (WORKSPACE)
*	BYTE(20,LLENA)
*		STORAGE FOR THE IDENTIFIERS
*	XA,YA (WORKSPACE)
*	REAL(LLENA)
*		STORAGE FOR X,Y POSITIONS
*	IDB (WORKSPACE)
*	BYTE(20,LLENB)
*		STORAGE FOR IDENTIFIERS
*	XB,YB (WORKSPACE)
*	REAL(LLENB)
*		STORAGE FOR X,Y POSITIONS
*	VALID (WORKSPACE)
*	LOGICAL(*)
*		USED TO HOLD FLAGS INDICATING WHICH POINTS WERE REJECTED
*		DURING FITTING. SHOULD BE AT LEAST MIN(LLENA,LLENB)
*		ELEMENTS LONG
*	IERR (OUT)
*	INTEGER
*		ERROR STATUS RETURN... ZERO INDICATES SUCCESS
*
*CALLS
*	EDRS PACKAGE:
*		EXTLST,XYMTCH,XYFITR
*	STARLINK:
*		WRUSER
*
*NOTES
*	USES BYTE ARRAYS
*
*WRITTEN BY
*	R.F. WARREN-SMITH AND A.J. PENNY         RGO         83-1-25
C ----------------------------------------------------------------------
C
C
C
      SUBROUTINE FITLSA(LISTA,NITEMA,LLENA,LISTB,NITEMB,LLENB,
     +                  MAXIT,GAMMA,IFIT,ILEVEL,C,
     +      		IDA,XA,YA,IDB,XB,YB,VALID,IERR)
C
C
      INTEGER LISTA(NITEMA,LLENA),LISTB(NITEMB,LLENB)
      REAL XA(LLENA),YA(LLENA),XB(LLENB),YB(LLENB),C(6)
      LOGICAL VALID(*)
      BYTE IDA(20,LLENA),IDB(20,LLENB)
      CHARACTER PRBUF*80,FITMSG(4)*80
C
C SET UP INFORMATION MESSAGES TO INDICATE THE FIT USED
C
      DATA FITMSG(1)/'   TRANSFORMATION IS A SHIFT OF ORIGIN ONLY'/,
     +	   FITMSG(2)/'   TRANSFORMATION IS A SHIFT AND ROTATION'/,
     +     FITMSG(3)/
     +     '   TRANSFORMATION IS A SHIFT,ROTATION AND MAGNIFICATION'/,
     +     FITMSG(4)/
     +     '   TRANSFORMATION IS A FULL 6 PARAMETER FIT'/
C
C EXTRACT IDENTIFIERS AND X,Y POSITIONS FROM THE INPUT LISTS
C
      IERR=0
      CALL EXTLST(LISTA,NITEMA,LLENA,IDA,1,20)
      CALL EXTLST(LISTA,NITEMA,LLENA,XA,21,24)
      CALL EXTLST(LISTA,NITEMA,LLENA,YA,25,28)
      CALL EXTLST(LISTB,NITEMB,LLENB,IDB,1,20)
      CALL EXTLST(LISTB,NITEMB,LLENB,XB,21,24)
      CALL EXTLST(LISTB,NITEMB,LLENB,YB,25,28)
C
C CALL XYMTCH TO MATCH THE ID'S IN EACH LIST AND PUT THE MATCHED
C POSITIONS AT THE START OF THE ID,X,Y ARRAYS
C
      CALL XYMTCH(XA,YA,IDA,LLENA,XB,YB,IDB,LLENB,NMTCH,ISTAT)
C
C IF NO MATCHES WERE FOUND, RETURN WITH IERR=1
C
      IF(NMTCH.LE.0) THEN
	IERR=1
	GO TO 99
      ENDIF
C
C GET WHICH MATCHED POSITIONS TO USE
C
      IF (ILEVEL.GE.2) THEN
      CALL WRUSER('NOS OF THE POINTS (IN 1ST LIST) TO REJECT ?',IERR)
      CALL WRUSER('TYPE -1 TO FINISH',IERR)
      ENDIF
      DO I=1,NMTCH
         VALID(I)=.TRUE.
      ENDDO
      KW = 1
      DO WHILE (KW.EQ.1)
         ANM = NMTCH
         KK = -1
         CALL GETPAR('REJECT','INTEGER',1,-1.0,ANM,.TRUE.,KK,RVAL,IERR)
         CALL CNPAR('REJECT',ISTAT)
         IF (KK.GT.0) THEN
            VALID(KK) = .FALSE.
         ELSE
            KW = -1
         ENDIF
      ENDDO
C
C  DO THE MATCH
C
      CALL XYFITR(XA,YA,XB,YB,VALID,NMTCH,MAXIT,GAMMA,IFIT,C,IERR)
C
C PRINT NUMBER OF MATCHED POSITIONS
C
      IF(ILEVEL.GE.2) THEN
	CALL WRUSER(' ',ISTAT)
	WRITE(PRBUF,10)NMTCH
   10   FORMAT(3X,I8,' POSITION(S) MATCHED BETWEEN INPUT LISTS')
        CALL LBGONE(PRBUF(4:))
        CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C IF TABLE OF RESULTS IS NEEDED, PRINT HEADINGS
C
      IF(ILEVEL.GE.3) THEN
	CALL WRUSER(' ',ISTAT)
	WRITE(PRBUF,11)
   11   FORMAT(3X,'TRANSFORMED A POSITION',12X,'B POSITION')
        CALL WRUSER(PRBUF,ISTAT)
	WRITE(PRBUF,12)
   12   FORMAT(3X,'----------------------',12X,'----------')
	CALL WRUSER(PRBUF,ISTAT)
	WRITE(PRBUF,13)
   13   FORMAT(2X,2(' X COORD.      Y COORD.     '),3X,
     +  'ALIGNMENT ERROR')
        CALL WRUSER(PRBUF,ISTAT)
	WRITE(PRBUF,14)
   14   FORMAT(2X,2(' --------      --------     '),3X,
     +  '---------------')
	CALL WRUSER(PRBUF,ISTAT)
      ENDIF
C
C CALCULATE RMS ERROR AND COUNT NUMBER OF POSITIONS REJECTED
C
      IF(ILEVEL.GE.2) THEN
	NREJ=0
	SIGMA=0.0
	DO 66 I=1,NMTCH
C
C CALCULATE TRANSFORMED 'A' POSITIONS AND ERROR
C
	  XD=C(1)+C(2)*XA(I)+C(3)*YA(I)
	  YD=C(4)+C(5)*XA(I)+C(6)*YA(I)
	  ERRSQ=(XD-XB(I))**2+(YD-YB(I))**2
	  ERR=SQRT(ERRSQ)
	  IF(VALID(I)) THEN
C
C INCREASE SUM OF ERRORS AND PRINT RESULT IF REQUIRED
C
	    SIGMA=SIGMA+ERRSQ
	    IF(ILEVEL.GE.3) WRITE(PRBUF,15)XD,YD,XB(I),YB(I),ERR,'OK'
	  ELSE
C
C COUNT ONE REJECTED POINT AND PRINT RESULT IF REQUIRED
C
	    NREJ=NREJ+1
	    IF(ILEVEL.GE.3) WRITE(PRBUF,15)XD,YD,XB(I),YB(I),ERR,'REJ'
	  ENDIF
   15     FORMAT(1X,4(1X,SS,G13.6),4X,SS,G12.4,2X,A3)
          IF(ILEVEL.GE.3) CALL WRUSER(PRBUF,ISTAT)
   66   CONTINUE
C
C CALCULATE RMS ERROR AND PRINT NO. OF REJECTED POINTS AND RMS ERROR
C
	IF(NREJ.LT.NMTCH) THEN
	  SIGMA=SQRT(SIGMA/(NMTCH-NREJ))
	ELSE
	  SIGMA=0.0
	ENDIF
	CALL WRUSER(' ',ISTAT)
	WRITE(PRBUF,16) NREJ
   16	FORMAT(3X,I8,' POSITION(S) REJECTED')
        CALL LBGONE(PRBUF(4:))
	CALL WRUSER(PRBUF,ISTAT)
	CALL WRUSER(' ',ISTAT)
	WRITE(PRBUF,17) SIGMA
   17   FORMAT(3X,'RMS ALIGNMENT ERROR=',SS,G11.4)
	CALL WRUSER(PRBUF,ISTAT)
	CALL WRUSER(' ',ISTAT)
C
C PRINT MESSAGE SHOWING TYPE OF FIT USED
C
	CALL WRUSER(FITMSG(IFIT),ISTAT)
	CALL WRUSER(' ',ISTAT)
C
C FINALLY PRINT THE TRANSFORMATION COEFFICIENTS
C
	WRITE(PRBUF,18)
   18   FORMAT(3X,'TRANSFORMATION COEFFICIENTS:')
	CALL WRUSER(PRBUF,ISTAT)
	CALL WRUSER(' ',ISTAT)
	DO 44 J=1,4,3
	  WRITE(PRBUF,19)((L,C(L)),L=J,J+2)
   19	  FORMAT(10X,3('C(',I1,')=',SS,G13.6,2X))
	  CALL WRUSER(PRBUF,ISTAT)
   44   CONTINUE
	CALL WRUSER(' ',ISTAT)
      ENDIF
   99 RETURN
      END
