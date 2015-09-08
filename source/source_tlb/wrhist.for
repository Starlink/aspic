C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   WRHIST *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               WRHIST   [ QUIET=true ]
C
C
C          FUNCTION:-
C               It computes, lists and stores the histogram of a frame.
C
C
C          USE:-
C               It may used to give a quick listing of a histogram (with  a
C               low  value  of  NUMBIN)  or to store a histogram away to be
C               plotted by HISTPLOT.
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               The input Starlink  frame  of
C                                             any dimension.
C
C         HMIN            Minimum             The data value  corresponding
C                                             to   the  first  bin  of  the
C                                             histogram.
C
C         HMAX            Maximum             The data value  corresponding
C                                             to   the   last  bin  of  the
C                                             histogram.
C
C         NUMBIN          HMAX-HMIN+1         The number  of  bins  in  the
C                                             histogram.   Note   that   if
C                                             QUIET=false (default) all  of
C                                             these  bins  will be typed on
C                                             the terminal.
C
C         HGRAM                               If present this is  the  name
C                                             of  a  1-D frame to store the
C                                             histogram.  It  is  also  the
C                                             name  of  a descriptor in the
C                                             INPUT frame, to which  it  is
C                                             also written.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         QUIET           F                   By default the  histogram  is
C                                             listed  on  the  terminal. If
C
C
C                                             TRUE  no  such   listing   is
C                                             produced,  which is desirable
C                                             if  the  histogram   contains
C                                             many bins.
C
C
C
C         W F Lupton               RGO                            13-JAN-82
C
C
C--------------------------------------------------------------------------



	PROGRAM WRHIST
C WFL RGO OCT 1981 (BASED ON WRHIST, WHERE OUTPUT GOES ONLY TO A
C 1D BDF - THIS VERSION IS EXPLORING HOW BEST USE MAY BE MADE OF THE
C STARLINK DATA STRUCTURE WHEN IT BECOMES AVAILABLE.)
C Modified by K F Hartley on 2/12/82 to store HMIN and HMAX as
C descriptor items in the output histogram.
	IMPLICIT INTEGER (A-Z)
	REAL HMIN,HMAX
	LOGICAL OUTPUT
	CHARACTER*72 TEXT
      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
	INTEGER AXIS(99)
	LOGICAL QUIET
C
C GET THE INPUT IMAGE
C
	CALL RDIMAG('INPUT',FMT_R,99,AXIS,NDIM,INPTR,STATUS)
	IF (STATUS.NE.ERR_NORMAL) THEN
		CALL WRERR('BADINP')
		CALL EXIT
	ENDIF
	SIZE=1
	DO I=1,NDIM
		SIZE=SIZE*AXIS(I)
	ENDDO
C
C DETERMINE RANGE OF PIXEL VALUES IN INPUT IMAGE
C
	CALL ASP_RANGE ('INPUT',%VAL(INPTR),SIZE,HMIN,HMAX,STATUS)
C
C AND RANGE OF INTENSITY FOR HISTOGRAM
C
	CALL RDKEYR('HMIN',.TRUE.,1,HMIN,I,STATUS)
	CALL RDKEYR('HMAX',.TRUE.,1,HMAX,I,STATUS)
C
C AND THE NUMBER OF OUTPUT BINS
C
	NUMBIN = MIN(32767.0,HMAX-HMIN)
	CALL RDKEYI('NUMBIN',.TRUE.,1,NUMBIN,I,STATUS)
C
C AND THE NAME OF THE OUTPUT IMAGE
C
	OUTPUT=.TRUE.
	CALL WRIMAG('HGRAM',FMT_SL,NUMBIN,1,OUTPTR,STATUS)
	IF (STATUS.EQ.ERR_PARNUL) THEN
		CALL GETDYN('WORK',FMT_SL,NUMBIN,OUTPTR,STATUS)
	ENDIF
	IF (STATUS.NE.ERR_NORMAL) THEN
		OUTPUT=.FALSE.
	END IF
C
C DETERMINE WHETHER USER TERMINAL OUTPUT REQUIRED
C
	QUIET = .FALSE.
	CALL RDKEYL('QUIET',.TRUE.,1,QUIET,I,STATUS)
C
C ROUTINE ASP_HGRAM DOES THE REST
C
	CALL ASP_HGRAM('INPUT',%VAL(INPTR),SIZE,HMIN,HMAX,NUMBIN,
	1	%VAL(OUTPTR),STATUS)
C
C NOW, IF QUIET IS FALSE, LIST HISTOGRAM ON TERMINAL
C
	IF (.NOT.QUIET) THEN
		CALL WRHIS2 (%VAL(OUTPTR),NUMBIN,HMIN,HMAX)
	ENDIF
C
C      Now write HMIN,HMAX into the image as  descriptor items.
C
	IF (OUTPUT) THEN
		CALL RTOC(HMIN,TEXT,ISTAT)
		CALL WRDSCR('HGRAM','HMIN',TEXT,1,ISTAT)
		CALL RTOC(HMAX,TEXT,ISTAT)
		CALL WRDSCR('HGRAM','HMAX',TEXT,1,ISTAT)
	END IF
	CALL FRDATA (' ',STATUS)
	END
C
	SUBROUTINE WRHIS2 (HGRAM,NUMBIN,HMIN,HMAX)
C
C ROUTINE TO LIST HISTOGRAM ON THE TERMINAL
C
	INTEGER NUMBIN,I,STATUS
	INTEGER HGRAM(NUMBIN)
	REAL HMIN,HMAX,BINWID
	CHARACTER RECORD*72
	BINWID = MAX(HMAX-HMIN,1E-18) / NUMBIN
	DO I=1,NUMBIN
		WRITE (RECORD,'(1PG15.7,'' TO '',1PG15.7,'' ,'','//
	1		'I8,'' PIXELS'')') HMIN+(I-1)*BINWID,
	1		HMIN+I*BINWID,HGRAM(I)
		CALL WRUSER (RECORD,STATUS)
	ENDDO
	END
