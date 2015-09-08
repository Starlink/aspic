C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C        ******************
C        *                *
C        * PROGRAM XYJOIN *
C        *                *
C        ******************
C
C
C
C          CALLING SEQUENCE:-
C                XYJOIN [ILEVEL=?]
C
C
C          FUNCTION:-
C              It joins together two XYfiles.
C
C          It allows you to shift the data in the files relative to each
C          other before joining.
C          Thus if you have
C                             _______      __________
C                             l     l      l        l
C                             l     l      l        l
C                             l     l      l        l
C                             l  A  l      l  B     l
C                             l     l      l        l
C                             l     l      l        l
C                             -------      ----------
C
C          you can join them
C
C                             _______
C                             l     l
C                             l  A  l
C                             l  __________
C                             l  l  l     l
C                             l  l  l     l
C                             l  l  l     l
C                             -------     l
C                                l   B    l
C                                l        l
c                                ----------
C
C          They do not even have to overlap.
C          The 2nd file is written on top of the 1st file data,identifiers
C          and headers deleting them. Parameters with no entry are put to 0.0
C
C          To simply add one file onto the end of another, use the default
C          displacements.
C
C
C
C
C          USER PARAMETERS
C
C          INPUTA                       The first  XY file
C
C          INPUTB                       The second XY file
C
C          PARSHFT       0              The no of columns that the
C                                       parameters in the 2nd file are to
C                                       be shifted along before the files
C                                       are added.
C
C          RECSHFT   Entries in 1st     The number of rows the records in
C                                       the 2nd file are to be shifted
C                                       down before the files are added.
C
C
C          OUTPUT                       The name of the file the data is
C                                       to be stored in.
C
C          TITLE  Output from XYJOIN    A title to be stored in the OUTPUT
C                                       file.
C
C
C          NORMALLY DEFAULTED PARAMETERS:-
C
C          ILEVEL           2           This controls the typing of
C                                       informational messages. If 1 ,then
C                                       no typing; if 2, then typing.
C
C
C
C          A.J.Penny and others  RGO                      JAN-83
C
C
C------------------------------------------------------------------



C   PROGRAM PURPOSE
C
C     TO JOIN 2 XY FILES
C
C  METHOD
C	OBTAIN INPUT FILES.
C	OBTAIN AN OUTPUT FILE OF THE REQUIRED SIZE AND INSERT
C	THE LISTS INTO IT. UPDATE THE OUTPUT DESCRIPTOR ITEMS
C
C  ARGUMENTS
C	NONE
C
C STARLINK PARAMETERS
C	ILEVEL
C		INTERACTION LEVEL: CONTROLS PRINTING OF INFORMATIONAL
C		MESSAGES
C	INPUTA
C             1st existing file
C     INPUTB
C             2nd existing file
C     HEADFILE
C             Flag for which input file headers to be copied from
C	NOLIST/ERROR/
C		ACCESSED IF NO ENTRIES IN INPUT FILES
C	OUTPUT
C		OUTPUT FILE
C	TITLE
C		A TITLE FOR THE OUTPUT FILE
C
C  CALLS
C	EDRS PACKAGE:
C		GETPAR,GTXYLR,GTXYLW,LBGONE,GTDSCR,PTDSCR
C	STARLINK:
C		GETDYN,WRERR,WRUSER,RDKEYC,CYDSCR,FRDATA
C
C  NOTES
C	USES VAX %VAL FACILITY
C
C  WRITTEN BY
C     A.J. PENNY
C ----------------------------------------------------------------------
C
C
      CHARACTER CVAL*1,TITLE*30,TEXT*72,TEXTH*7,HEADER*30
      LOGICAL VALID
C
C
C
      VALID = .TRUE.
C
C OBTAIN INTERACTION LEVEL
C
      ILEVEL=2
      CALL GETPAR('ILEVEL','INTEGER',1,1.0,3.0,.TRUE.,ILEVEL,RVAL,IERR)
C
C OBTAIN THE INPUT DATA FRAMES
C
      CALL GTXYLR('INPUTA',.TRUE.,NITEMA,LSTLNA,IPINA,IERR1)
      NPARA = NITEMA - 5
      IF (IERR1.NE.0) THEN
         VALID = .FALSE.
      ELSE
         CALL GTXYLR('INPUTB',.TRUE.,NITEMB,LSTLNB,IPINB,IERR2)
         NPARB = NITEMB - 5
         IF (IERR2.NE.0) THEN
            VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Get displacement of 2nd file from 1st
C
      IF (VALID) THEN
         KSHXB = 0
         CALL GETPAR('PARSHFT','INTEGER',1,-1.0E6,1.0E6,.TRUE.,
     +               KSHXB,RVAL,IERR)
         KSHYB = LSTLNA
         CALL GETPAR('RECSHFT','INTEGER',1,-1.0E6,1.0E6,.TRUE.,
     +               KSHYB,RVAL,IERR)
C
C  Work out shifts of files and size of Output file
C
         KSHPAR = KSHXB
         KSHREC = KSHYB
         KSHXA = 0
         KSHYA = 0
         KXST = 1 + KSHXB
         IF (KXST.GE.1) THEN
            KXST = 1
         ELSE
            KSHXA = -1*KSHXB
            KSHXB = 0
         ENDIF
         KXEND = NPARB + KSHPAR
         IF (NPARA.GT.KXEND) KXEND = NPARA
         KYST = 1 + KSHYB
         IF (KYST.GE.1) THEN
            KYST = 1
         ELSE
            KSHYA = -1*KSHYB
            KSHYB = 0
         ENDIF
         KYEND = LSTLNB + KSHREC
         IF (LSTLNA.GT.KYEND) KYEND = LSTLNA
         NPARC = KXEND - KXST + 1
         LSTLNC = KYEND - KYST + 1
      ENDIF
C
C  Open Output list
C
      IF (VALID) THEN
         IF (LSTLNC.EQ.0) THEN
           CALL WRERR('NOLIST')
         ELSE
            NITEMC = NPARC + 5
            CALL GTXYLW('OUTPUT',.FALSE.,NITEMC,LSTLNC,IPOUT,IERR)
            IF (IERR.NE.0) VALID = .FALSE.
         ENDIF
      ENDIF
C
C  Clear Output list
C
      IF (VALID) THEN
         CALL XYLOAD(%VAL(IPOUT),NITEMC,LSTLNC)
C
C  Put in 1st list
C
         LSHYA = KSHYA + 1
         CALL EXTLSA(%VAL(IPINA),NITEMA,LSTLNA,1,LSTLNA,1,5,
     +               %VAL(IPOUT),NITEMC,LSTLNC,LSHYA,1)
         LSHXA = KSHXA + 6
         LSHYA = KSHYA + 1
         CALL EXTLSA(%VAL(IPINA),NITEMA,LSTLNA,1,LSTLNA,6,NITEMA,
     +               %VAL(IPOUT),NITEMC,LSTLNC,LSHYA,LSHXA)
C
C  Put in 2nd list
C
         LSHYB = KSHYB + 1
         CALL EXTLSA(%VAL(IPINB),NITEMB,LSTLNB,1,LSTLNB,1,5,
     +               %VAL(IPOUT),NITEMC,LSTLNC,LSHYB,1)
         LSHXB = KSHXB + 6
         LSHYB = KSHYB + 1
         CALL EXTLSA(%VAL(IPINB),NITEMB,LSTLNB,1,LSTLNB,6,NITEMB,
     +               %VAL(IPOUT),NITEMC,LSTLNC,LSHYB,LSHXB)
      ENDIF
C
C  Put Title
C
      IF (VALID) THEN
	   CALL GTDSCR('INPUTA','TITLE','CHARACTER',IVAL,RVAL,
     +                TITLE,IERR)
         CALL CHARLN(TITLE,KLEN)
	   IF (KLEN.EQ.0) TITLE='Output from XYJOIN'
         CALL RDKEYC('TITLE',.TRUE.,1,TITLE,NVAL,ISTAT)
C
C  Load Output Descriptors
C
	   CALL PTDSCR('OUTPUT','NITEM','INTEGER',NITEMC,RVAL,CVAL,IERR)
	   CALL PTDSCR('OUTPUT','LSTLEN','INTEGER',LSTLNC,RVAL,CVAL,
     +               IERR)
	   CALL PTDSCR('OUTPUT','TITLE','CHARACTER',IVAL,RVAL,TITLE,
     +	       IERR)
         DO K = 1,NPARC
            WRITE(TEXTH,900)K
            HEADER = ' '
            CALL PTDSCR('OUTPUT',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
         ENDDO
         DO K = 1,NPARA
            WRITE(TEXTH,900)K
  900       FORMAT('HEAD',I3.3)
            CALL GTDSCR('INPUTA',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
            IF (IERR.NE.0) HEADER = ' '
            NX = K + KSHXA
            WRITE(TEXTH,900)NX
            CALL PTDSCR('OUTPUT',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
         ENDDO
         DO K = 1,NPARB
            WRITE(TEXTH,900)K
            CALL GTDSCR('INPUTB',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
            IF (IERR.NE.0) HEADER = ' '
            NX = K + KSHXB
            WRITE(TEXTH,900)NX
            CALL PTDSCR('OUTPUT',TEXTH,'CHARACTER',IVAL,RVAL,HEADER,
     +                  IERR)
         ENDDO
      ENDIF
C
C TELL USER HOW MANY ENTRIES IN OUTPUT LIST
C
	IF (ILEVEL.GE.2.AND.VALID) THEN
         WRITE(TEXT,104)LSTLNC
	   IF(LSTLNC.EQ.1) TEXT(28:)='ENTRY'
  104    FORMAT('   OUTPUT LIST HAS ',I7,' ENTRIES')
         CALL LBGONE(TEXT(20:))
	   CALL WRUSER(' ',ISTAT)
         CALL WRUSER(TEXT,ISTAT)
	   CALL WRUSER(' ',ISTAT)
      ENDIF
C
C FREE ALL DATA AREAS AND RETURN
C
      CALL FRDATA(' ',ISTAT)
C
C
C
      END


C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C      **************
C      *            *
C      * S/R XYLOAD *
C      *            *
C      **************
C
C
C
C   PURPOSE
C    Loads an array with an XYlist of identifiers and zeroes
C
C   ARGUMENTS
C   IN/OUT
C         DATA      Real(NX,NY)      The array
C   IN
C         NX        Integer          The X length of the array
C         NY        Integer          The Y length of the array
C
C   CALLS
C     Edrs
C     LBGONE
C   USES
C    Equivalences Real and Character
C
C
C   A.J.PENNY                   RGO                    83-2-17
C
C -----------------------------------------------------------------
C
C
C
      SUBROUTINE XYLOAD(DATA,NX,NY)
C
C
C
      REAL DATA(NX,NY)
      REAL DTEXT(5)
      CHARACTER*20 TEXT
      EQUIVALENCE(DTEXT(1),TEXT)
C
C
C
      DO K = 1,NY
         DO J = 6,NX
            DATA(J,K) = 0.0
         ENDDO
      ENDDO
C
C
C
      DO K = 1,NY
         WRITE(TEXT,'(I20)')K
         TEXT(1:1) = '#'
         CALL LBGONE(TEXT(2:))
         DO J = 1,5
            DATA(J,K) = DTEXT(J)
         ENDDO
      ENDDO
C
C
C
      END



