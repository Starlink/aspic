      PROGRAM STUFF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   STUFF *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ***** Non-Starlink Program  Only used within PUTDOC *****
C
C
C          FUNCTION:-
C               It takes a documentation file prog.DOC and inserts it  into
C               a program source prog.FOR and also removes the + and - from
C               any C+ etc. comments.
C
C
C          USE:-
C               It is called by the DSCL procedure and should not  be  used
C               in any other way.
C
C
C         K F Hartley              RGO                            13-JAN-82
C
C
C--------------------------------------------------------------------------



C
C   Non-Starlink program which reads Fortran source from logical unit 1
C   and documentation from logical unit 2
C   and merges them together, writing the results to logical unit 3.
C
      CHARACTER*80 SRCTXT,CH*1
      INTEGER TRULEN
C
C   The open statement is required so that the carriage control
C   parameter may be set to, generate compilable output.
C
      OPEN (UNIT=3,FILE='STUFFOUT',CARRIAGECONTROL='LIST',STATUS='NEW')
C
C   The first line is read to check if it is a PRPGRAM statement.
C
      READ (1,900) SRCTXT
      L=TRULEN(SRCTXT)
  900 FORMAT(A80)
      IPOS=INDEX(SRCTXT,'PROGRAM')
      IF (IPOS.NE.0.AND.SRCTXT(1:1).EQ.' ') THEN
C
C   If it is then it is copied out followed by the documentation.
C
         WRITE (3,910) SRCTXT(1:L)
         CALL COPYDOC
      ELSE
C
C   Otherwise the documentation is copied out first.
C
         CALL COPYDOC
C
C   In case the first line was a "C+" comment the plus is replaced.
C
         L1=INDEX(SRCTXT,'+')
         IF (L1.NE.0) THEN
            SRCTXT(L1:L1)=' '
         END IF
         WRITE (3,910) SRCTXT(1:L)
      END IF
C
C   The remainder of the SOURCE is Now copied out.
C   Any C+ or *+ lines have the + replaced by a space.
C
  100 CONTINUE
      READ (1,900,ERR=200,END=200) SRCTXT
      CH=SRCTXT(1:1)
      IF (CH.EQ.'C'.OR.CH.EQ.'*') THEN
         SRCTXT(2:2)=' '
      END IF
      L=TRULEN(SRCTXT)
      WRITE (3,910) SRCTXT(1:L)
      GO TO 100
  200 CONTINUE
  910 FORMAT (A)
      END
      SUBROUTINE COPYDOC
C
C   This subroutine copies documentation from LU 1 to 3.
C   Each line is made into a comment .
C
      CHARACTER*80 DOCTXT
      INTEGER TRULEN
  100 CONTINUE
      READ (2,900,ERR=100,END=200) DOCTXT
      L=TRULEN(DOCTXT)
      DOCTXT(1:1)='C'
C
C   GEROFF gives a margin of 15 spaces (by default)
C   The first and last lines of documentation have these spaces
C   replaced by +++ or --- as required.
C
      IF (INDEX(DOCTXT,'++++++').NE.0) DOCTXT(2:15)='++++++++++++++'
      IF (INDEX(DOCTXT,'------').NE.0) DOCTXT(2:15)='--------------'
      WRITE (3,920) DOCTXT(1:L+1)
C
C   A line of ------ marKS the useful end of the documentation
C   so its existence forces a break out of the loop.
C
      L=INDEX(DOCTXT,'--------')
      IF (L.EQ.0) THEN
         GO TO 100
      END IF
  200 CONTINUE
      WRITE (3,930)
  900 FORMAT (A80)
  920 FORMAT (A)
  930 FORMAT (//)
      END
      INTEGER FUNCTION TRULEN(STRING)
C
C   This function returns as its value the significant
C   length of STRING.
C
C   Input parameters :-
C      STRING   Character   The string whose length is required.
C
C   Returned:-
C      TRULEN   Integer     The length of the string.
C
C   Written by K F Hartley at RGO on 8/12/81
C
      CHARACTER*(*) STRING
      CHARACTER*1 NULL
      DATA NULL/'0'X/
C
C   It is assumed that STRING is full of a sequence of significant
C   characters, followed by a sequence of spaces and/or null characters
C
      L=LEN(STRING)
C
C   L now holds the total length of STRING
C
  100 CONTINUE
      IF (L.GT.0) THEN
         IF (STRING(L:L).EQ.' '.OR.STRING(L:L).EQ.NULL) THEN
            L=L-1
            GO TO 100
         END IF
         TRULEN=L
      ELSE
         TRULEN=0
      END IF
      END
