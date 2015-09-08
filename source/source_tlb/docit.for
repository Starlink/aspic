C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   DOCIT *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               DOCIT   [ PROC ]
C
C
C          FUNCTION:-
C               This is the program which is run by the  procedure  CREDOC.
C               Its  function  is to create documentation which describes a
C               program or procedure. The output is ready to  be  formatted
C               by GEROFF.
C
C
C          USE:-
C               It is used as part of the set of three  procedures  CREDOC,
C               EDDOC  and  PUTDOC  which  together  create, edit and store
C               program documentation.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         PROG            PROGRAM             If it has the default  value,
C                                             the  program  assumes  that a
C                                             program is being  documented;
C                                             the only other possible value
C                                             is PROC when a  procedure  is
C                                             being processed.
C
C
C
C         K F Hartley              RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------



      CHARACTER*9 PROG
      CHARACTER*9 NAME
      CHARACTER*13 OUTFLE
      INTEGER TRULEN
C
C   A file is going to be created on logical unit 1
C
      LU=1
      PRINT *,'WHEN ASKED FOR A DESCRIPTION'
      PRINT *,'ANYWHERE IN THE FOLLOWING'
      PRINT *,'      ENTER AS MANY LINES AS YOU WISH'
      PRINT *,'      TERMINATING EACH WITH A CARRIAGE RETURN'
      PRINT *,'      AND ENDING WITH A BLANK LINE'
C
C   Firstly it picks up a value for the parameter PROG.
C   This will be either "PROG" from the connection file
C   or will have been overwritten by the command line.
C
C *********************
C Non-standard feature*
C *********************
C
      CALL RDKEYC('PROG',.FALSE.,1,PROG,IC,ISTAT)
C
C   Whatever value it gets it is set to an acceptable value.
C   NP holds the number of characters present in PROG
C
      IF (PROG(1:4).EQ.'PROG') THEN
         PROG='Program  '
         NP=7
      ELSE
         PROG='Procedure'
         NP=9
      END IF
C
C   Now prompt for the name of the program
C
      CALL RDKEYC('PROGNAME',.FALSE.,1,NAME,I,ISTAT)
      IF (ISTAT.NE.0) THEN
            NAME='                  '
      END IF
C
C   IC holds the number of significant characters in the name.
C
      IC=TRULEN(NAME)
C
C   The rest of the program is only executed if there were some
C   characters input as program name.
C
      IF (IC.GT.0) THEN
C
C   The output file to be created will have the same name as the
C   program and be of type .GRF
C **********************
C Non-standard feature *
C **********************
C
         OUTFLE=NAME//'.GRF'
         OPEN (UNIT=LU,NAME=OUTFLE,TYPE='UNKNOWN')
C
C   The first task is to set-up and output the information
C   at the top of the file.
C
         CALL DOC_HEADER(PROG,NP,NAME,IC,LU)
C
C   Next the user is prompted for an example of the way of invoking
C   the program.
C
         PRINT *,'CALLING SEQUENCE [...] FOR OPTIONAL PARAMETERS'
         WRITE (LU,*) 'CALLING SEQUENCE:-'
         CALL DOC_TEXT(LU)
         CALL DOC_NEWLIN(2,LU)
C
C   Then to describe what it does.
C
         PRINT *,'DESCRIBE ITS FUNCTION'
         WRITE (LU,*) 'FUNCTION:-'
         CALL DOC_TEXT(LU)
         CALL DOC_NEWLIN(2,LU)
C
C   and finally what it can be used for.
C
         PRINT *,'WHAT CAN IT BE USED FOR?'
         WRITE (LU,*) 'USE:-'
         CALL DOC_TEXT(LU)
         CALL DOC_NEWLIN(2,LU)
C
C   The next routine handles documentation of any parameters.
C
         CALL DOC_PAR(LU)
C
C   Then describe use of the ARGS trackerball buttons.
C
      CALL DOC_BUTTON(LU)
C
C   Finally it creates the last record containing Author,Site and Date.
C
         CALL DOC_TRAILER(LU)
         CLOSE (UNIT=LU)
      END IF
      END
      SUBROUTINE DOC_HEADER(TYPE,NT,NAME,NN,LU)
C
C      This subroutine creates and outputs a box of asterisks
C      surrounding a message contained in the input parameters.
C
C      Input parameters:-
C         TYPE      Character      Contains the descriptive string
C                                  for example PROGRAM or PROCEDURE
C         NT        Integer        The number of significant
C                                  characters in TYPE.
C         NAME      Character      Contains the name of the object
C                                  under discussion.
C         NN        Integer        The number of significant
C                                  characters in NAME.
C         LU        Integer        The logical unit to which output
C                                  is to be directed.
C
C      Written by K F Hartley at RGO on 8/12/81
C
      CHARACTER*(*) TYPE,NAME
C
C   The box may be no longer than 32 characters.
C
      CHARACTER*32 TEXT
      CHARACTER*65 TERM
      DO I=1,32
         TEXT(I:I)=' '
      END DO
C
C   NTOT is the total number of characters making up
C         * TYPE   NAME *
C   for example.
C
      NTOT=NT+NN+7
C
C   This write sends a message for GEROFF saying, leave
C   the next part of the output alone.
C
      DO I=1,65
        TERM(I:I)='+'
      END DO
      WRITE (LU,900) TERM
C
C   The first line is just asterisks
C
      DO I=1,NTOT
         TEXT(I:I)='*'
      END DO
      WRITE (LU,910) TEXT
C
C   Then a line of spaces with asterisks at the ends.
C
      DO I=2,NTOT-1
         TEXT(I:I)=' '
      END DO
      WRITE (LU,910) TEXT
C
C   Then comes the difficult one as shown above.
C
      TEXT(1:2)='* '
      DO I=3,NT+2
         K=I-2
         TEXT(I:I)=TYPE(K:K)
      END DO
      TEXT(NT+3:NT+5)='   '
      DO I=NT+6,NTOT-2
         K=I-NT-5
         TEXT(I:I)=NAME(K:K)
      END DO
      TEXT(NTOT-1:NTOT)=' *'
      WRITE (LU,910) TEXT
      DO I=1,NTOT
         TEXT(I:I)=' '
      END DO
      TEXT(1:1)='*'
      TEXT(NTOT:NTOT)='*'
      WRITE (LU,910) TEXT
C
C   Finally the first two lines are repeated (in reverse order)
C   to complete the box.
C
      DO I=2,NTOT-1
         TEXT(I:I)='*'
      END DO
      WRITE (LU,910) TEXT
C
C   Three blank lines are then output.
C
      CALL DOC_NEWLIN(3,LU)
  900 FORMAT ('.NF'/,A65///)
  910 FORMAT (12X,A32)
      END
      SUBROUTINE DOC_NEWLIN(N,LU)
C
C   This trivial subroutine simply outputs
C   a variable number of blank lines.
C
C   Input parameters:-
C      N      Integer      The number of blank lines to be output.
C      LU     Integer      The logical unit to which they are
C                          to be written.
C
C   Written by K F Hartley   at RGO   on 8/12/81
C
      DO I=1,N
         WRITE (LU,900)
      END DO
  900 FORMAT ('  ')
      END
      SUBROUTINE DOC_TEXT(LU)
C
C   This subroutine reads in lines of text until a blank
C   line is found, writing them so that GEROFF will left and right
C   justify them but using a TAB position as the left hand margin.
C
C   Input parameters:-
C      LU      Integer      The logical unit to which the
C                           output is to be sent.
C
C   Written by K F Hartley (and S J Keir)   at RGO   on 8/12/81
C
      CHARACTER*80 TEXT
      INTEGER TRULEN
C
C   Tell GEROFF to spread the text out to fill lines.
C
      WRITE (LU,900)
      KCOUNT=0
C
C   Loop around reading writing text
C
  100 CONTINUE
      READ (5,920) TEXT
      L=TRULEN(TEXT)
C
C   L is the number of significant characters in the line just input.
C   A value of zero therefore denotes an empty line.
C
      IF (L.GT.0) THEN
         IF (KCOUNT.EQ.0) THEN
C
C   To get nicely tabbed output, the first line of the paragraph
C   must have a TAB mark . KCOUNT is a flag for the first line.
C
            WRITE (LU,909) TEXT
            KCOUNT=1
         ELSE
            WRITE (LU,910) TEXT
         END IF
         GO TO 100
      END IF
  900 FORMAT ('.FI')
C
C   The ' ' is realy a TAB character as described above.
C
  909 FORMAT ('	',A80)
  910 FORMAT (A80)
  920 FORMAT (A80)
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
      SUBROUTINE DOC_PAR(LU)
C
C   This subroutine controls the handling of program parameters.
C
C   Input parameters:-
C      LU       Integer      The logical unit to which output
C                            is to be directed.
C
C   written by K F Hartley at RGO on 8/12/81
C
      CHARACTER*1 FNAME*44,FILE*40,LINE*80,CON*4,ACCESS*4,TYPE*8
      CHARACTER*16 PARAM(32),VALUE(32)
C
C   Note that only 32 parameters can be handled.
C
      LOGICAL DEFAULT
      INTEGER STAT,TRULEN
C
C   First a logical unit is set up for the (possible) connection file.
C
      LUCON=LU+1
C
C   Now pick up the name of a connection file.
C   This is needed because it may be in a different directory
C   from the current one.
C
      PRINT *,'ENTER NAME OF .CON FILE (NO FILE TYPE)'
      READ (*,900) FILE
C
C   A null response may indicate that the .CON file does not exist
C   for example because a procedure is being analysed.
C
      IF (TRULEN(FILE).EQ.0) GO TO 200
C
C   Now try to open the connection - again if there is an error
C   assume that it does not exist.
C
      FNAME=FILE//'.CON'
      OPEN (UNIT=LUCON,NAME=FNAME,TYPE='OLD',READONLY,ERR=200)
      NP=1
C
C   It now loops around reading the connection file.
C   All ERROR entries are ignored, otherwise a table is
C   created of parameter names and the corresponding
C   connection file default value, if present.
C   The last line forces a branch to the end (label=300).
C
  100 CONTINUE
C
C   Note maximum of 32 parameters.
C
      IF (NP.GT.32) GO TO 300
      PARAM(NP)='                '
      VALUE(NP)='                '
      READ (LUCON,910,ERR=200,END=300) LINE
C
C   This routine (by W F Lupton) analyzes a single connection file
C   entry and breaks it up into its component parts,
C
      CALL ANALYZE(LINE,PARAM(NP),TYPE,DEFAULT,VALUE(NP),ACCESS,
     :             STAT)
      IERR=INDEX(TYPE,'ERROR')
      IF (STAT.EQ.0.AND.IERR.EQ.0) THEN
         IF (.NOT.DEFAULT) THEN
C
C   The absence of a connection file default is signified
C   by four asterisks.
C
            VALUE(NP)='****'
         END IF
         NP=NP+1
      END IF
      GO TO 100
C
C   This marks the end of the connection file reading and
C   analysis loop.
C
C   Control passes to one or other of the two labels 200 or 300
C
  200 CONTINUE
C
C   This path is executed if anything has gone wrong with
C   reading the connection file.
C
      CALL DOC_NOPAR(LU)
      GO TO 400
  300 CONTINUE
C
C   This path is executed if a good table of parameter names
C   has been found.
C
      CALL DOC_HASPAR(LU,PARAM,VALUE)
  400 CONTINUE
      CLOSE (UNIT=LUCON)
  900 FORMAT (A40)
  910 FORMAT (A80)
      END
      SUBROUTINE DOC_NOPAR(LU)
C
C   This subroutine prompts the user for the names of parameters
C   in two lists - fistly the usual ones and then the ones which
C   are given values elsewhere and not normally prompted for.
C
C   Input parameters:-
C      LU      Integer      The logical unit to which output
C                           is to be written.
C
C   Written by K F Hartley at RGO on 9/12/81
C
      CHARACTER*1 PARAM*16,VALUE*16
      INTEGER TRULEN
C
C   First the "USER" parameters.
C
      WRITE (LU,900)
      PRINT *,'ENTER USER PARAMETERS IN ORDER'
  100 CONTINUE
      PRINT *,'PARAMETER'
      READ (*,910) PARAM
C
C   Note that a PARAM name of length 0 ie. a RETURN only
C   exits from this loop.
C
      IF (TRULEN(PARAM).EQ.0) GO TO 200
      PRINT *,'ENTER ANY RUN_TIME DEFAULT (UP TO 16 CHARACTERS)'
      READ (*,910) VALUE
      IF (TRULEN(VALUE).EQ.0) VALUE='                '
C
C   The parameter name and (optional) run-time default are
C   then passed on to be formatted and output with a description.
C
      CALL DOC_PARTXT(PARAM,VALUE,LU)
      GO TO 100
  200 CONTINUE
C
C   The same cycle is now repeated for those parameters
C   which are usually defaulted in the connection file.
C
      WRITE (LU,920)
      PRINT *,'NOW ENTER NORMALLY DEFAULTED PARAMETERS'
  300 CONTINUE
      PRINT *,'PARAMETER'
      READ (*,910) PARAM
      IF (TRULEN(PARAM).EQ.0) GO TO 400
      PRINT *,'DEFAULT VALUE? (UP TO 16 CHARACTERS)'
      READ (*,910) VALUE
      IF (TRULEN(VALUE).EQ.0) VALUE='                '
      CALL DOC_PARTXT(PARAM,VALUE,LU)
      GO TO 300
  400 CONTINUE
C
C   Note that the '.FI' ensures that GEROFF acts correctly.
C
  900 FORMAT ('.FI'/'USER PARAMETERS:-'/)
  910 FORMAT (A16)
  920 FORMAT (/'NORMALLY DEFAULTED PARAMETERS:-'/)
      END
      SUBROUTINE DOC_PARTXT(PARAM,VALUE,LU)
C
C   This subroutine prompts for the description of a parameter
C   and then outputs a suitable piece of text.
C
C   Input parameters:-
C      PARAM      Character      The name of the parameter under
C                                discussion.
C      VALUE      Character      This may contain a default value
C                                for this parameter (stored as a
C                                parameter string.
C
C      LU         Integer        The logical unit to which output
C                                is to be sent.
C
C   Written by K F Hartley 9/12/81
C
      CHARACTER*(*) PARAM,VALUE
      CHARACTER*1 TEXT*72,LINE*104,TAB*1
      DATA TAB/'	'/
      INTEGER TRULEN
C
C   The first line of the descriptrion is compulsory, and will
C   contain the name and value of the parameter as well as the first
C   part of its description.
C
      PRINT *,'DESCRIPTION?'
  100 CONTINUE
      READ (*,900) TEXT
      L=TRULEN(TEXT)
      IF (L.EQ.0) THEN
         PRINT *,'THERE MUST BE A DESCRIPTION'
         GO TO 100
      END IF
C
C   The full first line of text is prepared.
C   Note the insertion of a TAB character to ensure that all
C   subsequent lines are tabbed to the correct point.
C
      LINE=PARAM//VALUE//TAB//TEXT
      WRITE (LU,910) LINE
C
C   Now loop round copying more lines, until a blank line
C   is input.
C
      DO WHILE (L.NE.0)
         READ (*,900) TEXT
         L=TRULEN(TEXT)
         IF (L.NE.0) THEN
            LINE=TEXT
            WRITE (LU,910) LINE
         END IF
      END DO
C
C   Finally output a blank line.
C
      CALL DOC_NEWLIN(1,LU)
  900 FORMAT (A72)
  910 FORMAT (A104)
      END
      SUBROUTINE DOC_HASPAR(LU,PARAM,VALUE)
C
C   This routine takes tables of parameters and values
C   and generates suitable text.
C
C   Input parameters:-
C      LU         Integer         The logical unit to which
C                                 output is to be directed.
C      PARAM      Character array A set of up to 32 parameter
C                                 names.
C      VALUE      Character array A set of corresponding default
C                                 values (if present) or '****'
C                                 if not.
C
C      Written by K F Hartley at RGO on 9/12/81
C
      CHARACTER*(*) PARAM(32),VALUE(32)
      CHARACTER*16 DEFVAL
      INTEGER TRULEN
      WRITE (LU,900)
C
C   First loop through all the parameters which have no
C   default value (from the connection file) and ask for
C   any run-time default vale.
C
      DO I=1,32
         L=TRULEN(PARAM(I))
         IF (L.NE.0.AND.VALUE(I)(1:4).EQ.'****') THEN
            PRINT *,'PARAMETER ',PARAM(I)
            PRINT *,'ENTER ANY RUN-TIME DEFAULT (UP TO 16 CHARS)'
            READ (*,910) DEFVAL
C
C   Pass on the information for addition of descriptive text.
C
            CALL DOC_PARTXT(PARAM(I),DEFVAL,LU)
         END IF
      END DO
C
C   Now repeat for those parameters which have connection file
C   default values.
C
      WRITE (LU,920)
      DO I=1,32
         L=TRULEN(PARAM(I))
         IF (L.NE.0.AND.VALUE(I)(1:4).NE.'****') THEN
            PRINT *,'PARAMETER ',PARAM(I)
            PRINT *,'NORMAL DEFAULT',VALUE(I)
            CALL DOC_PARTXT(PARAM(I),VALUE(I),LU)
         END IF
      END DO
  900 FORMAT (/'USER PARAMETERS:-'/)
  910 FORMAT (A16)
  920 FORMAT (/'NORMALLY DEFAULTED PARAMETERS:-'/)
      END
      SUBROUTINE DOC_TRAILER(LU)
C
C   This routine creates a single line of text containing
C   the Author , Site and date.
C
      CHARACTER*1 AUTHOR*16,SITE*16,CDATE*9,TEXT*50
      CHARACTER*65 TERM
      PRINT *,'AUTHOR?'
      READ (*,900) AUTHOR
      PRINT *,'SITE?'
      READ (*,900) SITE
C
C   This picks up the current data in the format DD-MMM-YY
C **********************
C Non-standard feature *
C **********************
C
      CALL DATE(CDATE)
C

C   This rather complicated concatenation eventually produces
C   a string in the form
C      .TL /K F Hartley/RGO/ 9-DEC-81/
C   which makes sense to GEROFF as a 3-part title line.
C
      TEXT='.TL /'//AUTHOR//'/'//SITE//'/'//CDATE//'/'
      WRITE (LU,910) TEXT
      DO I=1,65
         TERM(I:I)='-'
      END DO
      WRITE (LU,920) TERM
  900 FORMAT (A16)
  910 FORMAT (//,A50,//)
  920 FORMAT (A65//)
      END
      SUBROUTINE DOC_BUTTON(LU)
      CHARACTER ANSWER*3,BUT*5,POSN*2
      PRINT *,'DOES IT USE THE BUTTONS?'
      READ (*,901) ANSWER
      IF (ANSWER(1:1).EQ.'Y') THEN
         WRITE (LU,900)
         BUT='GREEN'
         POSN=' 1'
         PRINT *,BUT,POSN
         CALL DOC_PARTXT(BUT,POSN,LU)
         BUT='WHITE'
         POSN=' 2'
         PRINT *,BUT,POSN
         CALL DOC_PARTXT(BUT,POSN,LU)
         POSN=' 3'
         PRINT *,BUT,POSN
         CALL DOC_PARTXT(BUT,POSN,LU)
         BUT='RED  '
         POSN=' 4'
         PRINT *,BUT,POSN
         CALL DOC_PARTXT(BUT,POSN,LU)
         WRITE (LU,910)
      END IF
  900 FORMAT (//'USE OF TRACKER-BALL BUTTONS:-'/)
  901 FORMAT (A3)
  910 FORMAT (//)
      END
