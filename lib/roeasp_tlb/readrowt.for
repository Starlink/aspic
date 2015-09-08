      SUBROUTINE READROWT (ROW,NROWS,NCOLS,TAB,TERMIN)

C+
C    READROWT
C
C	general subroutine to read a row from keyboard into table TAB of reals
C	part of general starlink table package
C	reads row into TAB(ROW,*) - deals with any no of cols up to MAXKEY
C	input row is free format reals separated by commas
C	which is read in with RDKEYC and converted with CTOR.
C	have to check that number of nos typed in = no. of cols
C	user types END to terminate input - signalled by TERMIN.
C
C    Given (arguments)
C	ROW (I)		row of table to be input
C	NROWS,NCOLS (I)	dimensions of TAB
C
C    Returned (arguments)
C	TAB (RA)	table of reals to which a row is read in
C	TERMIN (L)	.true. if end of input (END) and .false. otherwise
C
C    D. Tudhope/ROE/Feb 1983
C-

      INTEGER ROW,NROWS,NCOLS
      REAL TAB(NROWS,NCOLS)
      LOGICAL TERMIN
C*  maximum number of columns that will be read in - any more are ignored
      INTEGER MAXKEY
      PARAMETER (MAXKEY=32)
      CHARACTER*15 NEXTLINE(MAXKEY)
      INTEGER J,ISTATUS,NTYPED

      IF (NCOLS.GT.MAXKEY) THEN
C*  error in calling program's no. of cols - too many !!!!
        CALL WRERR('MISMAX')
      ELSE
C*  restart here if error in typing in line
100     ISTATUS=0
        CALL CNPAR('NEXTLINE',ISTATUS)
        CALL RDKEYC('NEXTLINE',.FALSE.,MAXKEY,NEXTLINE,NTYPED,ISTATUS)
        IF (ISTATUS.NE.0) THEN
          CALL WRERR('MISTYPE')
          GOTO 100
        ENDIF
        IF ((NTYPED.EQ.1) .AND.
     :     ((NEXTLINE(1).EQ.'end') .OR. (NEXTLINE(1).EQ.'END'))) THEN
          TERMIN=.TRUE.
        ELSE
          IF (NTYPED.NE.NCOLS) THEN
C*  too many nos. typed in
            CALL WRERR('MISCOL')
            GOTO 100
          ENDIF
          DO J=1,NCOLS
            CALL CTOR(NEXTLINE(J),TAB(ROW,J),ISTATUS)
            IF (ISTATUS.NE.0) THEN
C*  number mistyped
              CALL WRERR('MISNUM')
              GOTO 100
            ENDIF
          ENDDO
          TERMIN=.FALSE.
        ENDIF
      ENDIF
      END
