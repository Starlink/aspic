      SUBROUTINE TYPLO2 (TYPE,CHAR)
C+
C       TYPLO2.
C
C       Subroutine to select the manner of plotting an array of data
C       points: plot the individual points as seperate symbols,
C       join the points with straight lines or join the points
C       with "histograms" ie. with vertical & horizontal lines.
C
C  Given;
C   None.
C
C  Returned;
C   TYPE  (I)  Type of plotting;
C              = 1 - Data plotted as points.
C              = 2 - Data joined with straight lines.
C              = 3 - Data joined with a "histogram".
C   CHAR  (I)  Defines the plotting symbol used if points
C              are selected. Valid range is 0 - 8.
C
C
C  Subroutines called;
C   Interfaces:- MULREP, OUTPUT, YESNO.
C
C  A C Davenhall./ROE/                                     6/8/82.
C-
      INTEGER TYPE,CHAR
      INTEGER NPRMPT,ISTAT
      CHARACTER BUFF*80,PRMPTS*3,REPLY*1
 2000 FORMAT(1X,'Data to be plotted as points, lines or ',
     :     '"histograms"?')
 2001 FORMAT(1X,'Is help required in selecting plotting symbol?')
 2002 FORMAT(1X,' ')
 2003 FORMAT(1X,'There are 9 valid plotting symbols (0-8):-')
 2004 FORMAT(3X,'0 - A dot.')
 2005 FORMAT(3X,'1 - A triangle - apex uppermost.')
 2006 FORMAT(3X,'2 - A triangle - base uppermost.')
 2007 FORMAT(3X,'3 - An uppright cross.')
 2008 FORMAT(3X,'4 - A cross of St Andrew.')
 2009 FORMAT(3X,'5 - A square.')
 2010 FORMAT(3X,'6 - A diamond.')
 2011 FORMAT(3X,'7 - A sort of vaguely circular blob.')
 2012 FORMAT(3X,'8 - An asterisk.')
 2013 FORMAT(1X,'Enter value for required plotting symbol ',
     :     '{0-8};')
      WRITE(BUFF,2000)
      ISTAT=0
      CALL MULREP (BUFF,'POINTS,P,LINES,L,HISTOGRAMS,H$',REPLY,
     :        ISTAT)
      IF (REPLY.EQ.'L'.OR.REPLY.EQ.'LINES')      TYPE=2
      IF (REPLY.EQ.'H'.OR.REPLY.EQ.'HISTOGRAMS') TYPE=3
      IF (REPLY.EQ.'P'.OR.REPLY.EQ.'POINTS') THEN
        TYPE=1
        CHAR=8
        WRITE(BUFF,2001)
        ISTAT=0
        CALL YESNO (BUFF,'N',REPLY,ISTAT)
        IF (REPLY.EQ.'Y') THEN
          WRITE(BUFF,2002)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2003)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2002)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2004)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2005)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2006)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2007)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2008)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2009)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2010)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2011)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2012)
          CALL OUTPUT (BUFF,ISTAT)
          WRITE(BUFF,2002)
          CALL OUTPUT (BUFF,ISTAT)
        END IF
        WRITE(BUFF,2013)
        CALL READI ('CHAR',BUFF,4,0,8,CHAR,ISTAT)
      END IF
      END
