      SUBROUTINE COLCY1 (CYCLE,NCOL,NLEVEL,INCOL,OUTCOL)
C+
C     COLCY1.
C
C     Subroutine to take an input colour table and construct from
C     it an output colour table consisting of a number of cyclic
C     replicas of the original table.
C
C  Given;
C   CYCLE  (I)  No. of replicas of the original table to appear
C               in the output table.
C   NCOL   (I)  No. of colour guns.
C   NLEVEL (I)  No. of intensity levels for each gun.
C   INCOL  (IA) Input colour table.
C
C  Given;
C   OUTCOL (IA) Output colour table.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                   26/10/82.
C-
      INTEGER CYCLE,NCOL,NLEVEL
      INTEGER INCOL(NCOL,NLEVEL),OUTCOL(NCOL,NLEVEL)
C
      INTEGER COL,PCYCLE,NEWCOL,PT,INDEX
C
C    Compute the no. of points in each cycle of the output table.
C
      PCYCLE=NLEVEL/CYCLE
C
C    Generate new colour table sequentially for each gun.
C
      DO COL=1,NCOL
C
C    Generate the new colour for each point in a cycle of the new
C    colour table.
C
        DO PT=1,PCYCLE
C
C    Average the original colour table.
C
          NEWCOL=0
          DO K=1,CYCLE
            INDEX=((PT-1)*CYCLE)+K
            NEWCOL=NEWCOL+INCOL(COL,INDEX)
          END DO
          NEWCOL=NEWCOL/CYCLE
C
C    Place this colour in each of the cycles of the output colour
C    table.
C
          DO K=1,CYCLE
            INDEX=PT+((K-1)*PCYCLE)
            OUTCOL(COL,INDEX)=NEWCOL
          END DO
        END DO
C
C    Pad out any points left over at the top of the colour table.
C
        DO K=PCYCLE*CYCLE,NLEVEL
          OUTCOL(COL,K)=INCOL(COL,NLEVEL)
        END DO
      END DO
      END
