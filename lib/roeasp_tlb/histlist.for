      SUBROUTINE HISTLIST                                                       
C+
C     HISTLIST
C
C     Lists instructions for histogram plot to terminal.
C
C     CALLS:
C     WRUSER      :STARLINK
C
C	B.D KELLY/ROE/1981
C-
      CALL WRUSER (' Histogram Plot.',ISTS)
      CALL WRUSER(' ',ISTS)
      CALL WRUSER (' P      Plot.',ISTS)
      CALL WRUSER (' C      Set X-scale using the cursor.',ISTS)
      CALL WRUSER (' S      Enter new X-scale from the keyboard.',ISTS)
      CALL WRUSER (' E      Exit.',ISTS)
      CALL WRUSER(' ',ISTS)
      CALL WRUSER(' ',ISTS)
      END
