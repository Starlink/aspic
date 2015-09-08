        SUBROUTINE HAPDISP
C+
C         Subroutine to list commands available for displaying
C          the sky brightnesses derived by integrating the
C         apertures & to compute a final sky brightness.
C
C         Subroutine called; OUTPUT.
C
C         A C Davenhall./ROE/                                18/12/81.
C-
        INTEGER ISTAT
        CALL OUTPUT ('   ',ISTAT)
        CALL OUTPUT (' Commands available to determine sky brightness;',
     :                ISTAT)
        CALL OUTPUT ('   ',ISTAT)
        CALL OUTPUT (' PLOT (P)     - Plot points.',ISTAT)
        CALL OUTPUT (' PLOTALL (PA) - Plot points, inc. deleted ones.',
     :                ISTAT)
        CALL OUTPUT (' DELETE (D)   - Delete a specified point.',ISTAT)
        CALL OUTPUT (' REINSTATE (R)- Reinstate a specified points.',
     :                ISTAT)
        CALL OUTPUT (' LIST (L)     - List all apertures.',ISTAT)
        CALL OUTPUT (' MEAN (M)     - Find mean sky value.',ISTAT)
        CALL OUTPUT (' PRINT (PR)   - List all the apertures on the',
     :               ISTAT)
        CALL OUTPUT ('                lineprinter.',ISTAT)
        CALL OUTPUT (' HELP (H)     - List commands available.',ISTAT)
        CALL OUTPUT (' EXIT (E)     - Terminate this process.',ISTAT)
        CALL OUTPUT ('   ',ISTAT)
        END
