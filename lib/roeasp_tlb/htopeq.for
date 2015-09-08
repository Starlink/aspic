        SUBROUTINE HTOPEQ
C+
C         Help subroutine for TOPEQ - List the
C         available commands.
C
C         Subroutine called; OUTPUT.
C
C         A C Davenhall. /ROE/                                10/12/81.
C-
        INTEGER IST
        CALL OUTPUT ('  ',IST)
        CALL OUTPUT (' Commands available are;',IST)
        CALL OUTPUT ('  ',IST)
        CALL OUTPUT (' EXTRACT (EX)  - Extract equivalent profile',IST)
        CALL OUTPUT ('                 from current image.',IST)
        CALL OUTPUT (' EXTRACTC (EC) - Extract equivalent profile',IST)
        CALL OUTPUT ('                 with correction for sky noise.',
     :                 IST)
        CALL OUTPUT (' INTABLE (I)   - Compute integration table',IST)
        CALL OUTPUT ('                 from current profile.',IST)
        CALL OUTPUT (' INFILE (INF)  - Input a profile from a',IST)
        CALL OUTPUT ('                 VAX VMS file.',IST)
        CALL OUTPUT (' CHANGE (C)    - Change scan parameters.',IST)
        CALL OUTPUT (' SAVE (S)      - Save the equivalent profile ',
     :                 IST)
        CALL OUTPUT ('                 as a Starlink image.',IST)
        CALL OUTPUT (' HELP (H)      - List commands available.',IST)
        CALL OUTPUT (' EXIT (E)      - Leave this node.',IST)
        CALL OUTPUT ('  ',IST)
        END
