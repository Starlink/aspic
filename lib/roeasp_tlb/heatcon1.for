       SUBROUTINE HEATCON1 (NCOL,NLEVEL,COLOUR)
C+
C      HEATCON1.
C
C      Subroutine to generate a continuous pseudo "heat sequence"
C      colour table.
C
C  Given;
C   NCOL   (I)  No. of colour guns.
C   NLEVEL (I)  No. of intensity levels for each gun.
C 
C  Returned;
C   COLOUR (IA) Colour table evaluated at each point for each gun.
C
C  Subroutines called;
C   E2D:-  SETCON.
C
C  A C Davenhall./ROE/                                       26/5/82.
C  A C Davenhall./ROE/   {Modified}                          26/10/82.
C-
       INTEGER NCOL,NLEVEL
       INTEGER COLOUR(NCOL,NLEVEL)
C
C
C       Initialise the entire colour table to black.
C
       DO I=1,NLEVEL
         DO J=1,NCOL
           COLOUR(J,I)=0
         END DO
       END DO
C
C       Setup the colour table.
C
C       Violet.
C
       CALL SETCON (60,50,255,0,255,COLOUR)
C
C       Red.
C
       CALL SETCON (105,50,255,0,0,COLOUR)
C
C       Orange.
C
       CALL SETCON (151,50,255,128,0,COLOUR)
C
C       Yellow.
C
       CALL SETCON (200,50,255,255,0,COLOUR)
C
C       Yellow/white.
C
       CALL SETCON (222,30,255,255,128,COLOUR)
C
C       White.
C
       CALL SETCON (256,30,255,255,255,COLOUR)
C
       END
