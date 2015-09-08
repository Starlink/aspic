        SUBROUTINE INPLT (ICST,IRST,PLTSCL,ISTAT)
C+
C         Subroutine to obtain the X & Y Stepsizes of the
C         current raster scan & the platescale from the
C         Starlink environment.
C
C         Given;
C         ISTAT - Input status.
C
C         Returned;
C         ICST - Stepsize in Y (columns) in microns.
C         IRST -    "     "  X (rows)    "    "    .
C         PLTSCL - Plate scale in Arcsec/mm.
C         ISTAT - Return status = max(input status, internal status).
C
C         Subroutines called;
C         READI, READR, YESNO.
C
C         A C Davenhall. /ROE/                                11/11/81.
C-
        INTEGER ICST,IRST,ISTAT
        REAL PLTSCL
        INTEGER STAT1,STAT2,STAT3,STAT4
        LOGICAL OK
        CHARACTER REPLY*1
        OK=.FALSE.
        DO WHILE (.NOT.OK)
          STAT1=0
          STAT2=0
          STAT3=0
          STAT4=0
          CALL READI ('ICST',
     :     ' Stepsize in Y (columns) in microns;',
     :     5,1,100,ICST,STAT1)
          CALL READI ('IRST',
     :     ' Stepsize in X (rows) in microns;',
     :     5,1,100,IRST,STAT2)
          CALL READR ('PLTSCL',
     :     ' Enter plate scale (Arcsec/mm);',
     :     67.13,0.1,1000.0,PLTSCL,STAT3)
          CALL YESNO (' Are these values ok?','Y',
     :     REPLY,STAT4)
          IF (REPLY.EQ.'Y') OK=.TRUE.
        END DO
        ISTAT=MAX(ISTAT,STAT1,STAT2,STAT3,STAT4)
        END
