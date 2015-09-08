      SUBROUTINE PRIAXE
C+
C     PRIAXE.
C
C     Subroutine to extract the principle (ie. major and minor)
C     axes from a nebular image. The axes are defined interactively,
C     using an appropriately shaped cursor.
C
C  Given;
C   None.
C
C  Returned;
C   None.
C
C  Subroutines called;
C   E2D:-          PRIAXH, ZENTRE, MARPOS, MAJMIN, PLOPRO,
C                  SVPROF, SELCOL.
C   Interfaces:-  INPICR, READC, OUTPUT, READR, CLEARIM.
C   Args:-         SRINIT, ARGCUR, ARGS_OVCLR, ARGS_PICWRT.
C
C  A C Davenhall./ROE/                                   6/7/82.
C-
      INTEGER IFAIL,STATUS,IXEXT,IYEXT,IMPTR,STAT,AXSTAT
      INTEGER NAXIS(2)
      REAL XCEN,YCEN
      REAL AXIS(1024,4),WORK1(1024),WORK2(1024)
      INTEGER AXISIZ
      PARAMETER (AXISIZ=1024)
      INTEGER PTS(4)
C
      INTEGER PLANE
C
C    Overlay plane to be used in plotting graphics to the Args.
C
      PARAMETER (PLANE=10)
C
      CHARACTER COLOUR*1,REPLY*10,OUTBUFF*80
      REAL PIXSIZ
      LOGICAL CENFIN,EXTRAC,MORE
      INTEGER ARM
C
C
C    Intialise the Args.
C
      CALL SRINIT (0,.FALSE.,IFAIL)
C
C    Attempt to obtain a pointer to the Starlink image.
C
      STATUS=0
      CALL INPICR ('INPIC1',
     :  ' Enter filename for the input image;',
     :    2,NAXIS,IMPTR,STATUS)
      IXEXT=NAXIS(1)
      IYEXT=NAXIS(2)
C
C    Proceed if an image pointer has been obtained successfully.
C
      IF (STATUS.EQ.0) THEN
C
C   Setup for using a command interpreter.
C
C   Default cursor colour.
C
        COLOUR='Y'
C
C   Default pixel size.
C
        PIXSIZ=1.0E0
C
C   Set logical flag saying that the centre of the galaxy
C   image has not yet been found.
C
        CENFIN=.FALSE.
C
C   Set a logical flag saying that the axes have not yet been
C   extracted.
C
        EXTRAC=.FALSE.
C
C    List the commands available.
C
        CALL PRIAXH
C
        MORE=.TRUE.
        DO WHILE (MORE)
C
C   Obtain command string.
C
          CALL READC ('COMMAND',
     :   ' Enter command;','  ','  ','~',REPLY,STAT)
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C   Locate the centre automatically.
C
          IF (REPLY.EQ.'CENTRE'.OR.REPLY.EQ.'C') THEN
            CALL ZENTRE (%VAL(IMPTR),IXEXT,IYEXT,XCEN,YCEN)
            CALL MARPOS (IXEXT,IYEXT,PLANE,COLOUR,XCEN,YCEN)
            WRITE(OUTBUFF,2000) XCEN,YCEN
 2000       FORMAT(1X,'Coords. of centre; X = ',0PF6.1,2X,
     :             'Y = ',F6.1)
            CALL OUTPUT (OUTBUFF,STAT)
            CENFIN=.TRUE.
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Locate the centre manually.
C
          IF (REPLY.EQ.'MANCEN'.OR.REPLY.EQ.'M') THEN
            CALL ARGCUR (XCEN,YCEN)
            CALL MARPOS (IXEXT,IYEXT,PLANE,COLOUR,XCEN,YCEN)
            WRITE(OUTBUFF,2000) XCEN,YCEN
            CALL OUTPUT (OUTBUFF,STAT)
            CENFIN=.TRUE.
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Enter pixel size
C
          IF (REPLY.EQ.'PIXSIZ'.OR.REPLY.EQ.'PI')
     :      CALL READR ('PIXSIZ',' Enter pixel size;',
     :            1.0E0,1.0E-9,1.0E6,PIXSIZ,STAT)
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Extract the Axes.
C
          IF (REPLY.EQ.'AXES'.OR.REPLY.EQ.'A') THEN
            IF (CENFIN) THEN
                CALL MAJMIN (%VAL(IMPTR),IXEXT,IYEXT,PLANE,   COLOUR,
     :                         XCEN,YCEN,AXIS,AXSIZ,PTS,AXSTAT)
                IF (AXSTAT.EQ.0) THEN
                  EXTRAC=.TRUE.
                  CALL OUTPUT (' Axes extracted Ok.',STAT)
                ELSE
                  CALL OUTPUT (' Axes not extracted.',STAT)
                END IF
            ELSE
                CALL OUTPUT (' ***ERROR Galaxy centre not defined.',
     :                           STAT)
            END IF
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Plot a selected axis.
C
          IF (REPLY.EQ.'PLOT'.OR.REPLY.EQ.'P') THEN
            IF (EXTRAC) THEN
                CALL PLOPRO (AXIS,AXISIZ,PTS,PIXSIZ,PLANE,COLOUR,
     :                       WORK1,WORK2)
            ELSE
                CALL OUTPUT (' ***ERROR Axes not yet extracted.',STAT)
            END IF
          END IF
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Save a selected axis as a Starlink image.
C
          IF (REPLY.EQ.'SAVE'.OR.REPLY.EQ.'S') THEN
            IF (EXTRAC) THEN
                CALL SVPROF (AXIS,AXISIZ,PTS,PIXSIZ)
            ELSE
                CALL OUTPUT (' ***ERROR Axes not yet extracted.',STAT)
            END IF
          END IF
C
C::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Change the colour of the cursor.
C
          IF (REPLY.EQ.'COLOUR'.OR.REPLY.EQ.'CO')
     :      CALL SELCOL (COLOUR,STAT)
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    List the commands available.
C
          IF (REPLY.EQ.'HELP'.OR.REPLY.EQ.'H') CALL PRIAXH
C
C:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
C
C    Exit from the command loop.
C
          IF (REPLY.EQ.'EXIT'.OR.REPLY.EQ.'E') MORE=.FALSE.
        END DO
      ELSE
C
C    Unable to obtain an image pointer.
C
        CALL OUTPUT (
     :   ' ***ERROR Unable to obtain image correctly.',STAT)
      END IF
C
C    Tidy up the image frame input.
C
      CALL CLEARIM ('INPIC1')
C
C    Clear the cursor plane and re-enable the picture planes.
C
      CALL ARGS_OVCLR (PLANE)
      CALL ARGS_PICWRT
      END
