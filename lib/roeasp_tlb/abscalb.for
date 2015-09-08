        SUBROUTINE ABSCALB (A,IXEXT,IYEXT,PARMFIL,PARMSIZ)
C+
C       ABSCALB.
C
C         Menu driven routine for interactive absolute calibration
C         using multiaperture photometry.
C
C         Given;
C         A        R  Array holding image.
C         IXEXT    I  X size of image.
C         IYEXT    I  Y  "   "    "  .
C         PARMFIL  R  Array of image parameters.
C         PARMSIZ  I  Size of array PARMFIL.
C
C         Returned;
C         PARMFIL  R  The element holding the value for the absolute
C                     Sky brightness may have been modified.
C
C         Subroutines called;
C                    HABSCLB,ZENTRE,APERINT,APDISP.
C         Graphics;  SELDE1,CIRCLE,STOPLOT.
C         Args;      ARGS_OVCLR, ARGS_NUMIM, ARGS_PTOA.
C         I/O;       OUTPUT,READC,UPPCAS,INFILE2.
C
C         Options available;
C           - Find centre of galaxy semi-interactively.
C           - Enter file of P.E. measures.
C           - Display apertures.
C           - Compute luminosity in each aperture.
C           - Display & manipulate the results.
C           - List commands available.
C           - Terminate this process.
C
C         W D Pence./Univ of Sussex/                        Oct. 1980.
C         A C Davenhall./ROE/                                17/12/81.
C         A C Davenhall./ROE/ {Modified to use Args database} 13/2/83.
C-
        INTEGER PARMSIZ,IXEXT,IYEXT
        REAL A(IXEXT,IYEXT)
        REAL PARMFIL(50)
C
C         Arrays for handling the "catalogue" of photoelectric
C         measures.
C
        REAL DIA(50),SCALDIA(50),LOGDIA(50),MAGNIT(50),LUMIN(50),
     :             SKYBRI(50)
        LOGICAL DELMARK (50)
        INTEGER MAXAP,NAP
        PARAMETER (MAXAP=50)
C
        INTEGER X1,X2,Y1,Y2
C
        CHARACTER BUFF*80,PROMPT*8,OUTBUFF*80
        INTEGER ISTAT
        LOGICAL MORE,INAPER,INTGTD,ARGPLT
        REAL SKYCON
        REAL XCEN,YCEN,RADIUS,PIXSIZ
        REAL XEXT,YEXT
        INTEGER XPIX,YPIX,XARG,YARG,RADPIX,RADARG,DUMMY,IMGNO,PLTSTT
C
C        Variables to control line graphics plotted to the Args.
C
        INTEGER PLANE
        PARAMETER (PLANE=15)
        CHARACTER COLOUR*1
        PARAMETER (COLOUR='Y')
C
C
C         Work out the size of each pixel in arcsec.
C
        PIXSIZ=PARMFIL(1)*PARMFIL(3)/1.0E3
        IF (ABS((PARMFIL(1)-PARMFIL(2))/(PARMFIL(1)+PARMFIL(2))).GE.
     :                1.0E-4) THEN
          CALL OUTPUT (' ***ERROR***; Non-square pixels.',ISTAT)
          CALL OUTPUT ('    Apertures will be distorted,',ISTAT)
          CALL OUTPUT ('    & results will be wrong.',ISTAT)
        END IF
C
C         Print out help subroutine.
C
        CALL OUTPUT ('  ',ISTAT)
        CALL OUTPUT (
     : ' Absolute Calibration by Multiaperture Photometry.',ISTAT)
        CALL OUTPUT ('  ',ISTAT)
        CALL HABSCLB
C
C         Set up for menu driven running & set defaults.
C
        INAPER=.FALSE.
        INTGTD=.FALSE.
        XCEN=FLOAT(IXEXT/2)
        YCEN=FLOAT(IYEXT/2)
        ARGPLT=.FALSE.
        MORE=.TRUE.
        DO WHILE (MORE)
          BUFF='  '
          ISTAT=0
          CALL READC ('COMMAND',' Enter command:-',
     :                '  ','  ','~',BUFF,ISTAT)
          CALL UPPCAS (BUFF)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Obtain the centre of the galaxy semi-interactively.
C
          IF (BUFF.EQ.'CENTRE'.OR.BUFF.EQ.'C') THEN
            CALL ZENTRE (A,IXEXT,IYEXT,XCEN,YCEN)
            CALL MARPOS (IXEXT,IYEXT,PLANE,COLOUR,XCEN,YCEN)
            ARGPLT=.TRUE.
            WRITE(OUTBUFF,2002) XCEN,YCEN
 2002       FORMAT(1X,'Coords of Centre;  X =',0PF6.1,2X,'Y = ',F6.1)
            CALL OUTPUT (OUTBUFF,ISTAT)
          END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Obtain a VAX VMS file that contains the p.e. measures.
C
          IF (BUFF.EQ.'INFILE'.OR.BUFF.EQ.'INF') THEN
            CALL INFILE2 (
     :      ' Enter name of file containing P.E. measures;',
     :                MAXAP,DIA,MAGNIT,NAP)
            DO I=1,NAP
              SCALDIA(I)=DIA(I)/PIXSIZ
              LOGDIA(I)=ALOG10(DIA(I))
            END DO
            INAPER=.TRUE.
            WRITE(OUTBUFF,2001) NAP
 2001       FORMAT(1X,I2,' Photoelectric apertures input.')
            CALL OUTPUT (OUTBUFF,ISTAT)
          END IF
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Display the apertures on the ARGS & print a message if one
C         of them exceeds the boundary.
C
          IF (BUFF.EQ.'DISPLAY'.OR.BUFF.EQ.'D') THEN
            IF (INAPER) THEN
              CALL SELDE1 (2,.FALSE.,PLANE,COLOUR)
              XEXT=FLOAT(IXEXT)
              YEXT=FLOAT(IYEXT)
              CALL WINDOL (0.0E0,5.11E2,0.0E0,5.11E2)
              DO I=1,NAP
                RADIUS=SCALDIA(I)/2.0E0
C         Check whether the aperture exceeds the array.
C
                X1=IFIX(XCEN-RADIUS)
                    Y1=IFIX(YCEN-RADIUS)
                X2=IFIX(XCEN+RADIUS)
                Y2=IFIX(YCEN+RADIUS)
                IF (X1.LT.1.OR.Y1.LT.1.OR.X2.GT.IXEXT.OR.Y2.GT.IYEXT) 
     :                        THEN
                  WRITE(OUTBUFF,2000) I
 2000 FORMAT(1X,'*WARNING* Aperture no. ',I2,' exceeds picture size.')
                  CALL OUTPUT (OUTBUFF,ISTAT)
                END IF
C
C          Convert from Pixel to Args coordinates.
C
                CALL ARGS_NUMIM (IMGNO)
                XPIX=NINT(XCEN)
                YPIX=NINT(YCEN)
                CALL ARGS_PTOA (IMGNO,XPIX,YPIX,XARG,YARG,PLTSTT)
                RADPIX=NINT(XCEN+RADIUS)
                CALL ARGS_PTOA (IMGNO,RADPIX,DUMMY,RADARG,DUMMY,PLTSTT)
                RADARG=RADARG-XARG
C
C          Plot the aperture.
C
                CALL CIRCLE (FLOAT(XARG),FLOAT(YARG),FLOAT(RADARG))
              END DO
              CALL STOPLOT
              ARGPLT=.TRUE.
            ELSE
              CALL OUTPUT (' Photoelectric measures not yet input.',
     :                ISTAT)
            END IF
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Compute the intensities inside each aperture.
C
          IF (BUFF.EQ.'INTEGRATE'.OR.BUFF.EQ.'I') THEN
            IF (INAPER) THEN
              CALL APERINT (XCEN,YCEN,SCALDIA,MAGNIT,MAXAP,NAP,PIXSIZ,
     :                                 A,IXEXT,IYEXT,LUMIN,SKYBRI)
              INTGTD=.TRUE.
            ELSE
              CALL OUTPUT (' Photoelectric measures not yet input.',
     :                        ISTAT)
            END IF
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Display the results & select a final value for the sky 
C         brightness.
C
          IF (BUFF.EQ.'RESULTS'.OR.BUFF.EQ.'R') THEN
            IF (INTGTD) THEN
              CALL APDISP (DIA,LOGDIA,SKYBRI,MAXAP,NAP,DELMARK,SKYCON)
              PARMFIL(4)=SKYCON
            ELSE
              CALL OUTPUT (' Apertures not yet integrated.',ISTAT)
            END IF
          END IF
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Print list of commands.
C
          IF (BUFF.EQ.'HELP'.OR.BUFF.EQ.'H') CALL HABSCLB
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C         Exit.
C
          IF (BUFF.EQ.'EXIT'.OR.BUFF.EQ.'E') MORE=.FALSE.
        END DO
C
C         Clear any line graphics from the Args and terminate any
C         Fings graphics that may still be active (as a precaution).
C
        CALL ARGS_OVCLR (PLANE)
        CALL STOPLOT
        END
