      SUBROUTINE PRPLTS (NXST,NXFN,NYST,NYFN,NAXT,TPOL,
     :                   NAXA,APOL,NPLT,PLTOT,PLANG)
*+
*
*    PRPLTS
*
*    this routine initializes the requested graphics device
*    and sets up the plotting area for plotting the 
*    polarization vectors .
*
*    Parameters :
*
*    Given     Type    Usage
*    NXST       I      Leftmost pixel to be plotted
*    NXFN       I      Rightmost pixel to be plotted
*    NYST       I      Lowest pixel to be plotted
*    NYFN       I      Highest pixel to be plotted
*    NAXT       IA     Dimensions of the total polarization array
*    TPOL       RA     Total polarization data
*    NAXT       IA     Dimensions of the polarization angle array
*    APOL       RA     Polarization angle array.
*    NPLT       IA     Dimensions of the polarization data workspace arrays
*    PLTOT      RA     Total polarization workspace array
*    PLANG      RA     Polarization angle workspace array
*
*    Subroutines called :
*    VERSA , T4010 , T4014 , PICCLE , VUPORT   : ROEFINGS
*    PENSEL , ARGS , WINDOL , DEVEND           : ROEFINGS
*    SRINIT                                    : ARGSLIB
*    MULREP , AXIS , POLPLT                    : E2DASP
*    READR                                     : ASPFACE
*
*    D.W.T.Baines/ROE/Feb 1983/
*
*-
      INTEGER NAXA(2) , NAXT(2) , NPLT(2)
      INTEGER NXST , NYST , NXFN , NYFN
      INTEGER IST , ISTAT , IFAIL
      REAL TPOL(NAXA(1),NAXA(2)) , APOL(NAXT(1),NAXT(2))
      REAL PLTOT(NPLT(1),NPLT(2)) , PLANG(NPLT(1),NPLT(2))
      REAL VECSCL , RXS , RXF , RYS , RYF , XINC , YINC
      CHARACTER*8 DEVICE
      CHARACTER*1 OVER
      LOGICAL CLEAR
*
*    set all status variables to 0
*
      IST   = 0
      ISTAT = 0
      IFAIL = 0
*
*    set the overlay flag to 'N' for no overlaying
*    and the "clear ARGS" flag to .TRUE.
*
      OVER = 'N'
      CLEAR = .TRUE.
*
*    request plotting device 
*
      CALL MULREP('Which plotting device for vectors ?',
     :            'ARGS,VERSATEK,T4010,T4014$',DEVICE,IST)
*
*    request the scale factor for the polarization vectors
*
      CALL READR('VECLEN','Give scale factor for vector length',
     :           1.0,0.1,10.0,VECSCL,IST)
      IF(DEVICE.EQ.'ARGS') THEN
*
*      find out if the diagram is to be overlayed
*
         CALL YESNO('Diagram to be overlayed on ARGS image ? Y/N',
     :              'N',OVER,ISTAT)
         CALL UPPCAS( OVER )
*
*      if the diagram is to be overlayed then dont want to clear the ARGS
*
         IF( OVER .EQ. 'Y' ) THEN
            CLEAR = .FALSE.
         ENDIF
*
*       initialise the args  
*
         CALL SRINIT(0,CLEAR,IFAIL)
*
*       if IFAIL is 0 then continue , otherwise end
*
         IF ( IFAIL .EQ. 0 ) THEN
            CALL ARGS
            CALL PENSEL( 1 )
         ELSE
*
*          here on an error initialising ARGS
*
            CALL WRUSER ('Error allocating the ARGS',ISTAT)
         ENDIF
      ELSE
         IF ( DEVICE .EQ. 'VERSATEK' ) THEN
*
*          here if versatek
*
            CALL VERSA
         ELSEIF ( DEVICE .EQ. 'T4010' ) THEN
*
*          here if tektronix T4010
*
            CALL T4010
         ELSEIF ( DEVICE .EQ. 'T4014' ) THEN
*
*          here if tektronix T4014
*
            CALL T4014
         ENDIF
      ENDIF
*
*    check IFAIL again for error allocating the ARGS
*
      IF ( IFAIL .EQ. 0 ) THEN
*
*       RXS , RXF , RYS and RYF are the real values of the plot limits
*
         RXS = REAL ( NXST )
         RXF = REAL ( NXFN )
         RYS = REAL ( NYST )
         RYF = REAL ( NYFN )
*
*       calculate the offsets in X and Y which have to be added to
*       or subtracted from the limits to put the plot in the central
*       area of the screen
*
         XINC = ( RXF - RXS + 1.0 ) *0.3
         YINC = ( RYF - RYS + 1.0 ) *0.3
*
*       calculate the new window limits
*
         WXST = RXS - XINC
         WXFN = RXF + XINC
         WYST = RYS - YINC
         WYFN = RYF + YINC
*
*       clear the graphics area
*
         CALL PICCLE
         CALL VUPORT ( 0. , 1. , 0. , 1. )
         CALL WINDOL ( WXST , WXFN , WYST , WYFN )
         CALL AXIS ( RXS-1.0 , RXF+1.0 , RYS-1.0 , RYF+1.0 , 'X' , 'Y' )
*
*       draw the 100% polarization marker
*
         XM = RXS - 1.0
         YM = RYS - ( 0.5 *YINC )
         CALL MOVTO2 ( XM , YM )
         XE = XM + (4.0 *VECSCL )
         CALL LINTO2 ( XE , YM )
         CALL CHAESC('*')
         CALL CHAHOL ('    = 100% POLARIZATION*.')
*
*       set up a new window to allow a subimage to be plotted
*
         WXST = WXST - RXS + 1.
         WXFN = WXFN - RXS + 1.
         WYST = WYST - RYS + 1.
         WYFN = WYFN - RYS + 1.
         CALL WINDOL ( WXST , WXFN , WYST , WYFN )
         CALL POLPLT (VECSCL,NAXA(1),NAXA(2),TPOL,
     :                NAXT(1),NAXT(2),APOL,
     :                NPLT(1),NPLT(2),PLTOT,PLANG)
         CALL MOVTO2 ( WXST , WYST )
         CALL CHAHOL ('.*.')
         CALL DEVEND
      ENDIF
      END
