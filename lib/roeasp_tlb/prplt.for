      SUBROUTINE PRPLT
*+
*     PRPLT
*
*     Given total polarization and polarization angle,
*     plots polarization vectors to ARGS or VERSATEK.
*
*     Subroutines called :
*     INPICR,OUTPICR,READI,READR,CLEARIM,WRUSER,MULREP    : ASPFACE
*     POLPLT,SUBIM1,ARGSET                                : E2DASP
*     VERSA,VUPORT,WINDOL,CLICTL,DEVEND                   : ROEFINGS
*     ARGS_OVGEN,ARGS_OVWRT,ARGS_OVCOL,ARGS_OVCLR,SRINIT  : ARGSLIB
*
*     B.D.Kelly/ROE/8.3.1982
*     D.W.T.Baines/ROE/JAN 1983/
*-
      INTEGER NAXT(2),NAXA(2),NPLT(2),NAXE(2)
      INTEGER NPT,NPA,NPE,IST,IFAIL,ISTAT,NTOT,NANG
      INTEGER N1DEF,N2DEF,N3DEF,N4DEF,NWK3,NWK4,NXFN,NXST,NYFN,NYST
      REAL VECSCL
      CHARACTER*10 REPLY
*
*     set all status return variables to 0
*
      IST = 0
      ISTAT = 0
      IFAIL = 0
*
*     request the total polarization and polarization angle frames
*
      CALL INPICR('INPIC2','Give the Total Polarization Input frame',
     :             2,NAXT,NPT,IST)
      CALL INPICR('INPIC3','Give the Polarization Angle Input frame',
     :            2,NAXA,NPA,IST)
*
*     if the input frames have been accessed o.k. continue , otherwise end
*
      IF ( IST .EQ. 0 ) THEN
         NPLT(1)=128
         NPLT(2)=128
         CALL OUTPICR('WORK1','TOT. POL. PLOTSPACE',2,NPLT,NTOT,IST)
         CALL OUTPICR('WORK2','POL. ANG. PLOTSPACE',2,NPLT,NANG,IST)
         N1DEF=1
         N2DEF=NAXT(1)
         N3DEF=1
         N4DEF=NAXT(2)
*
*        request the X and Y limits for the plot
*
         CALL READI('XSTART','Give X-Coord of Leftmost pixel',
     :              N1DEF,1,NAXT(1),NXST,IST)
         CALL READI('XFIN','Give X-Coord of Rightmost pixel',
     :             N2DEF,NXST,NAXT(1),NXFN,IST)
         CALL READI('YSTART','Give Y-Coord of Bottom pixel',
     :             N3DEF,1,NAXT(2),NYST,IST)
         CALL READI('YFIN','Give Y-Coord of Top pixel',
     :             N4DEF,NYST,NAXT(2),NYFN,IST)
         NAXE(1)=NXFN-NXST+1
         NAXE(2)=NYFN-NYST+1
         CALL OUTPICR('WORK3','TOT. POL. SUBIMAGE',2,NAXE,NWK3,IST)
         CALL OUTPICR('WORK4','POL. ANG. SUBIMAGE',2,NAXE,NWK4,IST)
*
*        if all image frames accessed o.k. continue
*
         IF ( IST .EQ. 0 ) THEN
*
*           get the subimages defined by NXST , NXFN , NYST , NYFN
*           putting them into the workspace arrays set up for them
*
            CALL SUBIM1(NAXT(1),NAXT(2),%VAL(NPT),NXST,NXFN,NYST,NYFN,
     :                  NAXE(1),NAXE(2),%VAL(NWK3))
            CALL SUBIM1(NAXA(1),NAXA(2),%VAL(NPA),NXST,NXFN,NYST,NYFN,
     :                  NAXE(1),NAXE(2),%VAL(NWK4))
            CALL PRPLTS (NXST,NXFN,NYST,NYFN,NAXE,%VAL(NWK3),
     :                   NAXE,%VAL(NWK4),NPLT,%VAL(NTOT),%VAL(NANG))
         ELSE
*
*           here if error setting up the workspace
*
            CALL WRUSER ('Error accessing required workspace',ISTAT)
         ENDIF
      ELSE
*
*        inform user of error
*
         CALL WRUSER ('Error accessing I/O frames',ISTAT)
      ENDIF
      CALL CLEARIM('INPIC2')
      CALL CLEARIM('INPIC3')
      CALL CLEARIM('WORK1')
      CALL CLEARIM('WORK2')
      CALL CLEARIM('WORK3')
      CALL CLEARIM('WORK4')
      END
