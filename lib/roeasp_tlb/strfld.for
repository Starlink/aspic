      SUBROUTINE STRFLD (MGLMFN,NLMFN,NBIN, MBIN,AREA,STMULT,
     :                   MAXSTAR,XSIZE,YSIZE,NSTAR,XCORD,YCORD,
     :                   APMAG,ISTAT)
C+
C      STRFLD.
C
C     Subroutine to generate the positions and apparent magnitudes
C     of a randomly distributed set of stars in an image,
C     according to a predetermined apparent luminosity function.
C
C  Given;
C  MGLMFN  (RA)  Set of apparent magnitudes for luminosity 
C                function.
C  NLMFN   (RA)  No. of stars observed at a given magnitude 
C                according to the adopted luminosity function.
C  NBIN    (I)   No. of bins in the apparent luminosity function.
C  MBIN    (I)   Size of arrays MGLMFN and NLMFN.
C  AREA    (R)   Area of image in square degrees.
C  STMULT  (R)   Multiplicative factor for stellar density.
C                = 1.0 at the galactic poles, increases with
C                  decreasing latitude.
C  MAXSTAR (I)   Max. no. of stars ever to be permitted in field
C                ( = size of arrays XCORD, YCORD & APMAG, below).
C  XSIZE   (R)   X extent of image (pixels).
C  YSIZE   (R)   Y   "    "    "   (  "   ).
C
C  Returned;
C  NSTAR   (I)   No. of stars in the field.
C  XCORD   (RA)  X coord. of stars in the field.
C  YCORD   (RA)  Y   "  . "    "   "   "    "  .
C  APMAG   (RA)  apparent magnitude of stars in the field.
C  ISTAT   (I)   Return status.
C                = 0 - successful (!!??) return.
C                = 1 - Tried to generate too many stars.
C                = 2 - Too many bins in apparent luminosity fn.
C
C  Subroutines called;
C  NAG;  G05CAF.
C
C    Structure:-
C    Set return status = 0.
C    If (program can cope with the number of bins in the apparent
C        luminosity function ) then
C       Compute the no. of stars in each magnitude bin & the
C       total no. of stars.
C       If (total no. of stars .le. max. permitted no of stars) then
C         Do while (all stars not generated)
C           Generate a Monte-Carlo Star
C           If star falls inside luminosity function
C             accept star
C           end if
C         end do
C         Generate random star positions.
C       else if too many stars
C         return status = 1
C       end if
C     else too many bins in luminosity function.
C       return status = 2
C     end if.
C
C     A C Davenhall./ROE/                              2/3/82.
C     Based on a routine by R. S. Stobie (ROE).
C-

      INTEGER NBIN,MBIN,MAXSTAR,NSTAR,ISTAT
      REAL MGLMFN(MBIN),NLMFN(MBIN),XCORD(MAXSTAR),YCORD(MAXSTAR),
     :     APMAG(MAXSTAR)
      REAL AREA,STMULT,XSIZE,YSIZE
C
C
      INTEGER MAXBIN,ICOUNT
      REAL ACLMFN(50)
      REAL SUMSTR,MAGRANG,NUMRANG,MAGMIN,NUMMIN,MAGSTR,
     :     NUMSTR,NUMINT
      DOUBLE PRECISION RX
      PARAMETER (MAXBIN=50)
C
C
      ISTAT=0
C
C      Check whether the program can handle the number of bins
C      in the apparent luminosity function that has been
C      input.
C 
      IF (NBIN.LE.MAXBIN) THEN
C
C      Can handle no. of bins.
C
C      Compute the no. of stars that actually fall inside
C      each magnitude bin, given the area of the field and
C      the galactic latitude factor. Also compute the
C      total no. of stars.
C
        SUMSTR=0.0E0
        DO I=1,NBIN
          ACLMFN(I)=AREA*STMULT*NLMFN(I)
          SUMSTR=SUMSTR+ACLMFN(I)
        END DO
        NSTAR=NINT(SUMSTR)
C
C      Chech whether this is more than the permitted no. of 
C      stars.
C
        IF (NSTAR.LE.MAXSTAR) THEN
C
C      A permissible no. of stars.
C
C      Perform Monte Carlo Simulation.
C*
C       Do while (less than required no. of stars generated)
C         generate random magnitude in range.
C         generate random no. in apparent luminosity 
C           function range = ab
C         interpolate to find no. of stars in app. lum. fn.
C           corresponding to magnitude generated = ac
C         if (ab.le.ac) then
C           increment no. of stars by +1
C           add magnitude to list of magnitudes.
C         end if
C       end do
C*
          MAGMIN=MGLMFN(1)
          MAGRANG=MGLMFN(NBIN)-MGLMFN(1)
          NUMMIN=NLMFN(1)
          NUMRANG=NLMFN(NBIN)-NLMFN(1)
          ICOUNT=0
          DO WHILE (ICOUNT.LT.NSTAR)
            RX=G05CAF (RX)
            MAGSTR=MAGMIN+(RX*MAGRANG)
            RX=G05CAF (RX)
            NUMSTR=NUMMIN+(RX*NUMRANG)
C
C      Interpolate to get number density corresponding to
C      generated magnitude.
C
            IF (MAGSTR.LE.MGLMFN(1)) THEN
              NUMINT=ACLMFN(1)
            ELSE IF (MAGSTR.GE.MGLMFN(NBIN)) THEN
              NUMINT=ACLMFN(NBIN)
            ELSE
              IK=0
              DO WHILE (MAGSTR.GT.MGLMFN(IK))
                IK=IK+1
              END DO
              IKK=IK-1
              NUMINT=ACLMFN(IKK)+((MAGSTR-MGLMFN(IKK))*
     :               (ACLMFN(IK)-ACLMFN(IKK))/
     :               (MGLMFN(IK)-MGLMFN(IKK)))
            END IF
C
C     Check if generated pt. falls inside luminosity distribution.
C
            IF (NUMSTR.LE.NUMINT) THEN
              ICOUNT=ICOUNT+1
              APMAG(ICOUNT)=MAGSTR
            END IF
          END DO
C
C     Generate a set of coords. for all the stars.
C
          DO I=1,NSTAR
            RX=G05CAF (RX)
            XCORD(I)=XSIZE*RX
            RX=G05CAF (RX)
            YCORD(I)=YSIZE*RX
          END DO
        ELSE
C
C     Case of more than permitted no. of stars.
C
          ISTAT=1
        END IF
      ELSE
C
C      Case of too many apparent magnitude bins.
C
        ISTAT=2
      END IF
      END
