      SUBROUTINE APERNF (MAXLEV,MAXBOX,PIXCT,NLEVEL,NBOX,MAXPAR,
     :                   CORBOX,NPIX,FPIX)
C+
C     APERNF.
C
C     Subroutine to compute the qantities mean quantities
C     N and F, choosing each aperture to be the smallest
C     aperture in turn, from the arrays of pixels 
C     counts for each isophotal level and each aperture.
C
C  Given;
C   MAXLEV (I)  Max. permitted no. of levels in profile.
C   MAXBOX (I)   " .     "     " . "  boxes.
C   PIXCT  (IA) Array of pixels counted for each level and box.
C   NLEVEL (I)  Actual no of intensity levels in the profile.
C   NBOX   (I)  Actual no. of apertures.
C   MAXPAR (I)  No. of coordinates for each box, at least 4.
C   CORBOX (IA) Coordinates. for each box.
C
C  Returned;
C   NPIX   (RA) Mean N for all apertures larger than the Jth,
C               where J ranges from 1 to NBOX - 1.
C   FPIX   (RA) Mean F for all apertures larger then the Jth
C               where J ranges from 1 to NBOX - 1.
C
C  Subroutines called;
C   None.
C
C  Structure:-
C   Do for all isophotal levels.
C     Do for apertures, J=1 to NBOX-1
C       sum f = 0
C       sum n = 0
C       Do for JJ=J to NBOX-1
C         Do for KK=J+1 to NBOX
C           compute f for JJ,KK.
C           compute n for JJ,KK.
C           add to running sums for f, n.
C           increment running count
C         end do
C       end do
C       Compute mean f and n for this aperture.
C       Store in the output array.
C     end do
C   end do
C
C  A C Davenhall./ROE/                                  24/8/82.
C-
      INTEGER MAXLEV,MAXBOX,NLEVEL,MAXPAR,NBOX
      INTEGER PIXCT(MAXLEV,MAXBOX),CORBOX(MAXBOX,MAXPAR)
      REAL NPIX(MAXLEV,MAXBOX), FPIX(MAXLEV,MAXBOX)
C
      INTEGER LEVEL,JJ,KK,COUNT
      REAL SUMF,SUMN
      REAL AREAJJ,AREAKK,F,N
C
C
      DO LEVEL=1,NLEVEL
        DO J=1,NBOX-1
          SUMF=0.0E0
          SUMN=0.0E0
          COUNT=0
          DO JJ=J,NBOX-1
            DO KK=JJ+1,NBOX
C
C    Compute N and F for the combination of apertures
C    JJ and KK.
C
              AREAJJ=FLOAT(CORBOX(JJ,3)-CORBOX(JJ,1))*
     :               FLOAT(CORBOX(JJ,4)-CORBOX(JJ,2))
              AREAKK=FLOAT(CORBOX(KK,3)-CORBOX(KK,1))*
     :               FLOAT(CORBOX(KK,4)-CORBOX(KK,2))
              N=((AREAJJ*FLOAT(PIXCT(LEVEL,KK)))-
     :          (AREAKK*FLOAT(PIXCT(LEVEL,JJ))))/
     :          (AREAJJ-AREAKK+FLOAT(PIXCT(LEVEL,KK))-
     :           FLOAT(PIXCT(LEVEL,JJ)))
              F=(FLOAT(PIXCT(LEVEL,JJ))-N)/(AREAJJ-N)
C
C    Add to the accumulating sums for this minimum aperture.
C
              COUNT=COUNT+1
              SUMN=SUMN+N
              SUMF=SUMF+F
            END DO
          END DO
C
C    Find and save the mean N and F for this (minimum) aperture.
C
          NPIX(LEVEL,J)=SUMN/FLOAT(COUNT)
          FPIX(LEVEL,J)=SUMF/FLOAT(COUNT)
        END DO
      END DO
      END
