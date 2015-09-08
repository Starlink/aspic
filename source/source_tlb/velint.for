       PROGRAM VELINT
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   VELINT *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               VELINT
C
C
C          FUNCTION:-
C               It combines two images - one an intensity map, the other  a
C               velocity  map  - into a single image which can be displayed
C               using VIDISP. The result is a colour coded  intensity  map,
C               where  the  colours  show the velocity at that point. If no
C               velocity information is available then a greyscale  display
C               is seen.
C
C
C          USE:-
C               It was written to handle TAURUS data, but provided the  two
C               images  are  different  but  pixels  correspond to the same
C               patch of sky, then it may be of value in other contexts.
C
C
C
C         USER PARAMETERS:-
C
C         VELMAP                              The input velocity map, which
C                                             is a 2-D Starlink image.
C
C         INTMAP                              The  input   intensity   map,
C                                             which  must  be  of  the same
C                                             dimension as VELMAP.
C
C         OUTPUT                              The combined image  which  is
C                                             of          the          form
C                                             16*Intensity+Velocity,  where
C                                             both  have been scaled from 0
C                                             to 15.
C
C         VREST           Ave. value          The velocity to  be  used  as
C                                             rest velocity for the scaling
C                                             of VELMAP.
C
C         VMAX            (Max-Min)/2         The cutoff (+ and -) for  the
C                                             velocity scaling.
C
C         INTLOW          Minimum             The   low   point   for   the
C                                             intensity scaling.
C
C
C
C
C         INTHI           Maximum             The  high   point   for   the
C                                             intensity scaling.
C
C
C
C         K F Hartley - R Hook     RGO                            13-JAN-82
C
C
C--------------------------------------------------------------------------



*   WRITTEN BY RICHARD HOOK AT RGO ON 21/11/81
*   WITH MINOR CHANGES BY KFH ON 27/11/81

      INCLUDE 'INTERIM(ERRPAR)'
      INCLUDE 'INTERIM(FMTPAR)'
       INTEGER*2 JOINT(512,512)
       INTEGER IDIMS(2)


*
*   READ IN THE VELOCITY AND INTENSITY MAPS
*
       CALL RDIMAG('VELMAP',FMT_R,2,IDIMS,N,IVPOINT,ISTAT)
       IF(ISTAT.NE.ERR_NORMAL) CALL EXIT

       CALL RDIMAG('INTMAP',FMT_R,2,IDIMS,N,IIPOINT,ISTAT)
       IF(ISTAT.NE.ERR_NORMAL) CALL EXIT

*
*   GET OUTPUT FRAME
*
       CALL WRIMAG('OUTPUT',FMT_R,IDIMS,2,IJOINT,ISTAT)
       IF (ISTAT.NE.ERR-NORMAL) CALL EXIT

*
*   TRANSFER TO A SUBROUTINE:
*
       NDP=IDIMS(1)
       NREC=IDIMS(2)
       CALL SUBVELINT(%VAL(IVPOINT),%VAL(IIPOINT),NDP,
     :               NREC,%VAL(IJOINT))

       CALL FRDATA(' ',ISTAT)
       CALL EXIT
       END




       SUBROUTINE SUBVELINT(VELMAP,INTMAP,NDP,NREC,JOINT)

       REAL VELMAP(NDP,NREC),INTMAP(NDP,NREC),
     :          JOINT(NDP,NREC)
       INTEGER*4 IDIMS(2)
       REAL INTLOW,INTUP

       IDIMS(1)=NDP
       IDIMS(2)=NREC

*
*   READ IN THE RANGES OF VALUES TO BE SCALED IN BOTH
*   VELOCITY AND INTENSITY:
*
       VELLOW=VELMAP(1,1)
       VELUP=VELMAP(I,J)
       DO J=1,NREC
          DO I=1,NDP
             IF (VELMAP(I,J).LT.VELLOW) VELLOW=VELMAP(I,J)
             IF (VELMAP(I,J).GT.VELUP) VELUP=VELMAP(I,J)
          END DO
       END DO
       VREST=(VELLOW+VELUP)/2.0
       VMAX=(VELUP-VELLOW)/2.0
       CALL RDKEYR('VREST',.TRUE.,1,VREST,N,ISTAT)

       CALL RDKEYR('VMAX',.TRUE.,1,VMAX,N,ISTAT)
       VELLOW=VREST-VMAX
       VELUP=VREST+VMAX

       INTUP=INTMAP(1,1)
       INTLOW=INTMAP(1,1)
       DO J=1,NREC
          DO I=1,NDP
             IF (INTMAP(I,J).LT.INTLOW) INTLOW=INTMAP(I,J)
             IF (INTMAP(I,J).GT.INTUP) INTUP=INTMAP(I,J)
          END DO
       END DO
       CALL RDKEYR('INTLOW',.TRUE.,1,INTLOW,N,ISTAT)

       CALL RDKEYR('INTHI',.TRUE.,1,INTUP,N,ISTAT)

*
*   SCALE VALUES INTO ARRAY "JOINT":
*
       DO J=1,NREC
          DO I=1,NDP
*
*      TRIM VALUES IF OUTSIDE RANGE:
*
             V=REAL(VELMAP(I,J))
             AI=REAL(INTMAP(I,J))

             IF(V.GT.VELUP) V=VELUP
             IF(V.LT.VELLOW) V=VELLOW
             IF(AI.GT.INTUP) AI=INTUP
             IF(AI.LT.INTLOW) AI=INTLOW

             JOINT(I,J)=16.0*INT(15.0*(AI-INTLOW)/
     :                      REAL(INTUP-INTLOW))+
     :                      15.0*(V-VELLOW)/
     :                      (VELUP-VELLOW)
          ENDDO
       ENDDO



       END



