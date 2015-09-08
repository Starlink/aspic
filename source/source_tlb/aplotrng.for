      program aplotrng
*+
*   aplotrng
*
*   PLOTS INTENSITY WEDGE TO ARGS WITH SCALE
*    should be run AFTER plotting a picture to args with APLOT
*    (no other plot program will do) assumes APLOT has been
*    called first (which sets TZEROD and TMAXD) assumes picture
*    is 512*512 occupying whole args screen as it plots colour
*    range right at the top of args screen uses colour table
*    that was last selected occupies top 21 rows of args
*    (from y=490 on)
*
*   Given      (program parameters)
*   TZEROD      DATA LEVEL MAPPED TO ZERO IN ARGS PLOT
*   TMAXD       MAX INTENSITY IN CURRENT IMAGE
*
*   Subroutines called :
*   ARGSET             : ROEASP
*   MOVTO2, CHAFIX, CHASIZ, DEVEND : FINGS
*   SRPXI2             : STARLINK
*
*   B.D KELLY/ROE/1981
*    d. tudhope/roe/dec 82
*-
      REAL TZEROD,TMAXD
      INTEGER*2 JDAT(512,1),DUMMY(1),ISTATUS

      istatus=0
      call srinit(0,.false.,istatus)
      call readr('TZEROD',' ',-1.0e20,-1.0e19,1.0e19,tzerod,istatus)
      call readr('TMAXD',' ',-1.0e20,-1.0e19,1.0e19,tmaxd,istatus)
      if (istatus.ne.0) then
        call wrerr('misread')
      else

*
*   CLEAR PREVIOUS WEDGE PLOT
*
      DO I=1,512
        JDAT(I,1)=0
      ENDDO
      DO J=490,511
        CALL SRPXI2(JDAT,512,512,1,0,J,16,.FALSE.,DUMMY,1)
      ENDDO
*
*   SET UP NEW WEDGE
*
      DO I=1,512
        JDAT(I,1)=(I-1)/2
      ENDDO

      IF(ABS(TMAXD-TZEROD).GT.1.0E-20) THEN
        CALL ARGSET(0)
        CALL CHASIZ(1.0)
        DO I=100,400,100
          JDAT(I,1)=255
          VAL=TZEROD+(FLOAT(I-1)/512.0)*(TMAXD-TZEROD)
          POS=I-50
          CALL MOVTO2(POS,495.0)
          LSIZE=12
          LPOS=2
          CALL CHAFIX(VAL,LSIZE,LPOS)
        ENDDO
        CALL DEVEND
      ENDIF
      DO J=505,511
        CALL SRPXI2(JDAT,512,512,1,0,J,16,.FALSE.,DUMMY,1)
      ENDDO

      endif
      END
