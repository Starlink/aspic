      SUBROUTINE RINGC1 (NCOL,NLEVEL,IDCOL)
C+
C     RINGC1.
C
C     Generates a continuous "spectrum" type colour table
C     adjusted to have equal levels of purple at both ends
C     so that it can be replicated to form a smooth series
C     of cycles.
C
C  Given;
C   NCOL   (I)  No. of primaries in l-u-t (always = 3).
C   NLEVEL (I)  No. of levels in l-u-t (for Args = 256).
C
C  Returned;
C   IDCOL  (IA) Colour l-u-t generated.
C
C  Subroutines called;
C   None.
C
C  J.A.Cooke/UOE/1981
C  A C Davenhall./ROE/                                  27/10/82.
C-
      INTEGER NCOL,NLEVEL
      INTEGER IDCOL(NCOL,NLEVEL)
      INTEGER INTVAL,ITAB,INCR,IDOWN,IUP
C
C
C     Calculate intervals and increments.
C
      INTVAL=NLEVEL/6
      INCR=NLEVEL/INTVAL
C
C     Fill in colour values.
C
      IDOWN=255
      IUP=0
      DO ISECT=1,INTVAL
        IDOWN=IDOWN-INCR
        IUP=IUP+INCR
        IF(IDOWN.LT.0)IDOWN=0
        IF(IUP.GT.255)IUP=255
        ITAB=ISECT
C
C     Sect 1
C
        IDCOL(1,ITAB)=IDOWN
        IDCOL(2,ITAB)=0
        IDCOL(3,ITAB)=255
C
C     Sect 2
C
        ITAB=ITAB+INTVAL
        IDCOL(1,ITAB)=0
        IDCOL(2,ITAB)=IUP
        IDCOL(3,ITAB)=255
C
C     Sect 3
C
        ITAB=ITAB+INTVAL
        IDCOL(1,ITAB)=0
        IDCOL(2,ITAB)=255
        IDCOL(3,ITAB)=IDOWN
C
C     Sect 4
C
        ITAB=ITAB+INTVAL
        IDCOL(1,ITAB)=IUP
        IDCOL(2,ITAB)=255
        IDCOL(3,ITAB)=0
C
C     Sect 5
C
        ITAB=ITAB+INTVAL
        IDCOL(1,ITAB)=255
        IDCOL(2,ITAB)=IDOWN
        IDCOL(3,ITAB)=0
C
C     Sect 6
C
        ITAB=ITAB+INTVAL
        IDCOL(1,ITAB)=255
        IDCOL(2,ITAB)=0
        IDCOL(3,ITAB)=IUP
        IF(ITAB.EQ.256)GOTO30
      END DO
C
C     Fill out end if needed.
C
 30   DO ISECT=ITAB,256
        IDCOL(1,ISECT)=255
        IDCOL(2,ISECT)=0
        IDCOL(3,ISECT)=255
      END DO
C
      END
