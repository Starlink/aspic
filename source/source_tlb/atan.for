*
*+ ATAN   IMAGEIN  IMAGEOUT
*
*  TAKE ATAN OF EVERY ELEMENT OF IMAGEIN GIVING IMAGEOUT
*

      INTEGER IDIMN(99)
      INCLUDE 'INTERIM(FMTPAR)'
      CALL RDIMAG('IN',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)
      CALL WRIMAG('OUT',FMT_R,IDIMN,NDIMS,IPOUT,JSTAT)
      NELS=1
      DO I=1,NDIMS
         NELS=NELS*IDIMN(I)
      END DO
      CALL TFUNC(NELS,SCALAR,%VAL(IPIN),%VAL(IPOUT))
      CALL FRDATA(' ',JSTAT)
      END
      SUBROUTINE TFUNC(NELS,SCALAR,A,B)
      INTEGER NELS
      REAL SCALAR,A(NELS),B(NELS)
      DO I=1,NELS
C
C   CHANGE THE NEXT LINE ONLY FOR OTHER TRIG FUNCTIONS
C
C   LINK PROGRAM BY "@ASPDIR:SLINK prog"
C
         B(I)=ATAN(A(I))
      END DO
      END
