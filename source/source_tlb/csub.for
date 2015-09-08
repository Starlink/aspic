*
*+ CSUB  SCALAR  IMAGEIN  IMAGEOUT
*
*  SUBTRACT SCALAR FROM EVERY ELEMENT OF IMAGEIN GIVING IMAGEOUT
*

      INTEGER IDIMN(99)
      INCLUDE 'INTERIM(FMTPAR)'
      CALL RDKEYR('SCALAR',.FALSE.,1,SCALAR,NVALS,JSTAT)
      CALL RDIMAG('IN',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)
      CALL WRIMAG('OUT',FMT_R,IDIMN,NDIMS,IPOUT,JSTAT)
      NELS=1
      DO I=1,NDIMS
         NELS=NELS*IDIMN(I)
      END DO
      CALL CSUB(NELS,SCALAR,%VAL(IPIN),%VAL(IPOUT))
      CALL FRDATA(' ',JSTAT)
      END
      SUBROUTINE CSUB(NELS,SCALAR,A,B)
      INTEGER NELS
      REAL SCALAR,A(NELS),B(NELS)
      DO I=1,NELS
         B(I)=A(I)-SCALAR
      END DO
      END
