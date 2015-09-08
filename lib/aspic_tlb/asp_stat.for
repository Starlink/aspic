      SUBROUTINE ASP_STAT (PARAM,INPUT,SIZE,VTOT,VMEAN,VMIN,VMAX,
     :    VSIG,STATUS)

*+  ASP_STAT
*
*   Obtain statistics of real BDF. If they have previously been
*   calculated, the values are retrieved from frame descriptors.
*   Otherwise, they are calculated and, if possible, the frame
*   descriptors are written.
*
*   Given:
*     PARAM   C    program parameter name corresponding to BDF
*     INPUT   RA   BDF data (arbitrary dimension)
*     SIZE    I    size of INPUT
*
*   Returned:
*     VTOT    R    array total
*     VMEAN   R    array mean
*     VMIN    R    array minimum value
*     VMAX    R    array maximum value
*     VSIG    R    array standard deviation
*     STATUS  I    return status (Starlink)
*
*   Called:
*     ASP_RDDSR, STATS, ASP_WRDSR: ASPIC
*
*   W.F.Lupton RGO 23 Oct 1981
*-

      INCLUDE 'INTERIM(ERRPAR)'

      INTEGER SIZE,STATUS,NVALS
      REAL INPUT(SIZE),VTOT,VMEAN,VMIN,VMAX,VSIG
      DOUBLE PRECISION DWK(0:999),DIT
      CHARACTER PARAM*(*)

*   If TOTAL dscriptor is defined, assume the others are too
      CALL ASP_RDDSR (PARAM,'TOTAL',1,VTOT,NVALS,STATUS)
      IF (STATUS.EQ.ERR_NORMAL) THEN
          CALL ASP_RDDSR (PARAM,'MEAN',1,VMEAN,NVALS,STATUS)
          CALL ASP_RDDSR (PARAM,'MIN',1,VMIN,NVALS,STATUS)
          CALL ASP_RDDSR (PARAM,'MAX',1,VMAX,NVALS,STATUS)
          CALL ASP_RDDSR (PARAM,'SIGMA',1,VSIG,NVALS,STATUS)
      ELSE IF (STATUS.EQ.ERR_DSCNPR) THEN

      DO I=0,999
         DWK(I)=0D0
      END DO

      VMIN=INPUT(1)
      VMAX=VMIN

      DO I=1,SIZE
         V=INPUT(I)
         VMIN=MIN(VMIN,V)
         VMAX=MAX(VMAX,V)
         IM=MOD(I,1000)
         DWK(IM)=DWK(IM)+DBLE(V)
      END DO

      DIT=0D0

      DO I=0,999
         DIT=DIT+DWK(I)
      END DO

      VTOT=REAL(DIT)

      VMEAN=VTOT/REAL(SIZE)

          VSIG = SQRT(MAX(VMEAN,0.0))
          CALL ASP_WRDSR (PARAM,'TOTAL',VTOT,1,STATUS)
          CALL ASP_WRDSR (PARAM,'MEAN',VMEAN,1,STATUS)
          CALL ASP_WRDSR (PARAM,'MIN',VMIN,1,STATUS)
          CALL ASP_WRDSR (PARAM,'MAX',VMAX,1,STATUS)
          CALL ASP_WRDSR (PARAM,'SIGMA',VSIG,1,STATUS)
          STATUS = ERR_NORMAL
      ENDIF

      END
