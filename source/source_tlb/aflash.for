C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program  AFLASH *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               AFLASH
C
C
C          FUNCTION:-
C               It displays an image on the  ARGS by simply copying
C               a 2-D starlink image to the ARGS memory.  AFLASH does
C               not do any intensity scaling, thus it is much faster
C               than the similar program ADISP.
C
C
C          USE:-
C               Mainly useful for displaying integer*2 arrays which have
C               pixels in the range 0 < N < 255, which is the extent of
C               the ARGS look up table.  Values outside this range will
C               be displayed with a LUT value of MOD(N,256)
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 2-d Starlink image (I*2)
C                                             to  be displayed.  It  may be
C                                             of any size, but no more than
C                                             512*512    pixels   will   be
C                                             visible.
C
C         XC              256                 The x co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C         YC              256                 The y co-ordinate on the ARGS
C                                             where the CENTER of the image
C                                             will be located.
C
C
C
C          W PENCE   AAO  30 SEPT 82
C          BASED ON THE ADISP PROGRAM WRITTEN BY PTW/WFL/KFH        RGO
C
C
C--------------------------------------------------------------------------
      INTEGER IDIMN(99)
      CALL SRINIT(0,.FALSE.,JSTAT)
      IF (JSTAT.NE.0) THEN
         CALL WRUSER('DISPLAY UNAVAILABLE',JSTAT)
      ELSE
         CALL WRUSER('NAME OF IMAGE FILE TO BE DISPLAYED=?',JSTAT)
         CALL RDIMAG('IMAGE',102,99,IDIMN,NDIMS,IPIN,JSTAT)
         IF (NDIMS.NE.2) THEN
            CALL WRUSER('MUST BE 2D IMAGE!',J)
         ELSE
            IXC=256
            IYC=256
            CALL WRUSER('IMAGE POSITION ON ARGS=?',JSTAT)
            CALL RDKEYI('XC',.TRUE.,1,IXC,NVALS,JSTAT)
            CALL RDKEYI('YC',.TRUE.,1,IYC,NVALS,JSTAT)
            CALL AFLASH2(%VAL(IPIN),IDIMN(1),IDIMN(2),IXC,IYC)
            CALL FRDATA(' ',JSTAT)
C
C           UPDATE THE ARGS DATABASE
C
            CALL ARGS_WRIM (IXC,IYC,IDIMN(1),IDIMN(2),IDIMN(1),IDIMN(2),
     &         JSTAT)
            IF(JSTAT.NE.0)THEN
               CALL WRUSER('COULDN''T UPDATE ARGS DATABASE',JSTAT)
            ENDIF
         END IF
      ENDIF
      END
C
      SUBROUTINE AFLASH2(IPIC,NX,NY,IXC,IYC)
      INTEGER NX,NY,IXC,IYC
      INTEGER*2 IPIC(NX*NY),IDUMMY
      CALL SRPXI2(IPIC,NX,NX,NY,IXC-NX/2,IYC-NY/2,
     &               16,.FALSE.,IDUMMY,1)
      END
