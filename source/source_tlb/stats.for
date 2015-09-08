C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   STATS *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               STATS  [ RMS=true ]  [ QUIET=true ]
C
C
C          FUNCTION:-
C               It  computes  and  returns  to  the  environment  some  key
C               statistical  parameters  for  a  Starlink  frame. The basic
C               information is also displayed on the terminal; the rest may
C               be inspected (within DSCL) by typing LOOK STATS
C
C
C          USE:-
C               It is an easy way of getting the basic  information   about
C               an  image  as a check on its state, or prior to display. In
C               particular the SIGM1 and SIGP7 were designed to  give  good
C               low and high values for image display to show data close to
C               the sky limit. In fact the sigma used is simply the  square
C               root  of  the mean so if there is a large offset the limits
C               will not be as good as possible. STDEV gives a much  better
C               estimate of the image noise.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               This is  the  input  Starlink
C                                             frame (any dimension).
C
C         NAXIS                               Returned  as  the  number  of
C                                             dimensions of the image.
C
C         NAXIS1                              Returned as the size  of  the
C                                             first dimension.
C
C         NAXIS2                              Returned as the size  of  the
C                                             second dimension.
C
C         NAXIS3                              Returned as the size  of  the
C                                             third dimension.
C
C         TOTAL                               Returned as the  sum  of  all
C                                             the data values in the frame.
C
C         MEAN                                Returned as the average  data
C                                             value.
C
C
C
C         MIN                                 Returned as the smallest data
C                                             value found.
C
C         MAX                                 Returned as the largest  data
C                                             value found.
C
C         SIGM1                               Returned as MEAN - SIGMA
C
C         SIGP7                               Returned as MEAN + 7*SIGMA
C
C         STDEV                               If RMS=TRUE (not the default)
C                                             this  is  returned as the RMS
C                                             deviation from the mean. This
C                                             is  usually a more meaningful
C                                             value than the SIGMA used  in
C                                             SIGM1,SIGP7   WHICH   IS  THE
C                                             SQUARE ROOT  OF  THE  AVERAGE
C                                             VALUE.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         QUIET           FALSE               By  default   some   of   the
C                                             parameter      values     are
C                                             displayed on the terminal. If
C                                             QUIET=TRUE  then  nothing  is
C                                             typed.
C
C         RMS             FALSE               If RMS=TRUE then a  value  is
C                                             returned    in   STDEV.   The
C                                             default is false to save time
C                                             computing  its value when not
C                                             needed.
C
C
C
C         Several                  RGO - RAL                      13-JAN-82
C
C
C--------------------------------------------------------------------------



* ORIGINALLY WRITTEN BY PTW AT RAL
* ALTERED ON 14 APR 1981 BY WFL RGO SO THAT THE DIMENSIONS (THE
* FIRST THREE ANYWAY!), MEAN, MAX AND MIN ARE WRITTEN TO THE
* ENVIRONMENT.
* ALTERED AGAIN BY WFL ON 19/5/81 TO GIVE OPTION OF QUIET OPERATION
*  ALTERED BY CDP ON 8/12/81 TO INCLUDE OPTION OF CALCULATING
*  THE RMS DEVIATION IF PARAMETER RMS IS TRUE. IT IS NORMALLY
*  CONNECTION FILE DEFAULTED TO .FALSE.

      INTEGER IDIMN(99)
      LOGICAL QUIET,RMS
      CHARACTER*40 MESS
      CHARACTER*3 DIGITS

      INCLUDE 'INTERIM(FMTPAR)'

      DATA DIGITS/'123'/

*  GET THE IMAGE
      CALL RDIMAG('IMAGE',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)

*  DETERMINE WHETHER TO OPERATE QUIETLY
      CALL RDKEYL('QUIET',.FALSE.,1,QUIET,I,JSTAT)
*
*  DECIDE WHETHER TO CALCULATE STD DEVIATION
*
      CALL RDKEYL('RMS',.FALSE.,1,RMS,I,JSTAT)


*  REPORT DIMENSIONS & DETERMINE TOTAL NUMBER OF PIXELS
      NELS=1
      CALL WRKEYI('NAXIS',NDIMS,1,JSTAT)
      DO I=1,NDIMS
         ID=IDIMN(I)
         WRITE (MESS,'('' DIM'',I2,''  ='',I5)') I,ID
         IF (.NOT.QUIET) CALL WRUSER(MESS,JSTAT)
         IF (I.LE.3) THEN
            CALL WRKEYI('NAXIS'//DIGITS(I:I),ID,1,JSTAT)
         ENDIF
         NELS=NELS*ID
      END DO

*  DETERMINE TOTAL, MEAN, MIN, MAX
      CALL STATS(%VAL(IPIN),NELS,RIT,RIM,RIMIN,RIMAX,RMS,STDEV)

*  REPORT
      WRITE (MESS,'('' TOTAL  ='',G14.6)') RIT
      IF (.NOT.QUIET) CALL WRUSER(MESS,JSTAT)
      CALL WRKEYR('TOTAL',RIT,1,JSTAT)
      WRITE (MESS,'(''  MEAN  ='',G14.6)') RIM
      IF (.NOT.QUIET) CALL WRUSER(MESS,JSTAT)
      CALL WRKEYR('MEAN',RIM,1,JSTAT)
      WRITE (MESS,'(''   MIN  ='',G14.6)') RIMIN
      IF (.NOT.QUIET) CALL WRUSER(MESS,JSTAT)
      CALL WRKEYR('MIN',RIMIN,1,JSTAT)
      WRITE (MESS,'(''   MAX  ='',G14.6)') RIMAX
      IF (.NOT.QUIET) CALL WRUSER(MESS,JSTAT)
      CALL WRKEYR('MAX',RIMAX,1,JSTAT)

*  TELL OUTSIDE WORLD ESTIMATED -1 & +7 SIGMA FOR DISPLAY PURPOSES
      SIG=SQRT(MAX(RIM,0.0))
      SIGM1=RIM-SIG
      SIGP7=RIM+7.0*SIG
      CALL WRKEYR('SIGM1',SIGM1,1,JSTAT)
      CALL WRKEYR('SIGP7',SIGP7,1,JSTAT)

*
*  IF CALCULATED THEN OUTPUT STD DEVIATION
*
      IF(RMS)  THEN
         IF(.NOT.QUIET) THEN
            WRITE(MESS,'('' STD DEVIATION ='',G14.6)') STDEV
            CALL WRUSER(MESS,JSTAT)
         ENDIF
         CALL WRKEYR('STDEV',STDEV,1,JSTAT)
      ENDIF

*  WRAP UP
      CALL FRDATA(' ',JSTAT)

      END
      SUBROUTINE STATS(PIC,NELS,RIT,RIM,RIMIN,RIMAX,RMS,STDEV)
*
*  ESTABLISH TOTAL, MEAN, MIN, MAX PIXEL VALUE
*
*  GIVEN:
*     PIC,NELS              PIXEL ARRAY, NUMBER OF PIXELS
*
*  RETURNED:
*     RIT,RIM,RIMIN,RIMAX   TOTAL, MEAN, MIN, MAX
*

      INTEGER NELS
      REAL PIC(NELS),RIT,RIM,RIMIN,RIMAX
      LOGICAL RMS

      DOUBLE PRECISION DWK(0:999),DIT

      DO I=0,999
         DWK(I)=0D0
      END DO

      RIMIN=PIC(1)
      RIMAX=RIMIN

      DO I=1,NELS
         V=PIC(I)
         RIMIN=MIN(RIMIN,V)
         RIMAX=MAX(RIMAX,V)
         IM=MOD(I,1000)
         DWK(IM)=DWK(IM)+DBLE(V)
      END DO

      DIT=0D0

      DO I=0,999
         DIT=DIT+DWK(I)
      END DO

      RIT=REAL(DIT)

      RIM=RIT/REAL(NELS)

*
*  IF REQUESTED CALCULATE STANDARD DEVIATION
*
      IF(RMS)  THEN
         STDEV = 0.0
         DO I=1,NELS
            STDEV = STDEV + (PIC(I)-RIM)**2
         ENDDO
         STDEV = SQRT(STDEV/REAL(NELS))
      ENDIF

      END
