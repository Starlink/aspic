      PROGRAM LIST
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ******************
C                     *                *
C                     * Program   LIST *
C                     *                *
C                     ******************
C
C
C
C          CALLING SEQUENCE:-
C               LIST
C
C
C          FUNCTION:-
C               It generates a formatted listing of a  1  or  2-D  Starlink
C               image.  The  resulting  file  may  be  printed/typed in the
C               normal way.
C
C
C          USE:-
C               Obviously - to look at your data. To have a look at a small
C               section of data the program PEEP may be used.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 1 or 2-D  Starlink  image
C                                             which is to be listed.
C
C         FILE            [IMAGELIS].LIS      This is the name of the  file
C                                             to  which  output is written.
C                                             If  omitted  completely   the
C                                             name IMAGELIS.LIS is used. If
C                                             a name only is entered then a
C                                             default type of .LIS is used.
C
C         FACTOR          1                   The    pixel    values    are
C                                             multiplied  by  FACTOR before
C                                             being printed. It  should  be
C                                             chosen   so  that  a  3-digit
C                                             representation of data*factor
C                                             is meaningful.
C
C
C         P T Wallace              RAL                             7-JAN-82
C
C
C--------------------------------------------------------------------------




*  WRITTEN BY P T WALLACE AT RAL
*  MODIFIED FOR 1-D BY K F HARTLEY AT RGO ON 23/10/81
*


      INTEGER IDIMN(99)
      INCLUDE 'INTERIM(FMTPAR)'

      PARAMETER (LFN=20)
      CHARACTER C,FILE*(LFN),FNAME*(LFN)


*
*  PREPARE TO READ IMAGE AND ENSURE 2-D
*  ------------------------------------
*  SECOND VERSION (BY KFH) HANDLES 1-D AS WELL

      CALL RDIMAG('IMAGE',FMT_R,99,IDIMN,NDIMS,IPIN,JSTAT)
      IF (NDIMS.GT.2) GO TO 9000

*
*  PICK UP AND TITIVATE FILENAME
*  -----------------------------

      FILE='IMAGELIS.LIS'
      CALL RDKEYC('FILE',.FALSE.,1,FILE,NVALS,JSTAT)

*  PULL OUT FIRST NON-SPACE STRING
      KS=1
      KD=1
      FNAME=' '
      JFLAG=0
      DO WHILE (JFLAG.EQ.0)
         C=FILE(KS:KS)
         IF (C.NE.' ') THEN
            FNAME(KD:KD)=C
            KD=KD+1
         ELSE
            IF (KD.NE.1) JFLAG=1
         END IF
         KS=KS+1
         IF (KS.GT.LFN) JFLAG=1
      END DO

*  APPEND FILETYPE IF OMITTED
      IF (INDEX(FNAME,'.').EQ.0) THEN
         ISP=INDEX(FNAME,' ')
         IF (ISP.EQ.0 .OR. ISP.GT.LFN-3) GO TO 9010
         FNAME(ISP:)='.LIS'
      END IF

*
*  PICK UP FACTOR
*  --------------

         FACTOR=1.0
         CALL RDKEYR('FACTOR',.FALSE.,1,FACTOR,NVALS,JSTAT)

*
*  LIST THE DATA ARRAY
*  -------------------

      CALL ILIST(%VAL(IPIN),IDIMN(1),IDIMN(2),FNAME,FACTOR)

*  EXIT
      GO TO 9900

*
*  ERRORS
*  ------

 9000 CONTINUE
      CALL WRUSER('CANNOT HANDLE MORE THAN 2D!',JSTAT)
      GO TO 9900

 9010 CONTINUE
      CALL WRUSER('INVALID FILENAME!',JSTAT)

*
*  WRAP UP
*  -------

 9900 CONTINUE
      CALL FRDATA(' ',JSTAT)

      END

      SUBROUTINE ILIST(A,NX,NY,FNAME,FACTOR)

*
*  LIST AN ARRAY
*
*
*  GIVEN:
*     A        ARRAY TO BE LISTED
*     NX,NY    DIMENSIONS OF ARRAY A
*     FNAME    FILENAME FOR REPORT
*     FACTOR   SCALING FACTOR FOR PIXEL VALUES
*
*  FOR EACH PIXEL A 3-DIGIT REPRESENTATION OF FACTOR TIMES ITS
*  VALUE IS LISTED.
*

      INTEGER NX,NY
      REAL A(NX,NY),FACTOR
      CHARACTER*(*) FNAME


      CHARACTER LINE*128


*  OPEN REPORT
      OPEN (UNIT=1,FILE=FNAME,STATUS='NEW')

*  INITIALISE STRIP COUNT
      NSTRIP=0

*  LIST IN VERTICAL STRIPS EACH 40 PIXELS WIDE
      DO IXS=1,NX,40

*     INCREMENT & REPORT STRIP COUNT
         NSTRIP=NSTRIP+1
*   TO SAVE PAPER WE ONLY THROW A NEW PAGE FOR EACH STRIP
*   IF THERE ARE MORE THAN 8 STRIPS
*   REALLY DONE TO HANDLE 1-D CASE!

         IF (NY.GT.8) THEN
            WRITE (1,'(''1STRIP'',I3,30X,
     :             ''DIVIDE LISTED VALUES BY'',G12.4/)') NSTRIP,FACTOR
         ELSE
            WRITE(1,'(''0STRIP'',I3,30X,
     :              ''DIVIDE LISTED VALUES BY'',G12.4/)') NSTRIP,FACTOR
         END IF

*     REPORT FIRST & LAST X
         IXE=MIN(IXS+39,NX)
         LINE=' '
         WRITE (LINE(:11),'(I11)') IXS-1
         K=4+3*(IXE-IXS)
         WRITE (LINE(K:K+7),'(I8)') IXE-1
         WRITE (1,'(A/)') LINE

*     LINE BY LINE
         DO IY=NY,1,-1

*        RESET LINE BUFFER
            LINE=' '

*        FORMAT Y-COORDINATE
            WRITE (LINE(:5),'(I5)') IY-1

*        PIXEL BY PIXEL
            DO IX=IXS,IXE

*           BUFFER POINTER
               K=9+3*(IX-IXS)

*           FORMAT PIXEL
               Z=FACTOR*A(IX,IY)
               IF (ABS(Z).GT.1000.0) Z=SIGN(Z,1000.0)
               IZ=NINT(Z)
               IF (IZ.LT.-99) THEN
                  LINE(K:K+2)='---'
               ELSE IF (IZ.GT.999) THEN
                  LINE(K:K+2)='+++'
               ELSE
                  WRITE (LINE(K:K+2),'(I3)') IZ
               END IF

*           NEXT PIXEL
            END DO

*        OUTPUT THE LINE
            WRITE (1,'(A)') LINE

*        NEXT LINE
         END DO

*     NEXT STRIP
      END DO

*  CLOSE REPORT
      WRITE (1,'(''1'')')
      CLOSE (UNIT=1)

*  EXIT

      END
