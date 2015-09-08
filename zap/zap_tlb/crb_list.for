	SUBROUTINE CRB_LIST
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
C               It generates a formatted listing of a  1  or  2-D  Starl
C               image.  The  resulting  file  may  be  printed/typed in
C               normal way.
C
C
C          USE:-
C               Obviously - to look at your data. To have a look at a sm
C               section of data the program PEEP may be used.
C
C
C
C         USER PARAMETERS:-
C
C         IMAGE                               The 1 or 2-D  Starlink  im
C                                             which is to be listed.
C
C         FILE            [IMAGELIS].LIS      This is the name of the  f
C                                             to  which  output is writt
C                                             If  omitted  completely
C                                             name IMAGELIS.LIS is used.
C                                             a name only is entered the
C                                             default type of .LIS is us
C
C         FACTOR          1                   The    pixel    values
C                                             multiplied  by  FACTOR bef
C                                             being printed. It  should
C                                             chosen   so  that  a  3-di
C                                             representation of data*fac
C                                             is meaningful.
C
C
C         P T Wallace              RAL                             7-JAN
C
C
C-----------------------------------------------------------------------
 
 
 
 
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
 
	CALL CNPAR('FILE',ISTAT)
	CALL CNPAR('FACTOR',ISTAT)
      END
 
