C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   LUTCONT *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               LUTCONT
C
C
C          FUNCTION:-
C               It fills a look-up-table for the  ARGS  with  the  7  basic
C               colours in turn (red,green,blue,yellow,cyan,magenta,white).
C               The results are seen on  the  ARGS  at  once,  and  may  be
C               modified  by  erasing or overwriting part of the table. The
C               resulting LUT may be stored for future use.
C
C
C          USE:-
C               It was originally designed to set up individual entries  in
C               a  LUT  to generate contour-like displays, but has now been
C               generalized as an alternative to LUTSET and LUTE as a means
C               of setting up a LUT.
C
C
C
C         USER PARAMETERS:-
C
C         CONTOURS        prev. value         1 or 2 or  3  values  may  be
C                                             input.   If   the   first  is
C                                             positive then LUT entries are
C                                             set-up,   but   if   negative
C                                             entries  are  set  to   zero,
C                                             thereby erasing them. If only
C                                             one value is input that entry
C                                             is set to the next colour. If
C                                             two values  are  input,  then
C                                             all entries between the first
C                                             and second  are  filled  with
C                                             the  next  colour.  If  three
C                                             values are input then  blocks
C                                             of    LUT   from   first   to
C                                             first+last,and so  on  up  to
C                                             second  are  filled  with the
C                                             colours in  succession.  EXIT
C                                             IS BY ENTERING NO VALUES.
C
C         OUTPUT                              This    is    an     optional
C                                             parameter. If present the LUT
C                                             which has been  created  will
C                                             be  stored  for  later use by
C                                             LUTREAD.
C
C
C
C         K F Hartley              RGO                            11-JAN-82
C
C
C--------------------------------------------------------------------------



C      WRITTEN BY K F HARTLEY AT RGO ON 12/1/81
      INTEGER*4 STATUS,INDEX(3),LAX(2),LP,LUT(3,0:255)

      LOGICAL*1 FALS
      DATA LUT/768*0/
C
C
C      FIRST SET-UP THE ARGS
C
      FALS=.FALSE.
      CALL SRINIT(0,FALS,IFL)
      IF (IFL.NE.0) CALL EXIT
C
C      NOW PICK UP THE THREE PARAMETERS (IF PRESENT) AND DO THE TESTS
C
  100 CALL RDKEYI('CONTOURS',.TRUE.,3,INDEX,I,STATUS)
      IF (STATUS.EQ.1) THEN
         LAX(1)=3
         LAX(2)=256
         CALL WRIMAG('OUTPUT',104,LAX,2,LP,STATUS)
         IF (STATUS.NE.0) CALL EXIT
         CALL CLUT(LUT,LAX(1),LAX(2),%VAL(LP))
         CALL EXIT
      END IF
      IF (STATUS.GT.1) THEN
         CALL WRERR('HELL1',STATUS)
         CALL EXIT
      END IF
      IF (I.LT.0.OR.I.GT.3) THEN
         CALL WRERR('HELL2',STATUS)
         CALL CNPAR('CONTOURS',STATUS)
         IF (STATUS.NE.0) CALL EXIT
         GO TO 100
      END IF
      IF (IABS(N1).GT.255) THEN
         CALL WRERR('HELL3',STATUS)
         CALL CNPAR('CONTOURS',STATUS)
         IF (STATUS.NE.0) CALL EXIT
         GO TO 100
      END IF
C
C     NOW CALL THE ROUTINE THAT DOES THE WORK
C
      ITYP=I
      CALL LUTCONT(INDEX,ITYP,LUT)
C
C     AND GO BACK FOR MORE
C
      CALL CNPAR('CONTOURS',STATUS)
      IF (STATUS.NE.0) CALL EXIT
      GO TO 100
C
C      NORMAL EXIT IS BY ENTERING NO PARAMETERS
C
      END
      SUBROUTINE LUTCONT(CONTOURS,ITYP,LUT)
C
C      SETS UP THE ARGS LUTS TO GIVE CONTOURS
C
C      CONTOURS CONTAINS FIRST,LAST AND INCREMENT POSITION IN LUT
C      ITYP SAYS HOW MANY VALUES ARE SET IN CONTOURS
C
C      LUT IS USED TO STORE THE VALUES TO BE WRITTEN TO THE ARGS
C      IVALS CONTAINS THE RGB INTENSITIES FOR THE COLOURS TO BE
C         USED IN SEQUENCE FOR THE CONTOURS : R , Y , G , C , M , B , W
C      IPOS IS A POINTER TO THE CURENT COLOUR
C
C      IF CONTOUR(1) IS NEGATIVE CONTOURS ARE TO BE ERASED
C         IE SET TO ZERO
C
      INTEGER*4 CONTOURS(3)
      INTEGER*4 LUT(3,0:255)
      INTEGER*4 IVALS(3,7)
      DATA IVALS/255,0,0,255,255,0,0,255,0,0,255,255,
     1           0,0,255,255,0,255,255,255,255/
      DATA IPOS/1/
      IF (CONTOURS(1).LT.0) THEN
         CONTOURS(1)=-CONTOURS(1)
         IVAL=0
      ELSE
         IVAL=255
      END IF
C
C      IF ITYP IS 1 , THEN ONE ENTRY ONLY IN THE LUT IS TO BE CHANGED
C
      IF (ITYP.EQ.1) THEN
         DO 100 I=1,3
           IF(IVAL.EQ.0) THEN
             LUT(I,CONTOURS(1))=IVAL
           ELSE
            LUT(I,CONTOURS(1))=IVALS(I,IPOS)
           END IF
  100    CONTINUE
         IF(IVAL.NE.0) THEN
            IPOS=IPOS+1
            IF (IPOS.GT.7) IPOS=1
         END IF
      END IF
C
C      IF ITYP IS 2 THEN VALUES FROM CONTOUR(1) TO CONTOUR(2) , INCL ,
C      ARE SET TO THE SAME VALUE , GIVING THICK CONTOURS.
C
      IF (ITYP.EQ.2) THEN
         DO 200 K=CONTOURS(1),CONTOURS(2)
            DO 199 I=1,3
              IF (IVAL.EQ.0) THEN
                LUT(I,K)=0
              ELSE
               LUT(I,K)=IVALS(I,IPOS)
              END IF
  199       CONTINUE
  200    CONTINUE
         IF (IVAL.NE.0) THEN
            IPOS=IPOS+1
            IF (IPOS.GT.7) IPOS=1
         END IF
      END IF
C
C      FINALLY , IF ITYP IS 3 THEN A SET OF CONTOURS AT EQUAL INCREMENTS
C      IS TO BE SEY UP , EACH WITH A DIFFERANT COLOUR
C
      IF (ITYP.EQ.3) THEN
         DO 300 K=CONTOURS(1),CONTOURS(2),CONTOURS(3)
            DO 299 I=1,3
              IF (IVAL.EQ.0) THEN
                 K2=MIN(K+CONTOURS(3)-1,255)
                 DO K1=K,K2
                    LUT(I,K1)=0
                 END DO
              ELSE
                  K2=MIN(K+CONTOURS(3)-1,255)
                  DO K1=K,K2
                     LUT(I,K1)=IVALS(I,IPOS)
                  END DO
              END IF
  299       CONTINUE
            IF (IVAL.NE.0) THEN
               IPOS=IPOS+1
               IF (IPOS.GT.7) IPOS=1
            END IF
  300    CONTINUE
      END IF
C
C     NOW SEND LUT TO THE ARGS
C
      CALL SRCOLS(0,256,LUT)
      IF (ISTAT.NE.0) CALL WRUSER('ERROR WRITING TO ARGS',ISTAT)
      RETURN
      END
      SUBROUTINE CLUT(IN,N,M,OUT)
      INTEGER*4 IN(N,M),OUT(N,M)
      DO J=1,M
         DO I=1,N
            OUT(I,J)=IN(I,J)
         END DO
      END DO
      END
