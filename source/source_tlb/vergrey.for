C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   VERGREY *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               VERGREY   [ RATIO=r ]
C
C
C          FUNCTION:-
C               It generates output which, when plotted  on  the  Versatec,
C               gives  a  greyscale  representation of the input image. The
C               technique used generates a density of dots  in  the  output
C               which  is proportional to the input data value. The pattern
C               of dots is random.
C
C
C          USE:-
C               It gives a satisfactory form of hard copy  of  image  data,
C               but it is rather slow and sometimes disappointing. The data
C               are written to a file called DIT.DAT which is printed on the
C               Versatec automatically when VERGREY terminates. Additional
C               copies may be obtained by typing the following command.
C                     PRINT/QUEUE=LVA0: DIT.DAT
C
C
C
C         USER PARAMETERS:-
C
C         INPUT                               This  is  the  2-D   Starlink
C                                             image which is to be plotted.
C
C         LIMITS          Min,max             These are the  low  and  high
C                                             limits   for  scaling  before
C                                             output.
C
C         TITLE       OUTPUT FROM VERGREY     This  is  a  title  which  is
C                                             plotted  after  the image, up
C                                             to 80 characters.
C
C         TYPE            1                   This defines the  scaling  to
C                                             be  used.  The  default  sets
C                                             LIMIT(1)   to    'white'  and
C                                             LIMIT(2)  to 'black' and uses
C                                             linear scaling; 2  gives  the
C                                             same  limits  but logarithmic
C                                             scaling  between  them.   The
C                                             corresponding    -ve   values
C                                             reverse black and white.
C
C         WIDTH           4.0                 This is the width of the plot
C                                             on  the  Versatec.  It may be
C                                             between 1.0 and 10.5 inches ,
C
C
C                                             but note that   the   time to
C                                             generate    the   output   is
C                                             proportional to the square of
C                                             the  width.  The  actual plot
C                                             will  be  as  close  to  this
C                                             value as possible, as each
C                                             pixel must contain an integral
C                                             number of dots.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         RATIO           1.0                 This  is  the  ratio  of  the
C                                             height  (y)  of the pixels to
C                                             their  width   (x).   A   the
C                                             nearest  value  is used given
C                                             the discrete  nature  of  the
C                                             medium.
C
C         KEEP            DELETE              This determines if the DIT.DAT
C                                             file is to be deleted when the
C                                             output  has  been produced. By
C                                             default it will be deleted. If
C                                             the file is to  be  kept  then
C                                             this parameter must be set  to
C                                             KEEP.
C
C
C         K F Hartley              RGO                            14-JAN-82
C         Modified to use Versatec format plot files              12-APR-85
C         Print command modified for VMS V4                       12-APR-85
C
C--------------------------------------------------------------------------



C      WRITTEN BY K F HARTLEY AT RGO ON 6/11/80
C      THE ALGORITHM USED IS BASED ON THE ASSUMPTION THAT
C      A DENSITY VALUE CAN BE REPRESENTED BY A RANDOM DISTRIBUTION
C      OF DOTS ON THE PAGE.
C      THIS HAS BEEN CALLED A "STOCHASTIC" APPROACH , AS OPPOSED
C      TO USING A FIXED PATTERN OF DOTS FOR EACH DISTINCT GREY LEVEL
C
C      A MORE THOROUGH DESCRIPTION OF THE TECHNIQUE IS GIVEN IN
C      RGO LOCAL USER NOTE 14
C      THE OUTPUT IS SENT TO A FILE "DIT.DAT" ON LOGICAL UNIT 90
C                 N.B. THE WIDTH INPUT IS ONLY A GUIDE .
C                      EACH PIXEL MUST HAVE AN INTEGRAL NUMBER OF DOTS
C                      AND SO THE PROGRAM DOES THE BEST IT CAN TO MAKE
C                      THE PICTURE THE SIZE REQUESTED .
C                      FOR EXAMPLE , IF AN ARRAY HAS 250 ELEMENTS IN EACH
C                      ROW THEN THE ACTUAL WIDTH CAN ONLY BE
C                      250 * I / 200 INCHES , WHERE I = 1,2,3,4,5....,9
C                      UP TO THE MAXIMUM OF 10.5 INCHES.
      INTEGER*4 AXIS(2),STATUS,PIN,TYPE
      REAL*4 LIM(2)
      CHARACTER*1 HEAD(80)
      CHARACTER*6 KEEP
      INCLUDE 'INTERIM(FMTPAR)'
      CHARACTER*80 TITLE
C
C      FIRST GET THE INPUT FRAME
C
      CALL RDIMAG('INPUT',FMT_R,2,AXIS,I,PIN,STATUS)
      IF (STATUS.GT.1.OR.I.NE.2) THEN
         CALL WRERR('DAMN')
         CALL EXIT
      END IF
      TYPE=1
      RATIO=1.0
      WIDTH=4.0
      CALL RDKEYI ('TYPE',.TRUE.,1,TYPE,I,STATUS)
      IF (STATUS.GT.1.OR.I.GT.1.OR.IABS(TYPE).GT.2) THEN
         CALL WRERR('HELL')
         CALL EXIT
      END IF
      CALL RDKEYR('RATIO',.TRUE.,1,RATIO,I,STATUS)
      IF (STATUS.GT.1.OR.I.GT.1.OR.RATIO.LE.0.0) THEN
         CALL WRERR('HELL')
         CALL EXIT
      END IF
      CALL RDKEYR('WIDTH',.TRUE.,1,WIDTH,I,STATUS)
      IF (STATUS.GT.1.OR.I.GT.1.OR.WIDTH.LT.1.0.OR.WIDTH.GT.10.5) THEN
         CALL WRERR('HELL')
         CALL EXIT
      END IF
      CALL MINMAX (%VAL(PIN),AXIS(1),AXIS(2),LIM(1),LIM(2))
  100 CALL RDKEYR('LIMITS',.TRUE.,2,LIM,I,STATUS)
      IF (STATUS.GT.1.OR.I.EQ.1.OR.I.GT.2) THEN
         CALL WRERR('HELL1')
         CALL CNPAR('LIMITS',STATUS)
         IF (STATUS.NE.0) THEN
            CALL EXIT
         ELSE
            GO TO 100
         END IF
      END IF
C
C      PICK UP A TITLE
C
      TITLE='OUTPUT FROM VERGREY'
      CALL RDKEYC('TITLE',.TRUE.,1,TITLE,I,STATUS)
      IF (STATUS.GT.1) THEN
         CALL WRERR('HELL2')
         CALL EXIT
      END IF
      IF (I.NE.0) THEN
         NCH=80
      ELSE
         NCH=19
      END IF
C
C     NOW CONVERT TO FORMAT NEEDED FOR "HEADER"
C
      DO 200 I=1,NCH
         HEAD(I)=TITLE(I:I)
  200 CONTINUE
      KEEP='DELETE'
      CALL RDKEYC('KEEP',.TRUE.,1,KEEP,I,STATUS)
C
C      NOW CALL ROUTINE AND THEN EXIT
C
      CALL VERGREY(%VAL(PIN),AXIS(1),AXIS(2),LIM(1),LIM(2),
     1            TYPE,RATIO,WIDTH,HEAD,NCH)
      CALL FRDATA (' ',STATUS)
      CALL DO_DCL('PRINT/QUEUE=SYS_VERSATEC/PASSALL/NOFEED DIT.DAT',
     :                                                         ISTAT)
      IF (KEEP.EQ.'DELETE') CALL DO_DCL('DELETE DIT.DAT;0',ISTAT)
      CALL EXIT
      END
C
      SUBROUTINE VERGREY(IN,N,M,LOW,HIGH,TYPE,RATIO,WIDTH,TITLE,NCHAR)
C
C
C      THIS ROUTINES ORGANIZES THE CALLS TO THE STOCHASTIC GREYSCALE
C      ROUTINES AS DESCRIBED IN RGO LOCAL USER NOTICE NUMBER 14
C
C      PARAMETERS ARE :-
C          IN   THE INPUT IMAGE OF TYPE R*4 AND DIMENSIONS N BY M
C          LOW  THE LOWER COUNTS LIMIT - BELOW IT THE RESULT IS WHITE
C          HIGH THE UPPER COUNTS LIMIT - ABOVE IT THE RESULT IS BLACK
C          TYPE IS + OR - 1 OR 2 ( SEE LUN 14 )
C          RATIO IS THE RATIO OF PIXEL SIZE IN Y TO SIZE IN X -...
C                ... NORMALLY 1
C          WIDTH IS THE SIZE OF THE PLOT IN INCHES ,
C                MUST BE LESS THAN 10.5 INCHES
C
C          TITLE CONTAINS THE TITLE OF THE PICTURE , CONTAINING
C          NCHAR CHARACTERS
C
      INTEGER*4 TYPE
      REAL*4 IN(N,M)
      REAL*4 ROW(2048),RATIO,WIDTH,LOW,HIGH
      CHARACTER*1 TITLE(80)
C
C      FIRST OPEN FILE DIT.DAT FOR OUTPUT
C      THE VAX USES BYTES TO STORE OUTPUT HENCE "8"
C
      CALL INIT(8)
C
C      NOW SET-UP PARAMETERS NEEDED FOR THIS IMPLEMENTATION
C
      IWORD=8
      NL=264
      NSAMP=N
      TYP=FLOAT(TYPE)
C
C      NOW LOOP THROUGH THE DATA A ROW AT A TIME ,
C      CONVERTING TO REAL AS WE GO
C
C      NOTE THAT THE PICTURE WOULD BE UPSIDE DOWN UNLESS WE
C      INVERT THE ORDER OF THE LINES
C
      DO 200 J=M,1,-1
        DO 100 I=1,N
            ROW(I)=IN(I,J)
  100   CONTINUE
        CALL STOCH (ROW,LOW,HIGH,N,IWORD,NL,TYP,WIDTH,RATIO)
  200 CONTINUE
C
C      NOW PUT OUT A TITLE , FOLLOWED BY SOME BLANK PAPER
C
      CALL GAP(90,264,100)
      CALL HEADER (TITLE,NCHAR)
      CALL GAP(90,264,300)
C
C      NOW CLOSE THE OUTPUT FILE
C
      CALL ENDIT
      RETURN
      END
      SUBROUTINE INIT(IWORD)
C
C ..LINKSTOCH IS THE COMMON BLOCK CONNECTING IPOW AND RAND...
C ..WITH SUBROUTINE STOCH
C
      LOGICAL*1 IPOW(8)
      INTEGER*4 LO
      COMMON /LINKSTOCH/IPOW,RAND(2000)
C
C ..THIS LOOP FILLS ELEMENTS OF IPOW WITH POWERS OF 2...
C
      DO 100 I=2,IWORD
        IPOW(I)=2**(IWORD-I)
  100 CONTINUE
C
C ..IPOW(1) NEEDS SPECIAL CARE...
C ..(OR NOT IN THE CASE OF THE VAX!!!!!)...
C
      IPOW(1)=128
C
C ..THIS LOOP FILLS RAND WITH RANDOM NUMBERS...
C
      LO=0
      DO 200 I=1,2000
        RAND(I)=RAN(LO)
  200 CONTINUE
C
C      THIS OPENS A DATA FILE
C
      OPEN(UNIT=90,NAME='DIT.DAT',STATUS='NEW',
     1 CARRIAGECONTROL='NONE',FORM='UNFORMATTED',RECORDTYPE='VARIABLE')
      RETURN
      END
C
C ..THE SUBROUTINE STOCH PROCESSES A LINE OF DATA...
C ..AND OUTPUTS THE PROCESSED PIXEL IMAGES TO A FILE...
C
      SUBROUTINE STOCH(DAT,FLO,HI,NSAMP,IWORD,NL,TYP,WIDE,RATIO)
C
C ..THE COMMON BLOCK LINKSTOCH LINKS WITH INIT...
C ..THE COMMON BLOCK LINKF LINKS WITH WRITBLOCK...
C
      LOGICAL*1 IPOW(8)
      INTEGER*4 LO
      COMMON /LINKSTOCH/IPOW,RAND(2000)/LINKF/IOUT
      LOGICAL*1 IOUT(264)
      DIMENSION DAT(NSAMP)
      DIMENSION IBIT(2112),DATA(2112)
C
C ..TESTS ARE CARRIED OUT TO CHECK THAT FLO AND HI...
C ..ARE IN THE VALID RANGE PRESCRIBED...
C ..AND THAT TYP IS A VALID NUMBER CODE...
C
      IF ((ABS(TYP).EQ.2.0).AND.(FLO.LT.0.0.OR.HI.LT.0.0)) RETURN
      IF(HI.EQ.FLO)RETURN
      IF(TYP.EQ.999.0)GOTO 999
      IF(ABS(TYP).GT.2.0)RETURN
      IF(TYP.EQ.0.0)RETURN
      IF(FLOAT(INT(TYP)).NE.TYP)RETURN
C
C ..THE VALUES OF FLO AND HI ARE SWAPPED IF...
C ..GIVEN IN REVERSE...
C
  999 IF (HI.GE.FLO) GO TO 50
      TEM=HI
      HI=FLO
      FLO=TEM
   50 CONTINUE
C
C ..IF FLO IS 0.0,FLO IS SET TO A VERY SMALL NUMBER...
C ..TO ALLOW LOGS TO BE TAKEN IF NECCESSARY...
C
      IF(FLO.EQ.0.0)FLO=1E-6
C
C      DATA ARE FIXED IF TYPE= + 2 OR - 2 AND VALUE IS NEGATIVE
C
      IF (ABS(TYP).EQ.2.0) THEN
            DO 60 I=1,NSAMP
               IF (DAT(I).LT.0.0) DAT(I)=-DAT(I)
               IF (DAT(I).EQ.0.0) DAT(I)=1.0E-6
   60       CONTINUE
                           END IF
C
C ..THE WIDTH OF THE IMAGE IS DECIDED...
C
      IWIDE=2112
      IF(WIDE.LE.10.56.AND.WIDE.GE.1.0)IWIDE=WIDE*200.0
      IF(NSAMP.LE.0.OR.NSAMP.GT.2112)RETURN
C
C ..IPIX IS THE NUMBER OF BITS PER PIXEL...
C
      IPIX=IWIDE/NSAMP
      ILENGTH=FLOAT(IPIX)*RATIO
      ISTART=(2112-IWIDE)/2
      IF (ABS(TYP).EQ.2.0) THEN
         DIV1=ALOG(HI)-ALOG(FLO)
         DIV2=-DIV1
      ELSE
         DIV1=1.0
         DIV2=-DIV1
      END IF
      IF(TYP.NE.999.0)GOTO 1000
      DO 123 L=1,NSAMP
        DATA(L)=DAT(L)
  123 CONTINUE
      GOTO 321
C
C ..THIS LOOP SCALES THE DATA OF DAT INTO ARRAY DATA...
C
 1000 DO 100 I=1,NSAMP
C
C ..THE CORRECT TYP OF SCALING IS CHOSEN...
C
        IF (TYP.EQ.1.0) DATA(I)=(DAT(I)-FLO)/(HI-FLO)
        IF (TYP.EQ.-1.0) DATA(I)=(DAT(I)-HI)/(FLO-HI)
        IF (TYP.EQ.2.0) DATA(I)=(ALOG(DAT(I))-ALOG(FLO))/DIV1
        IF (TYP.EQ.-2.0) DATA(I)=(ALOG(DAT(I))-ALOG(HI))/DIV2
  100 CONTINUE
C
C ..THIS LOOP OUTPUTS THE ROW ILENGTH TIMES TO PRODUCE...
C ..PIXELS OF THE DESIRED SHAPE...
C
  321 DO 400 IROW=1,ILENGTH
C
C ..THE POSITION AT WHICH THE POINTER IS SET IS RANDOM...
C
        IPOS=INT(RAN(LO)*1999.0+1.0)
        DO 250 ISET=1,2112
          IBIT(ISET)=0
  250   CONTINUE
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF ELEMENTS OF DAT...
C
        DO 300 ISAMP=1,NSAMP
C
C ..THIS LOOP COUNTS THROUGH THE NUMBER OF BITS IN EACH PIXEL...
C
          DO 200 IBITS=1,IPIX
C
C ..THE POSITION OF THE BIT UNDER CONSIDERATION IS CALCULATED...
C
            K=(ISAMP-1)*IPIX+IBITS+ISTART
C
C ..THE VALUE OF IBIT(K) IS SET ACCORDING...
C ..TO THE PROBABILITY OF THE VALUE OF DATA BEING...
C ..GREATER THAN A RANDOM NUMBER...
C
            IF (DATA(ISAMP).GT.RAND(IPOS)) IBIT(K)=1
C
C ..THE POINTER IS INCREMENTED,AND IF THE END OF THE...
C ..RAND IS REACHED,IT IS SET TO THE START OF RAND...
C
            IPOS=IPOS+1
            IF (IPOS.GT.2000) IPOS=1
  200     CONTINUE
  300   CONTINUE
C
C ..THE WORDS CONTAINING THE BITS TO WRITE TO THE TAPE...
C ..ARE THEN ASSEMBLED FROM THE BITS ALREADY FORMED...
C
        DO 370 IINT=1,NL
          IOUT(IINT)=0
          DO 350 JBIT=1,IWORD
            K=(IINT-1)*IWORD+JBIT
            IF (IBIT(K).EQ.1) IOUT(IINT)=IOUT(IINT)+IPOW(JBIT)
  350     CONTINUE
  370   CONTINUE
C
C ..THE LINE IS NOW WRITTEN TO THE FILE...
C
        CALL WRITBLOK(90,NL)
  400 CONTINUE
  500 RETURN
      END
C
C
      SUBROUTINE HEADER(HEAD,N)
C
C      WRITTEN BY K F HARTLEY      ON 16/9/80
C
C
C      THIS IS ONLY TO BE USED WITH THE OTHER "STOCHASTIC" ROUTINES
C      ************************************************************
C
C      IT ALLOWS A LABEL TO BE PLACED IN THE "DIT.DAT" FILE
C
C      PARAMETERS - HEAD IS A CHARACTER*1 STRING OF  LENGTH N
C
C      IALPH CONTAINS THE BIT PATTERNS TO BE STORED FOR ALL CHARACTERS
C      ALF   CONTAINS THE ASCII FORM , FOR COMPARISON WITH "HEAD"
C      IPOS  IS USED TO STORE THE NUMBER FOR EACH CHARACTER IN "HEAD"
C            INDICATING WHICH ENTRY IN IALPH IS TO BE USED
C      IOUT  IS THE (BYTE) ARRAY USED FOR OUTPUT BY WRITBLOK
C
C      N.B. AS ALLWAYS THE CRAZY ADDRESS SCHEME OF THE VAX ENSURES
C           THAT THE BYTES HAVE TO BE REVERSED.
      LOGICAL*1 IOUT(264),ITEM
      CHARACTER*1 ALF(45),HEAD(80)
      INTEGER*2 IDAT(132),IALPH(20,45),IPOS(81)
      DATA ALF/'A','B','C','D','E','F','G','H','I','J','K','L',
     1         'M','N','O','P','Q','R','S','T','U','V','W','X',
     2         'Y','Z','1','2','3','4','5','6','7','8','9','0',
     3         '.',',',';','+','-','/','*','"',' '/
C   A
      DATA (IALPH(I,1),I=1,20)
     *     /48,120,204,390,4*771,2*1023,6*771,4*0/
C   B
      DATA (IALPH(I,2),I=1,20)
     *    /388,892,774,2*771,774,892,880,892,774,3*771,
     *     774,892,388,4*0/
C   C
      DATA (IALPH(I,3),I=1,20)
     *    /120,254,385,10*768,385,254,120,4*0/
C   D
      DATA (IALPH(I,4),I=1,20)
     *    /1016,1020,774,10*771,774,1020,1016,4*0/
C   E
      DATA (IALPH(I,5),I=1,20)
     *    /2*1023,5*768,2*1016,5*768,2*1023,4*0/
C   F
      DATA (IALPH(I,6),I=1,20)
     *    /2*1023,5*768,2*1016,7*768,4*0/
C   G
      DATA (IALPH(I,7),I=1,20)
     *    /120,254,385,5*768,2*775,3*771,387,254,120,4*0/
C   H
      DATA (IALPH(I,8),I=1,20)
     *    /7*771,2*1023,7*771,4*0/
C   I
      DATA (IALPH(I,9),I=1,20)
     *    /2*120,12*48,2*120,4*0/
C   J
      DATA (IALPH(I,10),I=1,20)
     *    /2*120,11*48,816,224,192,4*0/
C   K
      DATA (IALPH(I,11),I=1,20)
     *    /771,770,774,782,784,816,864,992,992,864,816,
     *         784,782,774,770,771,4*0/
C   L
      DATA (IALPH(I,12),I=1,20)
     *    /14*768,2*1023,4*0/
C   M
      DATA (IALPH(I,13),I=1,20)
     *    /771,903,1007,635,819,11*771,4*0/
C   N
      DATA (IALPH(I,14),I=1,20)
     *  /899,963,835,867,2*803,819,787,795,2*779,2*783,2*775,
     *      771,4*0/
C   O
      DATA (IALPH(I,15),I=1,20)
     *  /113,252,390,10*771,390,252,113,4*0/
C   P
      DATA (IALPH(I,16),I=1,20)
     *  /1008,1016,780,776,2*771,776,780,1016,1008,6*768,4*0/
C   Q
      DATA (IALPH(I,17),I=1,20)
     *  /48,120,220,390,6*771,779,783,398,206,123,49,4*0/
C   R
      DATA (IALPH(I,18),I=1,20)
     *  /1008,1016,780,774,2*771,774,780,1016,1008,880,816,792,
     *      780,774,771,4*0/
C   S
      DATA (IALPH(I,19),I=1,20)
     *   /48,248,396,774,391,192,96,48,24,12,6,3,771,390,252,
     *      124,4*0/
C   T
      DATA (IALPH(I,20),I=1,20)
     *  /2*1023,14*48,4*0/
C   U
      DATA (IALPH(I,21),I=1,20)
     *  /12*771,390,204,120,48,4*0/
C   V
      DATA (IALPH(I,22),I=1,20)
     *  /3*771,3*390,3*204,3*120,4*48,4*0/
C   W
      DATA (IALPH(I,23),I=1,20)
     *  /10*771,3*803,886,476,136,4*0/
C   X
      DATA (IALPH(I,24),I=1,20)
     *  /513,771,258,390,132,204,72,2*48,72,204,132,390,258,
     *   771,513,4*0/
C   Y
      DATA (IALPH(I,25),I=1,20)
     *  /771,390,204,120,12*48,4*0/
C   Z
      DATA (IALPH(I,26),I=1,20)
     *  /2*1023,3,2,6,4,12,56,96,192,128,384,256,
     *         768,2*1023,4*0/
C   1
      DATA (IALPH(I,27),I=1,20)
     *  /16*48,4*0/
C   2
      DATA (IALPH(I,28),I=1,20)
     *  /120,252,390,2*771,774,780,24,48,96,192,384,2*768,
     *   2*1023,4*0/
C   3
      DATA (IALPH(I,29),I=1,20)
     *  /120,252,390,771,6,12,24,2*48,24,12,6,771,392,252,120,4*0/
C   4
      DATA (IALPH(I,30),I=1,20)

     *  /12,28,60,108,204,396,2*780,2*1023,6*12,4*0/
C   5
      DATA (IALPH(I,31),I=1,20)
     *  /2*1023,5*768,1008,1016,12,6,3,771,390,204,120,4*0/
C   6
      DATA (IALPH(I,32),I=1,20)
     *  /24,48,96,192,384,768,888,972,796,774,2*771,390,204,
     *   120,48,4*0/
C   7
      DATA (IALPH(I,33),I=1,20)
     *  /2*1023,3,2,6,4,12,8,24,16,48,32,96,64,192,128,4*0/
C   8
      DATA (IALPH(I,34),I=1,20)
     *  /48,120,204,390,771,390,204,2*120,204,390,771,390,
     *      204,120,48,4*0/
C   9
      DATA (IALPH(I,35),I=1,20)
     *  /120,252,390,2*771,391,255,115,8*3,4*0/
C   0
      DATA (IALPH(I,36),I=1,20)
     *  /120,252,390,2*779,2*787,2*803,2*835,2*899,390,252,120,4*0/
C   .
      DATA (IALPH(I,37),I=1,20)
     *  /14*0,2*48,4*0/
C   ,
      DATA (IALPH(I,38),I=1,20)
     *  /12*0,2*48,2*16,4*0/
C   ;
      DATA (IALPH(I,39),I=1,20)
     *  /6*0,2*48,4*0,2*48,2*16,4*0/
C   +
      DATA (IALPH(I,40),I=1,20)
     *  /5*0,2*48,2*254,2*48,9*0/
C   -
      DATA (IALPH(I,41),I=1,20)
     *  /7*0,2*254,11*0/
C   /
      DATA (IALPH(I,42),I=1,20)
     *  /3,2,6,4,12,8,24,16,48,32,96,64,192,128,384,256,4*0/
C   *
      DATA (IALPH(I,43),I=1,20)
     *  /3*0,819,438,180,120,2*1023,120,180,438,819,7*0/
C   "
      DATA (IALPH(I,44),I=1,20)
     *  /204,408,816,17*0/
C   SPACE
      DATA (IALPH(I,45),I=1,20)
     *  /20*0/
      COMMON /LINKF/IOUT
      EQUIVALENCE (IDAT,IOUT)
      DO 10 I=1,81
         IPOS(I)=45
   10 CONTINUE
      DO 30 I=1,N
         DO 20 K=1,45
            IF(HEAD(I).EQ.ALF(K)) IPOS(I+1)=K
   20    CONTINUE
   30 CONTINUE
      DO 100 J=1,20
      DO 50 I=1,81
      IDAT(I)=IALPH(J,IPOS(I))
   50 CONTINUE
      DO 70 I1=1,264,2
      ITEM=IOUT(I1)
      IOUT(I1)=IOUT(I1+1)
      IOUT(I1+1)=ITEM
   70 CONTINUE

      CALL WRITBLOK(90,264)
  100 CONTINUE
  200 CONTINUE
      RETURN
      END
      SUBROUTINE ENDIT
      COMMON /LINKF/IOUT
      LOGICAL*1 IOUT(264)
      CLOSE(UNIT=90)
      RETURN
      END
      SUBROUTINE WRITBLOK(LU,NL)
C
C      THIS OUTPUTS BINARY RECORDS TO A FILE
C      ASSIGNED TO LOGICAL UNIT LU
C      EACH RECORD IS NL BYTES LONG ( UP TO 264 BYTES OR 2112 BITS)
C
      LOGICAL*1 IOUT(264)
      BYTE BYTE1,BYTE2
      DATA BYTE1,BYTE2/18,0/
      COMMON /LINKF/IOUT
      WRITE (LU) BYTE1,BYTE2,IOUT
      RETURN
      END
      SUBROUTINE GAP(LU,NL,NR)
C
C      USED TO OUTPUT BLANK PAPER TO A VERSATEC PLOTTING FILE
C
      INTEGER*4 LU,NL,NR
      LOGICAL*1 IOUT(264)
      COMMON /LINKF/IOUT
C
C      SET ARRAY TO ZEROS
C

      DO 100 I=1,NL
         IOUT(I)=0
  100 CONTINUE
C
C      NOW WRITE THE BLANK RECORD NR TIMES
C
      DO 200 I=1,NR
         CALL WRITBLOK(LU,NL)
  200 CONTINUE
      RETURN
      END

      SUBROUTINE MINMAX (DATA,N,M,DMIN,DMAX)
      REAL DATA(N,M),DMIN,DMAX

      DMAX=DATA(1,1)
      DMIN=DATA(1,1)
      DO J=1,M
         DO I=1,N
            IF (DATA(I,J).GT.DMAX) DMAX = DATA(I,J)
            IF (DATA(I,J).LT.DMIN) DMIN = DATA(I,J)
         END DO
      END DO
      END
