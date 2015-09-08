      PROGRAM WAVE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   WAVEGET *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               WAVEGET
C
C
C          FUNCTION:-
C               It generates a  Starlink  image  from  a  stellar  spectrum
C               atlas.  Available  atlases  a  the  Solar Spectrum Atlas of
C               Delbouille, Roland and Neven and the Spectrum  of  Arcturus
C               by Griffin.
C
C
C          USE:-
C               It was written  to  generate  input  for  model  atmopshere
C               programs  of  Dickens  et  al.  [it  is  not really part of
C               ASPIC!]
C
C
C
C         USER PARAMETERS:-
C
C         ATLAS                               This is the full file name of
C                                             the atlas to be used.
C
C         RANGE                               This is  the  start  and  end
C                                             wavelength  of  the  required
C                                             range.
C
C         FACTOR          11                  This is the number of samples
C                                             to  be  averaged  together to
C                                             generate one s in the  output
C                                             data.
C
C         OUT                                 This  is  the  name  of   the
C                                             output Starlink image.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C
C
C         J.V.Carey                RGO                            18-FEB-82
C
C
C--------------------------------------------------------------------------



C



C
C  This program gets a selected area of the Photometric Atlas
C  of The Solar Spectrum from 3000 to 10000 angstroms.
C  By L.Delbouille, G.Roland and L.Neven.
C  This version of the atlas covers the wavelengths 4000 to 8000
C  angstroms.
C  There is a fault on the tape from Belgium which formed it. The
C  first two records of the instrumental profile are unreadable
C  and so the instrumental profile starts at the third record and
C  spans 6 records.
C
C  J.V.Carey
C  1981 December 11
C
C  Modification 1. made 1982 February 18 by J.V.Carey
C
C  The program has been modified to accept any spectrum atlas
C  which is formatted in the same way. The first additional atlas
C  to be done is the Spectrum of Arcturus by R.Griffin. The original
C  files were provided by S.P.Caldwell. The format of its records is:
C  Each record is 192 bytes long consisting of 48 characters of header
C  information (most of which is blank) followed by 144 characters
C  containing 36 four digit intensity values.
C
C  The format of the records is as follows:
C  Each record is 2048 bytes long consisting of 48 characters
C  of identification and information followed by 2000 characters
C  containing 500 four digit intensity values.
C
C  Format of the instrumental profile record
C  1 wavelength in Angstroms  (ex. 4680)
C  2 grating order (ex. bb13)
C  3 sampling interval (.2MA)
C  4 UP for the first half (bUPb)
C    DOWN for the second half (DOWN)
C  5 number of points (b500)
C    remaining 28 characters are blank
C
C  n.b. the letter "b" means "blank"
C
C  Format of the Atlas records
C  1 wavelength in Angstroms (ex. 4300)
C  2 grating order (ex. bb13)
C  3 sampling interval (b2MA)
C  4 bbbb (4 "blank")
C  5 number of points (b500)
C  6 Doppler shift (integer part ex.-b10 , +bb8)
C  7 Doppler shift (fractional part ex. .6MA)
C  8 mean air mass (ex. 2.34)
C  9 conditions changing (ex. 0358)
C 10 conditions changing
C    remaining 8 characters blank
C
C   The program prompts for:-
C
C        1   wavelength range required (RANGE) 2 numbers are
C            required, minimum and maximum, they may be given in
C            any order but must not be equal.They will be rounded
C            to the nearest integer above or below respectively
C        2   Number of points which will be averaged to produce one
C            point in the output file (FACTOR)
C        3   Name of file to store averaged data in (OUT)
C            The file is a standard Starlink image file with the
C            following extra information stored in descripters
C            Name of Atlas
C            Wavelength span
C            Value of FACTOR
C            Copies of the "Header information" for each record
C            read from the Atlas
C
      PARAMETER (IN=10)
      INCLUDE 'INTERIM(FMTPAR)'
      REAL A(2), RANGE(2)
      INTEGER IRANGE(2), NSIZE, IDIM(1), IPOINT, STATUS, FACT(1)
      REAL*8 FACTOR,STLAM
      CHARACTER OUT(1)*8, TITLE(2)*72, SPAN(1)*72, HEAD*48
      CHARACTER START(1)*16
      CHARACTER TITLEG(2)*72,catalog*60

      DATA TITLE(1)/'Photometric Atlas of The Solar Spectrum from 3000 t
     :o 10000'/
      DATA TITLE(2)/'Angstroms by L.Delbouille, G.Roland and L.Neven'/
      DATA TITLEG(1)/'Atlas of the spectrum of Arcturus from 3600 to 520
     :0'/
      DATA TITLEG(2)/'Angstroms by R.Griffin'/
      DATA SPAN/'Min Lambda ....... Max Lambda .......  Factor .... '/
      DATA START/'                '/
C
C  Read the name of the Atlas required
C
      CALL RDKEYC('ATLAS',.FALSE.,1,CATALOG,IVAL,STATUS)

C
C  Read the wavelength range in angstroms
C
      CALL RDKEYR('RANGE',.FALSE.,2,A,IVAL,STATUS)
C
C ************* Test STATUS and IVAL *****************
C

C
C  Open the Atlas: it may be in a file on the disc or
C                            in a file on magnetic tape
C
      OPEN(UNIT=IN,FILE=CATALOG,STATUS='OLD',RECL=2048,READONLY)
C
C  "READONLY" used as Fortran77 has no way in the OPEN to set
C  read access only
C

C
C  Read the wavelength range of this particular version of the atlas
C  If this is the full atlas then the first record contains the
C  instrumental profile and we set up RANGE to contain 4006 and 8000
C  we then skip to position the tape before the atlas records start
C
      READ (IN, '(A)' ) HEAD
      IF ( HEAD(1:4).EQ.'4825'.AND.HEAD(14:15).EQ.'UP' ) THEN
         RANGE(1) = 4006.0
         RANGE(2) = 8000.0
         XFCT = 0.002
         DO KK = 1,5
            READ ( IN, '(A)' ) HEAD
         ENDDO
      ELSE
         CALL CTOI( HEAD(1:4), IRANGE(1), STATUS)
         CALL CTOI( HEAD(5:8), IRANGE(2), STATUS)
         CALL CTOI( HEAD(9:12), IPT, STATUS)
         RANGE(1) = IRANGE(1)
         RANGE(2) = IRANGE(2)
         IF (IPT.EQ.0) THEN
            XFCT = 0.002
         ELSE
            XFCT = 1.0/REAL(IPT)
         ENDIF
      ENDIF
C
C  We now test if valid data has been input into "A"
C  First we sort "A" into order A(1) will contain the minimum
C  and A(2) will contain the maximum
C  If they are equal we make A(2)=A(1)+1 and inform the user
C
      IF ( A(1).EQ.A(2)) THEN
         A(2) = A(1) + 1.0
      ELSEIF (A(1).GT.A(2)) THEN
         ITEMP = A(2)
         A(2)  = A(1)
         A(1)  = ITEMP
      ENDIF
      IF ( A(1).GE.RANGE(1).AND.A(1).LT.RANGE(2).AND.
     :     A(2).GT.RANGE(1).AND.A(2).LE.RANGE(2) )    THEN

C
C  Read the compression factor
C
         FACT(1) = 11
         CALL RDKEYI('FACTOR',.TRUE.,1,FACT(1),IVAL,STATUS)
C
C ********* We ought to do some more tests here *************
C

C
C  Read name of output file = OUT done implicitly by WRIMAG
C

C
C  Work out array size and a few other things and then set
C  up the image
C
         FACTOR = DBLE(FACT(1))
         NSIZE = INT ( DBLE(A(2) - A(1)) / ( FACTOR * XFCT ))
         IDIM(1) = NSIZE
         STLAM = DBLE(A(1)) - FACTOR * XFCT * 0.5
C
C  Test if STLAM comes before RANGE(1) and make it equal to it
C  if it does
         IF (STLAM.LT.DBLE(RANGE(1))) THEN
            STLAM = DBLE(RANGE(1))
         ENDIF
         CALL WRIMAG ( 'OUT', FMT_SW, IDIM, 1, IPOINT, STATUS)
C
C  Write header information
C
         IF(XFCT.EQ.0.002) THEN
         CALL WRDSCR ( 'OUT', 'HEAD', TITLE, 1, STATUS)
         ELSE
         CALL WRDSCR ( 'OUT', 'HEAD', TITLEG,1, STATUS)
         ENDIF
         WRITE(SPAN(1)(12:18),'(F7.2)') A(1)
         WRITE(SPAN(1)(31:38),'(F7.2)') A(2)
         WRITE(SPAN(1)(47:50),'(I4)') FACT(1)
         CALL WRDSCR ( 'OUT', 'SPAN', SPAN,1, STATUS)
         WRITE(START,'(D16.8)') STLAM
         CALL WRDSCR ( 'OUT', 'START', START, 1,STATUS)
C
C  Create the data points in the image
C
         CALL AVERAGE ( %VAL(IPOINT), NSIZE, A, FACT(1), STLAM, IN,
     :                  XFCT)
      ELSE
         CALL WRERR ( 'WAVERONG' )
      ENDIF
      CALL EXIT
      END

C
C  ***************************************************************
C  ******************* SUBROUTINE AVERAGE ************************
C  ***************************************************************
C
      SUBROUTINE AVERAGE ( IMAGE, NSIZE, A, FACTOR, STLAM, IN,
     :                     XFCT)
C
C  This subroutine performs the reading of the intensity values
C  from the atlas, compresses the values by FACTOR and writes
C  to the array IMAGE
C
      INTEGER*2 IMAGE(NSIZE)
      INTEGER FACTOR,LAM,STATUS,POINTS
      REAL*8 DATA(500,0:1), SUM, STLAM
      REAL A(2)
      CHARACTER HEAD(1)*48, CLAM*4

C
C  Begin here
C
      HEAD(1)(1:4) = '    '

C
C  STLAM is starting point for compression
C
      LAM = INT( STLAM)
      WRITE ( CLAM, '(I4)' ) LAM
C
C  Position the tape at this point
C
      DO WHILE ( CLAM.NE.HEAD(1)(1:4))
         READ ( IN, '(A)' ) HEAD
      ENDDO

C
C  Back one record
C
      BACKSPACE IN
C
C  Now do the important bit
C
C     WE TEST HERE HOW MANY POINTS ARE IN THE RECORD. THIS DEPENDS
C     ON WHICH ATLAS WE ARE READING. THE SOLAR ATLAS HAS 500 POINTS
C     AND THE ARCTURUS ATLAS BY GRIFFIN HAS 36 POINTS
C
         CALL CTOI( HEAD(1)(17:20), POINTS, STATUS)
      LAM = A(2) + 1
      WRITE ( CLAM, '(I4)' ) LAM
C
C  Some definitions
C           INDEX   determines ehich part of DATA to use
C           SUM     used while summing DATA
C           J       index used for writing to IMAGE
C           N       index used while compressing
C           K       start of DO for picking out points from DATA
C
      INDEX = 0
      SUM = 0
      J   = 1
      N   = 0
C
C  Calculate initial value of K
C
      FRAC = MOD(A(1),1.0)
      IF ( FRAC.LT.REAL(FACTOR)*0.5) THEN
         IF( STLAM.EQ.A(1)) THEN
            K = 1
         ELSE
            K =  POINTS - FACTOR/2 + 1
         ENDIF
      ELSE
         K = INT(FRAC/XFCT) - FACTOR/2
         IF ( K.LT.0 ) THEN
            K = K + POINTS
         ENDIF
      ENDIF

      DO WHILE ( J.LE.NSIZE )

         READ ( IN, '(A,500F4.0)' ) HEAD, ( DATA(I,INDEX),I=1,POINTS)
C
C  Write header information to the image OUT as descriptor name
C  RECORD
C

         CALL ADDSCR ( 'OUT', 'RECORD', HEAD, 1,STATUS)

            DO I=K,POINTS
               SUM = SUM + DATA(I,INDEX)
               N = N + 1
               IF (N.GE.FACTOR) THEN
                  IMAGE(J) = SUM/REAL(FACTOR)
                  J = J + 1
                  N = 0
                  SUM = 0.0
               ENDIF
            ENDDO
         K = 1
         INDEX = IEOR ( INDEX, 1)
      ENDDO

      END
