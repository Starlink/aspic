      PROGRAM COPY
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *********************
C                     *                   *
C                     * Program   CATCOPY *
C                     *                   *
C                     *********************
C
C
C
C          CALLING SEQUENCE:-
C               CATCOPY
C
C
C          FUNCTION:-
C               It is  a  non-standard  Starlink  program  which  copies  a
C               selcted  region of the Solar Spectrum Atlas of L Delbouille
C               et al.
C
C
C          USE:-
C               It may be used to copy part of the Atlas from tape to disk,
C               for example as a preliminary to using WAVEGET.
C
C
C
C         USER PARAMETERS:-
C
C         RANGE                               This is  the  start  and  end
C                                             wavelength  of  the  required
C                                             region.
C
C         OUT                                 This  is  the  name  of   the
C                                             output  file  (NB  NOT A .BDF
C                                             FILE).
C
C
C         J V Carey                RGO                            29-JAN-82
C
C
C--------------------------------------------------------------------------



C  J.V.Carey
C  1981 December 14
C

C
C  The first record on the created file indicates the wavelengths
C  covered
C  i.e.     Min wavelength (ex 4300)
C           Max wavelength (ex 4900)
C           The rest of the record is blank
C           The length of the first record is 48 characters
C
      PARAMETER (IN=10)
      PARAMETER (OUT=12)
      INTEGER STATUS, IVAL, A(2), DATA(500)
      CHARACTER FILEOUT(1)*48, HEAD*48, RANGE(2)*4
C
C  Read wavelength range
C
      CALL RDKEYI('RANGE',.FALSE.,2,A,IVAL,STATUS)
C
C  Read name of output file
C
      CALL RDKEYC('OUT',.FALSE.,1,FILEOUT,IVAL,STATUS)
C
C  Open files
C
      OPEN(UNIT=IN,FILE='CATALOG',STATUS='OLD',RECL=2048,READONLY)
      OPEN(UNIT=OUT,FILE=FILEOUT(1),STATUS='NEW',RECL=2048)
C
C  Write the first record
C
      WRITE(HEAD,'(2I4)') A
      WRITE(OUT,'(A)') HEAD
C
C  Position the "CATALOG"
C
      WRITE(RANGE(1),'(I4)') A(1)
      WRITE(RANGE(2),'(I4)') A(2)

      DO I = 1,6
         READ(IN,'(A)') HEAD
      ENDDO

      HEAD(1:4) = '    '

      DO WHILE ( LLT(HEAD(1:4),RANGE(1)))
         READ(IN,'(A)') HEAD
         PRINT *,' SKIPPING RECORD ', HEAD(1:4)
      ENDDO

      BACKSPACE IN
C
C  Now copy until we reach RANGE(2)
C
      DO WHILE ( LLT(HEAD(1:4),RANGE(2)))
         READ(IN,'(A,500I4)') HEAD,DATA
         PRINT *,' COPYING RECORD ', HEAD(1:4)
         WRITE(OUT,'(A,500I4)') HEAD,DATA
      ENDDO

      ENDFILE OUT
      CLOSE ( UNIT=IN)
      CLOSE ( UNIT=OUT)
      CALL EXIT
      END
