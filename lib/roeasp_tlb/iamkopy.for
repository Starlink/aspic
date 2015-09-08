C
C
C
      SUBROUTINE IAMKOPY (OFFSET,FIELDS,IMAGES,PARAMS,IAMLIST,
     :                    RECORDS,RECSIZE,EDRSLIST)
C+
C     IAMKOPY.
C
C     Subroutine to copy an array of ASPIC IAM parameterised
C     data into an array of EDRS XYlist format.
C
C  Given;
C   OFFSET   (I)  Offset at the start of each EDRS record to allow
C                 space for the identifier (words).
C   FIELDS   (I)  No. of paramerters in the IAM dataset.
C   IMAGES   (I)  No. of images in the IAM dataset.
C   PARAMS   (I)  No. of columns in the IAM dataset array (usually
C                 greater than FIELDS.
C   IAMLIST  (RA) Array of IAM parameterised data.
C   RECORDS  (I)  No. of images permitted in the EDRS output dataset.
C   RECSIZE  (I)  Total no. of fields in the EDRS output dataset
C                 (must be at least OFFSET + FIELDS).
C
C  Returned;
C   EDRSLIST (RA) Output EDRS dataset.
C
C  Subroutines called;
C   None.
C
C  A C Davenhall./ROE/                                       5/10/83.
C-
      IMPLICIT NONE
C
      INTEGER OFFSET,IMAGES,PARAMS,FIELDS,RECORDS,RECSIZE
      REAL    IAMLIST(PARAMS,IMAGES),
     :        EDRSLIST(RECSIZE,RECORDS)
C
      INTEGER   IMAGE,INDEX,LEN
      REAL      LWORK
      CHARACTER WORK*7
C
C    Mapping for elements of the IAM arrays into the EDRS list.
C
      INTEGER MAP(15)
      DATA MAP/1,2,12,3,4,5,6,7,8,9,10,11,13,18,19/
      SAVE MAP
C
      CHARACTER BUFFER*20
      REAL      IDENT(5)
      EQUIVALENCE (IDENT(1),BUFFER)
C
C    Copy the images.
C
      DO IMAGE=1,IMAGES
        DO INDEX=1,FIELDS
          EDRSLIST(OFFSET+INDEX,IMAGE)=IAMLIST(MAP(INDEX),IMAGE)
        END DO
C
C    generate and write a unique identifier for this image.
C
        BUFFER=' '
        BUFFER(1:3)='IAM'
        WRITE(WORK,'(I7)') IMAGE
        LWORK=ALOG10(FLOAT(IMAGE))
        LEN=IFIX(LWORK)+1
        BUFFER(4:3+LEN)=WORK(8-LEN:7)
C
C    Note; IDENT and BUFFER are equivalenced.
C
        DO INDEX=1,OFFSET
          EDRSLIST(INDEX,IMAGE)=IDENT(INDEX)
        END DO
      END DO
C
      END
