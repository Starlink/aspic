*+  FITSRG - Read the data of a FITS tape

      SUBROUTINE FITSRG ( MT, SIZE, BPV, PCOUNT, BLKSIZ, ACTSIZ, BUFFER,
     :                    OFFSET, PARAM, DARRAY, STATUS )
*
*    Description :
*
*     This routine reads a data block from the FITS tape that has data
*     in the groups format, and writes the data directly into the HDS
*     data structure.  The values of the parameters associated with the
*     data array are also obtained.
*
*    Invocation :
*
*     CALL FITSRG ( MT, SIZE, BPV, PCOUNT, BLKSIZ, ACTSIZ, BUFFER,
*    :              OFFSET, PARAM, DARRAY, STATUS )
*
*    Arguments :
*
*     MT     = INTEGER ( READ )
*         The tape descriptor
*     SIZE   = INTEGER( READ )
*         Number of elements in the data array
*     BPV    = INTEGER ( READ )
*         The number of bytes per data value
*     PCOUNT    = INTEGER ( READ )
*         The number of parameters associated with the data array
*     BLKSIZ = INTEGER ( READ )
*         The maximum blocksize and dimension of BUFFER
*     ACTSIZ = INTEGER ( READ, WRITE )
*         The actual block size (a multiple of the FITS record length of
*           2880 bytes)
*     BUFFER( ACTSIZ ) = CHAR( READ, WRITE )
*         The buffer containing the block of data. This is only read
*           when OFFSET is non-zero on input, i.e. there are some 
*           unprocessed data within the current block.
*     OFFSET = INTEGER ( READ, WRITE )
*         The number of bytes in the current block already interpreted
*     PARAM( PCOUNT * BPV ) = BYTE ( WRITE )
*         The parameters associated with the data array
*     DARRAY( SIZE * BPV ) = BYTE ( WRITE )
*         The data array in the data structure
*     STATUS  = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Compute number of bytes of parameters to be read
*     Compute offset of data array in its starting block
*     Initialise count of parameter bytes read
*     While not end of parameter data
*       If buffer contains only previously processed information
*         Read a data block into the buffer
*         If an error occurred, report context and exit
*       Endif
*       Find number of bytes in current block remaining to be processed
*       Put the parts of the buffer containing parameters into the
*         parameter array
*       Increment byte count
*       Reset offset of data within input buffer
*     End while
*     Compute number of bytes of data to be read
*     Initialise count of data-array bytes read
*     While not end of data
*       If buffer contains only previously processed information
*         Read a data block into the buffer
*         If an error occurred, report context and exit
*       Endif
*       Find number of bytes in current block remaining to be processed
*       Put the parts of the buffer containing parameters into the
*         data array
*       Increment byte count
*       Reset offset of data within input buffer
*     End while
*     Set offset of bytes read in current block ready for any further
*       group data arrays and their parameters
*     End
*
*    Bugs :
*
*     None known.
*
*    Authors :
*
*     Malcolm J. Currie RAL ( UK.AC.RL.STAR::CUR )
*
*    History :
*
*     1988 Sep 22 : Original ( RL.STAR::CUR ).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'MAG_ERR'       ! MAG-system error definitions

*    Import :

      INTEGER
     :  BLKSIZ,                ! The maximum allowed blocksize on
                               ! the FITS tape
     :  BPV,                   ! Number of bytes per value
     :  MT,                    ! Tape descriptor
     :  PCOUNT,                ! Number of parameters associated with
                               ! the data array
     :  SIZE                   ! Number of elements in the data array


*    Import/export :

      INTEGER
     :  ACTSIZ,                ! The actual blocksizes on the FITS tape
     :  OFFSET                 ! The number of bytes of the input
                               ! block that must be skipped. Zero means
                               ! read a new block.

      BYTE
     :  BUFFER( BLKSIZ )       ! The input buffer

*    Export:

      BYTE 
     :  DARRAY( * ),           ! Data array
     :  PARAM( * )            ! Parameter array

*    Status :

      INTEGER STATUS

*    Local variables :

      INTEGER
     :  DISP,                  ! Displacement pointer
     :  I,                     ! Loop counter
     :  J,                     ! Loop counter
     :  NBT,                   ! Number of bytes in input buffer
                               ! to be transferred to the data array
                               ! or parameters
     :  NBYTES,                ! Number of bytes in data array or 
                               ! parameters
     :  OFSETD,                ! Offset of the data array in the
                               ! buffer (bytes)
     :  IVAL

*-

*    Check for error on entry

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Calculate the number of bytes in the parameters associated with
*    the data array

      NBYTES = BPV * PCOUNT

*    Offset of data array will be OFFSET2 either in this block or a
*    later one

      OFSETD = MOD( OFFSET + NBYTES, ACTSIZ )

*    Initialise the displacement pointer

      DISP = 0

*    Read the blocks of data

      DO WHILE ( DISP .LT. NBYTES )

*       but first are there data already read in the last block of
*       header cards, waiting to be transferred to the parameter array?

         IF ( OFFSET .EQ. 0 ) THEN
            CALL MAG_READ ( MT, BLKSIZ, BUFFER, ACTSIZ, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'DISP', DISP )
               CALL ERR_REP( 'ERR_FITSRG_READP',
     :           'FITSRG: Error ^STATUS reading tape after ^DISP '/
     :           /'bytes read of parameters.', STATUS )
               GOTO 999
            END IF
         END IF

*       Determine number of bytes remaining in current block

         NBT = ACTSIZ - OFFSET

*       Copy the data from the buffer to the parameter array, swapping
*       bytes since a Vax stores bytes in the reverse order to what is
*       on the tape

         DO I = 1, MIN( NBT, NBYTES-DISP ), BPV
            DO J = 1, BPV
               PARAM( DISP+I+J-1 ) = BUFFER( I+BPV-J+OFFSET )
            END DO
         END DO
         DISP = DISP + MIN( NBT, NBYTES-DISP )

*       Input buffer transferred

         OFFSET = 0
      END DO

*    Calculate the number of bytes in the data

      NBYTES = BPV * SIZE

*    Calculate offset into this or a later block at which the next set
*    of group parameters start. NB THIS REPLACES THE LINE COMMENTED OUT
*    AT THE END OF THIS ROUTINE (DSB)

      OFFSET=MOD(OFSETD+NBYTES,ACTSIZ)

*    Initialise the displacement pointer

      DISP = 0

*    Read the blocks of data

      DO WHILE ( DISP .LT. NBYTES )

*       but first are there data already read in the last block of
*       header cards, waiting to be transferred to the array?

         IF ( OFSETD .EQ. 0 ) THEN
            CALL MAG_READ ( MT, BLKSIZ, BUFFER, ACTSIZ, STATUS )
            IF ( STATUS .NE. SAI__OK ) THEN
               CALL MSG_SETI( 'DISP', DISP )
               CALL ERR_REP( 'ERR_FITSRG_READ',
     :           'FITSRG: Error ^STATUS reading tape after ^DISP '/
     :           /'bytes read.', STATUS )
               GOTO 999
            END IF
         END IF

*       Determine number of bytes remaining in current block

         NBT = ACTSIZ - OFSETD

*       Copy the data from the buffer to the data array, swapping bytes
*       since a Vax stores bytes in the reverse order to what is on the
*       tape

         DO I = 1, MIN( NBT, NBYTES-DISP ), BPV
            DO J = 1, BPV
               DARRAY( DISP+I+J-1 ) = BUFFER( I+BPV-J+OFSETD )
            END DO
         END DO
         DISP = DISP + NBT

         OFSETD = 0
      END DO

*    Compute offset in current block for any further group-format 
*    arrays and their parameters

C      OFFSET = ACTSIZ - NBYTES + DISP - NBT  ! This may not be correct

 999  CONTINUE

      END
