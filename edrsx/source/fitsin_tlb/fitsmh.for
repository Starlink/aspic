*+  FITSMH - Process the header on a FITS tape

      SUBROUTINE FITSMH( BUFFER, BITPIX, NDIM, AXIS, DARRAY, NONSDA,
     :                   SIZE, STATUS )
*
*    Description :
*
*     This routine searches for the mandatory FITS header cards
*     stored in a buffer, and their values are returned, if they
*     are present and in the correct order.  Should an item be missing
*     or have an unsupported value an error is reported, a bad status
*     is set and the routine exits. This version supports mandatory
*     descriptors that are not in the correct order.
*
*     Currently, only simple FITS and group-format FITS are supported.
*
*     The number of dimensions is reduced when the highest dimension is
*     one.
*
*    Invocation :
*
*     CALL FITSMH( BUFFER, BITPIX, NDIM, AXIS, DARRAY, NONSDA, SIZE,
*                  STATUS )
*
*    Arguments :
*
*     BUFFER( * ) = CHAR( READ, WRITE )
*         The buffer for reading a tape block into
*     BITPIX = INTEGER ( WRITE )
*         The number of bits per pixel of the data array
*     NDIM = INTEGER ( WRITE )
*         The number of active dimensions
*     AXIS( DAT__MXDIM ) = INTEGER ( WRITE )
*         The dimensions of the data array
*     DARRAY = LOGICAL ( WRITE )
*         If true there is a data array
*     NONSDA = LOGICAL ( WRITE )
*         If true the data array is non-standard
*     SIZE = INTEGER ( WRITE )
*         The number of pixels in the (or each) data array
*     STATUS = INTEGER( READ, WRITE )
*         Global status value
*
*    Method :
*
*     Check for error on entry - return if not o.k.
*     Check for extension
*     If present report that it is unsupported, set bad status and
*       return
*     If tape is not SIMPLE, report that, set bad status and return
*     Locate and decode BITPIX value and check status - report context
*       and return if bad
*     If BITPIX not present, report error, set bad status and return
*     Check that BITPIX has valid value
*     Locate and decode NAXIS value and check status - report context
*       and return if bad
*     If NAXIS not present, report error, set bad status and return
*     Check that NAXIS has valid value
*     Initialise size counter
*     If NAXIS is not 0 then
*        For each AXIS
*           Generate name of the NAXISn keyword
*           Locate and decode AXISn value, and check status - report
*             context and return if bad
*           If AXISn not present report error, set bad status and return
*           Check that NAXISn has a valid value
*           Look for AXIS1=0, which is group format
*           If group format set nonstandard flag to true
*           If AXISn value is meaningless or non-integer, report error,
*             set bad status and return
*           Increment the number of pixels in the array so far (unless
*             AXIS1=0)
*        Endfor
*     Else
*        Set flag to say no data array is present
*     Endif
*     Remove any redundant (set to 1) higher dimensions 
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
*     1988 Sep 16 : First version ( RL.STAR::CUR ).
*     1988 Sep 25 : Allow BITPIX, NAXIS and NAXISn to appear in any
*                   order in the header ( RL.STAR::CUR ).
*
*    Type definitions :

      IMPLICIT  NONE           ! no default typing allowed

*    Global constants :

      INCLUDE  'SAE_PAR'       ! SSE global definitions
      INCLUDE  'PRM_PAR'       ! Magic-value definitions

*    Import :

      CHARACTER*(*)
     :  BUFFER(*)              ! FITS tape buffer


*    Export :

      INTEGER
     :  BITPIX,
     :  NDIM,
     :  SIZE,
     :  AXIS(DAT__MXDIM)

      LOGICAL
     :  DARRAY,
     :  NONSDA

*    Status :

      INTEGER STATUS

*    Local variables :

      LOGICAL                  ! True if:
     :  THERE,                 ! There is a card with the requested
     :                         ! keyword in the header
     :  IPAC		       ! The tape is in non-standard IPAC format

      CHARACTER
     :  CNDIM*3,               ! Axis number
     :  NAXNAM*8               ! NAXISn keyword name

      INTEGER
     :  N, NAX,                ! Loop counters
     :  NC,                    ! Number of characters in axis number
     :  TPSTAT                 ! Temporary status
 
*-

*    Check for error on entry

      IF ( STATUS .NE. SAI__OK ) RETURN

*    Unfortunately, no FITS extensions are currently supported

      IF ( BUFFER( 1 )(1:8) .EQ. 'XTENSION' ) THEN
         CALL ERR_OUT( 'ERR_FITSMH_NOEXT',
     :     'FITSMH: FITS extensions not supported', STATUS )
         STATUS = SAI__ERROR
         GOTO 990
      END IF

*    Check the tape is simple FITS

      IF ( BUFFER( 1 )(1:6) .NE. 'SIMPLE' .OR.
     :     BUFFER( 1 )( 30:30 ) .NE. 'T' ) THEN
         CALL ERR_OUT( 'ERR_FITSMH_SIMPLE',
     :     'FITSMH: SIMPLE not present or in wrong '/
     :     /'order, or the format is not simple', STATUS )
         STATUS = SAI__ERROR
         GOTO 990
      END IF

*    Continue checking the mandatory descriptors... BITPIX is
*    number of bits per pixel

      CALL FITSGI( BUFFER, 'BITPIX', THERE, BITPIX, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         TPSTAT = STATUS
         CALL ERR_OUT( 'ERR_FITSMH_FBITPI',
     :     'FITSMH: Error evaluating BITPIX', STATUS )
         STATUS = TPSTAT
         GOTO 990
      END IF

*    See if there was a BITPIX header card

      IF ( .NOT. THERE ) THEN
         CALL ERR_OUT( 'ERR_FITSMH_BITPIX',
     :     'FITSMH: BITPIX not present', STATUS )
         STATUS = SAI__ERROR
         GOTO 990
      END IF

*    BITPIX can currently only have values of 8, 16 and 32. Later
*    -32 will probably mean 32-bit floating point.

      IF ( BITPIX .NE. 8 .AND. BITPIX .NE. 16 .AND.
     :     BITPIX .NE. 32 ) THEN
         CALL ERR_OUT( 'ERR_FITSMH_IBITPI',
     :     'FITSMH: BITPIX is not 8, 16 or 32.', STATUS )
         STATUS = SAI__ERROR
         GOTO 990
      END IF

*    Now to the number of dimensions

      CALL FITSGI( BUFFER, 'NAXIS', THERE, NDIM, STATUS )

      IF ( STATUS .NE. SAI__OK ) THEN
         TPSTAT = STATUS
         CALL ERR_OUT( 'ERR_FITSMH_INAXIS',
     :     'FITSMH: Error converting NAXIS', STATUS )
         STATUS = TPSTAT
         GOTO 990
      END IF

      IF ( .NOT. THERE ) THEN
         CALL ERR_OUT( 'ERR_FITSMH_NAXIS',
     :     'FITSMH: NAXIS not present', STATUS )
         STATUS = SAI__ERROR
         GOTO 990
      END IF

*    Zero dimension means there is no main data array.  Upper limit
*    imposed by the data system

      IF ( NDIM .LT. 0 .OR. NDIM .GT. DAT__MXDIM ) THEN
         CALL MSG_SETI( 'NAXIS', NDIM )
         CALL ERR_OUT( 'ERR_FITSMH_INAXIS',
     :     'FITSMH: Cannot process NAXIS = ^NAXIS', STATUS )
         STATUS = SAI__ERROR
         GOTO 990
      END IF

      NONSDA = .FALSE.
      DARRAY = .TRUE.

*    Decode NAXISn values

      SIZE = 1

*    Allow for the case when there is no data array

      IF ( NDIM .GT. 0 ) THEN

*       For each dimension

         DO N = 1, NDIM

*          Generate name of the Nth axis dimension, NAXISn

            CALL CHR_ITOC( N, CNDIM, NC )
            NAXNAM = 'NAXIS'//CNDIM( :NC )

*          Get value of NAXISn

            CALL FITSGI( BUFFER, NAXNAM, THERE, AXIS( N ), STATUS )

            CALL MSG_SETI( 'N', N )
            IF ( STATUS .NE. SAI__OK ) THEN
               TPSTAT = STATUS
               CALL ERR_OUT( 'ERR_FITSMH_FAXISN',
     :              'FITSMH: Error converting AXIS^N value', STATUS )
               STATUS = TPSTAT
               GOTO 990
            END IF

            IF ( .NOT. THERE ) THEN
               CALL ERR_OUT( 'ERR_FITSMH_NAXISn',
     :           'FITSMH: NAXIS^N not present', STATUS )
               STATUS = SAI__ERROR
               GOTO 990
            END IF


*          File may be in group format, where AXIS1=0

            IF ( N .EQ. 1 .AND. AXIS( N ) .EQ. 0 ) THEN

*          So there is a non-standard data-array structure present

               NONSDA = .TRUE.
            END IF

*          Check that it is physical or does not have integer value

            IF ( AXIS( N ) .LT. 1 .AND. ( .NOT. NONSDA ) ) THEN
               CALL MSG_SETI( 'AXIS', AXIS( N ) )
               CALL ERR_OUT( 'ERR_FITSMH_AXISIZ',
     :           'FITSMH: Axis ^N has invalid dimension ^AXIS', STATUS )
               STATUS = SAI__ERROR

*          Tidy and abort

               GOTO 990
            END IF

*          Evaluate number of pixels in array

            IF ( AXIS ( N ) .GT. 0 ) SIZE = SIZE * AXIS( N )
         END DO
      ELSE

*       No data array

         DARRAY = .FALSE.
         AXIS( 1 ) = VAL__BADI
      END IF

*-----------------------------------------------------------

*    This section was inserted by D. Berry to provide a quick way
*    of reading FITS GROUP tapes from the IRAS processing centre at
*    Pasadena (IPAC). They generate tapes in which there are a set
*    of higher dimensions set to 1. In fact these tapes should be
*    interpreted by setting NAXIS2 equal to the number dimensions
*    which are set to 1, and setting NAXIS3 equal to the old value
*    of NAXIS2

*    Check that all dimensions above NAXIS2 are set to 1. If not, then
*    this tape is not of the format being considered here.

      IPAC=.TRUE.
      DO NAX=3,NDIM
         IF(AXIS(NAX).NE.1) IPAC=.FALSE.
      ENDDO            
      
*    If this could be an IPAC tape, set NAXIS2 equal to the number of
*    redundant dimensions and set NAXIS3 equal to the old value of 
*    NAXIS2. Also update the size of each data array.

      IF(IPAC) THEN
         AXIS(3)=AXIS(2)
         AXIS(2)=NDIM-2
         SIZE=AXIS(3)*AXIS(2)
         CALL MSG_OUT('IPAC_TAPE','*** File is in non-standard IPAC '//
     :                'format',STATUS)
      ENDIF

*-----------------------------------------------------------

*    Occasionally higher dimensions may be set to one.
*    Remove such useless dimensions.

      DO NAX = NDIM, 1, -1
         IF ( AXIS( NAX ) .NE. 1 ) GOTO 950
      END DO

      NAX = 1
  950 CONTINUE
      NDIM = NAX

  990 CONTINUE

      END
