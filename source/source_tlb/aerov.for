      PROGRAM AEROV
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   AEROV  *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               AEROV
C
C
C          FUNCTION:-
C               It erases ARGS overlays in a specified bit plane.
C
C
C          USE:-
C               It may be used to erase any graphics or text written to any
C               one of the ARGS overlay planes.
C
C
C         USER PARAMETERS:-
C
C         PLANE           0                   The  bit  plane to be erased.
C                                             Setting the bit plane to zero
C                                             will  erase  planes  all  the
C                                             overlay planes (8-15).
C
C
C         D J King                 RGO                            14-Mar-83
C
C
C--------------------------------------------------------------------------





      INTEGER STATUS,PLANE,I
      DATA PLANE/8/

      CALL SRINIT (0,.FALSE.,STATUS)

      IF (STATUS.EQ.0) THEN
	  PLANE=0
          CALL RDKEYI ('PLANE',.TRUE.,1,PLANE,I,STATUS)
	    IF (PLANE.EQ.0) THEN
		DO I=8,15
		CALL ARGS_CLS(I)
		ENDDO
	    ELSE
		IF (PLANE.LT.8.OR.PLANE.GT.15) THEN
			CALL WRERR('PLANERR')
		ELSE
		        CALL ARGS_CLS (PLANE)
		ENDIF
	    ENDIF
      ENDIF

      END


	subroutine args_cls(n)

*
*   Enable specified plane for writing then clear the plane
*

	integer n,maskn

	maskn=iand(n,'000f'x)
	call args_flush(2)
	call args_s1('ZWE1',ishft('0001'x,maskn))
	call args_s1('CLS1',ishft('0001'x,maskn))
	call srsend

	end


