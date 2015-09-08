      PROGRAM ADISOV
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     ********************
C                     *                  *
C                     * Program   ADISOV *
C                     *                  *
C                     ********************
C
C
C
C          CALLING SEQUENCE:-
C               ADISOV [n]
C
C
C          FUNCTION:-
C               It disables ARGS overlays for a specified bit plane.
C
C
C          USE:-
C               It may be used to erase any graphics or text written to any
C               one of the ARGS overlay planes.
C
C
C         NORMALLY DEFAULTED PARAMETERS:-
C
C         PLANE           8                   The bit plane to be disabled.
C                                             [In   fact   it  is  actually
C                                             8+MOD(PLANE,8) which is used.]
C
C
C         W F Lupton               RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------




*   After execution, all bit-planes will be enabled for writing and the
*   next write will go into all bit-planes.
*
*   The lookup table entry corresponding to the specified plane will be
*   reset on the assumption that the current lookup table is a greyscale.
*   In practice it is unlikely to matter if this is not so.
*

      INTEGER STATUS,PLANE,I
      DATA PLANE/8/

      CALL SRINIT (0,.FALSE.,STATUS)

      IF (STATUS.EQ.0) THEN
          CALL RDKEYI ('PLANE',.TRUE.,1,PLANE,I,STATUS)
          CALL ARGS_OVCL (PLANE,.TRUE.)
      ENDIF

      END
