	SUBROUTINE AST1
C+
C
C	Subroutine AST1
C
C	This is used by the Control/C Interrupt routines.
C
C	Written by W F Lupton
C
C-
	INCLUDE 'SYS$SYSDEVICE:[STARLOCAL.LIB]INTERRUPT.INC'
	INTFLAG=1
	CALL SETAST
	RETURN
	END
