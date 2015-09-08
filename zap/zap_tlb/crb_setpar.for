C
C   SUBROUTINE CRB_SETPAR
C   2 September 1986
C   Allows user to reset or clear parameters
C
	subroutine crb_setpar
	character*20 name
	character*1 ans
										
				
	PRINT *,'Type in the parameter name'
	READ(5,100) NAME
100	FORMAT(A20)
	PRINT *,'Type S to set it, C to clear it'
	READ(5,101) ANS
101	FORMAT(A1)
	call cnpar(name,istat)
	IF(ANS.ne.'C'.and.ANS.ne.'c') THEN
		PRINT *,'Type value for ',NAME
		PRINT *,'Type R, L, C for real, logical or character'
		READ(5,101) ans
		IF(ANS.EQ.'R'.OR.ANS.EQ.'r') THEN
			PRINT *,'Type value'
		call rdkeyr(name,.true.,1,val,iact,istat)
		endif
	endif
	end
