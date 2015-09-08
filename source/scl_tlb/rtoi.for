!+	Convert contents of 'P1' (a real number)
!	to an integer and put in global symbol 'P2'.
!
!	Will not work if 'P1' is in E notation!
!	If 'P2' is null symbol INTSYM will be set up.
!
!	WFL RGO 15 Oct 1981
!
	IF P2 .EQS. "" THEN P2 := INTSYM
	DOT := 'F$LOCATE(".",P1)'
	'P2' :== 'F$EXTRACT(0,DOT,P1)'
