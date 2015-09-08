C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C
C                     *******************
C                     *                 *
C                     * Program   ALIST *
C                     *                 *
C                     *******************
C
C
C
C          CALLING SEQUENCE:-
C               ALIST
C
C
C          FUNCTION:-
C               It lists details of the size, location and size of all
C               images held in the ARGS database..
C
C
C          USE:-
C               It should tell you what images are  actually  displayed  on
C               the  ARGS.  If  any non-ASPIC programs have been used it is
C               likely that no entry was made in the ARGS database.
C
C
C         W F Lupton               RGO                             6-JAN-82
C
C
C--------------------------------------------------------------------------



*
*  ROUTINE TO LIST IMAGES AND THEIR PARAMTERS AS CURRENTLY STORED
*  IN THE ARGS DATABASE.
*
*  NO PARAMETERS ARE REQUIRED
*
*  WFL/RGO
*
*

	implicit integer (a-z)
	character cc*4
	call args_dopdb ('ARGS_DEVICE',i)
	call args_qidmx (idmax)
	print *,'Number of images is',idmax
	if (idmax.gt.0) then
	print '(/''  n  type  acx  acy  asx  asy  psx  psy'')'
	endif
	do i=1,idmax
	call args_qdb7p(i,cc,j,k,l,m,n,nn,nnn)
	print '(1x,i2,2x,a,6i5)',i,cc,j,k,l,m,n,nn
	enddo
	end
