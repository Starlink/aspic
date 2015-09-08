C
C          PROGRAM ZAP
C          CHRIS BENN  RGO  31 APRIL 1986
C          PILE OF ASPIC ROUTINES
C
C          Note that around line 330 is subroutine AFLASH2
C          which is out of order - called by AFLASH
C
      INTEGER IDIMN(99)
      LOGICAL TRIM,LLOG
	character*1 ans1
      CHARACTER VALUE*80
      CHARACTER*1 CRBANS
	character*6 ans
	CHARACTER*70 command
	INTEGER ISTAT
      INCLUDE 'INTERIM(FMTPAR)'
      INCLUDE 'INTERIM(ERRPAR)'
101   FORMAT(/,(1X,18('ZAP '),/))
	icurrent_image=0
	WRITE(*,702)
702   FORMAT(
     1' ZAP>',/,
     1' ZAP>  Welcome to the ZAP environment for ASPIC',/,
     1' ZAP>',/,
     5' ZAP>  ZAP stands for Zero-delay ASPIC Processor ',
     1' -  it''s much faster than DSCL',/,
     1' ZAP>',/,
	1  ' ZAP>  Type $command (e.g. $dir) for DCL commands',/,
	1  ' ZAP>',/,
	1  ' ZAP>  Please report bugs to Chris Benn (RGVAD::CRB)',
     :/,' ZAP>')
200	if(icurrent_image.eq.0) call cnpar('IMAGE',istat)
	pRINT *,'ZAP>  Enter command name (or press RETURN ',
	1   'for menu)'
	write(*,149)
149	format(' ZAP>  ',$)
103   FORMAT(/,
     7' Select a program by name or number (0 to stop):',//,
     1'  1 ADHC     2 ADISOV   3 ADISP    4 AFLASH   5 AHARDCOPY',/,
     2'  6 APAN     7 APERASP  8 ARESET   9 ASAVE   10 ATEXT',/,
     3' 11 CCD     12 CONTOUR 13 DESCR   14 GREYSCAL15 INCARN',/,
     4' 16 INSPECT 17 LIST    18 LUTCOL  19 LUTE    20 LUTGREY',/,
     5' 21 LUTLIN  22 LUTROT  23 PEEP    24 PICK    25 SLICE',/,
     6' 26 STARFIT 27 STARMAG 28 STATS   29 TBXY    30 XYCURA',/,
     7' 31 XYCURB  32 XYDRAWA 33 XYLIST        [24 Aug 1986]  ',/,
     1' 34 XYEDIT  35 APERCUR 36 ADD10   ',/,
	1' 40 SEARCHPIX',' 41 FITSINDEX',' 42 IAMFILE',' 42 JAMFILE',/,
	1 ' 43 BLANK',/,'  44 CCD17',/,
	1 ' 50 HELP   51 RGO     98 CURRENT 99 IMAGE',//,
     :' Notes:',/
     1' 1) Prefer ADISP (AFLASH mucks up overlay planes)',
     1/,
     1' 2) Use commands 8, 20 or 2 to tidy up the display',
     :/,' 3) ASPIC programs sometimes crash on first accessing',
     :' an image',/,
     :'    If these, or any other, errors occur, exit from ZAP',
     :' and start anew',//)
	READ(*,135) command
135   FORMAT(a70)
	CALL STR$UPCASE(command,command)
	if(command(1:1).eq.'$') then
		istat=lib$spawn(command)
	else

	ans=command(1:6)
      IF(ANS.EQ.' ') WRITE(*,103)
	IF(ANS.EQ.'0 '.OR.ANS.EQ.'EXIT') WRITE(*,101)
	IF(ANS.EQ.'0 '.OR.ANS.EQ.'EXIT') STOP
	IF(ANS.EQ.'1 '.OR.ANS.EQ.'ADHC') CALL CRB_ADHC
	IF(ANS.EQ.'2 '.OR.ANS.EQ.'ADISOV') CALL CRB_ADISOV
	IF(ANS.EQ.'3 '.OR.ANS.EQ.'ADISP') then
		call crb_areset
		call crb_adisp
	endif
	IF(ANS.EQ.'4 '.OR.ANS.EQ.'AFLASH') CALL CRB_AFLASH
	IF(ANS.EQ.'5 '.OR.ANS.EQ.'AHARDC') CALL CRB_AHARDCOPY
	IF(ANS.EQ.'6 '.OR.ANS.EQ.'APAN') CALL CRB_APAN
	IF(ANS.EQ.'7 '.OR.ANS.EQ.'APERAS') CALL CRB_APER
	IF(ANS.EQ.'8 '.OR.ANS.EQ.'ARESET') CALL CRB_ARESET
	IF(ANS.EQ.'9 '.OR.ANS.EQ.'ASAVE') CALL CRB_ASAVE
	IF(ANS.EQ.'10'.OR.ANS.EQ.'ATEXT') CALL CRB_ATEXT
	IF(ANS.EQ.'11'.or.ans.eq.'CCD   ') then
		print *,'Type P or S for:'
		print *,'P  Prime-focus CCD'
		print *,'S  Spectroscopy (with pre-flash)'
		read(5,105) ans1
105 	format(a1)
		if(ans1.eq.'P'.or.ans1.eq.'p') call ccdp
		if(ans1.eq.'S'.or.ans1.eq.'s') call ccds
	endif
	IF(ANS.EQ.'12'.OR.ANS.EQ.'CONTOU') CALL CRB_CONTOUR
	IF(ANS.EQ.'13'.OR.ANS.EQ.'DESCR') CALL CRB_DESCR
	IF(ANS.EQ.'14'.OR.ANS.EQ.'GREYSC') CALL CRB_GREYSCALE
	IF(ANS.EQ.'15'.OR.ANS.EQ.'INCARN') CALL CRB_INCARN
	IF(ANS.EQ.'16'.OR.ANS.EQ.'INSPEC') CALL CRB_INSPECT
	IF(ANS.EQ.'17'.OR.ANS.EQ.'LIST') CALL CRB_LIST
	IF(ANS.EQ.'18'.OR.ANS.EQ.'LUTCOL') CALL CRB_LUTCOL
	IF(ANS.EQ.'19'.OR.ANS.EQ.'LUTE') CALL CRB_LUTE
	IF(ANS.EQ.'20'.OR.ANS.EQ.'LUTGRE') CALL CRB_LUTREAD
	IF(ANS.EQ.'21'.OR.ANS.EQ.'LUTLIN') CALL CRB_LUTLIN
	IF(ANS.EQ.'22'.OR.ANS.EQ.'LUTROT') CALL CRB_LUTROT
	IF(ANS.EQ.'23'.OR.ANS.EQ.'PEEP') CALL CRB_PEEP
	IF(ANS.EQ.'24'.OR.ANS.EQ.'PICK') CALL CRB_PICK
	IF(ANS.EQ.'25'.OR.ANS.EQ.'SLICE') CALL CRB_SLICE
	IF(ANS.EQ.'26'.OR.ANS.EQ.'STARFI') CALL CRB_STARFI
	IF(ANS.EQ.'27'.OR.ANS.EQ.'STARMA') CALL CRB_STARMA
	IF(ANS.EQ.'28'.OR.ANS.EQ.'STATS') CALL CRB_STATS
	IF(ANS.EQ.'29'.OR.ANS.EQ.'TBXY'.OR.ANS.EQ.'XY  '
     1 ) CALL CRB_TBXY
	IF(ANS.EQ.'30'.OR.ANS.EQ.'XYCURA') CALL CRB_XYCURA
	IF(ANS.EQ.'31'.OR.ANS.EQ.'XYCURB') CALL CRB_XYCURB
	IF(ANS.EQ.'32'.OR.ANS.EQ.'XYDRAW') CALL CRB_XYDRAWA
	IF(ANS.EQ.'33'.OR.ANS.EQ.'XYLIST') CALL CRB_XYLIST
	    IF(ANS.EQ.'34'.OR.ANS.EQ.'XYEDIT') CALL CRB_XYEDIT
	if(ans.eq.'35'.or.ans.eq.'APERCU') CALL CRB_APERCUR
	if(ans.eq.'36'.or.ans.eq.'ADD10 ') CALL ADD10
	if(ans.eq.'40'.or.ans.eq.'SEARCH') call searchpix
	if(ans.eq.'41'.or.ans.eq.'FITSIN') call crb_fitsindex
	if(ans.eq.'42'.or.ans.eq.'IAMFIL'.
	1   or.ans.eq.'JAMFIL') call crb_iamfile
	if(ans.eq.'43'.or.ans.eq.'BLANK') call crb_blank
	if(ans.eq.'44'.or.ans.eq.'CCD17') call crb_CCD17
	IF(ANS.EQ.'50'.OR.ANS.EQ.'HELP') CALL HELPADVICE(1)
	IF(ANS.EQ.'51'.OR.ANS.EQ.'RGO   ') CALL HELPADVICE(2)
	IF(ANS.EQ.'98'.OR.ANS.EQ.'CURREN') then
		Print *,'Type 1 to make current image the',
	1 ' default for future commands'
		Print *,'Type 0 to remove default'
		read(5,*) icurrent_image
	endif
	IF(ANS.EQ.'99'.OR.ANS.EQ.'IMAGE') THEN
	CALL CNPAR('IMAGE',ISTAT)
	CALL RDIMAG('IMAGE',102,99,IDIMN,
     1NDIMS,IPIN,JSTAT)
	ELSE
	ENDIF
	endif
	go to 200
	END
