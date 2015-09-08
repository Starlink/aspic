      subroutine args_lrdcto (ib,x,y)

*+  Low-level ARGS calls to read cursor using trackerball and control panel
*   buttons. Loop until a button is pressed and then return button number
*   in 'ib' (0 if error) and cursor position (ARGS units) in '(x,y)'.
*   If tracker ball is not moved in a set time then a timeout will
*   occur and 'ib' will be 0

      integer ib,x,y
      integer*2 inargs,outargs(3)
*       load trackerball/cursor program into args and send input data to args
	  inargs = 0
	  call load_tbto
	  call write_tbto(inargs)

*   execute trackerball/cursor program in args
      call run_tbto
      call read_tbto(outargs)
      x = outargs(1)
      y = outargs(2)
      ib = 0
      if (outargs(3).eq.8)    ib = 1
      if (outargs(3).eq.16)   ib = 2
      if (outargs(3).eq.32)   ib = 3
      if (outargs(3).eq.64)   ib = 4
      if (outargs(3).eq.24)   ib = 12
      if (outargs(3).eq.40)   ib = 13
      if (outargs(3).eq.72)   ib = 14
      if (outargs(3).eq.48)   ib = 23
      if (outargs(3).eq.80)   ib = 24
      if (outargs(3).eq.96)   ib = 34
      if (outargs(3).eq.56)   ib = 123
      if (outargs(3).eq.88)   ib = 124
      if (outargs(3).eq.104)  ib = 134
      if (outargs(3).eq.112)  ib = 234
      if (outargs(3).eq.120)  ib = 1234

      end
