      subroutine args_lrdc (ib,x,y)

*+  Low-level ARGS calls to read cursor using trackerball and control panel
*   buttons. Loop until a button is pressed and then return button number
*   in 'ib' (0 if error) and cursor position (ARGS units) in '(x,y)'.

      integer ib,x,y
      integer*2 inargs,outargs(3)
*       load trackerball/cursor program into args and send input data to args
	  inargs = 0
	  call load_tbcx
	  call write_tbcx(inargs)


*   execute trackerball/cursor program in args
      call run_tbcx
      call read_tbcx(outargs)
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
