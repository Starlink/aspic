      subroutine idenim(ax,ay,id)

C+  IDENIM
C
C    finds out which image in data base point ax,ay is on and returns
C    result in id - setting id=0 if ax,ay on no image or if trouble.
C    if ax,ay on more than one image, return most recent one.
C    so go thru images in reverse order till come to one containing ax,ay.
C    assumed that all entries in the data base are of image type and
C    hence no type checking is performed.
C
C    given (arguments)
C      AX,AY    (I)    point in args coords
C
C    returned (arguments)
C      ID       (I)    db. id number of image point belongs to - 0 if not on an image
C
C      D. Tudhope    ROE     May 1982
C-

      character ALGNAM*(*)
      parameter (ALGNAM='ARGS_DEVICE')
      integer id,ax,ay,status,idmax
      logical args_insid

*   open database file
      call args_dopdb (ALGNAM,status)
      if (status.eq.0) then
*       determine number of records in file (must be > 0)
        call args_qidmx (idmax)
        if (idmax.ne.0) then
*               go through images in reverse order
          do id = idmax,1,-1
            if (args_insid (id,ax,ay)) goto 10
          enddo
C*  image not found
          id=0
        endif
      endif
C*  here if image not found or data base error
      id=0
*   don't need to close database as have only read from it
10    end
