      subroutine args_strmr (ref1,ref2,m,status)

*+  Set up 2x3 transformation matrix 'm' which will map the three points
*   in 'ref1' onto those in 'ref2'. This is possible unless the three
*   'ref1' points are collinear (or very nearly collinear).
*
*   Status returns are 0 for success and 7 for collinear 'ref1' points.

      integer status
      real ref1(2,3),ref2(2,3),m(2,3),tref1(3,3),tref2(3,3),
     :    iref1(3,3),tm(3,3)

*   calculate inverse of 'ref1'
      call args_cmext (1,ref1,tref1)
      call args_minv3 (tref1,iref1,status)
      if (status.eq.0) then

*       'm' is just 'ref2'*'iref1'
          call args_cmext (1,ref2,tref2)
          call args_mmul (3,3,3,tref2,iref1,tm)
          call args_cmcur (tm,m)
      else

*       if error inverting 'ref1', ensure 'm' is the identity
          call args_cmid (m)
      endif

      end
