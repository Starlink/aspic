      subroutine per_slowft(x,y,nn,cd,sd,cw,sw,freq,f1,f2,nf,itype)
 
*
*      This is a slow Fourier transform for irregularly
*      spaced data ... in the form of pairs of DOUBLE
*      PRECISION values X,Y ( NN of them )
*
*      The Fourier Transform is computed for NF frequencies
*      from F1 to F2 inclusive.
*      FREQ is used to store the frequencies at which the FT was computed
*
*      Three options are available , selected by ITYPE
*         ITYPE = 1 means compute the FT of the data and store
*                   it in CD and SD ( COS and SIN parts )
*         ITYPE = 2 means compute the window function
*                   ie. the FT of the data sampling , by replacing
*                   each sample by a constant , 1.0
*                   the results are stored in CW and SW
*         ITYPE = 3 means do both of the other two modes.
*
*      The method used to compute the trig functions is
*      Chebyshev recursion as described by Bell in
*      "Intro to Fourier Spectroscopy" , Academic Press ,1970 , p234
*
*      This routine derives from P.G.Murdin  as part of N25C
*      on the RGO ICL 1903T
*      This version is by K F Hartley at RGO
*
      implicit double precision (a-h,o-z)
      real f1,f2
 
*
******* NB THE USE OF DOUBLE PRECISION FOR SAFETY'S SAKE
*
      double precision x(nn),y(nn),cd(nf),sd(nf),cw(nf),sw(nf),freq(nf)
      logical ftdata,ftwind
      character*72 text
 
*
*      FTDATA and FTWIND are two LOGICAL variables , which if .TRUE.
*      demand that the FT of the data and/or window should be computed
*
 
      if (itype.eq.1.or.itype.eq.3) then
         ftdata=.true.
 
      else
         ftdata=.false.
      end if
 
 
      if (itype.eq.2.or.itype.eq.3) then
         ftwind=.true.
 
      else
         ftwind=.false.
      end if
 
 
*
*      Input frequencies are in cycles/day , so ......
*      ......convert to OMEGAS
*
      pi=3.1415926535897932284d0
 
*
******* VALUE OF PI COURTESY OF P J SCOTT
*
      twopi=2.0*pi
      f1=f1*twopi
      f2=f2*twopi
 
*
*      DF is the (constant) frequency increment
*
      df=(f2-f1)/dble(nf-1)
 
*
*     Now zeroize the sums
*
 
      do i=1,nf
         cd(i)=0.0
         sd(i)=0.0
         cw(i)=0.0
         sw(i)=0.0
      end do
 
 
*
*      Loop through all the data values
*
 
      do j=1,nn
 
*
*      Compute the first sines and cosines for the
*      recursion formulae
*
         data = y(j)
         omx=f1*x(j)
         dox=df*x(j)
         szero=sin(omx)
         czero=cos(omx)
         sdom=sin(dox)
         cdom=cos(dox)
         skm2=0.0
         ckm2=1.0
 
*
*     Note that the first two frequncies must be handled separately
*     before the recursion can take over
*
 
         if (ftdata) then
            sd(1)=sd(1)+szero*data
            cd(1)=cd(1)+czero*data
         end if
 
 
         if (ftwind) then
            sw(1)=sw(1)+szero
            cw(1)=cw(1)+czero
         end if
 
 
*
*      If only one frequency , that was all so ...
*
 
         if (nf.eq.1) go to 400
 
*
*     otherwise compute the next frequency
*
         skm1=sdom
         ckm1=cdom
         fac1=szero*ckm1+czero*skm1
         fac2=czero*ckm1-szero*skm1
 
         if (ftdata) then
            sd(2)=sd(2)+fac1*data
            cd(2)=cd(2)+fac2*data
         end if
 
 
         if (ftwind) then
            sw(2)=sw(2)+fac1
            cw(2)=cw(2)+fac2
         end if
 
 
*
*      If only two frequencies , that was all so ...
*
 
         if (nf.eq.2) go to 400
         cdom2=2.0*cdom
 
*
*      otherwise loop through all the other frequencies
*
 
         do k=3,nf
 
*
*      First update SK and CK
*
            sk=skm1*cdom2-skm2
            ck=ckm1*cdom2-ckm2
            fac1=szero*ck+czero*sk
            fac2=czero*ck-szero*sk
 
            if (ftdata) then
               sd(k)=sd(k)+fac1*data
               cd(k)=cd(k)+fac2*data
            end if
 
 
            if (ftwind) then
               sw(k)=sw(k)+fac1
               cw(k)=cw(k)+fac2
            end if
 
 
*
*      Now prepare for next frequency , by updating SKM1,SKM2,CKM1,CKM2
*
            skm2=skm1
            ckm2=ckm1
            skm1=sk
            ckm1=ck
         end do
 
 
*
*     That finishes all the frequncies for THIS data sample
*
*      Encourage the user!
*
 
         if (mod(j,50).eq.0) then
            write (text,900) j
900         format (i5,' data samples processed')
            call wruser(text,istat)
         end if
 
      end do
 
 
*
*      and that finishes all the data samples
*
400   continue
 
*
*      If only 1 or 2 frequencies were required it will branch to here
*
*
*     Now we rescale the FT and set up the frequencies
*
      f=2.0/dble(nn-1)
      fq=f1
      df=(f2-f1)/dble(nf-1)
 
      do i=1,nf
         sd(i)=sd(i)*f
         cd(i)=cd(i)*f
         sw(i)=sw(i)*f
         cw(i)=cw(i)*f
 
*
*      Note conversion back to cycles/day for plotting
*
         freq(i)=fq/twopi
         fq=fq+df
      end do
 
 
*
*      Job done , so (unique) exit
*
 
      end
 
 
 
