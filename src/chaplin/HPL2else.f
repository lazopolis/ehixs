C=============================================================================
C---  HPLs  of Rank 2  
C=============================================================================

      double  complex function HPL2else(n1,n2,x)
      implicit none 
      double precision pi,ll2,xre
      double complex x, ris,myi,ll1x,ll1mx,llx
      double complex basis2_1,basis2_2,basis2_3
      integer n1,n2,j,bcflag

      pi=3.1415926535897932385D0
      myi=dcmplx(0d0,1d0)
      bcflag = 0

      ll2 = dlog(2d0)
      j = 3*(n1+1) + (n2+1) +1

      ris=dcmplx(0d0,0d0)
      
c---  +i*epsilon to get branch cuts right ---
      if (dimag(x).eq.0d0) then
         x = x + dcmplx(0d0,1d-60)
         bcflag = 1
      endif
c---  
      ll1x = log(1d0+x)
      ll1mx = log(1d0-x)
      llx = log(x)
      
      select case(j)

c     #####################################################
c     basis2_1(z) = cli2(z)
c     basis2_2(z) = cli2(-z)) 
c     basis2_3(z) = cli2((1-z)/2)
c     #####################################################

      case(1)
         ris=ll1x**2/2d0
      case(2)
         ris=basis2_2(x) + llx*ll1x
      case(3)
         ris= pi**2/12d0 - ll2**2/2d0 + ll2*ll1mx 
     &        - ll1mx*ll1x - basis2_3(x)
      case(4)
         ris=-basis2_2(x)
      case(5)
         ris=llx**2/2d0
      case(6)
         ris=basis2_1(x)
      case(7)
         ris=-pi**2/12d0 + basis2_3(x) + ll2**2/2d0 
     &        -ll2*ll1mx
      case(8)
         ris=-basis2_1(x)-ll1mx*llx
      case(9)
         ris=ll1mx**2/2d0
      end select

c --- set the imaginary part back to zero if it has been modified to
c --- get the branch cuts right (and should be zero).
      if (bcflag.eq.1) then
         xre = dreal(x)
         if (n2.eq.0.and.xre.gt.0d0) then
            if (xre.lt.1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
c
         else if (n2.eq.1.and.xre.lt.1d0) then
            if (n1.ne.-1) then
               ris = dcmplx(dreal(ris),0d0)
            else if (xre.gt.-1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
c            
         else if (n2.eq.-1.and.xre.gt.-1d0) then
            if (n1.ne.1) then
               ris = dcmplx(dreal(ris),0d0)
            else if (xre.lt.1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
         endif
      endif   
      HPL2else=ris 
      return
      end
