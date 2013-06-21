      double complex function HPL3else(n1, n2, n3, x)
      implicit none
      double precision pi, zeta2, zeta3,ll2,xre
      double complex x, ris
      double complex basis3_1,basis3_2,basis3_3,basis3_4
      double complex basis3_5,basis3_6,basis3_7,basis3_8
      double complex basis2_1,basis2_2,basis2_3
      double complex ll1px,ll1mx,llx
      integer n1,n2,n3,j,bcflag

      pi=3.1415926535897932385D0
      zeta3=1.20205690315959428539973816151d0
      zeta2=pi**2/6d0
      ll2 = dlog(2d0)
      bcflag = 0

      j=1+(n3+1)*1+(n2+1)*3+(n1+1)*9

      ris=dcmplx(0d0,0d0)
      bcflag=0

c---  +i*epsilon to get branch cuts right ---
      if (dimag(x).eq.0d0) then
         x = x + dcmplx(0d0,1d-60)
         bcflag = 1
      endif
c---  
      ll1px = log(1d0+x)
      ll1mx = log(1d0-x)
      llx = log(x)

      select case(j)
c     #####################################################
c     basis3_1(z) = cli3(z) 
c     basis3_2(z) = cli3(-z)
c     basis3_3(z) = cli3(1-z)
c     basis3_4(z) = cli3(1/(1+z)) 
c     basis3_5(z) = cli3((1+z)/2) 
c     basis3_6(z) = cli3((1-z)/2) 
c     basis3_7(z) = cli3((1-z)/(1+z)) 
c     basis3_8(z) = cli3(2z/(z-1))
c     basis2_1(z) = cli2(z)
c     basis2_2(z) = cli2(-z)) 
c     basis2_3(z) = cli2((1-z)/2)
c     #####################################################
      case(1)
         ris = ll1px**3/6d0
      case(2)
         ris = -(pi**2*ll1px)/6d0 + ll1px**3/6d0 
     &- basis3_4(x) 
     &        + zeta3
      case(3)
         ris = (pi**2*ll2)/12d0 - ll2**3/6d0 
     &- (pi**2*ll1px)/12d0 + (ll2**2*ll1px)/2d0 
     &- (ll2*ll1px**2)/2d0 + basis3_5(x) - (7*zeta3)/8d0
      case(4)
         ris = (pi**2*ll1px)/3d0 + llx*ll1px**2 
     &- ll1px**3/3d0 + ll1px*basis2_2(x)+2*basis3_4(x)
     &-2*zeta3
      case(5)
         ris = (llx**2*ll1px)/2d0+llx*basis2_2(x)
     &-basis3_2(x)
      case(6)
         ris = (pi**2*ll2)/6d0 - ll2**3/3d0 
     &- (pi**2*ll1mx)/12d0 + (ll2**2*ll1mx)/2d0 
     &- (pi**2*ll1px)/12d0 + (ll2**2*ll1px)/2d0 
     &- ll2*ll1mx*ll1px - ll1mx*llx*ll1px 
     &+(ll1mx*ll1px**2)/2d0-ll1mx*basis2_2(x)
     &+basis3_6(x) 
     &- basis3_3(x)-basis3_4(x)+ basis3_7(x) + basis3_5(x)
     &-(3*zeta3)/4d0
      case(7)
         ris = -(pi**2*ll2)/6d0 + ll2**3/3d0 
     &+ (pi**2*ll1px)/4d0 - (3*ll2**2*ll1px)/2d0 
     &+ ll2*ll1mx*ll1px + ll2*ll1px**2 
     &- ll1mx*ll1px**2 - ll1px*basis2_3(x) 
     &- 2*basis3_5(x) + (7*zeta3)/4d0
      case(8)
         ris = -(pi**2*ll2)/12d0 + ll2**3/6d0 
     &+ (pi**2*ll1mx)/6d0 - (ll2*ll1mx**2)/2d0 
     &+ ll1mx**3/6d0+(pi**2*llx)/12d0- (ll2**2*llx)/2d0 
     &+ ll2*ll1mx*llx - (ll1mx**2*llx)/2d0 
     &+ (pi**2*ll1px)/12d0 - (ll2**2*ll1px)/2d0 
     &+ ll2*ll1mx*ll1px - (ll1mx*ll1px**2)/2d0 
     &- llx*basis2_3(x) + basis3_2(x)-basis3_1(x)-basis3_8(x) 
     &+ basis3_4(x) - basis3_7(x) - basis3_5(x) + (7*zeta3)/8d0
      case(9)
         ris = -(pi**2*ll2)/12d0 + ll2**3/6d0 
     &- (ll2*ll1mx**2)/2d0 + (ll1mx**2*ll1px)/2d0 
     &+ ll1mx*basis2_3(x) - basis3_6(x) + (7*zeta3)/8d0
      case(10)
         ris = -(pi**2*ll1px)/6d0 - (llx*ll1px**2)/2d0 
     &+ ll1px**3/6d0 - ll1px*basis2_2(x) - basis3_4(x) 
     &+ zeta3
      case(11)
         ris = -(llx*basis2_2(x)) + 2*basis3_2(x)
      case(12)
         ris = -(pi**2*ll2)/12d0 + ll2**3/6d0 
     &- (pi**2*ll1mx)/12d0 - (ll2**2*ll1mx)/2d0 
     &+ (ll2*ll1mx**2)/2d0 - ll1mx**3/6d0 
     &+ (ll1mx**2*llx)/2d0 + ll1mx*basis2_2(x) 
     &- basis3_6(x) + basis3_3(x) - basis3_2(x) +basis3_1(x)
     &+basis3_8(x) 
     &- zeta3/8d0
      case(13)
         ris = -basis3_2(x)
      case(14)
         ris = llx**3/6d0
      case(15)
         ris = basis3_1(x)
      case(16)
         ris = -(pi**2*ll2)/12d0 + ll2**3/6d0 
     &+ (pi**2*ll1mx)/6d0 - (ll2*ll1mx**2)/2d0 
     &+ ll1mx**3/6d0 - (ll1mx**2*llx)/2d0 
     &+ (pi**2*ll1px)/12d0 - (ll2**2*ll1px)/2d0 
     &+ ll2*ll1mx*ll1px + ll1mx*llx*ll1px 
     &-(ll1mx*ll1px**2)/2d0+ll1px*basis2_1(x)
     &+basis3_2(x) 
     &- basis3_1(x) - basis3_8(x) + basis3_4(x) -basis3_7(x)
     &-basis3_5(x) 
     &+ (7*zeta3)/8d0
      case(17)
         ris = llx*basis2_1(x) - 2*basis3_1(x)
      case(18)
         ris = (pi**2*ll1mx)/6d0 - (ll1mx**2*llx)/2d0 
     &- ll1mx*basis2_1(x) - basis3_3(x) + zeta3
      case(19)
         ris = (pi**2*ll2)/12d0 - ll2**3/6d0 
     &- (pi**2*ll1px)/6d0 + ll2**2*ll1px 
     &- ll2*ll1mx*ll1px - (ll2*ll1px**2)/2d0 
     &+ (ll1mx*ll1px**2)/2d0 + ll1px*basis2_3(x) 
     &+ basis3_5(x) - (7*zeta3)/8d0
      case(20)
         ris = -(pi**2*ll2)/12d0 + ll2**3/6d0 
     &- (pi**2*ll1mx)/12d0 - (ll2**2*ll1mx)/2d0 
     &+ (ll2*ll1mx**2)/2d0 - ll1mx**3/6d0 
     &- (pi**2*llx)/12d0 + (ll2**2*llx)/2d0 
     &- ll2*ll1mx*llx + (ll1mx**2*llx)/2d0 
     &+ llx*basis2_3(x) - basis3_6(x) + basis3_3(x) 
     &- basis3_2(x) + basis3_1(x) + basis3_8(x) - zeta3/8d0
      case(21)
         ris = (pi**2*ll2)/6d0 - ll2**3/3d0 
     &- (pi**2*ll1mx)/12d0 + (ll2**2*ll1mx)/2d0 
     &- ll1mx*basis2_3(x) + 2*basis3_6(x) - (7*zeta3)/4d0
      case(22)
         ris = (pi**2*ll2)/6d0 - ll2**3/3d0 
     &- (pi**2*ll1mx)/12d0 + (ll2**2*ll1mx)/2d0 
     &- (pi**2*ll1px)/12d0 + (ll2**2*ll1px)/2d0 
     &- ll2*ll1mx*ll1px - ll1mx*llx*ll1px 
     &+(ll1mx*ll1px**2)/2d0-ll1px*basis2_1(x)
     &+basis3_6(x) 
     &- basis3_3(x) - basis3_4(x)+basis3_7(x)+basis3_5(x)
     &- (3*zeta3)/4d0
      case(23)
         ris =-(ll1mx*llx**2)/2d0-llx*basis2_1(x)
     &+basis3_1(x)
      case(24)
         ris = -(pi**2*ll1mx)/3d0 + ll1mx**2*llx 
     &+ ll1mx*basis2_1(x) + 2*basis3_3(x) - 2*zeta3
      case(25)
         ris = -(pi**2*ll2)/12d0 + ll2**3/6d0 
     &+ (pi**2*ll1mx)/12d0 - (ll2**2*ll1mx)/2d0 
     &+ (ll2*ll1mx**2)/2d0 - basis3_6(x) + (7*zeta3)/8d0
      case(26)
         ris = (pi**2*ll1mx)/6d0 - basis3_3(x) + zeta3
      case(27)
         ris = -ll1mx**3/6d0
      end select
c --- set the imaginary part back to zero if it has been modified to
c --- get the branch cuts right (and should be zero).
      if (bcflag.eq.1) then
         xre = dreal(x)
         if (n3.eq.0.and.xre.gt.0d0) then
            if (xre.lt.1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
c
         else if (n3.eq.1.and.xre.lt.1d0) then
            if (n1.ne.-1.and.n2.ne.-1) then
               ris = dcmplx(dreal(ris),0d0)
            else if (xre.gt.-1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
c            
         else if (n3.eq.-1.and.xre.gt.-1d0) then
            if (n1.ne.1.and.n2.ne.1) then
               ris = dcmplx(dreal(ris),0d0)
            else if (xre.lt.1d0) then
               ris = dcmplx(dreal(ris),0d0)
            endif
         endif
      endif      
      
      HPL3else=ris
      return
      end
