

C---------------------------------------------------------------------------
C     Leptonic Higgs decays in one routine:
C---------------------------------------------------------------------------      

      subroutine higgszerfall(
     &      mh,mw,mz,gamW,gamZ,GF,pH,idecaymode,
     $      x,   !Input
     &     GAMMA
     &,p1,p2,p3,p4
     & )                       !Output
      implicit none      
C     Input/Output variables
      doubleprecision mh,mw,mz,GF,gamw,gamz,x(5),GAMMA
      character*10 decaymode
      integer idecaymode
      doubleprecision p1(4),p2(4),p3(4),p4(4),pH(4)
C     Internal
      doubleprecision v(3)
      doubleprecision Jac,S12,S13,S14,S23,S24,S34,pi
      doubleprecision HWWlnln,HZZlnln,HZZllll,HWWZZlnln,HZZeemm,Msq
      parameter(pi=3.141592653589793d0)
      logical bwmap

C     Branch into different decaymode, generate phase space and compute decay rate: 
          
      if (idecaymode.eq.1) then
                                decaymode = 'HZZeemm'
      else if (idecaymode.eq.2) then
                                decaymode = 'HZZllll'
      else if (idecaymode.eq.3) then
                                decaymode = 'HWWlnln'
      else if (idecaymode.eq.4) then
                                decaymode = 'HWWZZlnln'
      end if

      if(decaymode.eq.'HZZeemm')then
         bwmap=.true.
         call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p2,p3,p4)
         call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
         Msq=HZZeemm(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Jac*Msq/(2*mh)         


      else if(decaymode.eq.'HZZllll')then
         bwmap=.false.
c         bwmap=.true.
         call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p3,p2,p4)
c         call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p2,p3,p4)
         call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
         Msq=HZZllll(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Jac*Msq/4d0/(2*mh)         

      else if(decaymode.eq.'HWWlnln')then
         bwmap=.true.
         call PS4(MH,Mw,Mw,Gamw,Gamw,x,bwmap,Jac,p1,p2,p3,p4)
         call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
         Msq=HWWlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Jac*Msq/(2*mh)         

      else if(decaymode.eq.'HWWZZlnln')then
         bwmap=.true.
c         bwmap=.false.
c         call PS4(MH,Mw,Mw,Gamw,Gamw,x,bwmap,Jac,p2,p3,p1,p4)
         call PS4(MH,Mw,Mw,Gamw,Gamw,x,bwmap,Jac,p1,p2,p3,p4)

         call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)

         Gamma=0d0
         Msq=HZZlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
c         GAMMA=Jac*Msq/(2*mh)         
c     Experimental factor of 3!!!
         GAMMA=3d0*Jac*Msq/(2*mh)         
         
         Msq=HWWlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Gamma+Jac*Msq/(2*mh)         

         Msq=HWWZZlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Gamma+Jac*Msq/(2*mh)         
         
      else
        !write(6,*)"channel",decaymode,"is not yet implemented"
      end if

C     Boost decay moenta into Higgs lab frame:
      call vec3(pH/pH(1),v)
 
      call boost(v,p1)      
      call boost(v,p2)      
      call boost(v,p3)      
      call boost(v,p4)      
      
      end subroutine


      subroutine PS4(MH,Mv1,Mv2,Gam1,Gam2,x,bwmap,Jac,p1,p2,p3,p4)
      implicit none
      doubleprecision MH,Mv1,Mv2,Gam1,Gam2,x(5),Jac
      doubleprecision p1(4),p2(4),p3(4),p4(4),Q1(4),Q2(4)
      doubleprecision pi,s1,s2,cos1,sin1,cos2,sin2,phi,ps1,ps2,es1,es2
      doubleprecision Jac1,Jac2
      doubleprecision kaellen,dot
      doubleprecision theta
      logical bwmap
      parameter(pi=3.141592653589793d0)
      
      if(bwmap)then
         call BWGEN(x(1),MH**2,0d0,Mv1,Gam1,S1,Jac1)
         call BWGEN(x(2),(MH-sqrt(s1))**2,0d0,Mv2,Gam2,S2,Jac2)
         Jac=Jac1*Jac2
      else
         s1=x(1)*MH**2
         s2=x(2)*(MH-sqrt(s1))**2
         Jac=1d0
         Jac=MH**2*(MH-sqrt(s1))**2
      end if

      Es1=(MH**2+s1-s2)/(2*MH)
      Es2=(MH**2+s2-s1)/(2*MH)
      ps1=sqrt(kaellen(MH**2,s1,s2))/MH/2d0
      ps2=-ps1
      
      cos1=2*x(3)-1
      Jac=Jac*s1/(es1+ps1*cos1)**2
      cos1=(ps1+cos1*es1)/((es1+cos1*ps1))
      
      cos2=2*x(4)-1
      Jac=Jac*s2/(es2+ps2*cos2)**2
      cos2=(ps2+cos2*es2)/((es2+cos2*ps2))
      
      sin1=sqrt(1-cos1**2)
      sin2=sqrt(1-cos2**2)
      phi=2*pi*x(5)
      
      Jac=Jac*(2*Pi)*(4*Pi)/8**3/(2*Pi)**6/(2*Pi)**2
      Jac=Jac*2*2*(2*pi)
      Jac=Jac*sqrt(kaellen(MH**2,s1,s2))/MH**2
      Jac=Jac*s1/(Es1-cos1*ps1)**2
      Jac=Jac*s2/(Es2+cos2*ps1)**2      
      
*     momenta: 
      Q1(1)=Es1
      Q1(2)=0d0
      Q1(3)=0d0
      Q1(4)=ps1      
      Q2(1)=Es2
      Q2(2)=0d0
      Q2(3)=0d0
      Q2(4)=ps2      
      p1(1)=s1/2d0/(Es1-ps1*cos1)
      p1(2)=0d0
      p1(3)=p1(1)*sin1
      p1(4)=p1(1)*cos1      
      p2=Q1-p1      
      p3(1)=s2/2d0/(Es2-ps2*cos2)
      p3(2)=p3(1)*sin2*sin(phi)
      p3(3)=p3(1)*sin2*cos(phi)
      p3(4)=p3(1)*cos2      
      p4=Q2-p3

CCC   The following are random rotations which are symmetries of the amplitude CCC
 
c     rotate moenta around z: flat 0<psi<2 Pi
      theta=rand()*2*Pi
      call rotate(p1,theta,3)
      call rotate(p2,theta,3)
      call rotate(p3,theta,3)
      call rotate(p4,theta,3)

c     rotate moenta around x: flat -1<cos theta<1
      theta=acos(2*rand()-1)
      call rotate(p1,theta,1)
      call rotate(p2,theta,1)
      call rotate(p3,theta,1)
      call rotate(p4,theta,1)

c     rotate moenta around y: flat 0<phi<2 Pi
      theta=rand()*2*Pi
      call rotate(p1,theta,2)
      call rotate(p2,theta,2)
      call rotate(p3,theta,2)
      call rotate(p4,theta,2)
      
      end subroutine
      

C     Breit Wigner Sampler:
      subroutine BWGEN(x,Qbsq,Qasq,M,Gam,Qsq,Jac)
      implicit none
      doubleprecision x,Qbsq,Qasq,M,Gam,Qsq,Jac
      doubleprecision y,yb,ya
      yb=atan((Qbsq-M**2)/M/Gam)
      ya=atan((Qasq-M**2)/M/Gam)
      y=(yb-ya)*x+ya
      Qsq=M**2+tan(y)*M*Gam
      Jac=(Qsq-M**2)**2+(M*Gam)**2
      Jac=Jac/(M*GAM)*(yb-ya)
      end subroutine

C     Vector product:      
      doubleprecision function dot(p1,p2)
      implicit none
      doubleprecision p1(4),p2(4)
      dot= p1(1)*p2(1)-p1(2)*p2(2)-p1(3)*p2(3)-p1(4)*p2(4)
      end function

c     routine to compute invariants:
      subroutine invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
      implicit none
      doubleprecision p1(4),p2(4),p3(4),p4(4)
      doubleprecision s12,s13,s14,s23,s24,s34
      doubleprecision dot
      S12=2*dot(p1,p2)
      S13=2*dot(p1,p3)
      S14=2*dot(p1,p4)
      S23=2*dot(p2,p3)
      S24=2*dot(p2,p4)
      S34=2*dot(p3,p4)      
      end subroutine

C     Kaellen's function:            
      doubleprecision function kaellen(x,y,z)
      implicit none
      doubleprecision x,y,z
      kaellen=x**2+y**2+z**2-2*(x*y+y*z+x*z)
      end 

c     3 vector projector:
      subroutine vec3(p,r)
      implicit none
      doubleprecision p(4),r(3)
      r(1)=p(2)
      r(2)=p(3)
      r(3)=p(4)
      end subroutine

c     3-vector scalar product:
      doubleprecision function dot3(v1,v2)
      implicit none
      doubleprecision v1(3),v2(3)
      dot3=v1(1)*v2(1)+v1(2)*v2(2)+v1(3)*v2(3)
      end function

C     A general boost routine:
      subroutine boost(v,p)
      implicit none
      doubleprecision p(4),v(3),pb(4),pb3(3)
      doubleprecision p3(3),gam,dot3,vv,pv
      vv=dot3(v,v)
      if(vv.eq.0d0)then
         pb=p
      else
         call vec3(p,p3)      
         pv=dot3(p3,v)      
         gam=1d0/sqrt(1d0-vv)
         pb(1)=gam*(p(1)-pv)
         pb3  =p3+ (  (gam-1d0)*pv/vv-gam*p(1))*v
         pb(2)=pb3(1)
         pb(3)=pb3(2)
         pb(4)=pb3(3)
      end if
      p=pb
      end subroutine

C     A rotation routine 
      subroutine rotate(r,theta,n)
      implicit none
      doubleprecision r(4),theta,rr(4)
      doubleprecision c,s
      integer n
      c=cos(theta)
      s=sin(theta)
      rr(1)=r(1)
      if(n.eq.1)then
         rr(2)=r(2)
         rr(3)=c*r(3)-s*r(4)
         rr(4)=s*r(4)+c*r(4)
      else if(n.eq.2)then
         rr(2)=s*r(4)+c*r(2)        
         rr(3)=r(3)
         rr(4)=c*r(4)-s*r(2)
      else if(n.eq.3)then
         rr(2)=c*r(2)-s*r(3)
         rr(3)=s*r(2)+c*r(3)                 
         rr(4)=r(4)
      else 
         write(6,*)"direction",n,"not defined in rotate"
      end if
      rr=r
      end subroutine








