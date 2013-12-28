C---------------------------------------------------------------------------
C     All leptonic and photonic Higgs decays in one routine:
c     written by Franz Herzog
C---------------------------------------------------------------------------      


      subroutine HiggsZerfall(
     &     pH,mw,mz,mt,gamW,gamZ,GF,alpha,imode,x  !Input
     &     ,GAMMA,p1,p2,p3,p4)                     !Output
      implicit none      
C     Input/Output variables
      integer imode
      doubleprecision mh,mw,mz,mt,GF,gamw,gamz,x(8),GAMMA,alpha
      character*20 decaymode,topology
      doubleprecision p1(4),p2(4),p3(4),p4(4),pH(4)
C     Internal
      doubleprecision v(3),dot
      doubleprecision Jac,S12,S13,S14,S23,S24,S34,pi
      doubleprecision HWWlnln,HZZlnln,HZZllll,HWWZZlnln,HZZeemm,Msq
      doubleprecision HphotonZ,Hphotonll, Hdiphoton
      parameter(pi=3.141592653589793d0)
      logical bwmap
C     Higgs zerfall remembers!
      doubleprecision xo(8),p1o(4),p2o(4),p3o(4),p4o(4),pHo(4),GAMMAo
c      common /memory/ xo,pHo,p1o,p2o,p3o,p4o,GAMMAo
      integer dummy

      if(x(1).eq.xo(1).and.x(2).eq.xo(2).and.x(3).eq.xo(3)
     -     .and.x(4).eq.xo(4).and.x(5).eq.xo(5).and.x(6).eq.xo(6)
     -     .and.x(7).eq.xo(7).and.x(8).eq.xo(8))then
c         dummy=dummy+1
c         print *,"old",dummy
         Gamma=Gammao
         p1=p1o
         p2=p2o
         p3=p3o
         p4=p4o

      else
c         dummy=0
c         print *, "new"
         
c==================================================================
c     !The imode brancher, notation is hopefully self explanatory!
c=================================================================
      if(imode.eq.1)then
         decaymode='HZZeemm'
      else if(imode.eq.2)then
         decaymode='HZZllll'
         topology ='all'
      else if(imode.eq.21)then
         decaymode='HZZllll'
         topology ='D1234'
      else if(imode.eq.22)then
         decaymode='HZZllll'
         topology ='D2314'
      else if(imode.eq.3)then
         decaymode='HWWlnln'
      else if(imode.eq.4)then
         decaymode='HWWZZlnln'
         topology ='all'
      else if(imode.eq.41)then
         decaymode='HWWZZlnln'
         topology ='D1234'
      else if(imode.eq.42)then
         decaymode='HWWZZlnln'
         topology ='D2314'         
      else if(imode.eq.5)then
         decaymode='HphotonZ'
      else if(imode.eq.6)then
         decaymode='Hphotonll'
      else if(imode.eq.7)then
         decaymode='Hdiphoton'
      end if
c================================================================

C     Compute (rest) Higgs mass:
      MH=sqrt(dot(ph,ph))

C     Branch into different decaymode, generate phase space and compute decay rate: 

      if(decaymode.eq.'HZZeemm')then
         bwmap=.true.
         call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p2,p3,p4)
         call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
         Msq=HZZeemm(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Jac*Msq/(2*mh)         
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else if(decaymode.eq.'HZZllll')then
         if(topology.eq.'D1234')then
            bwmap=.true.         
            call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p2,p3,p4)
            call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
            Msq=HZZllll(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34
     &           ,topology)            
         else if(topology.eq.'D2314')then
            bwmap=.true.         
            call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p2,p3,p4,p1)
            call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
            Msq=HZZllll(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34
     &           ,topology)  
         else if(topology.eq.'all')then
            bwmap=.false.         
            call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p3,p2,p4)
            call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
            Msq=HZZllll(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34
     &           ,topology)     
         end if
         GAMMA=Jac*Msq/4d0/(2*mh)              
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else if(decaymode.eq.'HWWlnln')then
         bwmap=.true.
         call PS4(MH,Mw,Mw,Gamw,Gamw,x,bwmap,Jac,p1,p2,p3,p4)
         call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
         Msq=HWWlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34)
         GAMMA=Jac*Msq/(2*mh)         
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else if(decaymode.eq.'HWWZZlnln')then
         if(topology.eq.'D1234')then
            bwmap=.true.         
            call PS4(MH,Mw,Mw,Gamw,Gamw,x,bwmap,Jac,p1,p2,p3,p4)
            call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
            Msq=HWWZZlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34,
     -           topology)   
         else if(topology.eq.'D2314')then
            bwmap=.true.         
            call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p2,p3,p4,p1)
            call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
            Msq=HWWZZlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34,
     -           topology)
         else if(topology.eq.'all')then
            bwmap=.false.         
            call PS4(MH,Mz,Mz,Gamz,Gamz,x,bwmap,Jac,p1,p3,p2,p4)
            call invars4(p1,p2,p3,p4,s12,s13,s14,s23,s24,s34)
            Msq=HWWZZlnln(GF,mW,GamW,mZ,GamZ,s12,s13,s14,s23,s24,s34,
     -           topology)
         end if
         GAMMA=Jac*Msq/(2*mh)        
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else if(decaymode.eq.'HphotonZ')then
         call PS2(mH,0d0,mZ,x(1),x(2),Jac,p1,p2)
         Msq=HphotonZ(GF,mW,GamW,mZ,GamZ,alpha,mh,mt,mz)
         GAMMA=Jac*Msq/(2d0*mh)   
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else if(decaymode.eq.'Hphotonll')then
         bwmap=.true.
         call PS3(MH,Mz,Gamz,x(1),x(2),x(3),x(4),x(5),bwmap,
     &        Jac,p2,p3,p1)
         call invars3(p1,p2,p3,s12,s13,s23)
         Msq=Hphotonll(GF,mW,GamW,mZ,GamZ,alpha,MH,MT,s12,s13,s23)
         GAMMA=Jac*Msq/(2d0*mh)   
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else if(decaymode.eq.'Hdiphoton')then
         call PS2(mH,0d0,0d0,x(1),x(2),Jac,p1,p2)
         Msq=Hdiphoton(GF,mW,alpha,MH,MT)
         GAMMA=Jac*Msq/(2d0*mh)

c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
      else
         write(6,*)"channel",decaymode,"is not yet implemented"
      end if

c     Save into memory:
      xo=x
      p1o=p1
      p2o=p2
      p3o=p3
      p4o=p4
      Gammao=Gamma
      end if
      

C     Boost decay momenta into Higgs lab frame:
      call vec3(pH/pH(1),v)
      if(imode.eq.5.or.imode.eq.7)then
         call boost(v,p1)      
         call boost(v,p2)      
      else if(imode.eq.6)then
         call boost(v,p1)      
         call boost(v,p2)      
         call boost(v,p3)      
      else
         call boost(v,p1)      
         call boost(v,p2)      
         call boost(v,p3)      
         call boost(v,p4)      
      end if

      end subroutine









c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
c     Phase space generation:
c~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

      subroutine PS4(MH,Mv1,Mv2,Gam1,Gam2,x,bwmap,Jac,p1,p2,p3,p4)
      implicit none
      doubleprecision MH,Mv1,Mv2,Gam1,Gam2,x(8),Jac
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
      
      theta=x(6)*2*Pi
  
      call rotate(p1,theta,3)
      call rotate(p2,theta,3)
      call rotate(p3,theta,3)
      call rotate(p4,theta,3)

c     rotate moenta around x: flat -1<cos theta<1
      theta=acos(2*x(7)-1d0)
      call rotate(p1,theta,1)
      call rotate(p2,theta,1)
      call rotate(p3,theta,1)
      call rotate(p4,theta,1)

c     rotate moenta around y: flat 0<phi<2 Pi
      theta=x(8)*2*Pi
      call rotate(p1,theta,2)
      call rotate(p2,theta,2)
      call rotate(p3,theta,2)
      call rotate(p4,theta,2)
      
      end subroutine



c
c     Another 4-particle phase space generator:
c



c
c     A 3-particle massless phase-space with Breit Wigner adaptation:
c

      subroutine PS3(MH,Mv1,Gam1,x1,x2,x3,x4,x5,bwmap,Jac,p1,p2,p3)
      implicit none
      doubleprecision MH,Mv1,Mv2,Gam1,x1,x2,x3,x4,x5,Jac
      doubleprecision p1(4),p2(4),p3(4),p4(4),Q1(4),Q2(4)
      doubleprecision pi,s1,s2,cos1,sin1,cos2,sin2,phi,ps1,ps2,es1,es2
      doubleprecision Jac1,Jac2
      doubleprecision kaellen,dot
      doubleprecision theta
      logical bwmap
      parameter(pi=3.141592653589793d0)
      
      if(bwmap)then
         call BWGEN(x1,MH**2,0d0,Mv1,Gam1,S1,Jac1)
         Jac=Jac1
      else
         s1=x1*MH**2
         Jac=MH**2
      end if

      s2=0d0

      Es1=(MH**2+s1-s2)/(2*MH)
      Es2=(MH**2+s2-s1)/(2*MH)
      ps1=sqrt(kaellen(MH**2,s1,s2))/MH/2d0
      ps2=-ps1
      
      cos1=2*x2-1
      Jac=Jac*s1/(es1+ps1*cos1)**2
      cos1=(ps1+cos1*es1)/((es1+cos1*ps1))            
      sin1=sqrt(1-cos1**2)
     
      Jac=Jac*(2*Pi)*(4*Pi)/8**2/(2*Pi)**5
      Jac=Jac*2
      Jac=Jac*sqrt(kaellen(MH**2,s1,s2))/MH**2
      Jac=Jac*s1/(Es1-cos1*ps1)**2
      
*     momenta: 
      Q1(1)=Es1
      Q1(2)=0d0
      Q1(3)=0d0
      Q1(4)=ps1      
      p1(1)=s1/2d0/(Es1-ps1*cos1)
      p1(2)=0d0
      p1(3)=p1(1)*sin1
      p1(4)=p1(1)*cos1      
      p2=Q1-p1      
      p3(1)=Es2
      p3(2)=0d0
      p3(3)=0d0
      p3(4)=ps2      

CCC   The following are random rotations which are symmetries of the amplitude CCC
 
c     rotate moenta around z: flat 0<psi<2 Pi
      theta=x3*2*Pi
      call rotate(p1,theta,3)
      call rotate(p2,theta,3)
      call rotate(p3,theta,3)

c     rotate moenta around x: flat -1<cos theta<1
      theta=acos(2*x4-1)
      call rotate(p1,theta,1)
      call rotate(p2,theta,1)
      call rotate(p3,theta,1)

c     rotate moenta around y: flat 0<phi<2 Pi
      theta=x5*2*Pi
      call rotate(p1,theta,2)
      call rotate(p2,theta,2)
      call rotate(p3,theta,2)
      
      end subroutine






c
c     A simple 1->2 phase space generator:
c
      subroutine PS2(MH,M1,M2,x1,x2,Jac,Q1,Q2)
      implicit none
      doubleprecision MH,Jac,m1,m2,x1,x2
      doubleprecision Q1(4),Q2(4)
      doubleprecision pi,s1,s2,ps1,es1,es2
      doubleprecision kaellen,dot
      doubleprecision theta
      logical bwmap
      parameter(pi=3.141592653589793d0)


      s1=m1**2
      s2=m2**2


      Jac=dsqrt(kaellen(MH**2,s1,s2))/(MH**2*8d0*pi)

      Es1=(MH**2+s1-s2)/(2d0*MH)
      Es2=(MH**2+s2-s1)/(2d0*MH)
      ps1=dsqrt(kaellen(MH**2,s1,s2))/MH/2d0      


*     momenta: 
      Q1(1)=Es1
      Q1(2)=0d0
      Q1(3)=0d0
      Q1(4)=ps1      
      Q2(1)=Es2
      Q2(2)=0d0
      Q2(3)=0d0
      Q2(4)=-ps1      

CCC   The following are random rotations which are symmetries of the amplitude CCC
c     rotate moenta around x: flat -1<cos theta<1
      theta=acos(2*x1-1)
      call rotate(Q1,theta,1)
      call rotate(Q2,theta,1)
c     rotate moenta around z: flat 0<psi<2 Pi
      theta=x2*2*Pi
      call rotate(Q1,theta,3)
      call rotate(Q2,theta,3)


      
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

      subroutine invars3(p1,p2,p3,s12,s13,s23)
      implicit none
      doubleprecision p1(4),p2(4),p3(4)
      doubleprecision s12,s13,s23
      doubleprecision dot
      S12=2*dot(p1,p2)
      S13=2*dot(p1,p3)
      S23=2*dot(p2,p3)
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


C     A general boost routine:
      subroutine boostoptimised(v,p)
      implicit none
      doubleprecision p(4),v(3),pb(4),pb3(3)
      doubleprecision p3(3),gam,dot3,vv,pv
      vv=v(1)*v(1)+v(2)*v(2)+v(3)*v(3)
      if(vv.eq.0d0)then
         pb=p
      else
         pv=p(2)*v(1)+p(3)*v(2)+p(4)*v(3) 
         gam=1d0/sqrt(1d0-vv)
         pb(1)=gam*(p(1)-pv)
         pb(2)  =p(2)+((gam-1d0)*pv/vv-gam*p(1))*v(1)
         pb(3)  =p(3)+((gam-1d0)*pv/vv-gam*p(1))*v(2)
         pb(4)  =p(4)+((gam-1d0)*pv/vv-gam*p(1))*v(3)
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
         rr(4)=s*r(3)+c*r(4)
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
      r=rr
      end subroutine























