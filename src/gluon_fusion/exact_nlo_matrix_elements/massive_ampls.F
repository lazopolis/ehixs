C In the following we  define the Born Matrix-Elements of 
C      http://arXiv.org/pdf/hep-ph/0611236
C 
C M_q^(0) (of rhs Eq: 7.2) =  i*GAMMA(1+e)*e^(gamma*e)/(1-e)^2*(mq^4/4/mh^4)*
C            (brn_q_ep0 + brn_q_ep1 * e+ brn_q_ep2 *e^2 +O(e^3)  ) 
C  


      double complex function brn_q_ep0(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex  HPL2 !,HPL1, HPL3, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL2(0,0,x)
      t2 = (1+x)**2
      t3 = 1/x
      t8 = (-1+x)**2
      tmp = -16*t2*t3*tfunct1+32*t8*t3
      brn_q_ep0 = tmp 
      END

      double complex function brn_q_ep1(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1, HPL2, HPL3!, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL1(0,x)
      tfunct2 = HPL2(0,0,x)
      tfunct3 = HPL3(0,-1,0,x)
      tfunct4 = HPL3(0,0,0,x)
      t1 = 1+x
      t2 = t1**2
      t3 = 1/x
      t4 = t2*t3
      t5 = pi**2
      t10 = -1+x
      t14 = x**2
      t25 = t10**2
      tmp = 8.D0/3.D0*t4*t5*tfunct1-32*t1*tfunct1*t10*t3+32*t3*(t14+1)*t
     #funct2+32*t4*tfunct3-16*t4*tfunct4+48*t4*zeta3+32*t25*t3
      brn_q_ep1 = tmp 
      END

      double complex function brn_q_ep2(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1, HPL2, HPL3, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL2(0,-1,x)
      tfunct2 = HPL1(0,x)
      tfunct3 = HPL4(0,0,-1,0,x)
      tfunct4 = HPL2(-1,0,x)
      tfunct5 = HPL2(0,0,x)
      tfunct6 = HPL4(0,-1,-1,0,x)
      tfunct7 = HPL4(0,-1,0,0,x)
      tfunct8 = HPL3(0,-1,0,x)
      tfunct9 = HPL3(0,0,0,x)
      tfunct10 = HPL4(0,0,0,0,x)
      t1 = 1+x
      t2 = t1**2
      t3 = 1/x
      t4 = t2*t3
      t5 = pi**2
      t9 = x**2
      t11 = t3*(1+t9)
      t19 = -1+x
      t25 = t19*t1
      t48 = t5**2
      t56 = t19**2
      tmp = -16.D0/3.D0*t4*t5*tfunct1-16.D0/3.D0*t11*t5*tfunct2+32*t4*tf
     #unct2*zeta3-32*t1*tfunct2*t19*t3+32*t4*tfunct3+64*t25*t3*tfunct4+8
     #.D0/3.D0*t4*t5*tfunct5-16*(3*x+1)*t19*t3*tfunct5-64*t4*tfunct6+32*
     #t4*tfunct7-64*t11*tfunct8+32*t11*tfunct9-16*t4*tfunct10+2.D0/9.D0*
     #t4*t48+16.D0/3.D0*t25*t3*t5-96*t11*zeta3+64*t56*t3
      brn_q_ep2 = tmp 
      END


      double complex function brn_q_ep0_der(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1, HPL2!, HPL3, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL1(0,x)
      tfunct2 = HPL2(0,0,x)
      t2 = 1+x
      t3 = (-1+x)*t2
      t4 = x**2
      t5 = 1/t4
      t11 = t2**2
      tmp = 32*t3*t5-16*t3*t5*tfunct2-16*t11*t5*tfunct1
      brn_q_ep0_der = tmp 
      END
      
      double complex function brn_q_ep1_der(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1, HPL2, HPL3!, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL1(0,x)
      tfunct2 = HPL2(-1,0,x)
      tfunct3 = HPL2(0,0,x)
      tfunct4 = HPL3(0,-1,0,x)
      tfunct5 = HPL3(0,0,0,x)
      t2 = 1+x
      t3 = (-1+x)*t2
      t4 = x**2
      t5 = 1/t4
      t17 = t2**2
      t18 = t17*t5
      t24 = pi**2
      tmp = -16*t3*t5*tfunct5+48*t3*t5*zeta3+16*t2*(x-3)*t5*tfunct3+32*t
     #18*tfunct2+32*t3*t5*tfunct4+8.D0/3.D0*t18*t24+8.D0/3.D0*t3*t5*t24*
     #tfunct1
      brn_q_ep1_der = tmp 
      END

c=======================================================================
c
c	Below is an implementation of the mu-independent, non-pi^2 part of the
c	two_loop virtual amplitude in the presence of heavy quarks, i.e. it 
c	contributes to the NLO gg_delta coefficient. 
c	Its limit as m->infty is 11/2*born
c	The implementation (by Stephan) follows eq.26-30 
c	in http://arxiv.org/abs/hep-ph/0611266
	
	double complex function twol_q_fin_bon(x)
      implicit none
      double complex F2lonehalf,G2lCA_onehalf,ris
      double precision lambda,N,CF
      double complex x


      
c     input
      N=3d0 ! the N from SU(N)
      lambda = 1d0 ! the Hff coupling strength w.r.t. SM

c     derived
      CF = (N**2-1)/N/2d0

c     result
      ris = lambda/2d0*(CF*F2lonehalf(x) + N*G2lCA_onehalf(x))
c	the line below divides by born*(-2/3) to get the 11/2 coefficient
c	it is corrected because we need 11/2*born, the full virtual contribution
     & / (-2d0/3d0)
      
      ris = ris + conjg(ris) ! + h.c.
      twol_q_fin_bon = ris
      return
      end function

c ----------------------------------------------------
c eq. (28)
      double complex function G2lCA_onehalf(x)
      implicit none
      double precision z2,z3,pi
      double complex ris,temp,x,HPL2,HPL3,HPL4,H2

      pi=3.141592653589793d0
      z2=pi**2/6d0
      z3=1.202056903159594d0
      
      
      H2=4d0/5d0*z2**2 + 2d0*z3 + 3d0*z3/2d0*log(x) - 3d0*z3*log(1d0-x)
     &     + z2*HPL2(1,0,x) + 1d0/4d0*(1d0+2*z2)*HPL2(0,0,x)
     &     - 2d0*HPL3(1,0,0,x) + HPL4(0,0,-1,0,x) 
     &     + 1d0/4d0*HPL4(0,0,0,0,x) + 2d0*HPL4(1,0,-1,0,x)
     &     - HPL4(1,0,0,0,x)
      
      temp = 3d0
      temp = temp + x*(1d0+8d0*x+3d0*x**2)/(x-1d0)**3*HPL3(0,0,0,x)
      temp = temp - 2d0*(1d0+x)**2/(x-1d0)**2*H2
      temp = temp + z3 - HPL3(1,0,0,x)
      
      ris = temp*4d0*x/(x-1d0)**2
      G2lCA_onehalf = ris
      return
      end function
c - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

      subroutine testing_bon()
      implicit none
      double precision double_mq(100),Yq(100)
	double complex Wq(100),Xq(100),Mq_square_complex(100)
	common /XWq/ Xq,Wq,Mq_square_complex,double_mq,Yq,
     # number_of_quarks
      integer number_of_quarks
      double complex mymt,w,x,H2,HPL2,HPL3,HPL4,limit,x1,x2,w2
      double complex G2lCA_onehalf,F2lonehalf,inside,H2factor
      double complex delta
      integer i,N
      double precision pi,z2,z3,mhsq,tantheta,r
      
      print*,"Testing G2l2Ca: as mh/mt->0 G2l2Ca 
     $            should go to -5/3=-1.666"
      pi=3.141592653589793d0
      z2=pi**2/6d0
      z3=1.202056903159594d0
      mhsq=10d0**2
      mymt=170d0*(170d0,-3.4d0)
      x=dcmplx(0d0,0d0)
      tantheta=1d20
      N=100
      
      print*,"mt","mh","x","G2lCA","G2lCa no width"
      do i=1,N
            mhsq=(10d0*i)**2
            w=4d0*mymt/mhsq
            x=-w/(sqrt(1d0-w)+1)**2
            w2=4d0*dcmplx(170d0**2,-1d-16)/mhsq
            x2=-w2/(sqrt(1d0-w2)+1)**2
            print*,sqrt(mymt),sqrt(mhsq),x,
     $G2lCA_onehalf(x),G2lCA_onehalf(x2)
            mhsq=(10d0*i)**2

      enddo
     
      stop
      end
            
            
                        
c ----------------------------------------------------
c eq. (11), neglecting the log(m^2/mu^2) term that we don't want to include

      double complex function F2lonehalf(x)
      implicit none
      double precision z2,z3,pi
      double complex ris,x,HPL2,HPL3,HPL4
      double complex H1,F2la
         
      pi=3.141592653589793d0
      z2=pi**2/6d0
      z3=1.202056903159594d0
   
      H1 = 9d0*z2**2/10d0 + 2d0*z3*log(x) + z2*HPL2(0,0,x) 
     &     + 1d0/4d0*HPL4(0,0,0,0,x) + 7d0/2d0*HPL4(0,1,0,0,x)
     &     - 2d0*HPL4(0,-1,0,0,x) + 4d0*HPL4(0,0,-1,0,x)
     &     - HPL4(0,0,1,0,x)
      
      F2la = 36d0*x/(x-1d0)**2 - 4*x*(1d0-14d0*x+x**2)/(x-1d0)**4*z3
     &     - 4*x*(1d0+x)*log(x)/(x-1d0)**3 - 8d0*x*(1d0+9d0*x+x**2)
     &     /(x-1d0)**4*HPL2(0,0,x) +2d0*x*(3d0+25d0*x-7d0*x**2+3d0*x**3)
     &     /(x-1d0)**5*HPL3(0,0,0,x) + 4d0*x*(1d0+2d0*x+x**2)/(x-1d0)**4
     &     *(z2*log(x) + 4d0*HPL3(0,-1,0,x) - HPL3(0,1,0,x))
     &     + 4d0*x*(5d0-6d0*x+5d0*x**2)/(x-1d0)**4*HPL3(1,0,0,x)
     &     - 8d0*x*(1d0+x+x**2+x**3)/(x-1d0)**5*H1
      
      ris = F2la

      F2lonehalf = ris
      return
      end function

c ----------------------------------------------------
c eq. (6), the ggH born amplitude

      double complex function F1lonehalf(x)
      implicit none
      double complex x,HPL2,ris,tau

      tau = -4d0*x/(x-1d0)**2
      ris = -tau*(2d0-(1d0-tau)*HPL2(0,0,x))

      F1lonehalf = ris
      return
      end function
c=======================================================================


C-- This is  the two-loop amplitude for gg->h via 
C   a  quark loop 
C   In the following we  define the amplitude in terms  
C    of the expressions in  
C      http://arXiv.org/pdf/hep-ph/0611236
C The finite part of the amplitude is reconstructed as: 
C M_q^(1)_fin (of rhs Eq: 7.4) =  
C 


      double complex function twol_q_fin(x)
      implicit double complex (s,t) 
      double complex x!, tmp
      double complex brn_q_ep0, brn_q_ep1, brn_q_ep2
      double complex brn_q_ep0_der, brn_q_ep1_der
      double complex  twol_q_epm0
      double complex HPL1!, HPL2, HPL3, HPL4      
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      !print*,"x=",x  
      ris = dcmplx(0d0, 0d0)
      tfunc1 = -12*HPL1(1,x)**2-3*HPL1(0,x)**2+20*HPL1(1,x)
     #-12*HPL1(0,x)*HPL1(1,x)+10*HPL1(0,x)+10D0
     #+dcmplx(0.D0,-16.D0)*pi
     # +pi**2               ! pi^2 term
      tfunc2=-6d0*HPL1(0,x)+10d0-12d0*HPL1(1,x)
      tfunc3=+8d0*x*(x-1)/(x+1)*(-2d0*HPL1(1,x)-1d0-HPL1(0,x)
     #+dcmplx(0d0,1d0)*pi)




      ris = ris+tfunc1*brn_q_ep0(x)
      ris = ris+tfunc2*brn_q_ep1(x)-6d0*brn_q_ep2(x)
      ris=ris+tfunc3*brn_q_ep0_der(x)
      ris=ris+twol_q_epm0(x)
      ris=ris-8d0*x*(-1d0+x)/(1+x)*brn_q_ep1_der(x)

      twol_q_fin =  ris 
      return
      end

      double complex function twol_q_epm2(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1!, HPL2, HPL3, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL1(0,x)
      t2 = (1+x)**2
      t3 = 1/x
      t5 = tfunct1**2
      t9 = (-1+x)**2
      tmp = -48*t2*t3*t5+192*t9*t3
      twol_q_epm2 = tmp 
      END
      
      
      double complex function twol_q_epm1(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1, HPL2, HPL3!, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL1(0,x)
      tfunct2 = HPL1(1,x)
      tfunct3 = HPL2(0,-1,x)
      tfunct4 = HPL2(0,1,x)
      tfunct5 = HPL3(0,-1,0,x)
      tfunct6 = HPL3(0,0,-1,x)
      tfunct7 = HPL3(0,0,1,x)
      tfunct8 = HPL3(0,1,0,x)
      t2 = (1+x)**2
      t3 = 1/x
      t4 = t2*t3
      t5 = pi**2
      t14 = tfunct1**2
      t21 = x**2
      t32 = -1+x
      t37 = t32**2
      t38 = t37*t3
      tmp = 16*t4*t5*tfunct1-576*t4*tfunct1*tfunct3+1152*t4*tfunct6-64*t
     #4*t14*tfunct1-96*t4*t14*tfunct2+16*t3*(7*t21+18*x+7)*t14+384*t4*tf
     #unct1*tfunct4-128*(x+4)*t32*t3*tfunct1+384*t38*tfunct2-768*t4*tfun
     #ct7-384*t4*tfunct8+768*t4*tfunct5+288*t4*zeta3+128*t38
      twol_q_epm1 = tmp 
      END
      
      
      
      double complex function twol_q_epm0(x)
      implicit double complex (s,t) 
      double complex x, tmp
      double complex HPL1, HPL2, HPL3, HPL4
      double precision pi, zeta3 

      pi = 3.141592654D0
      zeta3 = 1.202056903D0

      tfunct1 = HPL1(0,x)
      tfunct2 = HPL1(1,x)
      tfunct3 = HPL2(-1,0,x)
      tfunct4 = HPL2(0,-1,x)
      tfunct5 = HPL2(0,1,x)
      tfunct6 = HPL2(1,0,x)
      tfunct7 = HPL2(1,1,x)
      tfunct8 = HPL3(0,-1,-1,x)
      tfunct9 = HPL3(0,-1,0,x)
      tfunct10 = HPL3(0,-1,1,x)
      tfunct11 = HPL3(0,0,-1,x)
      tfunct12 = HPL3(0,0,1,x)
      tfunct13 = HPL3(0,1,-1,x)
      tfunct14 = HPL3(0,1,0,x)
      tfunct15 = HPL3(0,1,1,x)
      tfunct16 = HPL3(1,0,0,x)
      tfunct17 = HPL4(0,-1,-1,0,x)
      tfunct18 = HPL4(0,-1,0,0,x)
      tfunct19 = HPL4(0,-1,0,1,x)  !Li22(-1, x)
      tfunct20 = HPL4(0,-1,1,0,x)  !Li22(-1, x)
      tfunct21 = HPL4(0,0,-1,0,x)
      tfunct22 = HPL4(0,0,0,-1,x)
      tfunct23 = HPL4(0,0,0,1,x)
      tfunct24 = HPL4(0,0,1,0,x)
      tfunct25 = HPL4(0,0,1,1,x)
      tfunct26 = HPL4(0,1,-1,0,x)
      tfunct27 = HPL4(0,1,0,0,x)
      tfunct28 = HPL4(0,1,0,1,x)
      tfunct29 = HPL4(0,1,1,0,x)
      tfunct30 = HPL4(1,0,0,0,x)
      tfunct31 = HPL4(1,0,0,1,x)
      tfunct32 = HPL4(1,1,0,0,x)
      t1 = 1+x
      t2 = x**2
      t6 = 1d0/x
      t8 = -1d0+x
      t9 = 1d0/t8
      t18 = pi**2
      t20 = tfunct1**2
      t24 = 53d0*t2
      t27 = t6*t9
      t28 = t18**2
      t41 = t20**2
      t62 = t1**2
      t63 = t62*t6
      t68 = tfunct5*tfunct1
      t85 = 86d0*x
      t87 = (25d0*t2+t85+25d0)*t6
      t94 = 32.D0/3.D0*t1*(131*t2-67)*t6*t9*tfunct1*zeta3+16.D0/9.D0*t1*
     #(35*t2-19)*t6*t9*t18*t20+4.D0/15.D0*t1*(t24-21)*t27*t28+64.D0/3.D0
     #*t1*(119*t2-151)*t27*tfunct18-8.D0/9.D0*t1*(43*t2-47)*t27*t41-64.D
     #0/3.D0*t1*(71*t2-55)*t27*tfunct24-64.D0/3.D0*t1*(t24-109)*t27*tfun
     #ct27+128.D0/3.D0*t1*(79*t2-47)*t27*tfunct21+1152*t63*tfunct2*tfunc
     #t1*tfunct4-768*t63*t68*tfunct2+32*t63*tfunct2*t18*tfunct1+64.D0/3.
     #D0*(x+4)*t8*t6*t18-2304*t63*tfunct2*tfunct11-16.D0/3.D0*t87*t20*tf
     #unct2+1536*t63*tfunct1*tfunct15
      t101 = t20*tfunct1
      t113 = tfunct2**2
      t154 = 128.D0/3.D0*(11*x-7)*t8*t6*tfunct1-96*t63*tfunct2*t101+1152
     #*t63*tfunct12*tfunct2+64.D0/3.D0*t87*t68+3456*t63*tfunct1*tfunct8-
     #288*t63*t20*t113-32.D0/9.D0*t6*(9*t2*x+82*t2+35*x+30)*t9*t101+768*
     #t63*t20*tfunct7+2304*t63*tfunct5*tfunct4-2304*t63*tfunct1*tfunct10
     #+768*t63*tfunct5*t20-2304*t63*tfunct1*tfunct13+640*t8*t1*t6*tfunct
     #3-1152*t63*t20*tfunct4-1920*t63*tfunct12*tfunct1-32*t87*tfunct1*tf
     #unct4
      t185 = t8**2
      t186 = t185*t6
      t209 = 2880*t63*tfunct1*tfunct11+1728*t63*tfunct2*zeta3-32*t63*t18
     #*tfunct4+64*t63*t18*tfunct6-16.D0/9.D0*t6*(29*t2+70*x+29)*t18*tfun
     #ct1-768*t63*tfunct32-32.D0/3.D0*(51*t2+82*x+51)*t6*tfunct16+1536*t
     #63*tfunct26+768*t186*tfunct2-32.D0/3.D0*t6*(t2+94*x+1)*zeta3+960*t
     #63*tfunct23+32.D0/3.D0*(t2+t85+1)*t6*tfunct9+32.D0/3.D0*(t2-106*x+
     #1)*t6*tfunct14-1536*t63*tfunct25-384*t63*tfunct28
      t226 = tfunct5**2
      t229 = tfunct4**2
      t232 = t6*t8
      t252 = -768*t63*tfunct29-576*t63*tfunct30-384*t63*tfunct31-1728*t6
     #3*tfunct22-16.D0/3.D0*(47*t2-126*x-85)*t6*t20+64*t87*tfunct11-384*
     #t63*t226-1728*t63*t229-768*t232*tfunct5-768*t232*tfunct6+768*t186*
     #tfunct7-3840*t63*tfunct17-32*t6*(19*t2+98*x+19)*tfunct12+1536*t63*
     #tfunct20-768*t63*tfunct19-1088*t186
      tmp = t94+t154+t209+t252
      twol_q_epm0 = tmp 
      END


C------- In the following we define the functions Aq1,Aq2a,Aq2b,Aq2c
C-------- ampl1      
      double complex function aq1(z,lambda,Mqu,QQQ)
      implicit none
c      implicit double precision (c)
      double precision s,t,u,z,lambda,QQQ
      double complex Mqu,boxf,triaf,bubf!,xx1,xx2
      
      
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      aq1 = 2d0 * (2d0 * s * t + t * u + s * u) / u *
     #boxf(Mqu, s, t, u) + 2d0
     # * (2d0 * t * u + s * t + s * u) / s * boxf(Mqu, t, u, s) 
     #+ 2d0 * (2d0 *
     # s * u + t * u + s * t) / t * boxf(Mqu, u, s, t) 
     #- 8d0 * (4d0 * t * u 
     #+ t ** 2 + u ** 2) / (u + t) ** 2 * s * bubf(Mqu, s, s + t + u) - 
     #8d0 * (u ** 2 + 4d0 * s * u + s ** 2) / (u + s) ** 2 * t * 
     # bubf(Mqu, t
     #, s + t + u) - 8d0 * (4d0 * s * t + s ** 2 + t ** 2) / 
     #(t + s) ** 2 * 
     #u * bubf(Mqu, u, s + t + u) + (-16d0 * (8d0 * t ** 3 * u ** 2 * s 
     #+ 4d0 
     #* t * u ** 4 * s + 8d0 * t * u ** 3 * s ** 2 + 8d0 * s ** 2 
     #* t ** 3 *
     # u + 8d0 * t ** 2 * u ** 3 * s + 4d0 * t ** 4 * s * u 
     #+ 4d0 * t ** 3 * u
     # ** 3 + 3d0 * t ** 2 * u ** 4 + 3d0 * t ** 4 * s ** 2 
     #+ 3d0 * s ** 2 * u
     # ** 4 + 3d0 * t ** 4 * u ** 2 + 4d0 * s ** 3 * u ** 3 
     #+ 3d0 * s ** 4 * u
     # ** 2 + 3d0 * s ** 4 * t ** 2 + 4d0 * t ** 3 * s ** 3 
     #+ 8d0 * s ** 3 * u
     # ** 2 * t + 4d0 * s ** 4 * u * t + 6d0 * s ** 2 * t ** 2 * u ** 2 
     #+ 8d0 
     #* s ** 3 * t ** 2 * u) / (u + s) ** 2 / (t + s) ** 2 / (u + t) ** 
     #2 * Mqu**2 + 4d0 * (5d0 * s ** 4 * u * t ** 2 
     #+ 5d0 * s * u ** 4 * 
     #t ** 2 +
     # 8d0 * s * u ** 3 * t ** 3 + 10d0 * s ** 2 * u ** 3 * t ** 2 
     #+ 10d0 * s 
     #** 2 * u ** 2 * t ** 3 + 10d0 * s ** 3 * u ** 2 * t ** 2 
     #+ 8d0 * s ** 
     #3 * u * t ** 3 + 5d0 * t ** 4 * u ** 2 * s + 
     #5d0 * s ** 2 * t ** 4 * u
     # + 5d0 * t * s ** 2 * u ** 4 + 8d0 * t * s ** 3 * u ** 3 
     #+ 5d0 * s ** 4 
     #* u ** 2 * t + 2d0 * u ** 4 * t ** 3 + 2d0 * u ** 3 * t ** 4 
     #+ 2d0 * s *
     #* 3d0 * u ** 4 + 2d0 * s ** 4 * u ** 3 
     #+ 2d0 * s ** 3 * t ** 4 
     #+ 2d0 * s ** 4 * t ** 3) / t / (u + t) 
     #/ s / (u + s) / u / (t + s)) 
     # * triaf(Mqu, s + t + u) 
     #+ (16d0 * (u ** 2 + t ** 2) / (u + t) ** 2 * Mqu**2 
     # - 4d0 * (2d0 * t ** 3 * s + 3d0 * s * t ** 2 * u 
     #          + 3d0 * s * u ** 2 * t 
     #          + 2d0 * s * u ** 3 + t ** 3 * u + u ** 3 * t) 
     #  / t / u / (u + t)) 
     #  * triaf(Mqu, s) 
     #+ (16d0 * (s ** 2 + u ** 2) / (u + s) ** 2 * Mqu**2 
     #  - 4d0 * (2d0 * t* s ** 3 
     #          + 3d0 * s ** 2 * u * t 
     #          + 3d0 * s * u ** 2 * t 
     #          + 2d0 * u ** 3 *t + s * u ** 3 + s ** 3 * u) 
     #          / s / u / (u + s)) 
     #      * triaf(Mqu, t) 
     # +
     # (16d0 * (t ** 2 + s ** 2) / (t + s) ** 2 * Mqu**2 - 4d0 * (2d0 * 
     #s ** 3 * 
     #u + 3d0 * s ** 2 * u * t + 3d0 * s * t ** 2 * u 
     #+ 2d0 * t ** 3 * u + t *
     #* 3d0 * s + t * s ** 3) / s / t / (t + s)) * triaf(Mqu, u) 
     # - 16d0 * (s
     # * t ** 2 * u + s ** 2 * u * t + s * u ** 2 * t + s ** 2 * t ** 2 
     #+ t ** 3 * u + s ** 3 * u + t ** 3 * s + t ** 2 * u ** 2 + s * u *
     #* 3 + t * s ** 3 + u ** 3 * t + s ** 2 * u ** 2) / (u + t) / (u + 
     #s) / (t + s)

      END


      double complex function aq2a(z,lambda,Mqu,QQQ)
      implicit none 
      double complex ampl2,Mqu
      double precision s,t,u,z,lambda,QQQ
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      aq2a=ampl2(s,t,u,Mqu)
      end
      
      double complex function aq2b(z,lambda,Mqu,QQQ)
      implicit none
      double complex ampl2,Mqu
      double precision s,t,u,z,lambda,QQQ
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      aq2b=ampl2(t,u,s,Mqu)
      end
      
      double complex function aq2c(z,lambda,Mqu,QQQ)
      implicit none
      double complex ampl2,Mqu
      double precision s,t,u,z,lambda,QQQ
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      aq2c=ampl2(u,s,t,Mqu)
      end

      double complex function ampl2(s,t,u,Mqu)
      implicit none
      double complex cg1,cg2,cg3,cg4,cg5,cg6,cg7,cg8,cg9,cg10,cg11
      double precision s,t,u
      double complex Mqu,boxf,triaf,bubf

      !print*,Mqu
        
      cg1 = (-16d0 * Mqu**2 - 2d0 * (-u ** 2 - t * u + 2d0 * s * t) / u)
      cg2 = (-16d0 * Mqu**2 + 2d0 * (s * t + s ** 2 - 2d0 * t * u) / s)
      cg3 =  (16d0 * Mqu**2 + 4d0 * s * u / t)
      cg4 = 8d0 * s * (-u + t) / (u + t)
      cg5 = 8d0 * (u ** 2 + 4d0 * s * u + s ** 2) / (u + s) ** 2 * t 
      cg6 = 8d0 * (s - t) / (t + s) * u
      cg7 =  (   -16d0 * (8d0 * s * t ** 2 * u + 3d0 * t ** 2 * u ** 2 
     #          + 5d0 * s ** 2 * u * t + u ** 3 * t + t * s ** 3 
     #          - s * u ** 3 + 3d0 * s ** 2 * t ** 2 - s** 3 * u 
     #          + 5d0 * s * u ** 2 * t
     #          ) / (u + s) ** 2 / (t + s) / (u + t)* Mqu**2 
     #      - 4d0 * (-s ** 2 * t ** 2 * u + 2d0 * s ** 3 * t ** 2 
     #              - s * t ** 2 * u ** 2 + 2d0 * t ** 2 * u ** 3 
     #              - 2d0 * t * u ** 2 * s ** 2 - 2d0 *s ** 2 * u ** 3 
     #              - 2d0 * s ** 3 * u ** 2
     #              ) / (u + s) / s / t / u)
      cg8 = (16d0 * (-u + t) / (u + t) * Mqu**2 
     #              + 4d0 * (-t **2 * u + 2d0 * s * t ** 2 
     #                  - 2d0 * u ** 2 * s) 
     #              / t / u)
      cg9 = (16d0 * (u ** 2 + 4d0 * s * u + s ** 2) / (u + s) ** 2 
     #              * Mqu**2 
     #          + 4d0 * (2d0 * t * s ** 3 - 2d0 * s ** 2 * u ** 2 
     #                  + s ** 2 * u * t + s * u ** 2 * t 
     #                  + 2d0 * u ** 3 * t) 
     #              / u / s / (u + s))
     
      cg10 = (-16d0 * (s - t) / (t + s) * Mqu**2 
     #          - 4d0 * (2d0 * u * s ** 2 + s * t ** 2 
     #           - 2d0 *t ** 2 * u) / s / t)
      cg11 = 16d0 * (s * u + s * t + t * u) / (u + s)
      
      
      ampl2 =  cg1 * boxf(Mqu, s, t, u) 
     #      +  cg2 * boxf(Mqu, t, u, s) 
     #      +  cg3 * boxf(Mqu, u, s,t) 
     #      +  cg4 * bubf(Mqu, s, s + t + u) 
     #      +  cg5 * bubf(Mqu, t, s +t + u) 
     #      -  cg6 * bubf(Mqu, u, s + t + u) 
     #      +  cg7 * triaf(Mqu, s + t + u) 
     #      +  cg8 * triaf(Mqu, s)
     #      +  cg9 * triaf(Mqu, t) 
     #      + cg10 * triaf(Mqu, u) 
     #      - cg11
     
           
       END


c------------------- kernel Aqqgh for NLO qq->gh and qg->qh

	double complex function aqqgh(z,mH,tau,Mq)
	implicit none
	double complex tau,Mq,x,brn_q_ep0,bubf,triaf,TS,TH
	double precision y,mH,s12,z
	
	y=1d0/z
	s12 = y*mH**2
	
	TS = triaf(Mq,s12)
	TH = triaf(Mq,mH**2)
	x = (sqrt(1d0-tau)-1d0) / (sqrt(1d0-tau)+1d0)
	
	aqqgh = 1d0/(1d0-y) * tau/64d0 * brn_q_ep0(x)
     $	+ y/(1d0-y)**2 * 2d0* bubf(Mq,s12,mH**2)
     $	+y*tau/(1d0-y)**2 * 
     $		(TS/y-TH)
     &	- 1d0/(1d0-y) * TS
      aqqgh = -aqqgh
	!the minus sign makes a diffference only in ewk interferences
	!With this minus sign the heavy mass limit 
	! tau->infinity => 3/4*tau*Aqqgh -> 1
     	end
	
	subroutine testing_Aqqgh_y_zero()
	implicit none
	double complex tau,Mq,res,limit,Aqqgh,x,brn_q_ep0
	double precision mH,s12,y
	integer i
	
	y=300.1234d0
	
	mH=123d0
	
	
	Mq = dcmplx(172d0,12d0)
	print*,"testing Aqqgh y->0, Mq=",Mq

	do i=1,40
		y=y/2d0
		!Mq=Mq+50d0
		s12=y* mH**2
		tau= 4d0*Mq**2/mH**2
		res = (3d0/4d0)*tau* Aqqgh(1d0/y,mH,tau,Mq)
		x = (sqrt(1d0-tau)-1d0) / (sqrt(1d0-tau)+1d0)
		limit = (3d0/4d0)*tau*tau/64d0 * brn_q_ep0(x)
		print*,"------"
		print*,"y=",y
		print*,"res=	",res
		print*,"limit=	",limit
		print*,"abs(res-limit)=",abs(res - limit)
	enddo
	
	end
	
	subroutine testing_Aqqgh_mt_infty()
	implicit none
	double complex tau,Mq,res,limit,Aqqgh,x,brn_q_ep0
	double precision mH,s12,y
	integer i
	
	y=3.1234d0
	
	mH=123d0
	
	
	Mq = dcmplx(10d0,1d0)
	print*,"testing Aqqgh Mq->infinity, y=",y

	do i=1,40
		!y=y/2d0
		Mq=Mq+50d0
		s12=y* mH**2
		tau= 4d0*Mq**2/mH**2
		res = (3d0/4d0)*tau* Aqqgh(1/y,mH,tau,Mq)
		x = (sqrt(1d0-tau)-1d0) / (sqrt(1d0-tau)+1d0)
		limit = (3d0/4d0)*tau*tau/64d0 * brn_q_ep0(x)
		print*,"Mq=",Mq,"\tres=",res
	enddo
	
	end



