

C------- In the following we define the functions Aq1,Aq2a,Aq2b,Aq2c
C-------- ampl1      
      double complex function Aq1(z,lambda,Mqu,QQQ)
      implicit none
c      implicit double precision (c)
      double precision s,t,u,z,lambda,QQQ
      double complex Mqu,boxf,triaf,bubf!,xx1,xx2
      
      
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      Aq1 = 2d0 * (2d0 * s * t + t * u + s * u) / u * 
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


      double complex function Aq2a(z,lambda,Mqu,QQQ)
      implicit none 
      double complex ampl2,Mqu
      double precision s,t,u,z,lambda,QQQ
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      Aq2a=ampl2(s,t,u,Mqu)
      end
      
      double complex function Aq2b(z,lambda,Mqu,QQQ)
      implicit none
      double complex ampl2,Mqu
      double precision s,t,u,z,lambda,QQQ
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      Aq2b=ampl2(t,u,s,Mqu)
      end
      
      double complex function Aq2c(z,lambda,Mqu,QQQ)
      implicit none
      double complex ampl2,Mqu
      double precision s,t,u,z,lambda,QQQ
      
      
      s = QQQ**2 / z
      t = -s*(1d0-z)*lambda
      u = -s*(1d0-z)*(1d0-lambda)
      
      Aq2c=ampl2(u,s,t,Mqu)
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

	double complex function Aqqgh(z,mH,tau,Mq)
	implicit none
	double complex tau,Mq,x,brn_q_ep0,bubf,triaf,TS,TH
	double precision y,mH,s12,z
	
	y=1d0/z
	s12 = y*mH**2
	
	TS = triaf(Mq,s12)
	TH = triaf(Mq,mH**2)
	x = (sqrt(1d0-tau)-1d0) / (sqrt(1d0-tau)+1d0)
	
	Aqqgh = 1d0/(1d0-y) * tau/64d0 * brn_q_ep0(x)
     $	+ y/(1d0-y)**2 * 2d0* bubf(Mq,s12,mH**2)
     $	+y*tau/(1d0-y)**2 * 
     $		(TS/y-TH)
     &	- 1d0/(1d0-y) * TS
      Aqqgh = -Aqqgh
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


C	Solely for testing purposes
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

