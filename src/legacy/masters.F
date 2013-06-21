 
C-------- master integrals necessary for the NLO gg->Hg : 
C           note the modification by kinematical variables 
C           described below.


C-------- bubble: bubf(m,m1,m2) = B_0(Mqu,M1)-B_0(Mqu,M2)
C         note that the bubble alone would be UV div     
C         but it only appears as differences!
  
      double complex function bubf(Mqu,M1,M2)
      implicit none
      double precision M1,M2
      double complex Bubble,Mqu
      bubf = Bubble(Mqu,M1) - Bubble(Mqu,M2)
      END

      double complex function Bubble(m,s)
      implicit none
      double complex x,m,Ie
      double precision s 

        Ie = dcmplx(0d0,1e-15)
        x = sqrt(1d0-4d0*m**2/s)
        Bubble = 2d0 - log(m**2)
     #                 +x*log((x-1d0)/(x+1d0)+Ie)
      end

C-------- bubble: e-proportional piece of bub(s,m,m)      
	double complex function BubE_ep(Q,mzsq)
	implicit none
	double complex Q,mzsq,Mzsquare
	double complex x,HPL2
	double precision Pi
	
	Pi = 3.1415926535897932385d0
	
	Mzsquare= mzsq - dcmplx(0d0,1d-16)
	x=Q/Mzsquare
	
	BubE_ep = 4d0+ (1d0-x)/x * (
     &			Pi**2/6d0-HPL2(0,1,1d0/(1d0-x))
     &			+log(1-x)*log(x)
     &			-3d0/2d0*log(1d0-x)**2
     &			+2d0*log(1d0-x)
     &			)
	end
	
C-------- triangle: triaf(m,s) = C_0(m,s)*s     
      double complex function triaf(m,s)
      double complex m,x,Ie
      double precision s
        
      Ie = dcmplx(0d0,1e-15)
      x=sqrt(1d0-4d0*m**2/s)
      triaf = 0.5d0*(log((x-1)/(x+1)+Ie))**2
      end
     
C------ Box according to Glover & Baur, Nucl.Phys.B339(1990)
C       Note that boxf(m,s,t,u) = D0(m,s,t,u)*s*t
        double complex function boxf(m,s,t,u)
        implicit none    
        double precision s,t,u,Q
        double complex m
        double complex Box_Intv2
        double complex andre(0:2),cs,ct,cQ,zero
        Q = s+t+u
        if (abs(dimag(m)).lt.1d-10) then 
        
        boxf = ( -Box_Intv2(s,t,u,m,Q)
     #                   +Box_Intv2(s,t,u,m,s)
     #                   +Box_Intv2(s,t,u,m,t)
     #                 )
        else
        cs=dcmplx(s,0d0)
        ct=dcmplx(t,0d0)
        cQ=dcmplx(Q,0d0)
        zero=dcmplx(0d0,0d0)
        call avh_olo_d0c(andre,zero,zero,zero,cQ,
     #			cs,ct,
     #			m**2,m**2,m**2,m**2)
        
        boxf=andre(0)*cs*ct
        endif
        END

c-----  The basic integral of eq.B9 in Glover & Baur, Nucl.Phys.B339(1990)
c       version 2 : all imaginary parts are calculated explicitly by an Ie
c       All results are cross checked for s*t*u>0 
        double complex function Box_Intv2(s,t,u,m,Q)
        implicit none
        double precision s,t,u,Q
        double complex Sp,the_log_1,the_log_2,y_tau,y,m
        double complex x_tau,ry,r
c        double precision signQ
        double complex x_plus, x_minus,Ie,res


        if (s*t*u<0d0) then 
            print*,"[Box_Intv2]: u/s/t < 0 - risky kinematical region",
     #     "for the box implementation. Check against other libraries."
        endif
        !if (abs(dimag(m)).gt.1d-10) then 
            
        Ie=dcmplx(0d0,1e-15)

        r = 1d0 + 4d0*(m**2-Ie)*u/t/s!+Ie
        x_tau = sqrt(r)
c        signQ = 1d0
c        if (Q.LT.0d0) then 
c            signQ=-1d0
c        endif
        ry=1d0-4d0*(m**2-Ie)/Q!+Ie/Q!signQ*Ie
        y_tau = sqrt(ry)
        x_plus = 0.5d0*(1d0+x_tau)
        x_minus = 0.5d0*(1d0-x_tau)
        y = 0.5*(1d0 + y_tau)

        the_log_1 = log((x_tau-1d0)/(x_tau+1d0))
        
        the_log_2 = log(1d0 + (Q-Ie)*u/s/t)! +Ie)
        
        res = 2d0/x_tau * (  
     #                     Sp(x_minus/(x_minus - y )-Ie)
     #                    -Sp(x_plus /(x_plus  - y )+Ie)
     #                    +Sp(x_minus/( y - x_plus )+Ie)
     #                    -Sp(x_plus /( y -x_minus )-Ie)
     #                     +the_log_1*the_log_2
     #                        )

        ! end of zero-imaginary-part-for-m  case    
c        else
c            print*,"non-zero-imaginary-part-for-m"
c            r = 1d0 + 4d0*m**2*u/t/s!+Ie
c            x_tau = sqrt(r)
c        signQ = 1d0
c        if (Q.LT.0d0) then 
c            signQ=-1d0
c        endif
c        ry=1d0-4d0*m**2/Q!+Ie/Q!signQ*Ie
c        y_tau = sqrt(ry)
c        x_plus = 0.5d0*(1d0+x_tau)
c        x_minus = 0.5d0*(1d0-x_tau)
c        y = 0.5*(1d0 + y_tau)
c
c        the_log_1 = log((x_tau-1d0)/(x_tau+1d0))
c        
c        the_log_2 = log(1d0 + dcmplx(abs(Q*u/s/t),0d0))! +Ie)
c        
c        res = 2d0/x_tau * (  
c     #                     Sp(x_minus/(x_minus - y ))
c     #                    -Sp(x_plus /(x_plus  - y ))
c     #                    +Sp(x_minus/( y - x_plus ))
c     #                    -Sp(x_plus /( y -x_minus ))
c     #                     +the_log_1*the_log_2
c     #                        )
c        endif ! end of non-zero-imaginary-part-for-m case
c            print*,Sp(x_minus/(x_minus - y ))
c            print*,  -Sp(x_plus /(x_plus  - y ))
c            print*,         +Sp(x_minus/( y - x_plus ))
c            print*,           -Sp(x_plus /( y -x_minus ))
c            print*,+the_log_1*the_log_2,
c     #            (x_tau-1d0)/(x_tau+1d0),1d0 + (Q)*u/s/t,Q
c            print*,res
        Box_Intv2=res
        END

c---  Spence function Sp(z) =  Li2(z) =  HPL2(0,1;z)
        double complex function Sp(z)
        implicit none
        double complex HPL2,z
        
        Sp =  HPL2(0,1,z)
        
        END





