      subroutine rrgg2qqbarht13
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt13
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt13
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhardt13s1e1  
      doubleprecision rrgg2qqbarhhardt13s1e0  
      doubleprecision rrgg2qqbarhhardt13s1em1  
      doubleprecision rrgg2qqbarhhardt13s1em2  
      doubleprecision rrgg2qqbarhhardt13s1em3  
      doubleprecision rrgg2qqbarhhardt13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt13s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = x3 * x1
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t8 = x3 * s * t7
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t13 = t10 * t7
      t14 = wd * nf
      t15 = t1 ** 2
      t16 = t14 * t15
      t17 = x4 * pi
      t18 = cos(t17)
      t19 = t18 ** 2
      t20 = x1 * z
      t21 = -z - x1 + t20
      t24 = Sqrt(x3 * t21 * t9)
      t25 = t24 ** 2
      t26 = t19 * t25
      t27 = 0.1D1 / t21
      t28 = 0.1D1 / x2
      t33 = t26 * t15
      t41 = x1 ** 2
      t42 = Sin(t17)
      t43 = t42 ** 2
      t45 = z ** 2
      t49 = t6 ** 2
      t54 = log(0.4D1 * t41 * t43 / t45 * x3 * t9 * t27 * t49)
      t62 = 0.6D1 * t16 * t26 * t27 * t28 + (0.180D3 * t14 * t33 - 0.180
     #D3 * t14 * t19 * t25 * t15 * lh - 0.90D2 * t54 * wd * nf * t33) * 
     #t27 / 0.15D2
      t63 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t62)
      t65 = x3 * z
      t66 = t3 * z
      t67 = x2 * x3
      t68 = t67 * z
      t69 = t67 * x1
      t70 = t67 * t20
      t71 = sqrt(x2)
      t79 = Sqrt(-x3 * (-0.1D1 + t71) * (t71 + 0.1D1) * t21 * t9)
      t81 = 0.2D1 * t18 * t71 * t79
      t87 = x2 * x1
      t88 = t87 * z
      t89 = z + x1 - t20 - x2 * z - t87 + t88 - t65 - t3 + t66 + t68 + t
     #69 - t70 + t67 + t81
      t98 = t21 ** 2
      t100 = t18 * t79
      t105 = t71 * x1
      t108 = x3 * t71
      t113 = x3 * t41
      t118 = t71 * t41
      t125 = t71 * x2
      t126 = t125 * x3
      t131 = t41 * t125
      t134 = x1 * t125
      t137 = -0.4D1 * t100 * z - 0.4D1 * t100 * x1 + 0.2D1 * t105 * z - 
     #0.2D1 * t108 * z - 0.2D1 * t108 * x1 - 0.4D1 * t113 * t71 - 0.3D1 
     #* t105 * t45 - 0.4D1 * t118 * z + 0.2D1 * t118 * t45 - 0.2D1 * t10
     #8 * t45 + 0.2D1 * t126 * t41 + 0.2D1 * t126 * x1 + 0.2D1 * t131 * 
     #z + t134 * t45 - t131 * t45
      t138 = t71 * z
      t139 = t71 * t45
      t143 = x2 * t18
      t144 = t79 * x1
      t168 = t138 + t105 - t134 + t139 - t131 + 0.2D1 * t118 + 0.4D1 * t
     #100 * t20 + 0.4D1 * t143 * t144 - 0.4D1 * t108 * t20 + 0.6D1 * x3 
     #* t45 * t105 + 0.8D1 * t113 * t138 - 0.4D1 * t113 * t139 - 0.2D1 *
     # t126 * t45 * x1 - 0.4D1 * t126 * t41 * z + 0.2D1 * t126 * t45 * t
     #41 - 0.4D1 * t143 * t144 * z
      t170 = (t137 + t168) ** 2
      t175 = 0.1D1 / t98 * t170 / (z + t88 - t87 + x1 - t20) * t28
      t178 = FJET(XB1, XB2, s, t2 * x1 * (-t65 - t3 + t66 + t68 + t69 - 
     #t70 - x2 + t67 + t81) * t27, -t8, -t2 * x1 * t89 * t27, t13, s * t
     #15 * x2 * x1 * t6 * t27, 0.3D1 / 0.8D1 * t16 * t175)
      rrgg2qqbarhhardt13s1e1 = t63 * t62 + 0.3D1 / 0.8D1 * t178 * wd * n
     #f * t15 * t175

      end function



      doubleprecision function rrgg2qqbarhhardt13s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      t1 = -0.1D1 + z
      t7 = t1 * (-0.1D1 + x1)
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t15 = t1 ** 2
      t18 = cos(x4 * pi)
      t19 = t18 ** 2
      t21 = -z - x1 + x1 * z
      t24 = Sqrt(x3 * t21 * t9)
      t25 = t24 ** 2
      t27 = 0.1D1 / t21
      t31 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, 0.6D1 * wd * nf * t15 * t19 * t25 * t27)
      rrgg2qqbarhhardt13s1e0 = 0.6D1 * t31 * wd * nf * t15 * t19 * t25 *
     # t27

      end function



      doubleprecision function rrgg2qqbarhhardt13s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt13s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhhardt13s1em4 = 0.0D0

      end function
  
      subroutine rrgg2qqbarhsoftt13
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt13s1e1  
      doubleprecision rrgg2qqbarhsoftt13s1e0  
      doubleprecision rrgg2qqbarhsoftt13s1em1  
      doubleprecision rrgg2qqbarhsoftt13s1em2  
      doubleprecision rrgg2qqbarhsoftt13s1em3  
      doubleprecision rrgg2qqbarhsoftt13s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt13s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt13s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt13s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt13s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt13s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt13s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt13s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt13s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt13s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt13s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt13s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt13s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt13s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt13s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt13s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt13s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt13s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s
      doubleprecision XB1
      doubleprecision XB2
      doubleprecision z
      doubleprecision lh
      doubleprecision wd
      doubleprecision nf
      doubleprecision x1
      doubleprecision x2
      doubleprecision x3
      doubleprecision x4

      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log

      doubleprecision Pi
      parameter(pi=3.141592653589793d0)
      doubleprecision zeta3
      parameter(zeta3=1.202056903159594d0)
      doubleprecision zeta5
      parameter(zeta5=1.036927755143370d0)
      doubleprecision zeta7
      parameter(zeta7=1.008349277381923d0)
      rrgg2qqbarhsoftt13s1em4 = 0.0D0

      end function
