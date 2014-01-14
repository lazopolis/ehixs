      subroutine rrgg2qqbarht10
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2qqbarhsoftt10
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2qqbarhhardt10
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2qqbarhhardt10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhhardt10s1e1  
      doubleprecision rrgg2qqbarhhardt10s1e0  
      doubleprecision rrgg2qqbarhhardt10s1em1  
      doubleprecision rrgg2qqbarhhardt10s1em2  
      doubleprecision rrgg2qqbarhhardt10s1em3  
      doubleprecision rrgg2qqbarhhardt10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhhardt10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhhardt10s1e1
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
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = x1 * t6
      t21 = 0.1D1 / z
      t23 = t19 * t20 * t21
      t25 = x1 ** 2
      t27 = x4 * pi
      t28 = Sin(t27)
      t29 = t28 ** 2
      t30 = z ** 2
      t31 = 0.1D1 / t30
      t35 = t6 ** 2
      t37 = x3 * t9 * t16 * t35
      t40 = log(0.4D1 * x2 * t25 * t29 * t31 * t37)
      t41 = cos(t27)
      t42 = t41 ** 2
      t46 = Sqrt(x3 * t15 * t9)
      t47 = t46 ** 2
      t52 = t19 * x1
      t53 = t6 * t21
      t59 = (0.180D3 * t52 * t53 * lh - 0.90D2 * t23) * wd
      t65 = 0.1D1 / x2
      t73 = log(0.4D1 * t25 * t29 * t31 * t37)
      t75 = t73 * t16 * t18
      t78 = (-t19 - t75) * x1 * t53
      t84 = t73 ** 2
      t92 = lh ** 2
      t94 = pi ** 2
      t105 = -(0.360D3 * t23 * wd * nf * t40 * t42 * t47 + 0.4D1 * t59 *
     # nf * t42 * t47) * t65 / 0.120D3 - (0.180D3 * (0.2D1 * t23 + t78) 
     #* lh - 0.270D3 * t23 - 0.180D3 * t78 - 0.90D2 * (t75 + t84 * t16 *
     # t18 / 0.2D1) * x1 * t53 + t52 * t53 * (-0.180D3 * t92 + 0.30D2 * 
     #t94)) * t42 * t47 * wd * nf / 0.30D2
      t106 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0,
     # t105)
      t108 = x3 * z
      t109 = t3 * z
      t110 = x2 * x3
      t111 = t110 * z
      t112 = t110 * x1
      t113 = t110 * t14
      t114 = sqrt(x2)
      t116 = -0.1D1 + t114
      t118 = t114 + 0.1D1
      t122 = Sqrt(-x3 * t116 * t118 * t15 * t9)
      t124 = 0.2D1 * t41 * t114 * t122
      t130 = x2 * x1
      t132 = z + x1 - t14 - x2 * z - t130 + t130 * z - t108 - t3 + t109 
     #+ t111 + t112 - t113 + t110 + t124
      t152 = log(-0.4D1 * t16 * x2 * t25 * t35 * t29 * t31 * t116 * t118
     # * x3 * t9)
      t159 = (0.2D1 * t114 * x3 - t114 + 0.2D1 * t41 * t122) ** 2
      t166 = -0.90D2 * t19 * t20 * t21 * wd * nf * t152 * t159 - t59 * n
     #f * t159
      t169 = FJET(XB1, XB2, s, t2 * x1 * (-t108 - t3 + t109 + t111 + t11
     #2 - t113 - x2 + t110 + t124) * t16, -t8, -t2 * x1 * t132 * t16, t1
     #3, s * t17 * x2 * t20 * t16, -t166 * t65 / 0.120D3)
      rrgg2qqbarhhardt10s1e1 = t106 * t105 - t169 * t166 * t65 / 0.120D3

      end function



      doubleprecision function rrgg2qqbarhhardt10s1e0
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
      t14 = x1 * z
      t15 = -z - x1 + t14
      t16 = 0.1D1 / t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t19 = t16 * t18
      t20 = x1 * t6
      t21 = 0.1D1 / z
      t23 = t19 * t20 * t21
      t25 = x4 * pi
      t26 = cos(t25)
      t27 = t26 ** 2
      t30 = Sqrt(x3 * t15 * t9)
      t31 = t30 ** 2
      t33 = 0.1D1 / x2
      t39 = t6 * t21
      t44 = x1 ** 2
      t45 = Sin(t25)
      t46 = t45 ** 2
      t48 = z ** 2
      t52 = t6 ** 2
      t57 = log(0.4D1 * t44 * t46 / t48 * x3 * t9 * t16 * t52)
      t70 = 0.3D1 * t23 * wd * nf * t27 * t31 * t33 - (0.180D3 * t19 * x
     #1 * t39 * lh - 0.180D3 * t23 - 0.90D2 * (-t19 - t57 * t16 * t18) *
     # x1 * t39) * t27 * t31 * wd * nf / 0.30D2
      t71 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t70)
      t73 = x3 * z
      t74 = t3 * z
      t75 = x2 * x3
      t76 = t75 * z
      t77 = t75 * x1
      t78 = t75 * t14
      t79 = sqrt(x2)
      t87 = Sqrt(-x3 * (-0.1D1 + t79) * (t79 + 0.1D1) * t15 * t9)
      t89 = 0.2D1 * t26 * t79 * t87
      t95 = x2 * x1
      t97 = z + x1 - t14 - x2 * z - t95 + t95 * z - t73 - t3 + t74 + t76
     # + t77 - t78 + t75 + t89
      t112 = (0.2D1 * t79 * x3 - t79 + 0.2D1 * t26 * t87) ** 2
      t115 = t21 * wd * nf * t112 * t33
      t118 = FJET(XB1, XB2, s, t2 * x1 * (-t73 - t3 + t74 + t76 + t77 - 
     #t78 - x2 + t75 + t89) * t16, -t8, -t2 * x1 * t97 * t16, t13, s * t
     #17 * x2 * t20 * t16, -0.3D1 / 0.4D1 * t19 * t20 * t115)
      rrgg2qqbarhhardt10s1e0 = t71 * t70 - 0.3D1 / 0.4D1 * t118 * t16 * 
     #t18 * x1 * t6 * t115

      end function



      doubleprecision function rrgg2qqbarhhardt10s1em1
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
      t6 = -0.1D1 + x1
      t7 = t1 * t6
      t9 = -0.1D1 + x3
      t10 = t9 * s
      t15 = -z - x1 + x1 * z
      t16 = 0.1D1 / t15
      t17 = t1 ** 2
      t18 = t17 ** 2
      t25 = cos(x4 * pi)
      t26 = t25 ** 2
      t30 = Sqrt(x3 * t15 * t9)
      t31 = t30 ** 2
      t33 = 0.1D1 / z * wd * nf * t26 * t31
      t36 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, 0.3D1 * t16 * t18 * x1 * t6 * t33)
      rrgg2qqbarhhardt10s1em1 = 0.3D1 * t36 * t16 * t18 * x1 * t6 * t33

      end function



      doubleprecision function rrgg2qqbarhhardt10s1em2
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
      rrgg2qqbarhhardt10s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt10s1em3
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
      rrgg2qqbarhhardt10s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhhardt10s1em4
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
      rrgg2qqbarhhardt10s1em4 = 0.0D0

      end function
  
      subroutine rrgg2qqbarhsoftt10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarhsoftt10s1e1  
      doubleprecision rrgg2qqbarhsoftt10s1e0  
      doubleprecision rrgg2qqbarhsoftt10s1em1  
      doubleprecision rrgg2qqbarhsoftt10s1em2  
      doubleprecision rrgg2qqbarhsoftt10s1em3  
      doubleprecision rrgg2qqbarhsoftt10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarhsoftt10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarhsoftt10s1e1
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
      rrgg2qqbarhsoftt10s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt10s1e0
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
      rrgg2qqbarhsoftt10s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt10s1em1
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
      rrgg2qqbarhsoftt10s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt10s1em2
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
      rrgg2qqbarhsoftt10s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt10s1em3
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
      rrgg2qqbarhsoftt10s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarhsoftt10s1em4
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
      rrgg2qqbarhsoftt10s1em4 = 0.0D0

      end function
