  
      subroutine rrgg2qqbarht10
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarht10s1e1  
      doubleprecision rrgg2qqbarht10s1e0  
      doubleprecision rrgg2qqbarht10s1em1  
      doubleprecision rrgg2qqbarht10s1em2  
      doubleprecision rrgg2qqbarht10s1em3  
      doubleprecision rrgg2qqbarht10s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht10s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht10s1e1
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
      t15 = 0.1D1 / z
      t16 = t15 * t6
      t17 = t1 ** 2
      t18 = t17 ** 2
      t20 = t14 * t16 * t18
      t21 = x1 * z
      t22 = -z - x1 + t21
      t23 = 0.1D1 / t22
      t25 = x1 ** 2
      t27 = x4 * pi
      t28 = Sin(t27)
      t29 = t28 ** 2
      t30 = z ** 2
      t31 = 0.1D1 / t30
      t34 = t6 ** 2
      t36 = x3 * t9
      t40 = log(0.4D1 * t23 * t25 * t29 * t31 * t34 * x2 * t36)
      t41 = cos(t27)
      t42 = t41 ** 2
      t46 = Sqrt(x3 * t22 * t9)
      t47 = t46 ** 2
      t59 = (-0.180D3 * t14 * t15 * t6 * t18 * lh + 0.90D2 * t20) * x1
      t65 = 0.1D1 / x2
      t68 = t42 * t47
      t73 = t68 * t14
      t81 = log(0.4D1 * t25 * t29 * t31 * t36 * t23 * t34)
      t84 = t47 * wd * nf
      t85 = t81 * t42 * t84
      t88 = (0.2D1 * t73 - t85) * t15 * t6
      t95 = t81 ** 2
      t105 = lh ** 2
      t107 = pi ** 2
      t117 = -(0.360D3 * t20 * x1 * t23 * t40 * t42 * t47 - 0.4D1 * t59 
     #* t23 * t42 * t47) * t65 / 0.120D3 + (-0.180D3 * (-t68 * wd * nf *
     # t15 * t6 + t88) * t18 * lh + 0.90D2 * (-t88 + (0.3D1 * t73 - 0.2D
     #1 * t85 + t95 * t42 * t84 / 0.2D1) * t15 * t6) * t18 + t73 * t16 *
     # t18 * (0.180D3 * t105 - 0.30D2 * t107)) * x1 * t23 / 0.30D2
      t118 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0,
     # t117)
      t120 = x3 * z
      t121 = t3 * z
      t122 = x2 * x3
      t123 = t122 * z
      t124 = t122 * x1
      t125 = t122 * t21
      t126 = sqrt(x2)
      t128 = -0.1D1 + t126
      t130 = t126 + 0.1D1
      t134 = Sqrt(-x3 * t128 * t130 * t22 * t9)
      t136 = 0.2D1 * t41 * t126 * t134
      t142 = x2 * x1
      t144 = z + x1 - t21 - x2 * z - t142 + t142 * z - t120 - t3 + t121 
     #+ t123 + t124 - t125 + t122 + t136
      t165 = log(-0.4D1 * t31 * t34 * t128 * t130 * t25 * x2 * t23 * t29
     # * x3 * t9)
      t172 = (0.2D1 * t126 * x3 + 0.2D1 * t41 * t134 - t126) ** 2
      t179 = -0.90D2 * t14 * t16 * t18 * x1 * t23 * t165 * t172 + t59 * 
     #t23 * t172
      t182 = FJET(XB1, XB2, s, t2 * x1 * (-t120 - t3 + t121 + t123 + t12
     #4 - t125 - x2 + t122 + t136) * t23, -t8, -t2 * x1 * t144 * t23, t1
     #3, s * t17 * x2 * x1 * t6 * t23, -t179 * t65 / 0.120D3)
      rrgg2qqbarht10s1e1 = t118 * t117 - t182 * t179 * t65 / 0.120D3

      end function



      doubleprecision function rrgg2qqbarht10s1e0
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
      t15 = 0.1D1 / z
      t16 = t15 * t6
      t17 = t1 ** 2
      t18 = t17 ** 2
      t21 = x1 * z
      t22 = -z - x1 + t21
      t23 = 0.1D1 / t22
      t25 = x4 * pi
      t26 = cos(t25)
      t27 = t26 ** 2
      t30 = Sqrt(x3 * t22 * t9)
      t31 = t30 ** 2
      t32 = t27 * t31
      t33 = 0.1D1 / x2
      t38 = t32 * t14
      t45 = nf * t15 * t6
      t48 = x1 ** 2
      t49 = Sin(t25)
      t50 = t49 ** 2
      t52 = z ** 2
      t56 = t6 ** 2
      t61 = log(0.4D1 * t48 * t50 / t52 * x3 * t9 * t23 * t56)
      t76 = 0.3D1 * t14 * t16 * t18 * x1 * t23 * t32 * t33 + (-0.180D3 *
     # t38 * t16 * t18 * lh + 0.90D2 * (-t32 * wd * t45 + (0.2D1 * t38 -
     # t61 * t27 * t31 * wd * nf) * t15 * t6) * t18) * x1 * t23 / 0.30D2
      t77 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t76)
      t79 = x3 * z
      t80 = t3 * z
      t81 = x2 * x3
      t82 = t81 * z
      t83 = t81 * x1
      t84 = t81 * t21
      t85 = sqrt(x2)
      t93 = Sqrt(-x3 * (-0.1D1 + t85) * (t85 + 0.1D1) * t22 * t9)
      t95 = 0.2D1 * t26 * t85 * t93
      t101 = x2 * x1
      t103 = z + x1 - t21 - x2 * z - t101 + t101 * z - t79 - t3 + t80 + 
     #t82 + t83 - t84 + t81 + t95
      t119 = (0.2D1 * t85 * x3 + 0.2D1 * t26 * t93 - t85) ** 2
      t122 = t18 * x1 * t23 * t119 * t33
      t125 = FJET(XB1, XB2, s, t2 * x1 * (-t79 - t3 + t80 + t82 + t83 - 
     #t84 - x2 + t81 + t95) * t23, -t8, -t2 * x1 * t103 * t23, t13, s * 
     #t17 * x2 * x1 * t6 * t23, -0.3D1 / 0.4D1 * t14 * t16 * t122)
      rrgg2qqbarht10s1e0 = t77 * t76 - 0.3D1 / 0.4D1 * t125 * wd * t45 *
     # t122

      end function



      doubleprecision function rrgg2qqbarht10s1em1
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
      t15 = 0.1D1 / z
      t18 = t1 ** 2
      t19 = t18 ** 2
      t22 = -z - x1 + x1 * z
      t25 = cos(x4 * pi)
      t26 = t25 ** 2
      t30 = Sqrt(x3 * t22 * t9)
      t31 = t30 ** 2
      t33 = t19 * x1 / t22 * t26 * t31
      t36 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, 0.3D1 * wd * nf * t15 * t6 * t33)
      rrgg2qqbarht10s1em1 = 0.3D1 * t36 * wd * nf * t15 * t6 * t33

      end function



      doubleprecision function rrgg2qqbarht10s1em2
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
      rrgg2qqbarht10s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht10s1em3
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
      rrgg2qqbarht10s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht10s1em4
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
      rrgg2qqbarht10s1em4 = 0.0D0

      end function
