  
      subroutine rrgg2gght11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gght11s1e1  
      doubleprecision rrgg2gght11s1e0  
      doubleprecision rrgg2gght11s1em1  
      doubleprecision rrgg2gght11s1em2  
      doubleprecision rrgg2gght11s1em3  
      doubleprecision rrgg2gght11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght11s1e1
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
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = t1 ** 2
      t15 = t14 ** 2
      t17 = x1 * t5
      t18 = t15 * wd * t17
      t19 = 0.1D1 / z
      t20 = x1 * z
      t21 = -z - x1 + t20
      t22 = 0.1D1 / t21
      t23 = t19 * t22
      t24 = x2 * x3
      t25 = x1 ** 2
      t26 = x4 * pi
      t27 = Sin(t26)
      t28 = t27 ** 2
      t31 = z ** 2
      t33 = t5 ** 2
      t34 = 0.1D1 / t31 * t33
      t36 = t34 * t22 * t8
      t39 = log(0.4D1 * t24 * t25 * t28 * t36)
      t40 = cos(t26)
      t41 = t40 ** 2
      t45 = Sqrt(x3 * t21 * t8)
      t46 = t45 ** 2
      t51 = t15 * t41
      t52 = t46 * wd
      t53 = t51 * t52
      t59 = 0.1D1 / x2
      t62 = t51 * t46
      t63 = wd * x1
      t66 = t62 * t63 * t5 * t19
      t69 = t62 * t63 * t5
      t75 = log(0.4D1 * x3 * t25 * t28 * t36)
      t78 = t52 * t17
      t79 = t75 * t15 * t41 * t78
      t81 = (-0.2D1 * t69 - t79) * t19
      t89 = t75 ** 2
      t99 = pi ** 2
      t101 = lh ** 2
      t108 = -(-0.360D3 * t18 * t23 * t39 * t41 * t46 - 0.720D3 * t53 * 
     #t17 * t23 * lh) * t59 / 0.40D2 + 0.18D2 * (0.2D1 * t66 + t81) * t2
     #2 * lh - 0.9D1 * (0.3D1 * t66 + 0.2D1 * t81 + (t69 + 0.2D1 * t79 +
     # t89 * t15 * t41 * t78 / 0.2D1) * t19) * t22 - t53 * t17 * t23 * (
     #-0.30D2 * t99 + 0.180D3 * t101) / 0.10D2
      t109 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, 
     #t108)
      t111 = x3 * z
      t112 = t3 * z
      t113 = t24 * z
      t114 = t3 * x2
      t115 = x2 * z
      t116 = t3 * t115
      t117 = sqrt(x2)
      t119 = -0.1D1 + t117
      t121 = t117 + 0.1D1
      t125 = Sqrt(-x3 * t119 * t121 * t21 * t8)
      t127 = 0.2D1 * t40 * t117 * t125
      t132 = x1 * x2
      t134 = z + x1 - t20 - t115 - t132 + t132 * z - t111 - t3 + t112 + 
     #t113 + t114 - t116 + t24 + t127
      t151 = log(-0.4D1 * t28 * x2 * x3 * t8 * t25 * t34 * t119 * t121 *
     # t22)
      t157 = (0.2D1 * t117 * x3 + 0.2D1 * t40 * t125 - t117) ** 2
      t166 = 0.90D2 * t18 * t23 * t151 * t157 + 0.180D3 * t18 * t23 * lh
     # * t157
      t169 = FJET(XB1, XB2, s, t2 * x1 * (-t111 - t3 + t112 + t113 + t11
     #4 - t116 - x2 + t24 + t127) * t22, -t7, -t2 * x1 * t134 * t22, t13
     #, s * t14 * x2 * t17 * t22, -t166 * t59 / 0.40D2)
      rrgg2gght11s1e1 = t109 * t108 - t169 * t166 * t59 / 0.40D2

      end function



      doubleprecision function rrgg2gght11s1e0
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
      t5 = -0.1D1 + x1
      t7 = t2 * t5 * x3
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t13 = t9 * t1 * t5
      t14 = t1 ** 2
      t15 = t14 ** 2
      t17 = x1 * t5
      t18 = t15 * wd * t17
      t19 = 0.1D1 / z
      t20 = x1 * z
      t21 = -z - x1 + t20
      t22 = 0.1D1 / t21
      t23 = t19 * t22
      t24 = x4 * pi
      t25 = cos(t24)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t21 * t8)
      t30 = t29 ** 2
      t32 = 0.1D1 / x2
      t37 = t15 * t26
      t38 = t30 * wd
      t44 = t37 * t30
      t45 = wd * x1
      t46 = t5 * t19
      t53 = x1 ** 2
      t55 = Sin(t24)
      t56 = t55 ** 2
      t58 = z ** 2
      t61 = t5 ** 2
      t66 = log(0.4D1 * x3 * t53 * t56 / t58 * t22 * t61 * t8)
      t76 = -0.9D1 * t18 * t23 * t26 * t30 * t32 + 0.18D2 * t37 * t38 * 
     #t17 * t23 * lh - 0.9D1 * (0.2D1 * t44 * t45 * t46 + (-0.2D1 * t44 
     #* t45 * t5 - t66 * t15 * t26 * t38 * t17) * t19) * t22
      t77 = FJET(XB1, XB2, s, t2 * t3, -t7, -t9 * t1 * x1, t13, 0.0D0, t
     #76)
      t79 = x3 * z
      t80 = t3 * z
      t81 = x2 * x3
      t82 = t81 * z
      t83 = t3 * x2
      t84 = x2 * z
      t85 = t3 * t84
      t86 = sqrt(x2)
      t94 = Sqrt(-x3 * (-0.1D1 + t86) * (t86 + 0.1D1) * t21 * t8)
      t96 = 0.2D1 * t25 * t86 * t94
      t101 = x1 * x2
      t103 = z + x1 - t20 - t84 - t101 + t101 * z - t79 - t3 + t80 + t82
     # + t83 - t85 + t81 + t96
      t116 = (0.2D1 * t86 * x3 + 0.2D1 * t25 * t94 - t86) ** 2
      t121 = FJET(XB1, XB2, s, t2 * x1 * (-t79 - t3 + t80 + t82 + t83 - 
     #t85 - x2 + t81 + t96) * t22, -t7, -t2 * x1 * t103 * t22, t13, s * 
     #t14 * x2 * t17 * t22, 0.9D1 / 0.4D1 * t18 * t23 * t116 * t32)
      rrgg2gght11s1e0 = t77 * t76 + 0.9D1 / 0.4D1 * t121 * t15 * t45 * t
     #46 * t22 * t116 * t32

      end function



      doubleprecision function rrgg2gght11s1em1
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
      t5 = -0.1D1 + x1
      t8 = -0.1D1 + x3
      t9 = t8 * s
      t14 = t1 ** 2
      t15 = t14 ** 2
      t19 = 0.1D1 / z
      t21 = -z - x1 + x1 * z
      t22 = 0.1D1 / t21
      t25 = cos(x4 * pi)
      t26 = t25 ** 2
      t29 = Sqrt(x3 * t21 * t8)
      t30 = t29 ** 2
      t35 = FJET(XB1, XB2, s, t2 * x1 * x3, -t2 * t5 * x3, -t9 * t1 * x1
     #, t9 * t1 * t5, 0.0D0, -0.9D1 * t15 * wd * x1 * t5 * t19 * t22 * t
     #26 * t30)
      rrgg2gght11s1em1 = -0.9D1 * t35 * t15 * wd * x1 * t5 * t19 * t22 *
     # t26 * t30

      end function



      doubleprecision function rrgg2gght11s1em2
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
      rrgg2gght11s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gght11s1em3
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
      rrgg2gght11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght11s1em4
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
      rrgg2gght11s1em4 = 0.0D0

      end function
