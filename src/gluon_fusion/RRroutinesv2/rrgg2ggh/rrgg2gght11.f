      subroutine rrgg2gght11
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      if(z.eq.1d0)then
         call rrgg2gghsoftt11
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      else
         call rrgg2gghhardt11
     &     (sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      end if
      end subroutine

  
      subroutine rrgg2gghhardt11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghhardt11s1e1  
      doubleprecision rrgg2gghhardt11s1e0  
      doubleprecision rrgg2gghhardt11s1em1  
      doubleprecision rrgg2gghhardt11s1em2  
      doubleprecision rrgg2gghhardt11s1em3  
      doubleprecision rrgg2gghhardt11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghhardt11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghhardt11s1e1
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
      t14 = x1 * t6
      t15 = x1 * z
      t16 = -z - x1 + t15
      t17 = 0.1D1 / t16
      t18 = t17 * wd
      t19 = t14 * t18
      t20 = t1 ** 2
      t21 = t20 ** 2
      t22 = 0.1D1 / z
      t23 = t21 * t22
      t24 = x1 ** 2
      t25 = x4 * pi
      t26 = Sin(t25)
      t27 = t26 ** 2
      t29 = z ** 2
      t30 = 0.1D1 / t29
      t34 = t6 ** 2
      t35 = t34 * t9
      t39 = log(0.4D1 * t24 * t27 * t30 * x2 * x3 * t17 * t35)
      t40 = cos(t25)
      t41 = t40 ** 2
      t45 = Sqrt(x3 * t16 * t9)
      t46 = t45 ** 2
      t51 = t14 * t17
      t53 = wd * t21 * t22
      t56 = t14 * lh
      t61 = -0.180D3 * t51 * t53 + (0.180D3 * t56 + 0.180D3 * t14) * t17
     # * t53
      t66 = 0.1D1 / x2
      t69 = t41 * t46
      t83 = log(0.4D1 * x3 * t24 * t27 * t30 * t17 * t35)
      t85 = t83 * t41 * t46
      t87 = (-0.2D1 * t69 - t85) * x1
      t98 = t83 ** 2
      t106 = lh ** 2
      t108 = pi ** 2
      t117 = (0.360D3 * t19 * t23 * t39 * t41 * t46 + 0.4D1 * t61 * t41 
     #* t46) * t66 / 0.40D2 - 0.27D2 * t69 * t14 * t18 * t23 + (0.180D3 
     #* t69 * t56 - 0.90D2 * t87 * t6) * t17 * t53 / 0.5D1 + (0.180D3 * 
     #t87 * t6 * lh - 0.90D2 * (t69 + 0.2D1 * t85 + t98 * t41 * t46 / 0.
     #2D1) * x1 * t6 + t69 * t14 * (-0.180D3 * t106 + 0.30D2 * t108)) * 
     #t17 * t53 / 0.10D2
      t118 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0,
     # t117)
      t120 = x3 * z
      t121 = t3 * z
      t122 = x2 * x3
      t123 = t122 * z
      t124 = t3 * x2
      t125 = x2 * z
      t126 = t3 * t125
      t127 = sqrt(x2)
      t129 = -0.1D1 + t127
      t131 = t127 + 0.1D1
      t135 = Sqrt(-x3 * t129 * t131 * t16 * t9)
      t137 = 0.2D1 * t40 * t127 * t135
      t142 = x1 * x2
      t144 = z + x1 - t15 - t125 - t142 + t142 * z - t120 - t3 + t121 + 
     #t123 + t124 - t126 + t122 + t137
      t161 = log(-0.4D1 * t27 * x2 * x3 * t9 * t24 * t34 * t17 * t30 * t
     #129 * t131)
      t167 = (0.2D1 * t127 * x3 - t127 + 0.2D1 * t40 * t135) ** 2
      t173 = -0.90D2 * t19 * t23 * t161 * t167 - t61 * t167
      t176 = FJET(XB1, XB2, s, t2 * x1 * (-t120 - t3 + t121 + t123 + t12
     #4 - t126 - x2 + t122 + t137) * t17, -t8, -t2 * x1 * t144 * t17, t1
     #3, s * t20 * x2 * t51, t173 * t66 / 0.40D2)
      rrgg2gghhardt11s1e1 = t118 * t117 + t176 * t173 * t66 / 0.40D2

      end function



      doubleprecision function rrgg2gghhardt11s1e0
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
      t14 = x4 * pi
      t15 = cos(t14)
      t16 = t15 ** 2
      t17 = x1 * z
      t18 = -z - x1 + t17
      t21 = Sqrt(x3 * t18 * t9)
      t22 = t21 ** 2
      t23 = t16 * t22
      t24 = x1 * t6
      t25 = t23 * t24
      t26 = 0.1D1 / t18
      t27 = t26 * wd
      t28 = t1 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / z
      t31 = t29 * t30
      t32 = 0.1D1 / x2
      t44 = x1 ** 2
      t46 = Sin(t14)
      t47 = t46 ** 2
      t49 = z ** 2
      t52 = t6 ** 2
      t57 = log(0.4D1 * x3 * t44 * t47 / t49 * t26 * t52 * t9)
      t66 = wd * t29
      t70 = -0.9D1 * t25 * t27 * t31 * t32 - 0.18D2 * t25 * t27 * t31 + 
     #(0.180D3 * t23 * t24 * lh - 0.90D2 * (-0.2D1 * t23 - t57 * t16 * t
     #22) * x1 * t6) * t26 * t66 * t30 / 0.10D2
      t71 = FJET(XB1, XB2, s, t2 * t3, -t8, -t10 * t1 * x1, t13, 0.0D0, 
     #t70)
      t73 = x3 * z
      t74 = t3 * z
      t75 = x2 * x3
      t76 = t75 * z
      t77 = t3 * x2
      t78 = x2 * z
      t79 = t3 * t78
      t80 = sqrt(x2)
      t88 = Sqrt(-x3 * (-0.1D1 + t80) * (t80 + 0.1D1) * t18 * t9)
      t90 = 0.2D1 * t15 * t80 * t88
      t95 = x1 * x2
      t97 = z + x1 - t17 - t78 - t95 + t95 * z - t73 - t3 + t74 + t76 + 
     #t77 - t79 + t75 + t90
      t111 = (0.2D1 * t80 * x3 - t80 + 0.2D1 * t15 * t88) ** 2
      t116 = FJET(XB1, XB2, s, t2 * x1 * (-t73 - t3 + t74 + t76 + t77 - 
     #t79 - x2 + t75 + t90) * t26, -t8, -t2 * x1 * t97 * t26, t13, s * t
     #28 * x2 * t24 * t26, 0.9D1 / 0.4D1 * t24 * t27 * t31 * t111 * t32)
      rrgg2gghhardt11s1e0 = t71 * t70 + 0.9D1 / 0.4D1 * t116 * x1 * t6 *
     # t26 * t66 * t30 * t111 * t32

      end function



      doubleprecision function rrgg2gghhardt11s1em1
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
      t15 = cos(x4 * pi)
      t16 = t15 ** 2
      t18 = -z - x1 + x1 * z
      t21 = Sqrt(x3 * t18 * t9)
      t22 = t21 ** 2
      t26 = 0.1D1 / t18
      t28 = t1 ** 2
      t29 = t28 ** 2
      t30 = 0.1D1 / z
      t35 = FJET(XB1, XB2, s, s * t1 * x1 * x3, -x3 * s * t7, -t10 * t1 
     #* x1, t10 * t7, 0.0D0, -0.9D1 * t16 * t22 * x1 * t6 * t26 * wd * t
     #29 * t30)
      rrgg2gghhardt11s1em1 = -0.9D1 * t35 * t16 * t22 * x1 * t6 * t26 * 
     #wd * t29 * t30

      end function



      doubleprecision function rrgg2gghhardt11s1em2
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
      rrgg2gghhardt11s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt11s1em3
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
      rrgg2gghhardt11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghhardt11s1em4
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
      rrgg2gghhardt11s1em4 = 0.0D0

      end function
  
      subroutine rrgg2gghsoftt11
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2gghsoftt11s1e1  
      doubleprecision rrgg2gghsoftt11s1e0  
      doubleprecision rrgg2gghsoftt11s1em1  
      doubleprecision rrgg2gghsoftt11s1em2  
      doubleprecision rrgg2gghsoftt11s1em3  
      doubleprecision rrgg2gghsoftt11s1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt11s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt11s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt11s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt11s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt11s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gghsoftt11s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gghsoftt11s1e1
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
      rrgg2gghsoftt11s1e1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt11s1e0
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
      rrgg2gghsoftt11s1e0 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt11s1em1
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
      rrgg2gghsoftt11s1em1 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt11s1em2
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
      rrgg2gghsoftt11s1em2 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt11s1em3
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
      rrgg2gghsoftt11s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gghsoftt11s1em4
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
      rrgg2gghsoftt11s1em4 = 0.0D0

      end function
