  
      subroutine bqqbH2n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision bqqbH21J1  
      doubleprecision bqqbH21J2  
      doubleprecision bqqbH2n1e1  
      doubleprecision bqqbH2n1e0  
      doubleprecision bqqbH2n1em1  
      doubleprecision bqqbH2n1em2  
      doubleprecision bqqbH2n1em3  
      doubleprecision bqqbH2n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=bqqbH2n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=bqqbH2n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=bqqbH2n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=bqqbH2n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=bqqbH2n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=bqqbH2n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function bqqbH2n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH21J1
      doubleprecision bqqbH21J2
      t1 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t7 * x4
      t10 = -0.1D1 + x4
      t11 = t7 * t10
      t13 = t1 ** 2
      t15 = t3 ** 2
      t17 = x1 * t6
      t20 = t15 * t3
      t21 = t20 * t6
      t22 = t13 * t1
      t24 = 0.1D1 / (-0.2D1 + t1)
      t25 = t22 * t24
      t26 = bqqbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t27 = t25 * t26
      t28 = x2 * 0.3141592653589793D1
      t29 = sin(t28)
      t30 = t29 ** 2
      t32 = z ** 2
      t33 = 0.1D1 / t32
      t34 = x1 ** 2
      t36 = x3 * t30 * t33 * t34
      t37 = t6 ** 2
      t38 = t37 * x4
      t39 = t13 ** 2
      t44 = log(-0.4D1 * t36 * t38 * t10 * t39)
      t46 = bqqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t53 = lh * t20 * t6
      t54 = t25 * t46
      t58 = 0.1D1 / x3
      t64 = x4 * t10
      t68 = log(-0.4D1 * t39 * t30 * t33 * t34 * t37 * t64)
      t74 = lh ** 2
      t76 = 0.3141592653589793D1 ** 2
      t80 = t68 ** 2
      t87 = -(0.90D2 * t21 * (t27 - t44 * t22 * t24 * t46) - 0.180D3 * t
     #53 * t54) * t58 / 0.2880D4 - (-0.180D3 * lh - 0.90D2 * t68) * t20 
     #* t6 * t27 / 0.2880D4 - (0.180D3 * t74 - 0.30D2 * t76 + 0.180D3 * 
     #t68 * lh + 0.45D2 * t80) * t20 * t6 * t54 / 0.2880D4
      t88 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, -t2 * t8, t2 * t11, s * t1
     #3 * t15 * t17 * t10, t87)
      t90 = 0.1D1 - x3
      t91 = KAPPA2(x1, x2, t90, x4, z)
      t92 = s * t91
      t93 = -t90
      t100 = t91 ** 2
      t105 = cos(t28)
      t108 = sqrt(x3 * t93 * t64)
      t114 = t100 * t91
      t116 = 0.1D1 / (-0.2D1 + t91)
      t117 = t114 * t116
      t118 = bqqbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, t90, x4)
      t120 = t100 ** 2
      t126 = log(0.4D1 * t36 * t38 * t10 * t120 * t93)
      t128 = bqqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t90, x4)
      t137 = -0.90D2 * t21 * (t117 * t118 - t126 * t114 * t116 * t128) +
     # 0.180D3 * t53 * t117 * t128
      t140 = FJET(XB1, XB2, s, -t92 * t4 * t93, t92 * t4 * x3, -t92 * t8
     #, t92 * t11, s * t100 * t15 * t17 * (-0.1D1 + x3 + x4 - 0.2D1 * x3
     # * x4 + 0.2D1 * t105 * t108), -t137 * t58 / 0.2880D4)
      bqqbH2n1e1 = t88 * t87 - t140 * t137 * t58 / 0.2880D4

      end function



      doubleprecision function bqqbH2n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH21J1
      doubleprecision bqqbH21J2
      t1 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t8 = t7 * x4
      t10 = -0.1D1 + x4
      t11 = t7 * t10
      t13 = t1 ** 2
      t15 = t3 ** 2
      t17 = x1 * t6
      t20 = t15 * t3
      t21 = t20 * t6
      t22 = t13 * t1
      t25 = 0.1D1 / (-0.2D1 + t1)
      t26 = bqqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t28 = 0.1D1 / x3
      t32 = t22 * t25
      t33 = bqqbH21J2(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t38 = t13 ** 2
      t39 = x2 * 0.3141592653589793D1
      t40 = sin(t39)
      t41 = t40 ** 2
      t43 = z ** 2
      t46 = x1 ** 2
      t47 = t6 ** 2
      t49 = x4 * t10
      t53 = log(-0.4D1 * t38 * t41 / t43 * t46 * t47 * t49)
      t61 = -t21 * t22 * t25 * t26 * t28 / 0.32D2 - t21 * t32 * t33 / 0.
     #32D2 - (-0.180D3 * lh - 0.90D2 * t53) * t20 * t6 * t32 * t26 / 0.2
     #880D4
      t62 = FJET(XB1, XB2, s, t2 * t4, 0.0D0, -t2 * t8, t2 * t11, s * t1
     #3 * t15 * t17 * t10, t61)
      t64 = 0.1D1 - x3
      t65 = KAPPA2(x1, x2, t64, x4, z)
      t66 = s * t65
      t67 = -t64
      t74 = t65 ** 2
      t79 = cos(t39)
      t82 = sqrt(x3 * t67 * t49)
      t88 = t74 * t65
      t91 = 0.1D1 / (-0.2D1 + t65)
      t92 = bqqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, t64, x4)
      t97 = FJET(XB1, XB2, s, -t66 * t4 * t67, t66 * t4 * x3, -t66 * t8,
     # t66 * t11, s * t74 * t15 * t17 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 *
     # x4 + 0.2D1 * t79 * t82), t21 * t88 * t91 * t92 * t28 / 0.32D2)
      bqqbH2n1e0 = t62 * t61 + t97 * t20 * t6 * t88 * t91 * t92 * t28 / 
     #0.32D2

      end function



      doubleprecision function bqqbH2n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH21J1
      doubleprecision bqqbH21J2
      t1 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t6 = -0.1D1 + x1
      t7 = t3 * t6
      t10 = -0.1D1 + x4
      t13 = t1 ** 2
      t15 = t3 ** 2
      t20 = t15 * t3
      t26 = bqqbH21J1(s, XB1, XB2, z, lh, wd, x1, x2, 0.10D1, x4)
      t27 = t13 * t1 / (-0.2D1 + t1) * t26
      t30 = FJET(XB1, XB2, s, t2 * t3 * x1, 0.0D0, -t2 * t7 * x4, t2 * t
     #7 * t10, s * t13 * t15 * x1 * t6 * t10, -t20 * t6 * t27 / 0.32D2)
      bqqbH2n1em1 = -t30 * t20 * t6 * t27 / 0.32D2

      end function



      doubleprecision function bqqbH2n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH21J1
      doubleprecision bqqbH21J2
      bqqbH2n1em2 = 0.0D0

      end function



      doubleprecision function bqqbH2n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH21J1
      doubleprecision bqqbH21J2
      bqqbH2n1em3 = 0.0D0

      end function



      doubleprecision function bqqbH2n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision bqqbH21J1
      doubleprecision bqqbH21J2
      bqqbH2n1em4 = 0.0D0

      end function
  
 

      doubleprecision function bqqbH21J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t5 = t4 * x3
      t7 = 0.1D1 - x3
      t10 = s - t2 * t5 - t2 * t4 * t7
      t11 = t10 ** 2
      t13 = t10 * s
      t14 = t1 ** 2
      t16 = t3 ** 2
      t18 = 0.1D1 - x1
      t22 = cos(x2 * 0.3141592653589793D1)
      t24 = 0.1D1 - x4
      t27 = sqrt(x3 * t7 * x4 * t24)
      t34 = t13 * t1
      t42 = s ** 2
      bqqbH21J1 = -0.16D2 * wd * (-0.2D1 * t11 + t13 * t14 * t16 * x1 * 
     #t18 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t22 * t27) - 0.2D1 * t3
     #4 * t5 + 0.2D1 * t13 * z + t34 * t3 * t18 * t24 - 0.2D1 * t42 * z 
     #* t1 * t5) / t11

      end function
  
   
 

      doubleprecision function bqqbH21J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = t10 ** 2
      t13 = t10 * s
      t15 = 0.1D1 - x1
      t17 = 0.1D1 - x4
      t20 = t1 ** 2
      t22 = t3 ** 2
      t27 = cos(x2 * 0.3141592653589793D1)
      t31 = sqrt(x3 * t7 * x4 * t17)
      bqqbH21J2 = -0.16D2 * wd * (0.2D1 * t11 - t13 * t1 * t3 * t15 * t1
     #7 + t13 * t20 * t22 * x1 * t15 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D
     #1 * t27 * t31)) / t11

      end function
  
 