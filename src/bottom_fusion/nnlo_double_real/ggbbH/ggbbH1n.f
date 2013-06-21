  
      subroutine ggbbH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision ggbbH11J1  
      doubleprecision ggbbH11J2  
      doubleprecision ggbbH11J3  
      doubleprecision ggbbH1n1e1  
      doubleprecision ggbbH1n1e0  
      doubleprecision ggbbH1n1em1  
      doubleprecision ggbbH1n1em2  
      doubleprecision ggbbH1n1em3  
      doubleprecision ggbbH1n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=ggbbH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=ggbbH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=ggbbH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=ggbbH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=ggbbH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=ggbbH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function ggbbH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH11J1
      doubleprecision ggbbH11J2
      doubleprecision ggbbH11J3
      t1 = KAPPA2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t7 = -0.1D1 + x3
      t10 = -0.1D1 + x1
      t11 = t3 * t10
      t14 = -0.1D1 + x4
      t17 = t1 ** 2
      t19 = t3 ** 2
      t21 = x1 * t10
      t24 = x2 * 0.3141592653589793D1
      t25 = cos(t24)
      t27 = x4 * t14
      t29 = sqrt(x3 * t7 * t27)
      t35 = t17 ** 2
      t37 = 0.1D1 / (-0.2D1 + t1)
      t38 = t35 * t37
      t39 = t19 ** 2
      t41 = ggbbH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t48 = x1 ** 2
      t50 = sin(t24)
      t51 = t50 ** 2
      t52 = z ** 2
      t56 = t10 ** 2
      t62 = log(0.4D1 * x3 * t48 * t51 / t52 * t27 * t56 * t7 * t35)
      t70 = ggbbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t74 = t38 * t39 * t21 * s * t41 / 0.32D2 + (-0.180D3 * t38 * lh - 
     #0.90D2 * t62 * t35 * t37) * t39 * x1 * t10 * s * t70 / 0.2880D4
      t75 = FJET(XB1, XB2, s, t2 * t4 * x3, -t2 * t4 * t7, -t2 * t11 * x
     #4, t2 * t11 * t14, s * t17 * t19 * t21 * (-x3 - x4 + 0.2D1 * x3 * 
     #x4 + 0.2D1 * t25 * t29), t74)
      ggbbH1n1e1 = t75 * t74

      end function



      doubleprecision function ggbbH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH11J1
      doubleprecision ggbbH11J2
      doubleprecision ggbbH11J3
      t1 = KAPPA2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = -0.1D1 + z
      t4 = t3 * x1
      t7 = -0.1D1 + x3
      t10 = -0.1D1 + x1
      t11 = t3 * t10
      t14 = -0.1D1 + x4
      t17 = t1 ** 2
      t19 = t3 ** 2
      t21 = x1 * t10
      t25 = cos(x2 * 0.3141592653589793D1)
      t29 = sqrt(x3 * t7 * x4 * t14)
      t35 = t17 ** 2
      t37 = 0.1D1 / (-0.2D1 + t1)
      t39 = t19 ** 2
      t41 = ggbbH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t43 = t21 * s * t41
      t46 = FJET(XB1, XB2, s, t2 * t4 * x3, -t2 * t4 * t7, -t2 * t11 * x
     #4, t2 * t11 * t14, s * t17 * t19 * t21 * (-x3 - x4 + 0.2D1 * x3 * 
     #x4 + 0.2D1 * t25 * t29), t35 * t37 * t39 * t43 / 0.32D2)
      ggbbH1n1e0 = t46 * t35 * t37 * t39 * t43 / 0.32D2

      end function



      doubleprecision function ggbbH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH11J1
      doubleprecision ggbbH11J2
      doubleprecision ggbbH11J3
      ggbbH1n1em1 = 0.0D0

      end function



      doubleprecision function ggbbH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH11J1
      doubleprecision ggbbH11J2
      doubleprecision ggbbH11J3
      ggbbH1n1em2 = 0.0D0

      end function



      doubleprecision function ggbbH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH11J1
      doubleprecision ggbbH11J2
      doubleprecision ggbbH11J3
      ggbbH1n1em3 = 0.0D0

      end function



      doubleprecision function ggbbH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision ggbbH11J1
      doubleprecision ggbbH11J2
      doubleprecision ggbbH11J3
      ggbbH1n1em4 = 0.0D0

      end function
  
 

      doubleprecision function ggbbH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 * s
      t14 = t11 * t13
      t15 = t3 * x1
      t18 = 0.1D1 - x3
      t21 = s - t2 * t15 * x3 - t2 * t15 * t18
      t22 = t14 * t21
      t23 = t1 * t3
      t24 = t4 * x4
      t25 = t23 * t24
      t28 = t11 ** 2
      t29 = t21 * t28
      t30 = t29 * t12
      t33 = t21 ** 2
      t34 = t33 * t11
      t35 = t34 * t12
      t38 = t1 ** 2
      t39 = t13 * t38
      t40 = t3 ** 2
      t44 = z * t21
      t50 = x3 * x1
      t51 = t23 * t50
      t58 = t38 * t40
      t59 = x1 ** 2
      t60 = x3 ** 2
      t61 = t59 * t60
      t66 = t13 * t1 * t3
      t67 = z * t33
      t75 = z * t28
      t79 = t39 * t40
      t83 = 0.72D2 * t22 * t25 - 0.18D2 * t30 * t25 - 0.36D2 * t35 * t25
     # - 0.72D2 * t39 * t40 * x1 * x3 * t24 * t44 * t11 + 0.45D2 * t30 +
     # 0.72D2 * t22 * t51 + 0.18D2 * t35 * t23 * x1 * t18 - 0.36D2 * t22
     # * t58 * t61 - 0.36D2 * t66 * t24 * t67 - 0.36D2 * t30 * t51 - 0.1
     #8D2 * t35 * t51 - 0.36D2 * t66 * t50 * t75 + 0.36D2 * t79 * t61 * 
     #t75
      t84 = t4 ** 2
      t85 = x4 ** 2
      t86 = t84 * t85
      t97 = t14 * t44
      t108 = t13 * z
      t116 = t12 * z
      t121 = z ** 2
      t127 = 0.36D2 * t79 * t86 * t67 + 0.18D2 * t30 * t23 * t4 * t8 - 0
     #.36D2 * t22 * t58 * t86 + 0.36D2 * t97 * t51 - 0.72D2 * t14 * t21 
     #* t38 * t40 * t4 * x4 * x1 * x3 + 0.26D2 * t108 * t28 + 0.26D2 * t
     #108 * t33 - 0.112D3 * t97 - 0.58D2 * t22 + 0.45D2 * t35 + 0.72D2 *
     # t29 * t116 + 0.72D2 * t34 * t116 - 0.144D3 * t14 * t21 * t121 + 0
     #.36D2 * t97 * t25
      ggbbH11J1 = 0.16D2 / 0.3D1 * wd * (t83 + t127) / t33 / t28 / t12

      end function
  
   
 

      doubleprecision function ggbbH11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = s ** 2
      t2 = s * t1
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t5 * t7
      t9 = 0.1D1 - x1
      t10 = t9 ** 2
      t11 = x4 ** 2
      t12 = t10 * t11
      t13 = s * t3
      t14 = t6 * x1
      t17 = 0.1D1 - x3
      t20 = s - t13 * t14 * x3 - t13 * t14 * t17
      t21 = t20 ** 2
      t22 = z * t21
      t26 = t6 * t9
      t29 = 0.1D1 - x4
      t32 = s - t13 * t26 * x4 - t13 * t26 * t29
      t33 = t32 * t2
      t34 = t20 * z
      t35 = t33 * t34
      t36 = t3 * t6
      t37 = t9 * x4
      t38 = t36 * t37
      t42 = t2 * t3 * t6
      t46 = x3 * x1
      t47 = t36 * t46
      t65 = t32 ** 2
      t66 = z * t65
      t71 = t20 * t65 * t1
      t73 = t2 * z
      t76 = t33 * t20
      t77 = t4 * t7
      t78 = x1 ** 2
      t79 = x3 ** 2
      t80 = t78 * t79
      t85 = t21 * t32 * t1
      t92 = -0.36D2 * t8 * t12 * t22 - 0.36D2 * t35 * t38 + 0.36D2 * t42
     # * t37 * t22 - 0.36D2 * t35 * t47 + 0.72D2 * t5 * t7 * x1 * x3 * t
     #37 * t34 * t32 + 0.72D2 * t33 * t20 * t4 * t7 * t9 * x4 * x1 * x3 
     #+ 0.36D2 * t42 * t46 * t66 - 0.52D2 * t71 - 0.36D2 * t73 * t65 + 0
     #.36D2 * t76 * t77 * t80 + 0.18D2 * t85 * t47 - 0.18D2 * t85 * t36 
     #* x1 * t17
      t118 = 0.36D2 * t85 * t38 + 0.36D2 * t71 * t47 - 0.18D2 * t71 * t3
     #6 * t9 * t29 - 0.72D2 * t76 * t47 + 0.18D2 * t71 * t38 - 0.72D2 * 
     #t76 * t38 + 0.40D2 * t35 - 0.36D2 * t8 * t80 * t66 + 0.36D2 * t76 
     #* t77 * t12 - 0.36D2 * t73 * t21 - 0.52D2 * t85 + 0.58D2 * t76
      ggbbH11J2 = 0.16D2 / 0.3D1 * wd * (t92 + t118) / t21 / t65 / t1

      end function
  
   
 

      doubleprecision function ggbbH11J3
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
      t10 = s - t2 * t4 * x3 - t2 * t4 * (0.1D1 - x3)
      t11 = t10 ** 2
      t13 = t3 * (0.1D1 - x1)
      t19 = s - t2 * t13 * x4 - t2 * t13 * (0.1D1 - x4)
      t21 = s ** 2
      t24 = t21 * s
      t25 = t24 * z
      t28 = t19 * t24
      t31 = t19 ** 2
      ggbbH11J3 = 0.16D2 / 0.3D1 * wd * (-0.18D2 * t11 * t19 * t21 + 0.1
     #0D2 * t25 * t11 + 0.36D2 * t28 * t10 + 0.10D2 * t25 * t31 - 0.18D2
     # * t10 * t31 * t21 + 0.36D2 * t28 * t10 * z) / t11 / t31 / t21

      end function
  
 