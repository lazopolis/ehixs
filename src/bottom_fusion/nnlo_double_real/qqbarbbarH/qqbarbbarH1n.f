  
      subroutine qqbarbbarH1n
     &(sector,pole,s, XB1, XB2, z, lh, wd, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision Log  
      doubleprecision qqbarbbarH11J1  
      doubleprecision qqbarbbarH11J2  
      doubleprecision qqbarbbarH1n1e1  
      doubleprecision qqbarbbarH1n1e0  
      doubleprecision qqbarbbarH1n1em1  
      doubleprecision qqbarbbarH1n1em2  
      doubleprecision qqbarbbarH1n1em3  
      doubleprecision qqbarbbarH1n1em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=qqbarbbarH1n1e1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=qqbarbbarH1n1e0(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=qqbarbbarH1n1em1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=qqbarbbarH1n1em2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=qqbarbbarH1n1em3(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=qqbarbbarH1n1em4(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function qqbarbbarH1n1e1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qqbarbbarH11J1
      doubleprecision qqbarbbarH11J2
      t1 = KAPPA2(x1, x2, x3, x4, z)
      t2 = t1 * s
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
      t41 = qqbarbbarH11J2(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t48 = x1 ** 2
      t50 = sin(t24)
      t51 = t50 ** 2
      t52 = z ** 2
      t56 = t10 ** 2
      t62 = log(0.4D1 * x3 * t48 * t51 / t52 * t27 * t56 * t7 * t35)
      t70 = qqbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t74 = t38 * t39 * t21 * s * t41 / 0.32D2 + (-0.180D3 * t38 * lh - 
     #0.90D2 * t62 * t35 * t37) * t39 * x1 * t10 * s * t70 / 0.2880D4
      t75 = FJET(XB1, XB2, s, t2 * t4 * x3, -t2 * t4 * t7, -t2 * t11 * x
     #4, t2 * t11 * t14, s * t17 * t19 * t21 * (-x3 - x4 + 0.2D1 * x3 * 
     #x4 + 0.2D1 * t25 * t29), t74)
      qqbarbbarH1n1e1 = t75 * t74

      end function



      doubleprecision function qqbarbbarH1n1e0
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qqbarbbarH11J1
      doubleprecision qqbarbbarH11J2
      t1 = KAPPA2(x1, x2, x3, x4, z)
      t2 = t1 * s
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
      t41 = qqbarbbarH11J1(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4)
      t43 = t21 * s * t41
      t46 = FJET(XB1, XB2, s, t2 * t4 * x3, -t2 * t4 * t7, -t2 * t11 * x
     #4, t2 * t11 * t14, s * t17 * t19 * t21 * (-x3 - x4 + 0.2D1 * x3 * 
     #x4 + 0.2D1 * t25 * t29), t35 * t37 * t39 * t43 / 0.32D2)
      qqbarbbarH1n1e0 = t46 * t35 * t37 * t39 * t43 / 0.32D2

      end function



      doubleprecision function qqbarbbarH1n1em1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qqbarbbarH11J1
      doubleprecision qqbarbbarH11J2
      qqbarbbarH1n1em1 = 0.0D0

      end function



      doubleprecision function qqbarbbarH1n1em2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qqbarbbarH11J1
      doubleprecision qqbarbbarH11J2
      qqbarbbarH1n1em2 = 0.0D0

      end function



      doubleprecision function qqbarbbarH1n1em3
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qqbarbbarH11J1
      doubleprecision qqbarbbarH11J2
      qqbarbbarH1n1em3 = 0.0D0

      end function



      doubleprecision function qqbarbbarH1n1em4
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      doubleprecision qqbarbbarH11J1
      doubleprecision qqbarbbarH11J2
      qqbarbbarH1n1em4 = 0.0D0

      end function
  
 

      doubleprecision function qqbarbbarH11J1
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t11 * s
      t13 = t10 * t12
      t14 = 0.1D1 - x1
      t15 = t3 * t14
      t18 = 0.1D1 - x4
      t21 = s - t2 * t15 * x4 - t2 * t15 * t18
      t23 = t13 * t21 * z
      t24 = t1 * t3
      t26 = t24 * t14 * t18
      t29 = t21 ** 2
      t30 = t29 * t11
      t31 = t30 * t10
      t33 = t12 * z
      t34 = t33 * t1
      t39 = t10 ** 2
      t44 = t1 ** 2
      t45 = t33 * t44
      t46 = t3 ** 2
      t47 = t14 ** 2
      t49 = t18 ** 2
      t54 = x1 ** 2
      t56 = t7 ** 2
      t61 = t13 * t21
      t63 = t24 * x1 * x3
      t66 = t21 * t11
      t67 = t66 * t39
      t69 = t44 * t46
      t75 = t24 * x1 * t7
      t88 = 0.2D1 * t23 * t26 - t31 * t26 - 0.2D1 * t34 * t4 * t7 * t29 
     #- 0.2D1 * t34 * t15 * t18 * t39 + 0.2D1 * t45 * t46 * t47 * t49 * 
     #t39 + 0.2D1 * t45 * t46 * t54 * t56 * t29 - 0.2D1 * t61 * t63 + t6
     #7 * t63 - 0.2D1 * t61 * t69 * t54 * t56 + 0.2D1 * t61 * t75 - t67 
     #* t75 - 0.2D1 * t31 * t75 + t31 * t24 * t14 * x4 - 0.2D1 * t61 * t
     #69 * t47 * t49
      t106 = z ** 2
      t127 = 0.4D1 * t61 * t26 - 0.2D1 * t67 * t26 - 0.4D1 * t13 * t21 *
     # t44 * t46 * t14 * t18 * x1 * t7 + t33 * t29 + t31 - t67 - 0.2D1 *
     # t39 * t29 * s + t33 * t39 - 0.2D1 * t13 * t21 * t106 - 0.2D1 * t2
     #3 + 0.2D1 * t66 * t39 * z + 0.2D1 * t30 * t10 * z - 0.4D1 * t33 * 
     #t69 * x1 * t7 * t14 * t18 * t21 * t10 + 0.2D1 * t23 * t75
      qqbarbbarH11J1 = -0.16D2 * wd * (t88 + t127) / t29 / t11 / t39

      end function
  
   
 

      doubleprecision function qqbarbbarH11J2
     &(s, XB1, XB2, z, lh, wd, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = t1 * s
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t7 = 0.1D1 - x3
      t10 = s - t2 * t4 * x3 - t2 * t4 * t7
      t11 = s ** 2
      t12 = t11 * s
      t14 = 0.1D1 - x1
      t15 = t3 * t14
      t18 = 0.1D1 - x4
      t21 = s - t2 * t15 * x4 - t2 * t15 * t18
      t25 = t21 ** 2
      t27 = t25 * t11 * t10
      t28 = t1 * t3
      t32 = t12 * z
      t33 = t10 ** 2
      t36 = t21 * t11 * t33
      qqbarbbarH11J2 = -0.16D2 * wd * (-0.2D1 * t10 * t12 * t21 * z - t2
     #7 * t28 * t14 * t18 - t32 * t33 - t36 * t28 * x1 * t7 - t27 * t28 
     #* t14 * x4 - t32 * t25 - t36 * t28 * x1 * x3 + t27 + t36 + 0.2D1 *
     # t33 * t25 * s) / t25 / t11 / t33

      end function
  
 