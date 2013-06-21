  
      subroutine rrgg2gght2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2ggh21J1  
      doubleprecision rrgg2ggh21J2  
      doubleprecision rrgg2ggh21J3  
      doubleprecision rrgg2ggh21J4  
      doubleprecision rrgg2ggh21J5  
      doubleprecision rrgg2ggh21J6  
      doubleprecision rrgg2ggh21J7  
      doubleprecision rrgg2gght2s1e1  
      doubleprecision rrgg2gght2s1e0  
      doubleprecision rrgg2gght2s1em1  
      doubleprecision rrgg2gght2s1em2  
      doubleprecision rrgg2gght2s1em3  
      doubleprecision rrgg2gght2s1em4  
      doubleprecision rrgg2gght2s2e1  
      doubleprecision rrgg2gght2s2e0  
      doubleprecision rrgg2gght2s2em1  
      doubleprecision rrgg2gght2s2em2  
      doubleprecision rrgg2gght2s2em3  
      doubleprecision rrgg2gght2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2gght2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2gght2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2gght2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = 0.1D1 - x3
      t10 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x4)
      t26 = t23 ** 2
      t27 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x4)
      t30 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t31 = t16 * x4
      t32 = t31 * t4
      t35 = log(-0.4D1 * t14 * t32)
      t36 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t38 = t35 ** 2
      t39 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t46 = 0.3141592653589793D1 * lh
      t47 = t6 * t8
      t54 = lh ** 2
      t56 = 0.3141592653589793D1 ** 2
      t58 = 0.180D3 * t54 - 0.30D2 * t56
      t59 = 0.3141592653589793D1 * t58
      t61 = t47 * (t27 - t39)
      t64 = 0.1D1 / x3
      t67 = t13 * t16
      t70 = log(-0.4D1 * t67 * t18)
      t71 = t70 * 0.3141592653589793D1
      t74 = t70 ** 2
      t75 = t74 * 0.3141592653589793D1
      t82 = rrgg2ggh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t110 = x1 ** 2
      t111 = x3 * t110
      t112 = t111 * t13
      t117 = log(0.4D1 * t112 * t31 * t4 * t19)
      t121 = log(-0.4D1 * t112 * t32)
      t131 = 0.1D1 / x1
      t134 = t110 * t13
      t137 = log(-0.4D1 * t134 * t32)
      t139 = t137 ** 2
      t156 = -(0.90D2 * t7 * t8 * (t10 - t23 * t24 + t26 * t27 / 0.2D1 -
     # t30 + t35 * t36 - t38 * t39 / 0.2D1) - 0.180D3 * t46 * t47 * (t24
     # - t23 * t27 - t36 + t35 * t39) + t59 * t61) * t64 / 0.1440D4 + (t
     #59 + 0.180D3 * t71 * lh + 0.45D2 * t75) * t6 * t8 * t36 / 0.1440D4
     # + t7 * t8 * t82 / 0.16D2 + (0.3141592653589793D1 * (0.60D2 * lh *
     # t56 - 0.2884936567583026D3 - 0.120D3 * t54 * lh) - t71 * t58 - 0.
     #90D2 * t75 * lh - 0.15D2 * t74 * t70 * 0.3141592653589793D1) * t6 
     #* t8 * t39 / 0.1440D4 + (-0.180D3 * t46 - 0.90D2 * t71) * t6 * t8 
     #* t30 / 0.1440D4 - (0.90D2 * t7 * t8 * (t24 - t117 * t27 - t36 + t
     #121 * t39) - 0.180D3 * t46 * t61) * t64 * t131 / 0.720D3 + (0.90D2
     # * t7 * t8 * (t30 - t137 * t36 + t139 * t39 / 0.2D1) - 0.180D3 * t
     #46 * t47 * (t36 - t137 * t39) + t59 * t47 * t39) * t131 / 0.720D3
      t157 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t156)
      t159 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t156)
      t161 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t162 = s * t161
      t163 = x1 * t1
      t164 = t162 * t163
      t165 = -0.1D1 + x1
      t166 = t1 * t165
      t167 = t166 * x4
      t168 = t162 * t167
      t169 = t166 * t4
      t170 = t162 * t169
      t171 = t161 ** 2
      t174 = x1 * t165
      t176 = s * t171 * t6 * t174 * t4
      t177 = t165 * t171
      t179 = 0.1D1 / (-0.2D1 + t161)
      t180 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t182 = t177 * t179 * t180
      t183 = t111 * t67
      t184 = t165 ** 2
      t185 = t171 ** 2
      t187 = t18 * t184 * t185
      t190 = log(-0.4D1 * t183 * t187)
      t192 = t171 * t179
      t193 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t194 = t192 * t193
      t200 = t46 * t47
      t202 = t177 * t179 * t193
      t208 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t214 = log(-0.4D1 * t134 * t16 * t187)
      t215 = t214 * t165
      t218 = t214 ** 2
      t236 = -(0.90D2 * t7 * t8 * (t182 - t190 * t165 * t194) - 0.180D3 
     #* t200 * t202) * t64 * t131 / 0.720D3 + (-0.90D2 * t7 * t8 * (t177
     # * t179 * t208 - t215 * t192 * t180 + t218 * t165 * t194 / 0.2D1) 
     #+ 0.180D3 * t46 * t47 * (t182 - t215 * t194) - t59 * t47 * t202) *
     # t131 / 0.720D3
      t237 = FJET(XB1, XB2, s, t164, 0.0D0, -t168, t170, t176, t236)
      t239 = FJET(XB1, XB2, s, -t168, t170, t164, 0.0D0, t176, t236)
      t241 = KAPPA2(x1, x2, t9, x4, z)
      t242 = s * t241
      t244 = t242 * t163 * t19
      t246 = t242 * t163 * x3
      t247 = t242 * t167
      t248 = t242 * t169
      t249 = t241 ** 2
      t254 = cos(t11)
      t257 = Sqrt(x3 * t19 * t18)
      t262 = s * t249 * t6 * t174 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t254 * t257)
      t263 = t165 * t249
      t265 = 0.1D1 / (-0.2D1 + t241)
      t266 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t9, x4)
      t270 = t249 ** 2
      t275 = log(0.4D1 * t183 * t18 * t184 * t19 * t270)
      t278 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t9, x4)
      t289 = -0.90D2 * t7 * t8 * (t263 * t265 * t266 - t275 * t165 * t24
     #9 * t265 * t278) + 0.180D3 * t200 * t263 * t265 * t278
      t292 = t289 * t64 * t131 / 0.720D3
      t293 = FJET(XB1, XB2, s, -t244, t246, -t247, t248, t262, -t292)
      t295 = t64 * t131
      t298 = FJET(XB1, XB2, s, -t247, t248, -t244, t246, t262, -t292)
      rrgg2gght2s1e1 = t157 * t156 + t159 * t156 + t237 * t236 + t239 * 
     #t236 - t293 * t289 * t295 / 0.720D3 - t298 * t289 * t295 / 0.720D3

      end function



      doubleprecision function rrgg2gght2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = 0.1D1 - x3
      t10 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x4)
      t26 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t28 = t16 * x4 * t4
      t31 = log(-0.4D1 * t14 * t28)
      t32 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t6 * t8
      t40 = t24 - t32
      t45 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t54 = x1 ** 2
      t55 = t54 * t13
      t58 = log(-0.4D1 * t55 * t28)
      t70 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t78 = log(-0.4D1 * t13 * t16 * t18)
      t79 = t78 * 0.3141592653589793D1
      t86 = lh ** 2
      t88 = 0.3141592653589793D1 ** 2
      t94 = t78 ** 2
      t102 = -(0.90D2 * t7 * t8 * (t10 - t23 * t24 - t26 + t31 * t32) - 
     #0.180D3 * t38 * t39 * t40) * t45 / 0.1440D4 - t7 * t8 * t40 * t45 
     #* t50 / 0.8D1 + (0.90D2 * t7 * t8 * (t26 - t58 * t32) - 0.180D3 * 
     #t38 * t39 * t32) * t50 / 0.720D3 + t7 * t8 * t70 / 0.16D2 + (-0.18
     #0D3 * t38 - 0.90D2 * t79) * t6 * t8 * t26 / 0.1440D4 + (0.31415926
     #53589793D1 * (0.180D3 * t86 - 0.30D2 * t88) + 0.180D3 * t79 * lh +
     # 0.45D2 * t94 * 0.3141592653589793D1) * t6 * t8 * t32 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t102)
      t105 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t107 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t108 = s * t107
      t109 = x1 * t1
      t110 = t108 * t109
      t111 = -0.1D1 + x1
      t112 = t1 * t111
      t113 = t112 * x4
      t114 = t108 * t113
      t115 = t112 * t4
      t116 = t108 * t115
      t117 = t107 ** 2
      t120 = x1 * t111
      t122 = s * t117 * t6 * t120 * t4
      t124 = t7 * t8 * t111
      t126 = 0.1D1 / (-0.2D1 + t107)
      t127 = t117 * t126
      t128 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t134 = t111 * t117
      t135 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4
     #)
      t139 = t117 ** 2
      t140 = t111 ** 2
      t145 = log(-0.4D1 * t55 * t16 * t18 * t139 * t140)
      t161 = -t124 * t127 * t128 * t45 * t50 / 0.8D1 + (-0.90D2 * t7 * t
     #8 * (t134 * t126 * t135 - t145 * t111 * t127 * t128) + 0.180D3 * t
     #38 * t39 * t134 * t126 * t128) * t50 / 0.720D3
      t162 = FJET(XB1, XB2, s, t110, 0.0D0, -t114, t116, t122, t161)
      t164 = FJET(XB1, XB2, s, -t114, t116, t110, 0.0D0, t122, t161)
      t166 = KAPPA2(x1, x2, t9, x4, z)
      t167 = s * t166
      t169 = t167 * t109 * t19
      t171 = t167 * t109 * x3
      t172 = t167 * t113
      t173 = t167 * t115
      t174 = t166 ** 2
      t179 = cos(t11)
      t182 = Sqrt(x3 * t19 * t18)
      t187 = s * t174 * t6 * t120 * (-0.1D1 + x3 + x4 - 0.2D1 * x3 * x4 
     #+ 0.2D1 * t179 * t182)
      t191 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t9, x4)
      t194 = t174 / (-0.2D1 + t166) * t191 * t45 * t50
      t196 = t124 * t194 / 0.8D1
      t197 = FJET(XB1, XB2, s, -t169, t171, -t172, t173, t187, t196)
      t199 = t39 * t111
      t203 = FJET(XB1, XB2, s, -t172, t173, -t169, t171, t187, t196)
      rrgg2gght2s1e0 = t103 * t102 + t105 * t102 + t162 * t161 + t164 * 
     #t161 + t197 * 0.3141592653589793D1 * t199 * t194 / 0.8D1 + t203 * 
     #0.3141592653589793D1 * t199 * t194 / 0.8D1

      end function



      doubleprecision function rrgg2gght2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, x
     #4)
      t10 = t8 * t9
      t11 = 0.1D1 / x1
      t15 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, 
     #x4)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t30 = log(-0.4D1 * t23 / t24 * x4 * t4)
      t38 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1 - 
     #x3, x4)
      t45 = t7 * t10 * t11 / 0.8D1 + t7 * t8 * t15 / 0.16D2 + (-0.180D3 
     #* 0.3141592653589793D1 * lh - 0.90D2 * t30 * 0.3141592653589793D1)
     # * t6 * t10 / 0.1440D4 - t7 * t8 * (t38 - t9) / x3 / 0.16D2
      t46 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t45)
      t48 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t45)
      t50 = KAPPA2(x1, x2, 0.10D1, x4, z)
      t51 = s * t50
      t53 = t51 * t1 * x1
      t54 = -0.1D1 + x1
      t55 = t1 * t54
      t57 = t51 * t55 * x4
      t59 = t51 * t55 * t4
      t60 = t50 ** 2
      t65 = s * t60 * t6 * x1 * t54 * t4
      t69 = 0.1D1 / (-0.2D1 + t50)
      t71 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, x4)
      t75 = t7 * t8 * t54 * t60 * t69 * t71 * t11 / 0.8D1
      t76 = FJET(XB1, XB2, s, t53, 0.0D0, -t57, t59, t65, -t75)
      t78 = t6 * t8
      t83 = t54 * t60 * t69 * t71 * t11
      t86 = FJET(XB1, XB2, s, -t57, t59, t53, 0.0D0, t65, -t75)
      rrgg2gght2s1em1 = t46 * t45 + t48 * t45 - t76 * 0.3141592653589793
     #D1 * t78 * t83 / 0.8D1 - t86 * 0.3141592653589793D1 * t78 * t83 / 
     #0.8D1

      end function



      doubleprecision function rrgg2gght2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1, x
     #4)
      t12 = 0.3141592653589793D1 * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2gght2s1em2 = t13 * 0.3141592653589793D1 * t16 / 0.16D2 + t18 
     #* 0.3141592653589793D1 * t16 / 0.16D2

      end function



      doubleprecision function rrgg2gght2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      rrgg2gght2s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      rrgg2gght2s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2gght2s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t25 = t22 ** 2
      t26 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t29 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t30 = t15 * x4
      t31 = t30 * t4
      t34 = log(-0.4D1 * t13 * t31)
      t35 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t37 = t34 ** 2
      t38 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t45 = 0.3141592653589793D1 * lh
      t46 = t6 * t8
      t53 = lh ** 2
      t55 = 0.3141592653589793D1 ** 2
      t57 = 0.180D3 * t53 - 0.30D2 * t55
      t58 = 0.3141592653589793D1 * t57
      t59 = -t26 + t38
      t63 = 0.1D1 / x3
      t66 = t12 * t15
      t69 = log(-0.4D1 * t66 * t17)
      t70 = t69 * 0.3141592653589793D1
      t73 = t69 ** 2
      t74 = t73 * 0.3141592653589793D1
      t81 = rrgg2ggh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t109 = x1 ** 2
      t110 = x3 * t109
      t111 = t110 * t12
      t114 = log(-0.4D1 * t111 * t31)
      t120 = log(0.4D1 * t111 * t30 * t4 * t18)
      t132 = 0.1D1 / x1
      t135 = t109 * t12
      t138 = log(-0.4D1 * t135 * t31)
      t140 = t138 ** 2
      t157 = (0.90D2 * t7 * t8 * (-t9 + t22 * t23 - t25 * t26 / 0.2D1 + 
     #t29 - t34 * t35 + t37 * t38 / 0.2D1) - 0.180D3 * t45 * t46 * (-t23
     # + t22 * t26 + t35 - t34 * t38) + t58 * t46 * t59) * t63 / 0.1440D
     #4 + (t58 + 0.180D3 * t70 * lh + 0.45D2 * t74) * t6 * t8 * t35 / 0.
     #1440D4 + t7 * t8 * t81 / 0.16D2 + (0.3141592653589793D1 * (0.60D2 
     #* lh * t55 - 0.2884936567583026D3 - 0.120D3 * t53 * lh) - t70 * t5
     #7 - 0.90D2 * t74 * lh - 0.15D2 * t73 * t69 * 0.3141592653589793D1)
     # * t6 * t8 * t38 / 0.1440D4 + (-0.180D3 * t45 - 0.90D2 * t70) * t6
     # * t8 * t29 / 0.1440D4 - (0.90D2 * t7 * t8 * (-t35 + t114 * t38 + 
     #t23 - t120 * t26) + 0.180D3 * t45 * t46 * t59) * t63 * t132 / 0.72
     #0D3 + (0.90D2 * t7 * t8 * (t29 - t138 * t35 + t140 * t38 / 0.2D1) 
     #- 0.180D3 * t45 * t46 * (t35 - t138 * t38) + t58 * t46 * t38) * t1
     #32 / 0.720D3
      t158 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t157)
      t160 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t161 = s * t160
      t162 = x1 * t1
      t163 = t162 * t161
      t164 = -0.1D1 + x1
      t165 = t1 * t164
      t166 = t165 * x4
      t167 = t161 * t166
      t168 = t165 * t4
      t169 = t161 * t168
      t170 = t160 ** 2
      t173 = x1 * t164
      t175 = s * t170 * t6 * t173 * x4
      t176 = t164 * t170
      t178 = 0.1D1 / (-0.2D1 + t160)
      t179 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t181 = t176 * t178 * t179
      t182 = t110 * t66
      t183 = t164 ** 2
      t184 = t170 ** 2
      t186 = t17 * t183 * t184
      t189 = log(-0.4D1 * t182 * t186)
      t191 = t170 * t178
      t192 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t193 = t191 * t192
      t199 = t46 * t45
      t201 = t176 * t178 * t192
      t206 = (0.90D2 * t7 * t8 * (t181 - t189 * t164 * t193) - 0.180D3 *
     # t199 * t201) * t63 * t132
      t207 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t213 = log(-0.4D1 * t135 * t15 * t186)
      t214 = t213 * t164
      t217 = t213 ** 2
      t221 = -t176 * t178 * t207 + t214 * t191 * t179 - t217 * t164 * t1
     #93 / 0.2D1
      t226 = -t181 + t214 * t193
      t231 = t58 * t46 * t201
      t235 = -t206 / 0.720D3 + (0.90D2 * t7 * t8 * t221 - 0.180D3 * t45 
     #* t46 * t226 - t231) * t132 / 0.720D3
      t236 = FJET(XB1, XB2, s, 0.0D0, t163, -t167, t169, -t175, t235)
      t238 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t157)
      t240 = KAPPA2(x1, x2, x3, x4, z)
      t241 = s * t240
      t243 = t241 * t162 * x3
      t245 = t241 * t162 * t18
      t246 = t241 * t166
      t247 = t241 * t168
      t248 = t240 ** 2
      t253 = cos(t10)
      t256 = Sqrt(x3 * t18 * t17)
      t261 = s * t248 * t6 * t173 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t253 * t256)
      t262 = t164 * t248
      t264 = 0.1D1 / (-0.2D1 + t240)
      t265 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t269 = t248 ** 2
      t274 = log(0.4D1 * t182 * t17 * t183 * t18 * t269)
      t277 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t280 = -t262 * t264 * t265 + t274 * t164 * t248 * t264 * t277
      t287 = 0.180D3 * t199 * t262 * t264 * t277
      t288 = 0.90D2 * t7 * t8 * t280 + t287
      t292 = FJET(XB1, XB2, s, t243, -t245, -t246, t247, t261, -t288 * t
     #63 * t132 / 0.720D3)
      t294 = t63 * t132
      t308 = -t206 / 0.720D3 + (0.90D2 * t7 * t8 * t221 - 0.180D3 * t45 
     #* t46 * t226 - t231) * t132 / 0.720D3
      t309 = FJET(XB1, XB2, s, -t167, t169, 0.0D0, t163, -t175, t308)
      t315 = 0.90D2 * t7 * t8 * t280 + t287
      t319 = FJET(XB1, XB2, s, -t246, t247, t243, -t245, t261, -t315 * t
     #63 * t132 / 0.720D3)
      rrgg2gght2s2e1 = t158 * t157 + t236 * t235 + t238 * t157 - t292 * 
     #t288 * t294 / 0.720D3 + t309 * t308 - t319 * t315 * t294 / 0.720D3

      end function



      doubleprecision function rrgg2gght2s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t25 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t27 = t15 * x4 * t4
      t30 = log(-0.4D1 * t13 * t27)
      t31 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t37 = 0.3141592653589793D1 * lh
      t38 = t6 * t8
      t39 = -t23 + t31
      t44 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t54 = x1 ** 2
      t55 = t54 * t12
      t58 = log(-0.4D1 * t55 * t27)
      t70 = rrgg2ggh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t78 = log(-0.4D1 * t12 * t15 * t17)
      t79 = t78 * 0.3141592653589793D1
      t86 = lh ** 2
      t88 = 0.3141592653589793D1 ** 2
      t94 = t78 ** 2
      t102 = (0.90D2 * t7 * t8 * (-t9 + t22 * t23 + t25 - t30 * t31) - 0
     #.180D3 * t37 * t38 * t39) * t44 / 0.1440D4 + t7 * t8 * t39 * t44 *
     # t50 / 0.8D1 + (0.90D2 * t7 * t8 * (t25 - t58 * t31) - 0.180D3 * t
     #37 * t38 * t31) * t50 / 0.720D3 + t7 * t8 * t70 / 0.16D2 + (-0.180
     #D3 * t37 - 0.90D2 * t79) * t6 * t8 * t25 / 0.1440D4 + (0.314159265
     #3589793D1 * (0.180D3 * t86 - 0.30D2 * t88) + 0.180D3 * t79 * lh + 
     #0.45D2 * t94 * 0.3141592653589793D1) * t6 * t8 * t31 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t102)
      t105 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t106 = s * t105
      t107 = x1 * t1
      t108 = t106 * t107
      t109 = -0.1D1 + x1
      t110 = t1 * t109
      t111 = t110 * x4
      t112 = t106 * t111
      t113 = t110 * t4
      t114 = t106 * t113
      t115 = t105 ** 2
      t118 = x1 * t109
      t120 = s * t115 * t6 * t118 * x4
      t122 = t7 * t8 * t109
      t124 = 0.1D1 / (-0.2D1 + t105)
      t125 = t115 * t124
      t126 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t131 = t122 * t125 * t126 * t44 * t50 / 0.8D1
      t132 = t109 * t115
      t133 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t137 = t115 ** 2
      t138 = t109 ** 2
      t143 = log(-0.4D1 * t55 * t15 * t17 * t137 * t138)
      t147 = -t132 * t124 * t133 + t143 * t109 * t125 * t126
      t155 = 0.180D3 * t37 * t38 * t132 * t124 * t126
      t159 = -t131 + (0.90D2 * t7 * t8 * t147 + t155) * t50 / 0.720D3
      t160 = FJET(XB1, XB2, s, 0.0D0, t108, -t112, t114, -t120, t159)
      t162 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t102)
      t164 = KAPPA2(x1, x2, x3, x4, z)
      t165 = s * t164
      t167 = t165 * t107 * x3
      t169 = t165 * t107 * t18
      t170 = t165 * t111
      t171 = t165 * t113
      t172 = t164 ** 2
      t177 = cos(t10)
      t180 = Sqrt(x3 * t18 * t17)
      t185 = s * t172 * t6 * t118 * (-x3 - x4 + 0.2D1 * x3 * x4 + 0.2D1 
     #* t177 * t180)
      t189 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t192 = t172 / (-0.2D1 + t164) * t189 * t44 * t50
      t194 = t122 * t192 / 0.8D1
      t195 = FJET(XB1, XB2, s, t167, -t169, -t170, t171, t185, t194)
      t197 = t38 * t109
      t208 = -t131 + (0.90D2 * t7 * t8 * t147 + t155) * t50 / 0.720D3
      t209 = FJET(XB1, XB2, s, -t112, t114, 0.0D0, t108, -t120, t208)
      t211 = FJET(XB1, XB2, s, -t170, t171, t167, -t169, t185, t194)
      rrgg2gght2s2e0 = t103 * t102 + t160 * t159 + t162 * t102 + t195 * 
     #0.3141592653589793D1 * t197 * t192 / 0.8D1 + t209 * t208 + t211 * 
     #0.3141592653589793D1 * t197 * t192 / 0.8D1

      end function



      doubleprecision function rrgg2gght2s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x4
     #)
      t10 = t8 * t9
      t11 = 0.1D1 / x1
      t15 = rrgg2ggh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x
     #4)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t30 = log(-0.4D1 * t23 / t24 * x4 * t4)
      t37 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4)
      t44 = t7 * t10 * t11 / 0.8D1 + t7 * t8 * t15 / 0.16D2 + (-0.180D3 
     #* 0.3141592653589793D1 * lh - 0.90D2 * t30 * 0.3141592653589793D1)
     # * t6 * t10 / 0.1440D4 + t7 * t8 * (-t37 + t9) / x3 / 0.16D2
      t45 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t44)
      t47 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t48 = s * t47
      t50 = t48 * t1 * x1
      t51 = -0.1D1 + x1
      t52 = t1 * t51
      t54 = t48 * t52 * x4
      t56 = t48 * t52 * t4
      t57 = t47 ** 2
      t62 = s * t57 * t6 * x1 * t51 * x4
      t66 = 0.1D1 / (-0.2D1 + t47)
      t68 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x4)
      t72 = t7 * t8 * t51 * t57 * t66 * t68 * t11 / 0.8D1
      t73 = FJET(XB1, XB2, s, 0.0D0, t50, -t54, t56, -t62, -t72)
      t75 = t6 * t8
      t80 = t51 * t57 * t66 * t68 * t11
      t83 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t44)
      t85 = FJET(XB1, XB2, s, -t54, t56, 0.0D0, t50, -t62, -t72)
      rrgg2gght2s2em1 = t45 * t44 - t73 * 0.3141592653589793D1 * t75 * t
     #80 / 0.8D1 + t83 * t44 - t85 * 0.3141592653589793D1 * t75 * t80 / 
     #0.8D1

      end function



      doubleprecision function rrgg2gght2s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2ggh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0, x4
     #)
      t12 = 0.3141592653589793D1 * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2gght2s2em2 = t13 * 0.3141592653589793D1 * t16 / 0.16D2 + t18 
     #* 0.3141592653589793D1 * t16 / 0.16D2

      end function



      doubleprecision function rrgg2gght2s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      rrgg2gght2s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2gght2s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2ggh21J1
      doubleprecision rrgg2ggh21J2
      doubleprecision rrgg2ggh21J3
      doubleprecision rrgg2ggh21J4
      doubleprecision rrgg2ggh21J5
      doubleprecision rrgg2ggh21J6
      doubleprecision rrgg2ggh21J7
      rrgg2gght2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2ggh21J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t2 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = t7 * x1
      t9 = t5 * t8
      t10 = s * t3
      t11 = t6 * x1
      t12 = t11 * x3
      t14 = 0.1D1 - x3
      t15 = t11 * t14
      t17 = s - t10 * t12 - t10 * t15
      t18 = x3 * t17
      t19 = 0.1D1 - x1
      t20 = t19 * x4
      t24 = t6 * t19
      t27 = 0.1D1 - x4
      t30 = s - t10 * t24 * x4 - t10 * t24 * t27
      t31 = t17 * t30
      t32 = s * t1
      t34 = t31 * t32 * t4
      t35 = t19 ** 2
      t37 = x4 * t27
      t41 = t2 * z
      t42 = t4 ** 2
      t43 = t7 ** 2
      t44 = t42 * t43
      t45 = t41 * t44
      t46 = x1 ** 2
      t47 = t46 * x1
      t48 = t14 ** 2
      t49 = t47 * t48
      t50 = t17 * t19
      t54 = cos(x2 * 0.3141592653589793D1)
      t57 = Sqrt(x3 * t14 * t37)
      t60 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t54 * t57
      t61 = t50 * t60
      t65 = x3 ** 2
      t66 = t47 * t65
      t70 = t4 * t3
      t71 = t7 * t6
      t72 = t70 * t71
      t73 = t72 * x1
      t74 = t41 * t73
      t75 = t14 * t17
      t76 = t35 * x4
      t77 = t76 * t27
      t78 = t75 * t77
      t81 = t42 * t3
      t82 = t43 * t6
      t83 = t81 * t82
      t84 = t41 * t83
      t85 = t47 * x3
      t86 = t17 * t35
      t87 = t60 ** 2
      t88 = t86 * t87
      t89 = t85 * t88
      t93 = t31 * t32 * t70
      t94 = t71 * t35
      t101 = t31 * t32 * t3
      t106 = t30 * t2
      t107 = t106 * t72
      t108 = t46 * x3
      t109 = z * t19
      t110 = t109 * t60
      t114 = t47 * t14
      t115 = t114 * t88
      t118 = t2 * t70
      t119 = t71 * t46
      t120 = t118 * t119
      t121 = t48 * t17
      t128 = t14 * t19
      t133 = x1 * x3
      t134 = x4 ** 2
      t135 = t35 * t134
      t136 = t133 * t135
      t139 = t32 * t42
      t141 = t31 * t139 * t43
      t142 = t46 * t35
      t143 = t60 * x4
      t145 = t142 * t143 * t14
      t152 = t2 * s
      t153 = t152 * t42
      t154 = t43 * t46
      t155 = t153 * t154
      t156 = x3 * t35
      t157 = t60 * t27
      t161 = t30 * t32
      t162 = t161 * t72
      t163 = t46 * t65
      t164 = t50 * x4
      t165 = t163 * t164
      t168 = t2 * t42
      t169 = t168 * t154
      t170 = t76 * t60
      t171 = t75 * t170
      t176 = t2 * t81 * t82 * t47
      t177 = t35 * t87
      t181 = -0.67D2 * t9 * t18 * t20 + 0.96D2 * t34 * t7 * t35 * t37 + 
     #0.16D2 * t45 * t49 * t61 + 0.16D2 * t45 * t66 * t61 + 0.16D2 * t74
     # * t78 - 0.16D2 * t84 * t89 - 0.128D3 * t93 * t94 * x4 * x1 * t60 
     #+ 0.768D3 * t101 * t11 * x3 * z + 0.384D3 * t107 * t108 * t110 - 0
     #.16D2 * t84 * t115 - 0.133D3 * t120 * t121 * t20 - 0.148D3 * t9 * 
     #t75 * t20 - 0.86D2 * t34 * t8 * t128 * t27 + 0.260D3 * t107 * t136
     # + 0.128D3 * t141 * t145 + 0.488D3 * t34 * t8 * t128 * x4 + 0.48D2
     # * t155 * t156 * t157 - 0.112D3 * t162 * t165 + 0.133D3 * t169 * t
     #171 - 0.48D2 * t176 * t75 * t177
      t182 = x1 * t14
      t183 = t27 ** 2
      t184 = t86 * t183
      t185 = t182 * t184
      t188 = t152 * t70
      t189 = t71 * x1
      t190 = t188 * t189
      t197 = t118 * t189
      t200 = t161 * t83
      t203 = t106 * t44
      t204 = t46 * t14
      t206 = t35 * t27 * t60
      t207 = t204 * t206
      t210 = t19 * t60
      t211 = t108 * t210
      t215 = t153 * t43 * x1
      t216 = t35 * t19
      t217 = x3 * t216
      t218 = x4 * t183
      t222 = t31 * t32
      t224 = x3 * t19
      t230 = t14 * t216
      t231 = t134 * t27
      t235 = t46 * t48
      t236 = t235 * t164
      t239 = t152 * t81
      t241 = t239 * t82 * t46
      t242 = t134 * t60
      t254 = t133 * t184
      t257 = t4 * t7
      t258 = t106 * t257
      t259 = t182 * t20
      t262 = t118 * t71
      t263 = t48 * t14
      t268 = t5 * t7
      t272 = t65 * x3
      t277 = 0.6D1 * t215 * t230 * t231 - 0.160D3 * t162 * t236 + 0.8D1 
     #* t241 * t217 * t242 - 0.6D1 * t215 * t217 * t231 + 0.96D2 * t141 
     #* t207 - 0.133D3 * t120 * t18 * t210 - 0.32D2 * t162 * t254 - 0.80
     #D2 * t258 * t259 - 0.48D2 * t262 * t47 * t263 * t17 + 0.133D3 * t2
     #68 * t163 * t17 - 0.48D2 * t262 * t47 * t272 * t17
      t280 = t106 * t70
      t281 = t71 * t47
      t288 = t106 * t3
      t297 = t41 * t44 * t46
      t298 = t18 * t170
      t301 = t31 * t139
      t305 = t19 * t27
      t306 = t133 * t305
      t310 = t2 * t3 * t6
      t314 = t106 * t4
      t315 = t7 * t46
      t328 = t14 * t35
      t332 = t35 * t183
      t333 = t182 * t332
      t338 = t142 * t60 * x3 * x4
      t351 = t152 * t42 * t4 * t43 * t7 * t47
      t352 = x4 * t87
      t358 = 0.5D1 * t280 * t281 * t263 + 0.5D1 * t280 * t281 * t272 + 0
     #.5D1 * t288 * t15 - 0.32D2 * t200 * t89 + 0.176D3 * t107 * t182 * 
     #t77 - 0.8D1 * t297 * t298 + 0.32D2 * t301 * t154 * t177 - 0.80D2 *
     # t258 * t306 + 0.74D2 * t310 * t133 * t17 + 0.74D2 * t314 * t315 *
     # t48 + 0.74D2 * t310 * t182 * t17 + 0.133D3 * t268 * t235 * t17 - 
     #0.80D2 * t203 * t66 * t210 + 0.48D2 * t155 * t328 * t143 + 0.260D3
     # * t107 * t333 + 0.96D2 * t141 * t338 - 0.488D3 * t93 * t119 * t22
     #4 * t60 + 0.205D3 * t169 * t298 - 0.24D2 * t351 * t230 * t352 + 0.
     #5D1 * t288 * t12
      t362 = t106 * t83
      t363 = t114 * t177
      t366 = t182 * t135
      t369 = t41 * t72
      t372 = t65 * t17
      t376 = t43 * t47
      t384 = t85 * t177
      t395 = t50 * t27
      t396 = t163 * t395
      t401 = t3 * t6
      t405 = z ** 2
      t413 = t188 * t71
      t414 = t133 * t332
      t426 = t153 * t43
      t428 = t216 * t183 * t27
      t432 = t239 * t82
      t435 = 0.24D2 * t369 * t396 - 0.8D1 * t369 * t185 - 0.488D3 * t222
     # * t401 * t182 - 0.384D3 * t288 * t11 * t14 * t405 - 0.112D3 * t22
     #2 * t401 * t305 - 0.6D1 * t413 * t414 + 0.488D3 * t222 * t257 * t2
     #35 - 0.112D3 * t222 * t401 * t20 + 0.32D2 * t222 * t257 * t332 + 0
     #.2D1 * t426 * t182 * t428 - 0.120D3 * t432 * t363
      t452 = t161 * t73
      t453 = t18 * t77
      t459 = t152 * t42 * t70 * t43 * t71
      t460 = t46 ** 2
      t463 = t216 * t87 * t60
      t468 = t152 * t4 * t7
      t474 = t41 * t70
      t476 = t281 * t263 * t17
      t488 = t216 * t134 * x4
      t493 = t281 * t272 * t17
      t513 = -0.384D3 * t288 * t11 * x3 * t405 + 0.32D2 * t222 * t257 * 
     #t135 - 0.7D1 * t197 * t18 * t135 + 0.74D2 * t197 * t18 * t332 - 0.
     #128D3 * t452 * t453 + 0.32D2 * t459 * t460 * x3 * t463 + 0.14D2 * 
     #t468 * t182 * t305 - 0.120D3 * t432 * t384 - 0.32D2 * t474 * t476 
     #+ 0.488D3 * t222 * t257 * t163 + 0.14D2 * t468 * t133 * t20 - 0.6D
     #1 * t413 * t366 - 0.2D1 * t426 * t182 * t488 - 0.32D2 * t474 * t49
     #3 - 0.6D1 * t413 * t333 - 0.488D3 * t222 * t401 * t133 - 0.2D1 * t
     #426 * t133 * t428 + 0.2D1 * t426 * t133 * t488 + 0.32D2 * t459 * t
     #460 * t14 * t463 - 0.14D2 * t468 * t306
      t518 = t161 * t70
      t525 = t204 * t210
      t528 = t152 * t3
      t541 = t87 * t27
      t553 = t108 * t206
      t556 = t86 * t134
      t557 = t133 * t556
      t560 = t168 * t376
      t574 = 0.67D2 * t197 * t453 - 0.24D2 * t351 * t217 * t541 - 0.384D
     #3 * t258 * t133 * t109 * t27 + 0.112D3 * t301 * t376 * t210 * t48 
     #+ 0.16D2 * t203 * t553 - 0.32D2 * t162 * t557 + 0.96D2 * t560 * t3
     #72 * t210 + 0.38D2 * t107 * t414 + 0.176D3 * t107 * t133 * t77 - 0
     #.176D3 * t203 * t338 - 0.7D1 * t197 * t75 * t332
      t581 = t182 * t556
      t584 = t75 * t206
      t604 = t37 * t60
      t631 = t235 * t395
      t634 = -0.8D1 * t369 * t557 + 0.24D2 * t369 * t236 - 0.8D1 * t369 
     #* t581 - 0.8D1 * t297 * t584 - 0.128D3 * t452 * t78 + 0.205D3 * t1
     #69 * t584 - 0.133D3 * t120 * t372 * t305 + 0.488D3 * t34 * t8 * t2
     #24 * t27 + 0.8D1 * t241 * t230 * t242 + 0.24D2 * t351 * t230 * t54
     #1 - 0.16D2 * t241 * t217 * t604 - 0.67D2 * t9 * t75 * t305 + 0.128
     #D3 * t141 * t553 - 0.128D3 * t93 * t94 * t27 * x1 * t60 + 0.160D3 
     #* t34 * t8 * t210 - 0.48D2 * t155 * t156 * t143 - 0.80D2 * t203 * 
     #t49 * t210 + 0.16D2 * t203 * t145 + 0.16D2 * t74 * t453 - 0.24D2 *
     # t369 * t631
      t635 = t18 * t206
      t659 = t183 * t60
      t696 = 0.8D1 * t241 * t217 * t659 + 0.12D2 * t190 * t328 * t37 - 0
     #.6D1 * t215 * t230 * t218 - 0.16D2 * t241 * t230 * t604 + 0.74D2 *
     # t197 * t75 * t135 + 0.8D1 * t297 * t171 - 0.160D3 * t162 * t396 -
     # 0.384D3 * t258 * t182 * t20 * z - 0.16D2 * t107 * t163 * t305 - 0
     #.112D3 * t162 * t631 - 0.488D3 * t93 * t119 * t128 * t60
      rrgg2ggh21J1 = 0.9D1 / 0.16D2 * wd * (t513 + t358 + 0.8D1 * t297 *
     # t635 - 0.8D1 * t369 * t254 - 0.32D2 * t162 * t581 + 0.133D3 * t16
     #9 * t635 - 0.32D2 * t162 * t185 + 0.104D3 * t413 * t525 - 0.42D2 *
     # t528 * t15 - 0.42D2 * t528 * t12 - 0.16D2 * t107 * t525 - 0.6D1 *
     # t413 * t136 + 0.104D3 * t413 * t211 - 0.16D2 * t518 * t476 - 0.16
     #D2 * t518 * t493 - 0.14D2 * t468 * t259 + 0.38D2 * t362 * t384 + 0
     #.38D2 * t362 * t363 + 0.38D2 * t107 * t366 - 0.24D2 * t369 * t165 
     #+ 0.67D2 * t197 * t78 - 0.32D2 * t200 * t115 - 0.176D3 * t203 * t2
     #07 - 0.16D2 * t107 * t211 + 0.112D3 * t301 * t376 * t210 * t65 + 0
     #.768D3 * t101 * t11 * t14 * z - 0.86D2 * t34 * t8 * t224 * x4 + t2
     #77 + t696 + 0.6D1 * t215 * t217 * t218 - 0.16D2 * t107 * t235 * t2
     #0 - 0.48D2 * t155 * t328 * t157 + 0.96D2 * t560 * t121 * t210 + 0.
     #384D3 * t107 * t204 * t110 - 0.148D3 * t9 * t18 * t305 - 0.205D3 *
     # t120 * t372 * t20 - 0.205D3 * t120 * t121 * t305 - 0.133D3 * t120
     # * t75 * t210 - 0.48D2 * t176 * t18 * t177 + 0.8D1 * t241 * t230 *
     # t659 + 0.12D2 * t190 * t156 * t37 + 0.24D2 * t351 * t217 * t352 +
     # 0.74D2 * t314 * t315 * t65 + 0.16D2 * t222 + t574 + t181 + t435 +
     # t634) / t30 / t1 / t17 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh21J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t4 * t3
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t3 ** 2
      t17 = t15 * t16
      t18 = t14 * t17
      t19 = 0.1D1 - x3
      t20 = x1 * t19
      t21 = t4 * x4
      t25 = 0.384D3 * t18 * t20 * t21 * z
      t26 = t13 * z
      t27 = t15 * t1
      t28 = t16 * t3
      t29 = t27 * t28
      t30 = t29 * x1
      t31 = t26 * t30
      t32 = t3 * x1
      t33 = t32 * x3
      t35 = t32 * t19
      t37 = s - t2 * t33 - t2 * t35
      t38 = x3 * t37
      t39 = t4 ** 2
      t40 = t39 * x4
      t41 = t40 * t8
      t42 = t38 * t41
      t44 = 0.16D2 * t31 * t42
      t45 = t26 * t29
      t46 = x1 ** 2
      t47 = t19 ** 2
      t48 = t46 * t47
      t49 = t37 * t4
      t50 = t49 * t8
      t51 = t48 * t50
      t52 = t45 * t51
      t54 = t15 ** 2
      t55 = t16 ** 2
      t56 = t54 * t55
      t58 = t26 * t56 * t46
      t63 = cos(x2 * 0.3141592653589793D1)
      t65 = x4 * t8
      t67 = Sqrt(x3 * t19 * t65)
      t70 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t63 * t67
      t71 = t39 * t8 * t70
      t72 = t38 * t71
      t74 = 0.8D1 * t58 * t72
      t75 = x1 * x3
      t76 = t37 * t39
      t77 = t8 ** 2
      t78 = t76 * t77
      t79 = t75 * t78
      t81 = 0.8D1 * t45 * t79
      t82 = t12 * s
      t83 = t11 * t82
      t84 = t83 * t29
      t85 = x3 ** 2
      t86 = t46 * t85
      t87 = t49 * x4
      t88 = t86 * t87
      t89 = t84 * t88
      t91 = t54 * t1
      t93 = t55 * t3
      t94 = t46 * x1
      t96 = t13 * t91 * t93 * t94
      t97 = t70 ** 2
      t98 = t39 * t97
      t100 = t96 * t38 * t98
      t102 = t48 * t87
      t103 = t84 * t102
      t105 = t37 * t11
      t107 = t105 * t82 * t15
      t108 = t16 * x1
      t109 = t19 * t4
      t112 = t107 * t108 * t109 * x4
      t114 = t13 * s
      t115 = t114 * t54
      t116 = t55 * t46
      t117 = t115 * t116
      t118 = x3 * t39
      t119 = t70 * t8
      t122 = 0.48D2 * t117 * t118 * t119
      t124 = t115 * t55 * x1
      t125 = t39 * t4
      t126 = x3 * t125
      t127 = x4 * t77
      t130 = 0.6D1 * t124 * t126 * t127
      t131 = t13 * t27
      t132 = t28 * t46
      t133 = t131 * t132
      t134 = t85 * t37
      t136 = t133 * t134 * t21
      t138 = t13 * t15
      t139 = t138 * t108
      t141 = t139 * t38 * t21
      t146 = 0.96D2 * t107 * t16 * t39 * t65
      t147 = x4 ** 2
      t148 = t147 * t8
      t151 = 0.6D1 * t124 * t126 * t148
      t152 = t19 * t37
      t154 = t96 * t152 * t98
      t156 = t20 * t78
      t158 = 0.32D2 * t84 * t156
      t159 = t114 * t27
      t160 = t28 * x1
      t161 = t159 * t160
      t164 = 0.12D2 * t161 * t118 * t65
      t165 = t82 * t54
      t167 = t105 * t165 * t55
      t168 = t46 * t39
      t171 = t168 * t70 * t19 * t8
      t173 = 0.96D2 * t167 * t171
      t174 = t4 * t70
      t176 = t133 * t38 * t174
      t178 = -t25 + t44 - 0.24D2 * t52 + t74 - t81 - 0.112D3 * t89 - 0.4
     #8D2 * t100 - 0.160D3 * t103 + 0.488D3 * t112 + t122 + t130 - 0.205
     #D3 * t136 - 0.67D2 * t141 + t146 - t151 - 0.48D2 * t154 - t158 + t
     #164 + t173 - 0.133D3 * t176
      t179 = t4 * t8
      t181 = t133 * t134 * t179
      t183 = x3 * t4
      t186 = t107 * t108 * t183 * t8
      t188 = t19 * t125
      t191 = 0.6D1 * t124 * t188 * t148
      t192 = t14 * t29
      t193 = t39 * t77
      t194 = t20 * t193
      t196 = 0.260D3 * t192 * t194
      t199 = t168 * t70 * x3 * x4
      t201 = 0.96D2 * t167 * t199
      t203 = t105 * t82 * t27
      t206 = t203 * t132 * t183 * t70
      t208 = t13 * t54
      t209 = t208 * t116
      t210 = t152 * t71
      t211 = t209 * t210
      t213 = t40 * t70
      t214 = t38 * t213
      t215 = t209 * t214
      t217 = t131 * t160
      t218 = t39 * t147
      t220 = t217 * t38 * t218
      t223 = t217 * t38 * t193
      t226 = t83 * t30
      t228 = 0.128D3 * t226 * t42
      t230 = t107 * t108 * t174
      t232 = x4 * t70
      t235 = 0.48D2 * t117 * t118 * t232
      t236 = t14 * t56
      t237 = t94 * t47
      t240 = 0.80D2 * t236 * t237 * t174
      t241 = t46 * t19
      t242 = t241 * t213
      t244 = 0.16D2 * t236 * t242
      t245 = t46 * x3
      t246 = z * t4
      t247 = t246 * t70
      t250 = 0.384D3 * t192 * t245 * t247
      t251 = t26 * t56
      t252 = t49 * t70
      t255 = 0.16D2 * t251 * t237 * t252
      t256 = t152 * t213
      t258 = 0.8D1 * t58 * t256
      t259 = t105 * t82
      t260 = t1 * t3
      t262 = t259 * t260 * t179
      t264 = t14 * t1
      t265 = z ** 2
      t269 = 0.384D3 * t264 * t32 * t19 * t265
      t271 = t259 * t260 * t20
      t273 = -t228 + 0.160D3 * t230 - t235 - t240 + t244 + t250 + t255 +
     # t258 - 0.112D3 * t262 - t269 - 0.488D3 * t271
      t277 = t259 * t17 * t48
      t279 = t159 * t28
      t280 = t75 * t193
      t282 = 0.6D1 * t279 * t280
      t285 = 0.32D2 * t259 * t17 * t193
      t287 = t259 * t260 * t21
      t289 = t115 * t55
      t291 = t125 * t77 * t8
      t294 = 0.2D1 * t289 * t20 * t291
      t297 = 0.32D2 * t259 * t17 * t218
      t301 = 0.384D3 * t264 * t32 * x3 * t265
      t302 = t114 * t91
      t303 = t302 * t93
      t304 = t94 * t19
      t305 = t304 * t98
      t307 = 0.120D3 * t303 * t305
      t311 = t114 * t54 * t27 * t55 * t28
      t312 = t46 ** 2
      t315 = t125 * t97 * t70
      t318 = 0.32D2 * t311 * t312 * x3 * t315
      t320 = t114 * t15 * t16
      t323 = 0.14D2 * t320 * t20 * t179
      t324 = t26 * t27
      t325 = t28 * t94
      t326 = t47 * t19
      t328 = t325 * t326 * t37
      t330 = 0.32D2 * t324 * t328
      t331 = t94 * x3
      t332 = t331 * t98
      t334 = 0.120D3 * t303 * t332
      t337 = 0.2D1 * t289 * t75 * t291
      t339 = t259 * t260 * t75
      t342 = 0.6D1 * t279 * t194
      t343 = t85 * x3
      t345 = t325 * t343 * t37
      t347 = 0.32D2 * t324 * t345
      t349 = t125 * t147 * x4
      t352 = 0.2D1 * t289 * t20 * t349
      t353 = t20 * t218
      t355 = 0.6D1 * t279 * t353
      t358 = 0.14D2 * t320 * t75 * t21
      t360 = t259 * t17 * t86
      t362 = 0.488D3 * t277 - t282 + t285 - 0.112D3 * t287 + t294 + t297
     # - t301 - t307 + t318 + t323 - t330 - t334 - t337 - 0.488D3 * t339
     # - t342 - t347 - t352 - t355 + t358 + 0.488D3 * t360
      t363 = t241 * t174
      t365 = 0.104D3 * t279 * t363
      t366 = t20 * t21
      t368 = 0.14D2 * t320 * t366
      t369 = t83 * t27
      t370 = t369 * t345
      t372 = t369 * t328
      t374 = t245 * t174
      t376 = 0.104D3 * t279 * t374
      t377 = t75 * t218
      t379 = 0.6D1 * t279 * t377
      t380 = t75 * t179
      t382 = 0.14D2 * t320 * t380
      t386 = 0.32D2 * t311 * t312 * t19 * t315
      t389 = 0.2D1 * t289 * t75 * t349
      t391 = 0.128D3 * t167 * t242
      t393 = t105 * t165
      t394 = t55 * t94
      t397 = t393 * t394 * t174 * t47
      t399 = t245 * t71
      t401 = 0.16D2 * t236 * t399
      t402 = t152 * t41
      t403 = t217 * t402
      t405 = t91 * t93
      t406 = t83 * t405
      t407 = t76 * t97
      t408 = t304 * t407
      t410 = 0.32D2 * t406 * t408
      t411 = t86 * t50
      t412 = t84 * t411
      t414 = t47 * t37
      t416 = t133 * t414 * t21
      t419 = t139 * t152 * t21
      t421 = t114 * t1
      t423 = 0.42D2 * t421 * t35
      t425 = 0.42D2 * t421 * t33
      t430 = t114 * t54 * t15 * t55 * t16 * t94
      t431 = x4 * t97
      t434 = 0.24D2 * t430 * t188 * t431
      t436 = t302 * t93 * t46
      t437 = t147 * t70
      t440 = 0.8D1 * t436 * t188 * t437
      t441 = 0.112D3 * t397 + t401 + 0.67D2 * t403 - t410 - 0.160D3 * t4
     #12 - 0.133D3 * t416 - 0.148D3 * t419 - t423 - t425 - t434 + t440
      t445 = t97 * t8
      t448 = 0.24D2 * t430 * t188 * t445
      t449 = t65 * t70
      t452 = 0.16D2 * t436 * t126 * t449
      t454 = t139 * t152 * t179
      t458 = 0.16D2 * t192 * t48 * t21
      t459 = t76 * t147
      t460 = t20 * t459
      t462 = 0.32D2 * t84 * t460
      t463 = t28 * t39
      t468 = 0.128D3 * t203 * t463 * x4 * x1 * t70
      t469 = t209 * t72
      t471 = t77 * t70
      t474 = 0.8D1 * t436 * t188 * t471
      t477 = 0.8D1 * t436 * t126 * t471
      t478 = t19 * t39
      t481 = 0.12D2 * t161 * t478 * t65
      t484 = 0.6D1 * t124 * t188 * t127
      t487 = 0.16D2 * t436 * t188 * t449
      t489 = 0.16D2 * t192 * t363
      t491 = t139 * t38 * t179
      t493 = t217 * t42
      t497 = 0.24D2 * t430 * t126 * t445
      t501 = 0.384D3 * t18 * t75 * t246 * t8
      t504 = 0.384D3 * t192 * t241 * t247
      t505 = t45 * t411
      t508 = 0.8D1 * t45 * t156
      t509 = t448 - t452 - 0.67D2 * t454 - t458 - t462 - t468 + 0.133D3 
     #* t469 + t474 + t477 + t481 - t484 - t487 - t489 - 0.148D3 * t491 
     #+ 0.67D2 * t493 - t497 - t501 + t504 + 0.24D2 * t505 - t508
      t510 = t75 * t459
      t512 = 0.32D2 * t84 * t510
      t513 = t208 * t394
      t515 = t513 * t134 * t174
      t518 = 0.38D2 * t192 * t280
      t521 = 0.176D3 * t192 * t75 * t41
      t524 = t107 * t108 * t109 * t8
      t528 = t203 * t132 * t109 * t70
      t532 = 0.8D1 * t436 * t126 * t437
      t534 = 0.8D1 * t58 * t210
      t536 = 0.128D3 * t226 * t402
      t537 = t84 * t51
      t541 = t26 * t405
      t543 = 0.16D2 * t541 * t408
      t544 = t331 * t407
      t546 = 0.32D2 * t406 * t544
      t549 = 0.176D3 * t192 * t20 * t41
      t551 = 0.8D1 * t58 * t214
      t554 = t393 * t394 * t174 * t85
      t558 = 0.24D2 * t430 * t126 * t431
      t559 = t14 * t405
      t561 = 0.38D2 * t559 * t332
      t563 = t133 * t414 * t179
      t566 = t133 * t152 * t174
      t569 = t513 * t414 * t174
      t571 = 0.16D2 * t259 - t543 - t546 + t549 - t551 + 0.112D3 * t554 
     #+ t558 + t561 - 0.205D3 * t563 - 0.133D3 * t566 + 0.96D2 * t569
      t576 = 0.48D2 * t117 * t478 * t119
      t578 = 0.5D1 * t264 * t33
      t579 = t14 * t15
      t580 = t16 * t46
      t583 = 0.74D2 * t579 * t580 * t85
      t585 = t13 * t1 * t3
      t587 = t585 * t75 * t37
      t591 = 0.74D2 * t579 * t580 * t47
      t593 = t585 * t20 * t37
      t595 = t138 * t16
      t597 = t595 * t48 * t37
      t599 = t131 * t28
      t602 = t599 * t94 * t326 * t37
      t605 = t595 * t86 * t37
      t609 = t599 * t94 * t343 * t37
      t611 = t14 * t27
      t614 = 0.5D1 * t611 * t325 * t326
      t617 = 0.5D1 * t611 * t325 * t343
      t619 = 0.5D1 * t264 * t35
      t622 = t107 * t108 * t183 * x4
      t624 = t94 * t85
      t627 = 0.80D2 * t236 * t624 * t174
      t630 = 0.48D2 * t117 * t478 * t232
      t632 = t217 * t152 * t218
      t636 = 0.16D2 * t251 * t624 * t252
      t638 = 0.16D2 * t31 * t402
      t640 = 0.16D2 * t541 * t544
      t641 = -t576 + t578 + t583 + 0.74D2 * t587 + t591 + 0.74D2 * t593 
     #+ 0.133D3 * t597 - 0.48D2 * t602 + 0.133D3 * t605 - 0.48D2 * t609 
     #+ t614 + t617 + t619 - 0.86D2 * t622 - t627 + t630 + 0.74D2 * t632
     # + t636 + t638 - t640
      t643 = 0.38D2 * t559 * t305
      t645 = 0.38D2 * t192 * t353
      t647 = 0.176D3 * t236 * t171
      t649 = 0.8D1 * t45 * t510
      t650 = t45 * t102
      t653 = 0.8D1 * t45 * t460
      t655 = 0.176D3 * t236 * t199
      t657 = 0.128D3 * t167 * t399
      t658 = t209 * t256
      t661 = 0.16D2 * t192 * t374
      t664 = 0.260D3 * t192 * t377
      t666 = 0.32D2 * t84 * t79
      t668 = 0.80D2 * t18 * t366
      t670 = t105 * t82 * t1
      t674 = 0.768D3 * t670 * t32 * x3 * z
      t679 = 0.128D3 * t203 * t463 * t8 * x1 * t70
      t683 = 0.768D3 * t670 * t32 * t19 * z
      t684 = t45 * t88
      t687 = t217 * t152 * t193
      t691 = 0.16D2 * t192 * t86 * t179
      t694 = 0.32D2 * t393 * t116 * t98
      t696 = 0.80D2 * t18 * t380
      t697 = t664 - t666 - t668 + t674 - t679 + t683 - 0.24D2 * t684 - 0
     #.7D1 * t687 - t691 + t694 - t696
      t713 = t25 - t44 + 0.48D2 * t52 - t74 + t81 + 0.128D3 * t89 + 0.12
     #0D3 * t100 + 0.176D3 * t103 - 0.597D3 * t112 - t122 - t130 + 0.102
     #D3 * t136 - 0.12D2 * t141 - t146 + t151 + 0.120D3 * t154 + t158 - 
     #t164 - t173 + 0.30D2 * t176
      t725 = t228 - 0.176D3 * t230 + t235 + t240 - t244 - t250 - t255 - 
     #t258 + 0.128D3 * t262 + t269 + 0.597D3 * t271
      t732 = -0.597D3 * t277 + t282 - t285 + 0.128D3 * t287 - t294 - t29
     #7 + t301 + t307 - t318 - t323 + t330 + t334 + t337 + 0.597D3 * t33
     #9 + t342 + t347 + t352 + t355 - t358 - 0.597D3 * t360
      t741 = -0.128D3 * t397 - t401 + 0.12D2 * t403 + t410 + 0.176D3 * t
     #412 + 0.30D2 * t416 + 0.80D2 * t419 + t423 + t425 + t434 - t440
      t750 = -t448 + t452 - 0.12D2 * t454 + t458 + t462 + t468 - 0.30D2 
     #* t469 - t474 - t477 - t481 + t484 + t487 + t489 + 0.80D2 * t491 +
     # 0.12D2 * t493 + t497 + t501 - t504 - 0.48D2 * t505 + t508
      t761 = -0.32D2 * t259 + t543 + t546 - t549 + t551 - 0.128D3 * t554
     # - t558 - t561 + 0.102D3 * t563 + 0.30D2 * t566 - 0.240D3 * t569
      t772 = t576 - t578 - t583 - 0.40D2 * t587 - t591 - 0.40D2 * t593 -
     # 0.30D2 * t597 + 0.120D3 * t602 - 0.30D2 * t605 + 0.120D3 * t609 -
     # t614 - t617 - t619 + 0.176D3 * t622 + t627 - t630 - 0.40D2 * t632
     # - t636 - t638 + t640
      t778 = -t664 + t666 + t668 - t674 + t679 - t683 + 0.48D2 * t684 + 
     #0.52D2 * t687 + t691 - t694 + t696
      rrgg2ggh21J2 = 0.9D1 / 0.16D2 * (0.2D1 * wd * (-t534 - t536 - 0.11
     #2D3 * t537 - t512 + 0.96D2 * t515 + t509 - t379 - t382 + t386 + t3
     #89 + 0.205D3 * t211 + t201 - t653 - t655 - 0.488D3 * t528 + t532 +
     # t645 - t647 - t649 + 0.24D2 * t650 - 0.86D2 * t524 - 0.16D2 * t37
     #2 - 0.488D3 * t206 + t273 + t697 + t641 + t643 + t376 - t368 - 0.1
     #6D2 * t370 + t196 - 0.7D1 * t220 + 0.74D2 * t223 + 0.205D3 * t215 
     #+ t191 + t518 + t521 + t441 + t362 + t365 + t571 + t391 - 0.133D3 
     #* t181 + 0.488D3 * t186 + t657 + 0.133D3 * t658 - t661 + t178) + w
     #d * (t534 + t536 + 0.128D3 * t537 + t512 - 0.240D3 * t515 + t379 +
     # t382 - t386 - t389 - 0.102D3 * t211 - t201 + t653 + t655 + 0.597D
     #3 * t528 - t532 - t645 + t647 + t649 - 0.48D2 * t650 + 0.176D3 * t
     #524 + 0.32D2 * t372 + 0.597D3 * t206 - t643 - t376 + t713 + t725 +
     # t368 + 0.32D2 * t370 - t196 + 0.52D2 * t220 - 0.40D2 * t223 - 0.1
     #02D3 * t215 - t191 - t518 - t521 - t365 + t761 - t391 + 0.30D2 * t
     #181 - 0.597D3 * t186 - t657 - 0.30D2 * t658 + t661 + t772 + t778 +
     # t732 + t741 + t750)) / t11 / t12 / t37 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh21J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t3 ** 2
      t17 = t15 * t16
      t18 = t14 * t17
      t19 = x1 * x3
      t20 = t4 * t8
      t21 = t19 * t20
      t22 = t18 * t21
      t23 = 0.80D2 * t22
      t24 = t13 * z
      t25 = t15 * t1
      t26 = t16 * t3
      t27 = t25 * t26
      t28 = t24 * t27
      t29 = x1 ** 2
      t30 = 0.1D1 - x3
      t31 = t30 ** 2
      t32 = t29 * t31
      t33 = t3 * x1
      t34 = t33 * x3
      t36 = t33 * t30
      t38 = s - t2 * t34 - t2 * t36
      t39 = t38 * t4
      t40 = t39 * x4
      t41 = t32 * t40
      t42 = t28 * t41
      t44 = x1 * t30
      t45 = t4 ** 2
      t46 = t38 * t45
      t47 = x4 ** 2
      t48 = t46 * t47
      t49 = t44 * t48
      t51 = 0.8D1 * t28 * t49
      t52 = t15 ** 2
      t53 = t16 ** 2
      t54 = t52 * t53
      t56 = t24 * t54 * t29
      t57 = t30 * t38
      t62 = cos(x2 * 0.3141592653589793D1)
      t64 = x4 * t8
      t66 = Sqrt(x3 * t30 * t64)
      t69 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t62 * t66
      t70 = t45 * t8 * t69
      t71 = t57 * t70
      t73 = 0.8D1 * t56 * t71
      t74 = t13 * t25
      t75 = t74 * t26
      t76 = t29 * x1
      t77 = t31 * t30
      t80 = t75 * t76 * t77 * t38
      t82 = t13 * t15
      t83 = t82 * t16
      t84 = x3 ** 2
      t85 = t29 * t84
      t87 = t83 * t85 * t38
      t89 = t84 * x3
      t92 = t75 * t76 * t89 * t38
      t94 = t14 * t25
      t95 = t26 * t76
      t97 = t94 * t95 * t77
      t98 = 0.5D1 * t97
      t100 = t94 * t95 * t89
      t101 = 0.5D1 * t100
      t102 = t14 * t1
      t103 = t102 * t36
      t104 = 0.5D1 * t103
      t105 = t13 * s
      t110 = t105 * t52 * t15 * t53 * t16 * t76
      t111 = t45 * t4
      t112 = t30 * t111
      t113 = t69 ** 2
      t114 = x4 * t113
      t117 = 0.24D2 * t110 * t112 * t114
      t118 = t52 * t1
      t119 = t105 * t118
      t120 = t53 * t3
      t122 = t119 * t120 * t29
      t123 = t47 * t69
      t126 = 0.8D1 * t122 * t112 * t123
      t127 = t105 * t52
      t128 = t53 * t29
      t129 = t127 * t128
      t130 = x3 * t45
      t131 = x4 * t69
      t134 = 0.48D2 * t129 * t130 * t131
      t135 = t14 * t54
      t136 = t76 * t31
      t137 = t4 * t69
      t139 = t135 * t136 * t137
      t140 = 0.80D2 * t139
      t141 = t29 * t30
      t142 = t45 * x4
      t143 = t142 * t69
      t144 = t141 * t143
      t146 = 0.16D2 * t135 * t144
      t147 = t14 * t27
      t148 = t29 * x3
      t149 = t148 * t137
      t151 = 0.16D2 * t147 * t149
      t152 = t45 * t47
      t153 = t19 * t152
      t155 = 0.260D3 * t147 * t153
      t156 = t8 ** 2
      t157 = t45 * t156
      t158 = t44 * t157
      t160 = 0.260D3 * t147 * t158
      t161 = t148 * t70
      t163 = 0.16D2 * t135 * t161
      t164 = x3 * t111
      t167 = 0.24D2 * t110 * t164 * t114
      t168 = -t23 + 0.24D2 * t42 - t51 - t73 - 0.48D2 * t80 + 0.133D3 * 
     #t87 - 0.48D2 * t92 + t98 + t101 + t104 - t117 + t126 - t134 - t140
     # + t146 - t151 + t155 + t160 + t163 + t167
      t169 = t26 * x1
      t170 = t74 * t169
      t171 = x3 * t38
      t173 = t170 * t171 * t152
      t175 = t38 * t11
      t176 = t12 * s
      t177 = t176 * t52
      t179 = t175 * t177 * t53
      t181 = 0.128D3 * t179 * t144
      t183 = t175 * t176 * t15
      t184 = t16 * x1
      t185 = t30 * t4
      t188 = t183 * t184 * t185 * x4
      t190 = t69 * t8
      t193 = 0.48D2 * t129 * t130 * t190
      t195 = t127 * t53 * x1
      t196 = x4 * t156
      t199 = 0.6D1 * t195 * t164 * t196
      t200 = t175 * t177
      t201 = t53 * t76
      t204 = t200 * t201 * t137 * t84
      t207 = t13 * t1 * t3
      t209 = t207 * t19 * t38
      t211 = t14 * t15
      t212 = t16 * t29
      t214 = t211 * t212 * t31
      t215 = 0.74D2 * t214
      t217 = t207 * t44 * t38
      t220 = t83 * t32 * t38
      t223 = t102 * t34
      t224 = 0.5D1 * t223
      t226 = t211 * t212 * t84
      t227 = 0.74D2 * t226
      t228 = t27 * x1
      t229 = t24 * t228
      t230 = t142 * t8
      t231 = t171 * t230
      t233 = 0.16D2 * t229 * t231
      t234 = t156 * t69
      t237 = 0.8D1 * t122 * t112 * t234
      t240 = 0.8D1 * t122 * t164 * t234
      t242 = t175 * t176 * t1
      t246 = 0.768D3 * t242 * t33 * t30 * z
      t247 = t85 * t40
      t248 = t28 * t247
      t250 = t26 * t29
      t251 = t74 * t250
      t252 = t84 * t38
      t253 = t4 * x4
      t255 = t251 * t252 * t253
      t257 = t82 * t184
      t259 = t257 * t171 * t253
      t261 = z * t4
      t262 = t261 * t69
      t265 = 0.384D3 * t147 * t148 * t262
      t266 = t24 * t54
      t267 = t39 * t69
      t270 = 0.16D2 * t266 * t136 * t267
      t271 = t224 + t227 + t233 + t237 + t240 + t246 - 0.24D2 * t248 - 0
     #.205D3 * t255 - 0.67D2 * t259 + t265 + t270
      t275 = t183 * t184 * t137
      t277 = t11 * t176
      t278 = t277 * t228
      t279 = t57 * t230
      t281 = 0.128D3 * t278 * t279
      t282 = t13 * t52
      t283 = t282 * t128
      t284 = t283 * t71
      t287 = t175 * t176 * t25
      t288 = t26 * t45
      t293 = 0.128D3 * t287 * t288 * x4 * x1 * t69
      t296 = t287 * t250 * t185 * t69
      t300 = 0.8D1 * t122 * t164 * t123
      t303 = t13 * t118 * t120 * t76
      t304 = t45 * t113
      t306 = t303 * t57 * t304
      t308 = t118 * t120
      t309 = t277 * t308
      t310 = t76 * x3
      t311 = t46 * t113
      t312 = t310 * t311
      t314 = 0.32D2 * t309 * t312
      t317 = 0.176D3 * t147 * t44 * t230
      t318 = t171 * t143
      t320 = 0.8D1 * t56 * t318
      t321 = t171 * t70
      t322 = t283 * t321
      t324 = t105 * t25
      t325 = t324 * t26
      t326 = t19 * t157
      t328 = 0.6D1 * t325 * t326
      t329 = t175 * t176
      t331 = t329 * t17 * t32
      t333 = t1 * t3
      t335 = t329 * t333 * t253
      t339 = 0.32D2 * t329 * t17 * t157
      t341 = t257 * t57 * t253
      t345 = t183 * t184 * t185 * t8
      t347 = t127 * t53
      t349 = t111 * t156 * t8
      t352 = 0.2D1 * t347 * t44 * t349
      t355 = 0.32D2 * t329 * t17 * t152
      t356 = t277 * t27
      t357 = t19 * t48
      t359 = 0.32D2 * t356 * t357
      t360 = 0.160D3 * t275 - t281 + 0.205D3 * t284 - t293 - 0.488D3 * t
     #296 + t300 - 0.48D2 * t306 - t314 + t317 - t320 + 0.133D3 * t322 -
     # t328 + 0.488D3 * t331 - 0.112D3 * t335 + t339 - 0.148D3 * t341 - 
     #0.86D2 * t345 + t352 + t355 - t359
      t361 = t119 * t120
      t362 = t76 * t30
      t363 = t362 * t304
      t364 = t361 * t363
      t365 = 0.120D3 * t364
      t366 = z ** 2
      t370 = 0.384D3 * t102 * t33 * x3 * t366
      t371 = t39 * t8
      t372 = t85 * t371
      t373 = t356 * t372
      t378 = 0.384D3 * t18 * t44 * t253 * z
      t380 = t329 * t333 * t19
      t384 = 0.2D1 * t347 * t19 * t349
      t385 = t310 * t304
      t386 = t361 * t385
      t387 = 0.120D3 * t386
      t388 = t24 * t25
      t390 = t95 * t77 * t38
      t392 = 0.32D2 * t388 * t390
      t394 = t105 * t15 * t16
      t397 = 0.14D2 * t394 * t44 * t20
      t401 = t105 * t52 * t25 * t53 * t26
      t402 = t29 ** 2
      t405 = t111 * t113 * t69
      t408 = 0.32D2 * t401 * t402 * x3 * t405
      t411 = 0.6D1 * t325 * t158
      t413 = t95 * t89 * t38
      t415 = 0.32D2 * t388 * t413
      t416 = t141 * t137
      t417 = t325 * t416
      t418 = 0.104D3 * t417
      t420 = t329 * t17 * t85
      t424 = 0.14D2 * t394 * t19 * t253
      t425 = t44 * t152
      t427 = 0.6D1 * t325 * t425
      t429 = t111 * t47 * x4
      t432 = 0.2D1 * t347 * t44 * t429
      t433 = t44 * t253
      t435 = 0.14D2 * t394 * t433
      t436 = t29 * t45
      t439 = t436 * t69 * x3 * x4
      t441 = 0.96D2 * t179 * t439
      t442 = x3 * t4
      t445 = t287 * t250 * t442 * t69
      t447 = t277 * t25
      t448 = t447 * t413
      t450 = -t411 - t415 + t418 + 0.488D3 * t420 + t424 - t427 - t432 -
     # t435 + t441 - 0.488D3 * t445 - 0.16D2 * t448
      t456 = 0.2D1 * t347 * t19 * t429
      t460 = 0.32D2 * t401 * t402 * t30 * t405
      t462 = 0.14D2 * t394 * t21
      t464 = 0.6D1 * t325 * t153
      t465 = t325 * t149
      t466 = 0.104D3 * t465
      t467 = t447 * t390
      t469 = t32 * t371
      t470 = t28 * t469
      t473 = 0.8D1 * t56 * t321
      t474 = t46 * t156
      t475 = t19 * t474
      t477 = 0.8D1 * t28 * t475
      t478 = t356 * t247
      t480 = t282 * t201
      t482 = t480 * t252 * t137
      t485 = 0.38D2 * t147 * t326
      t488 = 0.176D3 * t147 * t19 * t230
      t490 = 0.176D3 * t135 * t439
      t492 = t170 * t57 * t157
      t494 = t24 * t308
      t496 = 0.16D2 * t494 * t312
      t497 = t362 * t311
      t499 = 0.16D2 * t494 * t497
      t501 = t170 * t171 * t157
      t504 = 0.128D3 * t278 * t231
      t505 = t57 * t143
      t506 = t283 * t505
      t508 = t456 + t460 - t462 - t464 + t466 - 0.16D2 * t467 - 0.24D2 *
     # t470 + t473 - t477 - 0.112D3 * t478 + 0.96D2 * t482 + t485 + t488
     # - t490 - 0.7D1 * t492 - t496 - t499 + 0.74D2 * t501 - t504 + 0.13
     #3D3 * t506
      t511 = t183 * t184 * t442 * t8
      t513 = t47 * t8
      t516 = 0.6D1 * t195 * t112 * t513
      t519 = t183 * t184 * t442 * x4
      t523 = 0.16D2 * t147 * t85 * t20
      t524 = t113 * t8
      t527 = 0.24D2 * t110 * t112 * t524
      t528 = t64 * t69
      t531 = 0.16D2 * t122 * t164 * t528
      t533 = t257 * t57 * t20
      t536 = 0.128D3 * t179 * t161
      t538 = t251 * t252 * t20
      t540 = t44 * t474
      t542 = 0.32D2 * t356 * t540
      t544 = t76 * t84
      t546 = t135 * t544 * t137
      t547 = 0.80D2 * t546
      t548 = t30 * t45
      t551 = 0.48D2 * t129 * t548 * t131
      t553 = 0.16D2 * t147 * t416
      t555 = t257 * t171 * t20
      t557 = t105 * t1
      t558 = t557 * t36
      t559 = 0.42D2 * t558
      t560 = t557 * t34
      t561 = 0.42D2 * t560
      t565 = 0.96D2 * t183 * t16 * t45 * t64
      t568 = 0.6D1 * t195 * t164 * t513
      t571 = t436 * t69 * t30 * t8
      t573 = 0.96D2 * t179 * t571
      t575 = t251 * t171 * t137
      t577 = t324 * t169
      t580 = 0.12D2 * t577 * t130 * t64
      t581 = -t547 + t551 - t553 - 0.148D3 * t555 - t559 - t561 + t565 -
     # t568 + t573 - 0.133D3 * t575 + t580
      t585 = t303 * t171 * t304
      t591 = 0.128D3 * t287 * t288 * t8 * x1 * t69
      t592 = t31 * t38
      t594 = t251 * t592 * t253
      t598 = 0.16D2 * t266 * t544 * t267
      t600 = 0.16D2 * t229 * t279
      t601 = t170 * t231
      t605 = 0.24D2 * t110 * t164 * t524
      t609 = 0.384D3 * t18 * t19 * t261 * t8
      t610 = t14 * t308
      t612 = 0.38D2 * t610 * t363
      t615 = 0.12D2 * t577 * t548 * t64
      t618 = 0.6D1 * t195 * t112 * t196
      t621 = 0.16D2 * t122 * t112 * t528
      t623 = t170 * t57 * t152
      t626 = 0.8D1 * t56 * t505
      t627 = t356 * t41
      t631 = t200 * t201 * t137 * t31
      t635 = 0.384D3 * t147 * t141 * t262
      t636 = t28 * t372
      t639 = 0.8D1 * t28 * t540
      t643 = 0.768D3 * t242 * t33 * x3 * z
      t644 = -0.48D2 * t585 - t591 - 0.133D3 * t594 + t598 + t600 + 0.67
     #D2 * t601 - t605 - t609 + t612 + t615 - t618 - t621 + 0.74D2 * t62
     #3 + t626 - 0.160D3 * t627 + 0.112D3 * t631 + t635 + 0.24D2 * t636 
     #- t639 + t643
      t646 = 0.38D2 * t147 * t425
      t649 = 0.32D2 * t200 * t128 * t304
      t651 = 0.8D1 * t28 * t357
      t652 = t283 * t318
      t656 = 0.38D2 * t610 * t385
      t658 = t251 * t592 * t20
      t661 = t251 * t57 * t137
      t664 = t480 * t592 * t137
      t668 = 0.48D2 * t129 * t548 * t190
      t670 = t170 * t279
      t673 = 0.32D2 * t309 * t497
      t675 = 0.176D3 * t135 * t571
      t677 = t329 * t333 * t44
      t682 = 0.384D3 * t102 * t33 * t30 * t366
      t684 = t329 * t333 * t20
      t688 = 0.16D2 * t147 * t32 * t253
      t690 = 0.32D2 * t356 * t49
      t691 = t356 * t469
      t694 = 0.32D2 * t356 * t475
      t695 = t18 * t433
      t696 = 0.80D2 * t695
      t697 = 0.67D2 * t670 - t673 - t675 - 0.488D3 * t677 - t682 - 0.112
     #D3 * t684 - t688 - t690 - 0.112D3 * t691 - t694 - t696
      t708 = t23 - 0.48D2 * t42 + t51 + t73 + 0.120D3 * t80 - 0.30D2 * t
     #87 + 0.120D3 * t92 - t98 - t101 - t104 + t117 - t126 + t134 + t140
     # - t146 + t151 - t155 - t160 - t163 - t167
      t719 = -t224 - t227 - t233 - t237 - t240 - t246 + 0.48D2 * t248 + 
     #0.102D3 * t255 - 0.12D2 * t259 - t265 - t270
      t731 = -0.176D3 * t275 + t281 - 0.102D3 * t284 + t293 + 0.597D3 * 
     #t296 - t300 + 0.120D3 * t306 + t314 - t317 + t320 - 0.30D2 * t322 
     #+ t328 - 0.597D3 * t331 + 0.128D3 * t335 - t339 + 0.80D2 * t341 + 
     #0.176D3 * t345 - t352 - t355 + t359
      t738 = t411 + t415 - t418 - 0.597D3 * t420 - t424 + t427 + t432 + 
     #t435 - t441 + 0.597D3 * t445 + 0.32D2 * t448
      t749 = -t456 - t460 + t462 + t464 - t466 + 0.32D2 * t467 + 0.48D2 
     #* t470 - t473 + t477 + 0.128D3 * t478 - 0.240D3 * t482 - t485 - t4
     #88 + t490 + 0.52D2 * t492 + t496 + t499 - 0.40D2 * t501 + t504 - 0
     #.30D2 * t506
      t757 = t547 - t551 + t553 + 0.80D2 * t555 + t559 + t561 - t565 + t
     #568 - t573 + 0.30D2 * t575 - t580
      t767 = 0.120D3 * t585 + t591 + 0.30D2 * t594 - t598 - t600 + 0.12D
     #2 * t601 + t605 + t609 - t612 - t615 + t618 + t621 - 0.40D2 * t623
     # - t626 + 0.176D3 * t627 - 0.128D3 * t631 - t635 - 0.48D2 * t636 +
     # t639 - t643
      t778 = 0.12D2 * t670 + t673 + t675 + 0.597D3 * t677 + t682 + 0.128
     #D3 * t684 + t688 + t690 + 0.128D3 * t691 + t694 + t696
      t797 = -0.16D2 * t22 + 0.64D2 * t87 - 0.32D2 * t97 - 0.32D2 * t100
     # - 0.32D2 * t103 - 0.16D2 * t139 - t151 - 0.80D2 * t173 + 0.48D2 *
     # t188 + 0.64D2 * t209 + 0.64D2 * t214 + 0.64D2 * t217 + 0.64D2 * t
     #220
      t812 = -0.32D2 * t223 + 0.64D2 * t226 - 0.48D2 * t255 - 0.16D2 * t
     #259 + 0.48D2 * t284 - 0.48D2 * t296 + 0.64D2 * t322 + 0.48D2 * t33
     #1 - 0.256D3 * t341 - 0.160D3 * t345 - 0.32D2 * t364 - 0.48D2 * t38
     #0 - 0.32D2 * t386 + 0.64D2 * t417
      t825 = 0.48D2 * t420 - 0.48D2 * t445 + 0.64D2 * t465 - 0.80D2 * t4
     #92 + 0.64D2 * t501 + 0.64D2 * t506 + 0.48D2 * t511 - 0.160D3 * t51
     #9 - t523 - 0.16D2 * t533 - 0.32D2 * t538 - 0.16D2 * t546 - t553
      t839 = -0.256D3 * t555 - 0.32D2 * t558 - 0.32D2 * t560 - 0.32D2 * 
     #t575 - 0.32D2 * t594 + 0.16D2 * t601 + 0.64D2 * t623 + 0.48D2 * t6
     #52 - 0.48D2 * t658 - 0.32D2 * t661 + 0.16D2 * t670 - 0.48D2 * t677
     # - t688 - 0.16D2 * t695
      rrgg2ggh21J3 = 0.9D1 / 0.16D2 * (0.3D1 * wd * (t168 + t697 - 0.133
     #D3 * t538 - t542 - t531 - 0.67D2 * t533 + t536 - t668 + 0.74D2 * t
     #217 + 0.133D3 * t220 - 0.133D3 * t661 + 0.96D2 * t664 + t527 + 0.1
     #12D3 * t204 + t199 + t360 - 0.86D2 * t519 - t523 - t384 - t387 + t
     #516 + 0.488D3 * t188 + t193 + t181 + t644 + t397 - 0.7D1 * t173 + 
     #0.488D3 * t511 + 0.16D2 * t329 + t271 - t378 - 0.488D3 * t380 + 0.
     #205D3 * t652 + t656 - 0.205D3 * t658 - t392 + t450 + t408 + t508 +
     # t646 + t649 - t651 - 0.160D3 * t373 + t581 - t365 - t370 + 0.74D2
     # * t209 + t215) + 0.2D1 * wd * (0.30D2 * t538 + t542 + t719 + t531
     # - 0.12D2 * t533 - t536 + t668 - 0.40D2 * t217 - 0.30D2 * t220 + 0
     #.30D2 * t661 - 0.240D3 * t664 + t708 - t527 - 0.128D3 * t204 + t74
     #9 - t199 + 0.176D3 * t519 + t523 + t384 + t387 + t757 - t516 - 0.5
     #97D3 * t188 - t193 - t181 + t738 - t397 + 0.52D2 * t173 - 0.597D3 
     #* t511 - 0.32D2 * t329 + t778 + t378 + 0.597D3 * t380 + t767 - 0.1
     #02D3 * t652 - t656 + 0.102D3 * t658 + t392 + t731 - t408 - t646 - 
     #t649 + t651 + 0.176D3 * t373 + t365 + t370 - 0.40D2 * t209 - t215)
     # + wd * (t797 + t812 + t825 + t839)) / t11 / t12 / t38 / z / 0.314
     #1592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh21J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t15 * t1
      t17 = t3 ** 2
      t18 = t17 * t3
      t19 = t16 * t18
      t20 = t14 * t19
      t21 = 0.1D1 - x3
      t22 = x1 * t21
      t23 = t4 ** 2
      t24 = t23 * x4
      t25 = t24 * t8
      t28 = 0.176D3 * t20 * t22 * t25
      t29 = t15 * t17
      t30 = t14 * t29
      t31 = t4 * x4
      t32 = t22 * t31
      t33 = t30 * t32
      t34 = 0.80D2 * t33
      t35 = x1 ** 2
      t36 = t21 ** 2
      t37 = t35 * t36
      t40 = 0.16D2 * t20 * t37 * t31
      t41 = t12 * s
      t42 = t11 * t41
      t43 = t42 * t19
      t44 = t3 * x1
      t45 = t44 * x3
      t47 = t44 * t21
      t49 = s - t2 * t45 - t2 * t47
      t50 = t49 * t23
      t51 = x4 ** 2
      t52 = t50 * t51
      t53 = t22 * t52
      t55 = 0.32D2 * t43 * t53
      t56 = t49 * t11
      t58 = t56 * t41 * t1
      t62 = 0.768D3 * t58 * t44 * x3 * z
      t63 = t35 * x3
      t64 = z * t4
      t68 = cos(x2 * 0.3141592653589793D1)
      t70 = x4 * t8
      t72 = Sqrt(x3 * t21 * t70)
      t75 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t68 * t72
      t76 = t64 * t75
      t79 = 0.384D3 * t20 * t63 * t76
      t80 = t13 * z
      t81 = t15 ** 2
      t82 = t17 ** 2
      t83 = t81 * t82
      t84 = t80 * t83
      t85 = t35 * x1
      t86 = t85 * t36
      t87 = t49 * t4
      t88 = t87 * t75
      t91 = 0.16D2 * t84 * t86 * t88
      t92 = t14 * t1
      t93 = t92 * t45
      t94 = 0.5D1 * t93
      t95 = t14 * t15
      t96 = t17 * t35
      t97 = x3 ** 2
      t99 = t95 * t96 * t97
      t100 = 0.74D2 * t99
      t102 = t13 * t1 * t3
      t103 = x1 * x3
      t105 = t102 * t103 * t49
      t108 = t95 * t96 * t36
      t109 = 0.74D2 * t108
      t111 = t102 * t22 * t49
      t113 = t13 * t15
      t114 = t113 * t17
      t116 = t114 * t37 * t49
      t118 = t13 * t16
      t119 = t118 * t18
      t120 = t36 * t21
      t123 = t119 * t85 * t120 * t49
      t125 = t35 * t97
      t127 = t114 * t125 * t49
      t129 = t97 * x3
      t132 = t119 * t85 * t129 * t49
      t134 = t14 * t16
      t135 = t18 * t85
      t137 = t134 * t135 * t120
      t138 = 0.5D1 * t137
      t140 = t134 * t135 * t129
      t141 = 0.5D1 * t140
      t142 = t92 * t47
      t143 = 0.5D1 * t142
      t144 = t23 * t51
      t145 = t22 * t144
      t147 = 0.38D2 * t20 * t145
      t148 = t28 - t34 - t40 - t55 + t62 + t79 + t91 + t94 + t100 + 0.74
     #D2 * t105 + t109 + 0.74D2 * t111 + 0.133D3 * t116 - 0.48D2 * t123 
     #+ 0.133D3 * t127 - 0.48D2 * t132 + t138 + t141 + t143 + t147
      t149 = t41 * t81
      t150 = t56 * t149
      t151 = t82 * t35
      t152 = t75 ** 2
      t153 = t23 * t152
      t156 = 0.32D2 * t150 * t151 * t153
      t157 = t4 * t8
      t158 = t103 * t157
      t159 = t30 * t158
      t160 = 0.80D2 * t159
      t164 = 0.384D3 * t30 * t103 * t64 * t8
      t165 = t81 * t1
      t166 = t82 * t3
      t167 = t165 * t166
      t168 = t14 * t167
      t169 = t85 * t21
      t170 = t169 * t153
      t172 = 0.38D2 * t168 * t170
      t174 = t56 * t41 * t16
      t175 = t18 * t35
      t176 = x3 * t4
      t179 = t174 * t175 * t176 * t75
      t181 = t87 * t8
      t182 = t37 * t181
      t183 = t43 * t182
      t185 = t21 * t4
      t188 = t174 * t175 * t185 * t75
      t190 = t125 * t181
      t191 = t43 * t190
      t196 = 0.384D3 * t30 * t22 * t31 * z
      t197 = t19 * x1
      t198 = t80 * t197
      t199 = x3 * t49
      t200 = t199 * t25
      t202 = 0.16D2 * t198 * t200
      t204 = t80 * t19
      t205 = t204 * t182
      t208 = t80 * t83 * t35
      t209 = t21 * t49
      t211 = t23 * t8 * t75
      t212 = t209 * t211
      t214 = 0.8D1 * t208 * t212
      t215 = t42 * t197
      t216 = t209 * t25
      t218 = 0.128D3 * t215 * t216
      t219 = t13 * t81
      t220 = t219 * t151
      t221 = t220 * t212
      t223 = t24 * t75
      t224 = t199 * t223
      t225 = t220 * t224
      t227 = t13 * s
      t232 = t227 * t81 * t15 * t82 * t17 * t85
      t233 = t23 * t4
      t234 = t21 * t233
      t235 = x4 * t152
      t238 = 0.24D2 * t232 * t234 * t235
      t239 = t227 * t165
      t241 = t239 * t166 * t35
      t242 = t51 * t75
      t245 = 0.8D1 * t241 * t234 * t242
      t246 = t152 * t8
      t249 = 0.24D2 * t232 * t234 * t246
      t250 = x3 * t233
      t251 = t70 * t75
      t254 = 0.16D2 * t241 * t250 * t251
      t255 = t17 * x1
      t256 = t113 * t255
      t258 = t256 * t209 * t157
      t261 = t56 * t149 * t82
      t262 = t35 * t23
      t263 = t75 * x3
      t265 = t262 * t263 * t8
      t267 = 0.128D3 * t261 * t265
      t268 = -0.24D2 * t205 - t214 - t218 + 0.205D3 * t221 + 0.205D3 * t
     #225 - t238 + t245 + t249 - t254 - 0.67D2 * t258 + t267
      t271 = t118 * t175
      t272 = t97 * t49
      t274 = t271 * t272 * t157
      t276 = t8 ** 2
      t277 = t50 * t276
      t278 = t22 * t277
      t280 = 0.32D2 * t43 * t278
      t281 = t227 * t16
      t282 = t18 * x1
      t283 = t281 * t282
      t284 = x3 * t23
      t287 = 0.12D2 * t283 * t284 * t70
      t290 = t13 * t165 * t166 * t85
      t292 = t290 * t199 * t153
      t294 = t87 * x4
      t295 = t37 * t294
      t296 = t43 * t295
      t298 = t82 * t85
      t299 = t4 * t75
      t302 = t150 * t298 * t299 * t36
      t304 = t14 * t83
      t306 = 0.16D2 * t304 * t265
      t307 = t118 * t282
      t308 = t307 * t216
      t312 = 0.8D1 * t241 * t250 * t242
      t313 = t80 * t167
      t314 = t85 * x3
      t315 = t50 * t152
      t316 = t314 * t315
      t318 = 0.16D2 * t313 * t316
      t319 = t169 * t315
      t321 = 0.16D2 * t313 * t319
      t322 = t42 * t167
      t324 = 0.32D2 * t322 * t316
      t325 = t56 * t41
      t326 = t1 * t3
      t328 = t325 * t326 * t157
      t330 = z ** 2
      t334 = 0.384D3 * t92 * t44 * t21 * t330
      t336 = t325 * t326 * t22
      t339 = t325 * t29 * t37
      t341 = t281 * t18
      t342 = t23 * t276
      t343 = t103 * t342
      t345 = 0.6D1 * t341 * t343
      t348 = 0.32D2 * t325 * t29 * t342
      t350 = t325 * t326 * t31
      t352 = t227 * t81
      t353 = t352 * t82
      t355 = t233 * t276 * t8
      t358 = 0.2D1 * t353 * t22 * t355
      t359 = -0.133D3 * t274 - t280 + t287 - 0.48D2 * t292 - 0.160D3 * t
     #296 + 0.112D3 * t302 + t306 + 0.67D2 * t308 + t312 - t318 - t321 -
     # t324 - 0.112D3 * t328 - t334 - 0.488D3 * t336 + 0.488D3 * t339 - 
     #t345 + t348 - 0.112D3 * t350 + t358
      t362 = 0.32D2 * t325 * t29 * t144
      t366 = 0.384D3 * t92 * t44 * x3 * t330
      t367 = t239 * t166
      t368 = t367 * t170
      t369 = 0.120D3 * t368
      t373 = t227 * t81 * t16 * t82 * t18
      t374 = t35 ** 2
      t377 = t233 * t152 * t75
      t380 = 0.32D2 * t373 * t374 * x3 * t377
      t382 = t227 * t15 * t17
      t385 = 0.14D2 * t382 * t22 * t157
      t386 = t80 * t16
      t388 = t135 * t120 * t49
      t390 = 0.32D2 * t386 * t388
      t391 = t314 * t153
      t392 = t367 * t391
      t393 = 0.120D3 * t392
      t396 = 0.2D1 * t353 * t103 * t355
      t398 = t325 * t326 * t103
      t400 = t22 * t342
      t402 = 0.6D1 * t341 * t400
      t405 = t135 * t129 * t49
      t407 = 0.32D2 * t386 * t405
      t409 = t233 * t51 * x4
      t412 = 0.2D1 * t353 * t22 * t409
      t414 = 0.6D1 * t341 * t145
      t417 = 0.14D2 * t382 * t103 * t31
      t419 = t325 * t29 * t125
      t421 = t35 * t21
      t422 = t421 * t299
      t423 = t341 * t422
      t424 = 0.104D3 * t423
      t426 = 0.14D2 * t382 * t32
      t427 = t42 * t16
      t428 = t427 * t405
      t430 = t427 * t388
      t432 = t63 * t299
      t433 = t341 * t432
      t434 = 0.104D3 * t433
      t435 = t103 * t144
      t437 = 0.6D1 * t341 * t435
      t438 = -t407 - t412 - t414 + t417 + 0.488D3 * t419 + t424 - t426 -
     # 0.16D2 * t428 - 0.16D2 * t430 + t434 - t437
      t443 = 0.14D2 * t382 * t158
      t447 = 0.32D2 * t373 * t374 * t21 * t377
      t450 = 0.2D1 * t353 * t103 * t409
      t452 = t352 * t82 * x1
      t453 = x4 * t276
      t456 = 0.6D1 * t452 * t234 * t453
      t459 = 0.16D2 * t241 * t234 * t251
      t461 = 0.8D1 * t208 * t224
      t465 = 0.768D3 * t58 * t44 * t21 * z
      t466 = t125 * t294
      t467 = t204 * t466
      t470 = t271 * t272 * t31
      t473 = t256 * t199 * t31
      t476 = t56 * t41 * t15
      t480 = 0.96D2 * t476 * t17 * t23 * t70
      t481 = t51 * t8
      t484 = 0.6D1 * t452 * t250 * t481
      t486 = 0.8D1 * t204 * t53
      t488 = t307 * t209 * t144
      t490 = t209 * t223
      t492 = 0.8D1 * t208 * t490
      t493 = t103 * t52
      t495 = 0.32D2 * t43 * t493
      t496 = t219 * t298
      t498 = t496 * t272 * t299
      t501 = 0.32D2 * t322 * t319
      t502 = t421 * t211
      t504 = 0.176D3 * t304 * t502
      t506 = 0.8D1 * t204 * t493
      t507 = -t443 + t447 + t450 - t456 - t459 - t461 + t465 - 0.24D2 * 
     #t467 - 0.205D3 * t470 - 0.67D2 * t473 + t480 - t484 - t486 + 0.74D
     #2 * t488 + t492 - t495 + 0.96D2 * t498 - t501 - t504 - t506
      t508 = t204 * t295
      t511 = 0.96D2 * t261 * t502
      t513 = t271 * t199 * t299
      t515 = t18 * t23
      t520 = 0.128D3 * t174 * t515 * x4 * x1 * t75
      t521 = t199 * t211
      t522 = t220 * t521
      t524 = t276 * t75
      t527 = 0.8D1 * t241 * t234 * t524
      t530 = 0.8D1 * t241 * t250 * t524
      t532 = 0.8D1 * t208 * t521
      t533 = t103 * t277
      t535 = 0.8D1 * t204 * t533
      t536 = t43 * t466
      t540 = 0.260D3 * t20 * t400
      t542 = t262 * t263 * x4
      t544 = 0.96D2 * t261 * t542
      t549 = 0.128D3 * t174 * t515 * t8 * x1 * t75
      t550 = t36 * t49
      t552 = t271 * t550 * t31
      t555 = t256 * t209 * t31
      t559 = t476 * t255 * t185 * t8
      t562 = t307 * t199 * t144
      t565 = t307 * t199 * t342
      t568 = 0.128D3 * t215 * t200
      t569 = t220 * t490
      t572 = t290 * t209 * t153
      t574 = t540 + t544 - t549 - 0.133D3 * t552 - 0.148D3 * t555 - 0.86
     #D2 * t559 - 0.7D1 * t562 + 0.74D2 * t565 - t568 + 0.133D3 * t569 -
     # 0.48D2 * t572
      t577 = t85 * t97
      t580 = 0.16D2 * t84 * t577 * t88
      t582 = 0.16D2 * t198 * t216
      t584 = 0.38D2 * t20 * t343
      t587 = 0.176D3 * t20 * t103 * t25
      t589 = 0.176D3 * t304 * t542
      t591 = t307 * t209 * t342
      t595 = 0.16D2 * t20 * t125 * t157
      t597 = t476 * t255 * t299
      t601 = t476 * t255 * t176 * t8
      t605 = 0.6D1 * t452 * t234 * t481
      t606 = t352 * t151
      t607 = t75 * t8
      t610 = 0.48D2 * t606 * t284 * t607
      t613 = 0.6D1 * t452 * t250 * t453
      t616 = t150 * t298 * t299 * t97
      t620 = 0.24D2 * t232 * t250 * t235
      t622 = 0.38D2 * t168 * t391
      t624 = t271 * t550 * t157
      t627 = t271 * t209 * t299
      t630 = t496 * t550 * t299
      t632 = t75 * x4
      t634 = t262 * t632 * t21
      t636 = 0.128D3 * t261 * t634
      t639 = t476 * t255 * t185 * x4
      t641 = t580 + t582 + t584 + t587 - t589 - 0.7D1 * t591 - t595 + 0.
     #160D3 * t597 + 0.488D3 * t601 + t605 + t610 + t613 + 0.112D3 * t61
     #6 + t620 + t622 - 0.205D3 * t624 - 0.133D3 * t627 + 0.96D2 * t630 
     #+ t636 + 0.488D3 * t639
      t644 = t476 * t255 * t176 * x4
      t647 = t304 * t577 * t299
      t648 = 0.80D2 * t647
      t649 = t21 * t23
      t652 = 0.48D2 * t606 * t649 * t632
      t654 = 0.16D2 * t20 * t422
      t656 = t256 * t199 * t157
      t658 = t307 * t200
      t662 = 0.24D2 * t232 * t250 * t246
      t665 = 0.12D2 * t283 * t649 * t70
      t668 = 0.48D2 * t606 * t284 * t632
      t670 = t304 * t86 * t299
      t671 = 0.80D2 * t670
      t674 = 0.16D2 * t304 * t634
      t676 = 0.16D2 * t20 * t432
      t678 = 0.260D3 * t20 * t435
      t681 = 0.384D3 * t20 * t421 * t76
      t682 = t204 * t190
      t685 = 0.8D1 * t204 * t278
      t686 = t227 * t1
      t687 = t686 * t47
      t688 = 0.42D2 * t687
      t689 = t686 * t45
      t690 = 0.42D2 * t689
      t693 = 0.32D2 * t43 * t533
      t696 = 0.48D2 * t606 * t649 * t607
      t697 = t674 - t676 + t678 + t681 + 0.24D2 * t682 - t685 - t688 - t
     #690 + 0.16D2 * t325 - t693 - t696
      t710 = -t28 + t34 + t40 + t55 - t62 - t79 - t91 - t94 - t100 - 0.4
     #0D2 * t105 - t109 - 0.40D2 * t111 - 0.30D2 * t116 + 0.120D3 * t123
     # - 0.30D2 * t127 + 0.120D3 * t132 - t138 - t141 - t143 - t147
      t720 = 0.48D2 * t205 + t214 + t218 - 0.102D3 * t221 - 0.102D3 * t2
     #25 + t238 - t245 - t249 + t254 - 0.12D2 * t258 - t267
      t732 = 0.30D2 * t274 + t280 - t287 + 0.120D3 * t292 + 0.176D3 * t2
     #96 - 0.128D3 * t302 - t306 + 0.12D2 * t308 - t312 + t318 + t321 + 
     #t324 + 0.128D3 * t328 + t334 + 0.597D3 * t336 - 0.597D3 * t339 + t
     #345 - t348 + 0.128D3 * t350 - t358
      t738 = t407 + t412 + t414 - t417 - 0.597D3 * t419 - t424 + t426 + 
     #0.32D2 * t428 + 0.32D2 * t430 - t434 + t437
      t747 = t443 - t447 - t450 + t456 + t459 + t461 - t465 + 0.48D2 * t
     #467 + 0.102D3 * t470 - 0.12D2 * t473 - t480 + t484 + t486 - 0.40D2
     # * t488 - t492 + t495 - 0.240D3 * t498 + t501 + t504 + t506
      t760 = -t540 - t544 + t549 + 0.30D2 * t552 + 0.80D2 * t555 + 0.176
     #D3 * t559 + 0.52D2 * t562 - 0.40D2 * t565 + t568 - 0.30D2 * t569 +
     # 0.120D3 * t572
      t771 = -t580 - t582 - t584 - t587 + t589 + 0.52D2 * t591 + t595 - 
     #0.176D3 * t597 - 0.597D3 * t601 - t605 - t610 - t613 - 0.128D3 * t
     #616 - t620 - t622 + 0.102D3 * t624 + 0.30D2 * t627 - 0.240D3 * t63
     #0 - t636 - 0.597D3 * t639
      t778 = -t674 + t676 - t678 - t681 - 0.48D2 * t682 + t685 + t688 + 
     #t690 - 0.32D2 * t325 + t693 + t696
      t797 = -0.16D2 * t33 - t40 - 0.32D2 * t93 + 0.64D2 * t99 + 0.64D2 
     #* t105 + 0.64D2 * t108 + 0.64D2 * t111 + 0.64D2 * t116 + 0.64D2 * 
     #t127 - 0.32D2 * t137 - 0.32D2 * t140 - 0.32D2 * t142 - 0.16D2 * t1
     #59
      t812 = -0.48D2 * t179 - 0.48D2 * t188 + 0.48D2 * t221 + 0.48D2 * t
     #225 - 0.16D2 * t258 - 0.32D2 * t274 + 0.16D2 * t308 - 0.48D2 * t33
     #6 + 0.48D2 * t339 - 0.32D2 * t368 - 0.32D2 * t392 - 0.48D2 * t398 
     #+ 0.48D2 * t419 + 0.64D2 * t423
      t827 = 0.64D2 * t433 - 0.48D2 * t470 - 0.16D2 * t473 + 0.64D2 * t4
     #88 - 0.32D2 * t513 + 0.64D2 * t522 - 0.32D2 * t552 - 0.256D3 * t55
     #5 - 0.160D3 * t559 - 0.80D2 * t562 + 0.64D2 * t565 + 0.64D2 * t569
     # - 0.80D2 * t591
      t839 = -t595 + 0.48D2 * t601 - 0.48D2 * t624 - 0.32D2 * t627 + 0.4
     #8D2 * t639 - 0.160D3 * t644 - 0.16D2 * t647 - t654 - 0.256D3 * t65
     #6 + 0.16D2 * t658 - 0.16D2 * t670 - t676 - 0.32D2 * t687 - 0.32D2 
     #* t689
      rrgg2ggh21J4 = 0.9D1 / 0.16D2 * (0.4D1 * wd * (-t164 + t697 - t160
     # - 0.112D3 * t183 + t532 - t535 - 0.112D3 * t536 - t668 - t671 - t
     #662 + t202 + t665 + t527 + t530 - t196 + t359 + t362 - t520 + 0.13
     #3D3 * t522 + t385 + t574 - 0.133D3 * t513 - 0.488D3 * t188 - 0.160
     #D3 * t191 - 0.488D3 * t179 + t641 - 0.86D2 * t644 - t396 - 0.488D3
     # * t398 + t172 + t156 + t511 + t268 + t380 + t148 + t652 - t654 - 
     #0.148D3 * t656 + 0.67D2 * t658 - t390 - t393 - t402 + t507 + 0.24D
     #2 * t508 - t648 - t366 - t369 + t438) + 0.3D1 * wd * (t164 + t720 
     #+ t160 + 0.128D3 * t183 - t532 + t535 + 0.128D3 * t536 + t668 + t6
     #71 + t662 - t202 - t665 + t710 - t527 - t530 + t747 + t196 - t362 
     #+ t520 - 0.30D2 * t522 - t385 + t760 + 0.30D2 * t513 + 0.597D3 * t
     #188 + 0.176D3 * t191 + 0.597D3 * t179 + t738 + 0.176D3 * t644 + t3
     #96 + 0.597D3 * t398 - t172 - t156 - t511 + t778 - t380 + t771 - t6
     #52 + t654 + 0.80D2 * t656 + 0.12D2 * t658 + t390 + t393 + t732 + t
     #402 - 0.48D2 * t508 + t648 + t366 + t369) + 0.2D1 * wd * (t797 + t
     #812 + t827 + t839)) / t11 / t12 / t49 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh21J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = 0.1D1 - x1
      t5 = t3 * t4
      t8 = 0.1D1 - x4
      t11 = s - t2 * t5 * x4 - t2 * t5 * t8
      t12 = s ** 2
      t13 = t12 ** 2
      t14 = t11 * t13
      t15 = t1 ** 2
      t16 = t3 ** 2
      t17 = t15 * t16
      t18 = t14 * t17
      t19 = 0.1D1 - x3
      t20 = t19 * x1
      t21 = t4 * x4
      t25 = 0.384D3 * t18 * t20 * t21 * z
      t26 = t13 * s
      t27 = t15 * t1
      t28 = t15 ** 2
      t31 = t16 * t3
      t32 = t16 ** 2
      t34 = t26 * t28 * t27 * t32 * t31
      t35 = x1 ** 2
      t36 = t35 ** 2
      t38 = t4 ** 2
      t39 = t38 * t4
      t43 = cos(x2 * 0.3141592653589793D1)
      t45 = x4 * t8
      t47 = Sqrt(x3 * t19 * t45)
      t50 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t43 * t47
      t51 = t50 ** 2
      t53 = t39 * t51 * t50
      t56 = 0.32D2 * t34 * t36 * x3 * t53
      t58 = t26 * t15 * t16
      t59 = t4 * t8
      t62 = 0.14D2 * t58 * t20 * t59
      t63 = t13 * z
      t64 = t63 * t27
      t65 = t35 * x1
      t66 = t31 * t65
      t67 = t19 ** 2
      t68 = t67 * t19
      t69 = t3 * x1
      t70 = t69 * x3
      t72 = t69 * t19
      t74 = s - t2 * t70 - t2 * t72
      t76 = t66 * t68 * t74
      t78 = 0.32D2 * t64 * t76
      t79 = t28 * t1
      t80 = t26 * t79
      t81 = t32 * t3
      t82 = t80 * t81
      t83 = t65 * x3
      t84 = t38 * t51
      t85 = t83 * t84
      t86 = t82 * t85
      t87 = 0.120D3 * t86
      t88 = t26 * t28
      t89 = t88 * t32
      t90 = x1 * x3
      t91 = t8 ** 2
      t93 = t39 * t91 * t8
      t96 = 0.2D1 * t89 * t90 * t93
      t97 = t74 * t11
      t98 = t12 * s
      t99 = t97 * t98
      t100 = t1 * t3
      t102 = t99 * t100 * t90
      t104 = t26 * t27
      t105 = t104 * t31
      t106 = t38 * t91
      t107 = t20 * t106
      t109 = 0.6D1 * t105 * t107
      t110 = x3 ** 2
      t111 = t110 * x3
      t113 = t66 * t111 * t74
      t115 = 0.32D2 * t64 * t113
      t116 = x4 ** 2
      t118 = t39 * t116 * x4
      t121 = 0.2D1 * t89 * t20 * t118
      t122 = t38 * t116
      t123 = t20 * t122
      t125 = 0.6D1 * t105 * t123
      t128 = 0.14D2 * t58 * t90 * t21
      t129 = t35 * t110
      t131 = t99 * t17 * t129
      t133 = t35 * t19
      t134 = t4 * t50
      t135 = t133 * t134
      t136 = t105 * t135
      t137 = 0.104D3 * t136
      t138 = t20 * t21
      t140 = 0.14D2 * t58 * t138
      t141 = t11 * t98
      t142 = t141 * t27
      t143 = t142 * t113
      t145 = t142 * t76
      t147 = t35 * x3
      t148 = t147 * t134
      t149 = t105 * t148
      t150 = 0.104D3 * t149
      t151 = t90 * t122
      t153 = 0.6D1 * t105 * t151
      t154 = t90 * t59
      t156 = 0.14D2 * t58 * t154
      t157 = -t25 + t56 + t62 - t78 - t87 - t96 - 0.488D3 * t102 - t109 
     #- t115 - t121 - t125 + t128 + 0.488D3 * t131 + t137 - t140 - 0.16D
     #2 * t143 - 0.16D2 * t145 + t150 - t153 - t156
      t161 = 0.32D2 * t34 * t36 * t19 * t53
      t164 = 0.2D1 * t89 * t90 * t118
      t165 = t27 * t31
      t166 = t141 * t165
      t167 = t74 * t4
      t168 = t167 * x4
      t169 = t129 * t168
      t170 = t166 * t169
      t172 = t14 * t165
      t174 = 0.16D2 * t172 * t135
      t175 = t13 * t15
      t176 = t16 * x1
      t177 = t175 * t176
      t178 = x3 * t74
      t180 = t177 * t178 * t59
      t182 = t13 * t27
      t183 = t31 * x1
      t184 = t182 * t183
      t185 = t38 * x4
      t186 = t185 * t8
      t187 = t178 * t186
      t188 = t184 * t187
      t194 = t26 * t28 * t15 * t32 * t16 * t65
      t195 = x3 * t39
      t196 = t51 * t8
      t199 = 0.24D2 * t194 * t195 * t196
      t200 = z * t4
      t204 = 0.384D3 * t18 * t90 * t200 * t8
      t205 = t14 * t1
      t206 = t205 * t70
      t207 = 0.5D1 * t206
      t208 = t14 * t15
      t209 = t16 * t35
      t211 = t208 * t209 * t110
      t212 = 0.74D2 * t211
      t215 = t13 * t1 * t3
      t217 = t215 * t90 * t74
      t220 = t208 * t209 * t67
      t221 = 0.74D2 * t220
      t223 = t215 * t20 * t74
      t225 = t13 * t28
      t226 = t32 * t65
      t227 = t225 * t226
      t228 = t110 * t74
      t230 = t227 * t228 * t134
      t232 = t90 * t106
      t234 = 0.38D2 * t172 * t232
      t237 = 0.176D3 * t172 * t90 * t186
      t238 = t28 * t32
      t239 = t14 * t238
      t240 = t185 * t50
      t241 = t147 * t240
      t243 = 0.176D3 * t239 * t241
      t245 = t63 * t238 * t35
      t246 = t19 * t74
      t247 = t246 * t240
      t249 = 0.8D1 * t245 * t247
      t251 = t99 * t100 * t59
      t253 = z ** 2
      t257 = 0.384D3 * t205 * t69 * t19 * t253
      t259 = t99 * t100 * t20
      t261 = 0.74D2 * t217 + t221 + 0.74D2 * t223 + 0.96D2 * t230 + t234
     # + t237 - t243 + t249 - 0.112D3 * t251 - t257 - 0.488D3 * t259
      t264 = t35 * t67
      t266 = t99 * t17 * t264
      t269 = 0.6D1 * t105 * t232
      t272 = 0.32D2 * t99 * t17 * t106
      t274 = t99 * t100 * t21
      t278 = 0.2D1 * t89 * t20 * t93
      t281 = 0.32D2 * t99 * t17 * t122
      t285 = 0.384D3 * t205 * t69 * x3 * t253
      t286 = t65 * t19
      t287 = t286 * t84
      t288 = t82 * t287
      t289 = 0.120D3 * t288
      t291 = t97 * t98 * t15
      t292 = x3 * t4
      t295 = t291 * t176 * t292 * x4
      t297 = t65 * t110
      t299 = t239 * t297 * t134
      t300 = 0.80D2 * t299
      t301 = t32 * t35
      t302 = t88 * t301
      t303 = t19 * t38
      t304 = x4 * t50
      t307 = 0.48D2 * t302 * t303 * t304
      t308 = t79 * t81
      t309 = t14 * t308
      t311 = 0.38D2 * t309 * t287
      t313 = 0.38D2 * t172 * t123
      t314 = t98 * t28
      t315 = t97 * t314
      t318 = t315 * t226 * t134 * t67
      t321 = t38 * t8 * t50
      t322 = t147 * t321
      t324 = 0.16D2 * t239 * t322
      t325 = t246 * t186
      t326 = t184 * t325
      t328 = t141 * t308
      t329 = t74 * t38
      t330 = t329 * t51
      t331 = t286 * t330
      t333 = 0.32D2 * t328 * t331
      t334 = t31 * t35
      t335 = t182 * t334
      t336 = t67 * t74
      t338 = t335 * t336 * t21
      t341 = t177 * t246 * t21
      t343 = t329 * t116
      t344 = t90 * t343
      t346 = 0.32D2 * t166 * t344
      t347 = 0.488D3 * t266 - t269 + t272 - 0.112D3 * t274 + t278 + t281
     # - t285 - t289 - 0.86D2 * t295 - t300 + t307 + t311 + t313 + 0.112
     #D3 * t318 + t324 + 0.67D2 * t326 - t333 - 0.133D3 * t338 - 0.148D3
     # * t341 - t346
      t349 = t335 * t178 * t134
      t351 = t167 * t8
      t352 = t264 * t351
      t353 = t166 * t352
      t356 = t97 * t98 * t27
      t357 = t19 * t4
      t360 = t356 * t334 * t357 * t50
      t363 = t80 * t81 * t35
      t364 = t116 * t50
      t367 = 0.8D1 * t363 * t195 * t364
      t368 = t133 * t321
      t370 = 0.176D3 * t239 * t368
      t371 = t63 * t165
      t373 = 0.8D1 * t371 * t344
      t374 = t264 * t168
      t375 = t371 * t374
      t377 = t20 * t343
      t379 = 0.8D1 * t371 * t377
      t380 = t246 * t321
      t382 = 0.8D1 * t245 * t380
      t383 = t165 * x1
      t384 = t141 * t383
      t386 = 0.128D3 * t384 * t325
      t389 = 0.260D3 * t172 * t107
      t391 = t97 * t314 * t32
      t393 = 0.96D2 * t391 * t241
      t396 = t356 * t334 * t292 * t50
      t398 = t63 * t383
      t400 = 0.16D2 * t398 * t187
      t401 = t371 * t352
      t403 = t178 * t321
      t405 = 0.8D1 * t245 * t403
      t406 = t329 * t91
      t407 = t90 * t406
      t409 = 0.8D1 * t371 * t407
      t412 = t291 * t176 * t357 * t8
      t414 = t371 * t169
      t417 = t335 * t228 * t21
      t420 = t184 * t246 * t106
      t422 = t389 + t393 - 0.488D3 * t396 + t400 - 0.24D2 * t401 + t405 
     #- t409 - 0.86D2 * t412 - 0.24D2 * t414 - 0.205D3 * t417 - 0.7D1 * 
     #t420
      t428 = 0.16D2 * t172 * t129 * t59
      t430 = t335 * t228 * t59
      t434 = t291 * t176 * t292 * t8
      t437 = t88 * t32 * x1
      t438 = t19 * t39
      t439 = t116 * t8
      t442 = 0.6D1 * t437 * t438 * t439
      t445 = t315 * t226 * t134 * t110
      t447 = x4 * t51
      t450 = 0.24D2 * t194 * t195 * t447
      t452 = 0.38D2 * t309 * t85
      t454 = t335 * t336 * t59
      t457 = t335 * t246 * t134
      t460 = t227 * t336 * t134
      t462 = t50 * t8
      t465 = 0.48D2 * t302 * t303 * t462
      t466 = t225 * t301
      t467 = t178 * t240
      t468 = t466 * t467
      t471 = t291 * t176 * t134
      t473 = x3 * t38
      t476 = 0.48D2 * t302 * t473 * t304
      t477 = t65 * t67
      t479 = t239 * t477 * t134
      t480 = 0.80D2 * t479
      t481 = t133 * t240
      t483 = 0.16D2 * t239 * t481
      t485 = 0.128D3 * t384 * t187
      t488 = t13 * t79 * t81 * t65
      t490 = t488 * t178 * t84
      t492 = t83 * t330
      t494 = 0.32D2 * t328 * t492
      t497 = 0.176D3 * t172 * t20 * t186
      t498 = -t428 - 0.133D3 * t430 + 0.488D3 * t434 + t442 + 0.112D3 * 
     #t445 + t450 + t452 - 0.205D3 * t454 - 0.133D3 * t457 + 0.96D2 * t4
     #60 - t465 + 0.205D3 * t468 + 0.160D3 * t471 - t476 - t480 + t483 -
     # t485 - 0.48D2 * t490 - t494 + t497
      t500 = 0.8D1 * t245 * t467
      t501 = t129 * t351
      t502 = t166 * t501
      t504 = t466 * t247
      t507 = t488 * t246 * t84
      t509 = t20 * t406
      t511 = 0.32D2 * t166 * t509
      t512 = t104 * t183
      t515 = 0.12D2 * t512 * t473 * t45
      t517 = 0.16D2 * t172 * t148
      t519 = 0.260D3 * t172 * t151
      t521 = 0.32D2 * t166 * t407
      t522 = t63 * t238
      t523 = t167 * t50
      t526 = 0.16D2 * t522 * t477 * t523
      t530 = 0.16D2 * t522 * t297 * t523
      t532 = 0.16D2 * t398 * t325
      t534 = 0.128D3 * t391 * t481
      t537 = t291 * t176 * t357 * x4
      t541 = 0.48D2 * t302 * t473 * t462
      t542 = x4 * t91
      t545 = 0.6D1 * t437 * t195 * t542
      t546 = t175 * t16
      t548 = t546 * t264 * t74
      t550 = t182 * t31
      t553 = t550 * t65 * t68 * t74
      t556 = t546 * t129 * t74
      t560 = t550 * t65 * t111 * t74
      t562 = t14 * t27
      t564 = t562 * t66 * t68
      t565 = 0.5D1 * t564
      t566 = t530 + t532 + t534 + 0.488D3 * t537 + t541 + t545 + 0.133D3
     # * t548 - 0.48D2 * t553 + 0.133D3 * t556 - 0.48D2 * t560 + t565
      t570 = t562 * t66 * t111
      t571 = 0.5D1 * t570
      t572 = t205 * t72
      t573 = 0.5D1 * t572
      t574 = t31 * t38
      t579 = 0.128D3 * t356 * t574 * t8 * x1 * t50
      t581 = t184 * t178 * t122
      t584 = t184 * t178 * t106
      t590 = 0.128D3 * t356 * t574 * x4 * x1 * t50
      t591 = t466 * t403
      t593 = t91 * t50
      t596 = 0.8D1 * t363 * t438 * t593
      t599 = 0.8D1 * t363 * t195 * t593
      t602 = 0.12D2 * t512 * t303 * t45
      t605 = 0.6D1 * t437 * t438 * t542
      t606 = t45 * t50
      t609 = 0.16D2 * t363 * t438 * t606
      t611 = t184 * t246 * t122
      t615 = 0.32D2 * t315 * t301 * t84
      t616 = t18 * t154
      t617 = 0.80D2 * t616
      t620 = 0.24D2 * t194 * t438 * t447
      t623 = 0.8D1 * t363 * t438 * t364
      t626 = 0.24D2 * t194 * t438 * t196
      t629 = 0.16D2 * t363 * t195 * t606
      t631 = t177 * t246 * t59
      t633 = t571 + t573 - t579 - 0.7D1 * t581 + 0.74D2 * t584 - t590 + 
     #0.133D3 * t591 + t596 + t599 + t602 - t605 - t609 + 0.74D2 * t611 
     #+ t615 - t617 - t620 + t623 + t626 - t629 - 0.67D2 * t631
      t635 = 0.128D3 * t391 * t322
      t637 = t97 * t98 * t1
      t641 = 0.768D3 * t637 * t69 * x3 * z
      t642 = t200 * t50
      t645 = 0.384D3 * t172 * t147 * t642
      t649 = 0.768D3 * t637 * t69 * t19 * z
      t650 = t18 * t138
      t651 = 0.80D2 * t650
      t654 = 0.16D2 * t172 * t264 * t21
      t656 = 0.32D2 * t166 * t377
      t657 = t166 * t374
      t661 = 0.384D3 * t172 * t133 * t642
      t662 = t371 * t501
      t666 = 0.8D1 * t371 * t509
      t667 = t63 * t308
      t669 = 0.16D2 * t667 * t492
      t671 = 0.16D2 * t667 * t331
      t672 = t26 * t1
      t673 = t672 * t72
      t674 = 0.42D2 * t673
      t675 = t672 * t70
      t676 = 0.42D2 * t675
      t678 = t177 * t178 * t21
      t683 = 0.96D2 * t291 * t16 * t38 * t45
      t686 = 0.6D1 * t437 * t195 * t439
      t688 = 0.96D2 * t391 * t368
      t689 = t466 * t380
      t692 = -t666 - t669 - t671 - t674 - t676 - 0.67D2 * t678 + t683 - 
     #t686 + t688 + 0.205D3 * t689 + 0.16D2 * t99
      t703 = t25 - t56 - t62 + t78 + t87 + t96 + 0.597D3 * t102 + t109 +
     # t115 + t121 + t125 - t128 - 0.597D3 * t131 - t137 + t140 + 0.32D2
     # * t143 + 0.32D2 * t145 - t150 + t153 + t156
      t713 = -0.40D2 * t217 - t221 - 0.40D2 * t223 - 0.240D3 * t230 - t2
     #34 - t237 + t243 - t249 + 0.128D3 * t251 + t257 + 0.597D3 * t259
      t723 = -0.597D3 * t266 + t269 - t272 + 0.128D3 * t274 - t278 - t28
     #1 + t285 + t289 + 0.176D3 * t295 + t300 - t307 - t311 - t313 - 0.1
     #28D3 * t318 - t324 + 0.12D2 * t326 + t333 + 0.30D2 * t338 + 0.80D2
     # * t341 + t346
      t735 = -t389 - t393 + 0.597D3 * t396 - t400 + 0.48D2 * t401 - t405
     # + t409 + 0.176D3 * t412 + 0.48D2 * t414 + 0.102D3 * t417 + 0.52D2
     # * t420
      t748 = t428 + 0.30D2 * t430 - 0.597D3 * t434 - t442 - 0.128D3 * t4
     #45 - t450 - t452 + 0.102D3 * t454 + 0.30D2 * t457 - 0.240D3 * t460
     # + t465 - 0.102D3 * t468 - 0.176D3 * t471 + t476 + t480 - t483 + t
     #485 + 0.120D3 * t490 + t494 - t497
      t758 = -t530 - t532 - t534 - 0.597D3 * t537 - t541 - t545 - 0.30D2
     # * t548 + 0.120D3 * t553 - 0.30D2 * t556 + 0.120D3 * t560 - t565
      t766 = -t571 - t573 + t579 + 0.52D2 * t581 - 0.40D2 * t584 + t590 
     #- 0.30D2 * t591 - t596 - t599 - t602 + t605 + t609 - 0.40D2 * t611
     # - t615 + t617 + t620 - t623 - t626 + t629 - 0.12D2 * t631
      t773 = t666 + t669 + t671 + t674 + t676 - 0.12D2 * t678 - t683 + t
     #686 - t688 - 0.102D3 * t689 - 0.32D2 * t99
      t792 = -0.32D2 * t86 - 0.48D2 * t102 + 0.48D2 * t131 + 0.64D2 * t1
     #36 + 0.64D2 * t149 - t174 - 0.256D3 * t180 + 0.16D2 * t188 - 0.32D
     #2 * t206 + 0.64D2 * t211 + 0.64D2 * t217 + 0.64D2 * t220 + 0.64D2 
     #* t223
      t807 = -0.48D2 * t259 + 0.48D2 * t266 - 0.32D2 * t288 - 0.160D3 * 
     #t295 - 0.16D2 * t299 + 0.16D2 * t326 - 0.32D2 * t338 - 0.256D3 * t
     #341 - 0.32D2 * t349 - 0.48D2 * t360 - 0.48D2 * t396 - 0.160D3 * t4
     #12 - 0.48D2 * t417 - 0.80D2 * t420
      t820 = -t428 - 0.32D2 * t430 + 0.48D2 * t434 - 0.48D2 * t454 - 0.3
     #2D2 * t457 + 0.48D2 * t468 - 0.16D2 * t479 + 0.64D2 * t504 - t517 
     #+ 0.48D2 * t537 + 0.64D2 * t548 + 0.64D2 * t556 - 0.32D2 * t564
      t834 = -0.32D2 * t570 - 0.32D2 * t572 - 0.80D2 * t581 + 0.64D2 * t
     #584 + 0.64D2 * t591 + 0.64D2 * t611 - 0.16D2 * t616 - 0.16D2 * t63
     #1 - 0.16D2 * t650 - t654 - 0.32D2 * t673 - 0.32D2 * t675 - 0.16D2 
     #* t678 + 0.48D2 * t689
      rrgg2ggh21J5 = 0.9D1 / 0.16D2 * (0.5D1 * wd * (t164 + t566 - 0.112
     #D3 * t353 - t651 + t645 + 0.24D2 * t662 - t379 + t633 + t635 + t26
     #1 - t204 - t199 - t521 - t382 - t386 + t515 - t517 + t519 + t526 +
     # t692 + 0.67D2 * t188 + t498 - 0.160D3 * t502 + t641 - t500 + t347
     # - 0.133D3 * t349 - 0.112D3 * t170 - t174 + t157 - t511 + t207 - 0
     #.160D3 * t657 + t661 - t654 - t656 + t161 - 0.148D3 * t180 + t422 
     #+ 0.133D3 * t504 - 0.48D2 * t507 + t649 - t373 + t367 - 0.488D3 * 
     #t360 - t370 + 0.24D2 * t375 + t212) + 0.4D1 * wd * (-t164 + 0.128D
     #3 * t353 + t651 - t645 + t735 + t748 + t758 - 0.48D2 * t662 + t379
     # - t635 + t204 + t199 + t766 + t773 + t521 + t382 + t386 - t515 + 
     #t713 + t723 + t517 - t519 - t526 + t703 + 0.12D2 * t188 + 0.176D3 
     #* t502 - t641 + t500 + 0.30D2 * t349 + 0.128D3 * t170 + t174 + t51
     #1 - t207 + 0.176D3 * t657 - t661 + t654 + t656 - t161 + 0.80D2 * t
     #180 - 0.30D2 * t504 + 0.120D3 * t507 - t649 + t373 - t367 + 0.597D
     #3 * t360 + t370 - 0.48D2 * t375 - t212) + 0.3D1 * wd * (t792 + t80
     #7 + t820 + t834)) / t11 / t12 / t74 / z / 0.3141592653589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh21J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = kappa2(x1, x2, x3, x4, z)
      t2 = s * t1
      t3 = 0.1D1 - z
      t4 = t3 * x1
      t5 = x3 * t4
      t7 = 0.1D1 - x3
      t8 = t4 * t7
      t10 = s - t2 * t5 - t2 * t8
      t11 = 0.1D1 - x1
      t12 = t3 * t11
      t15 = 0.1D1 - x4
      t18 = s - t2 * t12 * x4 - t2 * t12 * t15
      t19 = t10 * t18
      t20 = s ** 2
      t21 = t20 * s
      t22 = t19 * t21
      t24 = t1 * t3
      t25 = t11 * t15
      t29 = t20 ** 2
      t30 = t18 * t29
      t31 = t30 * t1
      t32 = z ** 2
      t37 = x1 * t7
      t39 = t22 * t24 * t37
      t41 = t1 ** 2
      t42 = t3 ** 2
      t43 = t41 * t42
      t44 = x1 ** 2
      t45 = t7 ** 2
      t46 = t44 * t45
      t48 = t22 * t43 * t46
      t50 = t29 * s
      t51 = t41 * t1
      t52 = t50 * t51
      t53 = t42 * t3
      t54 = t52 * t53
      t55 = x1 * x3
      t56 = t11 ** 2
      t57 = t15 ** 2
      t58 = t56 * t57
      t59 = t55 * t58
      t65 = t11 * x4
      t69 = t41 ** 2
      t70 = t50 * t69
      t71 = t42 ** 2
      t72 = t70 * t71
      t73 = t56 * t11
      t75 = t73 * t57 * t15
      t79 = x4 ** 2
      t80 = t56 * t79
      t88 = t69 * t1
      t89 = t50 * t88
      t90 = t71 * t3
      t91 = t89 * t90
      t92 = t44 * x1
      t93 = t92 * t7
      t97 = cos(x2 * 0.3141592653589793D1)
      t99 = x4 * t15
      t101 = Sqrt(x3 * t7 * t99)
      t104 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t97 * t101
      t105 = t104 ** 2
      t106 = t56 * t105
      t107 = t93 * t106
      t108 = t91 * t107
      t113 = t50 * t69 * t51 * t71 * t53
      t114 = t44 ** 2
      t117 = t73 * t105 * t104
      t122 = t50 * t41 * t42
      t126 = t29 * z
      t127 = t126 * t51
      t128 = t53 * t92
      t129 = t45 * t7
      t131 = t128 * t129 * t10
      t134 = t92 * x3
      t135 = t134 * t106
      t136 = t91 * t135
      t142 = t22 * t24 * t55
      t144 = t37 * t58
      t147 = x3 ** 2
      t148 = t147 * x3
      t150 = t128 * t148 * t10
      t153 = -0.32D2 * t22 + 0.128D3 * t22 * t24 * t25 + 0.384D3 * t31 *
     # t4 * t7 * t32 + 0.597D3 * t39 - 0.597D3 * t48 + 0.6D1 * t54 * t59
     # - 0.32D2 * t22 * t43 * t58 + 0.128D3 * t22 * t24 * t65 - 0.2D1 * 
     #t72 * t37 * t75 - 0.32D2 * t22 * t43 * t80 + 0.384D3 * t31 * t4 * 
     #x3 * t32 + 0.120D3 * t108 - 0.32D2 * t113 * t114 * x3 * t117 - 0.1
     #4D2 * t122 * t37 * t25 + 0.32D2 * t127 * t131 + 0.120D3 * t136 + 0
     #.2D1 * t72 * t55 * t75 + 0.597D3 * t142 + 0.6D1 * t54 * t144 + 0.3
     #2D2 * t127 * t150
      t155 = t73 * t79 * x4
      t159 = t37 * t80
      t165 = t44 * t147
      t167 = t22 * t43 * t165
      t169 = t44 * t7
      t170 = t11 * t104
      t171 = t169 * t170
      t172 = t54 * t171
      t174 = t37 * t65
      t177 = t18 * t21
      t178 = t177 * t51
      t183 = t44 * x3
      t184 = t183 * t170
      t185 = t54 * t184
      t187 = t55 * t80
      t191 = t55 * t25
      t201 = t51 * t53
      t202 = t30 * t201
      t205 = t56 * x4
      t206 = t205 * t15
      t210 = t69 * t71
      t211 = t30 * t210
      t212 = t205 * t104
      t213 = t183 * t212
      t216 = t177 * t201
      t217 = t10 * t11
      t218 = t217 * t15
      t219 = t46 * t218
      t223 = t19 * t21 * t51
      t224 = t53 * t44
      t225 = t7 * t11
      t228 = t223 * t224 * t225 * t104
      t231 = t89 * t90 * t44
      t232 = x3 * t73
      t233 = t79 * t104
      t237 = t29 * t51
      t238 = t53 * x1
      t239 = t237 * t238
      t240 = t7 * t10
      t242 = t239 * t240 * t58
      t246 = 0.16D2 * t202 * t165 * t25
      t247 = 0.14D2 * t122 * t191 - 0.32D2 * t113 * t114 * t7 * t117 - 0
     #.2D1 * t72 * t55 * t155 - 0.38D2 * t202 * t59 - 0.176D3 * t202 * t
     #55 * t206 + 0.176D3 * t211 * t213 + 0.128D3 * t216 * t219 + 0.597D
     #3 * t228 - 0.8D1 * t231 * t232 * t233 + 0.52D2 * t242 + t246
      t251 = t19 * t21 * t41
      t252 = t42 * x1
      t256 = t71 * t44
      t257 = t70 * t256
      t258 = x3 * t56
      t259 = x4 * t104
      t263 = t92 * t45
      t265 = t211 * t263 * t170
      t267 = t169 * t212
      t271 = 0.16D2 * t202 * t171
      t272 = t29 * t41
      t273 = t272 * t252
      t274 = x3 * t10
      t276 = t273 * t274 * t25
      t278 = t274 * t206
      t279 = t239 * t278
      t285 = t50 * t69 * t41 * t71 * t42 * t92
      t286 = t105 * t15
      t290 = t30 * t43
      t291 = z * t11
      t296 = t88 * t90
      t297 = t30 * t296
      t302 = t21 * t69
      t303 = t19 * t302
      t307 = t71 * t92
      t312 = x4 * t105
      t318 = t237 * t224
      t319 = t45 * t10
      t321 = t318 * t319 * t25
      t324 = t318 * t240 * t170
      t326 = t29 * t69
      t327 = t326 * t307
      t332 = t19 * t21 * t1
      t337 = t291 * t104
      t341 = -0.176D3 * t251 * t252 * t170 + 0.48D2 * t257 * t258 * t259
     # + 0.80D2 * t265 - 0.16D2 * t211 * t267 + t271 + 0.80D2 * t276 + 0
     #.12D2 * t279 + 0.24D2 * t285 * t232 * t286 + 0.384D3 * t290 * t55 
     #* t291 * t15 - 0.38D2 * t297 * t107 - 0.38D2 * t202 * t159 - 0.32D
     #2 * t303 * t256 * t106 - 0.128D3 * t303 * t307 * t170 * t147 - 0.2
     #4D2 * t285 * t232 * t312 - 0.38D2 * t297 * t135 + 0.102D3 * t321 +
     # 0.30D2 * t324 - 0.240D3 * t327 * t319 * t170 - 0.768D3 * t332 * t
     #4 * x3 * z - 0.384D3 * t202 * t183 * t337
      t342 = t126 * t210
      t343 = t217 * t104
      t347 = t92 * t147
      t351 = t201 * x1
      t352 = t126 * t351
      t353 = t240 * t206
      t356 = t126 * t296
      t357 = t10 * t56
      t358 = t357 * t105
      t359 = t134 * t358
      t363 = 0.16D2 * t202 * t184
      t366 = t357 * t57
      t367 = t55 * t366
      t370 = t290 * t174
      t374 = 0.16D2 * t202 * t46 * t65
      t375 = t357 * t79
      t376 = t37 * t375
      t380 = t126 * t201
      t384 = t126 * t210 * t44
      t386 = t56 * t15 * t104
      t387 = t274 * t386
      t392 = t217 * x4
      t393 = t165 * t392
      t399 = t19 * t302 * t71
      t402 = x3 * t11
      t405 = t223 * t224 * t402 * t104
      t407 = t177 * t351
      t410 = t326 * t256
      t411 = t240 * t212
      t412 = t410 * t411
      t416 = t29 * t88 * t90 * t92
      t420 = t37 * t366
      t423 = 0.48D2 * t380 * t219 - 0.8D1 * t384 * t387 + 0.8D1 * t380 *
     # t367 + 0.128D3 * t216 * t393 - 0.260D3 * t202 * t144 - 0.96D2 * t
     #399 * t213 + 0.597D3 * t405 + 0.128D3 * t407 * t278 - 0.30D2 * t41
     #2 + 0.120D3 * t416 * t240 * t106 + 0.32D2 * t216 * t420
      t427 = t52 * t238
      t434 = t169 * t386
      t437 = t55 * t375
      t440 = t46 * t392
      t445 = t240 * t386
      t448 = t7 * t56
      t449 = t104 * t15
      t453 = t165 * t218
      t456 = t183 * t386
      t459 = t239 * t353
      t461 = t177 * t296
      t462 = t93 * t358
      t467 = t410 * t445
      t469 = t274 * t212
      t470 = t410 * t469
      t472 = t7 * t73
      t482 = t99 * t104
      t488 = -0.12D2 * t427 * t258 * t99 + 0.120D3 * t416 * t274 * t106 
     #+ 0.176D3 * t211 * t434 + 0.8D1 * t380 * t437 - 0.48D2 * t380 * t4
     #40 + 0.8D1 * t380 * t376 + 0.8D1 * t384 * t445 + 0.48D2 * t257 * t
     #448 * t449 + 0.176D3 * t216 * t453 - 0.16D2 * t211 * t456 + 0.12D2
     # * t459 + 0.32D2 * t461 * t462 + 0.128D3 * t407 * t353 - 0.102D3 *
     # t467 - 0.102D3 * t470 + 0.24D2 * t285 * t472 * t312 - 0.8D1 * t23
     #1 * t472 * t233 - 0.24D2 * t285 * t472 * t286 + 0.16D2 * t231 * t2
     #32 * t482 - 0.8D1 * t384 * t411
      t491 = t147 * t10
      t499 = t251 * t252 * t225 * x4
      t505 = t70 * t71 * x1
      t506 = x4 * t57
      t511 = t273 * t240 * t25
      t516 = t318 * t491 * t25
      t520 = t251 * t252 * t402 * t15
      t523 = t79 * t15
      t529 = t251 * t252 * t402 * x4
      t532 = t211 * t347 * t170
      t544 = t50 * t1
      t545 = t544 * t8
      t547 = t544 * t5
      t549 = t57 * t104
      t556 = -0.6D1 * t505 * t472 * t523 + 0.176D3 * t529 + 0.80D2 * t53
     #2 - 0.48D2 * t257 * t448 * t259 - 0.384D3 * t202 * t169 * t337 - 0
     #.48D2 * t380 * t453 + 0.8D1 * t380 * t420 + 0.42D2 * t545 + 0.42D2
     # * t547 - 0.8D1 * t231 * t232 * t549 - 0.12D2 * t427 * t448 * t99
      t566 = t239 * t240 * t80
      t568 = t31 * t5
      t570 = t30 * t41
      t571 = t42 * t44
      t573 = t570 * t571 * t147
      t576 = t29 * t1 * t3
      t578 = t576 * t55 * t10
      t581 = t570 * t571 * t45
      t584 = t576 * t37 * t10
      t586 = t272 * t42
      t588 = t586 * t46 * t10
      t590 = t237 * t53
      t596 = t586 * t165 * t10
      t602 = t30 * t51
      t604 = t602 * t128 * t129
      t607 = t602 * t128 * t148
      t609 = t31 * t8
      t621 = t318 * t274 * t170
      t623 = t53 * t56
      t629 = 0.6D1 * t505 * t472 * t506 + 0.16D2 * t231 * t472 * t482 - 
     #0.40D2 * t566 - 0.5D1 * t568 - 0.74D2 * t573 - 0.40D2 * t578 - 0.7
     #4D2 * t581 - 0.40D2 * t584 - 0.30D2 * t588 + 0.120D3 * t590 * t92 
     #* t129 * t10 - 0.30D2 * t596 + 0.120D3 * t590 * t92 * t148 * t10 -
     # 0.5D1 * t604 - 0.5D1 * t607 - 0.5D1 * t609 - 0.96D2 * t251 * t42 
     #* t56 * t99 + 0.6D1 * t505 * t232 * t523 - 0.96D2 * t399 * t434 + 
     #0.30D2 * t621 + 0.128D3 * t223 * t623 * x4 * x1 * t104
      t630 = t410 * t387
      t636 = t318 * t491 * t65
      t639 = t273 * t274 * t65
      t641 = t290 * t191
      t665 = t318 * t319 * t65
      t668 = t273 * t240 * t65
      t672 = t251 * t252 * t225 * t15
      t675 = t239 * t274 * t80
      t678 = t239 * t274 * t58
      t692 = 0.48D2 * t380 * t393 + 0.128D3 * t223 * t623 * t15 * x1 * t
     #104 + 0.30D2 * t665 + 0.80D2 * t668 + 0.176D3 * t672 + 0.52D2 * t6
     #75 - 0.40D2 * t678 + 0.384D3 * t290 * t37 * t65 * z - 0.16D2 * t35
     #2 * t278 + 0.176D3 * t216 * t440 - 0.128D3 * t303 * t307 * t170 * 
     #t45
      t710 = -0.48D2 * t39 + 0.48D2 * t48 - 0.32D2 * t108 - 0.32D2 * t13
     #6 - 0.48D2 * t142 + 0.48D2 * t167 + 0.64D2 * t172 + 0.64D2 * t185 
     #- 0.48D2 * t228 - 0.80D2 * t242 - t246 - 0.16D2 * t265 - t271
      t723 = -0.256D3 * t276 + 0.16D2 * t279 - 0.48D2 * t321 - 0.32D2 * 
     #t324 - t363 - 0.16D2 * t370 - t374 - 0.48D2 * t405 + 0.64D2 * t412
     # + 0.16D2 * t459 + 0.48D2 * t467 + 0.48D2 * t470 + 0.48D2 * t499 -
     # 0.16D2 * t511
      t738 = -0.32D2 * t516 + 0.48D2 * t520 - 0.160D3 * t529 - 0.16D2 * 
     #t532 - 0.32D2 * t545 - 0.32D2 * t547 + 0.64D2 * t566 - 0.32D2 * t5
     #68 + 0.64D2 * t573 + 0.64D2 * t578 + 0.64D2 * t581 + 0.64D2 * t584
     # + 0.64D2 * t588
      t753 = 0.64D2 * t596 - 0.32D2 * t604 - 0.32D2 * t607 - 0.32D2 * t6
     #09 - 0.32D2 * t621 + 0.64D2 * t630 - 0.48D2 * t636 - 0.16D2 * t639
     # - 0.16D2 * t641 - 0.32D2 * t665 - 0.256D3 * t668 - 0.160D3 * t672
     # - 0.80D2 * t675 + 0.64D2 * t678
      rrgg2ggh21J6 = 0.9D1 / 0.16D2 * (0.5D1 * wd * (-0.597D3 * t167 + t
     #556 - 0.104D3 * t185 - 0.768D3 * t332 * t4 * t7 * z + t488 + t629 
     #+ 0.8D1 * t384 * t469 + 0.6D1 * t54 * t159 + 0.14D2 * t122 * t174 
     #+ 0.32D2 * t178 * t150 + 0.32D2 * t178 * t131 + 0.6D1 * t54 * t187
     # - 0.16D2 * t352 * t353 + 0.16D2 * t356 * t359 - 0.260D3 * t202 * 
     #t187 + 0.32D2 * t216 * t367 + 0.32D2 * t216 * t376 + 0.32D2 * t216
     # * t437 - 0.128D3 * t399 * t267 - 0.128D3 * t399 * t456 + 0.16D2 *
     # t356 * t462 + 0.32D2 * t461 * t359 - 0.597D3 * t520 + 0.30D2 * t5
     #16 + t692 + t247 - 0.12D2 * t639 + 0.80D2 * t641 - 0.597D3 * t499 
     #- 0.104D3 * t172 + t153 - 0.12D2 * t511 - 0.30D2 * t630 + 0.102D3 
     #* t636 + t341 + t423 - 0.16D2 * t342 * t263 * t343 - 0.6D1 * t505 
     #* t232 * t506 - 0.48D2 * t257 * t258 * t449 - 0.16D2 * t342 * t347
     # * t343 - 0.176D3 * t202 * t37 * t206 - 0.14D2 * t122 * t55 * t65 
     #+ 0.2D1 * t72 * t37 * t155 - 0.240D3 * t327 * t491 * t170 - 0.8D1 
     #* t231 * t472 * t549 + t374 + t363 + 0.80D2 * t370) + 0.4D1 * wd *
     # (t710 + t723 + t738 + t753)) / t18 / t20 / t10 / z / 0.3141592653
     #589793D1

      end function
  
   
 

      doubleprecision function rrgg2ggh21J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = t1 ** 2
      t3 = t2 * s
      t4 = kappa2(x1, x2, x3, x4, z)
      t5 = t3 * t4
      t6 = 0.1D1 - z
      t7 = t6 * x1
      t8 = 0.1D1 - x3
      t9 = t7 * t8
      t12 = t7 * x3
      t15 = s * t4
      t16 = 0.1D1 - x1
      t17 = t6 * t16
      t20 = 0.1D1 - x4
      t23 = s - t15 * t17 * x4 - t15 * t17 * t20
      t24 = t23 * t2
      t25 = t4 ** 2
      t26 = t25 ** 2
      t27 = t6 ** 2
      t28 = t27 ** 2
      t30 = t24 * t26 * t28
      t31 = x1 ** 2
      t32 = t31 * x1
      t33 = t8 ** 2
      t38 = cos(x2 * 0.3141592653589793D1)
      t42 = Sqrt(x3 * t8 * x4 * t20)
      t45 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t38 * t42
      t46 = t16 * t45
      t50 = t25 * t4
      t51 = t27 * t6
      t53 = t24 * t50 * t51
      t55 = t31 * x3 * t46
      t58 = t25 * t27
      t59 = t24 * t58
      t60 = x1 * t8
      t61 = t16 * x4
      t65 = t31 * t33
      t71 = s - t15 * t12 - t15 * t9
      t72 = t71 * t23
      t73 = s * t1
      t75 = t72 * t73 * t25
      t76 = t27 * x1
      t77 = x3 * t16
      t83 = t72 * t73 * t50
      t84 = t51 * t31
      t85 = t8 * t16
      t90 = x3 ** 2
      t96 = t31 * t8 * t46
      t99 = t2 * t25
      t100 = t99 * t76
      t101 = x3 * t71
      t102 = t16 * t20
      t106 = t2 * t50
      t108 = t106 * t51 * x1
      t109 = t16 ** 2
      t110 = t109 * x4
      t111 = t110 * t20
      t115 = x1 * x3
      t119 = -0.32D2 * t5 * t9 - 0.32D2 * t5 * t12 - 0.16D2 * t30 * t32 
     #* t33 * t46 - 0.16D2 * t53 * t55 - 0.16D2 * t59 * t60 * t61 - 0.16
     #D2 * t53 * t65 * t61 + 0.48D2 * t75 * t76 * t77 * t20 - 0.48D2 * t
     #83 * t84 * t85 * t45 - 0.16D2 * t30 * t32 * t90 * t46 - 0.16D2 * t
     #53 * t96 - 0.256D3 * t100 * t101 * t102 + 0.16D2 * t108 * t101 * t
     #111 - 0.16D2 * t59 * t115 * t102
      t124 = t24 * t4
      t127 = t24 * t25
      t128 = t27 * t31
      t133 = t2 * t4 * t6
      t143 = t99 * t27
      t158 = t3 * t26 * t4 * t28 * t6
      t160 = t45 ** 2
      t161 = t109 * t160
      t169 = t72 * t73
      t173 = t4 * t6
      t177 = t31 * t90
      t181 = -0.160D3 * t75 * t76 * t77 * x4 - 0.32D2 * t124 * t12 + 0.6
     #4D2 * t127 * t128 * t90 + 0.64D2 * t133 * t115 * t71 + 0.64D2 * t1
     #27 * t128 * t33 + 0.64D2 * t133 * t60 * t71 + 0.64D2 * t143 * t65 
     #* t71 - 0.48D2 * t83 * t84 * t77 * t45 + 0.48D2 * t75 * t76 * t85 
     #* x4 - 0.32D2 * t158 * t32 * x3 * t161 - 0.32D2 * t158 * t32 * t8 
     #* t161 + 0.48D2 * t169 * t58 * t65 - 0.48D2 * t169 * t173 * t60 + 
     #0.48D2 * t169 * t58 * t177
      t187 = t3 * t50 * t51
      t192 = t106 * t84
      t193 = t90 * t71
      t203 = t33 * t71
      t209 = t2 * t26 * t28 * t31
      t211 = t109 * t20 * t45
      t215 = t8 * t71
      t216 = x4 ** 2
      t217 = t109 * t216
      t221 = t20 ** 2
      t222 = t109 * t221
      t232 = t110 * t45
      t236 = -0.48D2 * t169 * t173 * t115 + 0.64D2 * t187 * t96 + 0.64D2
     # * t187 * t55 - 0.48D2 * t192 * t193 * t61 - 0.16D2 * t100 * t101 
     #* t61 - 0.32D2 * t192 * t101 * t46 - 0.48D2 * t192 * t203 * t102 +
     # 0.64D2 * t209 * t101 * t211 + 0.64D2 * t108 * t215 * t217 - 0.80D
     #2 * t108 * t215 * t222 - 0.80D2 * t108 * t101 * t217 + 0.64D2 * t1
     #08 * t101 * t222 + 0.64D2 * t209 * t215 * t232
      t271 = t24 * t50
      t272 = t51 * t32
      t283 = 0.16D2 * t108 * t215 * t111 + 0.48D2 * t209 * t215 * t211 -
     # 0.256D3 * t100 * t215 * t61 - 0.160D3 * t75 * t76 * t85 * t20 - 0
     #.16D2 * t53 * t177 * t102 + 0.48D2 * t209 * t101 * t232 - 0.16D2 *
     # t100 * t215 * t102 - 0.32D2 * t192 * t193 * t102 - 0.32D2 * t192 
     #* t215 * t46 - 0.32D2 * t192 * t203 * t61 + 0.64D2 * t143 * t177 *
     # t71 - 0.32D2 * t271 * t272 * t33 * t8 - 0.32D2 * t271 * t272 * t9
     #0 * x3 - 0.32D2 * t124 * t9
      rrgg2ggh21J7 = 0.45D2 / 0.16D2 * wd * (t119 + t181 + t236 + t283) 
     #/ t23 / t1 / t71 / z / 0.3141592653589793D1

      end function
  
 