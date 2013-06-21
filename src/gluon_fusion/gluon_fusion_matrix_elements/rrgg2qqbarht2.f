  
      subroutine rrgg2qqbarht2
     &(sector,pole,s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4,fff)     
      implicit none  
      integer sector,pole  
      doubleprecision x1, x2, x3, x4,ff(5),fff  
      doubleprecision s, XB1, XB2, z, lh, wd, nf  
      doubleprecision KAPPA  
      doubleprecision KAPPA2  
      doubleprecision KAPPAF  
      doubleprecision Log  
      doubleprecision rrgg2qqbarh21J1  
      doubleprecision rrgg2qqbarh21J2  
      doubleprecision rrgg2qqbarh21J3  
      doubleprecision rrgg2qqbarh21J4  
      doubleprecision rrgg2qqbarh21J5  
      doubleprecision rrgg2qqbarh21J6  
      doubleprecision rrgg2qqbarh21J7  
      doubleprecision rrgg2qqbarht2s1e1  
      doubleprecision rrgg2qqbarht2s1e0  
      doubleprecision rrgg2qqbarht2s1em1  
      doubleprecision rrgg2qqbarht2s1em2  
      doubleprecision rrgg2qqbarht2s1em3  
      doubleprecision rrgg2qqbarht2s1em4  
      doubleprecision rrgg2qqbarht2s2e1  
      doubleprecision rrgg2qqbarht2s2e0  
      doubleprecision rrgg2qqbarht2s2em1  
      doubleprecision rrgg2qqbarht2s2em2  
      doubleprecision rrgg2qqbarht2s2em3  
      doubleprecision rrgg2qqbarht2s2em4  
      if(pole.eq.1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2e1  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.0)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2e0  
     &    (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-1)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em1  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-2)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em2  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-3)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em3  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      else if(pole.eq.-4)then  
      if(sector.eq.1)then  
         fff=rrgg2qqbarht2s1em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      else if(sector.eq.2)then  
         fff=rrgg2qqbarht2s2em4  
     &     (s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)  
      end if  
      end if  
      end subroutine

      doubleprecision function rrgg2qqbarht2s1e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = 0.1D1 - x3
      t10 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x
     #4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x
     #4)
      t26 = t23 ** 2
      t27 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x
     #4)
      t30 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t31 = t16 * x4
      t32 = t31 * t4
      t35 = log(-0.4D1 * t14 * t32)
      t36 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t38 = t35 ** 2
      t39 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
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
      t82 = rrgg2qqbarh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
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
      t163 = t1 * x1
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
      t180 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t182 = t177 * t179 * t180
      t183 = t111 * t67
      t184 = t165 ** 2
      t185 = t171 ** 2
      t187 = t18 * t184 * t185
      t190 = log(-0.4D1 * t183 * t187)
      t192 = t171 * t179
      t193 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t194 = t192 * t193
      t200 = t46 * t47
      t202 = t177 * t179 * t193
      t208 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
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
      t266 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, t9, x4)
      t270 = t249 ** 2
      t275 = log(0.4D1 * t183 * t18 * t184 * t19 * t270)
      t278 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t9, x4)
      t289 = -0.90D2 * t7 * t8 * (t263 * t265 * t266 - t275 * t165 * t24
     #9 * t265 * t278) + 0.180D3 * t200 * t263 * t265 * t278
      t292 = t289 * t64 * t131 / 0.720D3
      t293 = FJET(XB1, XB2, s, -t244, t246, -t247, t248, t262, -t292)
      t295 = t64 * t131
      t298 = FJET(XB1, XB2, s, -t247, t248, -t244, t246, t262, -t292)
      rrgg2qqbarht2s1e1 = t157 * t156 + t159 * t156 + t237 * t236 + t239
     # * t236 - t293 * t289 * t295 / 0.720D3 - t298 * t289 * t295 / 0.72
     #0D3

      end function



      doubleprecision function rrgg2qqbarht2s1e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = 0.1D1 - x3
      t10 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x
     #4)
      t11 = x2 * 0.3141592653589793D1
      t12 = sin(t11)
      t13 = t12 ** 2
      t14 = x3 * t13
      t15 = z ** 2
      t16 = 0.1D1 / t15
      t18 = x4 * t4
      t19 = -t9
      t23 = log(0.4D1 * t14 * t16 * t18 * t19)
      t24 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, t9, x
     #4)
      t26 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t28 = t16 * x4 * t4
      t31 = log(-0.4D1 * t14 * t28)
      t32 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t38 = 0.3141592653589793D1 * lh
      t39 = t6 * t8
      t40 = t24 - t32
      t45 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t54 = x1 ** 2
      t55 = t54 * t13
      t58 = log(-0.4D1 * t55 * t28)
      t70 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
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
      t109 = t1 * x1
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
      t128 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
      t134 = t111 * t117
      t135 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1,
     # x4)
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
      t191 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, t9, x4)
      t194 = t174 / (-0.2D1 + t166) * t191 * t45 * t50
      t196 = t124 * t194 / 0.8D1
      t197 = FJET(XB1, XB2, s, -t169, t171, -t172, t173, t187, t196)
      t199 = t39 * t111
      t203 = FJET(XB1, XB2, s, -t172, t173, -t169, t171, t187, t196)
      rrgg2qqbarht2s1e0 = t103 * t102 + t105 * t102 + t162 * t161 + t164
     # * t161 + t197 * 0.3141592653589793D1 * t199 * t194 / 0.8D1 + t203
     # * 0.3141592653589793D1 * t199 * t194 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s1em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t10 = t8 * t9
      t11 = 0.1D1 / x1
      t15 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D
     #1, x4)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t30 = log(-0.4D1 * t23 / t24 * x4 * t4)
      t38 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.1D1
     # - x3, x4)
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
      t71 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.10D1, 
     #x4)
      t75 = t7 * t8 * t54 * t60 * t69 * t71 * t11 / 0.8D1
      t76 = FJET(XB1, XB2, s, t53, 0.0D0, -t57, t59, t65, -t75)
      t78 = t6 * t8
      t83 = t54 * t60 * t69 * t71 * t11
      t86 = FJET(XB1, XB2, s, -t57, t59, t53, 0.0D0, t65, -t75)
      rrgg2qqbarht2s1em1 = t46 * t45 + t48 * t45 - t76 * 0.3141592653589
     #793D1 * t78 * t83 / 0.8D1 - t86 * 0.3141592653589793D1 * t78 * t83
     # / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s1em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.10D1
     #, x4)
      t12 = 0.3141592653589793D1 * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2qqbarht2s1em2 = t13 * 0.3141592653589793D1 * t16 / 0.16D2 + t
     #18 * 0.3141592653589793D1 * t16 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht2s1em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      rrgg2qqbarht2s1em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht2s1em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      rrgg2qqbarht2s1em4 = 0.0D0

      end function


      doubleprecision function rrgg2qqbarht2s2e1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t25 = t22 ** 2
      t26 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t29 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t30 = t15 * x4
      t31 = t30 * t4
      t34 = log(-0.4D1 * t13 * t31)
      t35 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t37 = t34 ** 2
      t38 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t45 = 0.3141592653589793D1 * lh
      t46 = t6 * t8
      t53 = lh ** 2
      t55 = 0.3141592653589793D1 ** 2
      t57 = 0.180D3 * t53 - 0.30D2 * t55
      t58 = 0.3141592653589793D1 * t57
      t59 = t26 - t38
      t63 = 0.1D1 / x3
      t66 = t12 * t15
      t69 = log(-0.4D1 * t66 * t17)
      t70 = t69 * 0.3141592653589793D1
      t73 = t69 ** 2
      t74 = t73 * 0.3141592653589793D1
      t81 = rrgg2qqbarh21J4(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t109 = x1 ** 2
      t110 = x3 * t109
      t111 = t110 * t12
      t114 = log(-0.4D1 * t111 * t31)
      t120 = log(0.4D1 * t111 * t30 * t4 * t18)
      t132 = 0.1D1 / x1
      t135 = t109 * t12
      t138 = log(-0.4D1 * t135 * t31)
      t140 = t138 ** 2
      t157 = -(0.90D2 * t7 * t8 * (t9 - t22 * t23 + t25 * t26 / 0.2D1 - 
     #t29 + t34 * t35 - t37 * t38 / 0.2D1) - 0.180D3 * t45 * t46 * (t23 
     #- t22 * t26 - t35 + t34 * t38) + t58 * t46 * t59) * t63 / 0.1440D4
     # + (t58 + 0.180D3 * t70 * lh + 0.45D2 * t74) * t6 * t8 * t35 / 0.1
     #440D4 + t7 * t8 * t81 / 0.16D2 + (0.3141592653589793D1 * (0.60D2 *
     # lh * t55 - 0.2884936567583026D3 - 0.120D3 * t53 * lh) - t70 * t57
     # - 0.90D2 * t74 * lh - 0.15D2 * t73 * t69 * 0.3141592653589793D1) 
     #* t6 * t8 * t38 / 0.1440D4 + (-0.180D3 * t45 - 0.90D2 * t70) * t6 
     #* t8 * t29 / 0.1440D4 + (0.90D2 * t7 * t8 * (t35 - t114 * t38 - t2
     #3 + t120 * t26) + 0.180D3 * t45 * t46 * t59) * t63 * t132 / 0.720D
     #3 - (0.90D2 * t7 * t8 * (-t29 + t138 * t35 - t140 * t38 / 0.2D1) -
     # 0.180D3 * t45 * t46 * (-t35 + t138 * t38) - t58 * t46 * t38) * t1
     #32 / 0.720D3
      t158 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t157)
      t160 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t161 = s * t160
      t162 = t1 * x1
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
      t179 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t181 = t176 * t178 * t179
      t182 = t110 * t66
      t183 = t164 ** 2
      t184 = t170 ** 2
      t186 = t17 * t183 * t184
      t189 = log(-0.4D1 * t182 * t186)
      t191 = t170 * t178
      t192 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t193 = t191 * t192
      t199 = t46 * t45
      t201 = t176 * t178 * t192
      t207 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t213 = log(-0.4D1 * t135 * t15 * t186)
      t214 = t213 * t164
      t217 = t213 ** 2
      t235 = (0.90D2 * t7 * t8 * (-t181 + t189 * t164 * t193) + 0.180D3 
     #* t199 * t201) * t63 * t132 / 0.720D3 - (0.90D2 * t7 * t8 * (t176 
     #* t178 * t207 - t214 * t191 * t179 + t217 * t164 * t193 / 0.2D1) -
     # 0.180D3 * t45 * t46 * (t181 - t214 * t193) + t58 * t46 * t201) * 
     #t132 / 0.720D3
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
      t265 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t269 = t248 ** 2
      t274 = log(0.4D1 * t182 * t17 * t183 * t18 * t269)
      t277 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t288 = 0.90D2 * t7 * t8 * (t262 * t264 * t265 - t274 * t164 * t248
     # * t264 * t277) - 0.180D3 * t199 * t262 * t264 * t277
      t291 = t288 * t63 * t132 / 0.720D3
      t292 = FJET(XB1, XB2, s, t243, -t245, -t246, t247, t261, t291)
      t294 = t63 * t132
      t297 = FJET(XB1, XB2, s, -t167, t169, 0.0D0, t163, -t175, t235)
      t299 = FJET(XB1, XB2, s, -t246, t247, t243, -t245, t261, t291)
      rrgg2qqbarht2s2e1 = t158 * t157 + t236 * t235 + t238 * t157 + t292
     # * t288 * t294 / 0.720D3 + t297 * t235 + t299 * t288 * t294 / 0.72
     #0D3

      end function



      doubleprecision function rrgg2qqbarht2s2e0
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x4
     #)
      t10 = x2 * 0.3141592653589793D1
      t11 = sin(t10)
      t12 = t11 ** 2
      t13 = x3 * t12
      t14 = z ** 2
      t15 = 0.1D1 / t14
      t17 = x4 * t4
      t18 = -0.1D1 + x3
      t22 = log(0.4D1 * t13 * t15 * t17 * t18)
      t23 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t25 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t27 = t15 * x4 * t4
      t30 = log(-0.4D1 * t13 * t27)
      t31 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t37 = 0.3141592653589793D1 * lh
      t38 = t6 * t8
      t39 = t23 - t31
      t44 = 0.1D1 / x3
      t50 = 0.1D1 / x1
      t54 = x1 ** 2
      t55 = t54 * t12
      t58 = log(-0.4D1 * t55 * t27)
      t70 = rrgg2qqbarh21J3(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t78 = log(-0.4D1 * t12 * t15 * t17)
      t79 = t78 * 0.3141592653589793D1
      t86 = lh ** 2
      t88 = 0.3141592653589793D1 ** 2
      t94 = t78 ** 2
      t102 = -(0.90D2 * t7 * t8 * (t9 - t22 * t23 - t25 + t30 * t31) - 0
     #.180D3 * t37 * t38 * t39) * t44 / 0.1440D4 - t7 * t8 * t39 * t44 *
     # t50 / 0.8D1 - (0.90D2 * t7 * t8 * (-t25 + t58 * t31) + 0.180D3 * 
     #t37 * t38 * t31) * t50 / 0.720D3 + t7 * t8 * t70 / 0.16D2 + (-0.18
     #0D3 * t37 - 0.90D2 * t79) * t6 * t8 * t25 / 0.1440D4 + (0.31415926
     #53589793D1 * (0.180D3 * t86 - 0.30D2 * t88) + 0.180D3 * t79 * lh +
     # 0.45D2 * t94 * 0.3141592653589793D1) * t6 * t8 * t31 / 0.1440D4
      t103 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t102)
      t105 = KAPPA2(x1, x2, 0.0D0, x4, z)
      t106 = s * t105
      t107 = t1 * x1
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
      t126 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t132 = t109 * t115
      t133 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, 
     #x4)
      t137 = t115 ** 2
      t138 = t109 ** 2
      t143 = log(-0.4D1 * t55 * t15 * t17 * t137 * t138)
      t159 = -t122 * t125 * t126 * t44 * t50 / 0.8D1 - (0.90D2 * t7 * t8
     # * (t132 * t124 * t133 - t143 * t109 * t125 * t126) - 0.180D3 * t3
     #7 * t38 * t132 * t124 * t126) * t50 / 0.720D3
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
      t189 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4)
      t192 = t172 / (-0.2D1 + t164) * t189 * t44 * t50
      t194 = t122 * t192 / 0.8D1
      t195 = FJET(XB1, XB2, s, t167, -t169, -t170, t171, t185, t194)
      t197 = t38 * t109
      t201 = FJET(XB1, XB2, s, -t112, t114, 0.0D0, t108, -t120, t159)
      t203 = FJET(XB1, XB2, s, -t170, t171, t167, -t169, t185, t194)
      rrgg2qqbarht2s2e0 = t103 * t102 + t160 * t159 + t162 * t102 + t195
     # * 0.3141592653589793D1 * t197 * t192 / 0.8D1 + t201 * t159 + t203
     # * 0.3141592653589793D1 * t197 * t192 / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s2em1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t4 = -0.1D1 + x4
      t5 = t2 * t4
      t6 = t1 ** 2
      t7 = 0.3141592653589793D1 * t6
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t10 = t8 * t9
      t11 = 0.1D1 / x1
      t15 = rrgg2qqbarh21J2(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0
     #, x4)
      t22 = sin(x2 * 0.3141592653589793D1)
      t23 = t22 ** 2
      t24 = z ** 2
      t30 = log(-0.4D1 * t23 / t24 * x4 * t4)
      t37 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, x3, x
     #4)
      t44 = t7 * t10 * t11 / 0.8D1 + t7 * t8 * t15 / 0.16D2 + (-0.180D3 
     #* 0.3141592653589793D1 * lh - 0.90D2 * t30 * 0.3141592653589793D1)
     # * t6 * t10 / 0.1440D4 - t7 * t8 * (t37 - t9) / x3 / 0.16D2
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
      t68 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, x1, x2, 0.0D0, x
     #4)
      t72 = t7 * t8 * t51 * t57 * t66 * t68 * t11 / 0.8D1
      t73 = FJET(XB1, XB2, s, 0.0D0, t50, -t54, t56, -t62, -t72)
      t75 = t6 * t8
      t80 = t51 * t57 * t66 * t68 * t11
      t83 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t44)
      t85 = FJET(XB1, XB2, s, -t54, t56, 0.0D0, t50, -t62, -t72)
      rrgg2qqbarht2s2em1 = t45 * t44 - t73 * 0.3141592653589793D1 * t75 
     #* t80 / 0.8D1 + t83 * t44 - t85 * 0.3141592653589793D1 * t75 * t80
     # / 0.8D1

      end function



      doubleprecision function rrgg2qqbarht2s2em2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      t1 = -0.1D1 + z
      t2 = s * t1
      t3 = t2 * x4
      t5 = t2 * (-0.1D1 + x4)
      t6 = t1 ** 2
      t8 = 0.1D1 / s
      t9 = rrgg2qqbarh21J1(s, XB1, XB2, z, lh, wd, nf, 0.0D0, x2, 0.0D0,
     # x4)
      t12 = 0.3141592653589793D1 * t6 * t8 * t9 / 0.16D2
      t13 = FJET(XB1, XB2, s, 0.0D0, 0.0D0, t3, -t5, 0.0D0, t12)
      t16 = t6 * t8 * t9
      t18 = FJET(XB1, XB2, s, t3, -t5, 0.0D0, 0.0D0, 0.0D0, t12)
      rrgg2qqbarht2s2em2 = t13 * 0.3141592653589793D1 * t16 / 0.16D2 + t
     #18 * 0.3141592653589793D1 * t16 / 0.16D2

      end function



      doubleprecision function rrgg2qqbarht2s2em3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      rrgg2qqbarht2s2em3 = 0.0D0

      end function



      doubleprecision function rrgg2qqbarht2s2em4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision FJET
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      doubleprecision rrgg2qqbarh21J1
      doubleprecision rrgg2qqbarh21J2
      doubleprecision rrgg2qqbarh21J3
      doubleprecision rrgg2qqbarh21J4
      doubleprecision rrgg2qqbarh21J5
      doubleprecision rrgg2qqbarh21J6
      doubleprecision rrgg2qqbarh21J7
      rrgg2qqbarht2s2em4 = 0.0D0

      end function
  
 

      doubleprecision function rrgg2qqbarh21J1
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = x1 ** 2
      t8 = t6 * t7
      t9 = x3 ** 2
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t18 = t4 * t6
      t19 = x1 * t13
      t20 = 0.1D1 - x1
      t21 = 0.1D1 - x4
      t22 = t20 * t21
      t26 = t1 * t2
      t27 = t5 * x1
      t31 = x3 * x1
      t32 = t20 * x4
      t45 = t1 * t3 * t2 * t6 * t5
      t50 = cos(x2 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t13 * x4 * t21)
      t58 = t20 * (x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t50 * t54)
      rrgg2qqbarh21J1 = -wd * (0.306D3 * t4 * t8 * t9 + 0.306D3 * t4 * t
     #8 * t14 + 0.122D3 * t18 * t19 * t22 - 0.528D3 * t26 * t27 * t13 + 
     #0.122D3 * t18 * t31 * t32 - 0.528D3 * t26 * t27 * x3 - 0.17D2 * t1
     #8 * t32 * t19 - 0.275D3 * t45 * t7 * t13 * t58 - 0.275D3 * t45 * t
     #7 * x3 * t58 - 0.17D2 * t18 * t31 * t22) * nf / s / z / 0.31415926
     #53589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J2
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = x1 ** 2
      t8 = t6 * t7
      t9 = x3 ** 2
      t11 = t4 * t8 * t9
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t16 = t4 * t8 * t14
      t18 = t4 * t6
      t19 = x1 * t13
      t20 = 0.1D1 - x1
      t21 = 0.1D1 - x4
      t22 = t20 * t21
      t24 = t18 * t19 * t22
      t26 = t1 * t2
      t27 = t5 * x1
      t29 = t26 * t27 * t13
      t31 = x3 * x1
      t32 = t20 * x4
      t34 = t18 * t31 * t32
      t37 = t26 * t27 * x3
      t40 = t18 * t32 * t19
      t45 = t1 * t3 * t2 * t6 * t5
      t50 = cos(x2 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t13 * x4 * t21)
      t57 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t50 * t54
      t58 = t20 * t57
      t60 = t45 * t7 * t13 * t58
      t64 = t45 * t7 * x3 * t58
      t67 = t18 * t31 * t22
      t86 = -0.16D2 * t18 * x1 * t20 * t57 + 0.156D3 * t64 - 0.134D3 * t
     #34 - 0.134D3 * t24 + 0.490D3 * t40 + 0.752D3 * t29 + 0.752D3 * t37
     # + 0.490D3 * t67 - 0.190D3 * t11 - 0.190D3 * t16 + 0.156D3 * t60
      rrgg2qqbarh21J2 = -(0.2D1 * wd * (0.306D3 * t11 + 0.306D3 * t16 + 
     #0.122D3 * t24 - 0.528D3 * t29 + 0.122D3 * t34 - 0.528D3 * t37 - 0.
     #17D2 * t40 - 0.275D3 * t60 - 0.275D3 * t64 - 0.17D2 * t67) + wd * 
     #t86) * nf / s / z / 0.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J3
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = x1 ** 2
      t8 = t6 * t7
      t9 = x3 ** 2
      t11 = t4 * t8 * t9
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t16 = t4 * t8 * t14
      t18 = t4 * t6
      t19 = x1 * t13
      t20 = 0.1D1 - x1
      t21 = 0.1D1 - x4
      t22 = t20 * t21
      t24 = t18 * t19 * t22
      t26 = t1 * t2
      t27 = t5 * x1
      t29 = t26 * t27 * t13
      t31 = x3 * x1
      t32 = t20 * x4
      t34 = t18 * t31 * t32
      t37 = t26 * t27 * x3
      t40 = t18 * t32 * t19
      t45 = t1 * t3 * t2 * t6 * t5
      t50 = cos(x2 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t13 * x4 * t21)
      t57 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t50 * t54
      t58 = t20 * t57
      t60 = t45 * t7 * t13 * t58
      t64 = t45 * t7 * x3 * t58
      t67 = t18 * t31 * t22
      t75 = 0.16D2 * t18 * x1 * t20 * t57
      t86 = -t75 + 0.156D3 * t64 - 0.134D3 * t34 - 0.134D3 * t24 + 0.490
     #D3 * t40 + 0.752D3 * t29 + 0.752D3 * t37 + 0.490D3 * t67 - 0.190D3
     # * t11 - 0.190D3 * t16 + 0.156D3 * t60
      t95 = t5 * t20
      t107 = 0.32D2 * t1 + 0.28224D5 * t16 + 0.28224D5 * t11 - t75 + 0.2
     #5220D5 * t40 - 0.74296D5 * t29 + 0.28372D5 * t60 - 0.16D2 * t26 * 
     #t95 * x4 - 0.16D2 * t26 * t95 * t21 - 0.304D3 * t24 - 0.74296D5 * 
     #t37 + 0.28372D5 * t64 - 0.304D3 * t34 + 0.25220D5 * t67
      rrgg2qqbarh21J3 = -(0.3D1 * wd * (0.306D3 * t11 + 0.306D3 * t16 + 
     #0.122D3 * t24 - 0.528D3 * t29 + 0.122D3 * t34 - 0.528D3 * t37 - 0.
     #17D2 * t40 - 0.275D3 * t60 - 0.275D3 * t64 - 0.17D2 * t67) + 0.2D1
     # * wd * t86 + wd * t107) * nf / s / z / 0.3141592653589793D1 / 0.9
     #6D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J4
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = x1 ** 2
      t8 = t6 * t7
      t9 = x3 ** 2
      t11 = t4 * t8 * t9
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t16 = t4 * t8 * t14
      t18 = t4 * t6
      t19 = x1 * t13
      t20 = 0.1D1 - x1
      t21 = 0.1D1 - x4
      t22 = t20 * t21
      t24 = t18 * t19 * t22
      t26 = t1 * t2
      t27 = t5 * x1
      t29 = t26 * t27 * t13
      t31 = x3 * x1
      t32 = t20 * x4
      t34 = t18 * t31 * t32
      t37 = t26 * t27 * x3
      t40 = t18 * t32 * t19
      t45 = t1 * t3 * t2 * t6 * t5
      t50 = cos(x2 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t13 * x4 * t21)
      t57 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t50 * t54
      t58 = t20 * t57
      t60 = t45 * t7 * t13 * t58
      t64 = t45 * t7 * x3 * t58
      t67 = t18 * t31 * t22
      t75 = 0.16D2 * t18 * x1 * t20 * t57
      t86 = -t75 + 0.156D3 * t64 - 0.134D3 * t34 - 0.134D3 * t24 + 0.490
     #D3 * t40 + 0.752D3 * t29 + 0.752D3 * t37 + 0.490D3 * t67 - 0.190D3
     # * t11 - 0.190D3 * t16 + 0.156D3 * t60
      t95 = t5 * t20
      t107 = 0.32D2 * t1 + 0.28224D5 * t16 + 0.28224D5 * t11 - t75 + 0.2
     #5220D5 * t40 - 0.74296D5 * t29 + 0.28372D5 * t60 - 0.16D2 * t26 * 
     #t95 * x4 - 0.16D2 * t26 * t95 * t21 - 0.304D3 * t24 - 0.74296D5 * 
     #t37 + 0.28372D5 * t64 - 0.304D3 * t34 + 0.25220D5 * t67
      rrgg2qqbarh21J4 = -(0.4D1 * wd * (0.306D3 * t11 + 0.306D3 * t16 + 
     #0.122D3 * t24 - 0.528D3 * t29 + 0.122D3 * t34 - 0.528D3 * t37 - 0.
     #17D2 * t40 - 0.275D3 * t60 - 0.275D3 * t64 - 0.17D2 * t67) + 0.3D1
     # * wd * t86 + 0.2D1 * wd * t107) * nf / s / z / 0.3141592653589793
     #D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J5
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = x1 ** 2
      t8 = t6 * t7
      t9 = x3 ** 2
      t11 = t4 * t8 * t9
      t13 = 0.1D1 - x3
      t14 = t13 ** 2
      t16 = t4 * t8 * t14
      t18 = t4 * t6
      t19 = x1 * t13
      t20 = 0.1D1 - x1
      t21 = 0.1D1 - x4
      t22 = t20 * t21
      t24 = t18 * t19 * t22
      t26 = t1 * t2
      t27 = t5 * x1
      t29 = t26 * t27 * t13
      t31 = x3 * x1
      t32 = t20 * x4
      t34 = t18 * t31 * t32
      t37 = t26 * t27 * x3
      t40 = t18 * t32 * t19
      t45 = t1 * t3 * t2 * t6 * t5
      t50 = cos(x2 * 0.3141592653589793D1)
      t54 = Sqrt(x3 * t13 * x4 * t21)
      t57 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t50 * t54
      t58 = t20 * t57
      t60 = t45 * t7 * t13 * t58
      t64 = t45 * t7 * x3 * t58
      t67 = t18 * t31 * t22
      t75 = 0.16D2 * t18 * x1 * t20 * t57
      t86 = -t75 + 0.156D3 * t64 - 0.134D3 * t34 - 0.134D3 * t24 + 0.490
     #D3 * t40 + 0.752D3 * t29 + 0.752D3 * t37 + 0.490D3 * t67 - 0.190D3
     # * t11 - 0.190D3 * t16 + 0.156D3 * t60
      t95 = t5 * t20
      t107 = 0.32D2 * t1 + 0.28224D5 * t16 + 0.28224D5 * t11 - t75 + 0.2
     #5220D5 * t40 - 0.74296D5 * t29 + 0.28372D5 * t60 - 0.16D2 * t26 * 
     #t95 * x4 - 0.16D2 * t26 * t95 * t21 - 0.304D3 * t24 - 0.74296D5 * 
     #t37 + 0.28372D5 * t64 - 0.304D3 * t34 + 0.25220D5 * t67
      rrgg2qqbarh21J5 = -(0.5D1 * wd * (0.306D3 * t11 + 0.306D3 * t16 + 
     #0.122D3 * t24 - 0.528D3 * t29 + 0.122D3 * t34 - 0.528D3 * t37 - 0.
     #17D2 * t40 - 0.275D3 * t60 - 0.275D3 * t64 - 0.17D2 * t67) + 0.4D1
     # * wd * t86 + 0.3D1 * wd * t107) * nf / s / z / 0.3141592653589793
     #D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J6
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t2 = kappa2(x1, x2, x3, x4, z)
      t3 = t2 ** 2
      t4 = t1 * t3
      t5 = 0.1D1 - z
      t6 = t5 ** 2
      t7 = t4 * t6
      t8 = 0.1D1 - x1
      t13 = cos(x2 * 0.3141592653589793D1)
      t14 = 0.1D1 - x3
      t16 = 0.1D1 - x4
      t19 = Sqrt(x3 * t14 * x4 * t16)
      t22 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t13 * t19
      t25 = 0.16D2 * t7 * x1 * t8 * t22
      t29 = t1 * t3 * t2 * t6 * t5
      t30 = x1 ** 2
      t32 = t8 * t22
      t34 = t29 * t30 * x3 * t32
      t36 = x3 * x1
      t37 = t8 * x4
      t39 = t7 * t36 * t37
      t41 = x1 * t14
      t42 = t8 * t16
      t44 = t7 * t41 * t42
      t47 = t7 * t37 * t41
      t49 = t1 * t2
      t50 = t5 * x1
      t52 = t49 * t50 * t14
      t55 = t49 * t50 * x3
      t58 = t7 * t36 * t42
      t60 = t6 * t30
      t61 = x3 ** 2
      t63 = t4 * t60 * t61
      t65 = t14 ** 2
      t67 = t4 * t60 * t65
      t71 = t29 * t30 * t14 * t32
      t73 = -t25 + 0.156D3 * t34 - 0.134D3 * t39 - 0.134D3 * t44 + 0.490
     #D3 * t47 + 0.752D3 * t52 + 0.752D3 * t55 + 0.490D3 * t58 - 0.190D3
     # * t63 - 0.190D3 * t67 + 0.156D3 * t71
      t82 = t5 * t8
      t94 = 0.32D2 * t1 + 0.28224D5 * t67 + 0.28224D5 * t63 - t25 + 0.25
     #220D5 * t47 - 0.74296D5 * t52 + 0.28372D5 * t71 - 0.16D2 * t49 * t
     #82 * x4 - 0.16D2 * t49 * t82 * t16 - 0.304D3 * t44 - 0.74296D5 * t
     #55 + 0.28372D5 * t34 - 0.304D3 * t39 + 0.25220D5 * t58
      rrgg2qqbarh21J6 = -(0.5D1 * wd * t73 + 0.4D1 * wd * t94) * nf / s 
     #/ z / 0.3141592653589793D1 / 0.96D2

      end function
  
   
 

      doubleprecision function rrgg2qqbarh21J7
     &(s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4) 
      IMPLICIT DOUBLE PRECISION(t)
      doubleprecision s, XB1, XB2, z, lh, wd, nf, x1, x2, x3, x4
      doubleprecision KAPPA
      doubleprecision KAPPA2
      doubleprecision KAPPAF
      doubleprecision Log
      t1 = s ** 2
      t3 = kappa2(x1, x2, x3, x4, z)
      t4 = t3 ** 2
      t5 = t1 * t4
      t6 = 0.1D1 - z
      t7 = t6 ** 2
      t8 = x1 ** 2
      t9 = t7 * t8
      t10 = 0.1D1 - x3
      t11 = t10 ** 2
      t15 = x3 ** 2
      t19 = t5 * t7
      t20 = 0.1D1 - x1
      t25 = cos(x2 * 0.3141592653589793D1)
      t27 = 0.1D1 - x4
      t30 = Sqrt(x3 * t10 * x4 * t27)
      t33 = x3 + x4 - 0.2D1 * x3 * x4 - 0.2D1 * t25 * t30
      t37 = t20 * x4
      t38 = x1 * t10
      t42 = t1 * t3
      t43 = t6 * x1
      t50 = t1 * t4 * t3 * t7 * t6
      t52 = t20 * t33
      t56 = t6 * t20
      t63 = t20 * t27
      t74 = x3 * x1
      t81 = 0.32D2 * t1 + 0.28224D5 * t5 * t9 * t11 + 0.28224D5 * t5 * t
     #9 * t15 - 0.16D2 * t19 * x1 * t20 * t33 + 0.25220D5 * t19 * t37 * 
     #t38 - 0.74296D5 * t42 * t43 * t10 + 0.28372D5 * t50 * t8 * t10 * t
     #52 - 0.16D2 * t42 * t56 * x4 - 0.16D2 * t42 * t56 * t27 - 0.304D3 
     #* t19 * t38 * t63 - 0.74296D5 * t42 * t43 * x3 + 0.28372D5 * t50 *
     # t8 * x3 * t52 - 0.304D3 * t19 * t74 * t37 + 0.25220D5 * t19 * t74
     # * t63
      rrgg2qqbarh21J7 = -0.5D1 / 0.96D2 * wd * t81 * nf / s / z / 0.3141
     #592653589793D1

      end function
  
 