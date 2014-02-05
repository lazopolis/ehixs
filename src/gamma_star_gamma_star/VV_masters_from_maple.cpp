#include <math.h>

double GstarGstarMeNNLOSoft::QQoc(double z,double zp,double w)
{
    if (z == zp) return 0.0;
    const double check = w-z*(1.0-zp);
    if (check>0.0) return Qoch(z,zp,w);
    else if (check<0.0) return Qoclow(z,zp,w);
    else if (check==0.0) return Qoczero(z,zp,w);
    else {
        cout<<"\n nan in two loop master QQoc: w-z*(1-zp)= "<<check<<endl;
        exit(0);
    }
}

double GstarGstarMeNNLOSoft::QQec(double z,double zp,double w)
{
    if (z == zp) return 0.0;
    const double check = w-z*(1.0-zp);
    if (check>0.0) return Qech(z,zp,w);
    else if (check<0.0) return Qeclow(z,zp,w);
    else if (check==0.0) return Qeczero(z,zp,w);
    else {
        cout<<"\n nan in two loop master QQec: w-z*(1-zp)= "<<check<<endl;
        exit(0);
    }
}

#include <math.h>

double GstarGstarMeNNLOSoft::Qoch (double z, double zp, double w)
{
  double t1;
  double t10;
  double t102;
  double t105;
  double t110;
  double t113;
  double t114;
  double t128;
  double t130;
  double t131;
  double t136;
  double t14;
  double t141;
  double t143;
  double t144;
  double t145;
  double t149;
  double t153;
  double t157;
  double t162;
  double t17;
  double t19;
  double t2;
  double t21;
  double t24;
  double t25;
  double t27;
  double t28;
  double t29;
  double t3;
  double t30;
  double t31;
  double t32;
  double t33;
  double t34;
  double t35;
  double t39;
  double t40;
  double t42;
  double t43;
  double t45;
  double t46;
  double t47;
  double t49;
  double t51;
  double t52;
  double t54;
  double t56;
  double t57;
  double t59;
  double t61;
  double t65;
  double t66;
  double t68;
  double t7;
  double t71;
  double t72;
  double t77;
  double t78;
  double t82;
  double t84;
  double t85;
  double t86;
  double t89;
  double t9;
  double t91;
  double t92;
  double t98;
  double t99;
  t1 = z * zp;
  t2 = 0.1e1 * z;
  t3 = t1 + w - t2;
  t7 = polylog(0.3e1, -0.1e1 * t3 / z);
  t9 = 0.1e1 / w;
  t10 = z - 0.1e1;
  t14 = polylog(0.3e1, -0.1e1 * t9 * t10 * t3);
  t17 = polylog(0.3e1, zp * t3 * t9);
  t19 = polylog(0.3e1, t9 * t3);
  t21 = -0.1e1 + zp;
  t24 = 0.1e1 * t21 * z * t9;
  t25 = polylog(0.3e1, -t24);
  t27 = log(w);
  t28 = t27 * t27;
  t29 = 0.500000000000000000000000000000e0 * t28;
  t30 = 0.1e1 * zp;
  t31 = t1 + w - t30;
  t32 = log(t31);
  t33 = 0.1e1 - t2;
  t34 = log(t33);
  t35 = 0.1e1 * t34;
  t39 = log(t3);
  t40 = -0.1e1 * t32 + t39;
  t42 = 0.1e1 - t30;
  t43 = log(t42);
  t45 = t1 + w;
  t46 = log(t45);
  t47 = t40 * t46;
  t49 = log(z);
  t51 = 0.1e1 * t27;
  t52 = log(zp);
  t54 = polylog(0.2e1, -t24);
  t56 = 0.1e1 * t49;
  t57 = 0.1e1 * t52;
  t59 = 0.1e1 / t45;
  t61 = polylog(0.2e1, z * t59);
  t65 = t31 / t10;
  t66 = polylog(0.2e1, t65);
  t68 = 0.1e1 * t43;
  t71 = t3 / t21;
  t72 = polylog(0.2e1, t71);
  t77 = 0.1e1 * zp * t10 * t9;
  t78 = polylog(0.2e1, -t77);
  t82 = polylog(0.2e1, zp * t59);
  t84 = t43 * t43;
  t85 = 0.500000000000000000000000000000e0 * t84;
  t86 = t40 * t43;
  t89 = t34 * t34;
  t91 = 0.1e1 * t39;
  t92 = t32 - t91;
  t98 = 0.500000000000000000000000000000e0 * t32;
  t99 = 0.500000000000000000000000000000e0 * t43;
  t102 = polylog(0.2e1, zp);
  t105 = polylog(0.2e1, z);
  t110 = t49 * t49;
  t113 = -0.1e1 * t7 + t14 + t17 - 0.2e1 * t19 - 0.1e1 * t25 + (-t29 + (t32 - t35) * t27 + t40 * t34 + t43 * t39 + t47 + 0.164493406684822643647241516665e1) * t49 + (t43 - t51 + t49 + t52 + t34) * t54 + (t27 - t56 - t57) * t61 + (t34 + t43 - t51) * t66 + (-t68 + t27 - t35) * t72 + (-t56 - t57 + t27 - t35 - t68) * t78 + (-t51 + t49 + t52) * t82 + (-t85 + t86 - 0.164493406684822643647241516665e1) * t34 + (-0.500000000000000000000000000000e0 * t89 + t92 * t34 + t85 + t92 * t43 + t92 * t46) * t27 + (-t98 + t99) * t89 + 0.2e1 * t27 * t102 - 0.2e1 * t27 * t105 + 0.500000000000000000000000000000e0 * t84 * t39 - 0.500000000000000000000000000000e0 * t110 * t39;
  t114 = t52 * t52;
  t128 = polylog(0.3e1, t31 * t9);
  t130 = polylog(0.3e1, t65);
  t131 = polylog(0.3e1, t71);
  t136 = polylog(0.3e1, -0.1e1 * t31 / zp);
  t141 = polylog(0.3e1, t33);
  t143 = polylog(0.3e1, t42);
  t144 = polylog(0.3e1, z);
  t145 = polylog(0.3e1, zp);
  t149 = polylog(0.3e1, z * t31 * t9);
  t153 = polylog(0.3e1, -t77);
  t157 = polylog(0.3e1, -0.1e1 * t9 * t21 * t31);
  t162 = 0.500000000000000000000000000000e0 * t114 * t32 + (t29 + (-t91 + t43) * t27 - 0.1e1 * t34 * t32 + t86 + t47 - 0.164493406684822643647241516665e1) * t52 + (0.500000000000000000000000000000e0 * t39 - t98 + 0.500000000000000000000000000000e0 * t34 - t99) * t28 + 0.2e1 * t128 + t130 - 0.1e1 * t131 + t136 - 0.333333333333333333333333333333e0 * t84 * t43 + 0.333333333333333333333333333333e0 * t89 * t34 - 0.1e1 * t141 + t143 + t144 - 0.1e1 * t145 - 0.1e1 * t149 - 0.166666666666666666666666666667e0 * t114 * t52 + t153 - 0.1e1 * t157 + 0.166666666666666666666666666667e0 * t110 * t49 + 0.164493406684822643647241516665e1 * t43;
  return(t113 + t162);
}
#include <math.h>

double GstarGstarMeNNLOSoft::Qoclow (double z, double zp, double w)
{
  double t1;
  double t10;
  double t101;
  double t103;
  double t105;
  double t107;
  double t11;
  double t112;
  double t113;
  double t115;
  double t118;
  double t12;
  double t132;
  double t133;
  double t136;
  double t137;
  double t14;
  double t143;
  double t145;
  double t146;
  double t147;
  double t15;
  double t151;
  double t156;
  double t158;
  double t162;
  double t167;
  double t17;
  double t172;
  double t174;
  double t175;
  double t179;
  double t180;
  double t19;
  double t20;
  double t23;
  double t24;
  double t25;
  double t28;
  double t30;
  double t31;
  double t32;
  double t33;
  double t34;
  double t35;
  double t36;
  double t37;
  double t38;
  double t39;
  double t42;
  double t44;
  double t45;
  double t46;
  double t47;
  double t49;
  double t5;
  double t50;
  double t52;
  double t53;
  double t54;
  double t55;
  double t56;
  double t58;
  double t6;
  double t61;
  double t63;
  double t65;
  double t66;
  double t68;
  double t7;
  double t71;
  double t76;
  double t79;
  double t8;
  double t80;
  double t83;
  double t86;
  double t9;
  double t94;
  t1 = z * zp;
  t5 = log(-0.1e1 * t1 - 0.1e1 * w + z);
  t6 = 0.500000000000000000000000000000e0 * t5;
  t7 = 0.1e1 * zp;
  t8 = t1 + w - t7;
  t9 = log(t8);
  t10 = 0.500000000000000000000000000000e0 * t9;
  t11 = log(w);
  t12 = 0.500000000000000000000000000000e0 * t11;
  t14 = log(zp);
  t15 = t14 * t14;
  t17 = t11 * t11;
  t19 = 0.1e1 - t7;
  t20 = log(t19);
  t23 = 0.1e1 * z;
  t24 = 0.1e1 - t23;
  t25 = log(t24);
  t28 = t20 * t20;
  t30 = 0.1e1 * t9;
  t31 = -t30 + t5;
  t32 = t31 * t20;
  t33 = t1 + w;
  t34 = log(t33);
  t35 = t34 * t34;
  t36 = 0.500000000000000000000000000000e0 * t35;
  t37 = t31 * t34;
  t38 = t5 * t5;
  t39 = 0.500000000000000000000000000000e0 * t38;
  t42 = 0.1e1 * t11;
  t44 = z - 0.1e1;
  t45 = 0.1e1 / t44;
  t46 = t8 * t45;
  t47 = polylog(0.2e1, t46);
  t49 = 0.1e1 * t20;
  t50 = 0.1e1 * t25;
  t52 = t1 + w - t23;
  t53 = -0.1e1 + zp;
  t54 = 0.1e1 / t53;
  t55 = t52 * t54;
  t56 = polylog(0.2e1, t55);
  t58 = log(z);
  t61 = -0.1e1 * t58 - 0.1e1 * t14 + t11 - t50 - t49;
  t63 = 0.1e1 / w;
  t65 = 0.1e1 * zp * t44 * t63;
  t66 = polylog(0.2e1, -t65);
  t68 = -t42 + t58 + t14;
  t71 = polylog(0.2e1, zp / t33);
  t76 = t58 * t58;
  t79 = polylog(0.3e1, -t65);
  t80 = polylog(0.2e1, zp);
  t83 = polylog(0.2e1, z);
  t86 = 0.500000000000000000000000000000e0 * t25;
  t94 = -0.1e1 * t5 + t9;
  t101 = t25 * t25;
  t103 = 0.1e1 / z;
  t105 = polylog(0.2e1, t103 * t33);
  t107 = 0.1e1 * t28;
  t112 = 0.1e1 * w * t54 * t103;
  t113 = polylog(0.2e1, -t112);
  t115 = 0.1e1 * t34;
  t118 = (-t6 + t10 + t12) * t15 + (-0.500000000000000000000000000000e0 * t17 + 0.2e1 * t20 * t11 - 0.1e1 * t25 * t9 - 0.500000000000000000000000000000e0 * t28 + t32 + t36 + t37 - 0.328986813369645287294483033329e1 - t39) * t14 + (t25 + t20 - t42) * t47 + (-t49 + t11 - t50) * t56 + t61 * t66 + t68 * t71 + 0.164493406684822643647241516665e1 * t20 - 0.333333333333333333333333333333e0 * t15 * t14 + 0.333333333333333333333333333333e0 * t76 * t58 + t79 + 0.2e1 * t11 * t80 - 0.2e1 * t11 * t83 + (-t86 + t6 - 0.150000000000000000000000000000e1 * t20 - t10) * t17 + ((t9 + t20) * t25 + 0.150000000000000000000000000000e1 * t28 + t94 * t20 - t36 + t94 * t34 + 0.328986813369645287294483033329e1) * t11 + (0.500000000000000000000000000000e0 * t20 - t10 - t6) * t101 + t68 * t105 + (-t107 + t32 - t39) * t25 + t61 * t113 + (-t49 + t12 - t6 - t86 - t115) * t76;
  t132 = 0.1e1 / zp;
  t133 = 0.1e1 / t52;
  t136 = polylog(0.3e1, t132 * t133 * w);
  t137 = polylog(0.3e1, -t112);
  t143 = polylog(0.3e1, t24);
  t145 = polylog(0.3e1, t19);
  t146 = polylog(0.3e1, z);
  t147 = polylog(0.3e1, zp);
  t151 = polylog(0.3e1, z * t8 * t63);
  t156 = polylog(0.3e1, -0.1e1 * w * t45 * t133);
  t158 = polylog(0.3e1, t133 * w);
  t162 = polylog(0.3e1, -0.1e1 * t52 * t103);
  t167 = polylog(0.3e1, -0.1e1 * t63 * t53 * t8);
  t172 = polylog(0.3e1, t8 * t63);
  t174 = polylog(0.3e1, t46);
  t175 = polylog(0.3e1, t55);
  t179 = polylog(0.3e1, -0.1e1 * t8 * t132);
  t180 = 0.500000000000000000000000000000e0 * t28 * t5 + ((-t49 - t115 + t11) * t14 - 0.150000000000000000000000000000e1 * t17 + (t9 + 0.2e1 * t20 + t34) * t11 + (-t30 + t5 - t49) * t25 - t107 + t20 * t5 + t36 + t37 - 0.164493406684822643647241516665e1) * t58 + t136 - 0.1e1 * t137 - 0.666666666666666666666666666667e0 * t28 * t20 + 0.166666666666666666666666666667e0 * t101 * t25 - 0.1e1 * t143 + t145 + t146 - 0.1e1 * t147 - 0.1e1 * t151 + t156 - 0.2e1 * t158 - 0.1e1 * t162 - 0.1e1 * t167 + 0.333333333333333333333333333333e0 * t17 * t11 + 0.2e1 * t172 + t174 - 0.1e1 * t175 + t179;
  return(t118 + t180);
}
#include <math.h>

double GstarGstarMeNNLOSoft::Qoczero (double z, double zp, double w)
{
  double t10;
  double t12;
  double t15;
  double t18;
  double t2;
  double t20;
  double t22;
  double t26;
  double t29;
  double t3;
  double t30;
  double t32;
  double t34;
  double t35;
  double t37;
  double t39;
  double t4;
  double t41;
  double t43;
  double t44;
  double t47;
  double t5;
  double t51;
  double t54;
  double t58;
  double t59;
  double t6;
  double t61;
  double t62;
  double t65;
  double t67;
  double t7;
  double t71;
  double t75;
  double t8;
  double t9;
  t2 = 0.1e1 - 0.1e1 * z;
  t3 = log(t2);
  t4 = 0.1e1 * t3;
  t5 = 0.1e1 * zp;
  t6 = 0.1e1 - t5;
  t7 = log(t6);
  t8 = 0.2e1 * t7;
  t9 = log(z);
  t10 = 0.1e1 * t9;
  t12 = polylog(0.2e1, z);
  t15 = polylog(0.2e1, zp);
  t18 = log(zp);
  t20 = 0.1e1 / z;
  t22 = polylog(0.2e1, zp * t20);
  t26 = z - 0.1e1;
  t29 = 0.1e1 / (-0.1e1 + zp);
  t30 = t29 * t20;
  t32 = polylog(0.2e1, zp * t26 * t30);
  t34 = polylog(0.3e1, z);
  t35 = polylog(0.3e1, zp);
  t37 = z - t5;
  t39 = polylog(0.3e1, t37 * t20);
  t41 = 0.1e1 / t26;
  t43 = polylog(0.3e1, t37 * t41);
  t44 = 0.1e1 / zp;
  t47 = polylog(0.3e1, t41 * t44 * t37);
  t51 = polylog(0.3e1, -0.1e1 * t44 * t37);
  t54 = polylog(0.3e1, -0.1e1 * t37 * t29);
  t58 = polylog(0.3e1, -0.1e1 * t30 * t37);
  t59 = polylog(0.3e1, t2);
  t61 = polylog(0.3e1, t6);
  t62 = t9 * t9;
  t65 = t3 * t3;
  t67 = log(t37);
  t71 = t7 * t7;
  t75 = t18 * t18;
  return((-t4 - t8 - t10) * t12 + (t9 + t8 + t3) * t15 + (-0.1e1 * t7 + t9 + t18 - t4) * t22 + (-0.1e1 * t18 - t10) * t32 + t34 - 0.1e1 * t35 - 0.1e1 * t39 + t43 - 0.1e1 * t47 + t51 - 0.1e1 * t54 + t58 - 0.1e1 * t59 + t61 - 0.150000000000000000000000000000e1 * t62 * t7 + (-0.1e1 * t65 + 0.2e1 * t7 * t67 + t18 * t7 - 0.500000000000000000000000000000e0 * t71) * t9 + 0.500000000000000000000000000000e0 * t3 * t75 + (0.500000000000000000000000000000e0 * t65 + t71 - 0.2e1 * t3 * t67) * t18 + 0.164493406684822643647241516665e1 * t7 + 0.164493406684822643647241516665e1 * t3);
}
#include <math.h>

double GstarGstarMeNNLOSoft::Qech (double z, double zp, double w)
{
  double t1;
  double t10;
  double t102;
  double t103;
  double t104;
  double t105;
  double t106;
  double t107;
  double t108;
  double t109;
  double t11;
  double t110;
  double t111;
  double t12;
  double t124;
  double t125;
  double t127;
  double t129;
  double t130;
  double t131;
  double t133;
  double t135;
  double t136;
  double t138;
  double t139;
  double t14;
  double t142;
  double t146;
  double t148;
  double t152;
  double t154;
  double t158;
  double t16;
  double t167;
  double t17;
  double t170;
  double t175;
  double t18;
  double t184;
  double t19;
  double t197;
  double t198;
  double t199;
  double t2;
  double t20;
  double t23;
  double t25;
  double t26;
  double t28;
  double t3;
  double t32;
  double t33;
  double t37;
  double t41;
  double t45;
  double t47;
  double t48;
  double t49;
  double t5;
  double t50;
  double t51;
  double t52;
  double t53;
  double t54;
  double t55;
  double t56;
  double t57;
  double t58;
  double t59;
  double t61;
  double t62;
  double t64;
  double t66;
  double t67;
  double t68;
  double t7;
  double t71;
  double t74;
  double t77;
  double t78;
  double t80;
  double t81;
  double t82;
  double t86;
  double t87;
  double t89;
  double t9;
  double t91;
  double t92;
  double t93;
  double t96;
  double t97;
  double t98;
  double t99;
  t1 = z * zp;
  t2 = t1 + w;
  t3 = log(t2);
  t5 = z - 0.1e1;
  t7 = 0.1e1 / w;
  t9 = 0.1e1 * zp * t5 * t7;
  t10 = polylog(0.3e1, -t9);
  t11 = 0.1e1 / t2;
  t12 = t11 * z;
  t14 = polylog(0.3e1, t12 * zp);
  t16 = 0.1e1 * z;
  t17 = 0.1e1 * zp;
  t18 = t1 + w - t16 - t17 + 0.1e1;
  t19 = 0.1e1 / t18;
  t20 = -0.1e1 + zp;
  t23 = polylog(0.3e1, t19 * t20 * t5);
  t25 = w * t11;
  t26 = polylog(0.3e1, t25);
  t28 = t1 + w - t17;
  t32 = polylog(0.3e1, -0.1e1 * t28 / zp);
  t33 = t1 + w - t16;
  t37 = polylog(0.3e1, -0.1e1 * t33 / z);
  t41 = polylog(0.3e1, -0.1e1 * t7 * t5 * t33);
  t45 = polylog(0.3e1, zp * t33 * t7);
  t47 = log(z);
  t48 = 0.1e1 * t47;
  t49 = log(zp);
  t50 = 0.1e1 * t49;
  t51 = log(w);
  t52 = 0.1e1 - t16;
  t53 = log(t52);
  t54 = 0.1e1 * t53;
  t55 = 0.1e1 - t17;
  t56 = log(t55);
  t57 = 0.1e1 * t56;
  t58 = -t48 - t50 + t51 - t54 - t57;
  t59 = polylog(0.2e1, -t9);
  t61 = 0.1e1 * t51;
  t62 = -t61 + t47 + t49;
  t64 = polylog(0.2e1, t11 * zp);
  t66 = 0.240411380631918857079947632302e1 - 0.328986813369645287294483033329e1 * t3 + t10 - 0.2e1 * t14 - 0.2e1 * t23 - 0.2e1 * t26 + t32 + t37 - 0.1e1 * t41 - 0.1e1 * t45 + t58 * t59 + t62 * t64;
  t67 = polylog(0.3e1, z);
  t68 = polylog(0.3e1, zp);
  t71 = polylog(0.3e1, z * t28 * t7);
  t74 = polylog(0.3e1, t28 * t7);
  t77 = t28 / t5;
  t78 = polylog(0.3e1, t77);
  t80 = t33 / t20;
  t81 = polylog(0.3e1, t80);
  t82 = log(t33);
  t86 = log(t28);
  t87 = 0.500000000000000000000000000000e0 * t86;
  t89 = t51 * t51;
  t91 = 0.2e1 * t89;
  t92 = log(t18);
  t93 = 0.3e1 * t3;
  t96 = t53 * t53;
  t97 = 0.375000000000000000000000000000e0 * t96;
  t98 = 0.1e1 * t86;
  t99 = 0.750000000000000000000000000000e0 * t56;
  t102 = t56 * t56;
  t103 = 0.375000000000000000000000000000e0 * t102;
  t104 = 0.1e1 * t82;
  t105 = -t98 - t104;
  t106 = t105 * t56;
  t107 = t3 * t3;
  t108 = 0.500000000000000000000000000000e0 * t107;
  t109 = t105 * t3;
  t110 = t92 * t92;
  t111 = 0.500000000000000000000000000000e0 * t110;
  t124 = 0.1e1 * t20 * z * t7;
  t125 = polylog(0.2e1, -t124);
  t127 = polylog(0.2e1, t12);
  t129 = -t50 - t54 - t57 - t48;
  t130 = w * t19;
  t131 = polylog(0.2e1, t130);
  t133 = t67 + t68 - 0.1e1 * t71 + 0.2e1 * t74 + t78 + t81 + (-0.500000000000000000000000000000e0 * t82 - 0.2e1 * t53 - 0.2e1 * t56 - t87) * t89 + (-t91 + (t92 + t56 + t93 + t82) * t51 - t97 + (-t98 - t99) * t53 - t103 + t106 - t108 + t109 - t111) * t49 + (-t91 + (t92 + t86 + t93 + t53) * t51 - t97 + (-t98 - t104 - t99) * t53 - t103 - 0.1e1 * t56 * t82 - t108 + t109 - t111) * t47 + t58 * t125 + t62 * t127 + t129 * t131;
  t135 = t53 + t56 - t61;
  t136 = polylog(0.2e1, t80);
  t138 = 0.500000000000000000000000000000e0 * t102;
  t139 = 0.150000000000000000000000000000e1 * t110;
  t142 = polylog(0.2e1, t25);
  t146 = polylog(0.2e1, t77);
  t148 = t47 * t47;
  t152 = polylog(0.3e1, t33 * t7);
  t154 = polylog(0.3e1, -t124);
  t158 = polylog(0.3e1, -0.1e1 * t7 * t20 * t28);
  t167 = t135 * t136 + (t138 + t106 - t139 - t108 + 0.493480220054467930941724549994e1) * t53 + t129 * t142 + (-t139 - t108 + 0.493480220054467930941724549994e1) * t56 + t135 * t146 - 0.166666666666666666666666666667e0 * t148 * t47 + 0.2e1 * t152 + t154 - 0.1e1 * t158 + 0.666666666666666666666666666667e0 * t89 * t51 + (-t87 + 0.500000000000000000000000000000e0 * t56) * t96 - 0.500000000000000000000000000000e0 * t102 * t82;
  t170 = t49 * t49;
  t175 = t82 + 0.3e1 * t92 + t86 + t3;
  t184 = polylog(0.3e1, t130);
  t197 = polylog(0.3e1, t52);
  t198 = polylog(0.3e1, t55);
  t199 = 0.500000000000000000000000000000e0 * t148 * t82 + 0.500000000000000000000000000000e0 * t170 * t86 + (-0.500000000000000000000000000000e0 * t96 + t53 * t175 - t138 + t175 * t56 - 0.2e1 * t107 + (t86 + t82) * t3 - 0.1e1 * t110 + 0.657973626739290574588966066659e1) * t51 - 0.2e1 * t184 - 0.328986813369645287294483033329e1 * t92 + 0.666666666666666666666666666667e0 * t110 * t92 - 0.166666666666666666666666666667e0 * t170 * t49 + 0.666666666666666666666666666667e0 * t107 * t3 + 0.333333333333333333333333333333e0 * t102 * t56 + 0.333333333333333333333333333333e0 * t96 * t53 + t197 + t198;
  return(t66 + t133 + t167 + t199);
}
#include <math.h>

double GstarGstarMeNNLOSoft::Qeclow (double z, double zp, double w)
{
  double t1;
  double t10;
  double t11;
  double t111;
  double t114;
  double t119;
  double t12;
  double t120;
  double t123;
  double t128;
  double t129;
  double t14;
  double t141;
  double t144;
  double t150;
  double t152;
  double t153;
  double t154;
  double t155;
  double t158;
  double t16;
  double t161;
  double t163;
  double t164;
  double t168;
  double t171;
  double t172;
  double t174;
  double t175;
  double t176;
  double t178;
  double t179;
  double t18;
  double t182;
  double t183;
  double t187;
  double t19;
  double t190;
  double t194;
  double t2;
  double t200;
  double t203;
  double t206;
  double t21;
  double t210;
  double t214;
  double t22;
  double t24;
  double t25;
  double t28;
  double t3;
  double t30;
  double t32;
  double t33;
  double t34;
  double t36;
  double t37;
  double t40;
  double t43;
  double t45;
  double t46;
  double t50;
  double t52;
  double t53;
  double t54;
  double t55;
  double t56;
  double t57;
  double t58;
  double t6;
  double t62;
  double t63;
  double t64;
  double t65;
  double t66;
  double t67;
  double t68;
  double t69;
  double t70;
  double t74;
  double t79;
  double t8;
  double t80;
  double t81;
  double t82;
  double t83;
  double t84;
  double t85;
  double t86;
  double t87;
  double t88;
  double t89;
  double t9;
  double t90;
  double t92;
  double t93;
  double t94;
  double t96;
  double t98;
  t1 = z * zp;
  t2 = t1 + w;
  t3 = 0.1e1 / t2;
  t6 = polylog(0.3e1, t3 * z * zp);
  t8 = 0.1e1 * z;
  t9 = 0.1e1 * zp;
  t10 = t1 + w - t8 - t9 + 0.1e1;
  t11 = 0.1e1 / t10;
  t12 = -0.1e1 + zp;
  t14 = z - 0.1e1;
  t16 = polylog(0.3e1, t11 * t12 * t14);
  t18 = w * t3;
  t19 = polylog(0.3e1, t18);
  t21 = w * t11;
  t22 = polylog(0.3e1, t21);
  t24 = log(t10);
  t25 = t24 * t24;
  t28 = 0.1e1 / t12;
  t30 = 0.1e1 / z;
  t32 = 0.1e1 * w * t28 * t30;
  t33 = polylog(0.3e1, -t32);
  t34 = 0.1e1 / t14;
  t36 = t1 + w - t8;
  t37 = 0.1e1 / t36;
  t40 = polylog(0.3e1, -0.1e1 * w * t34 * t37);
  t43 = polylog(0.3e1, t37 * w);
  t45 = log(z);
  t46 = t45 * t45;
  t50 = 0.1e1 / w;
  t52 = 0.1e1 * zp * t14 * t50;
  t53 = polylog(0.3e1, -t52);
  t54 = 0.240411380631918857079947632302e1 - 0.2e1 * t6 - 0.2e1 * t16 - 0.2e1 * t19 - 0.2e1 * t22 + 0.666666666666666666666666666667e0 * t25 * t24 + t33 - 0.1e1 * t40 + 0.2e1 * t43 - 0.333333333333333333333333333333e0 * t46 * t45 + t53;
  t55 = log(w);
  t56 = t55 * t55;
  t57 = 0.1e1 * t56;
  t58 = log(t2);
  t62 = 0.1e1 - t8;
  t63 = log(t62);
  t64 = t63 * t63;
  t65 = 0.375000000000000000000000000000e0 * t64;
  t66 = t1 + w - t9;
  t67 = log(t66);
  t68 = 0.1e1 * t67;
  t69 = 0.1e1 - t9;
  t70 = log(t69);
  t74 = t70 * t70;
  t79 = log(-0.1e1 * t1 - 0.1e1 * w + z);
  t80 = 0.1e1 * t79;
  t81 = -t80 - t68;
  t82 = t81 * t70;
  t83 = t58 * t58;
  t84 = 0.1e1 * t83;
  t85 = t81 * t58;
  t86 = 0.500000000000000000000000000000e0 * t25;
  t87 = t79 * t79;
  t88 = 0.500000000000000000000000000000e0 * t87;
  t89 = -t57 + (0.3e1 * t58 + t24) * t55 - t65 + (-t68 - 0.750000000000000000000000000000e0 * t70) * t63 + 0.125000000000000000000000000000e0 * t74 + t82 - t84 + t85 - t86 + t88 + 0.164493406684822643647241516665e1;
  t90 = log(zp);
  t92 = 0.500000000000000000000000000000e0 * t67;
  t93 = 0.500000000000000000000000000000e0 * t55;
  t94 = 0.500000000000000000000000000000e0 * t79;
  t96 = t90 * t90;
  t98 = 0.1e1 * t55;
  t111 = (t70 + t58 - t98) * t90 - t57 + (t24 + t67 + 0.2e1 * t58 - 0.2e1 * t70) * t55 - t65 + (-t80 - t68 + 0.250000000000000000000000000000e0 * t70) * t63 + 0.625000000000000000000000000000e0 * t74 - 0.1e1 * t70 * t79 - t84 + t85 - t86 + 0.328986813369645287294483033329e1;
  t114 = polylog(0.2e1, -t32);
  t119 = 0.1e1 * t45;
  t120 = 0.1e1 * t90;
  t123 = polylog(0.2e1, t30 * t2);
  t128 = 0.3e1 * t24;
  t129 = 0.1e1 * t70;
  t141 = 0.1e1 / zp;
  t144 = polylog(0.3e1, t141 * t37 * w);
  t150 = t89 * t90 + (t92 - t93 + t94) * t96 + t111 * t45 + (t70 - t98 + t45 + t90 + t63) * t114 + (t70 + 0.500000000000000000000000000000e0 * t63 + t58 + t94 - t93) * t46 + (t55 - t119 - t120) * t123 - 0.500000000000000000000000000000e0 * t74 * t79 + (-0.1e1 * t64 + (t128 + t67 + t58 - t129) * t63 - 0.150000000000000000000000000000e1 * t74 + (t58 + t79 + t67 + t128) * t70 - 0.150000000000000000000000000000e1 * t83 + (t79 + t67) * t58 + 0.328986813369645287294483033329e1 - 0.1e1 * t25) * t55 - 0.1e1 * t144 + 0.666666666666666666666666666667e0 * t83 * t58 - 0.328986813369645287294483033329e1 * t58 - 0.328986813369645287294483033329e1 * t24;
  t152 = polylog(0.3e1, t62);
  t153 = polylog(0.3e1, t69);
  t154 = polylog(0.3e1, z);
  t155 = polylog(0.3e1, zp);
  t158 = polylog(0.3e1, z * t66 * t50);
  t161 = polylog(0.3e1, t66 * t50);
  t163 = t66 * t34;
  t164 = polylog(0.3e1, t163);
  t168 = 0.1e1 * t63;
  t171 = -t120 - t168 - t129 - t119;
  t172 = polylog(0.2e1, t21);
  t174 = t63 + t70 - t98;
  t175 = t36 * t28;
  t176 = polylog(0.2e1, t175);
  t178 = 0.500000000000000000000000000000e0 * t83;
  t179 = 0.150000000000000000000000000000e1 * t25;
  t182 = t152 + t153 + t154 + t155 - 0.1e1 * t158 + 0.2e1 * t161 + t164 + (t94 - t92 + 0.500000000000000000000000000000e0 * t70) * t64 + (-t168 - t92 - t129 - t94) * t56 + t171 * t172 + t174 * t176 + (t74 + t82 + 0.328986813369645287294483033329e1 - t178 - t179 + t88) * t63;
  t183 = polylog(0.2e1, t18);
  t187 = polylog(0.2e1, t163);
  t190 = polylog(0.2e1, -t52);
  t194 = polylog(0.2e1, zp * t3);
  t200 = polylog(0.3e1, t175);
  t203 = polylog(0.3e1, -0.1e1 * t66 * t141);
  t206 = polylog(0.3e1, -0.1e1 * t30 * t36);
  t210 = polylog(0.3e1, -0.1e1 * t50 * t12 * t66);
  t214 = t171 * t183 + (-t179 - t178 + 0.493480220054467930941724549994e1) * t70 + t174 * t187 + (-t119 - t120 + t55 - t168 - t129) * t190 + (-t98 + t45 + t90) * t194 + 0.666666666666666666666666666667e0 * t74 * t70 + 0.500000000000000000000000000000e0 * t64 * t63 + t200 + t203 + t206 - 0.1e1 * t210 + 0.333333333333333333333333333333e0 * t56 * t55;
  return(t54 + t150 + t182 + t214);
}
#include <math.h>

double GstarGstarMeNNLOSoft::Qeczero (double z, double zp, double w)
{
  double t1;
  double t10;
  double t12;
  double t15;
  double t17;
  double t2;
  double t20;
  double t22;
  double t26;
  double t29;
  double t3;
  double t30;
  double t32;
  double t34;
  double t36;
  double t38;
  double t4;
  double t40;
  double t42;
  double t44;
  double t45;
  double t48;
  double t52;
  double t56;
  double t6;
  double t60;
  double t61;
  double t63;
  double t65;
  double t69;
  double t7;
  double t71;
  double t75;
  double t8;
  double t82;
  double t9;
  double t94;
  t1 = 0.1e1 * zp;
  t2 = 0.1e1 - t1;
  t3 = log(t2);
  t4 = 0.1e1 * t3;
  t6 = 0.1e1 - 0.1e1 * z;
  t7 = log(t6);
  t8 = 0.2e1 * t7;
  t9 = log(zp);
  t10 = 0.1e1 * t9;
  t12 = polylog(0.2e1, z);
  t15 = polylog(0.2e1, zp);
  t17 = log(z);
  t20 = 0.1e1 / z;
  t22 = polylog(0.2e1, zp * t20);
  t26 = z - 0.1e1;
  t29 = 0.1e1 / (-0.1e1 + zp);
  t30 = t29 * t20;
  t32 = polylog(0.2e1, zp * t26 * t30);
  t34 = polylog(0.3e1, z);
  t36 = polylog(0.3e1, zp);
  t38 = z - t1;
  t40 = polylog(0.3e1, t38 * t20);
  t42 = 0.1e1 / t26;
  t44 = polylog(0.3e1, t38 * t42);
  t45 = 0.1e1 / zp;
  t48 = polylog(0.3e1, t42 * t45 * t38);
  t52 = polylog(0.3e1, -0.1e1 * t45 * t38);
  t56 = polylog(0.3e1, -0.1e1 * t38 * t29);
  t60 = polylog(0.3e1, -0.1e1 * t30 * t38);
  t61 = polylog(0.3e1, t6);
  t63 = polylog(0.3e1, t2);
  t65 = t17 * t17;
  t69 = t7 * t7;
  t71 = t3 * t3;
  t75 = log(t38);
  t82 = t9 * t9;
  t94 = 0.480822761263837714159895264604e1 - 0.1e1 * t56 + t60 - 0.1e1 * t61 - 0.1e1 * t63 - 0.150000000000000000000000000000e1 * t3 * t65 + (t9 * t3 - 0.137500000000000000000000000000e1 * t69 - 0.875000000000000000000000000000e0 * t71 + 0.250000000000000000000000000000e0 * t7 * t3 + 0.2e1 * t3 * t75) * t17 + (t3 + 0.500000000000000000000000000000e0 * t7) * t82 + (0.125000000000000000000000000000e0 * t69 + (-0.2e1 * t75 + 0.250000000000000000000000000000e0 * t3) * t7 - 0.375000000000000000000000000000e0 * t71) * t9 + 0.328986813369645287294483033329e1 * t7 + 0.328986813369645287294483033329e1 * t3;
  return((-t4 - t8 - t10) * t12 + (t9 + t8 + t3) * t15 + (-t4 + t17 + t9 - 0.1e1 * t7) * t22 + (-t10 - 0.1e1 * t17) * t32 - 0.1e1 * t34 - 0.1e1 * t36 - 0.1e1 * t40 + t44 - 0.1e1 * t48 + t52 + t94);
}
