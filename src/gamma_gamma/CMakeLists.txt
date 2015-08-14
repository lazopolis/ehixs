file( GLOB QQ2YYG_COEFFS_D qq2yyg/coeffs/*dbl.cpp )
file( GLOB QQ2YYG_COEFFS_Q qq2yyg/coeffs/*qpl.cpp )

if (CLN_FOUND)
    file( GLOB QQ2YYG_COEFFS_R qq2yyg/coeffs/*rtn.cpp )
endif (CLN_FOUND)

add_library(gammagamma
            ${QQ2YYG_COEFFS_D}
            ${QQ2YYG_COEFFS_Q}
            ${QQ2YYG_COEFFS_R}
            qq2gammagammaX
            gammagamma
            gammagamma_me
            qq2yyg/qq2yyg0
            qq2yyg/qq2yyg1
            qq2yyg/qq2yyg1rescue
            )
