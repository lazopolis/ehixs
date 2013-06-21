
#include "QuarkObject.h"

QuarkObject::QuarkObject(double imq,double iGq,double iYq,double m_higgs)
{
mq_nominal=imq;
Yq=iYq;
Gq=iGq;
mq_sq=complex<double>(imq*imq,-imq*iGq);
Wq = 4.0*mq_sq/m_higgs/m_higgs;
Xq = - Wq/pow(sqrt(1.0-Wq)+1.0,2.0);
}
