#include "iostream"
#include "iomanip"
using namespace std;
#include "result_pair.h"


ostream& operator<<(ostream& stream, const ResultPair& rs)
{
    stream<<setw(11)<<setprecision(6)<<right<<rs.val()
    <<scientific<<setprecision(0)<<left<<"["<<rs.err()<<"] "
    <<setprecision(16)<<fixed;
    return stream;
}