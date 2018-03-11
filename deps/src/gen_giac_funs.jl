print("""
#include "giac/giac.h"
#include "giac_c.h"
#include<string>

using namespace std;
using namespace giac;

extern "C" {

    static _Gen* _error(const string &msg) 
    {
        cerr << msg << endl;
        return giac_undef();
    }


""")


open("giac_funs.txt","r") do f
for line in eachline(f)
fun=strip(line)
if fun[1]=='#'   #comment
    continue
end

print("""

    _Gen* giac_$fun(_Gen* g, void *cp)
    {
        gen* r0;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = _$fun(*g0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }
""")



end
end


println("""

}
""")
