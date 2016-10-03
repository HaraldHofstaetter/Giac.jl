// http://stackoverflow.com/questions/2045774/developing-c-wrapper-api-for-object-oriented-c-code


#include "giac.h"
#include "giac_c.h"

using namespace std;
using namespace giac;

extern "C" {

    _Gen* gen_new_int(int val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* gen_new_int64_t(int64_t val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* gen_new_int128_t(int128_t val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* gen_new_double(double val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* gen_new_int_int(int a, int b) /* -> complex */
    {
        return reinterpret_cast<_Gen*>( new gen(a, b) );
    }

    _Gen* gen_new_double_double(double a, double b) /* -> complex */
    {
        return reinterpret_cast<_Gen*>( new gen(a, b) );
    }

    _Gen* gen_new_fraction(_Gen* num, _Gen* den)
    {
        gen* num0 = reinterpret_cast<gen*>(num);
        gen* den0 = reinterpret_cast<gen*>(den);
        gen r0(fraction(*num0, *den0));
        return reinterpret_cast<_Gen*>( new gen(r0) );
    }    


    void gen_delete(_Gen* g)
    {
        delete reinterpret_cast<gen*>(g);
    }

    _Gen* gen_op_plus(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen r0 = (*a0) + (*b0);
        return reinterpret_cast<_Gen*>( new gen(r0) );
    }

    _Gen* gen_op_minus(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen r0 = (*a0) - (*b0);
        return reinterpret_cast<_Gen*>( new gen(r0) );
    }

    _Gen* gen_op_uminus(_Gen *a)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen r0 = -(*a0);
        return reinterpret_cast<_Gen*>( new gen(r0) );
    }

    _Gen* gen_op_times(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen r0 = (*a0) * (*b0);
        return reinterpret_cast<_Gen*>( new gen(r0) );
    }

    _Gen* gen_op_rdiv(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen r0 = (*a0) / (*b0);
        return reinterpret_cast<_Gen*>( new gen(r0) );
    }




    char* gen_to_c_string(_Gen* g)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        //string s(g0->print(context0)); 
        // the above does not work, don't know which context...
        ostringstream os;
        os << *g0;
        string s(os.str());
        char* cs = (char*) malloc(sizeof(char)*(s.length()+1));
        strcpy(cs, s.c_str());
        return cs;
    }

}
