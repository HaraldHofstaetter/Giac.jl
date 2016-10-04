// http://stackoverflow.com/questions/2045774/developing-c-wrapper-api-for-object-oriented-c-code


#include "giac.h"
#include "giac_c.h"
#include<string>

using namespace std;
using namespace giac;

extern "C" {

    void* get_context_ptr()
    {
         return reinterpret_cast<void*>( new context() );
    }

    _Gen* giac_new_int(int val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* giac_new_int64_t(int64_t val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* giac_new_int128_t(int128_t val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* giac_new_bigint(void* m)
    {
        mpz_t* m0 = reinterpret_cast<mpz_t*>(m);
        return reinterpret_cast<_Gen*>( new gen(*m0) );
    }

    _Gen* giac_new_double(double val)
    {
        return reinterpret_cast<_Gen*>( new gen(val) );
    }

    _Gen* giac_new_bigfloat(void* m)
    {
        mpfr_t* m0 = reinterpret_cast<mpfr_t*>(m);
        return reinterpret_cast<_Gen*>( new gen(real_object(*m0)) );
    }

    _Gen* giac_new_complex_int(int re, int im) 
    {
        return reinterpret_cast<_Gen*>( new gen(re,im) );
    }

    _Gen* giac_new_complex_double(double re, double im) /* -> complex */
    {
        return reinterpret_cast<_Gen*>( new gen(re, im) );
    }

    _Gen* giac_new_complex(_Gen* re, _Gen* im)
    {
        gen* re0 = reinterpret_cast<gen*>(re);
        gen* im0 = reinterpret_cast<gen*>(im);
        gen* r0 = new gen(gen(*re0, *im0));
        return reinterpret_cast<_Gen*>(r0);
    }    

    _Gen* giac_new_rational(_Gen* num, _Gen* den)
    {
        gen* num0 = reinterpret_cast<gen*>(num);
        gen* den0 = reinterpret_cast<gen*>(den);
        gen* r0 = new gen(fraction(*num0, *den0));
        return reinterpret_cast<_Gen*>(r0);
    }    

    _Gen* giac_new_symbolic(char *cs, void *cp)
    {
        context* cp0 = reinterpret_cast<context*>(cp);
        std::string s(cs);
        gen* r0 = new gen(s, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_new_ident(char *cs)
    {
        gen* r0 = new gen(identificateur(cs));
        return reinterpret_cast<_Gen*>(r0);
    }


    void giac_delete(_Gen* g)
    {
        delete reinterpret_cast<gen*>(g);
    }

    _Gen* giac_plus(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen* r0 = new gen();
        *r0 = (*a0) + (*b0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_minus(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen* r0 = new gen();
        *r0 = (*a0) - (*b0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_uminus(_Gen *a)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* r0 = new gen();
        *r0 = -(*a0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_times(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen* r0 = new gen();
        *r0 = (*a0) * (*b0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_rdiv(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        gen* r0 = new gen();
        *r0 = (*a0) / (*b0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_pow(_Gen *a, _Gen *b, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = pow(*a0, *b0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_real(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = re(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_imag(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = im(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_conj(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = conj(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_abs(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = abs(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }


    _Gen* giac_sqrt(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = sqrt(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_exp(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = exp(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_log(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = log(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_sin(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = sin(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_cos(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = cos(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_tan(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = tan(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_sinh(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = sinh(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_cosh(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = cosh(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_tanh(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = tanh(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_asin(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = asin(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_acos(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = acos(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_atan(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = atan(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_asinh(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = asinh(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_acosh(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = acosh(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_atanh(_Gen* a, void *cp)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = atanh(*a0, cp0);
        return reinterpret_cast<_Gen*>(r0);
    }


    
    _Gen* giac_eval(_Gen* g, int levels, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = g0->eval(levels, cp0);
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_evalf(_Gen* g, int levels, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = g0->evalf(levels, cp0);
        return reinterpret_cast<_Gen*>( r0 );
    }

    
    _Gen* giac_simplify(_Gen* g, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = simplify(*g0, cp0);
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_expand(_Gen* g, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = expand(*g0, cp0);
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_factor(_Gen* g, int with_sqrt, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        gen* r0 = new gen();
        *r0 = factor(*g0, with_sqrt!=0, cp0);
        return reinterpret_cast<_Gen*>( r0 );
    }
    

    char* giac_to_string(_Gen* g, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        string s(g0->print(cp0)); 
        char* cs = (char*) malloc(sizeof(char)*(s.length()+1));
        strcpy(cs, s.c_str());
        return cs;
    }

    char* giac_to_latex(_Gen* g, void *cp)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        context* cp0 = reinterpret_cast<context*>(cp);
        string s(gen2tex(*g0,cp0)); 
        char* cs = (char*) malloc(sizeof(char)*(s.length()+1));
        strcpy(cs, s.c_str());
        return cs;
    }

}
