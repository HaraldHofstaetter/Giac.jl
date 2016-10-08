// http://stackoverflow.com/questions/2045774/developing-c-wrapper-api-for-object-oriented-c-code


#include "giac.h"
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

    void* giac_context_ptr(void)
    {
         context* r0;
         try{ 
            r0 = new context(); 
         } 
         catch(runtime_error &e) {return 0;}
         return reinterpret_cast<void*>( r0 );
    }


    _Gen* giac_new_int(int val)
    {
        gen* r0;
        try{ 
            r0 = new gen(val); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_int64_t(int64_t val)
    {
        gen* r0;
        try{ 
            r0 = new gen(val); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_int128_t(int128_t val)
    {
        gen* r0;
        try{ 
            r0 = new gen(val); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_bigint(void* m)
    {
        gen* r0;
        try{ 
            mpz_t* m0 = reinterpret_cast<mpz_t*>(m);
            r0 = new gen(*m0); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_double(double val)
    {
        gen* r0;
        try{ 
            r0 = new gen(val); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_bigfloat(void* m)
    {
        gen* r0;
        try{ 
            mpfr_t* m0 = reinterpret_cast<mpfr_t*>(m);
            r0 = new gen(real_object(*m0)); 
            //r0 = new gen(*m0); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_complex_int(int re, int im) 
    {
        gen* r0;
        try{ 
            r0 = new gen(re, im); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_complex_double(double re, double im) /* -> complex */
    {
        gen* r0;
        try{ 
            r0 = new gen(re, im); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_new_complex(_Gen* re, _Gen* im)
    {
        gen* r0;
        try{ 
            gen* re0 = reinterpret_cast<gen*>(re);
            gen* im0 = reinterpret_cast<gen*>(im);
            r0 = new gen(*re0, *im0); 
        } 
        catch(runtime_error &e) {return _error(e.what());} 
        return reinterpret_cast<_Gen*>(r0);
    }    

    _Gen* giac_new_rational(_Gen* num, _Gen* den)
    {
        gen* r0;
        try{ 
            gen* num0 = reinterpret_cast<gen*>(num);
            gen* den0 = reinterpret_cast<gen*>(den);
            r0 = new gen(fraction(*num0, *den0)); 
        } 
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }    

    _Gen* giac_new_symbolic(char *cs, void *cp)
    {
        gen* r0;
        try{
            context* cp0 = reinterpret_cast<context*>(cp);
            std::string s(cs);
            r0 = new gen(s, cp0); 
        }
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_new_ident(char *cs)
    {
        gen* r0;
        try{ 
            r0 = new gen(identificateur(cs)); 
        }
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_new_vector(_Gen* v[], int len, short int subtype)
    {
        gen* r0;
        try{
            vecteur v0(len);
            for (int i=0; i<len; i++) {
                v0[i] = *(reinterpret_cast<gen*>(v[i]));
            }
            r0 = new gen(v0, subtype);
        }
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_copy_gen(_Gen* g)
    {
        gen* r0;
        try{ 
            gen* g0 = reinterpret_cast<gen*>(g);
            r0 = new gen(*g0); 
        } 
        catch(runtime_error &e) {return _error(e.what());} 
        return reinterpret_cast<_Gen*>(r0);
    }  

    int giac_get_int(_Gen* g)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        if (g0->type == _INT_) {
             return g0->val;
        }
        else {
            cerr << "giac_get_int: type _INT_ expected." << endl;
            return 0;
        }    
    }

    double giac_get_double(_Gen* g)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        if (g0->type == _DOUBLE_) {
             return g0->_DOUBLE_val;
        }
        else if (g0->type == _FLOAT_) {
             return g0->_FLOAT_val;
        }
        else {
            cerr << "giac_get_double: type _DOUBLE_ expected." << endl;
            return 0;
        }    
    }

    void* giac_get_bigint(_Gen* g)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        if (g0->type == _ZINT) {
             return g0->_ZINTptr;
        }
        else {
            cerr << "giac_get_int: type _ZINT expected." << endl;
            return 0;
        }    
    }

    void* giac_get_bigfloat(_Gen* g)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        if (g0->type == _REAL) {
             return g0->_REALptr->inf;
        }
        else {
            cerr << "giac_get_int: type _REAL expected." << endl;
            return 0;
        }    
    }



    void giac_change_subtype(_Gen* g, int subtype)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        g0->change_subtype(subtype);
    }




    void giac_delete(_Gen* g)
    {
        delete reinterpret_cast<const gen*>(g);
    }

    
    _Gen* giac_undef(void) {
        return reinterpret_cast<_Gen*>(const_cast<gen*>(&undef));
    }

    _Gen* giac_plus(_Gen *a, _Gen *b)
    {
        gen* r0;
        try{
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
           r0 = new gen();
           *r0 = (*a0) + (*b0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_minus(_Gen *a, _Gen *b)
    {
        gen* r0; 
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            gen* b0 = reinterpret_cast<gen*>(b);
            r0 = new gen();
            *r0 = (*a0) - (*b0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_uminus(_Gen *a)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            r0 = new gen();
            *r0 = -(*a0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_times(_Gen *a, _Gen *b)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            gen* b0 = reinterpret_cast<gen*>(b);
            r0 = new gen();
            *r0 = (*a0) * (*b0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_rdiv(_Gen *a, _Gen *b)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            gen* b0 = reinterpret_cast<gen*>(b);
            r0 = new gen();
            *r0 = (*a0) / (*b0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_pow(_Gen *a, _Gen *b, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            gen* b0 = reinterpret_cast<gen*>(b);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = pow(*a0, *b0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    int giac_equal_bool(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        return (*a0)==(*b0)?1:0;
    }

    int giac_greater_than(_Gen *a, _Gen *b)
    {
        gen* a0 = reinterpret_cast<gen*>(a);
        gen* b0 = reinterpret_cast<gen*>(b);
        return (*a0)>(*b0)?1:0;
    }

    int giac_size1(_Gen *g)
    {
        gen* g0 = reinterpret_cast<gen*>(g);
        switch(g0->type) {
           case _SYMB:
                return g0->symb_size();
           case _VECT:
                return g0->_VECTptr->size();
           default:
                return 1;
        }
    }

    _Gen* giac_getindex(_Gen* a, int i)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            r0 = new gen();
            *r0 = (*a0)[i];
        }    
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_real(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = re(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_imag(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = im(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_conj(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = conj(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_abs(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = abs(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }


    _Gen* giac_sqrt(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = sqrt(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_exp(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = exp(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_log(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = log(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_sin(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = sin(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_cos(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = cos(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_tan(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = tan(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_sinh(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = sinh(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_cosh(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = cosh(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_tanh(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = tanh(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_asin(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = asin(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_acos(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = acos(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_atan(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = atan(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_asinh(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = asinh(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_acosh(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = acosh(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }

    _Gen* giac_atanh(_Gen* a, void *cp)
    {
        gen* r0;
        try{
            gen* a0 = reinterpret_cast<gen*>(a);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = atanh(*a0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>(r0);
    }


    
    _Gen* giac_eval(_Gen* g, int levels, void *cp)
    {
        gen* r0;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = g0->eval(levels, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_evalf(_Gen* g, int levels, void *cp)
    {
        gen* r0;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = g0->evalf(levels, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    
    _Gen* giac_simplify(_Gen* g, void *cp)
    {
        gen* r0;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = simplify(*g0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_expand(_Gen* g, void *cp)
    {
        gen* r0;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = expand(*g0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }

    _Gen* giac_factor1(_Gen* g, int with_sqrt, void *cp)
    {
        gen* r0;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            r0 = new gen();
            *r0 = factor(*g0, with_sqrt!=0, cp0);
        }   
        catch(runtime_error &e) {return _error(e.what());}
        return reinterpret_cast<_Gen*>( r0 );
    }


    char* giac_to_string(_Gen* g, void *cp)
    {
        char* cs;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            string s(g0->print(cp0)); 
            cs = (char*) malloc(sizeof(char)*(s.length()+1));
            strcpy(cs, s.c_str());
        }
        catch(runtime_error &e) { 
            cerr << e.what() << endl;
            return 0; 
        }
        return cs;
    }

    char* giac_to_latex(_Gen* g, void *cp)
    {
        char* cs;
        try{
            gen* g0 = reinterpret_cast<gen*>(g);
            context* cp0 = reinterpret_cast<context*>(cp);
            string s(gen2tex(*g0,cp0)); 
            cs = (char*) malloc(sizeof(char)*(s.length()+1));
            strcpy(cs, s.c_str());
        }
        catch(runtime_error &e) { 
            cerr << e.what() << endl;
            return 0; 
        }
        return cs;
    }


}
