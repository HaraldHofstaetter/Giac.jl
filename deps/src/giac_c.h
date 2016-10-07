#ifdef __cplusplus
extern "C"
{
#endif
    struct _Gen;
    typedef struct _Gen _Gen;

    void* giac_context_ptr();

    _Gen* giac_new_int(int val);
    _Gen* giac_new_int64_t(int64_t val);
    _Gen* giac_new_int128_t(int128_t val);
    _Gen* giac_new_bigint(void* m);
    _Gen* giac_new_double(double val); 
    _Gen* giac_new_bigint(void* m);
    _Gen* giac_new_complex_int(int re, int im); 
    _Gen* giac_new_complex_double(double re, double im); 
    _Gen* giac_new_complex(_Gen* re, _Gen* im); 
    _Gen* giac_new_rational(_Gen* num, _Gen* den);
    _Gen* giac_new_symbolic(char *cs, void *context_ptr);
    _Gen* giac_new_ident(char *cs);

    _Gen* giac_new_vector(_Gen* v[], int len, short int subtype);

    void giac_change_subtype(_Gen* g, int subtype);

    void giac_delete(_Gen* g);

    char* giac_to_string(_Gen* g, void *context_ptr);
    char* giac_to_latex(_Gen* g, void *context_ptr);

    _Gen* giac_undef(void);

    _Gen* giac_plus(_Gen *a, _Gen *b);
    _Gen* giac_minus(_Gen *a, _Gen *b);
    _Gen* giac_times(_Gen *a, _Gen *b);
    _Gen* giac_rdiv(_Gen *a, _Gen *b); /* rational division */
    _Gen* giac_uminus(_Gen *a);
    _Gen* giac_pow(_Gen *a, _Gen *b, void *context_ptr);

    int giac_equal_bool(_Gen *a, _Gen *b);
    int giac_greater_than(_Gen *a, _Gen *b);

    _Gen* giac_real(_Gen* a, void *context_ptr);
    _Gen* giac_imag(_Gen* a, void *context_ptr);
    _Gen* giac_conj(_Gen* a, void *context_ptr);
    _Gen* giac_abs(_Gen* a, void *context_ptr);

    _Gen* giac_sqrt(_Gen* a, void *context_ptr);
    _Gen* giac_exp(_Gen* a, void *context_ptr);
    _Gen* giac_log(_Gen* a, void *context_ptr);
    _Gen* giac_sin(_Gen* a, void *context_ptr);
    _Gen* giac_cos(_Gen* a, void *context_ptr);
    _Gen* giac_tan(_Gen* a, void *context_ptr);
    _Gen* giac_sinh(_Gen* a, void *context_ptr);
    _Gen* giac_cosh(_Gen* a, void *context_ptr);
    _Gen* giac_tanh(_Gen* a, void *context_ptr);
    _Gen* giac_asin(_Gen* a, void *context_ptr);
    _Gen* giac_acos(_Gen* a, void *context_ptr);
    _Gen* giac_atan(_Gen* a, void *context_ptr);
    _Gen* giac_asinh(_Gen* a, void *context_ptr);
    _Gen* giac_acosh(_Gen* a, void *context_ptr);
    _Gen* giac_atanh(_Gen* a, void *context_ptr);
    

    _Gen* giac_eval(_Gen* g, int levels, void *context_ptr);
    _Gen* giac_evalf(_Gen* g, int levels, void *context_ptr);
    _Gen* giac_simplify(_Gen* g, void *context_ptr);
    _Gen* giac_expand(_Gen* g, void *context_ptr);
    _Gen* giac_factor(_Gen* g, int with_sqrt, void *context_ptr);

    _Gen* giac_integrate(_Gen* a, void *context_ptr);
    _Gen* giac_sum1(_Gen* ex, _Gen* x, _Gen* a, _Gen* b, void *context_ptr);

#ifdef __cplusplus
}
#endif
