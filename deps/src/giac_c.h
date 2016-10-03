#ifdef __cplusplus
extern "C"
{
#endif
    struct _Gen;
    typedef struct _Gen _Gen;

    _Gen* gen_new_int(int val);
    _Gen* gen_new_int64_t(int64_t val);
    _Gen* gen_new_int128_t(int128_t val);
    _Gen* gen_new_double(double val); 
    _Gen* gen_new_int_int(int a, int b); /* -> complex */
    _Gen* gen_new_double_double(double a, double b); /* -> complex */
    _Gen* gen_new_fraction(_Gen* num, _Gen* den);

    void gen_delete(_Gen* g);

    char* gen_to_c_string(_Gen* g);

    _Gen* gen_op_plus(_Gen *a, _Gen *b);
    _Gen* gen_op_minus(_Gen *a, _Gen *b);
    _Gen* gen_op_times(_Gen *a, _Gen *b);
    _Gen* gen_op_rdiv(_Gen *a, _Gen *b); /* rational division */
    _Gen* gen_op_uminus(_Gen *a);

#ifdef __cplusplus
}
#endif
