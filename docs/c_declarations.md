Uninitialized, named declarator of simple type:

    int x;
    float y;
    char z;

Uninitialized, named declarator of derived type:
    
    uint8_t buffer[2048][16];
    fpos_t	(*_seek) (void *, fpos_t, int);

Function pointer declaration with function pointer argument:

    void (*signal(int sig, void (*func)(int)))(int);
    
Named declarator of derived type with compound initializer:
    
    int a[2][2] = {{1, 2}, {3, 4}};
    
Declaration of named composite type, without typedef:
    
    struct __sbuf {
    	unsigned char	*_base;
    	int _size;
    };
    
Declaration of variable of anonymous enum type with trailing commas:

    enum { red, blue, green, } color;
    

Forward declaration of variadic function with anonymous parameters:
    
    int	snprintf(char * __restrict, size_t, const char * __restrict, ...)
    
Forward declaration of function with function-pointer parameters with unnamed parameters:

    FILE	*funopen(const void *,
                     int (*)(void *, char *, int),
                     int (*)(void *, const char *, int),
                     fpos_t (*)(void *, fpos_t, int),
                     int (*)(void *));

Forward declaration of composite type:
    
    struct __sFILEX;

Simultaneous typedef and forward declaration of composite type:

    typedef struct cache_s cache_t;
    
Typedef of function pointer:
    
    typedef uintptr_t (*cache_key_hash_cb_t)(void *key, void *user_data);

Struct with flexible array member as its last field:

    struct s { int n; double d[]; };
    
Evil structure that uses one compound declaration with sized, anonymous, untyped members for padding bits:

    struct __darwin_fp_control
    {
        unsigned short __invalid :1,
          __denorm :1,
          __zdiv :1,
          __ovrfl :1,
          __undfl :1,
          __precis :1,
            :2,
          __pc :2,
          __rc :2,
            :1,
            :3;
    };
    
Function with variably-sized argument:

    void doStuff(size_t n, char foo[n]) {}
    
Scoped typedefs:

    char foo() {
        typedef char MyType;
        MyType c = 'a';
        returhn c;
    }