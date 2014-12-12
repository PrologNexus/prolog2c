//// pc.h - prolog runtime system


#ifdef PC_H
# error "pc.h included multiple times"
#endif


// as if superficial warnings would make C any safer...
#if defined(_FORTIFY_SOURCE)
# undef _FORTIFY_SOURCE
#endif


/// header files

#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <unistd.h>
#include <time.h>
#include <sys/stat.h>
#include <errno.h>
#include <strings.h>
#include <limits.h>
#include <sys/mman.h>
#include <fcntl.h>
#include <sys/types.h>


/// limits

#ifndef HEAP_SIZE
# define HEAP_SIZE  100000000
#endif

#ifndef HEAP_RESERVE
# define HEAP_RESERVE 20
#endif

#ifndef CONTROL_STACK_SIZE
# define CONTROL_STACK_SIZE 10000
#endif

#ifndef TRAIL_STACK_SIZE
# define TRAIL_STACK_SIZE 1000
#endif

#ifndef CHOICE_POINT_STACK_SIZE
# define CHOICE_POINT_STACK_SIZE 1000
#endif

#ifndef ENVIRONMENT_STACK_SIZE 
# define ENVIRONMENT_STACK_SIZE 10000
#endif

#ifndef SYMBOL_TABLE_SIZE
# define SYMBOL_TABLE_SIZE 3001
#endif


#define DEREF_STACK_SIZE  100
#define MAXIMAL_NUMBER_OF_ARGUMENTS 100
#define DEBUG_WRITE_TERM_LIST_LENGTH_LIMIT 10
#define TRACE_DEBUG_WRITE_LIMIT 5


/// miscellanous

#ifdef __LLP64__
# define WORD_OUTPUT_FORMAT_LENGTH  "ll"
#else
# define WORD_OUTPUT_FORMAT_LENGTH  "l"
#endif

#define WORD_OUTPUT_FORMAT   "%" WORD_OUTPUT_FORMAT_LENGTH "d"
#define FLOAT_OUTPUT_FORMAT  "%.15g"


/// types

typedef void *X;

#ifdef __LLP64__
typedef long long WORD;
typedef unsigned long long UWORD;
#else
typedef long WORD;
typedef unsigned long UWORD;
#endif

typedef double FLOAT;
typedef char CHAR;

typedef struct BLOCK { 
  WORD h;
  X d[];
} BLOCK;

typedef struct SPECIALBLOCK { 
  WORD h;
  void *p;
  X d[];
} SPECIALBLOCK;

typedef struct BYTEBLOCK { 
  WORD h;
  char d[];
} BYTEBLOCK;

typedef struct STRING_BLOCK {
  WORD h;
  CHAR s[];
} STRING_BLOCK;

typedef struct SYMBOL_BLOCK {
  WORD h;
  X s;
  struct SYMBOL_STRUCT *next, *previous;
} SYMBOL_BLOCK;

typedef struct STRUCTURE_BLOCK {
  WORD h;
  X f;
  X a[];
} STRUCTURE_BLOCK;

typedef struct FLONUM_BLOCK {
  WORD h;
  double n;
} FLONUM_BLOCK;

typedef struct PORT_BLOCK {
  WORD h;
  FILE *fp;
  X dir;			/* 1 = input */
  X open;			/* 1 = open */
  X data;
} PORT_BLOCK;

#define PREVIOUS_SYMBOL  END_OF_LIST_VAL

typedef struct CHOICE_POINT {
  X *T, *R, *E;
  void **S;
  struct CHOICE_POINT *C0;
  void *P;
} CHOICE_POINT;


typedef struct FINALIZER
{
  X object;
  void (*finalizer)(X);
  struct FINALIZER *next;
} FINALIZER;


/// tags and type-codes

#define FIXNUM_MARK_BIT 1

#define ALIGNMENT_HOLE_MARKER  ((X)0xdeadbeefL)

//XXX add more here
#if defined(__x86_64__) || defined(__LLP64__) || defined(__LP64__)
# define SIXTYFOUR
#endif

#ifdef __LLP64__
# define WORD_SIGN_BIT   0x8000000000000000LL
# define WORD_TOP_BIT    0x4000000000000000LL
#elif defined(SIXTYFOUR)
# define WORD_SIGN_BIT   0x8000000000000000L
# define WORD_TOP_BIT    0x4000000000000000L
#else
# define WORD_SIGN_BIT   0x80000000L
# define WORD_TOP_BIT    0x40000000L
#endif


#ifdef SIXTYFOUR
# define TYPE_SHIFT 56
# define WORD_SIZE  8
#else
# define TYPE_SHIFT 24
# define WORD_SIZE  4
#endif

#define GC_MARK_BIT ((WORD)0x80 << TYPE_SHIFT)
#define BYTEBLOCK_MARK_BIT ((WORD)0x20 << TYPE_SHIFT)
#define SPECIALBLOCK_MARK_BIT ((WORD)0x40 << TYPE_SHIFT)

#define HEADER_BITS_MASK  ((WORD)0xff << TYPE_SHIFT)
#define HEADER_SIZE_MASK  (~HEADER_BITS_MASK)
#define HEADER_TYPE_MASK  ((WORD)0x3f << TYPE_SHIFT)

#define FIXNUM_TYPE  1
#define END_OF_LIST_TYPE  2
#define SYMBOL_TYPE 3
#define FLONUM_TYPE  0x24
#define PORT_TYPE  0x45
#define VAR_TYPE 6
#define STRING_TYPE  0x27
#define STRUCTURE_TYPE  8
#define PAIR_TYPE  9

#define is_atomic_type(t)   ((t) < VAR_TYPE)
#define is_compound_type(t)   ((t) >= STRUCTURE_TYPE)

#define TYPE_TO_TAG(t)  ((WORD)(t) << TYPE_SHIFT)
#define TAG_TO_TYPE(t)  ((WORD)(t) >> TYPE_SHIFT)

#define PAIR_TAG  TYPE_TO_TAG(PAIR_TYPE)
#define VAR_TAG  TYPE_TO_TAG(VAR_TYPE)
#define STRING_TAG  TYPE_TO_TAG(STRING_TYPE)
#define STRUCTURE_TAG  TYPE_TO_TAG(STRUCTURE_TYPE)
#define FLONUM_TAG  TYPE_TO_TAG(FLONUM_TYPE)
#define END_OF_LIST_TAG  TYPE_TO_TAG(END_OF_LIST_TYPE)
#define SYMBOL_TAG  TYPE_TO_TAG(SYMBOL_TYPE)
#define PORT_TAG  TYPE_TO_TAG(PORT_TYPE)

#define fixnum_to_word(n)  ((WORD)(n) >> 1)
#define word_to_fixnum(n)  ((X)((WORD)(n) << 1 | FIXNUM_MARK_BIT))
#define word_to_float(n)   ((FLOAT)(n))
#define float_to_word(n)   ((WORD)trunc(n))
#define flonum_to_float(x)  (((FLONUM_BLOCK *)(x))->n)
#define flonum_to_word(x)  ((WORD)((FLONUM_BLOCK *)(x))->n)
#define fixnum_to_float(x)  ((FLOAT)fixnum_to_word(x))

#define is_forwarded(x)    ((objbits(x) & GC_MARK_BIT) != 0)

#define ptr_to_fptr(ptr)        (((WORD)(ptr) >> 1) | GC_MARK_BIT)
#define fptr_to_ptr(fptr)       ((X)((fptr) << 1))

#define ZERO     word_to_fixnum(0)
#define ONE     word_to_fixnum(0)


/// predefined literals and global variables

static BLOCK END_OF_LIST_VAL_BLOCK = { END_OF_LIST_TAG, {}};

#define END_OF_LIST_VAL  ((X)(&END_OF_LIST_VAL_BLOCK))


static PORT_BLOCK default_input_port = { PORT_TAG|4, NULL, ONE, ZERO, ZERO };
static PORT_BLOCK default_output_port = { PORT_TAG|4, NULL, ZERO, ZERO, ZERO };
static PORT_BLOCK default_error_port = { PORT_TAG|4, NULL, ZERO, ZERO, ZERO };

static X standard_input_port = (X)(&default_input_port);
static X standard_output_port = (X)(&default_output_port);
static X standard_error_port = (X)(&default_error_port);

static X *fromspace, *fromspace_end, *fromspace_limit, *tospace, *tospace_end, *tospace_top, *scan_ptr;
static X *alloc_top;
static X symbol_table[ SYMBOL_TABLE_SIZE ];
static WORD heap_reserve;
static int verbose = 0;
static int variable_counter = 0;
static X environment_stack[ ENVIRONMENT_STACK_SIZE ];
static X trail_stack[ TRAIL_STACK_SIZE ];
static void *control_stack[ CONTROL_STACK_SIZE ];
static X *trail_top;
static CHOICE_POINT choice_point_stack[ CHOICE_POINT_STACK_SIZE ];
static FINALIZER *active_finalizers = NULL, *free_finalizers = NULL;
static WORD gc_count = 0;
static char *mmapped_heap = NULL;
static char **global_argv;
static int global_argc;

static CHAR *type_names[] = { 
  "invalid", "fixnum", "null", "symbol", "flonum", "stream", "variable", "string", "structure", "pair"
};


#define type_name(t)         (type_names[ (WORD)(t) & 0x1f ])
#define tag_to_type_name(t)  type_name(objtype(t))


/// debugging and termination

#define OUTPUT(...)  { fflush(stdout); fprintf(stderr, __VA_ARGS__); } 
#define CRASH(...)   { OUTPUT(__VA_ARGS__); crash_hook(); exit(1); }

#ifdef UNSAFE
# define NDEBUG
#endif

#ifdef NDEBUG
# define ASSERT(x, ...)   ;
# define DBG(...)         ;
# define DRIBBLE          ;
#else
# define ASSERT(x, ...)   { if(!(x)) CRASH(__VA_ARGS__) }
# define DBG              OUTPUT
# define DRIBBLE(...)     { if(verbose) OUTPUT(__VA_ARGS__); }
#endif


static void crash_hook()
{
  return;
}


/// accessors and type-testing

#ifdef SIXTYFOUR
# define FPALIGNED(x)  (1)
# define FPALIGN
# define ALIGN(n)  (((n) + 7) & ~7L)
#else
# define FPALIGNED(x)  (((WORD)objdata(x) & 0x7) == 0)
# define FPALIGN  alloc_top = (X)((((WORD)alloc_top + 7) & ~7L) + 4)
# define ALIGN(n)  (((n) + 3) & ~3L)
#endif

#define bytes_to_words(n)  (ALIGN(n) / WORD_SIZE)
#define words_to_bytes(n)  ((n) * WORD_SIZE)
#define floats_to_bytes(n)  ((n) * sizeof(FLOAT))

#define objbits(x)  (((BLOCK *)(x))->h)
#define objtype(x)  (objbits(x) >> TYPE_SHIFT)
#define objdata(x)  (((BLOCK *)(x))->d)
#define objsize(x)  (((BLOCK *)(x))->h & HEADER_SIZE_MASK)

#define is_byteblock(x)  ((objbits(x) & BYTEBLOCK_MARK_BIT) != 0)
#define is_specialblock(x)  ((objbits(x) & SPECIALBLOCK_MARK_BIT) != 0)

#define is_FIXNUM(x)  (((WORD)(x) & FIXNUM_MARK_BIT) != 0)
#define is_END_OF_LIST(x)  ((x) == END_OF_LIST_VAL)
#define is(t, x)   ({ X x_ = (x); !is_FIXNUM(x_) && objtype(x_) == (t); })
#define is_PAIR(x)  is(PAIR_TYPE, (x))
#define is_VAR(x)  is(VAR_TYPE, (x))
#define is_STRING(x)  is(STRING_TYPE, (x))
#define is_SYMBOL(x)  is(SYMBOL_TYPE, (x))
#define is_STRUCTURE(x)  is(STRUCTURE_TYPE, (x))
#define is_PORT(x)  is(PORT_TYPE, (x))
#define is_FLONUM(x)  is(FLONUM_TYPE, (x))


static inline int is_number(X x)
{
  return is_FIXNUM(x) || is_FLONUM(x);
}


static inline int is_atomic(X x)
{
  return is_FIXNUM(x) || is_atomic_type(objtype(x));
}


static inline int is_compound(X x)
{
  return !is_FIXNUM(x) || is_compound_type(objtype(x));
}


static inline int is_in_fixnum_range(WORD n) {
  return (n & WORD_SIGN_BIT) == (((n) & WORD_TOP_BIT) << 1);
}


#define string_length(x)  ((objsize(x) / sizeof(CHAR)) - 1)

#define IS_IN_HEAP(ptr)  ((X*)(ptr) >= fromspace && (X*)(ptr) < fromspace_end)

#define slot_ref(x, i)          (objdata(x)[ i ])
#define atomic_slot_set(x, i, y)  (objdata(x)[ i ] = (y))
#define string_ref(x, i)        code_char(((CHAR *)objdata(x))[ i ])
#define string_set(x, i, y)     ({ X c_ = (y); (((CHAR *)objdata(x))[ i ]) = char_code(c_); c_; })

#define ASSIGN(dest, x)  dest = (x)

#define SLOT_SET(x, i, y)       ASSIGN(objdata(x)[ i ], (y))
#define SLOT_INIT(x, i, y)      atomic_slot_set(x, i, y)

#define port_file(p)  (((PORT_BLOCK *)(p))->fp)


static void write_barrier_error() { CRASH("assignment to non-mutable data detected\n"); }


static inline X fill_block(X x, X y, WORD from, WORD to) 
{
  if(from == to) return x;

  for(X *p = objdata(x) + from; from < to; ++from)
    ASSIGN(*(p++), y);

  return x;
}


static inline X atomic_fill_block(X x, X y, WORD from, WORD to)
{
  if(from == to) return x;

  for(X *p = objdata(x) + from; from < to; ++from)
    *(p++) = y;

  return x;
}


// takes char-code as argument
static inline X fill_string(X x, WORD code, WORD from, WORD to)
{
  if(from == to) return x;

  for(CHAR *p = (CHAR *)objdata(x) + from; from < to; ++from)
    *(p++) = (CHAR)code;

  return x;
}


static inline X copy_bytes(X src, WORD srcp, X dest, const WORD destp, WORD len) 
{
  memcpy((char *)objdata(dest) + destp, (char *)objdata(src) + srcp, len);
  return dest;
}


static inline X copy_string(X src, WORD srcp, X dest, WORD destp, WORD len) 
{
  return copy_bytes(src, srcp * sizeof(CHAR), dest, destp * sizeof(CHAR), len * sizeof(CHAR));
}


/// type checking


static void check_type_failed(WORD t, X x)
{
  if(is_FIXNUM(x))
    CRASH("type check failed - expected type %s but got fixnum\n", type_name(t));
  
  if(objtype(x) != t)
    CRASH("type check failed - expected type %s but got type %s\n", type_name(t), type_name(objtype(x)));
}


static inline X check_type(WORD t, X x)
{
#ifndef UNSAFE
  if(is_FIXNUM(x) || objtype(x) != t)
    check_type_failed(t, x);
#endif

  return x;
}


static inline X check_fixnum(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x))
    check_type_failed(FIXNUM_TYPE, x);
#endif

  return x;
}


static void check_number_failed(X x)
{
  CRASH("type check failed - expected number, but got %s\n", type_name(objtype(x)));
}


static inline X check_number(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x) && objtype(x) != FLONUM_TYPE)
    check_number_failed(x);
#endif
  
  return x;
}


static void check_integer_failed(X x)
{
  // x is not a fixnum, so using objtype is safe
  CRASH("type check failed - expected integer, but got %s\n", type_name(objtype(x)));
}


static inline X check_integer(X x)
{
#ifdef UNSAFE
  return x;
#else

  if(is_FIXNUM(x))
    return x;

  if(objtype(x) == FLONUM_TYPE) {
    FLOAT n;

    if(modf(flonum_to_float(x), &n) == 0)
      return x;
  }

  check_integer_failed(x);
#endif
}


static void check_atomic_failed(X x)
{
  CRASH("type check failed - expected atomic, but got %s\n", type_name(objtype(x)));
}


static inline X check_atomic(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x) && !is_atomic_type(objtype(x)))
    check_atomic_failed(x);
#endif
  
  return x;
}


static inline WORD check_index(X x, WORD i)
{
#ifndef UNSAFE
  if(i < 0 || i >= objsize(x))
    CRASH("index out of range - index is " WORD_OUTPUT_FORMAT ", size is " WORD_OUTPUT_FORMAT "\n", i, objsize(x)); 
#endif

  return i;
}


static inline WORD check_index_STRING(X x, WORD i)
{
#ifndef UNSAFE
  WORD len = string_length(x);

  if(i < 0 || i >= len)
    CRASH("string index out of range - index is " WORD_OUTPUT_FORMAT ", size is " WORD_OUTPUT_FORMAT "\n",
	  i, len); 
#endif

  return i;
}

  
static inline X check_range(X x, WORD i, WORD j)
{
#ifndef UNSAFE
  WORD s = objsize(x);

  if(i < 0 || i > s || j < 0 || j > s)	      
    CRASH("index out of range - range is " WORD_OUTPUT_FORMAT "..." WORD_OUTPUT_FORMAT ", size is " 
	  WORD_OUTPUT_FORMAT "\n", i, j, s); 
#endif

  return x;
}


static inline X check_range_STRING(X x, WORD i, WORD j)				
{
#ifndef UNSAFE
  WORD s = string_length(x);

  if(i < 0 || i > s || j < 0 || j > s) 
    CRASH("string index out of range - range is " WORD_OUTPUT_FORMAT "..." WORD_OUTPUT_FORMAT ", size is " 
	  WORD_OUTPUT_FORMAT "\n", i, j, s); 
#endif

  return x;
}


#define check_type_PAIR(x)  check_type(PAIR_TYPE, (x))
#define check_type_STRING(x)  check_type(STRING_TYPE, (x))
#define check_type_SYMBOL(x)  check_type(SYMBOL_TYPE, (x))
#define check_type_FLONUM(x)  check_type(FLONUM_TYPE, (x))
#define check_type_SYMBOL(x)  check_type(SYMBOL_TYPE, (x))
#define check_type_PORT(x)  check_type(PORT_TYPE, (x))
#define check_type_STRUCTURE(x)  check_type(VECTOR_TYPE, (x))


static inline int is_port_and_direction(X x, X d)
{
  return is_PORT(x) && slot_ref(x, 1) == d;
}


static inline X check_input_port(X x)
{
#ifndef UNSAFE
  check_type_PORT(x);
  
  if(slot_ref(x, 1) == ZERO)
    CRASH("not an input-port\n");
#endif

  return x;
}


static inline X check_output_port(X x)
{
#ifndef UNSAFE
  check_type_PORT(x);

  if(slot_ref(x, 1) != ZERO)
    CRASH("not an output-port\n");
#endif

  return x;
}


/// allocators

#define ALLOCATE_BLOCK(dest, type, size)				\
  dest = (void *)alloc_top;						\
  ((BLOCK *)alloc_top)->h = ((WORD)(type) << TYPE_SHIFT) | (size);		\
  alloc_top += (size) + 1

#define ALLOCATE_BYTEBLOCK(dest, type, size)				\
  dest = (void *)alloc_top;							\
  ((BLOCK *)alloc_top)->h = ((WORD)(type) << TYPE_SHIFT) | (size);		\
  alloc_top = (X)ALIGN((WORD)alloc_top + (size) + sizeof(WORD))

#define FLONUM(m)  ({ FPALIGN; ALLOCATE_BYTEBLOCK(FLONUM_BLOCK *p_, FLONUM_TYPE, sizeof(FLOAT)); p_->n = (m); (X)p_; })
#define PAIR(x, y) ({ ALLOCATE_BLOCK(BLOCK *p_, PAIR_TYPE, 2); p_->d[ 0 ] = (x); p_->d[ 1 ] = (y); (X)p_; })

#define PORT(f, i, o, d) \
  ({ ALLOCATE_BLOCK(PORT_BLOCK *p_, PORT_TYPE, 4); \
     p_->fp = (f); \
     p_->dir = (i); \
     p_->open = (o); \
     p_->data = (d); \
     (X)p_; })

#define STRING(len)  \
  ({ WORD len2_ = (len);							\
    ALLOCATE_BYTEBLOCK(BLOCK *p_, STRING_TYPE, len2_ + 1);	\
    ((CHAR *)(p_->d))[ len2_ ] = '\0';					\
    (X)p_; })
  
#define CSTRING(str) \
  ({ char *str_ = str; \
    WORD len3_ = strlen(str);						\
    X s_ = (X)STRING(len3_);						\
    memcpy(objdata(s_), str_, len3_);					\
    s_; })
  
#define SYMBOL(name, next, prev)					\
  ({ ALLOCATE_BLOCK(BLOCK *p_, SYMBOL_TYPE, 3);			\
    SLOT_INIT((X)p_, 0, (name));					\
    SLOT_INIT((X)p_, 1, (next));					\
    SLOT_INIT((X)p_, 2, (prev));					\
    (X)p_; })


/// garbage collection

static void mark(X *addr) 
{
  WORD h = ((BLOCK *)(*addr))->h;

  if((h & GC_MARK_BIT) != 0) {
    *addr = fptr_to_ptr(h);
    return;
  }

  WORD size = h & HEADER_SIZE_MASK;

  if((h & BYTEBLOCK_MARK_BIT) == 0) 
    size *= sizeof(WORD);
  else
    size = ALIGN(size);

#ifndef SIXTYFOUR
  WORD t = TAG_TO_TYPE(h);

  if((t == FLONUM_TYPE || t == PACKEDFLOATVECTOR_TYPE) && !FPALIGNED(tospace_top))
    *(tospace_top++) = ALIGNMENT_HOLE_MARKER;
#endif

  X nptr = tospace_top;
  WORD fptr = ptr_to_fptr(tospace_top);
  memcpy(tospace_top, *addr, size + sizeof(WORD));
  tospace_top = (X)((WORD)tospace_top + size + sizeof(WORD));
  ((BLOCK *)(*addr))->h = fptr;
  *addr = nptr;
}


static inline void mark1(X *addr)  
{ 
  if(((WORD)(*addr) & FIXNUM_MARK_BIT) == 0 && IS_IN_HEAP(*addr))
    mark(addr); 
}


static void collect_garbage(X *E, X *A, int args)
{
  va_list va;
  DRIBBLE("[GC ... ");							
  tospace_top = tospace; 
  scan_ptr = tospace_top;				

  // mark argument variables
  for(X *p = A; args > 0; --args) 
    mark1(p);

  // mark local environments
  for(X *p = environment_stack; p < E; ++p)
    mark1(p);

  //XXX preliminary - later don't mark and remove unforwarded items
  //    (adjusting T pointers in CP-stack accordingly)
  for(X *p = trail_stack; p < trail_top; ++p)
    mark1(p);

  // mark standard ports
  mark1(&standard_input_port);
  mark1(&standard_output_port);
  mark1(&standard_error_port);

  while(scan_ptr < tospace_top) {						
#ifndef SIXTYFOUR
    if(((WORD)scan_ptr & 7) == 0 && *scan_ptr == ALIGNMENT_HOLE_MARKER) 
      ++scan_ptr; 
#endif

    WORD h = (WORD)(*scan_ptr);						
    WORD size = h & HEADER_SIZE_MASK;					

    // DRIBBLE("H: %08lx\n", h);

    if((h & BYTEBLOCK_MARK_BIT) != 0) 
      scan_ptr += bytes_to_words(size) + 1;	
    else {
      ++scan_ptr;		/* skip header */

      if((h & SPECIALBLOCK_MARK_BIT) != 0) {
	++scan_ptr;		/* skip ptr */
	--size;
      }

      while(size-- > 0)
	mark1(scan_ptr++);

      ASSERT(scan_ptr < tospace_end, "scan_ptr exceeded fromspace\n");
    }
  }

  // remove unforwarded symbols from symbol-table
  int gcdsyms = 0;

  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i) {
    X prevsym = END_OF_LIST_VAL;

    for(X sym = symbol_table[ i ]; sym != END_OF_LIST_VAL; sym = slot_ref(sym, 1)) {
      // all further symbols in this chain will be static
      if(!IS_IN_HEAP(sym)) {
	if(prevsym != END_OF_LIST_VAL)
	  SLOT_SET(prevsym, 1, sym);

	break;
      }

      if(is_forwarded(sym)) {
	sym = fptr_to_ptr(objbits(sym));
	SLOT_SET(sym, 2, prevsym);
	SLOT_SET(sym, 1, END_OF_LIST_VAL);

	if(prevsym != END_OF_LIST_VAL) 
	  SLOT_SET(prevsym, 1, sym);

	prevsym = sym;
      }
      else ++gcdsyms;
    }
  }

  if(gcdsyms > 0)
    DRIBBLE("%d symbols collected ", gcdsyms);

  void *tmp = fromspace; 
  fromspace = tospace; 
  tospace = tmp;		
  tmp = fromspace_end; 
  fromspace_end = tospace_end;
  tospace_end = tmp;	
  fromspace_limit = (X)((char *)fromspace_end - heap_reserve);	
  alloc_top = fromspace;
  DRIBBLE("finished (" WORD_OUTPUT_FORMAT " bytes in use)]\n", ((long)((char *)tospace_top - (char *)fromspace))); 
  ++gc_count;
  
  if(tospace_top >= fromspace_limit) 
    CRASH("heap exhausted\n");				

  // check finalizers
  FINALIZER *prev = NULL;
  FINALIZER *fp = active_finalizers; 

  while(fp != NULL) {
    WORD h = objbits(fp->object);

    if((h & GC_MARK_BIT) != 0) {
      fp->object = fptr_to_ptr(h);
      prev = fp;
      fp = fp->next;
    }
    else {
      DRIBBLE("[running finalizer on %s %p]\n", type_name(TAG_TO_TYPE(h)), fp->object);

      if(prev != NULL)
	prev->next = fp->next;
      else
	active_finalizers = fp->next;

      FINALIZER *fp2 = fp->next;
      fp->next = free_finalizers;
      free_finalizers = fp;
      fp->finalizer(fp->object);
      fp = fp2;
    }
  }
}


static void set_finalizer(X x, void (*fn)(X))
{
  FINALIZER *fp = malloc(sizeof(FINALIZER));
  fp->next = active_finalizers;
  fp->finalizer = fn;
  fp->object = x;
  active_finalizers = fp;
}


/// symbol-table management

static WORD hash_name(CHAR *name, int len)
{
  unsigned long key = 0;

  while(len--)
    key ^= (key << 6) + (key >> 2) + *(name++);

  return (WORD)key;
}


static X intern(X name)
{
  WORD len = string_length(name);
  WORD key = hash_name((CHAR *)objdata(name), len) % SYMBOL_TABLE_SIZE;

  for(X sym = symbol_table[ key ]; sym != END_OF_LIST_VAL; sym = slot_ref(sym, 1)) {
    X name2 = slot_ref(sym, 0);

    if(string_length(name2) == len &&
       !strncmp((CHAR *)objdata(name), (CHAR *)objdata(name2), len)) //UUU
      return sym;
  }

  X oldsym = symbol_table[ key ];
  X sym = SYMBOL(name, oldsym, END_OF_LIST_VAL);

  if(oldsym != END_OF_LIST_VAL)
    SLOT_SET(oldsym, 2, sym);

  symbol_table[ key ] = sym;
  return sym;
}


static void intern_static_symbols(X sym1)
{
  while(sym1 != END_OF_LIST_VAL) {
    X name = slot_ref(sym1, 0);
    WORD len = string_length(name);
    WORD key = hash_name((CHAR *)objdata(name), len) % SYMBOL_TABLE_SIZE;
    X sym = symbol_table[ key ];
    X nextsym = slot_ref(sym1, 1);
    SLOT_SET(sym1, 1, sym);

    if(sym != END_OF_LIST_VAL) 
      SLOT_SET(sym, 2, sym1);

    symbol_table[ key ] = sym;
    sym1 = nextsym;
  }
}


/// runtime initialization and abnormal exit


static WORD numeric_arg(char *arg)
{
  int len = strlen(arg);
  WORD m = 1L;

  switch(toupper(arg[ len - 1 ])) {
  case 'K': m = 1024L; break;
  case 'M': m = 1024L * 1024L; break;
  case 'G': m = 1024L * 1024L * 1024L; break;
#ifdef SIXTYFOUR
  case 'T': m = 1024L * 1024L * 1024L * 1024L; break;
#endif
  default: return atol(arg);
  }

  arg[ len - 1 ] = '\0';
  return m * atol(arg);
}


static void initialize(int argc, char *argv[])
{
  WORD heapsize = HEAP_SIZE;

  // scan argv for runtime-parameters
  for(int i = argc - 1; i > 0; --i) {
    char *arg = argv[ i ];
    int len = strlen(arg);

    if(arg[ 1 ] == ':') {
      switch(arg[ 2 ]) {
      case 'h':
	heapsize = numeric_arg(arg + 3);
	break;

      case 'v':
	verbose = 1;
	break;

      case 'm':
	mmapped_heap = arg + 3;
	break;

      default:
	OUTPUT("WARNING: invalid runtime option \"%s\" (ignored)\n", arg);
      }
    }
  }

  DRIBBLE("[allocating heap of size 2 x " WORD_OUTPUT_FORMAT "]\n", heapsize / 2);
  heap_reserve = heapsize / HEAP_RESERVE;

  if(mmapped_heap) {
    int fd = open(mmapped_heap, O_RDWR | O_CREAT, S_IRUSR | S_IWUSR);

    if(fd == -1) 
      CRASH("unable to open heap file: %s\n", strerror(errno));

    if(ftruncate(fd, heapsize) == -1)
      CRASH("unable to create heap file: %s\n", strerror(errno));

    fromspace = mmap(NULL, heapsize, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

    if(fromspace == MAP_FAILED) 
      CRASH("unable to map heap file into memory: %s\n", strerror(errno));

    tospace = (X *)((WORD)fromspace + heapsize / 2);
  }
  else {
    fromspace = malloc(heapsize / 2);
    tospace = malloc(heapsize / 2);
    ASSERT(fromspace && tospace, "can not allocate heap");
  }

  fromspace_end = (X *)((WORD)fromspace + heapsize / 2);
  tospace_end = (X *)((WORD)tospace + heapsize / 2);
  fromspace_limit = (X *)((WORD)fromspace_end - heap_reserve);
  alloc_top = fromspace;
  default_input_port.fp = stdin;
  default_output_port.fp = stdout;
  default_error_port.fp = stderr;
  trail_top = trail_stack;

  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i)
    symbol_table[ i ] = END_OF_LIST_VAL;
}


static X command_line_arguments()
{
  X lst = END_OF_LIST_VAL;

  // build argument-list for "command-line-arguments"
  for(int i = global_argc - 1; i > 0; --i) {
    int len = strlen(global_argv[ i ]);
    
    // ignore runtime options
    if(global_argv[ i ][ 1 ] != ':') {
      X str = CSTRING(global_argv[ i ]);
      X sym = intern(str);
      X pr = PAIR(sym, lst);
      lst = pr;
    }
  }

  return lst;
}


/// variable handling

static inline X make_var()
{
  ALLOCATE_BLOCK(BLOCK *v, VAR_TYPE, 2);
  v->d[ 0 ] = (X)v;
  v->d[ 1 ] = word_to_fixnum(variable_counter++);
  return v;
}


#define deref1(var)  (((BLOCK *)(var))->d[ 0 ])


static X deref(X val)
{
  static X stack[ DEREF_STACK_SIZE ];
  X *sp = stack;

  for(;;) {
    if(is_FIXNUM(val) || objtype(val) != VAR_TYPE)
      return val;

    for(X *p = sp - 1; p >= stack; --p) {
      if(*p == val) 
	return val;
    }

    *(sp++) = val;
    val = deref1(val);

#ifndef UNSAFE
    if(sp >= stack + DEREF_STACK_SIZE)
      CRASH("deref-stack overflow\n");
#endif
  }
}


static void unwind_trail(X *tp)
{
  while(trail_top != tp) {
    BLOCK *var = (BLOCK *)(*(--trail_top));
    SLOT_SET(var, 0, var);
  }
}


static inline void push_trail(X var)
{
#ifndef UNSAFE
  if(trail_top >= trail_stack + TRAIL_STACK_SIZE)
    CRASH("trail-stack overflow.\n");
#endif

  *(trail_top++) = var;
}


/// unification

#define unify(x, y)   ({ X _x = (x), _y = (y); _x == _y ? 1 : unify1(_x, _y); })


static int unify1(X x, X y)
{
  x = deref(x);
  y = deref(y);

  WORD xt = is_FIXNUM(x) ? FIXNUM_TYPE : objtype(x);
  WORD yt = is_FIXNUM(y) ? FIXNUM_TYPE : objtype(y);

  if(xt == VAR_TYPE) {
    SLOT_SET(x, 0, y);
    push_trail(x);
    return 1;
  }

  if(yt == VAR_TYPE) {
    SLOT_SET(y, 0, x);
    push_trail(y);
    return 1;
  }

  if(xt != yt) 
    return 0;
  
  if(xt == FIXNUM_TYPE)
    return 1;

  WORD s = objsize(x);

  if(s != objsize(y)) 
    return 0;

  if(is_byteblock(x))
    return !memcmp(objdata(x), objdata(y), s);

  int i = 0;

  if(is_specialblock(x)) 
    ++i;

  while(i < s) {
    if(!unify(slot_ref(x, i), slot_ref(y, i)))
      return 0;

    ++i;
  }

  return 1;
}


/// term-construction

static X make_term(int arity, ...)
{
  va_list va;
  va_start(va, arity);
  X f = va_arg(va, X);
  check_type(SYMBOL_TYPE, f);

  if(arity == 2 && !strcmp(".", (CHAR *)objdata(slot_ref(f, 0)))) {
    X car = va_arg(va, X);
    return PAIR(car, va_arg(va, X));
  }

  ALLOCATE_BLOCK(BLOCK *s, STRUCTURE_TYPE, arity + 1);

  for(int i = 0; i <= arity; ++i)
    s->d[ i ] = va_arg(va, X);

  return (X)s;
}


#define make_pair    PAIR


/// comparisons

static inline int is_identical(X x, X y)
{
  if(x == y) return 1;
  
  return deref(x) == deref(y);
}


/// numerical operations

#define NUMERIC_BINARY_CMP(name, op)		\
  static inline int name(X x, X y)		\
  { x = deref(x); \
    y = deref(y); \
    if(is_FIXNUM(x)) {					\
      if(is_FIXNUM(y))					\
	return x op y;					\
      if(is_FLONUM(y))					\
	return fixnum_to_float(x) op flonum_to_float(y);	\
      check_number_failed(y); }					\
    if(is_FLONUM(x)) {						\
      if(is_FIXNUM(y))						\
	return flonum_to_float(x) op fixnum_to_float(y);	\
      if(is_FLONUM(y))						\
	return flonum_to_float(x) op flonum_to_float(y);	\
      check_number_failed(y); }					\
    check_number_failed(x); }

NUMERIC_BINARY_CMP(is_num_eq, ==)
NUMERIC_BINARY_CMP(is_num_gt, >)
NUMERIC_BINARY_CMP(is_num_lt, <)

#define NUMERIC_BINARY_OP(name, op)		\
  static inline X name(X x, X y)		\
  { x = deref(x); \
    y = deref(y); \
    if(is_FIXNUM(x)) {					\
      if(is_FIXNUM(y))					\
	return word_to_fixnum(fixnum_to_word(x) op fixnum_to_word(y));	\
      if(is_FLONUM(y))					\
	return FLONUM(fixnum_to_float(x) op flonum_to_float(y));	\
      check_number_failed(y); }					\
    if(is_FLONUM(x)) {						\
      if(is_FIXNUM(y))						\
	return FLONUM(flonum_to_float(x) op fixnum_to_float(y));	\
      if(is_FLONUM(y))						\
	return FLONUM(flonum_to_float(x) op flonum_to_float(y));	\
      check_number_failed(y); }						\
    check_number_failed(x); }

NUMERIC_BINARY_OP(num_add, +)
NUMERIC_BINARY_OP(num_sub, -)
NUMERIC_BINARY_OP(num_mul, *)

static inline X num_div(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y))
      return FLONUM(fixnum_to_float(x) / fixnum_to_float(y));	

    if(is_FLONUM(y))						
      return FLONUM(fixnum_to_float(x) / flonum_to_float(y));	
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y))						
      return FLONUM(flonum_to_float(x) / fixnum_to_float(y));  

    if(is_FLONUM(y))							
      return FLONUM(flonum_to_float(x) / flonum_to_float(y));		

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
}


static inline X num_quo(X x, X y) 
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y))
      return word_to_fixnum(fixnum_to_word(x) / fixnum_to_word(y));	

    if(is_FLONUM(y))						
      return word_to_fixnum(fixnum_to_word(x) / flonum_to_word(y));	
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y))						
      return word_to_fixnum(flonum_to_word(x) / fixnum_to_word(y));  

    if(is_FLONUM(y))							
      return word_to_fixnum(flonum_to_word(x) / flonum_to_word(y));		

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
}


/// VM operations

#define CURRENT_NAME
#define CURRENT_ARITY

#define ENTER  \
  { TRACE_ENTER(CURRENT_NAME, CURRENT_ARITY); \
    CHECK_LIMIT; \
    PUSH(C0); C0 = C; PUSH(R); PUSH(E); \
    R = R0; }

#define FAIL     { TRACE_FAIL(CURRENT_NAME, CURRENT_ARITY); goto fail; }
#define REDO     TRACE_REDO(CURRENT_NAME, CURRENT_ARITY)

#define EXIT \
  { TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    R0 = R; POP(E); POP(R); POP(C0); \
    goto *R0; }

#define CHECK_LIMIT  \
  if(alloc_top > fromspace_limit) { \
    collect_garbage(E, A, CURRENT_ARITY);	\
  }

#define ENVIRONMENT(len)  E = E + (len); 

#define PUSH(x)  *(S++) = (x)
#define POP(x)   (x) = *(--S)
#define RESERVE(n)   for(int _i = (n); _i--;) PUSH(ZERO)

#define PUSHCP(lbl) \
  { C->T = trail_top; C->R = R; C->S = S; C->E = E;	\
    C->C0 = C0; C->P = (lbl); ++C; }

#define POPCP     ({ CHOICE_POINT *_cp; POP(_cp); if(C != C0) C = _cp; })
#define CLEARCP   C = C0

#define INVOKE_CHOICE_POINT  \
  { CHOICE_POINT *_cp = --C; \
    unwind_trail(_cp->T);	      \
  R = _cp->R; S = _cp->S; E = _cp->E; \
  goto *(_cp->P); }


/// Boilerplate code

#define BOILERPLATE				\
  X A[ MAXIMAL_NUMBER_OF_ARGUMENTS ];		\
  CHOICE_POINT *C, *C0;				\
  void *R0 = &&success_exit;			\
  X *E = environment_stack;			\
  initialize(argc, argv);			\
  intern_static_symbols(PREVIOUS_SYMBOL);	\
  C0 = C = choice_point_stack;			\
  void **S = control_stack;				\
  void *R = NULL;				\
  PUSHCP(&&fail_exit);				\
  goto INIT_GOAL;					\
fail: INVOKE_CHOICE_POINT;				\
fail_exit: fprintf(stderr, "false.\n"); exit(1);	\
success_exit: DRIBBLE("true.\n"); exit(0);


/// primitives (all expect their argument to be deref'd)


static int debug_hook(X x) { return 1; }
static inline int write_char(X c) { check_fixnum(c); fputc(fixnum_to_word(c), port_file(standard_output_port)); return 1; }


// doesn't quote atoms or respects operators
static void basic_write_term(FILE *fp, int limit, int quote, X x) { 
  if(limit == 0) fputs("...", fp);
  else if(is_FIXNUM(x)) 
    fprintf(fp, WORD_OUTPUT_FORMAT, fixnum_to_word(x));
  else {
    switch(objtype(x)) {
    case FLONUM_TYPE:
      fprintf(fp, FLOAT_OUTPUT_FORMAT, flonum_to_float(x));
      break;

    case END_OF_LIST_TYPE:
      fputs("[]", fp);
      break;

    case VAR_TYPE:
      fprintf(fp, "_" WORD_OUTPUT_FORMAT, fixnum_to_word(slot_ref(x, 1)));
      break;

    case SYMBOL_TYPE: {
      X str = slot_ref(x, 0);
      char *name = (char *)objdata(str);
      WORD len = string_length(str);
      int q = 0;

      if(quote) {
	for(int i = 0; i < len; ++i) {
	  if((i == 0 && !islower(name[ i ])) || (name[ i ] != '_' && !isalpha(name[ i ]) && !isdigit(name[ i ]))) {
	    q = 1;
	    break;
	  }
	}
      }
      
      if(q) { 
	fputc('\'', fp);

	while(len--) {
	  char c = *(name++);
	  
	  switch(c) {
	  case '\n': fputs("\\n", fp); break;
	  case '\r': fputs("\\r", fp); break;
	  case '\t': fputs("\\t", fp); break;
	  default:
	    if(c < 32 || c > 127) 
	      fprintf(fp, "\\x%02x", c);
	    else 
	      fputc(c, fp);
	  }
	}

	fputc('\'', fp);
      }
      else 
	fprintf(fp, "%s", name);

      break;
    }

    case PORT_TYPE: { 
      PORT_BLOCK *p = (PORT_BLOCK *)x;
      fprintf(fp, "<%s-stream>(%p)", p->dir != ZERO ? "input" : "output", p->fp);
      break;
    }

    case PAIR_TYPE: { 
      fputc('[', fp);
      --limit;
      basic_write_term(fp, limit, quote, slot_ref(x, 0));
      int len = DEBUG_WRITE_TERM_LIST_LENGTH_LIMIT;

      for(x = slot_ref(x, 1); --len > 0 && objtype(x) == PAIR_TYPE; x = slot_ref(x, 1)) {
	fputs(", ", fp); 
	basic_write_term(fp, limit, quote, slot_ref(x, 0));
      }

      if(x == END_OF_LIST_VAL)
	fputc(']', fp);
      else if(len == 0)
	fputs("|...]", fp);
      else {
	fputc('|', fp);
	basic_write_term(fp, limit, quote, x);
	fputc(']', fp);
      }

      break;
    }

    case STRUCTURE_TYPE: {
      --limit;
      basic_write_term(fp, limit, quote, slot_ref(x, 0));
      fputc('(', fp);
      WORD len = objsize(x);
      
      for(int i = 1; i < len; ++i) {
	if(i > 1) 
	  fputs(", ", fp);

	basic_write_term(fp, limit, quote, slot_ref(x, i));
      }

      fputc(')', fp);
      break;
    }

    default:
      fprintf(fp, "<object of unknown type %p:" WORD_OUTPUT_FORMAT ">", (void *)x, objtype(x));
    }
  }
}


static void trace_write(char *title, char *name, int arity, X *A, CHOICE_POINT *C)
{
  FILE *fp = port_file(standard_error_port);

  fflush(port_file(standard_output_port));
  fprintf(fp, "[(%d) %s: %s", (int)(C - choice_point_stack), title, name);

  if(arity > 0) {
    fputc('(', fp);

    for(int i = 0; i < arity; ++i) {
      if(i > 0)
	fputs(", ", fp);

      basic_write_term(fp, TRACE_DEBUG_WRITE_LIMIT, 1, deref(A[ i ]));
    }
    
    fputc(')', fp);
  }

  fputs("]\n", fp);
}


#ifdef TRACE
# define TRACE_ENTER(name, arity)  trace_write("CALL", name, arity, A, C)
# define TRACE_REDO(name, arity)   trace_write("REDO", name, arity, A, C)
# define TRACE_EXIT(name, arity)   trace_write("EXIT", name, arity, A, C)
# define TRACE_FAIL(name, arity)   { if(C0 == C) trace_write("FAIL", name, arity, A, C); }
#else
# define TRACE_ENTER(name, arity)
# define TRACE_REDO(name, arity)
# define TRACE_EXIT(name, arity)
# define TRACE_FAIL(name, arity)
#endif


static int basic_write(X x) 
{ 
  basic_write_term(port_file(standard_output_port), 99999, 0, x); 
  return 1; 
}


static int basic_writeq(X x) 
{ 
  basic_write_term(port_file(standard_output_port), 99999, 1, x); 
  return 1; 
}
