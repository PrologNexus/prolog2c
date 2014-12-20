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

#ifndef TRAIL_STACK_SIZE
# define TRAIL_STACK_SIZE 10000
#endif

#ifndef CHOICE_POINT_STACK_SIZE
# define CHOICE_POINT_STACK_SIZE 10000
#endif

#ifndef ENVIRONMENT_STACK_SIZE 
# define ENVIRONMENT_STACK_SIZE 10000
#endif

#ifndef SYMBOL_TABLE_SIZE
# define SYMBOL_TABLE_SIZE 3001
#endif

#ifndef ARGUMENT_STACK_SIZE
# define ARGUMENT_STACK_SIZE 10000
#endif

#ifndef IFTHEN_STACK_SIZE
# define IFTHEN_STACK_SIZE 1000
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
  X *T, *R, *E, *A, *env_top, *arg_top;
  struct CHOICE_POINT *C0;
  void *P;
  WORD timestamp;
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

static STRING_BLOCK dot_name = { STRING_TYPE|3, "." };
static BLOCK dot_atom = { SYMBOL_TYPE|3, { &dot_name, NULL, NULL } };

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
static X argument_stack[ ARGUMENT_STACK_SIZE ];
static X trail_stack[ TRAIL_STACK_SIZE ];
static X *trail_top, *env_top, *arg_top;
static CHOICE_POINT choice_point_stack[ CHOICE_POINT_STACK_SIZE ];
static FINALIZER *active_finalizers = NULL, *free_finalizers = NULL;
static WORD gc_count = 0;
static char *mmapped_heap = NULL;
static char **global_argv;
static int global_argc;
static void *ifthen_stack[ IFTHEN_STACK_SIZE ], **ifthen_top = ifthen_stack;
static WORD clock_ticks = 0;

static CHAR *type_names[] = { 
  "invalid", "fixnum", "null", "symbol", "flonum", "stream", "variable", "string", "structure", "pair"
};


#define type_name(t)         (type_names[ (WORD)(t) & 0x1f ])
#define tag_to_type_name(t)  type_name(objtype(t))


/// debugging and termination

#define OUTPUT(...)  { fflush(stdout); fprintf(stderr, __VA_ARGS__); } 
#define CRASH(...)   { fflush(stdout); fprintf(stderr, "\n" __VA_ARGS__); fputc('\n', stderr); crash_hook(); exit(1); }

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


static inline int is_atom(X x)
{
  return x == END_OF_LIST_VAL || is_SYMBOL(x);
}


static inline int is_atomic(X x)
{
  return x == END_OF_LIST_VAL || is_FIXNUM(x) || is_FLONUM(x) || is_SYMBOL(x) || is_PORT(x);
}


static inline int is_compound(X x)
{
  return !is_FIXNUM(x) && (is_PAIR(x) || is_STRUCTURE(x));
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


static void write_barrier_error() { CRASH("assignment to non-mutable data detected"); }


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
    CRASH("type check failed - expected type %s but got fixnum", type_name(t));
  
  if(objtype(x) != t)
    CRASH("type check failed - expected type %s but got type %s", type_name(t), type_name(objtype(x)));
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
  CRASH("type check failed - expected number, but got %s", type_name(objtype(x)));
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
  CRASH("type check failed - expected integer, but got %s", type_name(objtype(x)));
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
  CRASH("type check failed - expected atomic, but got %s", type_name(objtype(x)));
}


static inline X check_atomic(X x)
{
#ifndef UNSAFE
  if(!is_FIXNUM(x) && !is_atomic(x))
    check_atomic_failed(x);
#endif
  
  return x;
}


static inline WORD check_index(X x, WORD i)
{
#ifndef UNSAFE
  if(i < 0 || i >= objsize(x))
    CRASH("index out of range - index is " WORD_OUTPUT_FORMAT ", size is " WORD_OUTPUT_FORMAT, i, objsize(x)); 
#endif

  return i;
}


static inline WORD check_index_STRING(X x, WORD i)
{
#ifndef UNSAFE
  WORD len = string_length(x);

  if(i < 0 || i >= len)
    CRASH("string index out of range - index is " WORD_OUTPUT_FORMAT ", size is " WORD_OUTPUT_FORMAT, i, len); 
#endif

  return i;
}

  
static inline X check_range(X x, WORD i, WORD j)
{
#ifndef UNSAFE
  WORD s = objsize(x);

  if(i < 0 || i > s || j < 0 || j > s)	      
    CRASH("index out of range - range is " WORD_OUTPUT_FORMAT "..." WORD_OUTPUT_FORMAT ", size is " 
	  WORD_OUTPUT_FORMAT, i, j, s); 
#endif

  return x;
}


static inline X check_range_STRING(X x, WORD i, WORD j)				
{
#ifndef UNSAFE
  WORD s = string_length(x);

  if(i < 0 || i > s || j < 0 || j > s) 
    CRASH("string index out of range - range is " WORD_OUTPUT_FORMAT "..." WORD_OUTPUT_FORMAT ", size is " 
	  WORD_OUTPUT_FORMAT, i, j, s); 
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
    CRASH("not an input-port");
#endif

  return x;
}


static inline X check_output_port(X x)
{
#ifndef UNSAFE
  check_type_PORT(x);

  if(slot_ref(x, 1) != ZERO)
    CRASH("not an output-port");
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


static void collect_garbage(int args)
{
  va_list va;
  DRIBBLE("[GC ... ");							
  tospace_top = tospace; 
  scan_ptr = tospace_top;				

  // mark local environments
  for(X *p = environment_stack; p < env_top; ++p)
    mark1(p);

  // mark argument stack
  for(X *p = argument_stack; p < arg_top; ++p)
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

      ASSERT(scan_ptr < tospace_end, "scan_ptr exceeded fromspace");
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
    DRIBBLE("%d symbols reclaimed ", gcdsyms);

  void *tmp = fromspace; 
  fromspace = tospace; 
  tospace = tmp;		
  tmp = fromspace_end; 
  fromspace_end = tospace_end;
  tospace_end = tmp;	
  fromspace_limit = (X)((char *)fromspace_end - heap_reserve);	
  alloc_top = tospace_top;
  DRIBBLE("finished (" WORD_OUTPUT_FORMAT " bytes in use, trail: " WORD_OUTPUT_FORMAT ")]\n",
	  ((WORD)((char *)tospace_top - (char *)fromspace)),
	  (WORD)(trail_top - trail_stack)); 
  ++gc_count;
  
  if(alloc_top >= fromspace_limit) 
    CRASH("heap exhausted");				

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
  global_argc = argc;
  global_argv = argv;

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
      CRASH("unable to open heap file: %s", strerror(errno));

    if(ftruncate(fd, heapsize) == -1)
      CRASH("unable to create heap file: %s", strerror(errno));

    fromspace = mmap(NULL, heapsize, PROT_READ | PROT_WRITE, MAP_SHARED, fd, 0);

    if(fromspace == MAP_FAILED) 
      CRASH("unable to map heap file into memory: %s", strerror(errno));

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
  env_top = environment_stack;
  arg_top = argument_stack;

  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i)
    symbol_table[ i ] = END_OF_LIST_VAL;
}


static void terminate(int code)
{
  DRIBBLE("[trail size: " WORD_OUTPUT_FORMAT ", terminating]\n", trail_top - trail_stack);
  exit(code);
}


/// variable + trail handling

static inline X make_var()
{
  ALLOCATE_BLOCK(BLOCK *v, VAR_TYPE, 3);
  v->d[ 0 ] = (X)v;
  v->d[ 1 ] = word_to_fixnum(variable_counter++);
  v->d[ 2 ] = word_to_fixnum(clock_ticks++);
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
      CRASH("deref-stack overflow");
#endif
  }
}


static void unwind_trail(X *tp)
{
  while(trail_top != tp) {
    BLOCK *var = (BLOCK *)(*(--trail_top));
    //DRIBBLE("[detrail: _" WORD_OUTPUT_FORMAT "]\n", fixnum_to_word(slot_ref((X)var, 1)));
    SLOT_SET(var, 0, var);
  }
}


static inline void push_trail(CHOICE_POINT *C0, X var)
{
  // trail-check
  if(fixnum_to_word(slot_ref(var, 2)) < C0->timestamp) {
#ifndef UNSAFE
    if(trail_top >= trail_stack + TRAIL_STACK_SIZE)
      CRASH("trail-stack overflow.");
#endif

    *(trail_top++) = var;
  }
}


/// basic I/O 

// stream-name
static char *port_name(X x)
{
  PORT_BLOCK *p = (PORT_BLOCK *)x;
  static CHAR buffer[ 256 ];
  sprintf(buffer, "<%s-stream>(%p)", p->dir != ZERO ? "input" : "output", p->fp);
  return buffer;
}


// doesn't quote atoms or respects operators - expect's deref'd datum
static void basic_write_term(FILE *fp, int debug, int limit, int quote, X x) { 
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

    case PORT_TYPE:
      fputs(port_name(x), fp);
      break;

    case PAIR_TYPE: { 
      fputc('[', fp);
      --limit;
      basic_write_term(fp, debug, limit, quote, deref(slot_ref(x, 0)));
      int len = debug ? DEBUG_WRITE_TERM_LIST_LENGTH_LIMIT : 999999;

      for(x = deref(slot_ref(x, 1)); 
	  --len > 0 && !is_FIXNUM(x) && objtype(x) == PAIR_TYPE; 
	  x = deref(slot_ref(x, 1))) {
	fputs(", ", fp); 
	basic_write_term(fp, debug, limit, quote, deref(slot_ref(x, 0)));
      }

      if(x == END_OF_LIST_VAL)
	fputc(']', fp);
      else if(len == 0)
	fputs("|...]", fp);
      else {
	fputc('|', fp);
	basic_write_term(fp, debug, limit, quote, deref(x));
	fputc(']', fp);
      }

      break;
    }

    case STRUCTURE_TYPE: {
      --limit;
      basic_write_term(fp, debug, limit, quote, slot_ref(x, 0));
      fputc('(', fp);
      WORD len = objsize(x);
      
      for(int i = 1; i < len; ++i) {
	if(i > 1) 
	  fputs(", ", fp);

	basic_write_term(fp, debug, limit, quote, deref(slot_ref(x, i)));
      }

      fputc(')', fp);
      break;
    }

    default:
      fprintf(fp, "<object of unknown type %p:" WORD_OUTPUT_FORMAT ">", (void *)x, objtype(x));
    }
  }
}


// debugging and tracing support

static void write_hook(X x)
{
  basic_write_term(stdout, 1, TRACE_DEBUG_WRITE_LIMIT, 1, deref(x));
  putchar('\n');
}


static void trace_write(char *title, char *name, int arity, X *A, CHOICE_POINT *C)
{
  FILE *fp = port_file(standard_error_port);

  fflush(port_file(standard_output_port));
  fprintf(fp, "[(%d/%d/%d) %s: %s", (int)(C - choice_point_stack), 
	  (int)(arg_top - argument_stack), (int)(trail_top - trail_stack),
	  title, name);

  if(arity > 0) {
    fputc('(', fp);

    for(int i = 0; i < arity; ++i) {
      if(i > 0)
	fputs(", ", fp);

      basic_write_term(fp, 1, TRACE_DEBUG_WRITE_LIMIT, 1, deref(A[ i ]));
    }
    
    fputc(')', fp);
  }

  fputs("]\n", fp);
}


#ifdef TRACE
# define TRACE_ENTER(name, arity)  trace_write("CALL", name, arity, C0->A, C)
# define TRACE_REDO(name, arity)   trace_write("REDO", name, arity, C0->A, C)
# define TRACE_EXIT(name, arity)   trace_write("EXIT", name, arity, C0->A, C)
# define TRACE_FAIL(name, arity)   { if(C0 == C) trace_write("FAIL", name, arity, C0->A, C); }
# define TRACE_DETERMINATE_CALL(name, arity)  trace_write("TAIL", name, arity, C0->A, C);
#else
# define TRACE_ENTER(name, arity)
# define TRACE_REDO(name, arity)
# define TRACE_EXIT(name, arity)
# define TRACE_FAIL(name, arity)
# define TRACE_DETERMINATE_CALL(name, arity)
#endif


/// unification

#define unify(C0, x, y)   ({ X _x = (x), _y = (y); _x == _y || unify1(C0, _x, _y); })


static int unify1(CHOICE_POINT *C0, X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x) && is_FIXNUM(y)) 
    return x == y;

  WORD xt = is_FIXNUM(x) ? FIXNUM_TYPE : objtype(x);
  WORD yt = is_FIXNUM(y) ? FIXNUM_TYPE : objtype(y);

  if(xt == VAR_TYPE) {
    /*
    if(verbose) {
      DRIBBLE("[binding _" WORD_OUTPUT_FORMAT " <- ", fixnum_to_word(slot_ref(x, 1)));
      basic_write_term(stderr, 1, 9999, 1, y);
      fputs("]\n", stderr);
    }
    */

    SLOT_SET(x, 0, y);
    push_trail(C0, x);
    return 1;
  }

  if(yt == VAR_TYPE) {
    /*
    if(verbose) {
      DRIBBLE("[binding _" WORD_OUTPUT_FORMAT " <- ", fixnum_to_word(slot_ref(y, 1)));
      basic_write_term(stderr, 1, 9999, 1, x);
      fputs("]\n", stderr);
    }
    */

    SLOT_SET(y, 0, x);
    push_trail(C0, y);
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
    if(!unify(C0, slot_ref(x, i), slot_ref(y, i)))
      return 0;

    ++i;
  }

  return 1;
}


/// term-construction

static X make_term(int arity, X functor, ...)
{
  va_list va;
  va_start(va, functor);
  functor = deref(functor);
  check_type(SYMBOL_TYPE, functor);

  if(arity == 2 && !strcmp(".", (CHAR *)objdata(slot_ref(functor, 0)))) {
    X car = va_arg(va, X);
    return PAIR(car, va_arg(va, X));
  }

  ALLOCATE_BLOCK(BLOCK *s, STRUCTURE_TYPE, arity + 1);
  s->d[ 0 ] = functor;

  for(int i = 1; i <= arity; ++i)
    s->d[ i ] = deref(va_arg(va, X));

  return (X)s;
}


#define make_pair    PAIR


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

#define INTEGER_BINARY_OP(name, op)  \
  static inline X name(X x, X y)		\
  { x = deref(x); \
    y = deref(y); \
    check_fixnum(x); \
    check_fixnum(y); \
    return word_to_fixnum(fixnum_to_word(x) op fixnum_to_word(y)); }

INTEGER_BINARY_OP(num_and, &)
INTEGER_BINARY_OP(num_or, |)
INTEGER_BINARY_OP(num_shl, <<)
INTEGER_BINARY_OP(num_shr, >>)


static inline X num_div(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y)) {
#ifndef UNSAFE
      if(y == word_to_fixnum(0))
	CRASH("division by zero");
#endif

      return FLONUM(fixnum_to_float(x) / fixnum_to_float(y));	
    }

    if(is_FLONUM(y))						
      return FLONUM(fixnum_to_float(x) / flonum_to_float(y));	
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y)) {
#ifndef UNSAFE
      if(y == word_to_fixnum(0))
	CRASH("division by zero");
#endif

      return FLONUM(flonum_to_float(x) / fixnum_to_float(y));  
    }

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
  check_fixnum(x);
  check_fixnum(y);

#ifndef UNSAFE
  if(y == word_to_fixnum(0))
    CRASH("division by zero");
#endif

  return word_to_fixnum(fixnum_to_word(x) / fixnum_to_word(y));
}


static inline X num_rem(X x, X y) 
{
  x = deref(x);
  y = deref(y);
  check_fixnum(x);
  check_fixnum(y);

#ifndef UNSAFE
  if(y == word_to_fixnum(0))
    CRASH("division by zero");
#endif

  return word_to_fixnum(fixnum_to_word(x) % fixnum_to_word(y));
}


static inline X num_mod(X x, X y)
{
  WORD x2 = fixnum_to_word(check_fixnum(deref(x)));
  WORD y2 = fixnum_to_word(check_fixnum(deref(y)));

#ifndef UNSAFE
  if(y2 == 0)
    CRASH("division by zero");
#endif

  WORD z = x2 % y2;
    
  if(y2 < 0)
    return z <= 0 ? word_to_fixnum(z) : word_to_fixnum(z + y2);
  else 
    return z >= 0 ? word_to_fixnum(z) : word_to_fixnum(z + y2);
}


static inline X num_pow(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x)) {
    if(is_FIXNUM(y))
      return FLONUM(pow(fixnum_to_float(x), fixnum_to_float(y)));	

    if(is_FLONUM(y))						
      return FLONUM(pow(fixnum_to_float(x), flonum_to_float(y)));	
  
    check_number_failed(y);
  }					

  if(is_FLONUM(x)) {						
    if(is_FIXNUM(y))
      return FLONUM(pow(flonum_to_float(x), fixnum_to_float(y)));  

    if(is_FLONUM(y))							
      return FLONUM(pow(flonum_to_float(x), flonum_to_float(y)));		

    check_number_failed(y);
  }						
  
  check_number_failed(x); 
}


static inline X num_frac(X x)
{
  double i;
  return FLONUM(modf(flonum_to_float(check_type_FLONUM(deref(x))), &i));
}


static inline X num_int(X x)
{
  double i;
  modf(flonum_to_float(check_type_FLONUM(deref(x))), &i);
  return FLONUM(i);
}


static inline X num_floor(X x) { return FLONUM(floor(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_ceiling(X x) { return FLONUM(ceil(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_round(X x) { return FLONUM(round(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_truncate(X x) { return FLONUM(trunc(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_sin(X x) { return FLONUM(sin(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_cos(X x) { return FLONUM(cos(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_atan(X x) { return FLONUM(atan(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_exp(X x) { return FLONUM(exp(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_log(X x) { return FLONUM(log(flonum_to_float(check_type_FLONUM(deref(x))))); }
static inline X num_sqrt(X x) { return FLONUM(sqrt(flonum_to_float(check_type_FLONUM(deref(x))))); }


static inline X num_abs(X x)
{ 
  x = deref(x);

  // no need to convert to word - we only manipulate the sign
  if(is_FIXNUM(x))
    return x < 0 ? (X)(-(WORD)x) : x;

  if(is_FLONUM(x)) {
    double n = flonum_to_float(x);

    if(n < 0) return FLONUM(-n);
    
    return x;
  }

  check_number_failed(x);
}


static inline X num_sign(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x)) {
    if(x < 0)
      return word_to_fixnum(-1);

    if(x > 0) 
      return word_to_fixnum(1);
      
    return word_to_fixnum(0);
  }

  if(is_FLONUM(x)) {
    double n = flonum_to_float(x);

    if(n < 0.0) return FLONUM(-1);
    
    if(n > 0.0) return FLONUM(1);

    return FLONUM(0);
  }

  check_number_failed(x);
}


static inline X num_negate(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x)) 
    return word_to_fixnum(-fixnum_to_word(x));

  if(is_FLONUM(x))
    return FLONUM(-flonum_to_float(x));

  check_number_failed(x);
}


static inline X num_not(X x) 
{ 
  return word_to_fixnum(~fixnum_to_word(check_fixnum(deref(x))));
}


/// term comparison

static inline int is_identical(X x, X y)
{
  if(x == y) return 1;
  
  return deref(x) == deref(y);
}


static inline int compare_strings(CHAR *str1, WORD len1, CHAR *str2, WORD len2)
{
  WORD d = strncmp(str1, str2, len1 < len2 ? len1 : len2);
  return d == 0 ? len2 - len1 : -d;
}


static int compare_terms(X x, X y)
{
  if(x == y) return 0;

  WORD xt = is_FIXNUM(x) ? FIXNUM_TYPE : objtype(x);
  WORD yt = is_FIXNUM(y) ? FIXNUM_TYPE : objtype(y);
  static int type_order[] = { 0, 3, 4, 4, 2, 4, 1, 0, 5, 5 };
  WORD d = type_order[ yt & 0x1f ] - type_order[ xt & 0x1f ];

  if(d != 0) return d;

  switch(xt) {
  case FIXNUM_TYPE: return fixnum_to_word(y) - fixnum_to_word(x);

  case END_OF_LIST_TYPE: 
    switch(yt) {
    case END_OF_LIST_TYPE: return 0;

    case SYMBOL_TYPE:
      { X str = slot_ref(y, 0);
	return compare_strings("[]", 2, (CHAR *)objdata(str), string_length(str)); }

    case PORT_TYPE: return '<' - '[';
    }

  case SYMBOL_TYPE:
    switch(yt) {
    case SYMBOL_TYPE:
      { X str1 = slot_ref(x, 0);
	X str2 = slot_ref(y, 0);
	WORD len1 = string_length(str1);
	WORD len2 = string_length(str2);
	return compare_strings((CHAR *)objdata(str1), len1, (CHAR *)objdata(str2), len2); }

    case END_OF_LIST_TYPE:
      { X str = slot_ref(x, 0);
	return compare_strings((CHAR *)objdata(str), string_length(str), "[]", 2); }
      
    case PORT_TYPE:
      { char *buf = port_name(y);
	X str = slot_ref(x, 0);
	return compare_strings((CHAR *)objdata(str), string_length(str), buf, strlen(buf)); }
    }

  case PORT_TYPE:
    switch(yt) {
    case END_OF_LIST_TYPE:
      { char *buf = port_name(x);
	return compare_strings(buf, strlen(buf), "[]", 2); }
      
    case SYMBOL_TYPE:
      { char *buf = port_name(x);
	X str = slot_ref(y, 0);
	return compare_strings(buf, strlen(buf), (CHAR *)objdata(str), string_length(str)); }
      
    case PORT_TYPE:
      { char *buf1 = port_name(x);
	char *buf2 = port_name(y);
	return compare_strings(buf1, strlen(buf1), buf2, strlen(buf2)); }
    }

  case FLONUM_TYPE: 
    if(flonum_to_float(x) < flonum_to_float(y)) return 1;
      
    if(flonum_to_float(x) > flonum_to_float(y)) return -1;
      
    return 0;
    
  case VAR_TYPE:
    return slot_ref(y, 1) - slot_ref(x, 1);

  case STRUCTURE_TYPE:
    switch(yt) {
    case STRUCTURE_TYPE:
      { WORD s = objsize(y);
	d = s - objsize(x);

	if(d != 0) return d;
      
	d = compare_terms(slot_ref(x, 0), slot_ref(y, 0));
    
	if(d != 0) return d;

	for(int i = 1; i < s; ++i) {
	  d = compare_terms(slot_ref(x, i), slot_ref(y, i));

	  if(d != 0) return d;
	}

	return 0; }

    case PAIR_TYPE:
      d = 3 - objsize(x);

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 0), (X)&dot_atom);

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 1), slot_ref(y, 0));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 2), slot_ref(y, 1));

    }

  case PAIR_TYPE:
    switch(yt) {
    case STRUCTURE_TYPE:
      d = objsize(y) - 3;

      if(d != 0) return d;

      d = compare_terms((X)&dot_atom, slot_ref(y, 0));

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 0), slot_ref(x, 1));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 1), slot_ref(y, 2));

    case PAIR_TYPE:
      d = compare_terms(slot_ref(x, 0), slot_ref(y, 0));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 1), slot_ref(y, 1));

    }
  }

  CRASH("compare_terms");
}


/// VM operations

#define CURRENT_NAME
#define CURRENT_ARITY

#define ENTER								\
  { CHECK_LIMIT;							\
    PUSH_CHOICE_POINT(NULL);						\
    TRACE_ENTER(CURRENT_NAME, CURRENT_ARITY); }

#define PUSH_CHOICE_POINT(lbl)					\
  { C->T = trail_top;						\
    C->R = R;							\
    C->E = E;							\
    C->A = A;							\
    C->timestamp = clock_ticks++;				\
    C->arg_top = arg_top;					\
    C->env_top = env_top;					\
    C->C0 = C0;							\
    C->P = lbl;							\
    C0 = C++;							\
    ASSERT(C < choice_point_stack + CHOICE_POINT_STACK_SIZE, "choice-point stack overflow"); }

#define COPY_CHOICE_POINT(lbl)						\
  { C->T = trail_top;							\
    C->R = C0->R;							\
    C->E = E;								\
    C->A = C0->A;							\
    C->timestamp = clock_ticks++;					\
    C->arg_top = arg_top;						\
    C->env_top = env_top;						\
    C->C0 = C0;								\
    C->P = lbl;								\
    ++C;								\
    ASSERT(C < choice_point_stack + CHOICE_POINT_STACK_SIZE, "choice-point stack overflow"); }

#define ADJUST_CHOICE_POINT(lbl)						\
  { C0->T = trail_top;							\
    C0->arg_top = arg_top;						\
    C0->env_top = env_top;						\
    C0->P = lbl; }

#define SAVE_CHOICE_POINTS						\
  { *(ifthen_top++) = C0->P; *(ifthen_top++) = C0; *(ifthen_top++) = C; \
    ASSERT(ifthen_top < ifthen_stack + IFTHEN_STACK_SIZE, "if-then stack overflow"); }

#define RESTORE_CHOICE_POINTS   { C = *(--ifthen_top); C0 = *(--ifthen_top); C0->P = *(--ifthen_top); }
#define POP_CHOICE_POINT        C0 = C0->C0

#define EXIT				     \
  { TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    R = C0->R;				     \
    E = C0->E;				     \
    env_top = C0->env_top;		     \
    C0 = C0->C0;			     \
    goto *R; }

#define DETERMINATE_EXIT		     \
  { TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    R = C0->R;				     \
    E = C0->E;				     \
    env_top = C0->env_top;		     \
    C0 = C0->C0;			     \
    C = C0 + 1;				     \
    goto *R; }

#define FAIL     { TRACE_FAIL(CURRENT_NAME, CURRENT_ARITY); goto fail; }
#define REDO     TRACE_REDO(CURRENT_NAME, CURRENT_ARITY)

#define POP_ARGUMENTS   arg_top = C0->arg_top - CURRENT_ARITY

#define DETERMINATE_CALL(lbl)				 \
  { TRACE_DETERMINATE_CALL(CURRENT_NAME, CURRENT_ARITY); \
    R = C0->R;						 \
    E = C0->E;						 \
    env_top = C0->env_top;				 \
    C0 = C0->C0;					 \
    C = C0 + 1;						 \
    goto lbl; }

#define CHECK_LIMIT				\
  if(alloc_top > fromspace_limit) {		\
    collect_garbage(CURRENT_ARITY);		\
  }

#define ENVIRONMENT(len)  { E = env_top; env_top += (len); }

#define SET_REDO(lbl)   C0->P = (lbl)
#define CUT             { C = C0 + 1; SET_REDO(NULL); }

#define INVOKE_CHOICE_POINT			\
  { for(C0 = C - 1; C0->P == NULL; --C0);	\
    C = C0 + 1;					\
    unwind_trail(C0->T);			\
    A = C0->A;					\
    arg_top = C0->arg_top;			\
    env_top = C0->env_top;			\
    goto *(C0->P); }


/// Boilerplate code

#define BOILERPLATE					\
  X *A = NULL;						\
  CHOICE_POINT *C, *C0;					\
  void *R = &&success_exit;				\
  void *R0;						\
  X *E = env_top;					\
  initialize(argc, argv);				\
  intern_static_symbols(PREVIOUS_SYMBOL);		\
  C0 = C = choice_point_stack;				\
  C->T = trail_top;					\
  C->timestamp = clock_ticks++;				\
  C->R = NULL;						\
  C->C0 = NULL;						\
  C->P = &&fail_exit;					\
  C++;							\
  goto INIT_GOAL;					\
fail: INVOKE_CHOICE_POINT;				\
fail_exit: fprintf(stderr, "false.\n"); terminate(1);	\
success_exit: DRIBBLE("true.\n"); terminate(0);


////////////////////////////////////////////////////////////////////////////////

/// PRIMITIVES (all expect their arguments to be deref'd)


#define PRIMITIVE(name, ...)	   static int name(CHOICE_POINT *C0, __VA_ARGS__)


PRIMITIVE(debug_hook, X x) { return 1; }
PRIMITIVE(write_char, X c) { check_fixnum(c); fputc(fixnum_to_word(c), port_file(standard_output_port)); return 1; }

PRIMITIVE(basic_write, X x) 
{ 
  basic_write_term(port_file(standard_output_port), 0, 99999, 0, x); 
  return 1; 
}

PRIMITIVE(basic_writeq, X x) 
{ 
  basic_write_term(port_file(standard_output_port), 0, 99999, 1, x); 
  return 1; 
}

PRIMITIVE(gc, X dummy) { alloc_top = fromspace_limit + 1; return 1; }

PRIMITIVE(halt, X code) 
{ 
  check_fixnum(code);
  terminate(fixnum_to_word(code));
  return 1;			/* never executed */
}

PRIMITIVE(command_line_arguments, X var)
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

  return unify(C0, lst, var);
}
