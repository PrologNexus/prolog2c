//// pc.h - prolog runtime system


#ifndef PC_H
#define PC_H


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
#include <setjmp.h>


/// limits - all sizes are in bytes

#ifndef HEAP_SIZE
# define HEAP_SIZE  100000000
#endif

#ifndef HEAP_RESERVE
// percentage
# define HEAP_RESERVE 20
#endif

#ifndef TRAIL_STACK_SIZE
# define TRAIL_STACK_SIZE 1000000
#endif

#ifndef CHOICE_POINT_STACK_SIZE
# define CHOICE_POINT_STACK_SIZE 10000000
#endif

#ifndef ENVIRONMENT_STACK_SIZE 
# define ENVIRONMENT_STACK_SIZE 10000000
#endif

#ifndef ARGUMENT_STACK_SIZE
# define ARGUMENT_STACK_SIZE 10000000
#endif

#ifndef IFTHEN_STACK_SIZE
# define IFTHEN_STACK_SIZE 10000
#endif

#ifndef SYMBOL_TABLE_SIZE
# define SYMBOL_TABLE_SIZE 3001
#endif


// these sizes are given in elements
#define DEREF_STACK_SIZE 100
#define MAXIMAL_NUMBER_OF_ARGUMENTS 100
#define DEBUG_WRITE_TERM_LIST_LENGTH_LIMIT 10
#define TRACE_DEBUG_WRITE_LIMIT 5
#define INITIAL_FREEZE_TERM_VAR_TABLE_SIZE 1000
#define CIRCULAR_TERM_TABLE_SIZE 1000
#define MAX_GLOBAL_VARIABLES 256
#define STRING_BUFFER_SIZE 1024
#define CATCHER_STACK_SIZE 1024


/// miscellanous

#ifdef __LLP64__
# define WORD_OUTPUT_FORMAT_LENGTH  "ll"
#else
# define WORD_OUTPUT_FORMAT_LENGTH  "l"
#endif

#define WORD_OUTPUT_FORMAT   "%" WORD_OUTPUT_FORMAT_LENGTH "d"
#define FLOAT_OUTPUT_FORMAT  "%.15g"

#ifdef UNSAFE
# ifdef TRACE
#  undef TRACE
# endif
#endif


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
  FLOAT n;
} FLONUM_BLOCK;

typedef struct PORT_BLOCK {
  WORD h;
  FILE *fp;
  X dir;			/* 1 = input */
  X open;			/* 1 = open */
  X data;
} PORT_BLOCK;

typedef struct CHOICE_POINT {
  X *T, *R, *E, *A, *env_top, *arg_top;
  struct CHOICE_POINT *C0;
  void *P;
  struct CATCHER *catch_top;
  WORD timestamp;
} CHOICE_POINT;

typedef struct FINALIZER
{
  X object;
  void (*finalizer)(X);
  struct FINALIZER *next;
} FINALIZER;

typedef struct DB
{
  char *name;
  WORD tablesize;
  struct DB_BUCKET **table;
} DB;

typedef struct DB_BUCKET
{
  DB *db;
  WORD index;
  char *key;
  WORD keylen;
  struct DB_ITEM *firstitem, *lastitem;
  struct DB_BUCKET *previous, *next;
} DB_BUCKET;

typedef struct DB_ITEM 
{
  X val;
  WORD refcount;
  WORD erased;
  DB_BUCKET *bucket;
  struct DB_ITEM *previous, *next;
  struct DB_ITEM *next_deleted;
} DB_ITEM;

typedef struct CATCHER
{
  CHOICE_POINT *C0;
  X ball;
  X *E, *T, *env_top, *arg_top;
  void *P;
  void **ifthen_top;
} CATCHER;


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
#define DBREFERENCE_TYPE 0x4a

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
#define DBREFERENCE_TAG TYPE_TO_TAG(DBREFERENCE_TYPE)

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
#define ONE      word_to_fixnum(1)

#define END_OF_LIST_VAL  ((X)(&END_OF_LIST_VAL_BLOCK))

#define PREVIOUS_SYMBOL  END_OF_LIST_VAL


/// predefined literals and global variables

#ifdef COMPILED_PROLOG_PROGRAM
static BLOCK END_OF_LIST_VAL_BLOCK = { END_OF_LIST_TAG, {0}};
static X dot_atom, system_error_atom;

static PORT_BLOCK default_input_port = { PORT_TAG|4, NULL, ONE, ONE, ZERO };
static PORT_BLOCK default_output_port = { PORT_TAG|4, NULL, ZERO, ONE, ZERO };
static PORT_BLOCK default_error_port = { PORT_TAG|4, NULL, ZERO, ONE, ZERO };

static X standard_input_port = (X)(&default_input_port);
static X standard_output_port = (X)(&default_output_port);
static X standard_error_port = (X)(&default_error_port);

static X *fromspace, *fromspace_end, *fromspace_limit, *tospace, *tospace_end, *tospace_top, *scan_ptr;
static X *alloc_top;
static X symbol_table[ SYMBOL_TABLE_SIZE ];
static WORD heap_reserve;
static int verbose = 0;
static int variable_counter = 0;
static X *environment_stack;
static X *argument_stack;
static X *trail_stack;
static X *trail_top, *env_top, *arg_top;
static CHOICE_POINT *choice_point_stack;
static FINALIZER *active_finalizers = NULL, *free_finalizers = NULL;
static WORD gc_count = 0;
static char *mmapped_heap = NULL;
static char **global_argv;
static int global_argc;
static void **ifthen_stack, **ifthen_top;
static WORD clock_ticks = 0;
static X *freeze_term_var_table;
static int freeze_term_var_table_size;
static int freeze_term_var_counter;
static int global_variable_counter = 0;
static X circular_term_table[ CIRCULAR_TERM_TABLE_SIZE * 2 ];
static int circular_term_counter = 0;
static char *string_buffer;
static int string_buffer_length;
static DB_ITEM *deleted_db_items = NULL;
static int debugging = 0;
static WORD environment_stack_size, argument_stack_size, choice_point_stack_size, trail_stack_size, ifthen_stack_size;
static jmp_buf exception_handler;
static CATCHER catch_stack[ CATCHER_STACK_SIZE ];
static CATCHER *catch_top = catch_stack;


// externally visible (the only one that is)
X global_variables[ MAX_GLOBAL_VARIABLES ];

static CHAR *type_names[] = { 
  "invalid", "fixnum", "null", "symbol", "flonum", "stream", "variable", "string", "structure", "pair", "dbreference"
};
#endif

#define type_name(t)         (type_names[ (WORD)(t) & 0x1f ])
#define tag_to_type_name(t)  type_name(objtype(t))


/// debugging and termination

#define OUTPUT(...)  { fflush(stdout); fprintf(stderr, __VA_ARGS__); } 
#define CRASH(...)   { fflush(stdout); fprintf(stderr, "\n" __VA_ARGS__); fputc('\n', stderr); crash_hook(); exit(EXIT_FAILURE); }

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
#define is_DBREFERENCE(x)  is(DBREFERENCE_TYPE, (x))

#define GLOBAL_REF(index)  global_variables[ index ]
#define GLOBAL_SET(index, x)  global_variables[ index ] = deref(x)


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


#ifdef COMPILED_PROLOG_PROGRAM


static char *port_name(X x)
{
  PORT_BLOCK *p = (PORT_BLOCK *)x;
  static CHAR buffer[ 256 ];
  sprintf(buffer, "<%s-stream>(%p)", p->dir != ZERO ? "input" : "output", (void *)p->fp);
  return buffer;
}


static void crash_hook()
{
  return;
}


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

// does not initialize args
#define STRUCTURE(functor, arity)				\
  ({ ALLOCATE_BLOCK(BLOCK *s_, STRUCTURE_TYPE, (arity) + 1);	\
  SLOT_INIT((X)s_, 0, (functor));				\
  (X)s_; })


/// Variable dereferencing

#define deref(x)   ({ X _x = (x); is_FIXNUM(_x) || !is_VAR(_x) ? _x : deref1(_x); })

static X deref1(X val)
{
  static X stack[ DEREF_STACK_SIZE ];
  X *sp = stack;

  for(;;) {
    if(is_FIXNUM(val) || !is_VAR(val))
      return val;

    for(X *p = sp - 1; p >= stack; --p) {
      if(*p == val) 
	return val;
    }

    *(sp++) = val;
    val = slot_ref(val, 0);

#ifndef UNSAFE
    if(sp >= stack + DEREF_STACK_SIZE)
      CRASH("deref-stack overflow");
#endif
  }
}


/// basic I/O 


// doesn't respect operators - expect's deref'd datum
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
	if(len == 0 || !islower(name[ 0 ])) q = 1;
	else {
	  for(int i = 0; i < len; ++i) {
	    if(name[ i ] != '_' && !isalpha(name[ i ]) && !isdigit(name[ i ])) {
	      q = 1;
	      break;
	    }
	  }
	}
      }
      
      if(q) { 
	fputc('\'', fp);

	while(len--) {
	  int c = *(name++);
	  
	  switch(c) {
	  case '\n': fputs("\\n", fp); break;
	  case '\r': fputs("\\r", fp); break;
	  case '\t': fputs("\\t", fp); break;
	  case '\'': fputs("\\'", fp); break;
	  case '\\': fputs("\\\\", fp); break;
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

    case DBREFERENCE_TYPE:
      fprintf(fp, "<dbreference>(%p)", (void *)slot_ref(x, 0));
      break;

    default:
      fprintf(fp, "<object of unknown type %p:" WORD_OUTPUT_FORMAT ">", (void *)x, objtype(x));
    }
  }
}


/// symbol-table management

static WORD hash_name(CHAR *name, int len)
{
  unsigned long key = 0;

  while(len--)
    key ^= (key << 6) + (key >> 2) + *(name++);

  return (WORD)(key & 0x7fffffffUL);
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

    symbol_table[ key ] = sym1;
    sym1 = nextsym;
  }

  dot_atom = intern(CSTRING("."));
  system_error_atom = intern(CSTRING("system_error"));
  type_error_atom = intern(CSTRING("type_error"));
  evaluation_error_atom = intern(CSTRING("evaluation_error"));
  instantiation_error_atom = intern(CSTRING("instantiation_error"));
}


/// Exception handling

static void throw_exception(X ball)
{
  if(catch_top == catch_stack) {
    fflush(stdout);
    fputs("\nUnhandled exception:\n", stderr);
    basic_write_term(stderr, 1, 10, 1, ball);
    fputc('\n', stderr);
    crash_hook();
    exit(EXIT_FAILURE);
  }

  --catch_top;
  arg_top = catch_top->arg_top;
  catch_top->ball = ball;
  longjmp(exception_handler, 1);
}


static void system_error(char *msg)
{
  X str = intern(CSTRING(msg));
  X exn = STRUCTURE(system_error_atom, 1);
  SLOT_INIT(exn, 1, str);
  throw_exception(exn);
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
  return x;			/* never executed */
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
#define check_type_STRUCTURE(x)  check_type(STRUCTURE_TYPE, (x))
#define check_type_DBREFERENCE(x)  check_type(DBREFERENCE_TYPE, (x))


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


/// Variable + trail handling

static inline X make_var()
{
  ALLOCATE_BLOCK(BLOCK *v, VAR_TYPE, 3);
  v->d[ 0 ] = (X)v;
  v->d[ 1 ] = word_to_fixnum(variable_counter++);
  v->d[ 2 ] = word_to_fixnum(clock_ticks++);
  return v;
}


static void unwind_trail(X *tp)
{
  while(trail_top != tp) {
    BLOCK *var = (BLOCK *)(*(--trail_top));
#ifdef DEBUGGING
    DRIBBLE("[detrail: _" WORD_OUTPUT_FORMAT "]\n", fixnum_to_word(slot_ref((X)var, 1)));
#endif
    SLOT_SET(var, 0, var);
  }
}


static inline void push_trail(CHOICE_POINT *C0, X var)
{
  // trail-check
  if(fixnum_to_word(slot_ref(var, 2)) < C0->timestamp) {
#ifndef UNSAFE
    if(trail_top >= trail_stack + trail_stack_size)
      CRASH("trail-stack overflow");
#endif

    *(trail_top++) = var;
  }
}


// returns location of associated key + value in circular-term-table
static X *lookup_circular_term(X x)
{
  WORD key = (WORD)x % CIRCULAR_TERM_TABLE_SIZE;
  int f = 0;
  key *= 2;

  while(circular_term_table[ key ] != x) {
    if(circular_term_table[ key ] == NULL) /* unused entry? */
      break;

    // try next
    key += 2;

    if(key >= (CIRCULAR_TERM_TABLE_SIZE * 2)) { /* beyond table size? */
      if(f) CRASH("circular-term table overflow"); /* already started from the beginning? */
      
      f = 1;
      key = 0;			/* start over from the beginning */
    }
    else if(key >= circular_term_counter) /* in unused table part? */
      break;
  }

  if(key > circular_term_counter)
    circular_term_counter = key + 2;

  return &(circular_term_table[ key ]); /* entry found */
} 


static X find_frozen_variable(X var)
{
  //XXX could use hashing, but probably not worth the trouble
  for(int i = 0; i < freeze_term_var_counter; i += 2) {
    if(var == freeze_term_var_table[ i ]) 
      return freeze_term_var_table[ i + 1 ];
  }

  return (X)NULL;
}


static void ensure_freeze_term_var_table_size(WORD index)
{
  if(index + 2 >= freeze_term_var_table_size) {
    WORD newsize = index + index / 4;
    freeze_term_var_table = (X *)realloc(freeze_term_var_table, newsize * sizeof(X));
    ASSERT(freeze_term_var_table, "out of memory - can not reallocate freeze-term variable table");
    memset(freeze_term_var_table + freeze_term_var_table_size, 0,
	   (newsize - freeze_term_var_table_size) * sizeof(X));
    freeze_term_var_table_size = newsize;
  }
}


// deref recursively, needed for global variables (or the assigned value may not survive backtracking)
static X deref_recursive(X val, int limit, int *failed)
{
  *failed = 0;

  if(limit <= 0 || is_FIXNUM(val)) return val;

  if(is_VAR(val)) {
    val = deref1(val);

    if(is_VAR(val)) {
      X x = find_frozen_variable(val);

      if(x == NULL) {
	ensure_freeze_term_var_table_size(freeze_term_var_counter);
	freeze_term_var_table[ freeze_term_var_counter++ ] = val;

	if(alloc_top + 4 > fromspace_limit) {
	  *failed = 1;
	  return val;
	}

	ALLOCATE_BLOCK(BLOCK *newvar, VAR_TYPE, 3);
	newvar->d[ 0 ] = (X)newvar;
	newvar->d[ 1 ] = word_to_fixnum(variable_counter++);
	newvar->d[ 2 ] = word_to_fixnum(clock_ticks++);
	freeze_term_var_table[ freeze_term_var_counter++ ] = (X)newvar;
	return (X)newvar;
      }

      return x;
    }
  }

  if(is_FIXNUM(val) || val == END_OF_LIST_VAL || is_byteblock(val) || is_SYMBOL(val) || !IS_IN_HEAP(val)) 
    return val;

  X *tp = lookup_circular_term(val);

  if(*tp == val) return tp[ 1 ];

  *tp = val;
  WORD s = objsize(val);

  if(alloc_top + s + 1 > fromspace_limit) {
    *failed = 1;
    return val;
  }

  ALLOCATE_BLOCK(BLOCK *p, objtype(val), s);
  tp[ 1 ] = (X)p;
  --limit;
  int i = 0;

  if(is_specialblock(val)) {
    p->d[ 0 ] = slot_ref(val, 0);
    i = 1;
  }

  while(i < s) {
    p->d[ i ] = deref_recursive(slot_ref(val, i), limit, failed);

    if(*failed) return val;

    ++i;
  }

  return (X)p;
}


static X deref_all(X val, int limit, int *failed)
{
  freeze_term_var_counter = 0;
  circular_term_counter = 0;
  X y = deref_recursive(val, limit, failed);
  memset(freeze_term_var_table, 0, freeze_term_var_counter * sizeof(X));
  memset(circular_term_table, 0, circular_term_counter * sizeof(X));
  return y;
}


/// Databases

// evict into malloc'd memory, replacing variables with new ones with indexes from 0 to N
static X freeze_term_recursive(X x)
{
  x = deref(x);

  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return x;

  if(is_VAR(x)) {
    X y = find_frozen_variable(x);

    if(y == NULL) {
      ensure_freeze_term_var_table_size(freeze_term_var_counter);
      freeze_term_var_table[ freeze_term_var_counter++ ] = x;
      BLOCK *newvar = (BLOCK *)malloc(sizeof(WORD) * 4);
      ASSERT(newvar, "out of memory - can mot freeze term");
      newvar->h = VAR_TAG | 3;
      newvar->d[ 0 ] = (X)newvar;
      newvar->d[ 1 ] = word_to_fixnum(variable_counter++);
      newvar->d[ 2 ] = word_to_fixnum(0);
      freeze_term_var_table[ freeze_term_var_counter++ ] = (X)newvar;
      return (X)newvar;
    }

    x = y;
  }

  X *tp = lookup_circular_term(x);

  if(*tp == x) return tp[ 1 ];

  *tp = x;

  if(is_byteblock(x)) {
    WORD size = objsize(x);
    BYTEBLOCK *b = (BYTEBLOCK *)malloc(sizeof(WORD) + size);
    ASSERT(b, "out of memory - can not allocate byteblock");
    tp[ 1 ] = (X)b;
    b->h = objbits(x);
    memcpy(b->d, objdata(x), size);
    return (X)b;
  }

  if(is_SYMBOL(x)) {
    // just freeze string - it will be interned when thawed
    X y = freeze_term_recursive(slot_ref(x, 0));
    tp[ 1 ] = y;
    return y;
  }

  WORD size = objsize(x);
  WORD i = 0;
  BLOCK *b = (BLOCK *)malloc(sizeof(WORD) * (size + 1));
  ASSERT(b, "out of memory - can not allocate block");
  tp[ 1 ] = (X)b;
  b->h = objbits(x);

  if(is_specialblock(x)) {
    ++i;
    b->d[ 0 ] = slot_ref(x, 0);
  }

  while(i < size) {
    b->d[ i ] = freeze_term_recursive(slot_ref(x, i));
    ++i;
  }

  return (X)b;
}


static X freeze_term(X x)
{
  circular_term_counter = 0;
  freeze_term_var_counter = 0;
  X y = freeze_term_recursive(x);
  memset(freeze_term_var_table, 0, freeze_term_var_counter * sizeof(X));
  memset(circular_term_table, 0, circular_term_counter * sizeof(X));
  return y;
}


// copy object back into GC'd memory - this will return 0, if heap-space is insufficient
// note: also re-interns symbols (atoms)
// another note: previously stored compound items will not be identical, with the exceptions of atoms
static int thaw_term_recursive(X *xp)
{
  X x = *xp;

  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return 1;

  if(is_VAR(x)) {
    WORD index = fixnum_to_word(slot_ref(x, 1));
    ensure_freeze_term_var_table_size(index * 2);

    if(freeze_term_var_table[ index * 2 ] != NULL) {
      *xp = freeze_term_var_table[ index * 2 ];
      return 1;
    }

    if(alloc_top + objsize(x) + 1 > fromspace_limit) return 0;

    *xp = make_var();
    freeze_term_var_table[ index * 2 ] = *xp;
    return 1;
  }

  X *tp = lookup_circular_term(x);

  if(*tp == x) {
    *xp = tp[ 1 ];
    return 1;
  }

  if(is_byteblock(x)) {
    WORD size = objsize(x);

    if(alloc_top + bytes_to_words(size + 1) > fromspace_limit)
      return 0;

    ALLOCATE_BYTEBLOCK(BYTEBLOCK *b, objtype(x), size);
    memcpy(b->d, objdata(x), size);

    if(is_STRING(x)) *xp = intern((X)b);
    else *xp = (X)b;

    tp[ 1 ] = *xp;
    return 1;
  }

  *tp = x;
  WORD i = 0;
  WORD size = objsize(x);

  if(alloc_top + size + 1 > fromspace_limit) return 0;

  ALLOCATE_BLOCK(BLOCK *b, objtype(x), size);
  tp[ 1 ] = (X)b;
  
  // initialize, in case thawing elements fails
  for(i = 0; i < size; ++i)
    b->d[ i ] = ZERO;
  
  if(is_specialblock(x)) {
    i = 1;
    b->d[ 0 ] = slot_ref(x, 0);
  }
  else i = 0;

  while(i < size) {
    b->d[ i ] = slot_ref(x, i);
    
    if(!thaw_term_recursive(&(b->d[ i ]))) {
      b->d[ i ] = ZERO;
      return 0;
    }

    ++i;
  }

  *xp = (X)b;
  return 1;
}


static X thaw_term(X x, int *failed)
{
  X y = x;
  freeze_term_var_counter = 0;
  circular_term_counter = 0;
  *failed = !thaw_term_recursive(&y);
  memset(freeze_term_var_table, 0, freeze_term_var_counter * sizeof(X));
  memset(circular_term_table, 0, circular_term_counter * sizeof(X));
  return y;
}


// delete frozen term, recursively
static void delete_term(X x)
{
  if(is_FIXNUM(x) || x == END_OF_LIST_VAL) return;

  if(is_VAR(x)) {
    WORD index = fixnum_to_word(slot_ref(x, 1));
    ensure_freeze_term_var_table_size(index);

    if(freeze_term_var_table[ index ] != NULL) 
      return;			/*  already deleted */

    free(x);
    freeze_term_var_table[ index ] = (X)1;
    return;
  }

  X *tp = lookup_circular_term(x);

  if(*tp == x) return;

  *tp = x;
  tp[ 1 ] = x;			/* just mark as already deleted */

  if(is_byteblock(x)) {
    free(x);
    return;
  }

  WORD i = 0;
  WORD size = objsize(x);

  if(is_specialblock(x)) i = 1;

  while(i < size) {
    delete_term(slot_ref(x, i));
    ++i;
  }

  free(x);
}


static DB *create_db(char *name, int namelen, WORD tablesize)
{
  DB *db = (DB *)malloc(sizeof(DB));
  ASSERT(db, "out of memory - can not create database");
  db->name = strndup(name, namelen);
  db->tablesize = tablesize;
  db->table = (DB_BUCKET **)malloc(sizeof(DB_BUCKET *) * tablesize);
  ASSERT(db->table, "out of memory - can not allocate database table");

  for(WORD i = 0; i < tablesize; ++i)
    db->table[ i ] = NULL;

  return db;
}


static DB_ITEM *db_insert_item(DB *db, char *key, int keylen, X val, int atend)
{
  WORD hash = hash_name(key, keylen) % db->tablesize;
  DB_ITEM *item = (DB_ITEM *)malloc(sizeof(DB_ITEM));
  ASSERT(item, "out of memory - can not allocate db-item");
  item->val = freeze_term(val);
  item->erased = 0;
  item->next_deleted = NULL;

  for(DB_BUCKET *bucket = db->table[ hash ]; bucket != NULL; bucket = bucket->next) {
    if(bucket->keylen == keylen && !strncmp(key, bucket->key, keylen)) {
      item->bucket = bucket;

      if(atend) {
	item->previous = bucket->lastitem;
	item->next = NULL;
	bucket->lastitem->next = item;
	bucket->lastitem = item;
      }
      else {
	item->previous = NULL;
	item->next = bucket->firstitem;
	bucket->firstitem->previous = item;
	bucket->firstitem = item;
      }

      return item;
    }
  }

  DB_BUCKET *bucket = (DB_BUCKET *)malloc(sizeof(DB_BUCKET));
  ASSERT(bucket, "out of memory - can not allocate db-bucket");
  bucket->db = db;
  bucket->index = hash;
  bucket->key = strndup(key, keylen);
  bucket->keylen = keylen;
  bucket->firstitem = item;
  item->next = NULL;
  item->previous = NULL;
  item->bucket = bucket;
  bucket->lastitem = item;
  bucket->previous = NULL;
  bucket->next = db->table[ hash ];
  db->table[ hash ] = bucket;
  return item;
}


static DB_ITEM *db_find_first_item(DB *db, char *key, int keylen)
{
  WORD hash = hash_name(key, keylen) % db->tablesize;

  for(DB_BUCKET *bucket = db->table[ hash ]; bucket != NULL; bucket = bucket->next) {
    if(bucket->keylen == keylen && !strncmp(key, bucket->key, keylen)) {
      DB_ITEM *item = bucket->firstitem; 

      while(item && item->erased) item = item->next;

      return item;
    }
  }

  return NULL;
}


static void db_mark_item_as_erased(DB_ITEM *item)
{
  item->erased = 1;
  item->next_deleted = deleted_db_items;
  deleted_db_items = item;
}


static void db_mark_bucket_as_erased(DB_ITEM *item)
{
  DB_BUCKET *bucket = item->bucket;
  item = bucket->firstitem;	/* any item will do */

  // mark all items as erased
  while(item != NULL) {
    db_mark_item_as_erased(item);
    item = item->next;
  }
}


static void db_erase_item(DB_ITEM *item)
{
  DB_BUCKET *bucket = item->bucket;
  delete_term(item->val);

  if(bucket->firstitem == item) {
    // erase bucket, if this is the last item in it
    if(bucket->lastitem == item) {
      if(bucket->previous) bucket->previous->next = bucket->next;
    
      if(bucket->next) bucket->next->previous = bucket->previous;

      bucket->db->table[ bucket->index ] = NULL;
      bucket->previous = bucket->next = NULL;
      free(bucket->key);
      free(bucket);
    }
    else {
      // first item in bucket
      if(item->next) item->next->previous = NULL; 

      bucket->firstitem = item->next;
    }
  }
  else {
    // otherwise remove from item-chain
    item->previous->next = item->next;
  
    if(item->next) item->next->previous = item->previous;
  }

  item->next = item->previous = NULL;
  item->bucket = NULL;
  free(item);
}


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

  WORD t = TAG_TO_TYPE(h);

#ifndef SIXTYFOUR
  if((t == FLONUM_TYPE || t == PACKEDFLOATVECTOR_TYPE) && !FPALIGNED(tospace_top))
    *(tospace_top++) = ALIGNMENT_HOLE_MARKER;
#endif

  if(t == DBREFERENCE_TYPE && slot_ref(*addr, 1) == ONE) {
    // increase refcount if DB-reference, to detect unreferenced ones that can be completely deleted
    DB_ITEM *item = (DB_ITEM *)slot_ref(*addr, 0);
    ++item->refcount;
  }

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


static void collect_garbage(CHOICE_POINT *C)
{
  DRIBBLE("[GC ... ");							
  tospace_top = tospace; 
  scan_ptr = tospace_top;				

  // clear ref-counts of all deleted DB items
  for(DB_ITEM *item = deleted_db_items; item != NULL; item = item->next_deleted)
    item->refcount = 0;

  // mark local environments
  for(X *p = environment_stack; p < env_top; ++p)
    mark1(p);

  // mark argument stack
  for(X *p = argument_stack; p < arg_top; ++p)
    mark1(p);

  // mark global variables
  for(int i = 0; i < global_variable_counter; ++i)
    mark1(&(global_variables[ i ]));

  // mark special symbols
  mark1(&dot_atom);
  mark1(&system_error_atom);

  //XXX preliminary - later don't mark and remove unforwarded items
  //    (adjusting trail-pointers in CP-stack accordingly)
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
    X sym = symbol_table[ i ];

    while(sym != END_OF_LIST_VAL) {
      if(!IS_IN_HEAP(sym)) {
	if(prevsym != END_OF_LIST_VAL)
	  SLOT_SET(prevsym, 1, sym);

	break;	// all further symbols in this chain will be static
      }

      if(is_forwarded(sym)) {
	sym = fptr_to_ptr(objbits(sym));
	SLOT_SET(sym, 2, prevsym);
	X next = slot_ref(sym, 1);
	SLOT_SET(sym, 1, END_OF_LIST_VAL);

	if(prevsym != END_OF_LIST_VAL) 
	  SLOT_SET(prevsym, 1, sym);
	else
	  symbol_table[ i ] = sym;

	prevsym = sym;
	sym = next;
      }
      else {
	// DRIBBLE("reclaimed symbol: \'%s\'\n", (CHAR *)objdata(slot_ref(sym, 0)));
	++gcdsyms;
	sym = slot_ref(sym, 1);
      }
    }
  }

  if(gcdsyms > 0)
    DRIBBLE("%d symbol(s) reclaimed ... ", gcdsyms);

  void *tmp = fromspace; 
  fromspace = tospace; 
  tospace = tmp;		
  tmp = fromspace_end; 
  fromspace_end = tospace_end;
  tospace_end = tmp;	
  fromspace_limit = (X)((char *)fromspace_end - heap_reserve);	
  alloc_top = tospace_top;
  DRIBBLE("finished (" WORD_OUTPUT_FORMAT " bytes in use, T: " WORD_OUTPUT_FORMAT
	  ", C: " WORD_OUTPUT_FORMAT ", A: " WORD_OUTPUT_FORMAT ", E: " WORD_OUTPUT_FORMAT ")]\n",
	  ((WORD)((char *)tospace_top - (char *)fromspace)),
	  (WORD)trail_top - (WORD)trail_stack,
	  (WORD)C - (WORD)choice_point_stack,
	  (WORD)arg_top - (WORD)argument_stack,
	  (WORD)env_top - (WORD)environment_stack);
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

  // really erase DB items that are not referenced
  DB_ITEM *previtem = NULL;
  int deleted = 0;
  DB_ITEM *item = deleted_db_items; 

  while(item != NULL) {
    DB_ITEM *next = item->next_deleted;

    if(item->refcount == 0) {
      if(previtem) 
	previtem->next_deleted = next;
      else
	deleted_db_items = next;

      db_erase_item(item);
      ++deleted;
    }
    else previtem = item;

    item = next;
  }

  if(deleted > 0)
    DRIBBLE("[%d db-items deleted]\n", deleted);
}


static void set_finalizer(X x, void (*fn)(X))
{
  FINALIZER *fp = malloc(sizeof(FINALIZER));
  fp->next = active_finalizers;
  fp->finalizer = fn;
  fp->object = x;
  active_finalizers = fp;
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
  environment_stack_size = ENVIRONMENT_STACK_SIZE;
  ifthen_stack_size = IFTHEN_STACK_SIZE;
  choice_point_stack_size = CHOICE_POINT_STACK_SIZE;
  trail_stack_size = TRAIL_STACK_SIZE;
  argument_stack_size = ARGUMENT_STACK_SIZE;
  global_argc = argc;
  global_argv = argv;

  // scan argv for runtime-parameters
  for(int i = argc - 1; i > 0; --i) {
    char *arg = argv[ i ];

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

      case 'd':
	debugging = 1;
	break;

      case 'A':
	argument_stack_size = numeric_arg(arg + 3);
	break;

      case 'E':
	environment_stack_size = numeric_arg(arg + 3);
	break;

      case 'C':
	choice_point_stack_size = numeric_arg(arg + 3);
	break;

      case 'T':
	trail_stack_size = numeric_arg(arg + 3);
	break;

	// no option for ifthen-stack, in the moment

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
  environment_stack = (X *)malloc(environment_stack_size);
  ASSERT(environment_stack, "out of memory - can not allocate environment stack");
  trail_stack = (X *)malloc(trail_stack_size);
  ASSERT(trail_stack, "out of memory - can not allocate environment stack");
  ifthen_stack = (void **)malloc(ifthen_stack_size);
  ASSERT(ifthen_stack, "out of memory - can not allocate if-then stack");
  choice_point_stack = (CHOICE_POINT *)malloc(choice_point_stack_size);
  ASSERT(choice_point_stack, "out of memory - can not allocate choice-point stack");
  argument_stack = (X *)malloc(argument_stack_size);
  ASSERT(argument_stack, "out of memory - can not allocate argument stack");
  trail_top = trail_stack;
  ifthen_top = ifthen_stack;
  env_top = environment_stack;
  arg_top = argument_stack;
  memset(circular_term_table, 0, CIRCULAR_TERM_TABLE_SIZE * 2 * sizeof(X));
  string_buffer = malloc(string_buffer_length = STRING_BUFFER_SIZE);
  ASSERT(argument_stack, "out of memory - can not allocate string buffer");
  freeze_term_var_table_size = INITIAL_FREEZE_TERM_VAR_TABLE_SIZE * 2;
  freeze_term_var_table = (X *)malloc(freeze_term_var_table_size * sizeof(X));
  ASSERT(freeze_term_var_table, "out of memory - can not allocate freeze-term variable table");

  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i)
    symbol_table[ i ] = END_OF_LIST_VAL;

  for(int i = 0; i < MAX_GLOBAL_VARIABLES; ++i)
    global_variables[ i ] = ZERO;
}


static void terminate(CHOICE_POINT *C, int code)
{
  DRIBBLE("[terminating - T: " WORD_OUTPUT_FORMAT ", C: " WORD_OUTPUT_FORMAT
	  ", A: " WORD_OUTPUT_FORMAT ", E: " WORD_OUTPUT_FORMAT "]\n",
	  (WORD)trail_top - (WORD)trail_stack,
	  (WORD)C - (WORD)choice_point_stack,
	  (WORD)arg_top - (WORD)argument_stack,
	  (WORD)env_top - (WORD)environment_stack);
  exit(code);
}


// debugging and tracing support

static void write_hook(X x)
{
  basic_write_term(stderr, 1, TRACE_DEBUG_WRITE_LIMIT, 1, deref(x));
  fputc('\n', stderr);
}


static void dump_symbol_table()
{
  for(int i = 0; i < SYMBOL_TABLE_SIZE; ++i) {
    int f = 0;

    for(X sym = symbol_table[ i ]; sym != END_OF_LIST_VAL; sym = slot_ref(sym, 1)) {
      X name2 = slot_ref(sym, 0);

      if(!f) {
	fprintf(stderr, "\n%d: ", i);
	f = 1;
      }

      fprintf(stderr, "'%s' ", (CHAR *)objdata(name2));
    }
  }

  fputc('\n', stderr);
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
# define TRACE_ENTER(name, arity)  { if(debugging) trace_write("CALL", name, arity, C0->A, C); }
# define TRACE_REDO(name, arity)   { if(debugging) trace_write("REDO", name, arity, C0->A, C); }
# define TRACE_EXIT(name, arity)   { if(debugging) trace_write("EXIT", name, arity, C0->A, C); }
# define TRACE_FAIL(name, arity)   { if(debugging && C0->P == NULL) trace_write("FAIL", name, arity, C0->A, C); }
# define TRACE_TAIL_CALL(name, arity)  { if(debugging) trace_write("TAIL", name, arity, C0->A, C); }
#else
# define TRACE_ENTER(name, arity)
# define TRACE_REDO(name, arity)
# define TRACE_EXIT(name, arity)
# define TRACE_FAIL(name, arity)
# define TRACE_TAIL_CALL(name, arity)
#endif


/// unification

#define unify(x, y)   ({ X _x = (x), _y = (y); _x == _y || unify1(C0, _x, _y); })


static int unify1(CHOICE_POINT *C0, X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(is_FIXNUM(x) && is_FIXNUM(y)) 
    return x == y;

  WORD xt = is_FIXNUM(x) ? FIXNUM_TYPE : objtype(x);
  WORD yt = is_FIXNUM(y) ? FIXNUM_TYPE : objtype(y);

  if(xt == VAR_TYPE) {
#ifdef DEBUGGING
    if(verbose) {
      DRIBBLE("[binding _" WORD_OUTPUT_FORMAT " <- ", fixnum_to_word(slot_ref(x, 1)));
      basic_write_term(stderr, 1, 9999, 1, y);
      fputs("]\n", stderr);
    }
#endif

    SLOT_SET(x, 0, y);
    push_trail(C0, x);
    return 1;
  }

  if(yt == VAR_TYPE) {
#ifdef DEBUGGING
    if(verbose) {
      DRIBBLE("[binding _" WORD_OUTPUT_FORMAT " <- ", fixnum_to_word(slot_ref(y, 1)));
      basic_write_term(stderr, 1, 9999, 1, x);
      fputs("]\n", stderr);
    }
#endif

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
    if(!unify(slot_ref(x, i), slot_ref(y, i)))
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

  if(arity == 2 && functor == dot_atom) {
    X car = va_arg(va, X);
    return PAIR(car, va_arg(va, X));
  }

  X s = STRUCTURE(functor, arity);

  for(int i = 1; i <= arity; ++i)
    SLOT_SET(s, i, va_arg(va, X));

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
	return fixnum_to_word(x) op fixnum_to_word(y);	\
      if(is_FLONUM(y))					\
	return fixnum_to_float(x) op flonum_to_float(y);	\
      check_number_failed(y); }					\
    if(is_FLONUM(x)) {						\
      if(is_FIXNUM(y))						\
	return flonum_to_float(x) op fixnum_to_float(y);	\
      if(is_FLONUM(y))						\
	return flonum_to_float(x) op flonum_to_float(y);	\
      check_number_failed(y); }					\
    check_number_failed(x);					\
    return 0; }

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
    check_number_failed(x);						\
    return 0; }

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
INTEGER_BINARY_OP(num_xor, ^)
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
  return x;			/* never executed */
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
  return x;			/* never executed */
}


static inline X num_float(X x)
{
  if(is_FIXNUM(x)) return FLONUM(fixnum_to_word(x));

  check_type_FLONUM(x);
  return x;
}


static inline X num_frac(X x)
{
  FLOAT i;
  return FLONUM(modf(flonum_to_float(check_type_FLONUM(deref(x))), &i));
}


static inline X num_int(X x)
{
  FLOAT i;
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

  if(is_FIXNUM(x))
    return fixnum_to_word(x) < 0 ? word_to_fixnum(-fixnum_to_word(x)) : x;

  if(is_FLONUM(x)) {
    FLOAT n = flonum_to_float(x);

    if(n < 0) return FLONUM(-n);
    
    return x;
  }

  check_number_failed(x);
  return x;			/* never executed */
}


static inline X num_sign(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x)) {
    if(fixnum_to_word(x) < 0)
      return word_to_fixnum(-1);

    if(x == ZERO) 
      return word_to_fixnum(0);
      
    return word_to_fixnum(1);
  }

  if(is_FLONUM(x)) {
    FLOAT n = flonum_to_float(x);

    if(n < 0.0) return FLONUM(-1);
    
    if(n > 0.0) return FLONUM(1);

    return FLONUM(0);
  }

  check_number_failed(x);
  return x;			/* never executed */
}


static inline X num_negate(X x)
{ 
  x = deref(x);

  if(is_FIXNUM(x)) 
    return word_to_fixnum(-fixnum_to_word(x));

  if(is_FLONUM(x))
    return FLONUM(-flonum_to_float(x));

  check_number_failed(x);
  return x;			/* never executed */
}


static inline X num_not(X x) 
{ 
  return word_to_fixnum(~fixnum_to_word(check_fixnum(deref(x))));
}


static inline X num_random(X x)
{
  return word_to_fixnum(rand() % fixnum_to_word(check_fixnum(deref(x))));
}


/// Term comparison

static int is_recursively_identical(X x, X y)
{
  x = deref(x);
  y = deref(y);

  if(x == y) return 1;

  if(is_FIXNUM(x) || is_FIXNUM(y)) return 0;

  WORD t = objtype(x);

  if(t != objtype(y)) return 0;

  if(t == SYMBOL_TYPE) return 0;

  if(t == VAR_TYPE) 
    return slot_ref(x, 1) == slot_ref(y, 1);

  WORD s = objsize(x);

  if(s != objsize(y)) return 0;

  if(is_byteblock(x))
    return !memcmp(objdata(x), objdata(y), s);

  WORD i = 0;

  if(is_specialblock(x)) {
    if(slot_ref(x, 0) != slot_ref(y, 0)) return 0;

    i = 1;
  }

  while(i < s) {
    X x2 = slot_ref(x, i);
    X y2 = slot_ref(y, i);

    if(x2 != y2 && !is_recursively_identical(x2, y2)) 
      return 0;

    ++i;
  }

  return 1;
}


static inline int is_identical(X x, X y)
{
  if(x == y) return 1;
  
  return is_recursively_identical(x, y);
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
    return (WORD)slot_ref(y, 1) - (WORD)slot_ref(x, 1);

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

      d = compare_terms(slot_ref(x, 0), dot_atom);

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

      d = compare_terms(dot_atom, slot_ref(y, 0));

      if(d != 0) return d;

      d = compare_terms(slot_ref(x, 0), slot_ref(x, 1));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 1), slot_ref(y, 2));

    case PAIR_TYPE:
      d = compare_terms(slot_ref(x, 0), slot_ref(y, 0));

      if(d != 0) return d;

      return compare_terms(slot_ref(x, 1), slot_ref(y, 1));

    }

  case DBREFERENCE_TYPE:
    return (WORD)slot_ref(y, 0) - (WORD)slot_ref(x, 0);
  }

  CRASH("unable to compare terms");
}


// assumes x is deref'd, returns pointer to char that should not be modified
static CHAR *to_string(X x, int *size)
{
  if(is_FIXNUM(x))
    CRASH("bad argument type - can not convert to string");

  switch(objtype(x)) {
  case SYMBOL_TYPE:
    { X str = slot_ref(x, 0);
      *size = string_length(str);
      return (CHAR *)objdata(str); }

  case PAIR_TYPE:
    { int len = 0;
      CHAR *ptr = string_buffer;
      
      while(!is_FIXNUM(x) && objtype(x) == PAIR_TYPE) {
	if(len >= string_buffer_length - 1) {
	  string_buffer = realloc(string_buffer, string_buffer_length *= 2);
	  ASSERT(string_buffer, 
		 "out of memory - can not increase size of string-buffer to " WORD_OUTPUT_FORMAT, 
		 (WORD)string_buffer_length);
	}

	X c = deref(slot_ref(x, 0));
	check_fixnum(c);
	*(ptr++) = fixnum_to_word(c);
	++len;
	x = deref(slot_ref(x, 1));
      }

      if(x != END_OF_LIST_VAL)
	CRASH("bad argument type - not a proper list");

      *ptr = '\0';
      *size = len;
      return string_buffer; }

  default:
    CRASH("bad argument type - can not convert to string");
    return NULL;
  }
}


// does not check for full heap, so make sure the string isn't too long
static X string_to_list(CHAR *str, int len)
{
  X lst = END_OF_LIST_VAL;
  CHAR *ptr = str + len;

  while(len > 0)
    lst = PAIR(word_to_fixnum(ptr[ --len ]), lst);

  return lst;
}


/// VM operations

#define CURRENT_NAME
#define CURRENT_ARITY
#define CURRENT_ENVIRONMENT_SIZE

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
    C->catch_top = catch_top;					\
    C->C0 = C0;							\
    C->P = lbl;							\
    C0 = C++;							\
    ASSERT((WORD)C < (WORD)choice_point_stack + choice_point_stack_size, "choice-point stack overflow"); }

#define COPY_CHOICE_POINT(lbl)						\
  { C->T = trail_top;							\
    C->R = C0->R;							\
    C->E = E;								\
    C->A = C0->A;							\
    C->timestamp = clock_ticks++;					\
    C->arg_top = arg_top;						\
    C->env_top = env_top;						\
    C->catch_top = catch_top;						\
    C->C0 = C0;								\
    C->P = lbl;								\
    ++C;								\
    ASSERT((WORD)C < (WORD)choice_point_stack + choice_point_stack_size, "choice-point stack overflow"); }

#define ADJUST_CHOICE_POINT(lbl)						\
  { C0->T = trail_top;							\
    C0->arg_top = arg_top;						\
    C0->env_top = env_top;						\
    C0->P = lbl; }

#define SAVE_CHOICE_POINTS						\
  { *(ifthen_top++) = arg_top;						\
    *(ifthen_top++) = env_top;						\
    *(ifthen_top++) = E;						\
    *(ifthen_top++) = C0->P;						\
    *(ifthen_top++) = C0;						\
    *(ifthen_top++) = C;						\
    ASSERT((WORD)ifthen_top < (WORD)ifthen_stack + ifthen_stack_size, "if-then stack overflow"); }

#define RESTORE_CHOICE_POINTS	 \
  { C = *(--ifthen_top);	 \
    C0 = *(--ifthen_top);	 \
    C0->P = *(--ifthen_top);	 \
    E = *(--ifthen_top);	 \
    env_top = *(--ifthen_top);	 \
    arg_top = *(--ifthen_top);						\
    ASSERT(ifthen_top >= ifthen_stack, "if-then stack underflow"); }

#define INVOKE_CHOICE_POINT			\
  { for(C0 = C - 1; C0->P == NULL; --C0) {	\
    E = C0->E;					\
    env_top = C0->env_top; }			\
    C = C0 + 1;					\
    unwind_trail(C0->T);			\
    A = C0->A;					\
    arg_top = C0->arg_top;			\
    catch_top = C0->catch_top;			\
    goto *(C0->P); }

#define POP_CHOICE_POINT  \
  { E = C0->E;		  \
    C0 = C0->C0; }
  
#define EXIT				     \
  { TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    R = C0->R;				     \
    E = C0->E;				     \
    C0 = C0->C0;			     \
    goto *R; }

#define DETERMINATE_EXIT		     \
  { TRACE_EXIT(CURRENT_NAME, CURRENT_ARITY); \
    R = C0->R;				     \
    E = C0->E;				     \
    env_top = C0->env_top;		     \
    arg_top = C0->arg_top - CURRENT_ARITY;   \
    C = C0;				     \
    C0 = C0->C0;			     \
    goto *R; }

#define CALL(lbl, ret)   { R = ret; goto lbl; }

#define FAIL     { TRACE_FAIL(CURRENT_NAME, CURRENT_ARITY); goto fail; }
#define REDO     TRACE_REDO(CURRENT_NAME, CURRENT_ARITY)

#define POP_ARGUMENTS   arg_top = C0->arg_top - CURRENT_ARITY

#define TAIL_CALL(lbl)				 \
  { TRACE_TAIL_CALL(CURRENT_NAME, CURRENT_ARITY); \
    R = C0->R;						 \
    E = C0->E;						 \
    env_top = C0->env_top;				 \
    C = C0;						 \
    C0 = C0->C0;					 \
    goto lbl; }

#define CHECK_LIMIT				\
  { if(alloc_top > fromspace_limit)			\
      collect_garbage(C);				\
    ASSERT((char *)arg_top < (char *)argument_stack + argument_stack_size, "argument-stack overflow"); } 

#define ENVIRONMENT(len)  \
  { E = env_top;	  \
    for(int _i = 0; _i < (len); ++_i) *(env_top++) = ZERO; }

#define SET_REDO(lbl)   C0->P = (lbl)

#define CUT								\
  { C = C0 + 1;								\
    arg_top = C0->arg_top;						\
    env_top = E + CURRENT_ENVIRONMENT_SIZE;				\
    SET_REDO(NULL); }

#define PUSH_CATCHER(lbl)			\
  { catch_top->C0 = C0;				\
    catch_top->E = E;				\
    catch_top->T = trail_top;			\
    catch_top->ifthen_top = ifthen_top;		\
    catch_top->env_top = env_top;		\
    catch_top->arg_top = arg_top;		\
    catch_top->P = lbl;				\
    catch_top->ball = ZERO;			\
    ++catch_top; }

#define POP_CATCHER  \
  { ASSERT(catch_top > catch_stack, "catch-stack underflow"); \
    --catch_top; }

#define RETHROW       throw_exception(catch_top->ball)


/// Boilerplate code

#define BOILERPLATE					\
  X *A = NULL;						\
  CHOICE_POINT *C, *C0;					\
  void *R = &&success_exit;				\
  void *R0;						\
  initialize(argc, argv);				\
  intern_static_symbols(PREVIOUS_SYMBOL);		\
  X *E = env_top;					\
  C0 = C = choice_point_stack;				\
  C->T = trail_top;					\
  C->timestamp = clock_ticks++;				\
  C->R = NULL;						\
  C->C0 = NULL;						\
  C->P = &&fail_exit;					\
  C++;							\
  if(setjmp(exception_handler)) {			\
    unwind_trail(catch_top->T);				\
    C0 = catch_top->C0;					\
    C = C0 + 1;						\
    A = arg_top = catch_top->arg_top;			\
    env_top = catch_top->env_top;			\
    ifthen_top = catch_top->ifthen_top;			\
    E = catch_top->E;					\
    goto *(catch_top->P); }				\
  goto INIT_GOAL;					\
fail: INVOKE_CHOICE_POINT;				\
fail_exit: fprintf(stderr, "no.\n"); terminate(C, EXIT_FAILURE);	\
success_exit:								\
 ASSERT(ifthen_stack == ifthen_top, "unbalanced if-then stack");	\
 ASSERT(catch_stack == catch_top, "unbalanced catcher stack");	\
 terminate(C, EXIT_SUCCESS);


////////////////////////////////////////////////////////////////////////////////

/// PRIMITIVES (all expect their arguments to be deref'd)


#define PRIMITIVE(name, ...)	   static int name(CHOICE_POINT *C0, __VA_ARGS__)


static int debug_hook(CHOICE_POINT *C0) { return 1; }

PRIMITIVE(put_byte, X c) 
{
  int code;

  if(is_FIXNUM(c)) code = fixnum_to_word(c);
  else if(is_SYMBOL(c)) code = *((char *)objdata(slot_ref(c, 0)));
  else CRASH("bad argument type - not a valid character");
  
  fputc(code, port_file(standard_output_port)); 
  return 1;
}

PRIMITIVE(put_string, X str)
{
  int len;
  CHAR *ptr = to_string(str, &len);
  fputs(ptr, port_file(standard_output_port));
}

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

// has no args, avoid ugly dummy parameter
static int gc(CHOICE_POINT *C0) { alloc_top = fromspace_limit + 1; return 1; }

PRIMITIVE(halt, X code) 
{ 
  check_fixnum(code);
  terminate(C0, fixnum_to_word(code));
  return 1;			/* never executed */
}

PRIMITIVE(command_line_arguments, X var)
{
  X lst = END_OF_LIST_VAL;

  // build argument-list for "command-line-arguments"
  for(int i = global_argc - 1; i > 0; --i) {
    // ignore runtime options
    if(global_argv[ i ][ 1 ] != ':') {
      X str = CSTRING(global_argv[ i ]);
      X sym = intern(str);
      X pr = PAIR(sym, lst);
      lst = pr;
    }
  }

  return unify(lst, var);
}

PRIMITIVE(db_create, X name, X size, X result) 
{
  check_type_SYMBOL(name);
  check_fixnum(size);
  X str = slot_ref(name, 0);
  DB *db = create_db((CHAR *)objdata(str), string_length(str), fixnum_to_word(size));
  // this is a fake db-reference, not usable for lookup
  ALLOCATE_BLOCK(BLOCK *dbr, DBREFERENCE_TYPE, 2);
  dbr->d[ 0 ] = (X)db;
  dbr->d[ 1 ] = ZERO;	/* marks dbref as ptr to db (not item) */
  return unify(result, (X)dbr);
}

PRIMITIVE(db_find, X dbr, X key, X ref)
{
  check_type_DBREFERENCE(dbr);
  check_type_SYMBOL(key);
  X str = slot_ref(key, 0);
  DB *db = (DB *)slot_ref(dbr, 0);
  DB_ITEM *item = db_find_first_item(db, (CHAR *)objdata(str), string_length(str));

  if(item) {
    ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
    b->d[ 0 ] = (X)item;
    b->d[ 1 ] = ONE;
    return unify(ref, (X)b);
  }

  return 0;
}

PRIMITIVE(db_next, X ref, X result)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = ((DB_ITEM *)slot_ref(ref, 0))->next;

  while(item && item->erased) item = item->next;

  if(item != NULL) {
    ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
    b->d[ 0 ] = (X)item;
    b->d[ 1 ] = ONE;
    return unify(result, (X)b);
  }

  return 0;
}

PRIMITIVE(db_ref, X ref, X result)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = (DB_ITEM *)slot_ref(ref, 0);
  int failed;
  ASSERT(slot_ref(ref, 1) != NULL, "attempting to reference database pointer");
  ASSERT(!item->erased, "attempting to reference erased database item");
  X x = thaw_term(item->val, &failed);
  return !failed && unify(result, x);
}

PRIMITIVE(db_erase, X ref)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = (DB_ITEM *)slot_ref(ref, 0);
  db_mark_item_as_erased(item);
  return 1;
}

PRIMITIVE(db_erase_all, X ref)
{
  check_type_DBREFERENCE(ref);
  DB_ITEM *item = (DB_ITEM *)slot_ref(ref, 0);
  db_mark_bucket_as_erased(item);
  return 1;
}

PRIMITIVE(db_record, X dbr, X atend, X key, X val, X result)
{
  check_type_DBREFERENCE(dbr);
  check_type_SYMBOL(key);
  X str = slot_ref(key, 0);
  DB *db = (DB *)slot_ref(dbr, 0);
  DB_ITEM *item = db_insert_item(db, (CHAR *)objdata(str), string_length(str), val, atend != ZERO);
  ALLOCATE_BLOCK(BLOCK *b, DBREFERENCE_TYPE, 2);
  b->d[ 0 ] = (X)item;
  b->d[ 1 ] = ONE;
  return unify(result, (X)b);
}

PRIMITIVE(file_exists, X name) 
{
  struct stat info;
  int len;
  CHAR *fname = to_string(name, &len);
  return !stat(fname, &info) && S_ISREG(info.st_mode);
}

PRIMITIVE(get_byte, X c)
{
  int g = fgetc(port_file(standard_input_port));
  return unify(word_to_fixnum(g), c);
}

PRIMITIVE(peek_byte, X c)
{
  FILE *fp = port_file(standard_input_port);
  int g = fgetc(fp);

  if(g != EOF) ungetc(g, fp);

  return unify(word_to_fixnum(g), c);
}

PRIMITIVE(open_stream, X name, X input, X mode, X result)
{
  int len;
  CHAR *str = to_string(name, &len);
  CHAR *m = to_string(mode, &len);
  FILE *fp = fopen(str, m);

  if(fp == NULL) 
    CRASH("can not open file - %s", strerror(errno));

  X port = PORT(fp, input, ONE, ZERO);
  return unify(port, result);
}

PRIMITIVE(close_stream, X stream)
{
  check_type_PORT(stream);

  if(stream == &default_input_port || stream == &default_output_port || stream == &default_error_port)
    return 1;

  if(slot_ref(stream, 2) != ZERO) {
    fclose(port_file(stream));
    SLOT_SET(stream, 2, ZERO);
  }

  if(stream == standard_input_port) standard_input_port = &default_input_port;
  else if(stream == standard_output_port) standard_output_port = &default_output_port;
  else if(stream == standard_error_port) standard_error_port = &default_error_port;

  return 1;
}

PRIMITIVE(shell_command, X cmd, X status)
{
  int len;
  CHAR *ptr = to_string(cmd, &len);
  int s = system(ptr);
  return unify(word_to_fixnum(s), status);
}

PRIMITIVE(get_environment_variable, X name, X result)
{
  int len;
  CHAR *ptr = to_string(name, &len);
  CHAR *val = getenv(ptr);

  if(!val) return 0;

  int slen = strlen(val);
  X str = STRING(slen);
  memcpy(objdata(str), val, slen);
  return val && unify(intern(str), result);
}

PRIMITIVE(current_input_stream, X stream) { return unify(standard_input_port, stream); }
PRIMITIVE(current_output_stream, X stream) { return unify(standard_output_port, stream); }
PRIMITIVE(current_error_stream, X stream) { return unify(standard_error_port, stream); }

PRIMITIVE(set_current_input_stream, X stream)
{
  if(stream == ZERO)			      /* 'user' */
    standard_input_port = &default_input_port;
  else {
    check_input_port(stream);
    standard_input_port = stream;
  }

  return 1;
}

PRIMITIVE(set_current_output_stream, X stream)
{
  if(stream == ZERO)		/* 'user' */
    standard_output_port = &default_output_port;
  else {
    check_output_port(stream);
    standard_output_port = stream;
  }

  return 1;
}

PRIMITIVE(set_current_error_stream, X stream)
{
  if(stream == ZERO)		/* 'user' */
    standard_error_port = &default_error_port;
  else {
    check_output_port(stream);
    standard_error_port = stream;
  }

  return 1;
}

PRIMITIVE(atom_codes, X atom, X lst)
{
  if(is_VAR(atom)) {
    int len;
    CHAR *ptr = to_string(lst, &len);
    X str = STRING(len);
    memcpy(objdata(str), ptr, len);
    return unify(atom, intern(str));
  }

  int len;
  CHAR *ptr;

  if(atom == END_OF_LIST_VAL) {
    len = 2;
    ptr = "[]";
  }
  else {
    check_type_SYMBOL(atom);
    X str = slot_ref(atom, 0);
    len = string_length(str);
    ptr = (CHAR *)objdata(str);
  }

  X p = END_OF_LIST_VAL;

  while(len)
    p = PAIR(word_to_fixnum(ptr[ --len ]), p);

  return unify(lst, p);
}

PRIMITIVE(number_codes, X num, X lst)
{
  if(is_VAR(num)) {
    int len;
    CHAR *ptr = to_string(lst, &len);
    CHAR *endptr;
    WORD n = strtol(ptr, &endptr, 10);

    if(*endptr != '\0' || ((n == LONG_MIN || n == LONG_MAX) && errno == ERANGE) || !is_in_fixnum_range(n)) {
      FLOAT f = strtod(ptr, &endptr);

      if(*endptr != '\0' || ((f == 0 || f == HUGE_VAL || f == -HUGE_VAL) && errno == ERANGE)) 
	return 0;

      return unify(num, FLONUM(f));
    }

    return unify(num, word_to_fixnum(n));
  }

  check_number(num);

  if(is_FIXNUM(num))
    sprintf(string_buffer, WORD_OUTPUT_FORMAT, fixnum_to_word(num));
  else
    sprintf(string_buffer, FLOAT_OUTPUT_FORMAT, flonum_to_float(num));

  int len = strlen(string_buffer);
  X p = END_OF_LIST_VAL;

  while(len)
    p = PAIR(word_to_fixnum(string_buffer[ --len ]), p);

  return unify(lst, p);
}

PRIMITIVE(functor, X term, X name, X arity)
{
  if(is_VAR(term)) {
    check_fixnum(arity);
    int n = fixnum_to_word(arity);
    X x;

    if(n == 0) {
      check_atomic(name);
      x = name;
    }
    else if(name == dot_atom && n == 2)
      x = PAIR(make_var(), make_var());
    else {
      check_type_SYMBOL(name);
      x = STRUCTURE(name, n);
    
      for(int i = 1; i <= n; ++i) 
	SLOT_SET(x, i, make_var());
    }

    return unify(term, x);
  }

  if(is_PAIR(term))
    return unify(arity, word_to_fixnum(2)) && unify(dot_atom, name);
  
  if(is_atomic(term))
    return unify(arity, word_to_fixnum(0)) && unify(term, name);

  check_type_STRUCTURE(term);
  return unify(word_to_fixnum(objsize(term) - 1), arity) && unify(name, slot_ref(term, 0));
}

PRIMITIVE(term_arg, X index, X term, X arg)
{
  check_fixnum(index);
  WORD i = fixnum_to_word(index);

  if(is_PAIR(term))
    return i > 0 && i < 3 && unify(arg, slot_ref(term, i - 1));

  check_type_STRUCTURE(term);
  return i > 0 && i < objsize(term) && unify(arg, slot_ref(term, i));
}

PRIMITIVE(deref_term, X in, X limit, X out)
{
  check_fixnum(limit);
  int failed;
  X x = deref_all(in, fixnum_to_word(limit), &failed);
  return !failed && unify(x, out);
}

PRIMITIVE(enable_trace, X flag)
{
  check_fixnum(flag);
  debugging = fixnum_to_word(flag);
  return 1;
}

PRIMITIVE(get_process_id, X pid) { return unify(pid, word_to_fixnum(getpid())); }
PRIMITIVE(sleep_for_seconds, X secs) { check_fixnum(secs); sleep(fixnum_to_word(secs)); return 1; }

PRIMITIVE(set_random_seed, X seed) 
{
  check_fixnum(seed);
  srand(fixnum_to_word(seed));
  return 1;
}

static int flush_output(CHOICE_POINT *C0) { fflush(port_file(standard_output_port)); return 1; }

PRIMITIVE(do_throw, X ball) { throw_exception(ball); return 0; }


#endif
#endif
