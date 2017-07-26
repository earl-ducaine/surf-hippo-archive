/*
 * $Id: soar.h,v 1.15 1995/02/06 18:18:33 rempel Exp $
 * $Log: soar.h,v $
 * Revision 1.15  1995/02/06  18:18:33  rempel
 * changed version from 6.2.4 to 6.2.5
 *
 * Revision 1.14  1994/12/06  22:04:01  rempel
 * For 6.2.4b
 *
 * Revision 1.13  1994/11/23  16:42:28  rempel
 * for 6.2.4
 *
 * Revision 1.12  1994/08/23  10:38:48  portelli
 * For 6.2.4
 *
 * Revision 1.11  1994/07/12  15:44:30  portelli
 * For 6.2.3
 *
 * Revision 1.10  94/07/01  15:55:28  portelli
 * For 6.2.2
 * 
 * Revision 1.9  1994/06/08  22:19:59  portelli
 * For 6.2.1
 *
 * Revision 1.8  94/06/01  18:10:07  rempel
 * added new lexeme type DOLLAR_STRING_LEXEME for fixing shell escape
 * 
 * Revision 1.7  1994/05/18  13:33:49  portelli
 * Soar 6.2.0 b
 *
 * Revision 1.6  94/05/13  18:06:49  rempel
 * added alias & directory stack
 * 
 * Revision 1.5  1994/05/10  15:05:11  rempel
 * *** empty log message ***
 *
 * Revision 1.4  1993/11/21  17:23:26  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:48:00  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:24:51  jtraub
 * 6.1_checkin
 *
 * Revision 9.7  1993/06/15  18:03:03  jtraub
 * split interface.c into interface.c and ma_interface.c.  Had to add one
 * function declaration here.
 *
 * Revision 9.6  1993/06/14  20:05:38  jtraub
 * added Match Set printing enhancement, and upped version number to 6.1.0
 *
 * Revision 9.5  1993/05/10  20:26:09  jtraub
 * Added one function prototype that was now needed because of source split.
 *
 * Revision 9.4  1993/05/10  18:36:10  jtraub
 * added RCS header information
 *
 */

/* ======================================================================
                           Soar 6 Include File

   This file gets #included in all Soar 6 source files.  It defines all
   sorts of constants and data structures, and gives prototype declarations
   for various functions.
====================================================================== */

#ifndef _SOAR_H_INCLUDED
#define _SOAR_H_INCLUDED

#ifdef THINK_C
#include "ThinkCPosix.h"
#define getwd(pathname) getcwd(pathname, (size_t)999)
#define __MSDOS__
#define NO_TIME_INTERFACE
#define NO_TIMING_STUFF
#else
#ifdef __SC__
#define getwd(pathname) getcwd(pathname, (size_t)999)
#define __MSDOS__
#define NO_TIME_INTERFACE
#define NO_TIMING_STUFF
#endif
#endif

#ifdef __hpux
#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_XOPEN_SOURCE
#define _INCLUDE_HPUX_SOURCE
#ifndef __SC__
#ifdef THINK_C
#include "sys/types.h"
#else
#include <sys/types.h>
#endif
#endif
#undef  _INCLUDE_POSIX_SOURCE
#undef  _INCLUDE_XOPEN_SOURCE
#endif /* __hpux */
#ifndef __SC__
#ifdef THINK_C
#include "sys/times.h"
#else
#include <sys/time.h>
#endif
#endif
#ifdef __hpux
#undef _INCLUDE_HPUX_SOURCE
#endif /* __hpux */

#if (defined(__STDC__) || defined(CMU))
#include <stdlib.h>
#endif

#include <stdio.h>

#include <stddef.h>

#ifdef _WINDOWS
#include <stdlib.h>
#include <malloc.h>
#include <string.h>
#define MULTI_AGENT_ENABLED
#define UNIX
#endif

#ifdef __cplusplus
#define extern extern "C"
#endif

/* Uncomment the following line to debug memory usage */
/* #define DEBUG_MEMORY */

/* Comment out the following line to avoid the overhead of keeping statistics
   on memory pool usage */
#define MEMORY_POOL_STATS

/* Comment out the following line to avoid the overhead of keeping statistics
   on how much time is spent in various parts of the system */
/* #define DETAILED_TIMING_STATS */

/* Uncomment following line if you want to use the ANSI stdarg facility */
#define USE_STDARGS

/* UnComment the following line if you want to use multiple agents.    */
/* Warning!  Multi-agent I/O code is NOT compatible with single-agent  */
/* I/O code, so conversion is necessary.                               */

/* #define MULTI_AGENT_ENABLED */

#ifdef MULTI_AGENT_ENABLED
#define current_agent(x) (soar_agent->x)
#else
#define current_agent(x) (x)
#endif  /* MULTI_AGENT_ENABLED */

/* UnComment the following line if you want to use X windows rather    */
/* than standard I/O.                                                  */

/* #define USE_X_DISPLAY */



/* --------------------------- */
/* Current Soar version number */
/* --------------------------- */

#define MAJOR_VERSION_NUMBER 6
#define MINOR_VERSION_NUMBER 2
#define MICRO_VERSION_NUMBER 5

/* --------------------------------------------------------- */
/* Line width of terminal (used for neatly formatted output) */
/* --------------------------------------------------------- */

#define COLUMNS_PER_LINE 80

/* ------------------------------ */
/* Global type declarations, etc. */
/* ------------------------------ */

typedef unsigned char byte;
typedef char bool;

/* Some compilers define these. */
#ifndef TRUE
#define TRUE (1)
#endif
#ifndef FALSE
#define FALSE (0)
#endif

#define NIL (0)

#define EOF_AS_CHAR ((char)EOF)

/* ---------------------------------------------------------------- */
/* Macros for Inserting and Removing Stuff from Doubly-Linked Lists */
/* ---------------------------------------------------------------- */

#define insert_at_head_of_dll(header,item,next_field_name,prev_field_name) { \
  ((item)->next_field_name) = (header) ; \
  ((item)->prev_field_name) = NIL ; \
  if (header) ((header)->prev_field_name) = (item) ; \
  (header) = (item) ; }

#define remove_from_dll(header,item,next_field_name,prev_field_name) { \
  if ((item)->next_field_name) \
    ((item)->next_field_name->prev_field_name) = ((item)->prev_field_name); \
  if ((item)->prev_field_name) { \
    ((item)->prev_field_name->next_field_name) = ((item)->next_field_name); \
  } else { \
    (header) = ((item)->next_field_name); \
  } }

/* ----------------- */
/* Goal Stack Levels */
/* ----------------- */

typedef signed short goal_stack_level;
#define TOP_GOAL_LEVEL 1
#define ATTRIBUTE_IMPASSE_LEVEL 32767
#define LOWEST_POSSIBLE_GOAL_LEVEL 32767

/* -------------------------------------------------- */
/*              Names of Rete Structures              */
/* (only pointers to these are used outside the rete) */
/* -------------------------------------------------- */

struct token_struct;
struct rete_node_struct;

/* --------------------------------------------------------------------
                    Transitive Closure Numbers

   In many places, we do transitive closures or some similar process in
   which we mark identifiers and/or variables so as not to repeat them
   later.  Marking is done by setting the "tc_num" field of the symbol
   to the "current transitive closure number".  We don't have to go
   back and unmark stuff when done--we just increment the current
   transitive closure number next time.  Whenever we need to start a
   new marking, we call get_new_tc_number() (see production.c comments
   below).
-------------------------------------------------------------------- */

typedef unsigned long tc_number;

/* ======================================================================
                                 mem.c

   Init_memory_utilities() should be called before any of these routines
   are used.

   Basic memory allocation utilities:

     All memory blocks are allocated via calls to allocate_memory().  It
     calls malloc() and aborts if we run out of memory.  Free_memory() is
     the inverse of allocate_memory().  Allocate_memory_and_zerofill()
     does the obvious thing.  These routines take a usage_code indicating
     what purpose the memory is for (hash tables, strings, etc.).  This
     is used purely for statistics keeping.

     Print_memory_statistics() prints out stats on the memory usage.

   String utilities:

     Make_memory_block_for_string() takes a pointer to a string, allocates
     a memory block just large enough to hold the string, and copies the
     string into the block.   Free_memory_block_for_string() frees the
     block.
     
     "Growable strings" provide a convenient way of building up a string
     piece by piece without having to pre-allocate the right amount of
     memory.  To initialize one, say "gs = make_blank_growable_string()"
     where "gs" is of type "growable_string".  To concatenate a new string
     onto the end of an existing growable string, call
     add_to_growable_string(&gs,new_string) [note the "&"].  When you're
     done using it, call free_growable_string(gs).  Growable strings are
     implemented by allocating a block of memory large enough to hold
     (1) the memory block's size, (2) the current string length, and (3)
     the current text of the string.  Add_to_growable_string() may result
     in a new (larger) memory block being allocated and the text of the 
     string being copied.  Three macros provide access to a growable string's
     parts:  memsize_of_growable_string(), length_of_growable_string(),
     and (most importantly) text_of_growable_string(), which is of type
     (char *).
     
   Memory pools:

     To allocate and free memory items efficiently at run time, we use
     pools of small fixed-size items and do allocation and freeing using
     inline macros.  Different memory pools are used for different things
     and contain different size items.  Each pool consists of a memory_pool
     structure (used for maintaining the pool) and a chain of big blocks
     of memory (currently about 32K per block) obtained from allocate_memory().
     We maintain a free_list of small items not being used, and allocate by
     grabbing the first item on the free list.  If the free list is empty,
     we add another big block to the pool.

     Init_memory_pool() should be called to initialize a memory_pool
     structure before it is used.  After that, the macro forms
     allocate_with_pool (&mem_pool, &pointer_to_be_set_to_new_item) and
     free_with_pool (&mem_pool, pointer_to_item)
     are used to allocate and free items.  Print_memory_pool_statistics()
     prints stats about the various pools in use and how much memory each
     is using.

   Cons cell and list utilities:

     This provides a simple facility for manipulating generic lists, just
     like in Lisp.  A "cons" is a pair of pointers; a "list" is just a cons
     (or NIL).  We maintain a memory pool (see above) of conses.
     Allocate_cons() is like allocate_with_pool() for conses; free_cons()
     is like free_with_pool.  Push(new_item,my_list) is a macro for adding
     a new item onto the front of an existing list.

     In addition to the regular conses, we also have a pool of "dl_cons"es--
     these are like conses, only doubly-linked.  A "dl_list" is a just a
     dl_cons (or NIL).

     Some (consed) list utility functions:  destructively_reverse_list()
     does just what it says, and returns a pointer to the new head of the
     list (formerly the tail).  Member_of_list() tests membership, using
     "==" as the equality predicates.  Add_if_not_member() is like Lisp's
     "pushnew"; it returns the new list (possibly unchanged) list.
     Free_list() frees all the cons cells in the list.

     Sometimes we need to surgically extract particular elements from a
     list.  Extract_list_elements() and extract_dl_list_elements() do this.
     They use a callback function that indicates which elements to extract:
     the callback function is called on each element of the list, and should
     return TRUE for the elements to be extracted.  The two extraction
     functions return a list (or dl_list) of the extracted elements.

   Hash table routines:

     We use hash tables in various places, and don't want to have to fix
     their size ahead of time.  These routines provide hash tables that
     are dynamically resized as items are added and removed.  We use
     "open hashing" with a hash table whose size is a power of two.  We
     keep track of how many items are in the table.  The table grows
     when # of items >= 2*size, and shrinks when # of items < size/2.
     To resize a hash table, we rehash every item in it.

     Each item must be a structure whose first field is reserved for use
     by these hash table routines--it points to the next item in the hash
     bucket.  Hash tables are created and initialized via make_hash_table();
     you give it a hash function (i.e., a C function) that finds the hash
     value for a given item, hashing it to a value num_bits wide.  For aid
     in this, we provide masks_for_n_low_order_bits[] that select out the
     low-order bits of a value:  (x & masks_for_n_low_order_bits[23]) picks
     out the 23 low-order bits of x.
     
     Items are added/removed from a hash table via add_to_hash_table() and
     remove_from_hash_table().  These calls resize the hash table if
     necessary.
     
     The contents of a hash table (or one bucket in the table) can be
     retrieved via do_for_all_items_in_hash_table() and
     do_for_all_items_in_hash_bucket().  Each uses a callback function,
     invoking it with each successive item.  The callback function should
     normally return FALSE.  If the callback function ever returns TRUE,
     iteration over the hash table items stops and the do_for_xxx()
     routine returns immediately.  
====================================================================== */

extern void init_memory_utilities (void);

/* ----------------------- */
/* basic memory allocation */
/* ----------------------- */

#define MISCELLANEOUS_MEM_USAGE  0
#define HASH_TABLE_MEM_USAGE     1
#define STRING_MEM_USAGE         2
#define POOL_MEM_USAGE           3
#define STATS_OVERHEAD_MEM_USAGE 4

#define NUM_MEM_USAGE_CODES 5

#ifdef DEBUG_MEMORY
#define fill_with_garbage(block,size) memset((void *)(block), 0xBB, (size))
#else
#define fill_with_garbage(block,size) { }
#endif

extern void *allocate_memory (unsigned long size, int usage_code);
extern void *allocate_memory_and_zerofill (unsigned long size, int usage_code);
extern void free_memory (void *mem, int usage_code);
extern void print_memory_statistics (void);

/* ---------------- */
/* string utilities */
/* ---------------- */

#define savestring(x) (char *) strcpy (malloc (strlen (x) + 1), (x))

extern char *make_memory_block_for_string (char *s);
extern void free_memory_block_for_string (char *p);

typedef void * growable_string;

#define memsize_of_growable_string(gs) (*((int *)(gs)))
#define length_of_growable_string(gs) (*(((int *)(gs))+1))
#define text_of_growable_string(gs) (((char *)(gs)) + 2*sizeof(int *))

extern growable_string make_blank_growable_string (void);
extern void add_to_growable_string (growable_string *gs, char *string_to_add);
extern void free_growable_string (growable_string gs);

/* ------------ */
/* memory pools */
/* ------------ */

#define MAX_POOL_NAME_LENGTH 15

typedef struct memory_pool_struct {
  void *free_list;             /* header of chain of free items */
#ifdef MEMORY_POOL_STATS
  long used_count;             /* for statistics only */
#endif
  long item_size;               /* bytes per item */
  long items_per_block;        /* number of items in each big block */
  long num_blocks;             /* number of big blocks in use by this pool */
  void *first_block;           /* header of chain of blocks */
  char name[MAX_POOL_NAME_LENGTH];  /* name of the pool (for memory-stats) */
  struct memory_pool_struct *next;  /* next in list of all memory pools */
} memory_pool;

extern void add_block_to_memory_pool (memory_pool *p);
extern void init_memory_pool (memory_pool *p, long item_size, char *name);
extern void print_memory_pool_statistics (void);

#ifdef MEMORY_POOL_STATS
#define increment_used_count(p) {(p)->used_count++;}
#define decrement_used_count(p) {(p)->used_count--;}
#else
#define increment_used_count(p) { }
#define decrement_used_count(p) { }
#endif

#define allocate_with_pool(p,dest_item_pointer) { \
  if (! (p)->free_list) add_block_to_memory_pool(p); \
  *(dest_item_pointer) = (p)->free_list; \
  (p)->free_list =  *(void * *)(*(dest_item_pointer)); \
  fill_with_garbage (*(dest_item_pointer), (p)->item_size); \
  increment_used_count(p); }

#define free_with_pool(p,item) { \
  fill_with_garbage ((item), (p)->item_size); \
  *(void * *)(item) = (p)->free_list; \
  (p)->free_list = (void *)(item); \
  decrement_used_count(p); }

/* ------------------------- */
/* Cons cell, list utilities */
/* ------------------------- */

typedef struct cons_struct {
  void *first;
  struct cons_struct *rest;
} cons;

typedef cons list;

typedef struct dl_cons_struct {
  void *item;
  struct dl_cons_struct *next;
  struct dl_cons_struct *prev;
} dl_cons;

typedef dl_cons dl_list;

#define allocate_cons(dest_cons_pointer) \
  allocate_with_pool (&current_agent(cons_cell_pool), (dest_cons_pointer))
#define free_cons(c) free_with_pool (&current_agent(cons_cell_pool), (c))

#define push(item,list_header) { \
  cons *push_cons_xy298; \
  allocate_cons (&push_cons_xy298); \
  push_cons_xy298->first = (item); \
  push_cons_xy298->rest = (list_header); \
  (list_header) = push_cons_xy298; }

extern list *destructively_reverse_list (list *c);
extern bool member_of_list (void *item, list *the_list);
extern list *add_if_not_member (void *item, list *old_list);
extern void free_list (list *the_list);

typedef bool (*cons_test_fn)(cons *c);
typedef bool (*dl_cons_test_fn)(dl_cons *dc);

extern list *extract_list_elements (list **header, cons_test_fn f);
extern dl_list *extract_dl_list_elements (dl_list **header, dl_cons_test_fn f);

/* ----------------------------- */
/* Resizable hash table routines */
/* ----------------------------- */

extern unsigned long masks_for_n_low_order_bits[33];
typedef unsigned long ((*hash_function)(void *item, short num_bits));

typedef struct item_in_hash_table_struct {
  struct item_in_hash_table_struct *next;
  char data;
} item_in_hash_table;

typedef item_in_hash_table *bucket_array;

typedef struct hash_table_struct {
  unsigned long count;      /* number of items in the table */
  unsigned long size;       /* number of buckets */
  short log2size;           /* log (base 2) of size */
  short minimum_log2size;   /* table never shrinks below this size */
  bucket_array *buckets;
  hash_function h;          /* call this to hash or rehash an item */
} hash_table;  

extern struct hash_table_struct *make_hash_table (short minimum_log2size,
                                                  hash_function h);
extern void remove_from_hash_table (struct hash_table_struct *ht, void *item);
extern void add_to_hash_table (struct hash_table_struct *ht, void *item);

typedef bool (*hash_table_callback_fn)(void *item);

extern void do_for_all_items_in_hash_table (struct hash_table_struct *ht,
                                            hash_table_callback_fn f);
extern void do_for_all_items_in_hash_bucket (struct hash_table_struct *ht,
                                             hash_table_callback_fn f,
                                             unsigned long hash_value);

/* ======================================================================
                             lexer.c

  The lexer reads files and returns a stream of lexemes.  Get_lexeme() is
  the main routine; it looks for the next lexeme in the input, and stores
  it in the global variable "lexeme".  See the structure definition below.

  Restrictions:  the lexer cannot read individual input lines longer than
  MAX_LEXER_LINE_LENGTH characters.  Thus, a single lexeme can't be longer
  than that either.

  The lexer maintains a stack of files being read, in order to handle nested
  loads.  Start_lex_from_file() and stop_lex_from_file() push and pop the
  stack.  Immediately after start_lex_from_file(), the current lexeme (global
  variable) is undefined.  Immediately after stop_lex_from_file(), the 
  current lexeme is automatically restored to whatever it was just before
  the corresponding start_lex_from_file() call.
  
  Determine_possible_symbol_types_for_string() is a utility routine which
  figures out what kind(s) of symbol a given string could represent.
  
  Print_location_of_most_recent_lexeme() is used to print an indication
  of where a parser error occurred.  It tries to print out the current
  source line with a pointer to where the error was detected.
  
  Current_lexer_parentheses_level() returns the current level of parentheses
  nesting (0 means no open paren's have been encountered).
  Skip_ahead_to_balanced_parentheses() eats lexemes until the appropriate
  closing paren is found (0 means eat until back at the top level).
  
  Fake_rparen_at_next_end_of_line() tells the lexer to insert a fake
  R_PAREN_LEXEME token the next time it reaches the end of a line.
  
  Set_lexer_allow_ids() tells the lexer whether to allow identifiers to
  be read.  If FALSE, things that look like identifiers will be returned
  as SYM_CONSTANT_LEXEME's instead.
====================================================================== */

#define reading_from_top_level() (! current_agent(current_file)->parent_file)

#define MAX_LEXER_LINE_LENGTH 1000
#define MAX_LEXEME_LENGTH (MAX_LEXER_LINE_LENGTH+5) /* a little bigger to avoid
                                                       any off-by-one-errors */

enum lexer_token_type {
  EOF_LEXEME,                        /* end-of-file */
  IDENTIFIER_LEXEME,                 /* identifier */
  VARIABLE_LEXEME,                   /* variable */
  SYM_CONSTANT_LEXEME,               /* symbolic constant */
  INT_CONSTANT_LEXEME,               /* integer constant */
  FLOAT_CONSTANT_LEXEME,             /* floating point constant */
  L_PAREN_LEXEME,                    /* "(" */
  R_PAREN_LEXEME,                    /* ")" */
  L_BRACE_LEXEME,                    /* "{" */
  R_BRACE_LEXEME,                    /* "}" */
  PLUS_LEXEME,                       /* "+" */
  MINUS_LEXEME,                      /* "-" */
  RIGHT_ARROW_LEXEME,                /* "-->" */
  GREATER_LEXEME,                    /* ">" */
  LESS_LEXEME,                       /* "<" */
  EQUAL_LEXEME,                      /* "=" */
  LESS_EQUAL_LEXEME,                 /* "<=" */
  GREATER_EQUAL_LEXEME,              /* ">=" */
  NOT_EQUAL_LEXEME,                  /* "<>" */
  LESS_EQUAL_GREATER_LEXEME,         /* "<=>" */
  LESS_LESS_LEXEME,                  /* "<<" */
  GREATER_GREATER_LEXEME,            /* ">>" */
  AMPERSAND_LEXEME,                  /* "&" */
  AT_LEXEME,                         /* "@" */
  TILDE_LEXEME,                      /* "~" */
  UP_ARROW_LEXEME,                   /* "^" */
  EXCLAMATION_POINT_LEXEME,          /* "!" */
  COMMA_LEXEME,                      /* "," */
  PERIOD_LEXEME,                     /* "." */
  QUOTED_STRING_LEXEME,              /* string in double quotes */
  DOLLAR_STRING_LEXEME };            /* string for shell escape */

#define LENGTH_OF_LONGEST_SPECIAL_LEXEME 3  /* length of "-->" and "<=>"--
                                               if a longer one is added, be
                                               sure to update this! */

struct lexeme_info {
  enum lexer_token_type type;         /* what kind of lexeme it is */
  char string[MAX_LEXEME_LENGTH+1];   /* text of the lexeme */
  int length;                         /* length of the above string */
  long int_val;                       /* for INT_CONSTANT_LEXEME's */
  float float_val;                    /* for FLOAT_CONSTANT_LEXEME's */
  char id_letter;                     /* for IDENTIFIER_LEXEME's */
  unsigned long id_number;            /* for IDENTIFIER_LEXEME's */
};

extern void determine_possible_symbol_types_for_string (char *s,
                                                        int length_of_s,
                                                        bool *possible_id,
                                                        bool *possible_var,
                                                        bool *possible_sc,
                                                        bool *possible_ic,
                                                        bool *possible_fc,
                                                        bool *rereadable);

extern void init_lexer (void);
extern void start_lex_from_file (char *filename, FILE *already_opened_file);
extern void stop_lex_from_file (void);

extern void get_lexeme (void);
extern void print_location_of_most_recent_lexeme (void);

extern int current_lexer_parentheses_level (void);
extern void skip_ahead_to_balanced_parentheses (int parentheses_level);
extern void fake_rparen_at_next_end_of_line (void);
extern void set_lexer_allow_ids (bool allow_identifiers);

/* =================================================================
                             interface.c                             

  User Interface Command Routines:

     Each user interface command has a corresponding function
     (user_interface_routine) to handle it.  These commands/functions
     should be installed at system startup time via add_command().  The
     command name string passed to add_command() must be permanently
     available (e.g., a constant in global data memory).

     When a user interface routine is called, the current lexeme is the
     command name.  The function should call the lexer to read its arguments,
     etc.  If successful, the function should return TRUE and should exit
     with the current lexeme being the closing right parenthesis (otherwise
     the dispatching function will print an error message about extra
     arguments being given).  If unsuccessful, the function should
     return FALSE.

  Dispatching commands:

     Dispatch_command() dispatches the appropriate user interface routine
     for the current command (i.e., the command named by the current lexeme).
     It calls set_lexer_allow_ids(TRUE) before dispatching the command,
     so if the command doesn't allow id's, it should call 
     set_lexer_allow_ids(FALSE) immediately.  Dispatch_command() returns 
     TRUE if the command was successful, FALSE if any error occurred.
   
     Repeatedly_read_and_dispatch_commands() keeps calling dispatch_command()
     until end-of-file is reached on the current input file.
     
     Load_file() sets up the lexer to read from a given open file, executes
     all the commands in that file, and then restore the lexer to reading
     the previous file.

  Help Information:

     Add_help() should be called at system startup time to specify to the
     "help" command what help info is available.  It takes a topic name and
     an array of lines of text for the helpscreen.  All these strings
     should be permanently available (e.g., constants in global data memory).
================================================================= */

typedef bool (*user_interface_routine)(void);
extern void add_command (char *command_name, user_interface_routine f);

extern bool dispatch_command (void);

extern void repeatedly_read_and_dispatch_commands (void);

extern void load_file (char *file_name, FILE *already_open_file);

extern void add_help (char *topic, char **lines_of_text);

extern void init_built_in_commands (void);
#ifdef MULTI_AGENT_ENABLED
extern void init_multi_agent_built_in_commands(void);
#endif

extern char *tilde_expand (char *filename);

/* AGR 568 begin */
typedef struct expansion_node {
  struct lexeme_info lexeme;
  struct expansion_node *next;
} expansion_node;

typedef struct alias_struct {
  char *alias;
  struct expansion_node *expansion;
  struct alias_struct *next;
} alias_struct;

typedef struct dir_stack_struct {
  char *directory;
  struct dir_stack_struct *next;
} dir_stack_struct;
/* AGR 568 end */

/* AGR 568  This bug fix concerned an alias command.  But I've expanded
   it a little to also include the pushd and popd commands, which are
   all being implemented for the release of 6.2.  11-May-94 */

/* =======================================================================
                             symtab.c

   Soar 6 uses five kinds of symbols:  symbolic constants, integer
   constants, floating-point constants, identifiers, and variables.
   We use five resizable hash tables, one for each kind of symbol.

   "symbol" is typedef-ed as a union of the five kinds of symbol
   structures.  Some fields common to all symbols are accessed via
   sym->common.field_name; fields particular to a certain kind of
   symbol are accessed via sym->var.field_name_on_variables, etc.
   (See the definitions below.)  Note that "common" is #defined below.

   Some (but not all) of the fields common to all symbols are:
      symbol_type:  indicates which of the five kinds of symbols this is
      reference_count:  current reference count for this symbol
      hash_id:  used for hash functions in the rete (and elsewhere)
      
   Fields on symbolic constants:
      name:  points to null-terminated string giving its name
      production:  points to a production structure, or NIL if there is
                   no production with that name

   Fields on integer constants:
      value:  gives the value of the symbol.  This is of type (long).

   Fields on floating-point constants:
      value:  gives the value of the symbol.  This is of type (float).

   Fields on variables:
      name:  points to null-terminated string giving its name
      tc_num:  used for transitive closure computations
      current_binding_value:  when productions are fired, this indicates
                              the variable's binding
      gensym_number:  used by the variable generator to prevent certain
                      already-in-use variables from being generated

   Fields on identifiers:

       name_number, name_letter:  indicate the name of the identifier

       isa_goal, isa_impasse:  indicate whether this is the identifier of a
                               goal or attribute impasse

       isa_problem_space:  keeps a count of how many (normal or acceptable
                           preference) wmes contain (^problem-space <this-id>).
                           The tracing code uses this to figure out whether
                           a given object is a problem space object.
       isa_state, isa_operator:  similar.                   

       allow_bottom_up_chunks:  Used for bottom-up chunking, and only on goal
         identifiers.  This is TRUE iff no chunk has yet been built for a
         subgoal of this goal.
        
       could_be_a_link_from_below:  TRUE if there might be a link to this id
         from some other id lower in the goal stack.
       level:  current goal_stack_level of this id
       promotion_level:  level to which this id is going to be promoted as
         soon as ownership info is updated.
       link_count:  count of how many links there are to this id.
       unknown_level:  if the goal_stack_level of this id is known, this is
         NIL.  If the level isn't known, it points to a dl_cons in a dl_list
         used by the demotion routines.

       slots:  this is the header for a dll of the slots for this id.

       tc_num:  used for transitive closures, marking id's, etc.
 
       variablization:  used by the chunker when variablizing chunks--this
         points to the variable to which this id gets changed

       impasse_wmes:  for goal and impasse ids only:  this is the header
         of the dll of architecture-created wmes (e.g., (G37 ^object G36))

       higher_goal, lower_goal:  for goals, these point to adjacent goals
         in the context stack
       problem_space_slot, state_slot, operator_slot:  for goals, these
         point to the corresponding context slots
       preferences_from_goal:  for goals, this is the header of the dll
         of all preferences supported by this goal.  This is needed so
         we can remove o-supported preferences when the goal goes away.

       associated_output_links:  used by the output module
       input_wmes:  dll of wmes added by input functions

   Reference counting for symbols:  I can't remember all the places I add
     reference counts to symbols.  Here's a bunch I can remember though.
     If you're not sure whether to add/remove a reference for something,
     it's better to play it safe and do the add/remove.

     +1 for each occurrence in a rete test or alpha mem constant test
     +1 for each occurrence in a condition test anywhere
     +1 for each occurrence in a Not
     +1 for each occurrence in a saved_test
     +1 for each occurrence in a WME
     +1 for each occurrence in a preference
     +1 for each occurrence as {id or attr} of a slot
     +1 for goal/impasse identifiers
     +1 if it's the name of a production
     +1 if it's a predefined symbol (e.g., "goal" or "operator")
     +1 for each enqueued add-link or remove-link to/from it
     +1 for each occurrence in a global var. (e.g., chunk-free-problem-spaces)

  We deallocate a symbol when its reference count goes to 0.
======================================================================= */

#define VARIABLE_SYMBOL_TYPE 0
#define IDENTIFIER_SYMBOL_TYPE 1
#define SYM_CONSTANT_SYMBOL_TYPE 2
#define INT_CONSTANT_SYMBOL_TYPE 3
#define FLOAT_CONSTANT_SYMBOL_TYPE 4

/* WARNING:  In the following structure, next_in_hash_table MUST be the
   first field.  This field is used by the resizable hash table routines. */

typedef struct symbol_common_data_struct {
  union symbol_union *next_in_hash_table;  /* next item in hash bucket */
  unsigned long reference_count;
  byte symbol_type;                /* one of the above xxx_SYMBOL_TYPE's */
  byte decider_flag;               /* used only by the decider */
  struct wme_struct *decider_wme;  /* used only by the decider */
  unsigned long hash_id;           /* used for hashing in the rete */
} symbol_common_data;

/* WARNING:  In the following structures (the five kinds of symbols),
   common_symbol_info MUST be the first field. */

typedef struct sym_constant_struct {
  symbol_common_data common_symbol_info;
  char *name;
  struct production_struct *production;  /* NIL if no prod. has this name */
} sym_constant;

typedef struct int_constant_struct {
  symbol_common_data common_symbol_info;
  long value;
} int_constant;

typedef struct float_constant_struct {
  symbol_common_data common_symbol_info;
  float value;
} float_constant;

typedef struct variable_struct {
  symbol_common_data common_symbol_info;
  char *name;
  tc_number tc_num;
  union symbol_union *current_binding_value;
  unsigned long gensym_number;
} variable;

/* Note: I arranged the fields below to try to minimize space */
typedef struct identifier_struct {
  symbol_common_data common_symbol_info;
  unsigned long name_number;
  char name_letter;

  bool isa_goal;        /* TRUE iff this is a goal identifier */
  bool isa_impasse;     /* TRUE iff this is an attr. impasse identifier */
  unsigned short isa_problem_space, isa_state, isa_operator;

  bool allow_bottom_up_chunks;

  /* --- ownership, promotion, demotion, & garbage collection stuff --- */
  bool could_be_a_link_from_below;
  goal_stack_level level;
  goal_stack_level promotion_level;
  unsigned long link_count;
  dl_cons *unknown_level;

  struct slot_struct *slots;  /* dll of slots for this identifier */
  tc_number tc_num;           /* used for transitive closures, marking, etc. */
  union symbol_union *variablization;  /* used by the chunker */

  /* --- fields used only on goals and impasse identifiers --- */
  struct wme_struct *impasse_wmes;

  /* --- fields used only on goals --- */
  union symbol_union *higher_goal, *lower_goal;
#ifndef NNPSCM
  struct slot_struct *problem_space_slot, *state_slot, *operator_slot;
#else
  struct slot_struct *operator_slot;
#endif
  struct preference_struct *preferences_from_goal;

  /* --- fields used for Soar I/O stuff --- */
  list *associated_output_links;
  struct wme_struct *input_wmes;
} identifier;

typedef union symbol_union {
  variable var;
  identifier id;
  sym_constant sc;
  int_constant ic;
  float_constant fc;
} Symbol;

/* WARNING: this #define's "common".  Don't use "common" anywhere in the
   code unless you intend this meaning of it.  This is so we can
   conveniently access fields used in all kinds of symbols, like this:
   "sym.common.reference_count" rather than "sym.var.common.reference_count"
   or "sym.id.common.reference_count", etc. */

#define common var.common_symbol_info

/* -----------------------------------------------------------------
                       Symbol Table Routines

   Initialization:

     Init_symbol_tables() should be called first, to initialize the
     module.

   Lookup and Creation:

     The find_xxx() routines look for an existing symbol and return it
     if found; if no such symbol exists, they return NIL.

     The make_xxx() routines look for an existing symbol; if the find one,
     they increment the reference count and return it.  If no such symbol
     exists, they create a new one, set the reference count to 1, and
     return it.

     Note that rather than a make_identifier() routine, we have a
     make_new_identifier() routine, which takes two arguments: the first
     letter for the new identifier, and its initial goal_stack_level.
     There is no way to force creation of an identifier with a particular
     name letter/number combination like J37.

   Reference Counting:

     Symbol_add_ref() and symbol_remove_ref() are macros for incrementing
     and decrementing the reference count on a symbol.  When the count
     goes to zero, symbol_remove_ref() calls deallocate_symbol().

   Other Utilities:

     Reset_id_counters() is called during an init-soar to reset the id
     gensym numbers to 1.  It first makes sure there are no existing
     identifiers in the system--otherwise we might generate a second
     identifier with the same name later on.

     Reset_id_and_variable_tc_numbers() resets the tc_num field of every
     existing id and variable to 0.
     
     Reset_variable_gensym_numbers() resets the gensym_number field of
     every existing variable to 0.
     
     Print_all_symbols() just prints a list of all existing symbols.
     (This is useful for debugging memory leaks.)
     
     Generate_new_sym_constant() is used to gensym new symbols that are
     guaranteed to not already exist.  It takes two arguments: "prefix"
     (the desired prefix of the new symbol's name), and "counter" (a
     pointer to a counter (unsigned long) that is incremented to produce
     new gensym names).
----------------------------------------------------------------- */

extern void init_symbol_tables (void);

extern Symbol *find_variable (char *name);
extern Symbol *find_identifier (char name_letter, unsigned long name_number);
extern Symbol *find_sym_constant (const char *name);  /* AGR 600 */
extern Symbol *find_int_constant (long value);
extern Symbol *find_float_constant (float value);

extern Symbol *make_variable (char *name);
extern Symbol *make_sym_constant (char *name);
extern Symbol *make_int_constant (long value);
extern Symbol *make_float_constant (float value);
extern Symbol *make_new_identifier (char name_letter, goal_stack_level level);

/* --- macros used for changing the reference count --- */
#define symbol_add_ref(x) {(x)->common.reference_count++;}
#define symbol_remove_ref(x) { \
  (x)->common.reference_count--; \
  if ((x)->common.reference_count == 0) \
  deallocate_symbol(x); \
  }

extern void deallocate_symbol (Symbol *sym);

extern void reset_id_counters (void);
extern void reset_id_and_variable_tc_numbers (void);
extern void reset_variable_gensym_numbers (void);
extern void print_all_symbols (void);
extern Symbol *generate_new_sym_constant (char *prefix,unsigned long *counter);

/* -----------------------------------------------------------------
                       Predefined Symbols

   Certain symbols are used so frequently that we create them at
   system startup time and never deallocate them.
   
   Create_predefined_symbols() should be called to do the creation.
   After that, the global variables can be accessed freely.  Note that
   the reference counts on these symbols should still be updated--
   symbol_add_ref() should be called, etc.--it's just that when the
   symbol isn't really being used, it stays around because the count
   is still 1.
----------------------------------------------------------------- */

extern void create_predefined_symbols (void);

/* ========================================================================= */
/*                                                                           */
/*                         Global Data Structures                            */
/*                                                                           */
/* ========================================================================= */

/* ------------------------------------------------------------------------
                      Working Memory Elements (WMEs)

   Fields in a WME:

      id, attr, value:  points to symbols for the wme fields

      acceptable:  TRUE iff this is an acceptable pref. wme

      timetag:  timetag of the wme

      reference count:  (see below)

      rete_next, rete_prev:  pointers in the doubly-linked list of all
         wmes currently known to the rete (header is all_wmes_in_rete)
         (this equals WM except while WM is being changed)

      next, prev:  pointers in a doubly-linked list of wmes.
         Depending on the wme type, the header of this DLL is:
           - slot.wmes (for ordinary wmes)
           - slot.acceptable_preference_wmes (for acceptable pref. wmes)
           - id.impasse_wmes (for architecture-created goal/impasse wmes)
           - id.input_wmes (for Soar I/O wmes)

      preference:  points to the preference supporting the wme.  For I/O
         wmes and (most) architecture-created wmes, this is NIL.

      output_link:  this is used only for top-state output links.
         It points to an output_link structure used by the I/O routines.

      grounds_tc, potentials_tc, locals_tc:  used by the chunker to indicate
         whether this wme is in the grounds, potentials, and/or locals sets

      chunker_bt_pref: used by the chunker; set to cond->bt.trace when
         a wme is added to either the potentials or locals set

   Reference counts on wmes:
      +1 if the wme is currently in WM
      +1 for each instantiation condition that points to it (bt.wme)
   We deallocate a wme when its reference count goes to 0.
------------------------------------------------------------------------ */

typedef struct wme_struct {
  /* WARNING:  The next three fields (id,attr,value) MUST be consecutive--
     the rete code relies on this! */
  Symbol *id;
  Symbol *attr;
  Symbol *value;
  bool acceptable;
  unsigned long timetag;
  unsigned long reference_count;
  struct wme_struct *rete_next, *rete_prev; /* used for dll of wmes in rete */
  struct wme_struct *next, *prev;           /* (see above) */
  struct preference_struct *preference;     /* pref. supporting it, or NIL */
  struct output_link_struct *output_link;   /* for top-state output commands */
  tc_number grounds_tc;                     /* for chunker use only */
  tc_number potentials_tc, locals_tc;
  struct preference_struct *chunker_bt_pref;
} wme;

/* ------------------------------------------------------------------------
                               Preferences

   Fields in a preference:

      type:  indicates the type of the preference.  This is one of the
             types defined below:  ACCEPTABLE_PREFERENCE_TYPE, etc.

      o_supported:  TRUE iff the preference has o-support

      in_tm:  TRUE iff the preference is currently in temporary memory

      on_goal_list:  TRUE iff the preference is on the list of preferences
                     supported by its match goal (see all_of_goal_next below)

      reference_count:  (see below)

      id, attr, value, referent:  points to the symbols.  Referent is only
                                  used for binary preferences.

      slot:  points to the slot this preference is for.  (NIL if the
        preference is not in TM.)

      next, prev:  used for a doubly-linked list of preferences of the
                   same type in that particular slot

      all_of_slot_next, all_of_slot_prev:  used for a doubly-linked list
          of all preferences (of any type) in that particular slot

      all_of_goal_next, all_of_goal_prev:  used for a doubly-linked list
          of all preferences supported by this particular match goal.
          This is needed in order to remove all o-support from a particular
          goal when that goal is removed from the context stack.

      next_clone, prev_clone:  used for a doubly-linked list of all "clones"
        of this preference.  When a result is returned from a subgoal and a
        chunk is built, we get two copies of the "same" preference, one from
        the subgoal's production firing, and one from the chunk instantiation.
        If results are returned more than one level, or the same result is
        returned simultaneously by multiple production firings, we can get
        lots of copies of the "same" preference.  These clone preferences
        are kept on a list so that we can find the right one to backtrace
        through, given a wme supported by "all the clones."

      inst:  points to the instantiation that generated this preference

      inst_next, inst_prev:  used for a doubly-linked list of all
        existing preferences that were generated by that instantiation

      next_candidate:  used by the decider for lists of candidate values
        for a certain slot

      next_result:  used by the chunker for a list of result preferences

   Reference counts on preferences:
      +1 if the preference is currently in TM
      +1 for each instantiation condition that points to it (bt.trace)
      +1 if it supports an installed context WME

   We deallocate a preference if:
      (1) reference_count==0 and all its clones have reference_count==0
          (hence it couldn't possibly be needed anymore)
   or (2) its match goal is removed from the context stack
          (hence there's no way we'll ever want to BT through it)
------------------------------------------------------------------------ */

/* WARNING: preference types must be numbered 0..(NUM_PREFERENCE_TYPES-1),
   because the slot structure contains an array using these indices. */
#define NUM_PREFERENCE_TYPES 13  /* number of different preference types */

#define ACCEPTABLE_PREFERENCE_TYPE 0
#define REQUIRE_PREFERENCE_TYPE 1
#define REJECT_PREFERENCE_TYPE 2
#define PROHIBIT_PREFERENCE_TYPE 3
#define RECONSIDER_PREFERENCE_TYPE 4
#define UNARY_INDIFFERENT_PREFERENCE_TYPE 5
#define UNARY_PARALLEL_PREFERENCE_TYPE 6
#define BEST_PREFERENCE_TYPE 7
#define WORST_PREFERENCE_TYPE 8
#define BINARY_INDIFFERENT_PREFERENCE_TYPE 9
#define BINARY_PARALLEL_PREFERENCE_TYPE 10
#define BETTER_PREFERENCE_TYPE 11
#define WORSE_PREFERENCE_TYPE 12
#define preference_is_unary(p) ((p)<9)
#define preference_is_binary(p) ((p)>8)

typedef struct preference_struct {
  byte type;         /* acceptable, better, etc. */
  bool o_supported;  /* is the preference o-supported? */
  bool in_tm;        /* is this currently in TM? */
  bool on_goal_list; /* is this pref on the list for its match goal */
  unsigned long reference_count;
  Symbol *id;
  Symbol *attr;
  Symbol *value;
  Symbol *referent;
  struct slot_struct *slot;

  /* dll of pref's of same type in same slot */
  struct preference_struct *next, *prev;

  /* dll of all pref's in same slot */
  struct preference_struct *all_of_slot_next, *all_of_slot_prev;

  /* dll of all pref's from the same match goal */
  struct preference_struct *all_of_goal_next, *all_of_goal_prev;
  
  /* dll (without header) of cloned preferences (created when chunking) */
  struct preference_struct *next_clone, *prev_clone;
    
  struct instantiation_struct *inst;
  struct preference_struct *inst_next, *inst_prev;
  struct preference_struct *next_candidate;
  struct preference_struct *next_result;
} preference;

/* ------------------------------------------------------------------------

                             Impasse Types

------------------------------------------------------------------------ */

#define NONE_IMPASSE_TYPE 0                   /* no impasse */
#define CONSTRAINT_FAILURE_IMPASSE_TYPE 1
#define CONFLICT_IMPASSE_TYPE 2
#define TIE_IMPASSE_TYPE 3
#define NO_CHANGE_IMPASSE_TYPE 4

/* ------------------------------------------------------------------------
                                Slots

   Fields in a slot:

      next, prev:  used for a doubly-linked list of all slots for a certain
        identifier.

      id, attr:   identifier and attribute of the slot

      wmes:  header of a doubly-linked list of all wmes in the slot

      acceptable_preference_wmes:  header of doubly-linked list of all
        acceptable preference wmes in the slot.  (This is only used for
        context slots.)

      all_preferences:  header of a doubly-linked list of all preferences
        currently in the slot

      preferences[NUM_PREFERENCE_TYPES]: array of headers of doubly-linked
        lists, one for each possible type of preference.  These store
        all the preferences, sorted into lists according to their types.
        Within each list, the preferences are sorted according to their
        match goal, with the pref. supported by the highest goal at the
        head of the list.

      impasse_id:  points to the identifier of the attribute impasse object
        for this slot.  (NIL if the slot isn't impassed.)

      isa_context_slot:  TRUE iff this is a context slot

      impasse_type:  indicates the type of the impasse for this slot.  This
        is one of NONE_IMPASSE_TYPE, CONSTRAINT_FAILURE_IMPASSE_TYPE, etc.
  
      marked_for_possible_removal:  TRUE iff this slot is on the list of
        slots that might be deallocated at the end of the current top-level
        phase.

      changed:  indicates whether the preferences for this slot have changed.
        For non-context slots, this is either NIL or a pointer to the
        corresponding dl_cons in changed_slots (see decide.c); for context
        slots, it's just a zero/nonzero flag.

      acceptable_preference_changed:  for context slots only; this is zero
        if no acceptable or require preference in this slot has changed;
        if one has changed, it points to a dl_cons.
------------------------------------------------------------------------ */

typedef struct slot_struct {
  struct slot_struct *next, *prev;  /* dll of slots for this id */
  Symbol *id;                       /* id, attr of the slot */
  Symbol *attr;
  wme *wmes;                        /* dll of wmes in the slot */
  wme *acceptable_preference_wmes;  /* dll of acceptable pref. wmes */
  preference *all_preferences;      /* dll of all pref's in the slot */
  preference *preferences[NUM_PREFERENCE_TYPES]; /* dlls for each type */
  Symbol *impasse_id;               /* NIL if slot is not impassed */
  bool isa_context_slot;            
  byte impasse_type;
  bool marked_for_possible_removal;
  dl_cons *changed;   /* for non-context slots: points to the corresponding
                         dl_cons in changed_slots;  for context slots: just
                         zero/nonzero flag indicating slot changed */
  dl_cons *acceptable_preference_changed; /* for context slots: either zero,
                                             or points to dl_cons if the slot
                                             has changed + or ! pref's */
} slot;

/* -------------------------------------------------------------------
                              Tests
   
   Tests in conditions can be blank (null) tests, tests for equality
   with a variable or constant, or more complicated tests (such as
   not-equal tests, conjunctive tests, etc.).  We use some bit twiddling
   here to minimize space.  We use just a pointer to represent any kind
   of test.  For blank tests, this is the NIL pointer.  For equality tests,
   it points to the symbol referent of the test.  For other kinds of tests,
   bit 0 of the pointer is set to 1, and the pointer (minus 1) points to
   a complex_test structure.  (A field in the complex_test structure 
   further indicates the type of the test.)
------------------------------------------------------------------- */

typedef char * test;

#define test_is_blank_test(t) ((t)==NIL)
#define test_is_complex_test(t) (((unsigned long)(t)) & 1)
#define test_is_blank_or_equality_test(t) (! test_is_complex_test(t))

#define make_blank_test() ((test)NIL)
#define make_equality_test(sym) ((sym)->common.reference_count++, (test)(sym))
#define make_equality_test_without_adding_reference(sym) ((test)(sym))
#define make_blank_or_equality_test(sym_or_nil) \
  ((sym_or_nil) ? make_equality_test(sym_or_nil) : make_blank_test() )
#define make_test_from_complex_test(ct) ((test) (((char *)(ct))+1))

#define referent_of_equality_test(t) ((Symbol *) (t))
#define complex_test_from_test(t) ((complex_test *) (((char *)(t))-1))

typedef struct complex_test_struct {
  byte type;                  /* see definitions below */
  union test_info_union {
    Symbol *referent;         /* for relational tests */
    list *disjunction_list;   /* for disjunction tests */
    list *conjunct_list;      /* for conjunctive tests */
  } data;
} complex_test;

/* types of the complex_test's */
#define NOT_EQUAL_TEST 1         /* various relational tests */
#define LESS_TEST 2
#define GREATER_TEST 3
#define LESS_OR_EQUAL_TEST 4
#define GREATER_OR_EQUAL_TEST 5
#define SAME_TYPE_TEST 6
#define DISJUNCTION_TEST 7       /* item must be one of a list of constants */
#define CONJUNCTIVE_TEST 8       /* item must pass each of a list of tests */
#define GOAL_ID_TEST 9           /* item must be a goal identifier */
#define IMPASSE_ID_TEST 10       /* item must be an impasse identifier */

/* -------------------------------------------------------------------
                             Conditions

   Conditions are used for two things:  (1) to represent the LHS of newly
   entered productions (new SP's or chunks); and (2) to represent the 
   instantiated LHS in production instantiations.
   
   Fields in a condition:

      type:  indicates the type of condition:  either POSITIVE_CONDITION,
        NEGATIVE_CONDITION, or CONJUNCTIVE_NEGATION_CONDITION.

      already_in_tc:  (reserved for use by the cond_is_in_tc() stuff in
        production.c)

      next, prev:  used for a doubly-linked list of all conditions on the
        LHS, or all subconditions of an NCC.

      data.tests.id_test, data.tests.attr_test, data.tests.value_test:
        for positive and negative conditions, these are the three wme
        field tests for the condition.

      test_for_acceptable_preference:  for positive and negative conditions,
        this is TRUE iff the condition tests for acceptable preference wmes.

      data.ncc.top, data.ncc.bottom:  for NCC's, these point to the top and
        bottom of the subconditions likned list.

      bt:  for top-level positive conditions in production instantiations,
        this structure gives information for that will be used in backtracing.

      reorder:  (reserved for use by the reorderer)
------------------------------------------------------------------- */

/* --- types of conditions --- */
#define POSITIVE_CONDITION 0
#define NEGATIVE_CONDITION 1
#define CONJUNCTIVE_NEGATION_CONDITION 2

/* --- info on conditions used for backtracing (and by the rete) --- */
typedef struct bt_info_struct {
  wme *wme;                 /* the actual wme that was matched */
  goal_stack_level level;   /* level (at firing time) of the id of the wme */
  preference *trace;        /* preference for BT, or NIL */

  /* mvp 5-17-94 */
  list *prohibits;          /* list of prohibit prefs to backtrace through */
} bt_info;

/* --- info on conditions used only by the reorderer --- */
typedef struct reorder_info_struct {
  list *vars_requiring_bindings;           /* used only during reordering */
  struct condition_struct *next_min_cost;  /* used only during reordering */
} reorder_info;

/* --- info on positive and negative conditions only --- */
typedef struct three_field_tests_struct {
  test id_test;
  test attr_test;
  test value_test;
} three_field_tests;

/* --- info on negated conjunctive conditions only --- */
typedef struct ncc_info_struct {
  struct condition_struct *top;
  struct condition_struct *bottom;
} ncc_info;

/* --- finally, the structure of a condition --- */
typedef struct condition_struct {
  byte type;
  bool already_in_tc;                 /* used only by cond_is_in_tc stuff */
  bool test_for_acceptable_preference;   /* for pos, neg cond's only */
  struct condition_struct *next, *prev;
  union condition_main_data_union {
    three_field_tests tests;             /* for pos, neg cond's only */
    ncc_info ncc;                        /* for ncc's only */
  } data;
  bt_info bt;  /* for top-level positive cond's: used for BT and by the rete */
  reorder_info reorder;  /* used only during reordering */
} condition;

/* -------------------------------------------------------------------
                      Right-Hand-Side Values
   
   Values on the RHS of productions can be given either by symbols
   (constants or variables) or by function calls.  We use the low-order
   bit of a pointer to differentiate between these types of values.
   An rhs_value is either a pointer to a symbol (if bit 0 is 0) or
   a pointer to a list (if bit 0 is 1).  For function calls, the list
   is a consed list, with the first element the rhs_function structure
   and the remaining elements the arguments of the function call.
   (Each argument is an rhs_value.)
------------------------------------------------------------------- */

typedef char * rhs_value;

#define rhs_value_is_funcall(rv) (((unsigned long)(rv)) & 1)
#define rhs_value_is_symbol(rv) (! rhs_value_is_funcall(rv))

/* Warning: symbol_to_rhs_value() doesn't symbol_add_ref.  The caller must
   do the reference count update */
#define symbol_to_rhs_value(sym) ((rhs_value) (sym))
#define funcall_list_to_rhs_value(fl) ((rhs_value) (((char *)(fl))+1))

#define rhs_value_to_symbol(rv) ((Symbol *)(rv))
#define rhs_value_to_funcall_list(rv) ((list *) (((char *)(rv))-1))

/* -------------------------------------------------------------------
                             RHS Actions

   Fields in an action:
 
      next:  points to the next action in a singly-linked list of all
        actions in the RHS.

      type:  indicates the type of action:  usually this is MAKE_ACTION,
        but for function calls it is FUNCALL_ACTION.

      preference_type:  for make actions, this indicates the type of the
        preference being created:  ACCEPTABLE_PREFERENCE_TYPE, etc.

      support:  indicates the compile-time calculated o-support of the action.
        This is either UNKNOWN_SUPPORT, O_SUPPORT, or NO_O_SUPPORT.
  
      already_in_tc:  (reserved for use by compile-time o-support calcs)

      id, attr:  for make actions, these give the symbols for the id and
        attribute fields of the preference.

      value:  for MAKE_ACTION's, this gives the value field of the preference
        (a symbol or function call).  For FUNCALL_ACTION's, this holds the
        function call itself.
  
      referent:  for MAKE_ACTION's of binary preferences, this gives the
        referent field of the preference.
------------------------------------------------------------------- */

#define MAKE_ACTION 0
#define FUNCALL_ACTION 1

#define UNKNOWN_SUPPORT 0
#define O_SUPPORT 1
#define NO_O_SUPPORT 2

typedef struct action_struct {
  struct action_struct *next;
  byte type;
  byte preference_type;
  byte support;
  bool already_in_tc;  /* used only by compile-time o-support calcs */
  Symbol *id;
  Symbol *attr;
  rhs_value value;   /* for FUNCALL_ACTION's, this holds the funcall */
  rhs_value referent;
} action;

/* -------------------------------------------------------------------
                            Productions

   Fields in a production:
 
      name:  points to the name of the production (a symbol)

      documentation:  points to a string (a memory_block_for_string) giving
        user-provided documentation about the production, or NIL if the
        user didn't give any documentation for it.
    
      reference_count:  (see below)

      firing_count:  the number of times this production has ever fired
        since it was created.  (Note that this is not reset by an init-soar.)

      next, prev:  used for a doubly-linked list of productions of the same
        type (see below).  The list header is all_productions_of_type[].

      type: the type of the production:  USER_PRODUCTION_TYPE,
        DEFAULT_PRODUCTION_TYPE, CHUNK_PRODUCTION_TYPE, or
        JUSTIFICATION_PRODUCTION_TYPE.

      declared_support:  indicates whether the production was declared
        :o-support or :no-o-support.  This field is either UNDECLARED_SUPPORT,
        DECLARED_O_SUPPORT, or DECLARED_NO_O_SUPPORT.

      trace_firings:  TRUE iff a (ptrace) has been set on this production.

      p_node:  If the production is currently in the Rete, this points to
        the corresponding p_node in the Rete.  If the production is not in
        the Rete, this field is NIL.

      action_list:  singly-linked list of the RHS actions of the production.

      rhs_unbound_variables:  A (consed) list of variables used on the RHS
        that aren't bound on the LHS.  These variables need to be instantiated
        with gensyms at firing time.

      instantiations:  header for a doubly-linked list of the instantiations
        of this production that are currently in the match set (i.e.,
        Rete-supported).

   Reference counts on productions:
      +1 if it's in production memory (i.e., hasn't been excised)
      +1 for each existing instantiation pointing to it
   We deallocate a production if its reference_count goes to 0.
------------------------------------------------------------------- */

#define UNDECLARED_SUPPORT 0
#define DECLARED_O_SUPPORT 1
#define DECLARED_NO_O_SUPPORT 2

typedef struct production_struct {
  Symbol *name;
  char *documentation;                    /* pointer to memory block, or NIL */
  unsigned long reference_count;
  unsigned long firing_count;             /* how many times it's fired */
  struct production_struct *next, *prev;  /* used for dll */
  byte type;
  byte declared_support;
  bool trace_firings;                     /* used by ptrace */
  struct rete_node_struct *p_node;        /* NIL if it's not in the rete */
  action *action_list;                    /* RHS actions */
  list *rhs_unbound_variables;            /* RHS vars not bound on LHS */
  struct instantiation_struct *instantiations; /* dll of inst's in MS */
} production;

/* -------------------------------------------------------------------
                        Instantiations and Nots

   Instantiations record three main things:
     (1) the instantiated LHS of the production,
     (2) any "<>" tests that are between identifiers and that occur in
         top-level positive conditions on the LHS, and
     (3) the still-existing preferences that were generated by the RHS.

   Fields in an instantiation:
 
      prod:  points to the production.  (Note: this can also be NIL, for
        fake instantiations used for goal ^item augmentations.  See decide.c.)

      next, prev:  used for a doubly-linked list of instantiations of this
        production that are still in the match set.

      rete_token, rete_wme:  these fields are reserved for use by the Rete.
        (The Rete needs them to find the proper instantiation to retract
        when a token is delted from a p_node.)

      top_of_instantiated_conditions, bottom_of_instantiated_conditions:
        point to the top and bottom of the instantiated LHS conditions.

      nots:  header of a singly-linked list of Nots from the LHS.

      preferences_generated:  header for a doubly-linked list of existing
        preferences that were created by this instantiation.

      match_goal:  points to the match goal of the instantiation, or NIL
        if there is none.

      match_goal_level:  goal stack level of the match goal, or
        ATTRIBUTE_IMPASSE_LEVEL if there is no match goal.

      okay_to_variablize:  TRUE iff it's okay to variablize a
        chunk/justification formed by backtracing through this instantiation.
        This is used to make sure we don't variablize a chunk that got formed
        by backtracing through some other justification.

      in_ms:  TRUE iff this instantiation is still in the match set (i.e.,
        Rete-supported).

      backtrace_number:  used by the chunker to avoid backtracing through
        the same instantiation twice during the building of the same chunk.

   Reference counts on instantiations:
      +1 if it's in the match set
      +1 for each preference it created that's still around
   The reference count is kept implicitly using the preferences_generated
   and in_ms fields.  We deallocate an instantiation if its reference count
   goes to 0.
------------------------------------------------------------------- */

typedef struct not_struct {
  struct not_struct *next;  /* next Not in the singly-linked list */
  Symbol *s1;               /* the two identifiers constrained to be "<>" */
  Symbol *s2;
} not;

typedef struct instantiation_struct {
  production *prod;
  struct instantiation_struct *next, *prev; /* dll of inst's from same prod */
  struct token_struct *rete_token;       /* used by Rete for retractions */
  wme *rete_wme;                         /* ditto */
  condition *top_of_instantiated_conditions;
  condition *bottom_of_instantiated_conditions;
  not *nots;
  preference *preferences_generated;    /* header for dll of prefs */
  Symbol *match_goal;                   /* symbol, or NIL if none */
  goal_stack_level match_goal_level;    /* level, or ATTRIBUTE_IMPASSE_LEVEL */
  byte okay_to_variablize;
  bool in_ms;  /* TRUE iff this inst. is still in the match set */
  tc_number backtrace_number;
} instantiation;

/* ====================================================================
             Global System Parameters and Related Definitions

   A set of system parameters (sysparam's for short) affect many operations
   of Soar, including learning, tracing, deciding, etc.  In order to
   provide a simple, uniform hook mechanism (a single hook routine that
   gets called when any parameter changes), we store these parameters in
   an array sysparams[].  Below, we #define various indices into this array
   corresponding to various system parameters.

   Most of the parameters are of type "long".  A few parameters are more
   naturally handled as lists; for these, the array value is just a dummy,
   and hook routines must inspect a global variable to get the real value.
   Chunk_free_problem_spaces is an example of this.

   The array of sysparams[] can be read directly, but should be modified
   ONLY via calls to set_sysparam(), which is defined in main.c.
==================================================================== */

/* -------------------------------
      Types of Productions
------------------------------- */

#define USER_PRODUCTION_TYPE 0
#define DEFAULT_PRODUCTION_TYPE 1
#define CHUNK_PRODUCTION_TYPE 2
#define JUSTIFICATION_PRODUCTION_TYPE 3

#define NUM_PRODUCTION_TYPES 4

/* ---------------------------------------
    Match Set print parameters
--------------------------------------- */

#define MS_ASSERT_RETRACT 0      /* print both retractions and assertions */
#define MS_ASSERT         1      /* print just assertions */
#define MS_RETRACT        2      /* print just retractions */

typedef byte ms_trace_type;   /* must be one of the above constants */

/* ---------------------------------------
    How much information to print about
    the wmes matching an instantiation
--------------------------------------- */

#define NONE_WME_TRACE    1      /* don't print anything */
#define TIMETAG_WME_TRACE 2      /* print just timetag */
#define FULL_WME_TRACE    3      /* print whole wme */
#define NO_WME_TRACE_SET  4

typedef byte wme_trace_type;   /* must be one of the above constants */

/* -------------------------------
      Ways to Do User-Select
------------------------------- */

#define USER_SELECT_FIRST  0     /* just choose the first candidate item */
#define USER_SELECT_ASK    1     /* ask the user */
#define USER_SELECT_RANDOM 2     /* pick one at random */
#define USER_SELECT_LAST   3     /* choose the last item   AGR 615 */

/* ---------------------------
   And now, the sysparam's
--------------------------- */

/* ====== Sysparams for what to trace === */

#define TRACE_CONTEXT_DECISIONS_SYSPARAM          1
#define TRACE_PHASES_SYSPARAM                     2

/* --- Warning: these next four MUST be consecutive and in the order of the
   production types defined above --- */
#define TRACE_FIRINGS_OF_USER_PRODS_SYSPARAM      3
#define TRACE_FIRINGS_OF_DEFAULT_PRODS_SYSPARAM   4
#define TRACE_FIRINGS_OF_CHUNKS_SYSPARAM          5
#define TRACE_FIRINGS_OF_JUSTIFICATIONS_SYSPARAM  6

#define TRACE_FIRINGS_WME_TRACE_TYPE_SYSPARAM     7
#define TRACE_FIRINGS_PREFERENCES_SYSPARAM        8
#define TRACE_WM_CHANGES_SYSPARAM                 9
#define TRACE_CHUNK_NAMES_SYSPARAM               10
#define TRACE_JUSTIFICATION_NAMES_SYSPARAM       11
#define TRACE_CHUNKS_SYSPARAM                    12
#define TRACE_JUSTIFICATIONS_SYSPARAM            13
#define TRACE_BACKTRACING_SYSPARAM               14

/* ====== Max Elaborations === */
#define MAX_ELABORATIONS_SYSPARAM                15

/* ====== Max Chunks === */
#define MAX_CHUNKS_SYSPARAM                      16

#define RESPOND_TO_LOAD_ERRORS_SYSPARAM          17

/* ====== Sysparams for control of learning === */
#define LEARNING_ON_SYSPARAM                     18
#define LEARNING_SPECIFY_SYSPARAM                19
#define LEARNING_ALL_GOALS_SYSPARAM              20

/* ====== User Select === */
#define USER_SELECT_MODE_SYSPARAM                21

/* ====== Print Warnings === */
#define PRINT_WARNINGS_SYSPARAM                  22

/* AGR 627 begin */
/* ====== Whether to print out aliases as they're defined === */
#define PRINT_ALIAS_SYSPARAM                     23
/* AGR 627 end */

/* --- Warning: if you add sysparams, be sure to update the next line! --- */
#define HIGHEST_SYSPARAM_NUMBER                  23

/* -----------------------------------------
   Sysparams[] stores the parameters;
   set_sysparam() should be used to modify
   them.
----------------------------------------- */

extern void set_sysparam (int param_number, long new_value);

/* ========================================================================
                                main.c
======================================================================== */

/* ---------------------------------------------------------------------
                            Exiting Soar

   Exit_soar() and abort_with_fatal_error() both terminate Soar, closing
   the log file before exiting.  Abort_with_fatal_error() also prints
   an error message before exiting.
--------------------------------------------------------------------- */

extern void exit_soar (void);
extern void abort_with_fatal_error (void);

/* ---------------------------------------------------------------------
                       Timer Utility Routines

   These are utility routines for using timers.  We use (struct timeval)'s
   (defined in a system include file) for keeping track of the cumulative
   time spent in one part of the system or another.  Reset_timer()
   clears a timer to 0.  Start_timer() and stop_timer() are used for
   timing an interval of code--the usage is:
   
     start_timer (&timeval_to_record_the_start_time_in); 
     ... other code here ...
     stop_timer (&timeval_to_record_the_start_time_in,
                 &timeval_holding_accumulated_time_for_this_code);

   Finally, timer_value() returns the accumulated value of a timer
   (in seconds).
--------------------------------------------------------------------- */

extern void reset_timer (struct timeval *tv_to_reset);
extern void start_timer (struct timeval *tv_for_recording_start_time);
extern void stop_timer (struct timeval *tv_with_recorded_start_time,
                        struct timeval *tv_with_accumulated_time);
extern double timer_value (struct timeval *tv);

/* ---------------------------------------------------------------------
                     Adding and Removing Ptraces

   Productions_being_traced is a (consed) list of all productions
   on which a ptrace has been set.  Ptraces are added/removed via
   calls to add_ptrace() and remove_ptrace().
--------------------------------------------------------------------- */

extern void add_ptrace (struct production_struct *prod);
extern void remove_ptrace (struct production_struct *prod);

/* ---------------------------------------------------------------------
   
                        Global Variables, Etc.

--------------------------------------------------------------------- */

/* --- list of symbols (problem space names) declared chunk-free --- */

/* --- during firing, this points to the production being fired --- */

/* --- to interrupt at the end of the current phase, set stop_soar to TRUE
   and reason_for_stopping to some appropriate string --- */

/* --- current top level phase --- */

enum top_level_phase { INPUT_PHASE, PREFERENCE_PHASE, WM_PHASE,
                       OUTPUT_PHASE, QUIESCENCE_PHASE };

/* --- the RHS action (halt) sets this TRUE --- */

/* --- accumulated cpu time spent in various parts of the system --- */

/* ---------------------------------------------------------------------
                         Reinitializing Soar

   Reinitialize_soar() does all the work for an init-soar.
--------------------------------------------------------------------- */

extern void reinitialize_soar (void);

/* ---------------------------------------------------------------------
                            Running Soar

   Each of the following routines runs Soar for a certain duration,
   or until stop_soar gets set to TRUE.
     - Run_forever() runs Soar forever.
     - Run_for_n_phases() runs Soar for a given number (n) of top-level
       phases.  (If n==-1, it runs forever.)
     - Run_for_n_elaboration_cycles() runs Soar for a given number (n)
       of elaboration cycles.  (Here, quiescence phase is counted as
       an elaboration cycle.)  (If n==-1, it runs forever.)
     - Run_for_n_decision_cycles() runs Soar for a given number (n) of
       decision cycles.  (If n==-1, it runs forever.)
     - Run_for_n_selections_of_slot (long n, Symbol *attr_of_slot): this
       runs Soar until the nth time a selection is made for a given
       type of slot.  Attr_of_slot should be either goal_symbol,
       problem_space_symbol, state_symbol, or operator_symbol.
     - Run_for_n_selections_of_slot_at_level (long n, Symbol *attr_of_slot,
       goal_stack_level level):  this runs Soar for n selections of the
       given slot at the given level, or until the goal stack is popped
       so that level no longer exists.
--------------------------------------------------------------------- */

extern void run_forever (void);
extern void run_for_n_phases (long n);
extern void run_for_n_elaboration_cycles (long n);
extern void run_for_n_decision_cycles (long n);
extern void run_for_n_selections_of_slot (long n, Symbol *attr_of_slot);
extern void run_for_n_selections_of_slot_at_level (long n,
                                                   Symbol *attr_of_slot,
                                                   goal_stack_level level);

/* =======================================================================
                                decide.c

   Decide.c contains the decider as well as routine for managing working
   memory, preference memory, slots, and the garbage collection of
   disconnected WMEs.
======================================================================= */

/* ---------------------------------------------------------------------
   Top_goal and bottom_goal point to the top and bottom goal identifiers,
   respectively.  (If there is no goal stack at all, they're both NIL.)
   Top_state points to the top state (symbol) if there is a top state, and
   is NIL of there isn't any top state selected.
--------------------------------------------------------------------- */

/* ---------------------------------------------------------------------
                        Slot Management Routines

   Find_slot() looks for an existing slot for a given id/attr pair, and
   returns it if found.  If no such slot exists, it returns NIL.
   Make_slot() looks for an existing slot for a given id/attr pair,
   returns it if found, and otherwise creates a new slot and returns it.
--------------------------------------------------------------------- */

extern slot *find_slot (Symbol *id, Symbol *attr);
extern slot *make_slot (Symbol *id, Symbol *attr);

/* ---------------------------------------------------------------------
             Working Memory Management and Utility Routines

   Reset_wme_timetags() resets the wme timetag generator back to 1.
   This should be called during an init-soar.

   Make_wme() creates and returns a new wme.  The caller should add the
   wme onto the appropriate dll (e.g., my_slot->wmes) and should call
   add_wme_to_wm() on it.

   Add_wme_to_wm() and remove_wme_from_wm() make changes to WM.  Again,
   the caller is responsible for manipulating the appropriate dll.  WM
   changes don't actually get stuffed down the rete until the end of the
   phase, when do_buffered_wm_and_ownership_changes() gets be called.

   Wme_add_ref() and wme_remove_ref() are macros for incrementing and
   decrementing the reference count on a wme.  Deallocate_wme() deallocates
   a wme; this should only be invoked via the wme_remove_ref() macro.

   Find_name_of_object() is a utility function for finding the value of
   the ^name attribute on a given object (symbol).  It returns the name,
   or NIL if the object has no name.
--------------------------------------------------------------------- */

extern void reset_wme_timetags (void);
extern wme *make_wme (Symbol *id, Symbol *attr, Symbol *value,bool acceptable);
extern void add_wme_to_wm (wme *w);
extern void remove_wme_from_wm (wme *w);

#define wme_add_ref(w) { (w)->reference_count++; }
#define wme_remove_ref(w) { \
  (w)->reference_count--; \
  if ((w)->reference_count == 0) deallocate_wme(w); }

extern void deallocate_wme (wme *w);
extern Symbol *find_name_of_object (Symbol *id);

/* ---------------------------------------------------------------------
                     Preference Management Routines

   Make_preference() creates a new preference structure of the given type
   with the given id/attribute/value/referent.  (Referent is only used
   for binary preferences.)  The preference is not yet added to preference
   memory, however.

   Preference_add_ref() and preference_remove_ref() are macros for
   incrementing and decrementing the reference count on a preference.
   
   Possibly_deallocate_preference_and_clones() checks whether a given
   preference and all its clones have reference_count 0, and deallocates
   them all if they do; it returns TRUE if they were actually deallocated,
   FALSE otherwise.   Deallocate_preference() deallocates a given
   preference.  These routines should normally be invoked only via the
   preference_remove_ref() macro.

   Add_preference_to_tm() adds a given preference to preference memory (and
   hence temporary memory).  Remove_preference_from_tm() removes a given
   preference from PM and TM.

   Process_o_rejects_and_deallocate_them() handles the processing of
   o-supported reject preferences.  This routine is called from the firer
   and passed a list of all the o-rejects generated in the current
   preference phase (the list is linked via the "next" fields on the
   preference structures).  This routine removes all preferences for
   matching values from TM, and deallocates the o-reject preferences when
   done.
--------------------------------------------------------------------- */
  
extern preference *make_preference (byte type, Symbol *id, Symbol *attr,
                                    Symbol *value, Symbol *referent);

#define preference_add_ref(p) { (p)->reference_count++; }
#define preference_remove_ref(p) { \
  (p)->reference_count--; \
  if ((p)->reference_count == 0) \
    possibly_deallocate_preference_and_clones(p); }

extern bool possibly_deallocate_preference_and_clones (preference *pref);
extern void deallocate_preference (preference *pref);

extern void add_preference_to_tm (preference *pref);
extern void remove_preference_from_tm (preference *pref);
extern void process_o_rejects_and_deallocate_them (preference *o_rejects);

/* ---------------------------------------------------------------------
                      Top-Level Decider Routines

   Init_decider() should be called at startup time to initialize this
   module.

   Do_buffered_wm_and_ownership_changes() does the end-of-phase processing
   of WM changes, ownership calculations, garbage collection, etc.

   Do_working_memory_phase() and do_quiescence_phase() are called from
   the top level to run those phases.

   Create_top_goal() creates the top goal in the goal stack.
   Clear_goal_stack() wipes out the whole goal stack--this is called
   during an init-soar.

   Print_lowest_slot_in_context_stack() is used for the watch 0 trace
   to print the context slot that was just decided.
--------------------------------------------------------------------- */

extern void init_decider (void);
extern void do_buffered_wm_and_ownership_changes (void);
extern void do_working_memory_phase (void);
extern void do_quiescence_phase (void);
extern void create_top_goal (void);
extern void clear_goal_stack (void);
extern void print_lowest_slot_in_context_stack (void);

/* ======================================================================
                              parser.c

                     The Production (SP) Parser 

   Init_parser() should be called at startup time.  Parse_production()
   reads an SP (starting from the production name), builds a production,
   adds it to the rete, and returns a pointer to the new production
   (or NIL if any error occurred).
====================================================================== */

extern struct production_struct *parse_production (void);
extern void init_parser (void);

/* ======================================================================
                              print.c                                

    Printing with an Optional Log File and with Redirection to a File

   We want to print stuff not only to the screen but also to a log
   file (if one is currently being used).  The print_string(), print(),
   print_with_symbols(), and print_spaces() routines do this.

   Start_log_file() and stop_log_file() open and close the current log
   file.  Print_string_to_log_file_only() is called by the lexer to
   echo keyboard input to the log file (it's already on the screen, so
   we don't want to print it there too).

   Print_string() and print_spaces() do the obvious things.
   Print() is exactly like printf() in C, except it prints to both
   the screen and log file (if there is one).  Print_with_symbols()
   is sort of like print, but only takes two kinds of escape sequences
   in the format string: 
       %y  -- print a symbol
       %%  -- print a "%" sign

   Sometimes we need to know the current output column so we can put
   a line break in the right place.  Get_printer_output_column() returns
   the current column number (1 means the start of the line). 
   Tell_printer_that_output_column_has_been_reset () is called from the
   lexer every time it reads a line from the keyboard--since after the
   user types a line (and hits return) the output column is reset.

   We also support temporarily redirecting all printing output to
   another file.  This is done by calling start_redirection_to_file()
   and stop_redirection_to_file().  In between these calls, all screen
   and log file output is turned off, and printing is done only to the
   redirection file.
====================================================================== */

extern void start_log_file (char *filename, bool append);
extern void stop_log_file (void);
extern void print_string_to_log_file_only (char *string);

extern int get_printer_output_column (void);
extern void tell_printer_that_output_column_has_been_reset (void);

extern void start_redirection_to_file (FILE *already_opened_file);
extern void stop_redirection_to_file (void);

extern void print_string (char *s);
#ifdef USE_STDARGS
extern void print (char *format, ... );
extern void print_with_symbols (char *format, ...);
#else
extern void print ();
extern void print_with_symbols ();
#endif
extern void print_spaces (int n);

/* ------------------------------------------------------------------------
                String to Escaped String Conversion
           {Symbol, Test, RHS Value} to String Conversion

   These routines produce strings.  Each takes an optional parameter "dest"
   which, if non-nil, points to the destination buffer for the result string.
   If dest is nil, these routines use a global buffer, and return a pointer
   to it.  (Otherwise "dest" itself is returned.)  Note that a single global
   buffer is shared by all three routines, so callers should assume the
   buffer will be destroyed by the next call to these routines with dest=NIL.

   String_to_escaped_string() takes a string and a first/last char,
   and produces an "escaped string" representation of the string; i.e.,
   a string that uses '\' escapes to include special characters.
   For example, input 'ab"c' with first/last character '"' yields
   '"ab\"c"'.  This is used for printing quoted strings and for printing
   symbols using |vbar| notation.
 
   Symbol_to_string() converts a symbol to a string.  The "rereadable"
   parameter indicates whether a rereadable representation is desired.
   Normally symbols are printed rereadably, but for (write) and Text I/O,
   we don't want this.

   Test_to_string() takes a test and produces a string representation.
   Rhs_value_to_string() takes an rhs_value and produces a string
   representation.
----------------------------------------------------------------------- */

extern char *string_to_escaped_string (char *s, char first_and_last_char,
                                       char *dest);
extern char *symbol_to_string (Symbol *sym, bool rereadable, char *dest);
extern char *test_to_string (test t, char *dest);
extern char *rhs_value_to_string (rhs_value rv, char *dest);

/* -----------------------------------------------------------------------
             Print Condition List, Action List, Production

   Print_condition_list() prints a list of conditions.  The "indent"
   parameter tells how many spaces to indent each line other than the
   first--the first line is not indented (the caller must handle this).
   The last line is printed without a trailing linefeed.  The "internal"
   parameter, if TRUE, indicates that the condition list should be printed
   in internal format--one condition per line, without grouping all the
   conditions for the same id into one line.

   Print_action_list() is similar except it prints actions instead of
   conditions.

   Print_production() prints a given production, optionally using internal
   format.
----------------------------------------------------------------------- */

extern void print_condition_list (condition *conds, int indent, bool internal);
extern void print_action_list (action *actions, int indent, bool internal);
extern void print_production (production *p, bool internal);

/* -----------------------------------------------------------------------
                       Other Printing Utilities

   Print_condition() prints a single condition.  Print_action() prints
   a single action.  Note that these routines work by calling
   print_condition_list() and print_action_list(), respectively, so
   they print a linefeed if the output would go past COLUMNS_PER_LINE.

   Preference_type_indicator() returns a character corresponding to
   a given preference type (byte)--for example, given BEST_PREFERENCE_TYPE,
   it returns '>'.

   Print_preference() prints a given preference.  Print_wme() prints a
   wme (including the timetag).  Print_instantiation_with_wmes() prints
   an instantiation's production name and the wmes it matched, using a
   given wme_trace_type (e.g., TIMETAG_WME_TRACE).
----------------------------------------------------------------------- */

extern void print_condition (condition *cond);
extern void print_action (action *a);
extern char preference_type_indicator (byte type);
extern void print_preference (preference *pref);
extern void print_wme (wme *w);
extern void print_instantiation_with_wmes (instantiation *inst,
                                           wme_trace_type wtt);

/* ========================================================================
                               production.c

   Various utility routines for manipulating productions and parts thereof.
   Also includes the reorderer and compile-time o-support calculations.

   Init_production_utilities() should be called before anything else here.
======================================================================== */

/* This structure is used to break ties in favor of non-multi-attributes */
typedef struct multi_attributes_struct {
  Symbol *symbol;
  long value;
  struct multi_attributes_struct *next;
} multi_attribute;

extern void init_production_utilities (void);

/* ------------------------------ */
/* Utilities for lists of symbols */
/* ------------------------------ */

/* --- Takes a list of symbols and returns a copy of the same list,
   incrementing the reference count on each symbol in the list. --- */
extern list *copy_symbol_list_adding_references (list *sym_list);

/* --- Frees a list of symbols, decrementing their reference counts. --- */
extern void deallocate_symbol_list_removing_references (list *sym_list);

/* ------------------- */
/* Utilities for tests */
/* ------------------- */

/* --- Takes a test and returns a new copy of it. --- */
extern test copy_test (test t);

/* --- Same as copy_test(), only it doesn't include goal or impasse tests
   in the new copy.  The caller should initialize the two flags to FALSE
   before calling this routine; it sets them to TRUE if it finds a goal
   or impasse test. --- */
extern test copy_test_removing_goal_impasse_tests
  (test t, bool *removed_goal, bool *removed_impasse);

/* --- Deallocates a test. --- */
extern void deallocate_test (test t);

/* --- Destructively modifies the first test (t) by adding the second
   one (add_me) to it (usually as a new conjunct).  The first test
   need not be a conjunctive test. --- */
extern void add_new_test_to_test (test *t, test add_me); 

/* --- Same as above, only has no effect if the second test is already
   included in the first one. --- */
extern void add_new_test_to_test_if_not_already_there (test *t, test add_me);

/* --- Returns TRUE iff the two tests are identical. --- */
extern bool tests_are_equal (test t1, test t2);

/* --- Returns a hash value for the given test. --- */
extern unsigned long hash_test (test t);

/* --- Returns TRUE iff the test contains an equality test for the given
   symbol.  If sym==NIL, returns TRUE iff the test contains any equality
   test. --- */
extern bool test_includes_equality_test_for_symbol (test t, Symbol *sym);

/* --- Looks for goal or impasse tests (as directed by the two flag
   parameters) in the given test, and returns TRUE if one is found. --- */
extern bool test_includes_goal_or_impasse_id_test (test t,
                                                   bool look_for_goal,
                                                   bool look_for_impasse);

/* --- Looks through a test, and returns a new copy of the first equality
   test it finds.  Signals an error if there is no equality test in the
   given test. --- */
extern test copy_of_equality_test_found_in_test (test t);

/* ------------------------ */
/* Utilities for conditions */
/* ------------------------ */

/* --- Deallocates a condition list (including any NCC's and tests in it). */
extern void deallocate_condition_list (condition *cond_list);

/* --- Returns a new copy of the given condition. --- */
extern condition *copy_condition (condition *cond);

/* --- Copies the given condition list, returning pointers to the
   top-most and bottom-most conditions in the new copy. --- */
extern void copy_condition_list (condition *top_cond, condition **dest_top,
                                 condition **dest_bottom);

/* --- Returns TRUE iff the two conditions are identical. --- */
extern bool conditions_are_equal (condition *c1, condition *c2);

/* --- Returns a hash value for the given condition. --- */
extern unsigned long hash_condition (condition *cond);

/* ------------------------------------ */
/* Utilities for actions and RHS values */
/* ------------------------------------ */

/* --- Deallocates the given rhs_value. --- */
extern void deallocate_rhs_value (rhs_value rv);

/* --- Returns a new copy of the given rhs_value. --- */
extern rhs_value copy_rhs_value (rhs_value rv);

/* --- Deallocates the given action (singly-linked) list. --- */
extern void deallocate_action_list (action *actions);

/* ------------------ */
/* Utilities for nots */
/* ------------------ */

/* --- Deallocates the given (singly-linked) list of Nots. --- */
extern void deallocate_list_of_nots (not *nots);

/* --------------------------------------------------------------------
                      Transitive Closure Utilities

   Get_new_tc_number() is called from lots of places.  Any time we need
   to mark a set of identifiers and/or variables, we get a new tc_number
   by calling this routine, then proceed to mark various ids or vars
   by setting the sym->id.tc_num or sym->var.tc_num fields.

   Sometimes in addition to marking symbols using their tc_num fields,
   we also want to build up a list of the symbols we've marked.  So,
   the routines here take an "id_list" or "var_list" argument.  This
   argument should be NIL if no such list is desired.  If non-NIL, it
   should point to the header of the linked list being built.

       Transitive Closure Calculations for Conditions and Actions

   Usage: 
     1. Set my_tc = get_new_tc_number() to start a new TC
     2. (optional) If you want linked lists of symbols in the TC, initialize
        id_list=NIL and var_list=NIL.
        If you're not using id_list and/or var_list, give NIL for "&id_list"
        and/or "&var_list" in the function calls below.
     3. (optional) setup any id's or var's that you want to include in the
        initial TC, by calling 
           add_symbol_to_tc (sym, my_tc, &id_list, &var_list)
        (If not using id_list or var_list, you can just mark
         sym->{id,var}.tc_num = my_tc instead.)
     4. To do the work you want, use any of the following any number of times:
            add_cond_to_tc (cond, my_tc, &id_list, &var_list);
            add_action_to_tc (cond, my_tc, &id_list, &var_list);
            result = cond_is_in_tc (cond, my_tc);
            result = action_is_in_tc (action, my_tc);
     5. When finished, free the cons cells in id_list and var_list (but
        don't call symbol_remove_ref() on the symbols in them).
-------------------------------------------------------------------- */

tc_number get_new_tc_number (void);

extern void add_symbol_to_tc (Symbol *sym, tc_number tc,
                              list **id_list, list **var_list);
extern void add_cond_to_tc (condition *c, tc_number tc,
                            list **id_list, list **var_list);
extern void add_action_to_tc (action *a, tc_number tc,
                              list **id_list, list **var_list);
extern bool cond_is_in_tc (condition *cond, tc_number tc);
extern bool action_is_in_tc (action *a, tc_number tc);

/* --------------------------------------------------------------------
                         Variable Generator

   These routines are used for generating new variables.  The variables
   aren't necessarily "completely" new--they might occur in some existing
   production.  But we usually need to make sure the new variables don't
   overlap with those already used in a *certain* production--for instance,
   when variablizing a chunk, we don't want to introduce a new variable that
   conincides with the name of a variable already in an NCC in the chunk.
   
   To use these routines, first call reset_variable_generator(), giving
   it lists of conditions and actions whose variables should not be
   used.  Then call generate_new_variable() any number of times; each
   time, you give it a string to use as the prefix for the new variable's
   name.  The prefix string should not include the opening "<".
-------------------------------------------------------------------- */

extern void reset_variable_generator (condition *conds_with_vars_to_avoid,
                                      action *actions_with_vars_to_avoid);
extern Symbol *generate_new_variable (char *prefix);

/* -------------------------------------------------------------------
                         Production Management
 
    For each type of production, we maintain a doubly-linked list of
    all productions of that type.  The headers of these dll's are
    stored in the arry all_productions_of_type[].  Another array,
    num_productions_of_type[], keeps counts of how many productions
    there are of each type.

    Production_add_ref() and production_remove_ref() are macros for
    incrementing and decrementing the reference count on a production.
    Production_remove_ref() also deallocates the production if the
    count goes to 0.
    
    Make_production() does reordering, compile-time o-support calc's,
    and builds and returns a production structure for a new production.
    It does not enter the production into the Rete net, however.
    The "type" argument should be one of USER_PRODUCTION_TYPE, etc.
    The flag "reorder_nccs" tells whether to recursively reorder
    the subconditions of NCC's--this is not necessary for newly
    built chunks, as their NCC's are copies of other NCC's in SP's that
    have already been reordered.  If any error occurs, make_production()
    returns NIL.

    Deallocate_production() and excise_production() do just what they
    say.  Normally deallocate_production() should be invoked only via
    the production_remove_ref() macro.
------------------------------------------------------------------- */

#define production_add_ref(p) { (p)->reference_count++; }
#define production_remove_ref(p) { \
  (p)->reference_count--; \
  if ((p)->reference_count == 0) \
    deallocate_production(p); }

extern production *make_production (byte type,
                                    Symbol *name,
                                    condition **lhs_top,
                                    condition **lhs_bottom,
                                    action **rhs_top,
                                    bool reorder_nccs);
extern void deallocate_production (production *prod);
extern void excise_production (production *prod, bool print_sharp_sign);

extern bool canonical_cond_greater(condition *c1, condition *c2);

/* =======================================================================
                                recmem.c

   Init_firer() and init_chunker() should be called at startup time, to
   do initialization.

   Do_preference_phase() runs the entire preference phase.  This is called
   from the top-level control in main.c.

   Possibly_deallocate_instantiation() checks whether an instantiation
   can be deallocated yet, and does so if possible.  This is used whenever
   the (implicit) reference count on the instantiation decreases.
======================================================================= */

extern void init_firer (void);
extern void do_preference_phase (void);
extern preference *find_clone_for_level (preference *, goal_stack_level);

/* mvp 5-17-94 */
extern void build_prohibits_list (instantiation *inst);

#define possibly_deallocate_instantiation(inst) { \
  if ((! (inst)->preferences_generated) && \
      (! (inst)->in_ms)) \
    deallocate_instantiation (inst); }

extern void deallocate_instantiation (instantiation *inst);

extern void init_chunker (void);

/* =======================================================================
                                 rete.c

   All_wmes_in_rete is the header for a dll of all the wmes currently
   in the rete.  (This is normally equal to all of WM, except at times
   when WM changes have been buffered but not yet done.)  The wmes
   are linked via their "rete_next" and "rete_prev" fields.
   Num_wmes_in_rete counts how many wmes there are in the rete.

   Init_rete() initializes the rete.  It should be called at startup time.

   Any_assertions_or_retractions_ready() returns TRUE iff there are any
   pending changes to the match set.  This is used to test for quiescence.
   Get_next_assertion() retrieves a pending assertion (returning TRUE) or
   returns FALSE is no more are available.  Get_next_retraction() is
   similar.

   Add_production_to_rete() adds a given production, with a given LHS,
   to the rete.  If "refracted_inst" is non-NIL, it should point to an
   initial instantiation of the production.  This routine returns one
   of NO_REFRACTED_INST, REFRACTED_INST_MATCHED, etc. (see below).
   Excise_production_from_rete() removes the given production from the
   rete, and enqueues all its existing instantiations as pending
   retractions.

   Add_wme_to_rete() and remove_wme_from_rete() inform the rete of changes
   to WM.

   P_node_to_conditions_and_nots() takes a p_node and (optionally) a
   token/wme pair, and reconstructs the (optionally instantiated) LHS
   for the production.

   Print_partial_match_information(), print_match_set(), and
   print_rete_statistics() do printouts for various interface routines.
======================================================================= */

typedef struct match_set_trace {
	Symbol *sym;
	int	count;
	struct match_set_trace *next;
} MS_trace;

extern void init_rete (void);

extern bool any_assertions_or_retractions_ready (void);
extern bool get_next_assertion (production **prod,
                                struct token_struct **tok,
                                wme **w);
extern bool get_next_retraction (struct instantiation_struct **inst);

#define NO_REFRACTED_INST 0              /* no refracted inst. was given */
#define REFRACTED_INST_MATCHED 1         /* there was a match for the inst. */
#define REFRACTED_INST_DID_NOT_MATCH 2   /* there was no match for it */
#define DUPLICATE_PRODUCTION 3           /* the prod. was a duplicate */
extern byte add_production_to_rete (production *p, condition *lhs_top,
                                    instantiation *refracted_inst,
                                    bool warn_on_duplicates);
extern void excise_production_from_rete (production *p);

extern void add_wme_to_rete (wme *w);
extern void remove_wme_from_rete (wme *w);

extern void p_node_to_conditions_and_nots (struct rete_node_struct *p_node,
                                           struct token_struct *tok,
                                           wme *w,
                                           condition **dest_top_cond,
                                           condition **dest_bottom_cond,
                                           not **dest_nots);

extern void print_partial_match_information (struct rete_node_struct *p_node,
                                             wme_trace_type wtt);

extern void print_match_set (wme_trace_type wtt, ms_trace_type  mst);
extern void print_rete_statistics (void);

/* ====================================================================
                             rhsfun.c

   The system maintains a list of available RHS functions.  Functions
   can appear on the RHS of productions either as values (in make actions
   or as arguments to other function calls) or as stand-alone actions
   (e.g., "write" and "halt").  When a function is executed, its C code
   is called with one parameter--a (consed) list of the arguments (symbols).
   The C function should return either a symbol (if all goes well) or NIL
   (if an error occurred, or if the function is a stand-alone action).

   All available RHS functions should be setup at system startup time via
   calls to add_rhs_function().  It takes as arguments the name of the
   function (a symbol), a pointer to the corresponding C function, the
   number of arguments the function expects (-1 if the function can take
   any number of arguments), and flags indicating whether the function can
   be a RHS value or a stand-alone action.

   Lookup_rhs_function() takes a symbol and returns the corresponding
   rhs_function structure (or NIL if there is no such function).

   Init_built_in_rhs_functions() should be called at system startup time
   to setup all the built-in functions.
==================================================================== */

typedef Symbol * ((*rhs_function_routine)(list *args));

typedef struct rhs_function_struct {
  struct rhs_function_struct *next;
  Symbol *name;
  rhs_function_routine f;
  int num_args_expected;     /* -1 means it can take any number of args */
  bool can_be_rhs_value;
  bool can_be_stand_alone_action;
} rhs_function;

extern void add_rhs_function (Symbol *name,
                              rhs_function_routine f,
                              int num_args_expected,
                              bool can_be_rhs_value,
                              bool can_be_stand_alone_action);
extern rhs_function *lookup_rhs_function (Symbol *name);
extern void init_built_in_rhs_functions (void);

/* ======================================================================
                                trace.c

   Object and stack trace formats are managed by this module.

   Init_tracing() initializes the tables; at this point, there are no trace
   formats for anything.  This routine should be called at startup time.

   Trace formats are changed by calls to add_trace_format() and
   remove_trace_format().  Add_trace_format() returns TRUE if the
   format was successfully added, or FALSE if the format string didn't
   parse right.  Remove_trace_format() returns TRUE if a trace format
   was actually removed, or FALSE if there was no such trace format for
   the given type/name restrictions.  These routines take a "stack_trace"
   argument, which should be TRUE if the stack trace format is intended,
   or FALSE if the object trace format is intended.  Their
   "type_restriction" argument should be one of FOR_ANYTHING_TF, ...,
   FOR_OPERATORS_TF.  The "name_restriction" argument should be either
   a pointer to a symbol, if the trace format is  restricted to apply
   to objects with that name, or NIL if the format can apply to any object.
   
   Print_all_trace_formats() prints out either all existing stack trace
   or object trace formats.

   Print_object_trace() takes an object (any symbol).  It prints the
   trace for that object.  Print_stack_trace() takes a (context)
   object (the g, ps, s, or op), the current goal, the "slot_type"
   (one of FOR_OPERATORS_TF, etc.), and a flag indicating whether to
   allow %dc and %ec escapes (this flag should normally be TRUE for
   watch 0 traces but FALSE during a "pgs" command).  It prints the
   stack trace for that context object.
====================================================================== */

/* trace format type restrictions */
#define FOR_ANYTHING_TF 0          /* format applies to any object */
#define FOR_GOALS_TF 1             /* format applies only to goals */
#define FOR_PROBLEM_SPACES_TF 2    /* ... etc. ... */
#define FOR_STATES_TF 3
#define FOR_OPERATORS_TF 4

extern void init_tracing (void);
extern bool add_trace_format (bool stack_trace, int type_restriction,
                              Symbol *name_restriction, char *format_string);
extern bool remove_trace_format (bool stack_trace, int type_restriction,
                                 Symbol *name_restriction);
extern void print_all_trace_formats (bool stack_trace);
extern void print_object_trace (Symbol *object);
extern void print_stack_trace (Symbol *object, Symbol *goal, int slot_type,
                               bool allow_cycle_counts);

extern char * help_on_trace_format_escapes[];

/* =======================================================================
                                 io.c

         General Soar I/O System Routines, and Text I/O Routines

   User-defined Soar I/O routines should be added at system startup time
   via calls to add_input_function() and add_output_function().  These 
   calls add things to the system's list of (1) functions to be called 
   every input cycle, and (2) symbol-to-function mappings for output
   commands.  File io.c contains the system I/O mechanism itself (i.e.,
   the stuff that calls the input and output functions), plus the text
   I/O routines.

   Init_soar_io() and init_text_io() do what they say.  Do_input_cycle()
   and do_output_cycle() perform the entire input and output cycles--
   these routines are called once per elaboration cycle.  The output module
   is notified about WM changes via a call to
   inform_output_module_of_wm_changes().

   The text I/O system maintains a mapping from symbols to streams (i.e.,
   file descriptors).  This is used to translate from the value of a
   text-command ^text-input-stream or ^text-output-stream augmentation
   to the file descriptor to use for the I/O.  The routines
   add_text_io_symbol_to_file_mapping() and
   remove_text_io_symbol_to_file_mapping() should be called to set up
   and remove this mapping.  The trace_io argument, if TRUE, causes all
   the I/O done with the stream to be echoed to the screen.
======================================================================= */

extern void init_soar_io (void);
extern void init_text_io (void);
extern void do_input_cycle (void);
extern void do_output_cycle (void);

extern void inform_output_module_of_wm_changes (list *wmes_being_added,
                                                list *wmes_being_removed);

extern void add_text_io_symbol_to_file_mapping (Symbol *sym,
                                                int fd,
                                                bool trace_io);
extern void remove_text_io_symbol_to_file_mapping (Symbol *sym);

/* =======================================================================
                            Input Functions
 
   Input functions take one parameter--a mode (integer) indicating why the 
   function is being called.  The mode is either TOP_STATE_JUST_CREATED, 
   NORMAL_INPUT_CYCLE, or TOP_STATE_JUST_REMOVED.  In the input cycle
   immediately following the installation of the top state, each input
   function is called once with TOP_STATE_JUST_CREATED and then once with
   NORMAL_INPUT_CYCLE.  In the input cycle immediately following the removal
   of the top state, the functions are called with TOP_STATE_JUST_REMOVED.
   If the top state is *replaced*, the functions are called with 
   TOP_STATE_JUST_REMOVED, then TOP_STATE_JUST_CREATED, and then
   NORMAL_INPUT_CYCLE.

   Input routines create, modify, and delete input structures via calls
   to add_input_wme() and remove_input_wme().  The arguments to add_input_wme()
   indicate the id/attr/value components of the wme to be added.  Each of
   these components must be either (1) the current value of the global
   variable "top_state", or (2) the returned value from a call to 
   get_new_io_identifier(), get_io_sym_constant(), get_io_int_constant(),
   or get_io_float_constant().  [The idea behind creating the components this
   way is to avoid having I/O functions deal with the reference counts on
   symbols.]  For every call an I/O function makes to get_xxx(), it should
   later call release_io_symbol().  Release_io_symbol() should *not* be
   called with the value of "top_state"--*only* the components obtained via
   get_xxx().
   
   The add_input_wme() routine returns a pointer to the wme added.  The input
   routine shouldn't use this pointer in any way except to save it around for
   a later call to remove_input_wme().  Example:

         float current_sensor_value;
         wme *w;
         Symbol *s1,*s2;
         ... insert code to read value into current_sensor_value here ...
         s1 = get_io_sym_constant ("sensor-value");
         s2 = get_io_float_constant (current_sensor_value);
         ... add to working memory (S1 ^sensor-value 37.5) ...
         w = add_input_wme (top_state, s1, s2);
         release_io_symbol (s1);
         release_io_symbol (s2);
   
   On some later call, the input function might call remove_input_wme (w)
   to remove (S1 ^sensor-value 37.5) from working memory.

   To remove an entire input structure, it is sufficient for the input
   function to call remove_input_wme() on just the top link wme.  The input
   function need not call remove_input_wme() on each and every wme in the
   structure.  (Soar automagically garbage collects all the wmes in the
   now-disconnected structure.)  Note that when an input function is called
   with TOP_STATE_JUST_REMOVED, all existing input structures have already
   been garbage collected (since the top state no longer exists), so the 
   input function should never call remove_input_wme() when mode is
   TOP_STATE_JUST_REMOVED.  Remove_input_wme() normally returns TRUE,
   indicating success.  It returns FALSE if an error occurs (e.g., if the
   wme argument isn't in WM).
======================================================================= */

#define TOP_STATE_JUST_CREATED 1
#define NORMAL_INPUT_CYCLE 2
#define TOP_STATE_JUST_REMOVED 3

typedef void (*input_function)(int mode);

extern void add_input_function (input_function f);

extern Symbol *get_new_io_identifier (char first_letter);
extern Symbol *get_io_sym_constant (char *name);
extern Symbol *get_io_int_constant (long value);
extern Symbol *get_io_float_constant (float value);
extern void release_io_symbol (Symbol *sym);

extern wme *add_input_wme (Symbol *id, Symbol *attr, Symbol *value);
extern bool remove_input_wme (wme *w);

/* =======================================================================
                            Output Functions
 
   Output functions take two parameters--a mode (integer) indicating why the 
   function is being called, and a pointer to a chain of io_wme structures.
   The mode is either ADDED_OUTPUT_COMMAND (used when an output link is first
   created), MODIFIED_OUTPUT_COMMAND (used when the transitive closure of an
   existing link changes), or REMOVED_OUTPUT_COMMAND (used when the output
   link is removed from working memory).

   The chain of io_wme structures is connected via the "next" fields in the
   structures; for the last io_wme, next==NIL.  When mode is either
   ADDED_OUTPUT_COMMAND or MODIFIED_OUTPUT_COMMAND, this chain contains
   all the wmes in the current transitive closure of the output link
   (including the output link wme itself).  When mode is
   REMOVED_OUTPUT_COMMAND, the chain consists of just one io_wme--the top-level
   ouput link being removed.
  
   Output functions should inspect the io_wme chain and take whatever
   actions are appropriate.  Note that Soar deallocates the io_wme chain
   after calling the output function, so the output function is responsible
   for saving any necessary information around for later.
   
   How can an output function examine the io_wme's?  The io_wme structures
   indicate the id/attr/value of the wmes in the output structure.  See
   the comments above for symtab.c for an explanation of the structure
   of these symbols.

   Get_output_value() is a simple utility routine for finding things in
   an io_wme chain.  It takes "outputs" (the io_wme chain), and "id" and
   "attr" (symbols to match against the wmes), and returns the value from
   the first wme in the chain with a matching id and attribute.  Either
   "id" or "attr" (or both) can be specified as "don't care" by giving
   NULL (0) pointers for them instead of pointers to symbols.  If no matching
   wme is found, the function returns a NULL pointer.
======================================================================= */

typedef struct io_wme_struct {
  struct io_wme_struct *next;  /* points to next io_wme in the chain */
  Symbol *id;                  /* id, attribute, and value of the wme */
  Symbol *attr;
  Symbol *value;
} io_wme;

#define ADDED_OUTPUT_COMMAND 1
#define MODIFIED_OUTPUT_COMMAND 2
#define REMOVED_OUTPUT_COMMAND 3

typedef void (*output_function)(int mode, io_wme *outputs);

extern void add_output_function (char *output_link_name, output_function f);

extern Symbol *get_output_value (io_wme *outputs, Symbol *id, Symbol *attr);

/* =======================================================================
                               hooks.c

   Hooks.c contains a bunch of "hook routines" that users can add their
   own code to.  For more details, see the descriptions of the individual
   routines, in hooks.c.
======================================================================= */

extern void system_startup_hook (void);
extern void system_termination_hook (bool normal_exit);
extern void after_init_agent_hook (void);
extern void before_init_soar_hook (void);
extern void after_init_soar_hook (void);
extern void after_halt_soar_hook (void);
extern void before_decision_cycle_hook (void);
extern void after_decision_cycle_hook (void);
extern void before_input_phase_hook (void);
extern void after_input_phase_hook (void);
extern void before_preference_phase_hook (void);
extern void after_preference_phase_hook (void);
extern void before_wm_phase_hook (void);
extern void after_wm_phase_hook (void);
extern void before_output_phase_hook (void);
extern void after_output_phase_hook (void);
extern void before_quiescence_phase_hook (void);
extern void after_quiescence_phase_hook (void);
extern void wm_changes_hook (list *wmes_being_added, list *wmes_being_removed);
extern void create_new_context_hook (Symbol *new_goal);
extern void pop_context_stack_hook (Symbol *goal_about_to_be_removed);
extern void create_new_attribute_impasse_hook (slot *slot_with_new_impasse);
extern void remove_attribute_impasse_hook (slot *slot_with_impasse_to_be_removed);
extern void production_just_added_hook (production *new_prod);
extern void production_about_to_be_excised_hook (production *prod_to_be_excised);
extern void firing_hook (instantiation *inst);
extern void retraction_hook (instantiation *inst);
extern void system_parameter_changed_hook (int param_num);

/* ===========================================================================
   Miscellaneous functions that I needed to declare because I used them in
   more than one file, even though they're not really part of a "module
   interface" or something like that.
=========================================================================== */

extern double my_strtod (char *ch, char **p, int base); /* in lexer.c */

#define MAX_TEXT_INPUT_LINE_LENGTH 1000

extern Symbol *get_next_io_symbol_from_text_input_line (char **text_read_position); /* in io.c */

/* -------------------------------------------------------------------- */
/*                                                                      */
/* Macros for handling multi-agent switching in Soar.                   */

#ifdef USE_X_DISPLAY
#include <X11/Xlib.h>
#include <X11/Xutil.h>

typedef struct x_info_struct {
  Window  parent_window;
  Window  window;
  GC      gc;
  XFontStruct * font_struct;
  int     char_height;
  char    input_buffer[2000];
  int     input_buffer_index;
  char    text_input_buffer[MAX_TEXT_INPUT_LINE_LENGTH + 2];
  int     text_input_buffer_index;
  int     window_x;
  int     window_y;
  int     width;
  int     height;
  unsigned long foreground;
  unsigned long background;
  int     borderwidth;
  Symbol * last_op_id; /* Used in monitors */
} x_info;
#endif


/*----------------------------------------------------------------------*/
/*                                                                      */
/*  Agent structure used to hold what were previously global variables  */
/*  in the single-agent Soar.                                           */
/*                                                                      */
/*----------------------------------------------------------------------*/

enum go_type_enum { GO_PHASE, GO_ELABORATION, GO_DECISION,
                    GO_GOAL, GO_PROBLEM_SPACE, GO_STATE, GO_OPERATOR,
                    GO_SLOT };


#define BUFSIZE (MAX_LEXER_LINE_LENGTH+2) /* +2 for newline and null at end */

/* --- we'll use one of these structures for each file being read --- */

typedef struct lexer_source_file_struct {
  struct lexer_source_file_struct *parent_file;
  char *filename;
  FILE *file;
  bool fake_rparen_at_eol;
  bool allow_ids;
  int parentheses_level;    /* 0 means top level, no left paren's seen */
  int current_column;       /* column number of next char to read (0-based) */
  unsigned long current_line;   /* line number of line in buffer (1-based) */
  int column_of_start_of_last_lexeme;   /* (used for error messages) */
  unsigned long line_of_start_of_last_lexeme;
  char buffer[BUFSIZE];              /* holds text of current input line */
  struct lexeme_info saved_lexeme;   /* save/restore it during nested loads */
  char saved_current_char;           /* save/restore this too */
} lexer_source_file;


#define UPDATE_LINKS_NORMALLY 0
#define UPDATE_DISCONNECTED_IDS_LIST 1
#define JUST_UPDATE_COUNT 2


typedef struct input_function_info_struct {
  struct input_function_info_struct *next;
  input_function f;
} input_function_info;

typedef struct output_function_info_struct {
  struct output_function_info_struct *next;
  output_function f;
  Symbol *link_name;
} output_function_info;


typedef struct output_link_struct {
  struct output_link_struct *next, *prev;  /* dll of all existing links */
  byte status;                             /* current xxx_OL_STATUS */
  wme *link_wme;                           /* points to the output link wme */
  list *ids_in_tc;                         /* ids in TC(link) */
  output_function_info *ofi;               /* corresponding output function */
} output_link;


typedef struct text_io_symbol_to_file_mapping_struct {
  struct text_io_symbol_to_file_mapping_struct *next, *prev;
  Symbol *sym;
  int fd;          /* file descriptor for the operating system */
  bool trace_io;
} text_io_symbol_to_file_mapping;


/* --------------------------------------------------------------------

         Text Environment, Symbols, and Input Buffer Definitions

-------------------------------------------------------------------- */

/* --- status of the top-state ^text-environment link --- */
struct text_environment {  
  text_io_symbol_to_file_mapping *input_channel;
  text_io_symbol_to_file_mapping *output_channel;
}; 


#define CHUNK_COND_HASH_TABLE_SIZE 1024
#define LOG_2_CHUNK_COND_HASH_TABLE_SIZE 10

typedef struct chunk_cond_struct {
  condition *cond;                /* points to the original condition */

  condition *instantiated_cond;   /* points to cond in chunk instantiation */
  condition *variablized_cond;    /* points to cond in the actual chunk */
  condition *saved_prev_pointer_of_variablized_cond; /* don't ask */

  /* dll of all cond's in a set (i.e., a chunk_cond_set, or the grounds) */
  struct chunk_cond_struct *next, *prev;

  /* dll of cond's in this particular hash bucket for this set */
  struct chunk_cond_struct *next_in_bucket, *prev_in_bucket; 

  unsigned long hash_value;             /* equals hash_condition(cond) */
  unsigned long compressed_hash_value;  /* above, compressed to a few bits */
} chunk_cond;


typedef struct chunk_cond_set_struct {
  chunk_cond *all;       /* header for dll of all chunk_cond's in the set */
  chunk_cond *table[CHUNK_COND_HASH_TABLE_SIZE];  /* hash table buckets */
} chunk_cond_set;


/* --- structure of each alpha memory --- */
typedef struct alpha_mem_struct {
  struct alpha_mem_struct *next_in_hash_table;  /* next mem in hash bucket */
  struct right_mem_struct *right_mems;  /* dll of right_mem structures */
  struct rete_node_struct *beta_nodes;  /* list of attached beta nodes */
  unsigned long reference_count;    /* number of beta nodes using this mem */
  Symbol *id;                  /* constants tested by this alpha mem */
  Symbol *attr;                /* (NIL if this alpha mem ignores that field) */
  Symbol *value;
  bool acceptable;             /* does it test for acceptable pref? */
  unsigned long am_id;         /* id for hashing */
} alpha_mem;


/* --- tells where to find a variable --- */
typedef unsigned short rete_node_level;

typedef struct var_location_struct {
  rete_node_level levels_up; /* 0=current node's alphamem, 1=parent's, etc. */
  byte field_num;            /* 0=id, 1=attr, 2=value */
} var_location;

/* --- gives data for a test that must be applied at a node --- */
typedef struct rete_test_struct {
  byte right_field_num;          /* field (0, 1, or 2) from wme */
  byte type;                     /* test type (ID_IS_GOAL_RETE_TEST, etc.) */
  union rete_test_data_union {
    var_location variable_referent;   /* for relational tests to a variable */
    Symbol *constant_referent;        /* for relational tests to a constant */
    list *disjunction_list;           /* list of symbols in disjunction test */
  } data;
  struct rete_test_struct *next; /* next in list of tests at the node */
} rete_test;


/* --- data for positive and negative nodes --- */
typedef struct normal_node_data_struct {
  var_location left_hash_loc;        /* used only for hashed nodes */
  rete_test *other_tests;
  alpha_mem *alpha_mem;
  struct rete_node_struct *next_from_alpha_mem;
  struct rete_node_struct *prev_from_alpha_mem;
} normal_node_data;


/* --- data for cn and cn_partner nodes --- */
typedef struct cn_node_data_struct {
  struct rete_node_struct *partner;  /* cn, cn_partner point to each other */
} cn_node_data;

/* --- data for p_nodes --- */
typedef struct p_node_data_struct {
  struct production_struct *prod;
  struct node_varnames_struct *parents_sparse_nvn;
  struct ms_change_struct *tentative_assertions;
  struct ms_change_struct *tentative_retractions;
} p_node_data;


/* --- structure of a rete beta node --- */
typedef struct rete_node_struct {
  byte node_type;
  byte right_mem_link_status;
  struct rete_node_struct *parent;
  struct rete_node_struct *first_child;
  struct rete_node_struct *next_sibling;
  unsigned long node_id;
  union rete_node_c_union {
    unsigned long num_left_tokens;       /* for pos. nodes */
    struct neg_token_struct *neg_tokens; /* for neg., cn nodes */
  } c;
  union rete_node_d_union {
    normal_node_data norm;              /* for pos, neg nodes */
    cn_node_data cn;                    /* for cn, cn_partner nodes */
    p_node_data p;                      /* for p nodes */
  } d;
} rete_node;


/* --- token structures for normal nodes and negative/cn nodes --- */
typedef struct token_struct {
  struct token_struct *parent;  /* put at offset 0 so upward scans are fast */
  struct token_struct *next_in_bucket;
  rete_node *node;
  wme *w;
} token;

typedef struct neg_token_struct {
  token tok;  /* WARNING: This MUST be the first field */
  unsigned long match_count;
  struct neg_token_struct *next;
  struct neg_token_struct *prev;
} neg_token;

typedef struct right_mem_item_struct {
  struct right_mem_item_struct *next_in_bucket;
  alpha_mem *am;
  wme *w;
} right_mem_item;

typedef struct right_mem_struct {
  struct right_mem_struct *next, *prev;
  right_mem_item rmi;
} right_mem;

typedef void *hash_bucket_array;

typedef struct token_hash_table_struct {
  unsigned long count;      /* number of items in the table */
  unsigned long size;       /* number of buckets */
  short log2size;           /* log (base 2) of size */
  short minimum_log2size;   /* table never shrinks below this size */
  hash_bucket_array *buckets; /* points to array of pointers to token chains */
} token_hash_table;

typedef char varnames;

typedef struct three_field_varnames_struct {
  varnames *id_varnames;
  varnames *attr_varnames;
  varnames *value_varnames;
} three_field_varnames;

typedef struct node_varnames_struct {
  struct node_varnames_struct *parent;
  union varname_data_union {
    three_field_varnames fields;
    struct node_varnames_struct *bottom_of_subconditions;
  } data;
} node_varnames;


/* --- info about a change to the match set --- */
typedef struct ms_change_struct {
  struct ms_change_struct *next;         /* dll for all p nodes */
  struct ms_change_struct *prev;
  struct ms_change_struct *next_of_node; /* dll for just this p node */
  struct ms_change_struct *prev_of_node;
  struct rete_node_struct *p_node;       /* for retractions, this can be NIL
                                            if the p_node has been excised */
  struct token_struct *tok;            /* for assertions only */
  wme *w;                              /* for assertions only */
  struct instantiation_struct *inst;   /* for retractions only */
} ms_change;

/* --- trace format types --- */

enum trace_format_type {
  STRING_TFT,                        /* print a string */
  PERCENT_TFT,                       /* print a percent sign */
  L_BRACKET_TFT,                     /* print a left bracket */
  R_BRACKET_TFT,                     /* print a right bracket */
  VALUES_TFT,                        /* print values of attr path or '*' */
  VALUES_RECURSIVELY_TFT,            /* ditto only print recursively */
  ATTS_AND_VALUES_TFT,               /* ditto only print attr's too */
  ATTS_AND_VALUES_RECURSIVELY_TFT,   /* combination of the two above */
  CURRENT_GOAL_TFT,                  /* print current goal */
  CURRENT_PROBLEM_SPACE_TFT,         /* print current problem space */
  CURRENT_STATE_TFT,                 /* print current state */
  CURRENT_OPERATOR_TFT,              /* print current operator */
  DECISION_CYCLE_COUNT_TFT,          /* print # of dc's */
  ELABORATION_CYCLE_COUNT_TFT,       /* print # of ec's */
  IDENTIFIER_TFT,                    /* print identifier of object */
  IF_ALL_DEFINED_TFT,                /* print subformat if it's defined */
  LEFT_JUSTIFY_TFT,                  /* left justify the subformat */
  RIGHT_JUSTIFY_TFT,                 /* right justify the subformat */
  SUBGOAL_DEPTH_TFT,                 /* print # of subgoal depth */
  REPEAT_SUBGOAL_DEPTH_TFT };        /* repeat subformat s.d. times */

/* --- trace_format structure --- */

typedef struct trace_format_struct {
  struct trace_format_struct *next; /* next in linked list of format items */
  enum trace_format_type type;      /* what kind of item this is */
  int num;                          /* for formats with extra numeric arg */
  union trace_format_data_union {   /* data depending on trace format type */
    char *string;                           /* string to print */
    struct trace_format_struct *subformat;  /* [subformat in brackets] */
    list *attribute_path;  /* list.of.attr.path.symbols (NIL if path is '*') */
  } data;
} trace_format;

/* AGR 564 begins */
/* ======================================================================
                              explain.c
====================================================================== */

/*
   For each production which is backtraced through, keep the name of the
   production, which condition was being traced (from the RHS of this
   production firing) and then the lists of grounds, potentials, locals
   and negateds generated during the backtrace.
   At the moment I'm not guaranteeing that each condition appears in the
   correct list -- this is because elements move between lists after their
   initial positioning.
*/

typedef struct backtrace_struct {
   int result;                    /* 1 when this is a result of the chunk */
   condition *trace_cond;         /* The (local) condition being traced */
   char   prod_name[256];         /* The production's name */
   condition *grounds;            /* The list of conds for the LHS of chunk */
   condition *potentials;         /* The list of conds which aren't linked */
   condition *locals;             /* Conds in the subgoal -- need to BT */
   condition *negated;            /* Negated conditions (sub/super) */
   struct backtrace_struct *next_backtrace; /* Pointer to next in this list */
} backtrace_str;

/*
   For each chunk (or justification) take a copy of its conds and actions,
   and the list of productions which were backtraced through in creating it.
   Also keep a list of all of the grounds (WMEs in the supergoal) which were
   tested as the chunk was formed.
*/

typedef struct explain_chunk_struct {
   char name[256];                      /* Name of this chunk/justification */
   condition *conds;                    /* Variablized list of conditions */
   action *actions;                     /* Variablized list of actions */
   struct backtrace_struct *backtrace;  /* List of back traced productions */
   struct explain_chunk_struct *next_chunk; /* Next chunk in the list */
   condition *all_grounds;             /* All conditions which go to LHS -- 
                                          must be in same order as the chunk's 
                                          conditions. */
} explain_chunk_str;
/* AGR 564 ends */


/*----------------------------------------------------------------------*/
/*                                                                      */
/*  Explanations of fields in agent structure (in alphabetic order):    */
/*                                                                      */
/*    cons_cell_pool     :  Pool for cons cell storage                  */
/*    current_char       :  Holds current input character for lexer     */
/*    current_file       :  File we're currently reading in lexer       */
/*    dl_cons_pool       :  Pool for doubly-linked list cell storage    */
/*    go_number          :  How many times to "go" for go command       */
/*    go_slot_attr       :  Context slot checked in go command          */
/*    go_slot_level      :  Goal stack level checked in go command      */
/*    go_type            :  Phase type used in go command               */
/*    interrupt_source   :  String indicating source of interrupt       */
/*    lexeme             :  Holds the current lexeme                    */
/*    mcs_counter        :  Make constant symbol counter                */
/*    memory_for_usage   :  Memory usage counters                       */
/*    memory_pools_in_use:  List of all memory pools in use?            */
/*    placeholder_counter:  Used to number dummy vars in parser         */
/*                                                                      */
/*----------------------------------------------------------------------*/
       /* --- headers of dll's of all productions of each type --- */
       /* --- counts of how many productions there are of each type --- */

    /* running total of WM sizes at end of phases */
    /* # of items included in above sum */

/* --- accumulated cpu time spent in various parts of the system --- */

#include "queue.h"

#ifdef MULTI_AGENT_ENABLED

typedef struct agent_struct {

#include "global_vars.c"

#ifdef USE_X_DISPLAY
  char              * display_class;
  x_info            * X_data;
  x_info            * monitor;
#endif  /* USE_X_DISPLAY */

} agent;

#else
#include "global_vars.h"
#endif  /* MULTI_AGENT_ENABLED */

#define PATHNAME_MAX 1000

#ifdef __SC__
#include "soar_mpw.h"
#endif


#ifdef MULTI_AGENT_ENABLED
extern agent * soar_agent;
extern list  * all_soar_agents;
extern bool    multi_agent_mode;
#endif  /* MULTI_AGENT_ENABLED */

extern int     agent_count;

#ifdef MULTI_AGENT_ENABLED
  extern agent * global_agent;
#ifdef USE_X_DISPLAY
  extern char *  x_input_buffer;
  extern int     x_input_buffer_index;
  extern bool    waiting_for_command;
#endif
#endif  /* MULTI_AGENT_ENABLED */

#ifdef MULTI_AGENT_ENABLED
extern agent * create_soar_agent (char * name);
extern void    destroy_soar_agent (agent * soar_agent);
#else
extern void    create_soar_agent (char * name);
#endif  /* MULTI_AGENT_ENABLED */


/* These functions define the protocol functions for the X interface to  */
/* Soar.                                                                 */

#ifdef MULTI_AGENT_ENABLED
#ifdef USE_X_DISPLAY
extern void create_global_display (void);
extern void create_agent_window (agent *, char * agent_class);
extern void create_monitor_window (agent *, char * command);
extern void handle_soar_x_events (void);
extern void refresh_monitor_window (agent *);
extern void destroy_soar_display (void);

extern bool text_io_mode;

#ifdef USE_STDARGS
extern void print_x_format_string (x_info * window, char *format, ... );
#else
extern void print_x_format_string ();
#endif

#endif  /* USE_X_DISPLAY */

#endif  /* MULTI_AGENT_ENABLED */

extern char * c_interrupt_msg;

/* Main pgm stuff */

extern void init_soar (void);
extern int  terminate_soar (void);

#ifdef __cplusplus
#undef extern
#endif

#ifdef _WINDOWS
#include "../pc_support/gui/winstubs.h"
#endif

#endif /* _SOAR_H_INCLUDED */
