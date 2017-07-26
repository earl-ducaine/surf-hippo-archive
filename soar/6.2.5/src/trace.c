/*
 * $Id: trace.c,v 1.7 1994/11/23 16:40:34 rempel Exp $
 * $Log: trace.c,v $
 * Revision 1.7  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.6  1994/08/23  10:39:38  portelli
 * For 6.2.4
 *
 * Revision 1.5  1994/07/12  15:41:27  portelli
 * For 6.2.3
 *
 * Revision 1.4  94/07/12  15:35:17  portelli
 * For 6.2.3
 *
 * Revision 1.3  1994/05/18  13:34:30  portelli
 * Soar 6.2.0 b
 *
 * Revision 1.2  93/11/21  17:25:52  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:48:09  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:25:48  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  18:28:46  jtraub
 * added RCS header information
 *
 */

/* =====================================================================
                   Trace Format Routines for Soar 6

   This file contains definitions and routines for dealing with the trace
   formats used by Soar 6.  Trace format are specified by the user as
   strings (with % escape sequences in them).  At entry time, Soar 6 
   parses these strings into trace_format structures.
===================================================================== */

#include <ctype.h>
#include "soar.h"

/* ----------------------------------------------------------------------
                     Deallocate Trace Format List

   This deallocates all the memory used by a (list of) trace_format.
---------------------------------------------------------------------- */

void deallocate_trace_format_list (trace_format *tf) {
  trace_format *next;
  
  while (tf) {
    switch (tf->type) {
    case STRING_TFT:
      free_memory_block_for_string (tf->data.string);
      break;
      
    case VALUES_TFT:
    case VALUES_RECURSIVELY_TFT:
    case ATTS_AND_VALUES_TFT:
    case ATTS_AND_VALUES_RECURSIVELY_TFT:
      deallocate_symbol_list_removing_references (tf->data.attribute_path);
      break;
      
    case IF_ALL_DEFINED_TFT:
    case LEFT_JUSTIFY_TFT:
    case RIGHT_JUSTIFY_TFT:
    case REPEAT_SUBGOAL_DEPTH_TFT:
      deallocate_trace_format_list (tf->data.subformat);
      break;
      
    default:
      break; /* do nothing */
    }
    next = tf->next;
    free_memory (tf, MISCELLANEOUS_MEM_USAGE);
    tf = next;
  }
}

/* ----------------------------------------------------------------------
                   Trace Format String Parsing Routines

   Parse_format_string() parses a format string and returns a trace_format
   structure for it, or NIL if any error occurred.  This is the top-level
   routine here.

   While parsing is in progress, the global variable "format" points to
   the current character in the string.  This is advanced through the 
   string as parsing proceeds.  Parsing is done by recursive descent.
   If any parsing routine encouters an error, it sets the global variable
   "format_string_error_message" to be some appropriate error message,
   and leaves "format" pointing to the location of the error.

   Parse_attribute_path_in_brackets() reads "[attr.path.or.star]" and
   returns the path (consed list, or NIL for the '*' path).  

   Parse_pattern_in_brackets() reads "[subformat pattern]" and returns
   the trace format.

   Parse_item_from_format_string() is the main workhorse.  It reads from
   the current location up to the end of the item--a string, an escape
   sequence (with arguments), etc.  The end of an item is delineated by
   the end-of-string, a "[" or "]", or the end of the escape sequence.
---------------------------------------------------------------------- */

trace_format *parse_item_from_format_string (void);

trace_format *parse_format_string (char *string) {
  trace_format *first, *prev, *new;

  current_agent(format) = string;
  current_agent(format_string_error_message) = NIL;

  prev = NIL;
  while (*current_agent(format)!=0) {
    new = parse_item_from_format_string ();
    if (!new) {
      if (prev) prev->next = NIL; else first = NIL;
      deallocate_trace_format_list (first);
      print ("Error:  bad trace format string: %s\n", string);
      if (current_agent(format_string_error_message)) {
        print ("        %s\n", current_agent(format_string_error_message));
        print ("        Error found at: %s\n", current_agent(format));
      }
      return NIL;
    }
    if (prev) prev->next = new; else first = new;
    prev = new;
  }
  if (prev) prev->next = NIL; else first = NIL;
  
  return first;
}

list *parse_attribute_path_in_brackets (void) {
  list *path;
  char name[MAX_LEXEME_LENGTH+20], *ch;
  Symbol *sym;

  /* --- look for opening bracket --- */
  if (*current_agent(format) != '[') {
    current_agent(format_string_error_message) = "Expected '[' followed by attribute (path)";
    return NIL;
  }
  current_agent(format)++;

  /* --- check for '*' (null path) --- */
  if (*current_agent(format)=='*') {
    path = NIL;
    current_agent(format)++;
  } else {
    /* --- normal case: read the attribute path --- */
    path = NIL;
    while (TRUE) {
      ch = name;
      while ((*current_agent(format)!=0)&&(*current_agent(format)!=']')&&(*current_agent(format)!='.')) *ch++ = *current_agent(format)++;
      if (*current_agent(format)==0) {
        current_agent(format_string_error_message) = "'[' without closing ']'";
        deallocate_symbol_list_removing_references (path);
        return NIL;
      }
      if (ch==name) {
        current_agent(format_string_error_message) = "null attribute found in attribute path";
        deallocate_symbol_list_removing_references (path);
        return NIL;
      }
      *ch = 0;
      sym = make_sym_constant (name);
      push (sym, path);
      if (*current_agent(format)==']') break;
      current_agent(format)++;  /* skip past '.' */
    }
    path = destructively_reverse_list (path);
  }

  /* --- look for closing bracket --- */
  if (*current_agent(format) != ']') {
    current_agent(format_string_error_message) = "'[' without closing ']'";
    deallocate_symbol_list_removing_references (path);
    return NIL;
  }
  current_agent(format)++;
  
  return path;
}

trace_format *parse_pattern_in_brackets (bool read_opening_bracket) {
  trace_format *first, *prev, *new;

  /* --- look for opening bracket --- */
  if (read_opening_bracket) {
    if (*current_agent(format) != '[') {
      current_agent(format_string_error_message) = "Expected '[' followed by attribute path";
      return NIL;
    }
    current_agent(format)++;
  }

  /* --- read pattern --- */
  prev = NIL;
  while ((*current_agent(format)!=0) && (*current_agent(format)!=']')) {
    new = parse_item_from_format_string ();
    if (!new) {
      if (prev) prev->next = NIL; else first = NIL;
      deallocate_trace_format_list (first);
      return NIL;
    }
    if (prev) prev->next = new; else first = new;
    prev = new;
  }
  if (prev) prev->next = NIL; else first = NIL;
  
  /* --- look for closing bracket --- */
  if (*current_agent(format) != ']') {
    current_agent(format_string_error_message) = "'[' without closing ']'";
    deallocate_trace_format_list (first);
    return NIL;
  }
  current_agent(format)++;
  
  return first;
}

trace_format *parse_item_from_format_string (void) {
  trace_format *tf, *pattern;
  char *ch;
  list *attribute_path;
  int n;

  if (*current_agent(format) == 0) return NIL;
  if (*current_agent(format) == ']') return NIL;
  if (*current_agent(format) == '[') {
    current_agent(format_string_error_message) = "unexpected '[' character";
    return NIL;
  }

  if (*current_agent(format) != '%') {
    char buf[MAX_LEXEME_LENGTH+20];
    
    ch = buf;
    while ((*current_agent(format) != 0) && (*current_agent(format) != '%') && (*current_agent(format) != '[') &&
           (*current_agent(format) != ']'))
      *ch++ = *current_agent(format)++;
    *ch = 0;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = STRING_TFT;
    tf->data.string = make_memory_block_for_string (buf);
    return tf;
  }
  
  /* --- otherwise *format is '%', so parse the escape sequence --- */  

  if (!strncmp(current_agent(format), "%v", 2)) {
    current_agent(format) += 2;
    attribute_path = parse_attribute_path_in_brackets ();
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = VALUES_TFT;
    tf->data.attribute_path = attribute_path;
    return tf;
  }
                          
  if (!strncmp(current_agent(format), "%o", 2)) {
    current_agent(format) += 2;
    attribute_path = parse_attribute_path_in_brackets ();
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = VALUES_RECURSIVELY_TFT;
    tf->data.attribute_path = attribute_path;
    return tf;
  }
                          
  if (!strncmp(current_agent(format), "%av", 3)) {
    current_agent(format) += 3;
    attribute_path = parse_attribute_path_in_brackets ();
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = ATTS_AND_VALUES_TFT;
    tf->data.attribute_path = attribute_path;
    return tf;
  }
                          
  if (!strncmp(current_agent(format), "%ao", 3)) {
    current_agent(format) += 3;
    attribute_path = parse_attribute_path_in_brackets ();
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = ATTS_AND_VALUES_RECURSIVELY_TFT;
    tf->data.attribute_path = attribute_path;
    return tf;
  }

  if (!strncmp(current_agent(format), "%cg", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = CURRENT_GOAL_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%cp", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = CURRENT_PROBLEM_SPACE_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%cs", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = CURRENT_STATE_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%co", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = CURRENT_OPERATOR_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%dc", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = DECISION_CYCLE_COUNT_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%ec", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = ELABORATION_CYCLE_COUNT_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%%", 2)) {
    current_agent(format) += 2;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = PERCENT_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%[", 2)) {
    current_agent(format) += 2;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = L_BRACKET_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%]", 2)) {
    current_agent(format) += 2;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = R_BRACKET_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%sd", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = SUBGOAL_DEPTH_TFT;
    return tf;
  }

  if (!strncmp(current_agent(format), "%id", 3)) {
    current_agent(format) += 3;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = IDENTIFIER_TFT;
    return tf;
  }

  if (! strncmp (current_agent(format), "%ifdef", 6)) {
    current_agent(format) += 6;
    pattern = parse_pattern_in_brackets (TRUE);
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = IF_ALL_DEFINED_TFT;
    tf->data.subformat = pattern;
    return tf;
  }

  if (! strncmp (current_agent(format), "%left", 5)) {
    current_agent(format) += 5;
    if (*current_agent(format) != '[') {
      current_agent(format_string_error_message) = "Expected '[' after %left";
      return NIL;
    }
    current_agent(format)++;
    if (! isdigit(*current_agent(format))) {
      current_agent(format_string_error_message) = "Expected number with %left";
      return NIL;
    }
    n = 0;
    while (isdigit(*current_agent(format))) n = 10*n + (*current_agent(format)++ - '0');
    if (*current_agent(format) != ',') {
      current_agent(format_string_error_message) = "Expected ',' after number in %left";
      return NIL;
    }
    current_agent(format)++;
    pattern = parse_pattern_in_brackets (FALSE);
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = LEFT_JUSTIFY_TFT;
    tf->num = n;
    tf->data.subformat = pattern;
    return tf;
  }

  if (! strncmp (current_agent(format), "%right", 6)) {
    current_agent(format) += 6;
    if (*current_agent(format) != '[') {
      current_agent(format_string_error_message) = "Expected '[' after %right";
      return NIL;
    }
    current_agent(format)++;
    if (! isdigit(*current_agent(format))) {
      current_agent(format_string_error_message) = "Expected number with %right";
      return NIL;
    }
    n = 0;
    while (isdigit(*current_agent(format))) n = 10*n + (*current_agent(format)++ - '0');
    if (*current_agent(format) != ',') {
      current_agent(format_string_error_message) = "Expected ',' after number in %right";
      return NIL;
    }
    current_agent(format)++;
    pattern = parse_pattern_in_brackets (FALSE);
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = RIGHT_JUSTIFY_TFT;
    tf->num = n;
    tf->data.subformat = pattern;
    return tf;
  }

  if (! strncmp (current_agent(format), "%rsd", 4)) {
    current_agent(format) += 4;
    pattern = parse_pattern_in_brackets (TRUE);
    if (current_agent(format_string_error_message)) return NIL;
    tf = allocate_memory (sizeof(trace_format), MISCELLANEOUS_MEM_USAGE);
    tf->type = REPEAT_SUBGOAL_DEPTH_TFT;
    tf->data.subformat = pattern;
    return tf;
  }

  /* --- if we haven't recognized it yet, we don't understand it --- */
  current_agent(format_string_error_message) = "Unrecognized escape sequence";
  return NIL;
}

/* ----------------------------------------------------------------------
                      Print Trace Format List

   This routine takes a trace format (list) and prints it out as a format
   string (without the surrounding quotation marks).
---------------------------------------------------------------------- */

void print_trace_format_list (trace_format *tf) {
  cons *c;

  for ( ; tf!=NIL; tf=tf->next) {
    switch (tf->type) {
    case STRING_TFT:
      { char *s;
        int i, len;
        
        s = string_to_escaped_string (tf->data.string, '"', NULL);
        len = strlen (s);
        for (i=1; i<len-1; i++) print ("%c", *(s+i));
      }
      break;
    case PERCENT_TFT: print_string ("%%"); break;
    case L_BRACKET_TFT: print_string ("%["); break;
    case R_BRACKET_TFT: print_string ("%]"); break;

    case VALUES_TFT:
    case VALUES_RECURSIVELY_TFT:
    case ATTS_AND_VALUES_TFT:
    case ATTS_AND_VALUES_RECURSIVELY_TFT:
      if (tf->type==VALUES_TFT) print_string ("%v[");
      else if (tf->type==VALUES_RECURSIVELY_TFT) print_string ("%o[");
      else if (tf->type==ATTS_AND_VALUES_TFT) print_string ("%av[");
      else print_string ("%ao[");
      if (tf->data.attribute_path) {
        for (c=tf->data.attribute_path; c!=NIL; c=c->rest) {
          print_string (((Symbol *)(c->first))->sc.name);
          if (c->rest) print_string (".");
        }
      } else {
        print_string ("*");
      }
      print_string ("]");
      break;

    case CURRENT_GOAL_TFT: print_string ("%cg"); break;
    case CURRENT_PROBLEM_SPACE_TFT: print_string ("%cp"); break;
    case CURRENT_STATE_TFT: print_string ("%cs"); break;
    case CURRENT_OPERATOR_TFT: print_string ("%co"); break;
    case DECISION_CYCLE_COUNT_TFT: print_string ("%dc"); break;
    case ELABORATION_CYCLE_COUNT_TFT: print_string ("%ec"); break;
    case IDENTIFIER_TFT: print_string ("%id"); break;

    case IF_ALL_DEFINED_TFT:
      print_string ("%ifdef[");
      print_trace_format_list (tf->data.subformat);
      print_string ("]");
      break;
      
    case LEFT_JUSTIFY_TFT:
    case RIGHT_JUSTIFY_TFT:
      if (tf->type==LEFT_JUSTIFY_TFT) print_string ("%left[");
      else print_string ("%right[");
      print ("%d,", tf->num);
      print_trace_format_list (tf->data.subformat);
      print_string ("]");
      break;

    case SUBGOAL_DEPTH_TFT: print_string ("%sd"); break;

    case REPEAT_SUBGOAL_DEPTH_TFT:
      print_string ("%rsd[");
      print_trace_format_list (tf->data.subformat);
      print_string ("]");
      break;

    default:
      print ("Internal error: bad trace format type\n");
      abort_with_fatal_error();      
    }
  }
}

/* ======================================================================
                    Trace Format Specification Tables

   We maintain tables of object trace formats and selection trace formats.
   Trace formats that apply to any *|g|p|s|o are stored in the arrays
   object_tf_for_anything[] and stack_tf_for_anything[].  (The array
   entry is NIL if no trace format has been specified.)  Trace formats that
   apply to *|g|p|s|o's with a certain name are stored in the hash tables
   object_tr_ht[] and stack_tr_ht[].  (Hash tables are used so we can
   look up the trace format from an object's name quickly.)

   Init_tracing() initializes the tables; at this point, there are no trace
   formats for anything.  This routine should be called at startup time.

   Trace formats are changed by calls to add_trace_format() and
   remove_trace_format().  Lookup_trace_format() returns the trace
   format matching a given type restriction and/or name restriction,
   or NIL if no such format has been specified.  Add_trace_format() returns
   TRUE if the format was successfully added, or FALSE if the format
   string didn't parse right.  Remove_trace_format() returns TRUE if
   a trace format was actually removed, or FALSE if there was no such
   trace format for the given type/name restrictions.  These routines take
   a "stack_trace" argument, which should be TRUE if the stack trace
   format is intended, or FALSE if the object trace format is intended.
   Their "type_restriction" argument should be one of FOR_ANYTHING_TF,
   ..., FOR_OPERATORS_TF (see soar.h).  The "name_restriction" argument
   should be either a pointer to a symbol, if the trace format is
   restricted to apply to objects with that name, or NIL if the format
   can apply to any object.
   
   Print_all_trace_formats() prints out either all existing stack trace
   or object trace formats.
====================================================================== */

/* --- trace formats that don't test the object name --- */

typedef struct tracing_rule_struct {
  /* Warning: this MUST be the first field, for the hash table routines */
  struct tracing_rule_struct *next_in_hash_bucket; 
  int type_restriction;      /* FOR_GOALS_TF, etc. */
  Symbol *name_restriction;  /* points to name Symbol, or NIL */
  trace_format *format;      /* indicates the format to use */
} tracing_rule;

#define hash_name_restriction(name,num_bits) \
  ((name)->common.hash_id & masks_for_n_low_order_bits[(num_bits)])

/* --- hash function for resizable hash table routines --- */
unsigned long tracing_rule_hash_function (void *item, short num_bits) {
  tracing_rule *tr;
  
  tr = item;
  return hash_name_restriction (tr->name_restriction, num_bits);
}

/* --- hash tables for stack and object traces --- */


void init_tracing (void) {
  int i;

  for (i=0; i<5; i++) {
    current_agent(object_tr_ht)[i] = make_hash_table (0, tracing_rule_hash_function);
    current_agent(stack_tr_ht)[i] = make_hash_table (0, tracing_rule_hash_function);
    current_agent(object_tf_for_anything)[i] = NIL;
    current_agent(stack_tf_for_anything)[i] = NIL;
  }
}

trace_format *lookup_trace_format (bool stack_trace,
                                   int type_restriction,
                                   Symbol *name_restriction) {
  unsigned long hash_value;
  hash_table *ht;
  tracing_rule *tr;

  if (name_restriction) {
    if (stack_trace)
      ht = current_agent(stack_tr_ht)[type_restriction];
    else
      ht = current_agent(object_tr_ht)[type_restriction];
    hash_value = hash_name_restriction (name_restriction, ht->log2size);
    tr = (tracing_rule *) (*(ht->buckets + hash_value));
    for ( ; tr!=NIL; tr = tr->next_in_hash_bucket)
      if (tr->name_restriction==name_restriction) return tr->format;
    return NIL;
  }
  /* --- no name restriction --- */
  if (stack_trace)
    return current_agent(stack_tf_for_anything)[type_restriction];
  else
    return current_agent(object_tf_for_anything)[type_restriction];
}

bool remove_trace_format (bool stack_trace,
                          int type_restriction,
                          Symbol *name_restriction) {
  unsigned long hash_value;
  hash_table *ht;
  tracing_rule *tr;
  trace_format **format;

  if (name_restriction) {
    if (stack_trace)
      ht = current_agent(stack_tr_ht)[type_restriction];
    else
      ht = current_agent(object_tr_ht)[type_restriction];
    hash_value = hash_name_restriction (name_restriction, ht->log2size);
    tr = (tracing_rule *) (*(ht->buckets + hash_value));
    for ( ; tr!=NIL; tr = tr->next_in_hash_bucket)
      if (tr->name_restriction==name_restriction) break;
    if (! tr) return FALSE;
    deallocate_trace_format_list (tr->format);
    remove_from_hash_table (ht, tr);
    free_memory (tr, MISCELLANEOUS_MEM_USAGE);
    symbol_remove_ref (name_restriction);
    return TRUE;
  }
  /* --- no name restriction --- */
  if (stack_trace)
    format = &(current_agent(stack_tf_for_anything)[type_restriction]);
  else
    format = &(current_agent(object_tf_for_anything)[type_restriction]);
  if (! *format) return FALSE;
  deallocate_trace_format_list (*format);
  *format = NIL;
  return TRUE;
}

bool add_trace_format (bool stack_trace,
                       int type_restriction,
                       Symbol *name_restriction,
                       char *format_string) {
  tracing_rule *tr;
  trace_format *new_tf;
  hash_table *ht;

  /* --- parse the format string into a trace_format --- */
  new_tf = parse_format_string (format_string);
  if (!new_tf) return FALSE;

  /* --- first remove any existing trace format with same conditions --- */
  remove_trace_format (stack_trace, type_restriction, name_restriction);

  /* --- now add the new one --- */
  if (name_restriction) {
    symbol_add_ref (name_restriction);
    if (stack_trace)
      ht = current_agent(stack_tr_ht)[type_restriction];
    else
      ht = current_agent(object_tr_ht)[type_restriction];
    tr = allocate_memory (sizeof(tracing_rule), MISCELLANEOUS_MEM_USAGE);
    tr->type_restriction = type_restriction;
    tr->name_restriction = name_restriction;
    tr->format = new_tf;
    add_to_hash_table (ht, tr);
    return TRUE;
  }
  /* --- no name restriction --- */
  if (stack_trace)
    current_agent(stack_tf_for_anything)[type_restriction] = new_tf;
  else
    current_agent(object_tf_for_anything)[type_restriction] = new_tf;

  return TRUE;
}

char tracing_object_letters[5] = {'*','g','p','s','o'};

void print_tracing_rule (int type_restriction, Symbol *name_restriction,
                         trace_format *format) {
  if (current_agent(printing_stack_traces))
    print_string ("(stack-trace-format");
  else
    print_string ("(object-trace-format");
  print (" :add %c ", tracing_object_letters[type_restriction]);
  if (name_restriction) print_with_symbols ("%y ", name_restriction);
  print_string ("\"");
  print_trace_format_list (format);
  print ("\")\n");
}

bool print_trace_callback_fn (void *item) {
  tracing_rule *tr;

  tr = item;
  print_tracing_rule (tr->type_restriction, tr->name_restriction, tr->format);
  return FALSE;
}

void print_all_trace_formats (bool stack_trace) {
  int i;

  current_agent(printing_stack_traces) = stack_trace;
  if (stack_trace) {
    for (i=0; i<5; i++) {
      if (current_agent(stack_tf_for_anything)[i])
        print_tracing_rule (i, NIL, current_agent(stack_tf_for_anything)[i]);
      do_for_all_items_in_hash_table (current_agent(stack_tr_ht)[i],print_trace_callback_fn);
    }
  } else {
    for (i=0; i<5; i++) {
      if (current_agent(object_tf_for_anything)[i])
        print_tracing_rule (i, NIL, current_agent(object_tf_for_anything)[i]);
      do_for_all_items_in_hash_table (current_agent(object_tr_ht)[i],print_trace_callback_fn);
    }
  }
}

/* ======================================================================
                      Trace Format List To String

   Trace_format_list_to_string() is the main routine which, given a
   trace format and a current object, builds and returns a growable_string
   for that object's printout.  A number of helper routines are used by
   trace_format_list_to_string().
====================================================================== */

growable_string object_to_trace_string (Symbol *object);


                        /* set to TRUE whenever an escape sequence result is
                           undefined--for use with %ifdef */

struct tracing_parameters {
  Symbol *current_g;          /* current goal, etc. -- for use in %cg, etc. */
  Symbol *current_p;
  Symbol *current_s;
  Symbol *current_o;
  bool allow_cycle_counts;    /* TRUE means allow %dc and %ec */
} tparams;

/* ----------------------------------------------------------------
   Adds all values of the given attribute path off the given object
   to the "*result" growable_string.  If "recursive" is TRUE, the
   values are printed recursively as objects, rather than as simple
   atomic values.  "*count" is incremented each time a value is
   printed.  (To get a count of how many values were printed, the
   caller should initialize this to 0, then call this routine.)
---------------------------------------------------------------- */

void add_values_of_attribute_path (Symbol *object,
                                   list *path,
                                   growable_string *result,
                                   bool recursive,
                                   int *count) {
  slot *s;
  wme *w;
  char *ch;
  growable_string gs;

  if (!path) {  /* path is NIL, so we've reached the end of the path */
    add_to_growable_string (result, " ");
    if (recursive) {
      gs = object_to_trace_string (object);
      add_to_growable_string (result, text_of_growable_string(gs));
      free_growable_string (gs);
    } else {
      ch = symbol_to_string (object, TRUE, NULL);
      add_to_growable_string (result, ch);
    }
    (*count)++;
    return;
  }

  /* --- not at end of path yet --- */
  /* --- can't follow any more path segments off of a non-identifier --- */
  if (object->common.symbol_type != IDENTIFIER_SYMBOL_TYPE) return;

  /* --- call this routine recursively on any wme matching the first segment
     of the attribute path --- */
  for (w=object->id.impasse_wmes; w!=NIL; w=w->next)
    if (w->attr == path->first)
      add_values_of_attribute_path (w->value, path->rest, result, recursive,
                                    count);
  for (w=object->id.input_wmes; w!=NIL; w=w->next)
    if (w->attr == path->first)
      add_values_of_attribute_path (w->value, path->rest, result, recursive,
                                    count);
  s = find_slot (object, path->first);
  if (s) {
    for (w=s->wmes; w!=NIL; w=w->next)
      add_values_of_attribute_path (w->value, path->rest, result, recursive,
                                    count);
  }
}

/* ----------------------------------------------------------------
   Adds info for a wme to the given "*result" growable_string. If
   "print_attribute" is TRUE, then "^att-name" is included.  If
   "recursive" is TRUE, the value is printed recursively as an
   object, rather than as a simple atomic value.
---------------------------------------------------------------- */

void add_trace_for_wme (growable_string *result,
                        wme *w,
                        bool print_attribute,
                        bool recursive) {
  char *ch;
  growable_string gs;

  add_to_growable_string (result, " ");
  if (print_attribute) {
    add_to_growable_string (result, "^");
    ch = symbol_to_string (w->attr, TRUE, NULL);
    add_to_growable_string (result, ch);
    add_to_growable_string (result, " ");
  }
  if (recursive) {
    gs = object_to_trace_string (w->value);
    add_to_growable_string (result, text_of_growable_string(gs));
    free_growable_string (gs);
  } else {
    ch = symbol_to_string (w->value, TRUE, NULL);
    add_to_growable_string (result, ch);
  }
}

/* ----------------------------------------------------------------
   Adds the trace for values of a given attribute path off a given
   object, to the given "*result" growable_string.  If
   "print_attributes" is TRUE, then "^att-name" is included.  If
   "recursive" is TRUE, the values are printed recursively as 
   objects, rather than as a simple atomic value.  If the given path
   is NIL, then all values of all attributes of the given object
   are printed.
---------------------------------------------------------------- */

void add_trace_for_attribute_path (Symbol *object,
                                   list *path,
                                   growable_string *result,
                                   bool print_attributes,
                                   bool recursive) {
  growable_string values;
  cons *c;
  char *ch;
  int count;
  slot *s;
  wme *w; 

  values = make_blank_growable_string();

  if (! path) {
    if (object->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) return;
    for (s=object->id.slots; s!=NIL; s=s->next)
      for (w=s->wmes; w!=NIL; w=w->next)
        add_trace_for_wme (&values, w, print_attributes, recursive);
    for (w=object->id.impasse_wmes; w!=NIL; w=w->next)
      add_trace_for_wme (&values, w, print_attributes, recursive);
    for (w=object->id.input_wmes; w!=NIL; w=w->next)
      add_trace_for_wme (&values, w, print_attributes, recursive);
    if (length_of_growable_string(values)>0)
      add_to_growable_string (result, text_of_growable_string(values)+1);
    free_growable_string (values);
    return;
  }

  count = 0;
  add_values_of_attribute_path (object, path, &values, recursive, &count);
  if (! count) {
    current_agent(found_undefined) = TRUE;
    free_growable_string (values);
    return;
  }

  if (print_attributes) {
    add_to_growable_string (result, "^");
    for (c=path; c!=NIL; c=c->rest) {
      ch = symbol_to_string (c->first, TRUE, NULL);
      add_to_growable_string (result, ch);
      if (c->rest) add_to_growable_string (result, ".");
    }
    add_to_growable_string (result, " ");
  }
  if (length_of_growable_string(values)>0)
    add_to_growable_string (result, text_of_growable_string(values)+1);
  free_growable_string (values);
}

/* ----------------------------------------------------------------
   This is the main routine here.  It returns a growable_string,
   given a trace format list (the format to use) and an object (the
   object being printed).
---------------------------------------------------------------- */

growable_string trace_format_list_to_string (trace_format *tf, Symbol *object){
  char buf[50], *ch;
  growable_string result, temp_gs;
  int i;

  result = make_blank_growable_string();

  for ( ; tf!=NIL; tf=tf->next) {
    switch (tf->type) {
    case STRING_TFT:
      add_to_growable_string (&result, tf->data.string);
      break;
    case PERCENT_TFT:
      add_to_growable_string (&result, "%");
      break;
    case L_BRACKET_TFT:
      add_to_growable_string (&result, "[");
      break;
    case R_BRACKET_TFT:
      add_to_growable_string (&result, "]");
      break;

    case VALUES_TFT:
      add_trace_for_attribute_path (object, tf->data.attribute_path, &result,
                                    FALSE, FALSE);
      break;
    case VALUES_RECURSIVELY_TFT:
      add_trace_for_attribute_path (object, tf->data.attribute_path, &result,
                                    FALSE, TRUE);
      break;
    case ATTS_AND_VALUES_TFT:
      add_trace_for_attribute_path (object, tf->data.attribute_path, &result,
                                    TRUE, FALSE);
      break;
    case ATTS_AND_VALUES_RECURSIVELY_TFT:
      add_trace_for_attribute_path (object, tf->data.attribute_path, &result,
                                    TRUE, TRUE);
      break;

    case CURRENT_GOAL_TFT:
      if (! tparams.current_g) {
        current_agent(found_undefined) = TRUE;
      } else {
        temp_gs = object_to_trace_string (tparams.current_g);
        add_to_growable_string (&result, text_of_growable_string(temp_gs));
        free_growable_string (temp_gs);
      }
      break;
    case CURRENT_PROBLEM_SPACE_TFT:
      if (! tparams.current_p) {
        current_agent(found_undefined) = TRUE;
      } else {
        temp_gs = object_to_trace_string (tparams.current_p);
        add_to_growable_string (&result, text_of_growable_string(temp_gs));
        free_growable_string (temp_gs);
      }
      break;
    case CURRENT_STATE_TFT:
      if (! tparams.current_s) {
        current_agent(found_undefined) = TRUE;
      } else {
        temp_gs = object_to_trace_string (tparams.current_s);
        add_to_growable_string (&result, text_of_growable_string(temp_gs));
        free_growable_string (temp_gs);
      }
      break;
    case CURRENT_OPERATOR_TFT:
      if (! tparams.current_o) {
        current_agent(found_undefined) = TRUE;
      } else {
        temp_gs = object_to_trace_string (tparams.current_o);
        add_to_growable_string (&result, text_of_growable_string(temp_gs));
        free_growable_string (temp_gs);
      }
      break;

    case DECISION_CYCLE_COUNT_TFT:
      if (tparams.allow_cycle_counts) {
        sprintf (buf, "%lu", current_agent(d_cycle_count));
        add_to_growable_string (&result, buf);
      } else {
        current_agent(found_undefined) = TRUE;
      }
      break;
    case ELABORATION_CYCLE_COUNT_TFT:
      if (tparams.allow_cycle_counts) {
        sprintf (buf, "%lu", current_agent(e_cycle_count));
        add_to_growable_string (&result, buf);
      } else {
        current_agent(found_undefined) = TRUE;
      }
      break;

    case IDENTIFIER_TFT:
      ch = symbol_to_string (object, TRUE, NULL);
      add_to_growable_string (&result, ch);
      break;

    case IF_ALL_DEFINED_TFT:
      { bool saved_found_undefined;
        saved_found_undefined = current_agent(found_undefined);
        current_agent(found_undefined) = FALSE;
        temp_gs = trace_format_list_to_string (tf->data.subformat, object);
        if (! current_agent(found_undefined))
          add_to_growable_string (&result, text_of_growable_string(temp_gs));
        free_growable_string (temp_gs);
        current_agent(found_undefined) = saved_found_undefined;
      }
      break;
      
    case LEFT_JUSTIFY_TFT:
      temp_gs = trace_format_list_to_string (tf->data.subformat, object);
      add_to_growable_string (&result, text_of_growable_string(temp_gs));
      for (i=tf->num - length_of_growable_string(temp_gs); i>0; i--)
        add_to_growable_string (&result, " ");
      free_growable_string (temp_gs);
      break;

    case RIGHT_JUSTIFY_TFT:
      temp_gs = trace_format_list_to_string (tf->data.subformat, object);
      for (i=tf->num - length_of_growable_string(temp_gs); i>0; i--)
        add_to_growable_string (&result, " ");
      add_to_growable_string (&result, text_of_growable_string(temp_gs));
      free_growable_string (temp_gs);
      break;

    case SUBGOAL_DEPTH_TFT:
      if (tparams.current_g) {
        sprintf (buf, "%lu", tparams.current_g->id.level - 1);
        add_to_growable_string (&result, buf);
      } else {
        current_agent(found_undefined) = TRUE;
      }
      break;

    case REPEAT_SUBGOAL_DEPTH_TFT:
      if (tparams.current_g) {
        temp_gs = trace_format_list_to_string (tf->data.subformat, object);
        for (i = tparams.current_g->id.level - 1; i>0; i--)
          add_to_growable_string (&result, text_of_growable_string(temp_gs));
        free_growable_string (temp_gs);
      } else {
        current_agent(found_undefined) = TRUE;
      }
      break;

    default:
      print ("Internal error: bad trace format type\n");
      abort_with_fatal_error();      
    }
  }
  return result;
}

/* ======================================================================
               Building Traces for Object and Selections

   Find_appropriate_trace_format() looks for an applicable trace_format
   among the current set of tracing rules.

   Object_to_trace_string() takes an object and returns a growable_string
   to use for its printed trace.

   Selection_to_trace_string() takes an object (the current selection),
   the current goal, a "selection_type" (one of FOR_OPERATORS_TF, etc.),
   and a flag indicating whether %dc, %ec, etc. escapes should be
   allowed, and returns a growable_string to use for the trace.
====================================================================== */


                          /* prevents infinite loops when printing circular
                               structures */

trace_format *find_appropriate_trace_format (bool stack_trace,
                                             int type,
                                             Symbol *name) {
  trace_format *tf;

  /* --- first try to find the exact one --- */
  tf = lookup_trace_format (stack_trace, type, name);
  if (tf) return tf;

  /* --- failing that, try ignoring the type but retaining the name --- */
  if (type!=FOR_ANYTHING_TF) {
    tf = lookup_trace_format (stack_trace, FOR_ANYTHING_TF, name);
    if (tf) return tf;
  }

  /* --- failing that, try ignoring the name but retaining the type --- */
  if (name) {
    tf = lookup_trace_format (stack_trace, type, NIL);
    if (tf) return tf;
  }

  /* --- last resort: find a format that applies to anything at all --- */
  return lookup_trace_format (stack_trace, FOR_ANYTHING_TF, NIL);
}

growable_string object_to_trace_string (Symbol *object) {
  growable_string gs;
  int type_of_object;
  trace_format *tf;
  Symbol *name;
  struct tracing_parameters saved_tparams;

  /* --- If it's not an identifier, just print it as an atom.  Also, if it's
     already being printed, print it as an atom to avoid getting into an
     infinite loop. --- */
  if ((object->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) ||
      (object->id.tc_num == current_agent(tf_printing_tc))) {
    gs = make_blank_growable_string ();
    add_to_growable_string (&gs, symbol_to_string (object, TRUE, NIL));
    return gs;
  }

  /* --- mark it as being printed --- */
  object->id.tc_num = current_agent(tf_printing_tc);

  /* --- determine the type and name of the object --- */
  if (object->id.isa_goal) type_of_object=FOR_GOALS_TF;

/* SBH 7/8/94 */
#ifndef NNPSCM
  else if (object->id.isa_problem_space) type_of_object=FOR_PROBLEM_SPACES_TF;
#endif
  else if (object->id.isa_state) type_of_object=FOR_STATES_TF;
  else if (object->id.isa_operator) type_of_object=FOR_OPERATORS_TF;
  else type_of_object=FOR_ANYTHING_TF;

  name = find_name_of_object (object);

  /* --- find the trace format to use --- */
  tf = find_appropriate_trace_format (FALSE, type_of_object, name);

  /* --- now call trace_format_list_to_string() --- */
  if (tf) {
    saved_tparams = tparams;
    tparams.current_g = tparams.current_p = tparams.current_s =
      tparams.current_o = NIL;
    tparams.allow_cycle_counts = FALSE;
    gs = trace_format_list_to_string (tf, object);
    tparams = saved_tparams;
  } else {
    /* --- no applicable trace format, so just print the object itself --- */
    gs = make_blank_growable_string ();
    add_to_growable_string (&gs, symbol_to_string (object, TRUE, NIL));
  }
  
  object->id.tc_num = 0;  /* unmark it now that we're done */  
  return gs;
}

growable_string selection_to_trace_string (Symbol *object,
                                           Symbol *current_goal,
                                           int selection_type,
                                           bool allow_cycle_counts) {
  trace_format *tf;
  Symbol *ps, *name;
  growable_string gs;
  struct tracing_parameters saved_tparams;

  /* --- find the problem space name --- */
  name = NIL;
#ifndef NNPSCM
  if (current_goal->id.problem_space_slot->wmes) {
    ps = current_goal->id.problem_space_slot->wmes->value;
    name = find_name_of_object (ps);
  }
#endif

  /* --- find the trace format to use --- */
  tf = find_appropriate_trace_format (TRUE, selection_type, name);  

  /* --- if there's no applicable trace format, print nothing --- */
  if (!tf) return make_blank_growable_string ();

  /* --- save/restore tparams, and call trace_format_list_to_string() --- */
  saved_tparams = tparams;
  tparams.current_g = tparams.current_p = tparams.current_s =
    tparams.current_o = NIL;
  if (current_goal) {  
    tparams.current_g = current_goal;
#ifndef NNPSCM
    if (current_goal->id.problem_space_slot->wmes)
      tparams.current_p = current_goal->id.problem_space_slot->wmes->value;
    if (current_goal->id.state_slot->wmes)
      tparams.current_s = current_goal->id.state_slot->wmes->value;
#endif
    if (current_goal->id.operator_slot->wmes)
      tparams.current_o = current_goal->id.operator_slot->wmes->value;
  }
  tparams.allow_cycle_counts = allow_cycle_counts;
  gs = trace_format_list_to_string (tf, object);
  tparams = saved_tparams;
  
  return gs;
}

/* ======================================================================
                   Printing Object and Stack Traces 

   Print_object_trace() takes an object (any symbol).  It prints
   the trace for that object.  Print_stack_trace() takes a (context)
   object (the g, ps, s, or op), the current goal, the "slot_type" (one
   of FOR_OPERATORS_TF, etc.), and a flag indicating whether to allow
   %dc and %ec escapes (this flag should normally be TRUE for watch 0
   traces but FALSE during a "pgs" command).  It prints the trace for
   that context object.
====================================================================== */

void print_object_trace (Symbol *object) {
  growable_string gs;

  current_agent(tf_printing_tc)  = get_new_tc_number();
  gs = object_to_trace_string (object);
  print_string (text_of_growable_string(gs));
  free_growable_string (gs);
}

void print_stack_trace (Symbol *object, Symbol *goal, int slot_type,
                        bool allow_cycle_counts) {
  growable_string gs;

  current_agent(tf_printing_tc)  = get_new_tc_number();
  gs = selection_to_trace_string (object, goal, slot_type, allow_cycle_counts);
  print_string (text_of_growable_string(gs));
  free_growable_string (gs);
}
