/*
 * $Id: io.c,v 1.5 1994/11/23 16:40:34 rempel Exp $
 * $Log: io.c,v $
 * Revision 1.5  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.4  1994/08/23  10:34:57  portelli
 * For 6.2.4
 *
 * Revision 1.3  1994/05/18  13:32:42  portelli
 * Soar 6.2.0 b
 *
 * Revision 1.2  93/11/21  16:58:43  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:47:29  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:21:07  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  19:34:05  jtraub
 * Added RCS header information
 *
 */

/* ==================================================================
                         I/O Code for Soar 6

         General Soar I/O System Routines, and Text I/O Routines

   See comments in soar.h for more information.
================================================================== */
 
#include <ctype.h>
#include <errno.h>
#ifdef __hpux
#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_XOPEN_SOURCE
#define _INCLUDE_HPUX_SOURCE
#endif /* __hpux */
#ifndef __SC__
#ifndef THINK_C
#include <sys/types.h>
#endif
#endif
#ifdef __hpux
#undef _INCLUDE_XOPEN_SOURCE
#undef _INCLUDE_POSIX_SOURCE
#endif /* __hpux */
#ifndef __SC__
#ifndef THINK_C
#include <sys/time.h>
#endif
#endif
#ifdef __hpux
#undef _INCLUDE_HPUX_SOURCE
#endif /* __hpux */

#ifdef _AIX
#include <sys/select.h>
#endif

#include "soar.h"

/* ====================================================================
                  Adding New Input and Output Functions

   The system maintains a list of all the input functions to be called
   every input cycle, and another list of all the symbol-to-function
   mappings for output commands.  Add_input_function() and
   add_output_function() should be called at system startup time to 
   install each I/O function.
==================================================================== */

void add_input_function (input_function f) {
  input_function_info *ifi;

  ifi = allocate_memory (sizeof(input_function_info),
                         MISCELLANEOUS_MEM_USAGE);
  ifi->next = current_agent(input_functions);
  current_agent(input_functions) = ifi;
  ifi->f = f;
}

void add_output_function (char *output_link_name, output_function f) {
  Symbol *sym;
  output_function_info *ofi;

  sym = make_sym_constant (output_link_name);
  for (ofi=current_agent(output_functions); ofi!=NIL; ofi=ofi->next)
    if (ofi->link_name == sym) {
      print ("Error: tried to add_output_function with duplicate name %s\n",
             output_link_name);
      abort_with_fatal_error();
    }
  ofi = allocate_memory (sizeof(output_function_info),
                         MISCELLANEOUS_MEM_USAGE);
  ofi->next = current_agent(output_functions);
  current_agent(output_functions) = ofi;
  ofi->f = f;
  ofi->link_name = sym;
}

/* ====================================================================
                            Input Routines

   Get_new_io_identifier(), get_io_sym_constant(), get_io_int_constant(),
   and get_io_float_constant() just call the appropriate symbol table
   routines.  This has the effect of incrementing the reference count
   on the symbol (or creating one with a reference count of 1).
   Release_io_symbol() just decrements the reference count.

   Add_input_wme() and remove_input_wme() call the add_wme_to_wm() and
   remove_wme_from_wm() routines in decide.c to do their work.  

   Do_input_cycle() is the top-level routine which calls all the
   individual user-defined input functions, etc.  

   All this stuff is really simple, and consequently pretty vulnerable
   to buggy user-written I/O code.  A more sophisticated version would
   be bullet-proofed against bad arguments to get_xxx(), add_input_wme(),
   and remove_input_wme().  Right now add_input_wme() and remove_input_wme()
   do some error checking, but they're nowhere near bullet-proof.
==================================================================== */

Symbol *get_new_io_identifier (char first_letter) {
  return make_new_identifier (first_letter, TOP_GOAL_LEVEL);
}

Symbol *get_io_sym_constant (char *name) {
  return make_sym_constant (name);
}

Symbol *get_io_int_constant (long value) {
  return make_int_constant (value);
}

Symbol *get_io_float_constant (float value) {
  return make_float_constant (value);
}

void release_io_symbol (Symbol *sym) {
  symbol_remove_ref (sym);
}

wme *add_input_wme (Symbol *id, Symbol *attr, Symbol *value) {
  wme *w;

  /* --- a little bit of error checking --- */
  if (! (id && attr && value)) {
    print ("Error: an input routine gave a NULL argument to add_input_wme.\n");
    return NIL;
  }
  /* --- go ahead and add the wme --- */
  w = make_wme (id, attr, value, FALSE);
  insert_at_head_of_dll (id->id.input_wmes, w, next, prev);
  add_wme_to_wm (w);
  return w;
}

bool remove_input_wme (wme *w) {
  wme *temp;

  /* --- a little bit of error checking --- */
  if (!w) {
    print ("Error: an input routine called remove_input_wme on a NULL wme.\n");
    return FALSE;
  }
  for (temp=w->id->id.input_wmes; temp!=NIL; temp=temp->next)
    if (temp==w) break;
  if (!temp) {
    print ("Error: an input routine called remove_input_wme on a wme that\n");
    print ("isn't one of the input wmes currently in working memory.\n");
    return FALSE;
  }
  /* Note: for efficiency, it might be better to use a hash table for the
     above test, rather than scanning the linked list.  We could have one
     global hash table for all the input wmes in the system. */
  /* --- go ahead and remove the wme --- */
  remove_from_dll (w->id->id.input_wmes, w, next, prev);
  remove_wme_from_wm (w);
  return TRUE;
}


void do_input_cycle (void) {
  input_function_info *ifi;

  if (current_agent(sysparams)[TRACE_PHASES_SYSPARAM]) print ("\n--- Input Phase --- \n");

  if (current_agent(prev_top_state) && (!current_agent(top_state))) {
    /* --- top state was just removed --- */
#ifdef NNPSCM
    release_io_symbol (current_agent(io_header));
#endif    
    for (ifi=current_agent(input_functions); ifi!=NIL; ifi=ifi->next)
      (ifi->f)(TOP_STATE_JUST_REMOVED);
  } else if ((!current_agent(prev_top_state)) && current_agent(top_state)) {
    /* --- top state was just created --- */
#ifdef NNPSCM
    /* Create io structure on top state. */
    current_agent(io_header) = get_new_io_identifier ('I');
    current_agent(io_header_link) = add_input_wme (current_agent(top_state),
                                         current_agent(io_symbol),
                                         current_agent(io_header));
#endif
    for (ifi=current_agent(input_functions); ifi!=NIL; ifi=ifi->next)
      (ifi->f)(TOP_STATE_JUST_CREATED);
#ifndef NNPSCM
  /* Top state is never "changed" under NNPSCM. */
  } else if (current_agent(prev_top_state) != current_agent(top_state)) {
    /* --- top state was just changed --- */
    for (ifi=current_agent(input_functions); ifi!=NIL; ifi=ifi->next)
      (ifi->f)(TOP_STATE_JUST_REMOVED);
    for (ifi=current_agent(input_functions); ifi!=NIL; ifi=ifi->next)
      (ifi->f)(TOP_STATE_JUST_CREATED);
#endif
  }

  /* --- if there is a top state, do the normal input cycle --- */
  if (current_agent(top_state)) {
    for (ifi=current_agent(input_functions); ifi!=NIL; ifi=ifi->next)
      (ifi->f)(NORMAL_INPUT_CYCLE);
  }

  /* --- do any WM resulting changes --- */
  do_buffered_wm_and_ownership_changes();
  
  /* --- save current top state for next time --- */
  current_agent(prev_top_state) = current_agent(top_state);
}

/* ====================================================================
                          Output Routines

   Inform_output_module_of_wm_changes() and do_output_cycle() are the
   two top-level entry points to the output routines.  The former is
   called by the working memory manager, and the latter from the top-level
   phase sequencer.
  
   This module maintains information about all the existing output links
   and the identifiers and wmes that are in the transitive closure of them.
   On each output link wme, we put a pointer to an output_link structure.
   Whenever inform_output_module_of_wm_changes() is called, we look for
   new output links and modifications/removals of old ones, and update
   the output_link structures accordingly.

   Transitive closure information is kept as follows:  each output_link
   structure has a list of all the ids in the link's TC.  Each id in
   the system has a list of all the output_link structures that it's
   in the TC of.

   After some number of calls to inform_output_module_of_wm_changes(),
   eventually do_output_cycle() gets called.  It scans through the list
   of output links and calls the necessary output function for each
   link that has changed in some way (add/modify/remove).
==================================================================== */

/* --- output link statuses --- */
#define NEW_OL_STATUS 0                    /* just created it */
#define UNCHANGED_OL_STATUS 1              /* normal status */
#define MODIFIED_BUT_SAME_TC_OL_STATUS 2   /* some value in its TC has been
                                              modified, but the ids in its TC
                                              are the same */
#define MODIFIED_OL_STATUS 3               /* the set of ids in its TC has
                                              changed */
#define REMOVED_OL_STATUS 4                /* link has just been removed */

/* --------------------------------------------------------------------
                   Output Link Status Updates on WM Changes

   Top-state link changes:

     For wme addition: (top-state ^link-attr anything)
        create new output_link structure; mark it "new"
     For wme removal:  (top-state ^link-attr anything)
        mark the output_link "removed"

   TC of existing link changes:

     For wme addition or removal: (<id> ^att constant):
       for each link in associated_output_links(id), 
         mark link "modified but same tc" (unless it's already marked
         some other more serious way)
 
     For wme addition or removal: (<id> ^att <id2>):
       for each link in associated_output_links(id), 
         mark link "modified" (unless it's already marked
         some other more serious way)

   Note that we don't update all the TC information after every WM change.
   The TC info doesn't get updated until do_output_cycle() is called.
-------------------------------------------------------------------- */

void update_for_top_state_wme_addition (wme *w) {
  output_link *ol;
  output_function_info *ofi;

  /* --- check whether the attribute is an output function --- */
  for (ofi=current_agent(output_functions); ofi!=NIL; ofi=ofi->next)
    if (ofi->link_name == w->attr) break;
  if (!ofi) return;
  
  /* --- create new output link structure --- */
  allocate_with_pool (&current_agent(output_link_pool), &ol);
  insert_at_head_of_dll (current_agent(existing_output_links), ol, next, prev);

  ol->status = NEW_OL_STATUS;
  ol->link_wme = w;
  wme_add_ref (w);
  ol->ids_in_tc = NIL;
  ol->ofi = ofi;
  /* --- make wme point to the structure --- */
  w->output_link = ol;
}

void update_for_top_state_wme_removal (wme *w) {
  if (! w->output_link) return;
  w->output_link->status = REMOVED_OL_STATUS;
}

void update_for_io_wme_change (wme *w) {
  cons *c;
  output_link *ol;
  
  for (c=w->id->id.associated_output_links; c!=NIL; c=c->rest) {
    ol = c->first;
    if (w->value->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) {
      /* --- mark ol "modified" --- */
      if ((ol->status==UNCHANGED_OL_STATUS) ||
          (ol->status==MODIFIED_BUT_SAME_TC_OL_STATUS))
        ol->status = MODIFIED_OL_STATUS;
    } else {
      /* --- mark ol "modified but same tc" --- */
      if (ol->status==UNCHANGED_OL_STATUS)
        ol->status = MODIFIED_BUT_SAME_TC_OL_STATUS;
    }
  }
}

void inform_output_module_of_wm_changes (list *wmes_being_added,
                                         list *wmes_being_removed) {
  cons *c;
  wme *w;

  for (c=wmes_being_added; c!=NIL; c=c->rest) {
    w = c->first;
#ifdef NNPSCM
    if (w->id==current_agent(io_header)) update_for_top_state_wme_addition (w);
#else
    if (w->id==current_agent(top_state)) update_for_top_state_wme_addition (w);
#endif
    if (w->id->id.associated_output_links) update_for_io_wme_change (w);
  }
  for (c=wmes_being_removed; c!=NIL; c=c->rest) {
    w = c->first;
#ifdef NNPSCM
    if (w->id==current_agent(io_header)) update_for_top_state_wme_removal (w);
#else
    if (w->id==current_agent(top_state)) update_for_top_state_wme_removal (w);
#endif
    if (w->id->id.associated_output_links) update_for_io_wme_change (w);
  }
}

/* --------------------------------------------------------------------
                     Updating Link TC Information

   We make no attempt to do the TC updating intelligently.  Whenever the
   TC changes, we throw away all the old TC info and recalculate the new
   TC from scratch.  I figure that this part of the system won't get
   used very frequently and I hope it won't be a time hog.

   Remove_output_link_tc_info() and calculate_output_link_tc_info() are
   the main routines here.
-------------------------------------------------------------------- */

void remove_output_link_tc_info (output_link *ol) {
  cons *c, *prev_c;
  Symbol *id;

  while (ol->ids_in_tc) {  /* for each id in the old TC... */
    c = ol->ids_in_tc;
    ol->ids_in_tc = c->rest;
    id = c->first;
    free_cons (c);

    /* --- remove "ol" from the list of associated_output_links(id) --- */
    prev_c = NIL;
    for (c=id->id.associated_output_links; c!=NIL; prev_c=c, c=c->rest)
      if (c->first == ol) break;
    if (!c) {
      print ("Internal error: can't find output link in id's list\n");
      abort_with_fatal_error();
    }
    if (prev_c) prev_c->rest = c->rest;
      else id->id.associated_output_links = c->rest;
    free_cons (c);
    symbol_remove_ref (id);
  }
}


void add_id_to_output_link_tc (Symbol *id) {
  slot *s;
  wme *w;
  
  /* --- if id is already in the TC, exit --- */
  if (id->id.tc_num == current_agent(output_link_tc_num)) return;
  id->id.tc_num = current_agent(output_link_tc_num);
  
  
  /* --- add id to output_link's list --- */
  push (id, current_agent(output_link_for_tc)->ids_in_tc);
  symbol_add_ref (id);  /* make sure the id doesn't get deallocated before we
                           have a chance to free the cons cell we just added */
  
  /* --- add output_link to id's list --- */
  push (current_agent(output_link_for_tc), id->id.associated_output_links);
  
  /* --- do TC through working memory --- */
  /* --- scan through all wmes for all slots for this id --- */
  for (w=id->id.input_wmes; w!=NIL; w=w->next)
    if (w->value->common.symbol_type==IDENTIFIER_SYMBOL_TYPE)
      add_id_to_output_link_tc (w->value);
  for (s=id->id.slots; s!=NIL; s=s->next)
    for (w=s->wmes; w!=NIL; w=w->next)
      if (w->value->common.symbol_type==IDENTIFIER_SYMBOL_TYPE)
        add_id_to_output_link_tc (w->value);
  /* don't need to check impasse_wmes, because we couldn't have a pointer
     to a goal or impasse identifier */
}

void calculate_output_link_tc_info (output_link *ol) {
  /* --- if link doesn't have any substructure, there's no TC --- */
  if (ol->link_wme->value->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) return;

  /* --- do TC starting with the link wme's value --- */
  current_agent(output_link_for_tc) = ol;
  current_agent(output_link_tc_num) = get_new_tc_number();
  add_id_to_output_link_tc (ol->link_wme->value);
}

/* --------------------------------------------------------------------
                    Building the list of IO_Wme's

   These routines create and destroy the list of io_wme's in the TC
   of a given output_link.  Get_io_wmes_for_output_link() and
   deallocate_io_wme_list() are the main entry points.  The TC info
   must have already been calculated for the given output link before
   get_io_wmes_for_output_link() is called.
-------------------------------------------------------------------- */

void add_wme_to_collected_io_wmes (wme *w) {
  io_wme *new;
  
  allocate_with_pool (&current_agent(io_wme_pool), &new);
  new->next = current_agent(collected_io_wmes);
  current_agent(collected_io_wmes) = new;
  new->id = w->id;
  new->attr = w->attr;
  new->value = w->value;
}

io_wme *get_io_wmes_for_output_link (output_link *ol) {
  cons *c;
  Symbol *id;
  slot *s;
  wme *w;

  current_agent(collected_io_wmes) = NIL;
  add_wme_to_collected_io_wmes (ol->link_wme);
  for (c=ol->ids_in_tc; c!=NIL; c=c->rest) {
    id = c->first;
    for (w=id->id.input_wmes; w!=NIL; w=w->next)
      add_wme_to_collected_io_wmes (w);
    for (s=id->id.slots; s!=NIL; s=s->next)
      for (w=s->wmes; w!=NIL; w=w->next)
        add_wme_to_collected_io_wmes (w);
  }
  return current_agent(collected_io_wmes);
}

void deallocate_io_wme_list (io_wme *iw) {
  io_wme *next;

  while (iw) {
    next = iw->next;
    free_with_pool (&current_agent(io_wme_pool), iw);
    iw = next;
  }
}

/* --------------------------------------------------------------------
                           Do Output Cycle

   This routine is called from the top-level sequencer, and it performs
   the whole output phase.  It scans through the list of existing output
   links, and takes the appropriate action on each one that's changed.
-------------------------------------------------------------------- */

void do_output_cycle (void) {
  output_link *ol, *next_ol;
  io_wme *iw_list;

  if (current_agent(sysparams)[TRACE_PHASES_SYSPARAM]) print ("\n--- Output Phase ---\n");
  
  for (ol=current_agent(existing_output_links); ol!=NIL; ol=next_ol) {
    next_ol = ol->next;

    switch (ol->status) {
    case UNCHANGED_OL_STATUS:
      /* --- link is unchanged, so do nothing --- */
      break;
      
    case NEW_OL_STATUS:

      /* --- calculate tc, and call the output function --- */
      calculate_output_link_tc_info (ol);
      iw_list = get_io_wmes_for_output_link (ol);
      (ol->ofi->f)(ADDED_OUTPUT_COMMAND, iw_list);
      deallocate_io_wme_list (iw_list);
      ol->status = UNCHANGED_OL_STATUS;
      break;
      
    case MODIFIED_BUT_SAME_TC_OL_STATUS:
      /* --- don't have to redo the TC, but do call the output function --- */
      iw_list = get_io_wmes_for_output_link (ol);
      (ol->ofi->f)(MODIFIED_OUTPUT_COMMAND, iw_list);
      deallocate_io_wme_list (iw_list);
      ol->status = UNCHANGED_OL_STATUS;
      break;
      
    case MODIFIED_OL_STATUS:
      /* --- redo the TC, and call the output function */
      remove_output_link_tc_info (ol);
      calculate_output_link_tc_info (ol);
      iw_list = get_io_wmes_for_output_link (ol);
      (ol->ofi->f)(MODIFIED_OUTPUT_COMMAND, iw_list);
      deallocate_io_wme_list (iw_list);
      ol->status = UNCHANGED_OL_STATUS;
      break;
      
    case REMOVED_OL_STATUS:
      /* --- call the output function, and free output_link structure --- */
      remove_output_link_tc_info (ol);            /* sets ids_in_tc to NIL */
      iw_list = get_io_wmes_for_output_link (ol); /* gives just the link wme */
      (ol->ofi->f)(REMOVED_OUTPUT_COMMAND, iw_list);
      deallocate_io_wme_list (iw_list);
      wme_remove_ref (ol->link_wme);
      remove_from_dll (current_agent(existing_output_links), ol, next, prev);
      free_with_pool (&current_agent(output_link_pool), ol);
      break;
    }
  } /* end of for ol */
}

/* --------------------------------------------------------------------
                          Get Output Value

   This is a simple utility function for use in users' output functions.
   It finds things in an io_wme chain.  It takes "outputs" (the io_wme
   chain), and "id" and "attr" (symbols to match against the wmes), and
   returns the value from the first wme in the chain with a matching id
   and attribute.  Either "id" or "attr" (or both) can be specified as
   "don't care" by giving NULL (0) pointers for them instead of pointers
   to symbols.  If no matching wme is found, the function returns a
   NULL pointer.
-------------------------------------------------------------------- */

Symbol *get_output_value (io_wme *outputs, Symbol *id, Symbol *attr) {
  io_wme *iw;

  for (iw=outputs; iw!=NIL; iw=iw->next)
    if ( ((id==NIL)||(id==iw->id)) &&
         ((attr==NIL)||(attr==iw->attr)) ) return iw->value;
  return NIL;
}

/* ====================================================================

                   Initialization for Soar I/O

==================================================================== */

void init_soar_io (void) {
  init_memory_pool (&current_agent(output_link_pool), sizeof(output_link), "output link");
  init_memory_pool (&current_agent(io_wme_pool), sizeof(io_wme), "io wme");
}

/* ====================================================================

                         Text I/O Routines

==================================================================== */

/* WINDOWS can't currently fully support TEXT-IO */
#ifndef _WINDOWS

/* --------------------------------------------------------------------
                       Symbol to File Mappings

   The text I/O system maintains a mapping from symbols to streams (i.e.,
   file descriptors).  This is used to translate from the value of a
   text-command ^text-input-stream or ^text-output-stream augmentation
   to the file descriptor to use for the I/O.  The routines
   add_text_io_symbol_to_file_mapping() and
   remove_text_io_symbol_to_file_mapping() should be called to set up
   and remove this mapping.  The trace_io argument, if TRUE, causes all
   the I/O done with the stream to be echoed to the screen.
-------------------------------------------------------------------- */

void add_text_io_symbol_to_file_mapping (Symbol *sym, int fd, bool trace_io) {
  text_io_symbol_to_file_mapping *new;

  new = allocate_memory (sizeof(text_io_symbol_to_file_mapping),
                         MISCELLANEOUS_MEM_USAGE);
  insert_at_head_of_dll (current_agent(text_io_symbol_to_file_mappings), new, next, prev);
  new->sym = sym;
  new->fd = fd;
  new->trace_io = trace_io;
}

void remove_text_io_symbol_to_file_mapping (Symbol *sym) {
  text_io_symbol_to_file_mapping *t;

  for (t=current_agent(text_io_symbol_to_file_mappings); t!=NIL; t=t->next)
    if (t->sym == sym) break;
  if (!t) return;
  remove_from_dll (current_agent(text_io_symbol_to_file_mappings), t, next, prev);
  free_memory (t, MISCELLANEOUS_MEM_USAGE);
}

text_io_symbol_to_file_mapping *lookup_text_io_mapping (Symbol *sym) {
  text_io_symbol_to_file_mapping *t;

  for (t=current_agent(text_io_symbol_to_file_mappings); t!=NIL; t=t->next)
    if (t->sym == sym) return t;
  return NIL;
}


/* --- buffer for the current line of text input --- */

char text_input_buffer[MAX_TEXT_INPUT_LINE_LENGTH+2];
int text_input_buffer_length;        /* current length of the input line */

/* --------------------------------------------------------------------

              Updating the ^Text-Environment Structure

-------------------------------------------------------------------- */

void create_text_environment (void) {
  /* --- create the new structure on the top state --- */
  current_agent(te_identifier) = get_new_io_identifier ('E');
  current_agent(text_environment_link) = add_input_wme (current_agent(top_state),
                                         current_agent(text_environment_symbol),
                                         current_agent(te_identifier));
  current_agent(te_input_stream_wme) = add_input_wme (current_agent(te_identifier),
                                       current_agent(text_input_stream_symbol),
                                       current_agent(stdin_symbol));
  current_agent(te_output_stream_wme) = add_input_wme (current_agent(te_identifier),
                                        current_agent(text_output_stream_symbol),
                                        current_agent(stdout_symbol));
  
  /* --- initialize the i/o channels to be standard input & output --- */
  current_agent(current_text_environment).input_channel =
    lookup_text_io_mapping (current_agent(stdin_symbol));
  current_agent(current_text_environment).output_channel =
    lookup_text_io_mapping (current_agent(stdout_symbol));
  
  /* --- don't want it to look like it just changed --- */
  current_agent(prev_text_environment) = current_agent(current_text_environment);
  
  /* --- no text input has been read yet --- */
  text_input_buffer_length = 0;
}

void remove_text_environment (void) {
  release_io_symbol (current_agent(te_identifier));
  /* don't actually have to remove the wme's--they've been gc'd already */
}

void update_text_environment (void) {
  /* --- if input channel changed, update the wme accordingly --- */
  if (current_agent(current_text_environment).input_channel !=
      current_agent(prev_text_environment).input_channel) {
    remove_input_wme (current_agent(te_input_stream_wme));
    current_agent(te_input_stream_wme) = add_input_wme (current_agent(te_identifier),
       current_agent(text_input_stream_symbol), current_agent(current_text_environment).input_channel->sym);
  }
  /* --- if output channel changed, update the wme accordingly --- */
  if (current_agent(current_text_environment).output_channel !=
      current_agent(prev_text_environment).output_channel) {
    remove_input_wme (current_agent(te_output_stream_wme));
    current_agent(te_output_stream_wme) = add_input_wme (current_agent(te_identifier),
      current_agent(text_output_stream_symbol), current_agent(current_text_environment).output_channel->sym);
  }
  /* --- save for next time --- */
  current_agent(prev_text_environment) = current_agent(current_text_environment);
}

/* --------------------------------------------------------------------

               Reading Available Text Input Characters

   Reading text input is tricky, since we don't want to just call
   fgets() or some other standard C routine--that would block and wait
   for the user to type something.  Instead, we have to use some Unix
   hackery by calling select().  This is ugly and probaby not portable
   to other OS's like the Mac.
-------------------------------------------------------------------- */

/* --- return TRUE iff a character is available --- */
bool text_input_available (void) {
#ifndef __SC__
#ifndef THINK_C
  int fd;
  int nfound;
  fd_set readfds;
  struct timeval timeout;

  fd = current_agent(current_text_environment).input_channel->fd;
  timeout.tv_sec = 0;
  timeout.tv_usec = 0;
  FD_ZERO (&readfds);
  FD_SET (fd, &readfds);

#ifdef __hpux
  nfound = select (fd + 1, (int *) &readfds, NIL, NIL, &timeout);
#else
  nfound = select (fd + 1, &readfds, NIL, NIL, &timeout);
#endif /* __hpux */

  /* Note: should include some .h file for select, but it doesn't seem to
     be in any standardized location */
  return (nfound == 1);
#else
	return(FALSE);
#endif
#else
	return(FALSE);
#endif
}

#ifdef USE_X_DISPLAY

bool get_available_text_input (void) {
  char * next_line;

  handle_soar_x_events();

  if (queue_is_empty(current_agent(text_input_queue))) {
    return FALSE;
  } else {
    queue_delete (current_agent(text_input_queue), 
		  (void * *) &next_line);
    strcpy (text_input_buffer, next_line);
    free_memory_block_for_string(next_line);

    if (current_agent(current_text_environment).input_channel->trace_io)
      print_string (text_input_buffer);

    return TRUE;
  }
}

#else

/* --- read available text, return TRUE iff a whole line has been read --- */
bool get_available_text_input (void) {
  char ch;
  int cc;

  while (text_input_available()) {
    cc = read (current_agent(current_text_environment).input_channel->fd, &ch, 1);
    if (cc != 1) return FALSE; /* i/o error occurred */
    text_input_buffer[text_input_buffer_length++] = ch;
    if (text_input_buffer_length >= MAX_TEXT_INPUT_LINE_LENGTH)
      text_input_buffer_length = MAX_TEXT_INPUT_LINE_LENGTH;
    if (ch=='\n') {
      text_input_buffer[text_input_buffer_length++] = 0;
      if (text_input_buffer_length >= MAX_TEXT_INPUT_LINE_LENGTH)
        print ("\nText Input Error: input line too long, truncated it.\n");
      if (current_agent(current_text_environment).input_channel->trace_io)
        print_string (text_input_buffer);
      return TRUE;
    }
  }
  return FALSE;
}

#endif

#endif /* _WINDOWS */

/* --------------------------------------------------------------------
                    Parsing a Line of Text Input

   Get_next_io_symbol_from_text_input_line (char **text_read_position) is
   the main text input parser.  It reads text from text_read_position
   and returns a (Symbol *) for the first item read.  It updates
   text_read_position to point to the next character not yet read.
   If end-of-line is reached without any symbol being read, NIL is
   returned.
-------------------------------------------------------------------- */

bool tio_constituent_char[256];
bool tio_whitespace[256];

Symbol *get_io_symbol_from_tio_constituent_string (char *input_string) {
  int int_val;
  float float_val;
  bool possible_id, possible_var, possible_sc, possible_ic, possible_fc;
  bool rereadable;
  
  determine_possible_symbol_types_for_string (input_string,
                                              strlen(input_string),
                                              &possible_id,
                                              &possible_var,
                                              &possible_sc,
                                              &possible_ic,
                                              &possible_fc,
                                              &rereadable);

  /* --- check whether it's an integer --- */
  if (possible_ic) {
    errno = 0;
    int_val = strtol (input_string,NULL,10);
    if (errno) {
      print ("Text Input Error: bad integer (probably too large)\n");
      return NIL;
    }
    return get_io_int_constant (int_val);
  }
    
  /* --- check whether it's a floating point number --- */
  if (possible_fc) {
    errno = 0;
    float_val = my_strtod (input_string,NULL,10); 
    if (errno) {
      print ("Text Input Error: bad floating point number\n");
      return NIL;
    }
    return get_io_float_constant (float_val);
  }
  
  /* --- otherwise it must be a symbolic constant --- */
  return get_io_sym_constant (input_string);
}

Symbol *get_next_io_symbol_from_text_input_line (char **text_read_position) {
  char *ch;
  char input_string[MAX_TEXT_INPUT_LINE_LENGTH+2];
  int input_lexeme_length;

  ch = *text_read_position;
  
  /* --- scan past any whitespace --- */
  while (tio_whitespace[(unsigned char)(*ch)]) ch++;

  /* --- if end of line, return NIL --- */
  if ((*ch=='\n')||(*ch==0)) { *text_read_position = ch; return NIL; }

  /* --- if not a constituent character, return single-letter symbol --- */
  if (! tio_constituent_char[(unsigned char)(*ch)]) {
    input_string[0] = *ch++;
    input_string[1] = 0;
    *text_read_position = ch;
    return get_io_sym_constant (input_string);
  }
    
  /* --- read string of constituents --- */
  input_lexeme_length = 0;
  while (tio_constituent_char[(unsigned char)(*ch)])
    input_string[input_lexeme_length++] = *ch++;

  /* --- return the appropriate kind of symbol --- */
  input_string[input_lexeme_length] = 0;
  *text_read_position = ch;
  return get_io_symbol_from_tio_constituent_string (input_string);
}

/* --------------------------------------------------------------------

                        Text Input Function

-------------------------------------------------------------------- */

/* Windows 3.1 can't fully support text-io */
#ifndef _WINDOWS

void put_new_text_input_line_into_wm (void) {
  Symbol *item;         /* item just read from the input line */
  Symbol *current_id;   /* we're about to add (current_id ^item item) */
  Symbol *prev_id;      /* we also need to add (prev_id ^next current_id) */
  char *read_position;
  
  /* --- remove previous text input line from wm --- */
  if (current_agent(text_input_link)) remove_input_wme (current_agent(text_input_link));

  /* --- setup for next input line --- */
  read_position = text_input_buffer;
  current_id = get_new_io_identifier ('T');
  prev_id = NIL;
  current_agent(text_input_link) = add_input_wme (current_agent(top_state), current_agent(text_input_symbol), current_id);

  /* --- iterate through each symbol in the input line --- */
  while (TRUE) {
    item = get_next_io_symbol_from_text_input_line (&read_position);
    if (!item) break;
    if (! current_id) current_id = get_new_io_identifier ('T');
    if (prev_id) add_input_wme (prev_id, current_agent(next_symbol), current_id);
    add_input_wme (current_id, current_agent(item_symbol), item);
    release_io_symbol (item);
    if (prev_id) release_io_symbol (prev_id);
    prev_id = current_id;
    current_id = NIL;
  }
  if (prev_id) release_io_symbol (prev_id);
  if (current_id) release_io_symbol (current_id);
}

void text_io_input_function (int mode) {
  bool line_ready;
  
  switch (mode) {
  case NORMAL_INPUT_CYCLE:
    update_text_environment ();
    line_ready = get_available_text_input ();
    if (line_ready) {
      put_new_text_input_line_into_wm ();
      text_input_buffer_length = 0;
    }
    break;
  case TOP_STATE_JUST_CREATED:
    create_text_environment();
    current_agent(text_input_link) = NIL;
    break;
  case TOP_STATE_JUST_REMOVED:
    remove_text_environment();
    current_agent(text_input_link) = NIL;
    break;
  }
}

/* --------------------------------------------------------------------

                Output Function for "text-command" Links

-------------------------------------------------------------------- */

void text_io_text_command_function (int mode, io_wme *outputs) {
  Symbol *status_id, *value;
  text_io_symbol_to_file_mapping *mapping;
  
  if (mode!=ADDED_OUTPUT_COMMAND) return;

  status_id = get_output_value (outputs, current_agent(top_state), current_agent(text_command_symbol));
  if (!status_id) return;
  
  /* --- look for ^text-input-stream augmentation --- */
  value = get_output_value (outputs, status_id, current_agent(text_input_stream_symbol));
  if (value) {
    mapping = lookup_text_io_mapping (value);
    if (mapping) {
      current_agent(current_text_environment).input_channel = mapping;
    } else {
      print_with_symbols ("Error in text-command: no stream named %y\n",value);
    }
  }
  
  /* --- look for ^text-output-stream augmentation --- */
  value = get_output_value (outputs, status_id, current_agent(text_output_stream_symbol));
  if (value) {
    mapping = lookup_text_io_mapping (value);
    if (mapping) {
      current_agent(current_text_environment).output_channel = mapping;
    } else {
      print_with_symbols ("Error in text-command: no stream named %y\n",value);
    }
  }
}

/* --------------------------------------------------------------------

                Output Function for "text-output" Links

-------------------------------------------------------------------- */

void text_io_text_output_function (int mode, io_wme *outputs) {
  Symbol *current_id, *next_id;
  Symbol *item, *type;
  char *output_string;
  int len;
  
  if (mode!=ADDED_OUTPUT_COMMAND) return;
  current_id = get_output_value (outputs, current_agent(top_state), current_agent(text_output_symbol));
  while (current_id) {
    /* --- make sure current_id is an identifier --- */
    if (current_id->common.symbol_type != IDENTIFIER_SYMBOL_TYPE) break;
    
    /* --- look at outputs to find aug's of current_id --- */
    item = get_output_value (outputs, current_id, current_agent(item_symbol));
    type = get_output_value (outputs, current_id, current_agent(type_symbol));
    next_id = get_output_value (outputs, current_id, current_agent(next_symbol));
    
    /* --- build the output string --- */
    if (type==current_agent(cc_symbol)) {
      if (item==current_agent(crlf_symbol)) output_string = "\n";
      else if (item==current_agent(space_symbol)) output_string = " ";
      else {
        print_with_symbols ("Error: unknown carriage-control symbol in text-output: %y\n:", item);
        output_string = "";
      }
    } else {
      output_string = symbol_to_string (item, FALSE, NIL);
    }
    
    /* --- output the item --- */
    len = strlen (output_string);
    if (len) {
      write (current_agent(current_text_environment).output_channel->fd, output_string, len);
      if (current_agent(current_text_environment).output_channel->trace_io)
        print_string (output_string);
    }
    
    /* --- go to the next item in the linked list --- */
    current_id = next_id;
  } /* end of while loop */
}

#endif /* _WINDOWS */

/* --------------------------------------------------------------------

                   Initialization for Text I/O

-------------------------------------------------------------------- */

char extra_tio_constituents[] = "+-._";

void init_text_io (void) {
  int i;
  
#ifndef _WINDOWS

  add_input_function (text_io_input_function);
  add_output_function ("text-command", text_io_text_command_function);
  add_output_function ("text-output", text_io_text_output_function);

  current_agent(text_environment_symbol) = get_io_sym_constant ("text-environment");
  current_agent(text_input_symbol) = get_io_sym_constant ("text-input");
  current_agent(text_input_stream_symbol) = get_io_sym_constant ("text-input-stream");
  current_agent(text_output_stream_symbol) = get_io_sym_constant ("text-output-stream");
  current_agent(text_output_symbol) = get_io_sym_constant ("text-output");
  current_agent(text_command_symbol) = get_io_sym_constant ("text-command");
  current_agent(stdin_symbol) = get_io_sym_constant ("*standard-input*");
  current_agent(stdout_symbol) = get_io_sym_constant ("*standard-output*");
  current_agent(next_symbol) = get_io_sym_constant ("next");
  current_agent(atom_symbol) = get_io_sym_constant ("atom");
  current_agent(cc_symbol) = get_io_sym_constant ("cc");
  current_agent(crlf_symbol) = get_io_sym_constant ("crlf");
  current_agent(space_symbol) = get_io_sym_constant ("space");

  add_text_io_symbol_to_file_mapping (current_agent(stdin_symbol), 0, FALSE);
  add_text_io_symbol_to_file_mapping (current_agent(stdout_symbol), 1, FALSE);

#endif /* _WINDOWS */

  /* --- setup constituent_char array --- */
  for (i=0; i<256; i++) tio_constituent_char[i] = isalnum(i);
  for (i=0; i<strlen(extra_tio_constituents); i++)
    tio_constituent_char[extra_tio_constituents[i]]=TRUE;
  
  /* --- setup whitespace array --- */
  for (i=0; i<256; i++) tio_whitespace[i] = isspace(i);
  tio_whitespace['\n']=FALSE;  /* for text i/o, crlf isn't whitespace */
}
