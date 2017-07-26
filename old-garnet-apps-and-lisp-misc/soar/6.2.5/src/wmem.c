/*
 * $Id: wmem.c,v 1.4 1994/11/23 16:40:34 rempel Exp $
 * $Log: wmem.c,v $
 * Revision 1.4  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.3  1994/08/23  10:39:49  portelli
 * For 6.2.4
 *
 * Revision 1.2  1993/11/21  17:26:27  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:48:24  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:25:59  jtraub
 * 6.1_checkin
 *
 * Revision 9.1  1993/05/10  20:33:46  jtraub
 * Initial checkin
 *
 */

/* ======================================================================
         Working memory routines for Soar 6
====================================================================== */

/* Debugging stuff:  #define DEBUG_WMES to get slot printouts */

/* #define DEBUG_WMES */

#ifdef __hpux
#define _INCLUDE_HPUX_SOURCE
#undef _STRUCT_TIMEVAL
#endif /* __hpux */
#ifndef __SC__
#ifndef THINK_C
#include <sys/time.h>
#endif
#endif
#ifdef __hpux
#undef _INCLUDE_HPUX_SOURCE
#define _STRUCT_TIMEVAL
#endif /* __hpux */
#include "soar.h"

/* ======================================================================

             Working Memory Management and Utility Routines

   Reset_wme_timetags() resets the wme timetag generator back to 1.
   This should be called during an init-soar.

   Make_wme() creates and returns a new wme.  The caller should add the
   wme onto the appropriate dll (e.g., my_slot->wmes) and should call
   add_wme_to_wm() on it.

   Add_wme_to_wm() and remove_wme_from_wm() make changes to WM.  Again,
   the caller is responsible for manipulating the appropriate dll.  WM
   changes don't actually get stuffed down the rete until the end of the
   phase, when do_buffered_wm_changes() gets be called.

   Remove_wme_list_from_wm() is a utility routine that scans through a
   list of wmes, linked by their "next" fields, and calls remove_wme_from_wm()
   on each one.

   Deallocate_wme() deallocates a wme.  This should only be invoked via
   the wme_remove_ref() macro.

   Find_name_of_object() is a utility function for finding the value of
   the ^name attribute on a given object (Symbol).  It returns the name,
   or NIL if the object has no name.
====================================================================== */


void reset_wme_timetags (void) {
  if (current_agent(num_existing_wmes) != 0) {
    print ("Internal warning:  wanted to reset wme timetag generator, but\n");
    print ("there are still some wmes allocated. (Probably a memory leak.)\n");
    print ("(Leaving timetag numbers alone.)\n");
    return;
  }
  current_agent(current_wme_timetag) = 1;
}

wme *make_wme (Symbol *id, Symbol *attr, Symbol *value, bool acceptable) {
  wme *w;

  current_agent(num_existing_wmes)++;
  allocate_with_pool (&current_agent(wme_pool), &w);
  w->id = id;
  w->attr = attr;
  w->value = value;
  symbol_add_ref (id);
  symbol_add_ref (attr);
  symbol_add_ref (value);
  w->acceptable = acceptable;
  w->timetag = current_agent(current_wme_timetag)++;
  w->reference_count = 0;
  w->preference = NIL;
  w->output_link = NIL;
  w->grounds_tc = 0;
  w->potentials_tc = 0;
  w->locals_tc = 0;
  return w;
}

/* --- lists of buffered WM changes --- */

void add_wme_to_wm (wme *w) {
  push (w, current_agent(wmes_to_add));
  if (w->value->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) {
    post_link_addition (w->id, w->value);
    if (w->attr==current_agent(problem_space_symbol)) w->value->id.isa_problem_space++;
    if (w->attr==current_agent(state_symbol)) w->value->id.isa_state++;
    if (w->attr==current_agent(operator_symbol)) w->value->id.isa_operator++;
  }
}

void remove_wme_from_wm (wme *w) {
  push (w, current_agent(wmes_to_remove));
  if (w->value->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) {
    post_link_removal (w->id, w->value);
    if (w->attr==current_agent(problem_space_symbol)) w->value->id.isa_problem_space--;
    if (w->attr==current_agent(state_symbol)) w->value->id.isa_state--;
    if (w->attr==current_agent(operator_symbol)) w->value->id.isa_operator--;
  }
}

void remove_wme_list_from_wm (wme *w) {
  wme *next_w;

  while (w) {
    next_w = w->next;
    remove_wme_from_wm (w);
    w = next_w;
  }
}

void do_buffered_wm_changes (void) {
  cons *c, *next_c;
  wme *w;
#ifdef DETAILED_TIMING_STATS
  struct timeval start_tv;
#endif

  /* --- if no wme changes are buffered, do nothing --- */
  if (!current_agent(wmes_to_add) && !current_agent(wmes_to_remove)) return;

  /* --- call output module in case any changes are output link changes --- */
  inform_output_module_of_wm_changes (current_agent(wmes_to_add), current_agent(wmes_to_remove));

  /* --- call hook routine --- */
  wm_changes_hook (current_agent(wmes_to_add), current_agent(wmes_to_remove));

  /* --- stuff wme changes through the rete net --- */
#ifdef DETAILED_TIMING_STATS
  start_timer (&start_tv);
#endif
  for (c=current_agent(wmes_to_add); c!=NIL; c=c->rest) add_wme_to_rete (c->first);
  for (c=current_agent(wmes_to_remove); c!=NIL; c=c->rest) remove_wme_from_rete (c->first);
#ifdef DETAILED_TIMING_STATS
  stop_timer (&start_tv, &current_agent(match_cpu_time));
#endif

  /* --- do tracing and cleanup stuff --- */
  for (c=current_agent(wmes_to_add); c!=NIL; c=next_c) {
    next_c = c->rest;
    w = c->first;
    if (current_agent(sysparams)[TRACE_WM_CHANGES_SYSPARAM]) {
      print ("=>WM: ");
      print_wme (w); 
    }
    wme_add_ref (w);
    free_cons (c);
    current_agent(wme_addition_count)++;
  }
  for (c=current_agent(wmes_to_remove); c!=NIL; c=next_c) {
    next_c = c->rest;
    w = c->first;
    if (current_agent(sysparams)[TRACE_WM_CHANGES_SYSPARAM]) {
       print ("<=WM: "); 
       print_wme (w); 
    }
    wme_remove_ref (w);
    free_cons (c);
    current_agent(wme_removal_count)++;
  }
  current_agent(wmes_to_add) = NIL;
  current_agent(wmes_to_remove) = NIL;
}

void deallocate_wme (wme *w) {
#ifdef DEBUG_WMES  
  print_with_symbols ("\nDeallocate wme: ");
  print_wme (w);
#endif
  symbol_remove_ref (w->id);
  symbol_remove_ref (w->attr);
  symbol_remove_ref (w->value);
  free_with_pool (&current_agent(wme_pool), w);
  current_agent(num_existing_wmes)--;
}

Symbol *find_name_of_object (Symbol *object) {
  slot *s;

  if (object->common.symbol_type != IDENTIFIER_SYMBOL_TYPE) return NIL;
  s = find_slot (object, current_agent(name_symbol));
  if (! s) return NIL;
  if (! s->wmes) return NIL;
  return s->wmes->value;
}
