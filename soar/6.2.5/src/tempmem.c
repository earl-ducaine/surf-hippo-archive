/*
 * $Id: tempmem.c,v 1.5 1994/11/23 16:40:34 rempel Exp $
 * $Log: tempmem.c,v $
 * Revision 1.5  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.4  1994/08/23  10:39:17  portelli
 * For 6.2.4
 *
 * Revision 1.3  1994/05/18  13:34:21  portelli
 * Soar 6.2.0 b
 *
 * Revision 1.2  93/11/21  17:24:59  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:48:22  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:25:24  jtraub
 * 6.1_checkin
 *
 * Revision 9.1  1993/05/10  20:33:24  jtraub
 * initial checkin.
 *
 */

/* ==========================================================================
             Temporary Memory and Slot routines for Soar 6
========================================================================== */

/* Debugging stuff:  #define DEBUG_SLOTS to get slot printouts */

/* #define DEBUG_SLOTS */

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

/* **********************************************************************

                        Temporary Memory

********************************************************************** */

/* ======================================================================

                        Slot Management Routines

   Find_slot() looks for an existing slot for a given id/attr pair, and
   returns it if found.  If no such slot exists, it returns NIL.
   Make_slot() looks for an existing slot for a given id/attr pair,
   returns it if found, and otherwise creates a new slot and returns it.

   Mark_slot_as_changed() is called by the preference manager whenever
   the preferences for a slot change.  This updates the list of
   changed_slots and highest_goal_whose_context_changed for use by the
   decider.
====================================================================== */

slot *find_slot (Symbol *id, Symbol *attr) {
  slot *s;

  for (s=id->id.slots; s!=NIL; s=s->next)
    if (s->attr==attr) return s;
  return NIL;
}

slot *make_slot (Symbol *id, Symbol *attr) {
  slot *s;
  int i;

  for (s=id->id.slots; s!=NIL; s=s->next)
    if (s->attr==attr) return s;
  allocate_with_pool (&current_agent(slot_pool), &s);
  insert_at_head_of_dll (id->id.slots, s, next, prev);
#ifndef NNPSCM
  if ((id->id.isa_goal) &&
      ((attr==current_agent(problem_space_symbol)) ||
       (attr==current_agent(state_symbol)) ||
       (attr==current_agent(operator_symbol))))
    s->isa_context_slot = TRUE;
  else
    s->isa_context_slot = FALSE;
#else
  if ((id->id.isa_goal) &&
      (attr==current_agent(operator_symbol)))
    s->isa_context_slot = TRUE;
  else
    s->isa_context_slot = FALSE;
#endif
  s->changed = NIL;
  s->acceptable_preference_changed = NIL;
  s->id = id;
  s->attr = attr;
  symbol_add_ref (id);
  symbol_add_ref (attr);
  s->wmes = NIL;
  s->all_preferences = NIL;
  for (i=0; i<NUM_PREFERENCE_TYPES; i++) s->preferences[i] = NIL;
  s->impasse_type = NONE_IMPASSE_TYPE;
  s->impasse_id = NIL;
  s->acceptable_preference_wmes = NIL;
  s->marked_for_possible_removal = FALSE;
  return s;  
}

void mark_slot_as_changed (slot *s) {
  dl_cons *dc;
  
  if (s->isa_context_slot) {
    if (current_agent(highest_goal_whose_context_changed)) {
      if (s->id->id.level <
          current_agent(highest_goal_whose_context_changed)->id.level)
        current_agent(highest_goal_whose_context_changed) = s->id;
    } else {
      current_agent(highest_goal_whose_context_changed) = s->id;
    }
    s->changed = (dl_cons *)s;  /* just make it nonzero */
  } else {
    if (! s->changed) {
      allocate_with_pool (&current_agent(dl_cons_pool), &dc);
      dc->item = s;
      s->changed = dc;
      insert_at_head_of_dll (current_agent(changed_slots), dc, next, prev);
    }
  }
}

/* -----------------------------------------------------------------
                      Slot Garbage Collection

   Old slots are garbage collected as follows:  whenever we notice that
   the last preference has been removed from a slot, we call
   mark_slot_for_possible_removal().  We don't deallocate the slot
   right away, because there might still be wmes in it, or we might
   be about to add a new preference to it (through some later action
   of the same production firing, for example).

   At the end of the phase, we call remove_garbage_slots(), which
   scans through each marked slot and garbage collects it if it has
   no wmes or preferences.
----------------------------------------------------------------- */

void mark_slot_for_possible_removal (slot *s) {
  if (s->marked_for_possible_removal) return;
  s->marked_for_possible_removal = TRUE;
  push (s, current_agent(slots_for_possible_removal));
}

void remove_garbage_slots (void) {
  cons *c;
  slot *s;
  
  while (current_agent(slots_for_possible_removal)) {
    c = current_agent(slots_for_possible_removal);
    current_agent(slots_for_possible_removal) = current_agent(slots_for_possible_removal)->rest;
    s = c->first;
    free_cons (c);
    
    if (s->wmes || s->all_preferences) {
      /* --- don't deallocate it if it still has any wmes or preferences --- */
      s->marked_for_possible_removal = FALSE;
      continue;
    }
    
    /* --- deallocate the slot --- */
#ifdef DEBUG_SLOTS
    print_with_symbols ("\nDeallocate slot %y ^%y", s->id, s->attr);
#endif
    
    if (s->changed && (! s->isa_context_slot)) {
      remove_from_dll (current_agent(changed_slots), s->changed, next, prev);
      free_with_pool (&current_agent(dl_cons_pool), s->changed);
    }
    remove_from_dll (s->id->id.slots, s, next, prev);
    symbol_remove_ref (s->id);
    symbol_remove_ref (s->attr);
    free_with_pool (&current_agent(slot_pool), s);
  }
}
