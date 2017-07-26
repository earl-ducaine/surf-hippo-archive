/*
 * $Id: recmem.c,v 1.12 1995/01/20 00:15:21 rempel Exp $
 * $Log: recmem.c,v $
 * Revision 1.12  1995/01/20  00:15:21  rempel
 * fixed SGI bug, moved in DEBUG_INSTANTIATIONS, for release of 6.2.4c
 *
 * Revision 1.11  1994/12/06  22:03:34  rempel
 * For 6.2.4b
 *
 * Revision 1.10  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.9  1994/08/23  10:37:18  portelli
 * For 6.2.4
 *
 * Revision 1.8  1994/07/01  15:56:34  portelli
 * For 6.2.2
 *
 * Revision 1.7  1994/06/09  21:17:56  portelli
 * Another fix for 6.2.1
 *
 * Revision 1.6  94/06/09  21:16:26  portelli
 * For 6.2.1
 * 
 * Revision 1.5  94/06/09  17:31:28  portelli
 * For 6.2.1
 * 
 * Revision 1.4  94/06/08  22:19:38  portelli
 * For 6.2.1
 * 
 * Revision 1.3  94/05/18  13:33:38  portelli
 * Soar 6.2.0 b
 * 
 * Revision 1.2  93/11/21  17:07:25  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:47:49  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:23:32  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  20:32:13  jtraub
 * Added RCS header information.
 * Split code out into other files.
 *
 */

/* =======================================================================

             Recognition Memory (Firer and Chunker) Routines
                   (Does not include the Rete net)

======================================================================= */

#include <ctype.h>
#ifdef __hpux
#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_XOPEN_SOURCE
#define _INCLUDE_HPUX_SOURCE
#include <sys/types.h>
#undef  _INCLUDE_POSIX_SOURCE
#undef  _INCLUDE_XOPEN_SOURCE
#endif /* __hpux */
#ifndef __SC__
#ifndef THINK_C
#include <sys/time.h>
#endif
#endif
#ifdef __hpux
#undef _INCLUDE_HPUX_SOURCE
#endif /* __hpux */
#include "soar.h"

/* Uncomment the following line to get instantiation printouts */
/* #define DEBUG_INSTANTIATIONS */

/* backtrace.c */
extern void backtrace_through_instantiation(instantiation *, goal_stack_level, condition *, int);
extern void trace_locals(goal_stack_level);
extern void trace_grounded_potentials(void);
extern bool trace_ungrounded_potentials(goal_stack_level);

/* chunk.c */
extern bool add_to_chunk_cond_set(chunk_cond_set *, chunk_cond *);
extern void chunk_instantiation(instantiation *, bool);
extern chunk_cond *make_chunk_cond_for_condition(condition *);

/* osupport.c */
extern void calculate_support_for_instantiation_preferences(instantiation *);
extern void calculate_compile_time_o_support(condition *, action *);

/* mvp 5-17-94 */
/* --------------------------------------------------------------------------
            Build Prohibit Preference List for Backtracing
--------------------------------------------------------------------------*/
preference *find_clone_for_level (preference *p, goal_stack_level level);

void build_prohibits_list (instantiation *inst) {
  condition *cond;
  preference *pref, *new_pref;

  for (cond=inst->top_of_instantiated_conditions; cond!=NIL; cond=cond->next) {
    cond->bt.prohibits = NIL;
    if (cond->type==POSITIVE_CONDITION && cond->bt.trace) {
      if (cond->bt.trace->slot) {
        pref = cond->bt.trace->slot->preferences[PROHIBIT_PREFERENCE_TYPE];
        while (pref) {
          new_pref = NIL;
          if (pref->inst->match_goal_level == inst->match_goal_level && pref->in_tm) {
            push (pref, cond->bt.prohibits);
            preference_add_ref (pref);
          } else {
            new_pref = find_clone_for_level (pref, inst->match_goal_level);
            if (new_pref) {
              if (new_pref->in_tm) {
                push (new_pref, cond->bt.prohibits);
                preference_add_ref (new_pref);
              }
            }
          }
          pref = pref->next;
        }
      }
    }
  }
}

/* -----------------------------------------------------------------------
                         Find Clone For Level

   This routines take a given preference and finds the clone of it whose
   match goal is at the given goal_stack_level.  (This is used to find the
   proper preference to backtrace through.)  If the given preference
   itself is at the right level, it is returned.  If there is no clone at
   the right level, NIL is returned.
----------------------------------------------------------------------- */

preference *find_clone_for_level (preference *p, goal_stack_level level) {
  preference *clone;

  if (! p) {
    /* --- if the wme doesn't even have a preference on it, we can't backtrace
       at all (this happens with I/O and some architecture-created wmes --- */
    return NIL;
  }

  /* --- look at pref and all of its clones, find one at the right level --- */

  if (p->inst->match_goal_level == level) return p;

  for (clone=p->next_clone; clone!=NIL; clone=clone->next_clone)
    if (clone->inst->match_goal_level==level) return clone;

  for (clone=p->prev_clone; clone!=NIL; clone=clone->prev_clone)
    if (clone->inst->match_goal_level==level) return clone;

  /* --- if none was at the right level, we can't backtrace at all --- */
  return NIL;
}
  
/* =======================================================================

                           Firer Utilities

======================================================================= */

/* -----------------------------------------------------------------------
                             Find Match Goal

   Given an instantiation, this routines looks at the instantiated
   conditions to find its match goal.  It fills in inst->match_goal and
   inst->match_goal_level.  If there is a match goal, match_goal is set
   to point to the goal identifier.  If no goal was matched, match_goal
   is set to NIL and match_goal_level is set to ATTRIBUTE_IMPASSE_LEVEL.
----------------------------------------------------------------------- */

void find_match_goal (instantiation *inst) {
  Symbol *lowest_goal_so_far;
  goal_stack_level lowest_level_so_far;
  condition *cond;
  Symbol *id;
  
  lowest_goal_so_far = NIL;
  lowest_level_so_far = -1;
  for (cond=inst->top_of_instantiated_conditions; cond!=NIL; cond=cond->next)
    if (cond->type==POSITIVE_CONDITION) {
      id = cond->bt.wme->id;
      if (id->id.isa_goal)
        if (cond->bt.level > lowest_level_so_far) {
          lowest_goal_so_far = id;
          lowest_level_so_far = cond->bt.level;
        }
    }
  
  inst->match_goal = lowest_goal_so_far;
  if (lowest_goal_so_far)
    inst->match_goal_level = lowest_level_so_far;
  else
    inst->match_goal_level = ATTRIBUTE_IMPASSE_LEVEL;
}

/* -----------------------------------------------------------------------

               Executing the RHS Actions of an Instantiation

   Execute_action() executes a given RHS action.  For MAKE_ACTION's, it
   returns the created preference structure, or NIL if an error occurs.
   For FUNCALL_ACTION's, it returns NIL.

   Instantiate_symbol() and instantiate_rhs_value() return the (symbol)
   instantiation of a symbol and rhs_value, respectively.  They return
   NIL if an error occurs.  These two routines take a new_id_level
   argument indicating what goal_stack_level a new id is to be created
   at, in case a gensym is needed for the instantiation of a variable.
   (BUGBUG I'm not sure this is really needed.)
----------------------------------------------------------------------- */

Symbol *instantiate_symbol (Symbol *sym, goal_stack_level new_id_level) {
  char new_id_letter;
  
  if (sym->common.symbol_type==VARIABLE_SYMBOL_TYPE) {
    if (sym->var.current_binding_value) {
      symbol_add_ref (sym->var.current_binding_value);
      return sym->var.current_binding_value;
    }
    new_id_letter = *(sym->var.name + 1);
    sym->var.current_binding_value = make_new_identifier (new_id_letter,
                                                          new_id_level);
    return sym->var.current_binding_value;
  }
  symbol_add_ref (sym);
  return sym;
}

Symbol *instantiate_rhs_value (rhs_value rv, goal_stack_level new_id_level) {
  list *fl;
  list *arglist;
  cons *c, *prev_c, *arg_cons;
  rhs_function *rf;
  Symbol *result;
  bool nil_arg_found;
  
  if (rhs_value_is_symbol(rv))
    return instantiate_symbol (rhs_value_to_symbol(rv), new_id_level);

  fl = rhs_value_to_funcall_list(rv);
  rf = fl->first;

  /* --- build up a list of the argument values --- */
  prev_c = NIL;
  nil_arg_found = FALSE;
  for (arg_cons=fl->rest; arg_cons!=NIL; arg_cons=arg_cons->rest) {
    allocate_cons (&c);
    c->first = instantiate_rhs_value (arg_cons->first, new_id_level);
    if (! c->first) nil_arg_found = TRUE;
    if (prev_c) prev_c->rest = c; else arglist = c;
    prev_c = c;
  }
  if (prev_c) prev_c->rest = NIL; else arglist = NIL;

  /* --- if all args were ok, call the function --- */
  if (!nil_arg_found)
    result = (*(rf->f))(arglist);
  else
    result = NIL;

  /* --- scan through arglist, dereference symbols and deallocate conses --- */
  for (c=arglist; c!=NIL; c=c->rest)
    if (c->first) symbol_remove_ref ((Symbol *)(c->first));
  free_list (arglist);

  return result;
}

preference *execute_action (action *a) {
  Symbol *id, *attr, *value, *referent;
  
  if (a->type==FUNCALL_ACTION) {
    value = instantiate_rhs_value (a->value, -1);
    if (value) symbol_remove_ref (value);
    return NIL;
  }

  id = instantiate_symbol (a->id, -1);
  if (id->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) {
    print_with_symbols ("Error: RHS makes a preference for %y (not an identifier)\n", id);
    symbol_remove_ref (id);
    return NIL;
  }
  
  attr = instantiate_symbol (a->attr, id->id.level);

  value = instantiate_rhs_value (a->value, id->id.level);
  if (!value) {
    symbol_remove_ref (id);
    symbol_remove_ref (attr);
    return NIL;
  }

  if (preference_is_binary(a->preference_type)) {
    referent = instantiate_rhs_value (a->referent, id->id.level);
    if (!referent) {
      symbol_remove_ref (id);
      symbol_remove_ref (attr);
      symbol_remove_ref (value);
      return NIL;
    }
  } else {
    referent = NIL;
  }

  return make_preference (a->preference_type, id, attr, value, referent);
}

/* -----------------------------------------------------------------------
                    Fill In New Instantiation Stuff

   This routine fills in a newly created instantiation structure with
   various information.   At input, the instantiation should have:
     - preferences_generated filled in; 
     - instantiated conditions filled in;
     - top-level positive conditions should have bt.wme, bt.level, and
       bt.trace filled in, but bt.wme and bt.trace shouldn't have their
       reference counts incremented yet.

   This routine does the following:
     - increments reference count on production;
     - fills in match_goal and match_goal_level;
     - for each top-level positive cond:
         replaces bt.trace with the preference for the correct level,
         updates reference counts on bt.pref and bt.wmetraces and wmes
     - for each preference_generated, adds that pref to the list of all
       pref's for the match goal
     - fills in backtrace_number;   
     - if "need_to_do_support_calculations" is TRUE, calculates o-support
       for preferences_generated;
----------------------------------------------------------------------- */

void fill_in_new_instantiation_stuff (instantiation *inst,
                                      bool need_to_do_support_calculations) {
  condition *cond;
  preference *p;
  goal_stack_level level;

  production_add_ref (inst->prod);
  
  find_match_goal (inst);

  level = inst->match_goal_level;

  /* Note: since we'll never backtrace through instantiations at the top
     level, it might make sense to not increment the reference counts
     on the wmes and preferences here if the instantiation is at the top
     level.  As it stands now, we could gradually accumulate garbage at
     the top level if we have a never-ending sequence of production
     firings at the top level that chain on each other's results.  (E.g.,
     incrementing a counter on every decision cycle.)  I'm leaving it this
     way for now, because if we go to S-Support, we'll (I think) need to
     save these around (maybe??). */

  for (cond=inst->top_of_instantiated_conditions; cond!=NIL; cond=cond->next)
    if (cond->type==POSITIVE_CONDITION) {
      wme_add_ref (cond->bt.wme);
      /* --- if trace is for a lower level, find one for this level --- */
      if (cond->bt.trace)
        if (cond->bt.trace->inst->match_goal_level > level)
          cond->bt.trace = find_clone_for_level (cond->bt.trace, level);
      if (cond->bt.trace) preference_add_ref (cond->bt.trace);
    }

  if (inst->match_goal) {
    for (p=inst->preferences_generated; p!=NIL; p=p->inst_next) {
      insert_at_head_of_dll (inst->match_goal->id.preferences_from_goal, p,
                             all_of_goal_next, all_of_goal_prev);
      p->on_goal_list = TRUE;
    }
  }
  inst->backtrace_number = 0;

  if (need_to_do_support_calculations)
    calculate_support_for_instantiation_preferences (inst);
}

/* =======================================================================

                          Main Firer Routines

   Init_firer() should be called at startup time.  Do_preference_phase()
   is called from the top level to run the whole preference phase.

   Preference phase follows this sequence:

   (1) Productions are fired for new matches.  As productions are fired,
   their instantiations are stored on the list newly_created_instantiations,
   linked via the "next" fields in the instantiation structure.  No
   preferences are actually asserted yet.
   
   (2) Instantiations are retracted; their preferences are retracted.

   (3) Preferences (except o-rejects) from newly_created_instantiations
   are asserted, and these instantiations are removed from the 
   newly_created_instantiations list and moved over to the per-production
   lists of instantiations of that production.

   (4) Finally, o-rejects are processed.
======================================================================= */

void init_firer (void) {
  init_memory_pool (&current_agent(instantiation_pool), sizeof(instantiation),
                    "instantiation");
}

/* --- Macro returning TRUE iff we're supposed to trace firings for the
   given instantiation, which should have the "prod" field filled in. --- */

#define trace_firings_of_inst(inst) \
  ((inst)->prod && \
   (current_agent(sysparams)[TRACE_FIRINGS_OF_USER_PRODS_SYSPARAM+(inst)->prod->type] || \
    ((inst)->prod->trace_firings)))

/* -----------------------------------------------------------------------
                         Create Instantiation

   This builds the instantiation for a new match, and adds it to
   newly_created_instantiations.  It also calls chunk_instantiation() to
   do any necessary chunk or justification building.
----------------------------------------------------------------------- */

void create_instantiation (production *prod,
                           struct token_struct *tok,
                           wme *w) {
  instantiation *inst;
  condition *cond;
  preference *pref;
  action *a;
  cons *c;
  bool need_to_do_support_calculations;
  bool trace_it;
  
  allocate_with_pool (&current_agent(instantiation_pool), &inst);
  inst->next = current_agent(newly_created_instantiations);
  current_agent(newly_created_instantiations) = inst;
  inst->prod = prod;
  inst->rete_token = tok;
  inst->rete_wme = w;
  inst->okay_to_variablize = TRUE;
  inst->in_ms = TRUE;

  current_agent(production_being_fired) = inst->prod;
  prod->firing_count++;
  current_agent(production_firing_count)++;
  
  /* --- build the instantiated conditions, and bind LHS variables --- */
  p_node_to_conditions_and_nots (prod->p_node, tok, w,
                                 &(inst->top_of_instantiated_conditions),
                                 &(inst->bottom_of_instantiated_conditions),
                                 &(inst->nots));

  /* --- record the level of each of the wmes that was positively tested --- */
  for (cond=inst->top_of_instantiated_conditions; cond!=NIL; cond=cond->next) {
    if (cond->type==POSITIVE_CONDITION) {
      cond->bt.level = cond->bt.wme->id->id.level;
      cond->bt.trace = cond->bt.wme->preference;
    }
  }

  /* --- mark RHS unbound variables as "unbound" by setting binding=NIL --- */
  for (c=prod->rhs_unbound_variables; c!=NIL; c=c->rest)
    ((Symbol *)(c->first))->var.current_binding_value = NIL;

  /* --- print trace info --- */
  trace_it = trace_firings_of_inst (inst);
  if (trace_it) {
    if (get_printer_output_column()!=1) print ("\n");  /* AGR 617/634 */
    print ("Firing ");
    print_instantiation_with_wmes
      (inst, current_agent(sysparams)[TRACE_FIRINGS_WME_TRACE_TYPE_SYSPARAM]);
  }

  /* --- execute the RHS actions, collect the results --- */
  inst->preferences_generated = NIL;
  need_to_do_support_calculations = FALSE;
  for (a=prod->action_list; a!=NIL; a=a->next) {
    pref = execute_action (a);
    if (pref) {
      pref->inst = inst;
      insert_at_head_of_dll (inst->preferences_generated, pref,
                             inst_next, inst_prev);
      if (inst->prod->declared_support==DECLARED_O_SUPPORT)
        pref->o_supported = TRUE;
      else if (inst->prod->declared_support==DECLARED_NO_O_SUPPORT)
        pref->o_supported = FALSE;
      else if (a->support==O_SUPPORT) pref->o_supported = TRUE;
      else if (a->support==NO_O_SUPPORT) pref->o_supported = FALSE;
      else need_to_do_support_calculations = TRUE;
    }
  }

  /* --- fill in lots of other stuff --- */
  fill_in_new_instantiation_stuff (inst, need_to_do_support_calculations);

  /* --- print trace info: printing preferences --- */
  /* Note: can't move this up, since fill_in_new_instantiation_stuff gives
     the o-support info for the preferences we're about to print */
  if (trace_it && current_agent(sysparams)[TRACE_FIRINGS_PREFERENCES_SYSPARAM]) {
    print (" -->\n");
    for (pref=inst->preferences_generated; pref!=NIL; pref=pref->inst_next) {
      print (" ");
      print_preference (pref);
    }
  }

  /* mvp 5-17-94 */
  build_prohibits_list (inst);

  current_agent(production_being_fired) = NIL;

  /* --- build chunks/justifications if necessary --- */
  chunk_instantiation (inst, current_agent(sysparams)[LEARNING_ON_SYSPARAM]);

  /* MVP 6-8-94 */
  if (!current_agent(system_halted)) {
  /* --- call hook function --- */
    firing_hook (inst);
  }
}

/* -----------------------------------------------------------------------
                        Deallocate Instantiation

   This deallocates the given instantiation.  This should only be invoked
   via the possibly_deallocate_instantiation() macro.
----------------------------------------------------------------------- */

void deallocate_instantiation (instantiation *inst) {
  condition *cond;

  /* mvp 5-17-94 */
  list *c, *c_old;
  preference *pref;

#ifdef DEBUG_INSTANTIATIONS
  if (inst->prod)
    print_with_symbols ("\nDeallocate instantiation of %y",inst->prod->name);
#endif

  for (cond=inst->top_of_instantiated_conditions; cond!=NIL; cond=cond->next)
    if (cond->type==POSITIVE_CONDITION) {

      /* mvp 6-22-94, modified 94.01.17 by AGR with lotsa help from GAP */
     if (cond->bt.prohibits) {
       c_old = c = cond->bt.prohibits;
       cond->bt.prohibits = NIL;
       for (; c!=NIL; c=c->rest) {
	 pref = (preference *) c->first;
	 preference_remove_ref (pref);
       }
       free_list (c_old);
     }
     /* mvp done */

     wme_remove_ref (cond->bt.wme);
     if (cond->bt.trace) preference_remove_ref (cond->bt.trace);
   }

  deallocate_condition_list (inst->top_of_instantiated_conditions);
  deallocate_list_of_nots (inst->nots);
  if (inst->prod) production_remove_ref (inst->prod);
  free_with_pool (&current_agent(instantiation_pool), inst);
}

/* -----------------------------------------------------------------------
                         Retract Instantiation

   This retracts the given instantiation.
----------------------------------------------------------------------- */

void retract_instantiation (instantiation *inst) {
  preference *pref, *next;
  bool retracted_a_preference;
  bool trace_it;

  /* --- call hook function --- */
  retraction_hook (inst);
  
  retracted_a_preference = FALSE;
  
  trace_it = trace_firings_of_inst (inst);

  /* --- retract any preferences that are in TM and aren't o-supported --- */
  pref = inst->preferences_generated;
  while (pref!=NIL) {
    next = pref->inst_next;
    if (pref->in_tm && (! pref->o_supported)) {

      if (trace_it) {
        if (!retracted_a_preference) {
	  if (get_printer_output_column()!=1) print ("\n");  /* AGR 617/634 */
          print ("Retracting ");
          print_instantiation_with_wmes
            (inst, current_agent(sysparams)[TRACE_FIRINGS_WME_TRACE_TYPE_SYSPARAM]);
          if (current_agent(sysparams)[TRACE_FIRINGS_PREFERENCES_SYSPARAM]) print (" -->");
        }
        if (current_agent(sysparams)[TRACE_FIRINGS_PREFERENCES_SYSPARAM]) {
          print (" ");
          print_preference (pref);
        }
      }

      remove_preference_from_tm (pref);
      retracted_a_preference = TRUE;
    }
    pref = next;
  }

  /* --- remove inst from list of instantiations of this production --- */
  remove_from_dll (inst->prod->instantiations, inst, next, prev);

  /* --- if retracting a justification, excise it --- */
  /*
   * if the reference_count on the production is 1 (or less) then the
   * only thing supporting this justification is the instantiation, hence
   * it has already been excised, and doing it again is wrong.
   */
  if (inst->prod->type==JUSTIFICATION_PRODUCTION_TYPE &&
      inst->prod->reference_count > 1)
    excise_production (inst->prod, FALSE);
  
  /* --- mark as no longer in MS, and possibly deallocate  --- */
  inst->in_ms = FALSE;
  possibly_deallocate_instantiation (inst);
}

/* -----------------------------------------------------------------------
                         Assert New Preferences

   This routine scans through newly_created_instantiations, asserting
   each preference generated except for o-rejects.  It also removes
   each instantiation from newly_created_instantiations, linking each
   onto the list of instantiations for that particular production.
   O-rejects are bufferred and handled after everything else.

   Note that some instantiations on newly_created_instantiations are not
   in the match set--for the initial instantiations of chunks/justifications,
   if they don't match WM, we have to assert the o-supported preferences
   and throw away the rest.
----------------------------------------------------------------------- */

void assert_new_preferences (void) {
  instantiation *inst, *next_inst;
  preference *pref, *next_pref;
  preference *o_rejects;

  o_rejects = NIL;  

  for (inst=current_agent(newly_created_instantiations); inst!=NIL; inst=next_inst) {
    next_inst = inst->next;
    if (inst->in_ms)
      insert_at_head_of_dll (inst->prod->instantiations, inst, next, prev);
    for (pref=inst->preferences_generated; pref!=NIL; pref=next_pref) {
      next_pref = pref->inst_next;
      if ((pref->type==REJECT_PREFERENCE_TYPE)&&(pref->o_supported)) {
        /* --- o-reject: just put it in the buffer for later --- */
        pref->next = o_rejects;
        o_rejects = pref;
      } else if (inst->in_ms || pref->o_supported) {
        /* --- normal case --- */
        add_preference_to_tm (pref);
      } else {
        /* --- inst. is refracted chunk, and pref. is not o-supported:
           remove the preference --- */
        /* --- first splice it out of the clones list--otherwise we might
           accidentally deallocate some clone that happens to have refcount==0
           just because it hasn't been asserted yet --- */
        if (pref->next_clone) pref->next_clone->prev_clone = pref->prev_clone;
        if (pref->prev_clone) pref->prev_clone->next_clone = pref->next_clone;
        pref->next_clone = pref->prev_clone = NIL;
        /* --- now add then remove ref--this should result in deallocation */
        preference_add_ref (pref);
        preference_remove_ref (pref);
      }
    }
  }

  if (o_rejects) process_o_rejects_and_deallocate_them (o_rejects);
}

/* -----------------------------------------------------------------------
                          Do Preference Phase

   This routine is called from the top level to run the preference phase.
----------------------------------------------------------------------- */

void do_preference_phase (void) {
  production *prod;
  struct token_struct *tok;
  wme *w;
  instantiation *inst;
#ifdef DETAILED_TIMING_STATS
  struct timeval saved_start_tv;
#endif

/* AGR 617/634:  These are 2 bug reports that report the same problem,
   namely that when 2 chunk firings happen in succession, there is an
   extra newline printed out.  The simple fix is to monitor
   get_printer_output_column and see if it's at the beginning of a line
   or not when we're ready to print a newline.  94.11.14 */

#ifdef DETAILED_TIMING_STATS
  start_timer (&saved_start_tv);
#endif

  if (current_agent(sysparams)[TRACE_PHASES_SYSPARAM]) print ("\n--- Preference Phase ---\n");
  current_agent(newly_created_instantiations) = NIL;

  /* MVP 6-8-94 */
  while (get_next_assertion (&prod, &tok, &w)) {
     if (current_agent(max_chunks_reached)) {
       current_agent(system_halted) = TRUE;
       return;
     }
     create_instantiation (prod, tok, w);
   }

#ifdef DETAILED_TIMING_STATS
  stop_timer (&saved_start_tv, &current_agent(create_instantiations_cpu_time));
#endif

  assert_new_preferences ();
 
  while (get_next_retraction (&inst))
    retract_instantiation (inst);

#ifdef DETAILED_TIMING_STATS
  stop_timer (&saved_start_tv, &current_agent(preference_phase_cpu_time));
#endif
}

