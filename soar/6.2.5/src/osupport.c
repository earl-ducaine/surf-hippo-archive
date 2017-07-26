/*
 * $Id: osupport.c,v 1.8 1994/11/23 16:40:34 rempel Exp $
 * $Log: osupport.c,v $
 * Revision 1.8  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.7  1994/08/23  10:35:55  portelli
 * For 6.2.4
 *
 * Revision 1.6  1994/07/01  15:56:58  portelli
 * For 6.2.2
 *
 * Revision 1.5  1994/06/09  17:32:56  portelli
 * Test
 *
 * Revision 1.4  94/06/08  22:20:24  portelli
 * For 6.2.1
 *
 * Revision 1.3  94/05/18  13:32:53  portelli
 * Soar 6.2.0 b
 *
 * Revision 1.2  93/11/21  17:02:00  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:48:17  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:22:10  jtraub
 * 6.1_checkin
 *
 * Revision 9.1  1993/05/10  20:31:37  jtraub
 * Initial checking
 *
 */

/* =========================================================================
             O Support calculation routines.
========================================================================= */

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

extern list *collect_root_variables(condition *, tc_number, bool);

/* backtrace.c */
extern void backtrace_through_instantiation(instantiation *, goal_stack_level, condition *, int);
extern void trace_locals(goal_stack_level);
extern void trace_grounded_potentials(void);
extern bool trace_ungrounded_potentials(goal_stack_level);

/* chunk.c */
extern bool add_to_chunk_cond_set(chunk_cond_set *, chunk_cond *);
extern void chunk_instantiation(instantiation *, bool);
extern chunk_cond *make_chunk_cond_for_condition(condition *);

/* recmem.c */
extern preference *find_clone_for_level(preference *, goal_stack_level);
extern void fill_in_new_instantiation_stuff(instantiation *, bool);
extern void deallocate_instantiation(instantiation *);
extern void do_preference_phase(void);

/* -----------------------------------------------------------------------
                  O-Support Transitive Closure Routines

   These routines are used by the o-support calculations to mark transitive
   closures through TM (= WM+PM) plus (optionally) the RHS-generated pref's.

   The caller should first call begin_os_tc (rhs_prefs_or_nil).  Then
   add_to_os_tc (id) should be called any number of times to add stuff
   to the TC.  (Note that the rhs_prefs shouldn't be modified between the
   begin_os_tc() call and the last add_to_os_tc() call.)

   Each identifier in the TC is marked with id.tc_num=o_support_tc; the
   caller can check for TC membership by looking at id.tc_num on any id.
----------------------------------------------------------------------- */

#ifndef NNPSCM
#define add_to_os_tc_if_needed(sym) \
  { if ((sym)->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) \
      add_to_os_tc (sym); }
#else 
#define add_to_os_tc_if_needed(sym) \
  { if ((sym)->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) \
      add_to_os_tc (sym,FALSE); }
#endif

#ifndef NNPSCM
#define add_to_os_tc_if_id(sym) \
  { if ((sym)->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) \
      add_to_os_tc (sym); }
#else 
#define add_to_os_tc_if_id(sym,flag) \
  { if ((sym)->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) \
      add_to_os_tc (sym,flag); }
#endif

/* SBH 4/14/93
 * For NNPSCM, we must exclude the operator slot from the transitive closure of a state.
 * Do that by passing a boolean argument, "isa_state" to this routine.
 * If it isa_state, check for the operator slot before the recursive call.
 */

#ifndef NNPSCM
void add_to_os_tc (Symbol *id) {
#else
void add_to_os_tc (Symbol *id, bool isa_state) {
#endif

  slot *s;
  preference *pref;
  wme *w;

  /* --- if id is already in the TC, exit; else mark it as in the TC --- */
  if (id->id.tc_num==current_agent(o_support_tc)) return;
  id->id.tc_num = current_agent(o_support_tc);
  
  /* --- scan through all preferences and wmes for all slots for this id --- */
  for (w=id->id.input_wmes; w!=NIL; w=w->next)
    add_to_os_tc_if_needed (w->value);
  for (s=id->id.slots; s!=NIL; s=s->next) {
#ifdef NNPSCM
    if ((!isa_state) || (s->attr != current_agent(operator_symbol))) {
#endif
      for (pref=s->all_preferences; pref!=NIL; pref=pref->all_of_slot_next) {
	add_to_os_tc_if_needed (pref->value);
	if (preference_is_binary(pref->type))
	  add_to_os_tc_if_needed (pref->referent);
      }
      for (w=s->wmes; w!=NIL; w=w->next)
	add_to_os_tc_if_needed (w->value);
#ifdef NNPSCM
    }
#endif
  } /* end of for slots loop */
  /* --- now scan through RHS prefs and look for any with this id --- */
  for (pref=current_agent(rhs_prefs_from_instantiation); pref!=NIL; pref=pref->inst_next) {
    if (pref->id==id) {
#ifdef NNPSCM
    if ((!isa_state) || (pref->attr != current_agent(operator_symbol))) {
#endif
      add_to_os_tc_if_needed (pref->value);
      if (preference_is_binary(pref->type))
        add_to_os_tc_if_needed (pref->referent);
#ifdef NNPSCM
    }
#endif
  }
  }
  /* We don't need to worry about goal/impasse wmes here, since o-support tc's
     never start there and there's never a pointer to a goal or impasse from
     something else. */
}

void begin_os_tc (preference *rhs_prefs_or_nil) {
  current_agent(o_support_tc) = get_new_tc_number();
  current_agent(rhs_prefs_from_instantiation) = rhs_prefs_or_nil;
}

/* -----------------------------------------------------------------------
           Utilities for Testing Inclusion in the O-Support TC

   After a TC has been marked with the above routine, these utility
   routines are used for checking whether certain things are in the TC.
   Test_has_id_in_os_tc() checks whether a given test contains an equality
   test for any identifier in the TC, other than the identifier
   "excluded_sym".  Id_or_value_of_condition_list_is_in_os_tc() checks whether
   any id or value test in the given condition list (including id/value tests
   inside NCC's) has a test for an id in the TC.  In the case of value tests,
   the id is not allowed to be "sym_excluded_from_value".
----------------------------------------------------------------------- */

bool test_has_id_in_os_tc (test t, Symbol *excluded_sym) {
  cons *c;
  Symbol *referent;
  complex_test *ct;

  if (test_is_blank_test(t)) return FALSE;
  if (test_is_blank_or_equality_test(t)) {
    referent = referent_of_equality_test(t);
    if (referent->common.symbol_type==IDENTIFIER_SYMBOL_TYPE)
      if (referent->id.tc_num==current_agent(o_support_tc))
        if (referent!=excluded_sym)
          return TRUE;
    return FALSE;
  }
  ct = complex_test_from_test(t);
  if (ct->type==CONJUNCTIVE_TEST) {
    for (c=ct->data.conjunct_list; c!=NIL; c=c->rest)
      if (test_has_id_in_os_tc (c->first, excluded_sym)) return TRUE;
    return FALSE;
  }
  return FALSE;
}

#ifdef NNPSCM
bool id_or_value_of_condition_list_is_in_os_tc (condition *conds,
                 Symbol *sym_excluded_from_value,
                 Symbol *match_state_to_exclude_test_of_the_operator_off_of) {
#else
bool id_or_value_of_condition_list_is_in_os_tc (condition *conds,
                                        Symbol *sym_excluded_from_value) {
#endif
  /* RBD 8/19/94 Under NNPSCM, when we use this routine to look for "something
     off the state", we want to exclude tests of (match_state ^operator _). */
  for ( ; conds!=NIL; conds=conds->next) {
    switch (conds->type) {
    case POSITIVE_CONDITION:
    case NEGATIVE_CONDITION:
#ifdef NNPSCM
      if (test_includes_equality_test_for_symbol (conds->data.tests.id_test,
                       match_state_to_exclude_test_of_the_operator_off_of) &&
          test_includes_equality_test_for_symbol (conds->data.tests.attr_test,
                                           current_agent(operator_symbol)))
        break;
#endif
      if (test_has_id_in_os_tc (conds->data.tests.id_test, NIL))
        return TRUE;
      if (test_has_id_in_os_tc (conds->data.tests.value_test,
                                sym_excluded_from_value))
        return TRUE;
      break;
    case CONJUNCTIVE_NEGATION_CONDITION:
      if (id_or_value_of_condition_list_is_in_os_tc (conds->data.ncc.top,
                                              sym_excluded_from_value
#ifdef NNPSCM
                     , match_state_to_exclude_test_of_the_operator_off_of
#endif

                                              ))
        return TRUE;
      break;
    }
  }
  return FALSE;
}

#ifdef NNPSCM
/* -----------------------------------------------------------------------

   is_state_id

   GAP 10-6-94

   This routine checks to see if the identifier is one of the context
   objects i.e. it is the state somewhere in the context stack.
   This is used to ensure that O-support is not given to context objects 
   in super-states.

----------------------------------------------------------------------- */
bool is_state_id(Symbol *sym,Symbol *match_state)
{
  Symbol *c;
  
  for(c = current_agent(top_goal); c != match_state; c = c->id.lower_goal) {
    if (sym == c)
      return TRUE;
  }

  if (sym == match_state)
    return TRUE;
  else
    return FALSE;
}
#endif

/* -----------------------------------------------------------------------
                    Run-Time O-Support Calculation

   This routine calculates o-support for each preference for the given
   instantiation, filling in pref->o_supported (TRUE or FALSE) on each one.

   The following predicates are used for support calculations.  In the
   following, "lhs has some elt. ..." means the lhs has some id or value
   at any nesting level.

     lhs_oa_support:
       (1) does lhs test (match_goal ^operator match_operator NO) ?
       (2) mark TC (match_operator) using TM;
           does lhs has some elt. in TC but != match_operator ?
       (3) mark TC (match_state) using TM;
           does lhs has some elt. in TC ?
     lhs_oc_support:
       (1) mark TC (match_state) using TM;
           does lhs has some elt. in TC but != match_state ?
     lhs_om_support:
       (1) does lhs tests (match_goal ^operator) ?
       (2) mark TC (match_state) using TM;
           does lhs has some elt. in TC but != match_state ?

     rhs_oa_support:
       mark TC (match_state) using TM+RHS;
       if pref.id is in TC, give support
     rhs_oc_support:
       mark TC (inst.rhsoperators) using TM+RHS;
       if pref.id is in TC, give support
     rhs_om_support:
       mark TC (inst.lhsoperators) using TM+RHS;
       if pref.id is in TC, give support

   BUGBUG the code does a check of whether the lhs tests the match state via
          looking just at id and value fields of top-level positive cond's.
          It doesn't look at the attr field, or at any negative or NCC's.
          I'm not sure whether this is right or not.  (It's a pretty
          obscure case, though.)
----------------------------------------------------------------------- */

/* RBD 8/91/94 changed calls to add_to_os_tc() in this routine to use
   add_to_os_tc_if_id() instead -- in case people use constant-symbols 
   (instead of objects) for states or operators */

void calculate_support_for_instantiation_preferences (instantiation *inst) {
  Symbol *match_goal, *match_state, *match_operator;
  wme *match_state_wme, *match_operator_wme;
  bool lhs_tests_operator_installed;
  bool lhs_tests_operator_acceptable_or_installed;
  bool lhs_tests_match_state;
  bool lhs_is_known_to_test_something_off_match_state;
  bool lhs_is_known_to_test_something_off_match_operator;
#ifndef NNPSCM
  bool rhs_has_some_non_goal_preference;
#endif
  bool rhs_does_an_operator_creation;
  bool oc_support_possible;
  bool om_support_possible;
  bool oa_support_possible;
  preference *rhs, *pref;
  wme *w;
  condition *lhs, *c;
#ifdef DETAILED_TIMING_STATS
  struct timeval saved_start_tv;
#endif

#ifdef DETAILED_TIMING_STATS
  start_timer (&saved_start_tv);
#endif

  /* --- initialize by giving everything NO o_support --- */  
  for (pref=inst->preferences_generated; pref!=NIL; pref=pref->inst_next)
    pref->o_supported = FALSE;

  /* --- find the match goal, match state, and match operator --- */
  match_goal = inst->match_goal;
  if (!match_goal) goto o_support_done;  /* nothing gets o-support */

#ifndef NNPSCM
  match_state_wme = match_goal->id.state_slot->wmes;
  if (! match_state_wme) goto o_support_done; /* no state --> no o-support */
  match_state = match_state_wme->value;
#else
  match_state = match_goal;
#endif

  match_operator_wme = match_goal->id.operator_slot->wmes;
  if (match_operator_wme)
    match_operator = match_operator_wme->value;
  else
    match_operator = NIL;

  lhs = inst->top_of_instantiated_conditions;
  rhs = inst->preferences_generated;
  
  /* --- scan through rhs to look for various things --- */
#ifndef NNPSCM
  rhs_has_some_non_goal_preference = FALSE;
#endif
  rhs_does_an_operator_creation = FALSE;  

  for (pref=rhs; pref!=NIL; pref=pref->inst_next) {
#ifndef NNPSCM
    if (! pref->id->id.isa_goal) rhs_has_some_non_goal_preference = TRUE;
#endif
    if ((pref->id==match_goal) &&
        (pref->attr==current_agent(operator_symbol)) &&
        ((pref->type==ACCEPTABLE_PREFERENCE_TYPE) ||
         (pref->type==REQUIRE_PREFERENCE_TYPE)) )
      rhs_does_an_operator_creation = TRUE;
  }

#ifndef NNPSCM
/* Removed for NNPSCM because all goal aug's are also state aug's. */

  /* --- if all rhs preferences are goal aug's, there's no o-support --- */
  if (! rhs_has_some_non_goal_preference) goto o_support_done;
#endif
  
  /* --- scan through lhs to look for various tests --- */
  lhs_tests_operator_acceptable_or_installed = FALSE;
  lhs_tests_operator_installed = FALSE;
  lhs_tests_match_state = FALSE;
  lhs_is_known_to_test_something_off_match_state = FALSE;
  lhs_is_known_to_test_something_off_match_operator = FALSE;

#ifdef NNPSCM
  /* In NNPSCM we ALWAYS test the match state. */
  lhs_tests_match_state = TRUE;
#endif

  for (c=lhs; c!=NIL; c=c->next) {
    if (c->type!=POSITIVE_CONDITION) continue;
    w = c->bt.wme;
#ifndef NNPSCM
    if (w->value==match_state) lhs_tests_match_state = TRUE;
    if (w->id==match_state)
      lhs_is_known_to_test_something_off_match_state = TRUE;
#else
    /* For NNPSCM, count something as "off the match state" only
       if it's not the OPERATOR. */
    if ((w->id==match_state) && (w->attr != current_agent(operator_symbol)))
      lhs_is_known_to_test_something_off_match_state = TRUE;
#endif
    if (w->id==match_operator)
      lhs_is_known_to_test_something_off_match_operator = TRUE;
    if (w==match_operator_wme) lhs_tests_operator_installed = TRUE;
    if ((w->id==match_goal)&&(w->attr==current_agent(operator_symbol)))
      lhs_tests_operator_acceptable_or_installed = TRUE;
  }

  /* --- calcluate lhs support flags --- */
  oa_support_possible = lhs_tests_operator_installed;
  oc_support_possible = rhs_does_an_operator_creation; 
  om_support_possible = lhs_tests_operator_acceptable_or_installed;

  if ((!oa_support_possible)&&(!oc_support_possible)&&(!om_support_possible))
    goto o_support_done;

  if (! lhs_is_known_to_test_something_off_match_state) {
    begin_os_tc (NIL);
#ifndef NNPSCM
    add_to_os_tc_if_id (match_state);
#else
    add_to_os_tc_if_id (match_state, TRUE);
#endif
#ifdef NNPSCM
    if (! id_or_value_of_condition_list_is_in_os_tc (lhs, match_state,
                                                     match_state)) {
#else
    if (! id_or_value_of_condition_list_is_in_os_tc (lhs, match_state)) {
#endif
      oc_support_possible = FALSE;
      om_support_possible = FALSE;
#ifndef NNPSCM
      if (! lhs_tests_match_state) oa_support_possible = FALSE;
#endif
    }
  }

  if (oa_support_possible) {
    if (! lhs_is_known_to_test_something_off_match_operator) {
      begin_os_tc (NIL);
#ifdef NNPSCM
      add_to_os_tc_if_id (match_operator,FALSE);
#else
      add_to_os_tc_if_id (match_operator);
#endif
#ifdef NNPSCM
      if (! id_or_value_of_condition_list_is_in_os_tc (lhs, match_operator,
                                                       NIL))
#else
      if (! id_or_value_of_condition_list_is_in_os_tc (lhs, match_operator))
#endif
        oa_support_possible = FALSE;
    }
  }

  /* --- look for rhs oa support --- */
  if (oa_support_possible) {
    begin_os_tc (rhs);
#ifdef NNPSCM
    add_to_os_tc_if_id (match_state,TRUE);
#else
    add_to_os_tc_if_id (match_state);
#endif
    for (pref=rhs; pref!=NIL; pref=pref->inst_next) {
      if (pref->id->id.tc_num==current_agent(o_support_tc))
#ifdef NNPSCM
        /* RBD 8/19/94 added extra NNPSCM test -- ^operator augs on the state
                                                  don't get o-support */
/* AGR 639 begin 94.11.01 */
	/* gap 10/6/94 You need to check the id on all preferences that have
	   an attribute of operator to see if this is an operator slot of a
	   context being modified. */
	if (!((pref->attr == current_agent(operator_symbol)) && 
	    (is_state_id(pref->id,match_state))))
/* AGR 639 end */
#endif
        pref->o_supported = TRUE;
    }
  }

  /* --- look for rhs oc support --- */
  if (oc_support_possible) {
    begin_os_tc (rhs);
    for (pref=rhs; pref!=NIL; pref=pref->inst_next) {
      if ((pref->id==match_goal) &&
          (pref->attr==current_agent(operator_symbol)) &&
          ((pref->type==ACCEPTABLE_PREFERENCE_TYPE) ||
           (pref->type==REQUIRE_PREFERENCE_TYPE)) ) {
#ifdef NNPSCM
          add_to_os_tc_if_id (pref->value,FALSE);
#else
          add_to_os_tc_if_id (pref->value);
#endif
      }
    }
    for (pref=rhs; pref!=NIL; pref=pref->inst_next) {
      /* SBH 6/23/94 */
#ifndef NNPSCM
      if (pref->id->id.tc_num==current_agent(o_support_tc))
#else
      if ((pref->id->id.tc_num==current_agent(o_support_tc)) &&
	  (pref->id != match_state))
	/* SBH: Added 2nd test to avoid circular assignment of o-support
	   to augmentations of the state: in, e.g.
	   (sp p2
	      (state <g> ^problem-space)(state <ig> ^problem-space.name top-ps )
	      -->
	      (<g> ^operator <o>)(<o> ^name opx ^circular-goal-test <ig>))
	   Here, the op acc. pref would get o-support (it's in the transitive
	   closure); this test rules it out.
	   
	   BUGBUG: this is not fully general; it does not rule out assiging
	   o-support to substructures of the state that are in the TC of an
	   operator creation; e.g.
	   (sp p2
	      (state <g> ^problem-space)(state <ig> ^problem-space.name top-ps )
	      -->
	      (<g> ^operator <o> ^other <x>)
	      (<o> ^name opx ^circular-goal-test <ig>)
	      (<x> ^THIS-GETS-O-SUPPORT T))
	 */
#endif
      /* end SBH 6/23/94 */
        pref->o_supported = TRUE;
    }
  }
  
  /* --- look for rhs om support --- */
  if (om_support_possible) {
    begin_os_tc (rhs);
    for (c=inst->top_of_instantiated_conditions; c!=NIL; c=c->next)
      if (c->type==POSITIVE_CONDITION) {
        w = c->bt.wme;
        if ((w->id==match_goal) && (w->attr==current_agent(operator_symbol)))
#ifdef NNPSCM
          add_to_os_tc_if_id (w->value,FALSE);
#else
          add_to_os_tc_if_id (w->value);
#endif
      }
    for (pref=rhs; pref!=NIL; pref=pref->inst_next)
      if (pref->id->id.tc_num==current_agent(o_support_tc))
        pref->o_supported = TRUE;
  }

  o_support_done:  {}
#ifdef DETAILED_TIMING_STATS
  stop_timer (&saved_start_tv, &current_agent(o_support_cpu_time));
#endif
}

/* *********************************************************************

                   Compile-Time O-Support Calculations

********************************************************************* */

/* ------------------------------------------------------------------
                         Test Is For Symbol

   This function determines whether a given symbol could be the match
   for a given test.  It returns YES if the symbol is the only symbol
   that could pass the test (i.e., the test *forces* that symbol to be
   present in WM), NO if the symbol couldn't possibly pass the test,
   and MAYBE if it can't tell for sure.  The symbol may be a variable;
   the test may contain variables.
------------------------------------------------------------------ */

typedef enum yes_no_maybe_enum { YES, NO, MAYBE } yes_no_maybe;

yes_no_maybe test_is_for_symbol (test t, Symbol *sym) {
  cons *c;
  yes_no_maybe temp;
  bool maybe_found;
  complex_test *ct;
  Symbol *referent;

  if (test_is_blank_test(t)) return MAYBE;

  if (test_is_blank_or_equality_test(t)) {
    referent = referent_of_equality_test(t);
    if (referent==sym) return YES;
    if (referent->common.symbol_type==VARIABLE_SYMBOL_TYPE) return MAYBE;
    if (sym->common.symbol_type==VARIABLE_SYMBOL_TYPE) return MAYBE;
    return NO;
  }

  ct = complex_test_from_test(t);
  
  switch (ct->type) {
  case DISJUNCTION_TEST:
    if (sym->common.symbol_type==VARIABLE_SYMBOL_TYPE) return MAYBE;
    if (member_of_list (sym, ct->data.disjunction_list)) return MAYBE;
    return NO;
  case CONJUNCTIVE_TEST:
    maybe_found = FALSE;
    for (c=ct->data.conjunct_list; c!=NIL; c=c->rest) {
      temp = test_is_for_symbol (c->first, sym);
      if (temp==YES) return YES;
      if (temp==MAYBE) maybe_found = TRUE;
    }
    if (maybe_found) return MAYBE;
    return NO;
  default:  /* goal/impasse tests, relational tests other than equality */
    return MAYBE;
  }
}

/* ------------------------------------------------------------------
                         Find Known Goals

   This routine looks at the LHS and returns a list of variables that
   are certain to be bound to goals.

   Note:  this uses the TC routines and clobbers any existing TC.
                         
   BUGBUG should follow ^object links up the goal stack if possible
------------------------------------------------------------------ */

list *find_known_goals (condition *lhs) {
  tc_number tc;
  list *vars;
  condition *c;

  tc = get_new_tc_number();
  vars = NIL;
  for (c=lhs; c!=NIL; c=c->next) {
    if (c->type != POSITIVE_CONDITION) continue;
    if (test_includes_goal_or_impasse_id_test (c->data.tests.id_test,
                                               TRUE,
                                               FALSE))
      add_bound_variables_in_test (c->data.tests.id_test, tc, &vars);
  }
  return vars;
}

/* ------------------------------------------------------------------
                  Find Compile Time Match Goal

   Given the LHS and a list of known goals (i.e., variables that must
   be bound to goals at run-time), this routine tries to determine
   which variable will be the match goal.  If successful, it returns
   that variable; if it can't tell which variable will be the match
   goal, it returns NIL.

   Note:  this uses the TC routines and clobbers any existing TC.
------------------------------------------------------------------ */

Symbol *find_compile_time_match_goal (condition *lhs, list *known_goals) {
  tc_number tc;
  list *roots;
  list *root_goals;
  int num_root_goals;
  cons *c, *prev_c, *next_c;
  Symbol *result;
  condition *cond;
  
  /* --- find root variables --- */
  tc = get_new_tc_number();
  roots = collect_root_variables (lhs, tc, FALSE);
  
  /* --- intersect roots with known_goals, producing root_goals --- */
  root_goals = NIL;
  num_root_goals = 0;
  for (c=roots; c!=NIL; c=c->rest)
    if (member_of_list (c->first, known_goals)) {
      push (c->first, root_goals);
      num_root_goals++;
    }
  free_list (roots);

  /* --- if more than one goal, remove any with "^object nil" --- */
  if (num_root_goals > 1) {
    for (cond=lhs; cond!=NIL; cond=cond->next) {
      if ((cond->type==POSITIVE_CONDITION) &&
#ifndef NNPSCM
          (test_is_for_symbol(cond->data.tests.attr_test,current_agent(object_symbol))==YES)&&
#else
          (test_is_for_symbol(cond->data.tests.attr_test,current_agent(superstate_symbol))==YES)&&
#endif
          (test_is_for_symbol(cond->data.tests.value_test,current_agent(nil_symbol))==YES)) {
        prev_c = NIL;
        for (c=root_goals; c!=NIL; c=next_c) {
          next_c = c->rest;
          if (test_is_for_symbol (cond->data.tests.id_test, c->first)==YES) {
            /* --- remove c from the root_goals list --- */
            if (prev_c) prev_c->rest = next_c; else root_goals = next_c;
            free_cons (c);
            num_root_goals--;
            if (num_root_goals==1) break; /* be sure not to remove them all */
          } else {
            prev_c = c;
          }
        } /* end of for (c) loop */
        if (num_root_goals==1) break; /* be sure not to remove them all */
      }
    } /* end of for (cond) loop */
  }
  
  /* --- if there's only one root goal, that's it! --- */
  if (num_root_goals==1)
    result = root_goals->first;
  else
    result = NIL;

  /* --- clean up and return result --- */
  free_list (root_goals);
  return result;      
}

/* ------------------------------------------------------------------
                       Find Thing Off Goal

   Given the LHS and a the match goal variable, this routine looks
   for a positive condition testing (goal ^attr) for the given attribute
   "attr".  If such a condition exists, and the value field contains
   an equality test for a variable, then that variable is returned.
   (If more than one such variable exists, one is chosen arbitrarily
   and returned.)  Otherwise the function returns NIL.

   Note:  this uses the TC routines and clobbers any existing TC.
------------------------------------------------------------------ */

Symbol *find_thing_off_goal (condition *lhs, Symbol *goal, Symbol *attr) {
  condition *c;
  list *vars;
  tc_number tc;
  Symbol *result;

  for (c=lhs; c!=NIL; c=c->next) {
    if (c->type != POSITIVE_CONDITION) continue;
    if (test_is_for_symbol (c->data.tests.id_test, goal) != YES) continue;
    if (test_is_for_symbol (c->data.tests.attr_test, attr) != YES) continue;
    if (c->test_for_acceptable_preference) continue;
    tc = get_new_tc_number();
    vars = NIL;
    add_bound_variables_in_test (c->data.tests.value_test, tc, &vars);
    if (vars) {
      result = vars->first;
      free_list (vars);
      return result;
    }
  }
  return NIL;
}

/* ------------------------------------------------------------------
                 Condition List Has Id Test For Sym

   This checks whether a given condition list has an equality test for
   a given symbol in the id field of any condition (at any nesting level
   within NCC's).
------------------------------------------------------------------ */

bool condition_list_has_id_test_for_sym (condition *conds, Symbol *sym) {
  for ( ; conds!=NIL; conds=conds->next) {
    switch (conds->type) {
    case POSITIVE_CONDITION:
    case NEGATIVE_CONDITION:
      if (test_includes_equality_test_for_symbol (conds->data.tests.id_test,
                                                  sym))
        return TRUE;
      break;
    case CONJUNCTIVE_NEGATION_CONDITION:
      if (condition_list_has_id_test_for_sym (conds->data.ncc.top, sym))
        return TRUE;
      break;
    }
  }
  return FALSE;
}


/* SBH 7/1/94 #2 */
#ifdef NNPSCM

/* ------------------------------------------------------------------

------------------------------------------------------------------ */

bool match_state_tests_non_operator_slot (condition *conds, Symbol *match_state) {
  yes_no_maybe ynm;

  for ( ; conds!=NIL; conds=conds->next) {
    switch (conds->type) {
    case POSITIVE_CONDITION:
    case NEGATIVE_CONDITION:
      if (test_includes_equality_test_for_symbol (conds->data.tests.id_test,
                                                  match_state)) {
	ynm = test_is_for_symbol (conds->data.tests.attr_test, current_agent(operator_symbol));
	if (ynm == NO) return TRUE;
      }
      break;
    case CONJUNCTIVE_NEGATION_CONDITION:
      if (match_state_tests_non_operator_slot (conds->data.ncc.top, match_state))
        return TRUE;
      break;
    }
  }
  return FALSE;
}

#endif 
/* end SBH 7/1/94 #2 */

/* ------------------------------------------------------------------
                      Add TC Through LHS and RHS

   This enlarges a given TC by adding to it any connected conditions
   in the LHS or actions in the RHS.
------------------------------------------------------------------ */

void add_tc_through_lhs_and_rhs (condition *lhs, action *rhs, tc_number tc,
                                 list **id_list, list **var_list) {
  condition *c;
  action *a;
  bool anything_changed;
  
  for (c=lhs; c!=NIL; c=c->next) c->already_in_tc = FALSE;
  for (a=rhs; a!=NIL; a=a->next) a->already_in_tc = FALSE;

  /* --- keep trying to add new stuff to the tc --- */  
  while (TRUE) {
    anything_changed = FALSE;
    for (c=lhs; c!=NIL; c=c->next)
      if (! c->already_in_tc)
        if (cond_is_in_tc (c, tc)) {
          add_cond_to_tc (c, tc, id_list, var_list);
          c->already_in_tc = TRUE;
          anything_changed = TRUE;
        }
    for (a=rhs; a!=NIL; a=a->next)
      if (! a->already_in_tc)
        if (action_is_in_tc (a, tc)) {
          add_action_to_tc (a, tc, id_list, var_list);
          a->already_in_tc = TRUE;
          anything_changed = TRUE;
        }
    if (! anything_changed) break;
  }
}

/* -----------------------------------------------------------------------
                   Calculate Compile Time O-Support

   This takes the LHS and RHS, and fills in the a->support field in each
   RHS action with either UNKNOWN_SUPPORT, O_SUPPORT, or NO_O_SUPPORT.
   (Actually, it only does this for MAKE_ACTION's--for FUNCALL_ACTION's,
   the support doesn't matter.)
----------------------------------------------------------------------- */

void calculate_compile_time_o_support (condition *lhs, action *rhs) {
  list *known_goals;
  cons *c;
  Symbol  *match_state, *match_operator;
#ifndef NNPSCM
  Symbol  *match_goal;
#endif
  yes_no_maybe lhs_oa_support, lhs_oc_support, lhs_om_support;
  action *a;
  condition *cond;
  yes_no_maybe ynm;
  bool operator_found, possible_operator_found;
  tc_number tc;

  /* --- initialize:  mark all rhs actions as "unknown" --- */
  for (a=rhs; a!=NIL; a=a->next)
    if (a->type==MAKE_ACTION) a->support=UNKNOWN_SUPPORT;

  /* --- if "operator" doesn't appear in any LHS attribute slot, and there
         are no RHS +/! makes for "operator", then nothing gets support --- */
  operator_found = FALSE;
  possible_operator_found = FALSE;
  for (cond=lhs; cond!=NIL; cond=cond->next) {
    if (cond->type != POSITIVE_CONDITION) continue;
    ynm = test_is_for_symbol (cond->data.tests.attr_test, current_agent(operator_symbol));
    if (ynm==YES) { operator_found = possible_operator_found = TRUE; break; }
    if (ynm==MAYBE) possible_operator_found = TRUE;
  }
  if (! operator_found)
    for (a=rhs; a!=NIL; a=a->next) {
      if (a->type != MAKE_ACTION) continue;
      if (a->attr==current_agent(operator_symbol))
        { operator_found = possible_operator_found = TRUE; break; }
      if (a->attr->common.symbol_type==VARIABLE_SYMBOL_TYPE)
        possible_operator_found = TRUE;
    }
  if (! possible_operator_found) {
    for (a=rhs; a!=NIL; a=a->next) {
      if (a->type == MAKE_ACTION) a->support=NO_O_SUPPORT;
    }
    return;
  }


  /* --- find known goals; RHS augmentations of goals get no support --- */
  known_goals = find_known_goals (lhs);
#ifndef NNPSCM
  for (c=known_goals; c!=NIL; c=c->rest)
    for (a=rhs; a!=NIL; a=a->next)
      if (a->type == MAKE_ACTION)
        if (a->id == c->first) a->support = NO_O_SUPPORT;
#else
 /* SBH: In NNPSCM, the only RHS-goal augmentations that can't get support are
    preferences for the "operator" slot. */
  for (c=known_goals; c!=NIL; c=c->rest)
    for (a=rhs; a!=NIL; a=a->next) {
      if (a->type != MAKE_ACTION) continue;
      if (a->attr==current_agent(operator_symbol)) {
        if (a->id == c->first) a->support = NO_O_SUPPORT;
      }
    }
#endif
  

  /* --- find match goal, state, and operator --- */
#ifndef NNPSCM
  match_goal = find_compile_time_match_goal (lhs, known_goals);
#else
  match_state = find_compile_time_match_goal (lhs, known_goals);
#endif
  free_list (known_goals);
#ifndef NNPSCM
  if (!match_goal) return;
  match_state = find_thing_off_goal (lhs, match_goal, current_agent(state_symbol));
  if (!match_state) return;
  match_operator = find_thing_off_goal (lhs, match_goal, current_agent(operator_symbol));
#else
  if (!match_state) return;
  match_operator = find_thing_off_goal (lhs, match_state, current_agent(operator_symbol));
#endif
  /* --- If when checking (above) for "operator" appearing anywhere, we
     found a possible operator but not a definite operator, now go back and
     see if the possible operator was actually the match goal or match state;
     if so, it's not a possible operator.  (Note:  by "possible operator" I
     mean something appearing in the *attribute* field that might get bound
     to the symbol "operator".)  --- */
  if (possible_operator_found && !operator_found) {
    possible_operator_found = FALSE;
    for (cond=lhs; cond!=NIL; cond=cond->next) {
      if (cond->type != POSITIVE_CONDITION) continue;
      ynm = test_is_for_symbol (cond->data.tests.attr_test, current_agent(operator_symbol));
      if ((ynm!=NO) &&
#ifndef NNPSCM
          (test_is_for_symbol (cond->data.tests.attr_test, match_goal)!=YES) &&
#endif
          (test_is_for_symbol (cond->data.tests.attr_test, match_state)!=YES))
        { possible_operator_found = TRUE; break; }
    }
    if (! possible_operator_found) {
      for (a=rhs; a!=NIL; a=a->next) {
        if (a->type != MAKE_ACTION) continue;
        /* we're looking for "operator" augs of goals only, and match_state
           couldn't get bound to a goal */
        if (a->id == match_state) continue;
        if ((a->attr->common.symbol_type==VARIABLE_SYMBOL_TYPE) &&
#ifndef NNPSCM
            (a->attr != match_goal) &&
#endif
            (a->attr != match_state))
          { possible_operator_found = TRUE; break; }
      }
    }
    if (! possible_operator_found) {
      for (a=rhs; a!=NIL; a=a->next)
        if (a->type == MAKE_ACTION) a->support=NO_O_SUPPORT;
      return;
    }
  }
  
  /* --- calculate LHS support predicates --- */
  lhs_oa_support = MAYBE;
  if (match_operator)

/* SBH 7/1/94 #2 */
#ifndef NNPSCM
    if (condition_list_has_id_test_for_sym (lhs, match_operator))
#else
    if ((condition_list_has_id_test_for_sym (lhs, match_operator)) &&
	(match_state_tests_non_operator_slot(lhs,match_state)))
#endif
/* end SBH 7/1/94 #2 */

      lhs_oa_support = YES;

  lhs_oc_support = MAYBE;
  lhs_om_support = MAYBE;

/* SBH 7/1/94 #2 */
#ifndef NNPSCM
  if (condition_list_has_id_test_for_sym (lhs, match_state)) 
#else
  /* For NNPSCM, must test that there is a test of a non-operator slot off 
     of the match_state. */
  if (match_state_tests_non_operator_slot(lhs,match_state)) 
#endif
    {
/* end SBH 7/1/94 #2 */

    lhs_oc_support = YES; 
    for (cond=lhs; cond!=NIL; cond=cond->next) {
      if (cond->type != POSITIVE_CONDITION) continue;
#ifndef NNPSCM
      if (test_is_for_symbol (cond->data.tests.id_test, match_goal) != YES) continue;
#else
      if (test_is_for_symbol (cond->data.tests.id_test, match_state) != YES) continue;
#endif
      if (test_is_for_symbol (cond->data.tests.attr_test, current_agent(operator_symbol))
          != YES)
        continue;
      lhs_om_support = YES;
      break;
    }
/* SBH 7/1/94 #2: removed #ifndef NNPSCM around the following bracket */
/* #ifndef NNPSCM */
  }     
/* #endif */
/* end SBH 7/1/94 #2 */

  if (lhs_oa_support == YES) {    /* --- look for RHS o-a support --- */
    /* --- do TC(match_state) --- */
    tc = get_new_tc_number();
    add_symbol_to_tc (match_state, tc, NIL, NIL);
    add_tc_through_lhs_and_rhs (lhs, rhs, tc, NIL, NIL);

    /* --- any action with id in the TC gets support --- */
    for (a=rhs; a!=NIL; a=a->next)  {

      if (action_is_in_tc (a, tc)) 
	/* SBH 7/1/94 Avoid resetting of support that was previously set to NO_O_SUPPORT. */
#ifdef NNPSCM
	/* gap 10/6/94 If the action has an attribue of operator, then you
	   don't know if it should get o-support until run time because of
	   the vagaries of knowing when this is matching a context object
	   or not. */
	if (a->attr==current_agent(operator_symbol)) {
	  if (a->support != NO_O_SUPPORT) a->support = UNKNOWN_SUPPORT;
	} else {
	  if (a->support != NO_O_SUPPORT) a->support = O_SUPPORT;
	}
#else 
        a->support = O_SUPPORT;
#endif
        /* end SBH 7/1/94 */
    }
  }
  
  if (lhs_oc_support == YES) {    /* --- look for RHS o-c support --- */
    /* --- do TC(rhs operators) --- */
    tc = get_new_tc_number();
    for (a=rhs; a!=NIL; a=a->next) {
      if (a->type != MAKE_ACTION) continue;
      if (
#ifndef NNPSCM
	  (a->id==match_goal) &&
#else 
	  (a->id==match_state) &&
#endif
          (a->attr==current_agent(operator_symbol)) &&
          ((a->preference_type==ACCEPTABLE_PREFERENCE_TYPE) ||
           (a->preference_type==REQUIRE_PREFERENCE_TYPE)) ) {
        if (rhs_value_is_symbol(a->value)) {
          add_symbol_to_tc (rhs_value_to_symbol(a->value), tc, NIL,NIL);
	}
      }
    }
    add_tc_through_lhs_and_rhs (lhs, rhs, tc, NIL, NIL);

    /* --- any action with id in the TC gets support --- */
    for (a=rhs; a!=NIL; a=a->next) 


      if (action_is_in_tc (a, tc)) {

	/* SBH 6/7/94:
	   Make sure the action is not already marked as "NO_O_SUPPORT".  This
	   avoids giving o-support in the case where the operator
	   points back to the goal, thus adding the goal to the TC,
	   thus adding the operator proposal itself to the TC; thus
	   giving o-support to an operator proposal.
	*/
	if (a->support != NO_O_SUPPORT) a->support = O_SUPPORT;
	/* End SBH 6/7/94 */
      }
  }

  if (lhs_om_support == YES) {    /* --- look for RHS o-m support --- */
    /* --- do TC(lhs operators) --- */
    tc = get_new_tc_number();
    for (cond=lhs; cond!=NIL; cond=cond->next) {
      if (cond->type != POSITIVE_CONDITION) continue;
#ifndef NNPSCM
      if (test_is_for_symbol (cond->data.tests.id_test, match_goal) == YES)
#else
      if (test_is_for_symbol (cond->data.tests.id_test, match_state) == YES)
#endif
        if (test_is_for_symbol (cond->data.tests.attr_test, current_agent(operator_symbol))
            == YES)
          add_bound_variables_in_test (cond->data.tests.value_test, tc, NIL);
    }
    add_tc_through_lhs_and_rhs (lhs, rhs, tc, NIL, NIL);

    /* --- any action with id in the TC gets support --- */
    for (a=rhs; a!=NIL; a=a->next) 

      if (action_is_in_tc (a, tc)) {
	/* SBH 7/1/94 Avoid resetting of support that was previously set to NO_O_SUPPORT. */
#ifdef NNPSCM	
	if (a->support != NO_O_SUPPORT) a->support = O_SUPPORT;
#else 
	a->support = O_SUPPORT;
#endif
	/* end SBH 7/1/94 */
      }
  }
}
