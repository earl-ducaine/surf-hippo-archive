/*
 * $Id: chunk.c,v 1.10 1994/12/06 22:02:45 rempel Exp $
 * $Log: chunk.c,v $
 * Revision 1.10  1994/12/06  22:02:45  rempel
 * For 6.2.4b
 *
 * Revision 1.9  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.8  1994/08/23  10:32:59  portelli
 * For 6.2.4
 *
 * Revision 1.6  1994/06/10  19:07:18  portelli
 * For 6.2.1
 *
 * Revision 1.5  94/06/09  21:20:11  portelli
 * For 6.2.1
 * 
 * Revision 1.4  94/06/08  22:16:58  portelli
 * For 6.2.1
 * 
 * Revision 1.3  94/05/18  13:31:09  portelli
 * Soar 6.2.0 b
 * 
 * Revision 1.2  93/11/21  16:21:25  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:48:14  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:19:39  jtraub
 * 6.1_checkin
 *
 * Revision 9.1  1993/05/10  20:30:20  jtraub
 * initial checkin
 *
 */

/* ====================================================================

                          Chunking Routines

==================================================================== */

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

#include "explain.h"

/* backtrace.c */
extern void backtrace_through_instantiation(instantiation *, goal_stack_level, condition *, int);
extern void trace_locals(goal_stack_level);
extern void trace_grounded_potentials(void);
extern bool trace_ungrounded_potentials(goal_stack_level);

/* osupport.c */
extern void calculate_support_for_instantiation_preferences(instantiation *);
extern void calculate_compile_time_o_support(condition *, action *);

/* recmem.c */
extern preference *find_clone_for_level(preference *, goal_stack_level);
extern void fill_in_new_instantiation_stuff(instantiation *, bool);
extern void deallocate_instantiation(instantiation *);
extern void do_preference_phase(void);

/* =====================================================================

                           Results Calculation

   Get_results_for_instantiation() finds and returns the result preferences
   for a given instantiation.  This is the main routine here.

   The results are accumulated in the list "results," linked via the
   "next_result" field of the preference structures.  (BUGBUG: to save
   space, just use conses for this.)

   Add_pref_to_results() adds a preference to the results.
   Add_results_for_id() adds any preferences for the given identifier.
   Identifiers are marked with results_tc_number as they are added.
===================================================================== */

void add_results_for_id (Symbol *id);

#define add_results_if_needed(sym) \
  { if ((sym)->common.symbol_type==IDENTIFIER_SYMBOL_TYPE) \
      if ( ((sym)->id.level >= current_agent(results_match_goal_level)) && \
           ((sym)->id.tc_num != current_agent(results_tc_number)) ) \
        add_results_for_id(sym); }
                                    
void add_pref_to_results (preference *pref) {
  preference *p;

  /* --- if an equivalent pref is already a result, don't add this one --- */
  for (p=current_agent(results); p!=NIL; p=p->next_result) {
    if (p->id!=pref->id) continue;
    if (p->attr!=pref->attr) continue;
    if (p->value!=pref->value) continue;
    if (p->type!=pref->type) continue;
    if (preference_is_unary(pref->type)) return;
    if (p->referent!=pref->referent) continue;
    return;
  }

  /* --- if pref isn't at the right level, find a clone that is --- */
  if (pref->inst->match_goal_level != current_agent(results_match_goal_level)) {
    for (p=pref->next_clone; p!=NIL; p=p->next_clone)
      if (p->inst->match_goal_level == current_agent(results_match_goal_level)) break;
    if (!p)
      for (p=pref->prev_clone; p!=NIL; p=p->prev_clone)
        if (p->inst->match_goal_level == current_agent(results_match_goal_level)) break;
    if (!p) return;  /* if can't find one, it isn't a result */
    pref = p;
  }

  /* --- add this preference to the result list --- */
  pref->next_result = current_agent(results); 
  current_agent(results) = pref;

  /* --- follow transitive closuse through value, referent links --- */
  add_results_if_needed (pref->value);
  if (preference_is_binary(pref->type))
    add_results_if_needed (pref->referent);
}

void add_results_for_id (Symbol *id) {
  slot *s;
  preference *pref;
  wme *w;

  id->id.tc_num = current_agent(results_tc_number);

  /* --- scan through all preferences and wmes for all slots for this id --- */
  for (w=id->id.input_wmes; w!=NIL; w=w->next)
    add_results_if_needed (w->value);
  for (s=id->id.slots; s!=NIL; s=s->next) {
    for (pref=s->all_preferences; pref!=NIL; pref=pref->all_of_slot_next)
      add_pref_to_results(pref);
    for (w=s->wmes; w!=NIL; w=w->next)
      add_results_if_needed (w->value);
  } /* end of for slots loop */
  /* --- now scan through extra prefs and look for any with this id --- */
  for (pref=current_agent(extra_result_prefs_from_instantiation); pref!=NIL;
       pref=pref->inst_next) {
    if (pref->id==id) add_pref_to_results(pref);
  }
}

preference *get_results_for_instantiation (instantiation *inst) {
  preference *pref;

  current_agent(results) = NIL;
  current_agent(results_match_goal_level) = inst->match_goal_level;
  current_agent(results_tc_number) = get_new_tc_number();
  current_agent(extra_result_prefs_from_instantiation) = inst->preferences_generated;
  for (pref=inst->preferences_generated; pref!=NIL; pref=pref->inst_next)
    if ( (pref->id->id.level < current_agent(results_match_goal_level)) &&
         (pref->id->id.tc_num != current_agent(results_tc_number)) ) {
      add_pref_to_results(pref);
    }
  return current_agent(results);
}

/* =====================================================================

                  Variablizing Conditions and Results

   Variablizing of conditions is done by walking over a condition list
   and destructively modifying it, replacing tests of identifiers with
   tests of tests of variables.  The identifier-to-variable mapping is
   built as we go along:  identifiers that have already been assigned
   a variablization are marked with id.tc_num==variablization_tc, and
   id.variablization points to the corresponding variable.

   Variablizing of results can't be done destructively because we need
   to convert the results--preferences--into actions.  This is done
   by copy_and_variablize_result_list(), which takes the result preferences
   and returns an action list.

   The global variable "variablize_this_chunk" indicates whether to
   variablize at all.  This flag is set to TRUE or FALSE before and during
   backtracing.  FALSE means the new production will become a justification;
   TRUE means it will be a chunk.
===================================================================== */

void variablize_symbol (Symbol **sym) {
  char prefix[2];
  Symbol *var;
  
  if ((*sym)->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) return;
  if (! current_agent(variablize_this_chunk)) return;
  
  if ((*sym)->id.tc_num == current_agent(variablization_tc)) {
    /* --- it's already been variablized, so use the existing variable --- */
    var = (*sym)->id.variablization;
    symbol_remove_ref (*sym);
    *sym = var;
    symbol_add_ref (var);
    return;
  }

  /* --- need to create a new variable --- */
  (*sym)->id.tc_num = current_agent(variablization_tc);
  prefix[0] = tolower((*sym)->id.name_letter);
  prefix[1] = 0;
  var = generate_new_variable (prefix);
  (*sym)->id.variablization = var;
  symbol_remove_ref (*sym);
  *sym = var;
}

void variablize_test (test *t) {
  cons *c;
  complex_test *ct;

  if (test_is_blank_test(*t)) return;
  if (test_is_blank_or_equality_test(*t)) {
    variablize_symbol ((Symbol **) t);
    /* Warning: this relies on the representation of tests */
    return;
  }

  ct = complex_test_from_test(*t);
  
  switch (ct->type) {
  case GOAL_ID_TEST:
  case IMPASSE_ID_TEST:
  case DISJUNCTION_TEST:
    return;
  case CONJUNCTIVE_TEST:
    for (c=ct->data.conjunct_list; c!=NIL; c=c->rest)
      variablize_test ((test *)(&(c->first)));
    return;
  default:  /* relational tests other than equality */
    variablize_symbol (&(ct->data.referent));
    return;
  }
}

void variablize_condition_list (condition *cond) {
  for (; cond!=NIL; cond=cond->next) {
    switch (cond->type) {
    case POSITIVE_CONDITION:
    case NEGATIVE_CONDITION:
      variablize_test (&(cond->data.tests.id_test));
      variablize_test (&(cond->data.tests.attr_test));
      variablize_test (&(cond->data.tests.value_test));
      break;
    case CONJUNCTIVE_NEGATION_CONDITION:
      variablize_condition_list (cond->data.ncc.top);
      break;
    }
  }
}

action *copy_and_variablize_result_list (preference *pref) {
  action *a;
  Symbol *temp;
  
  if (!pref) return NIL;
  allocate_with_pool (&current_agent(action_pool), &a);
  a->type = MAKE_ACTION;

  a->id = pref->id;
  symbol_add_ref (pref->id);
  variablize_symbol (&(a->id));

  a->attr = pref->attr;
  symbol_add_ref (pref->attr);
  variablize_symbol (&(a->attr));

  temp = pref->value;
  symbol_add_ref (temp);
  variablize_symbol (&temp);
  a->value = symbol_to_rhs_value (temp);

  a->preference_type = pref->type;

  if (preference_is_binary(pref->type)) {
    temp = pref->referent;
    symbol_add_ref (temp);
    variablize_symbol (&temp);
    a->referent = symbol_to_rhs_value (temp);
  }
  
  a->next = copy_and_variablize_result_list (pref->next_result);
  return a;  
}

/* ====================================================================

     Chunk Conditions, and Chunk Conditions Set Manipulation Routines

   These structures have two uses.  First, for every ground condition,
   one of these structures maintains certain information about it--
   pointers to the original (instantiation's) condition, the chunks's
   instantiation's condition, and the variablized condition, etc.

   Second, for negated conditions, these structures are entered into
   a hash table with keys hash_condition(this_cond).  This hash table
   is used so we can add a new negated condition to the set of negated
   potentials quickly--we don't want to add a duplicate of a negated
   condition that's already there, and the hash table lets us quickly
   determine whether a duplicate is already there.

   I used one type of structure for both of these uses, (1) for simplicity
   and (2) to avoid having to do a second allocation when we move
   negated conditions over to the ground set.
==================================================================== */

/* --------------------------------------------------------------------
                      Chunk Cond Set Routines

   Init_chunk_cond_set() initializes a given chunk_cond_set to be empty.
   
   Make_chunk_cond_for_condition() takes a condition and returns a
   chunk_cond for it, for use in a chunk_cond_set.  This is used only
   for the negated conditions, not grounds.

   Add_to_chunk_cond_set() adds a given chunk_cond to a given chunk_cond_set
   and returns TRUE if the condition isn't already in the set.  If the 
   condition is already in the set, the routine deallocates the given
   chunk_cond and returns FALSE.

   Remove_from_chunk_cond_set() removes a given chunk_cond from a given
   chunk_cond_set, but doesn't deallocate it.
-------------------------------------------------------------------- */

                             /* set of all negated conditions we encounter
                                during backtracing--these are all potentials
                                and (some of them) are added to the grounds
                                in one pass at the end of the backtracing */

void init_chunk_cond_set (chunk_cond_set *set) {
  int i;
  
  set->all = NIL;
  for (i=0; i<CHUNK_COND_HASH_TABLE_SIZE; i++) set->table[i] = NIL;
}

chunk_cond *make_chunk_cond_for_condition (condition *cond) {
  chunk_cond *cc;
  unsigned long remainder, hv;
  
  allocate_with_pool (&current_agent(chunk_cond_pool), &cc);
  cc->cond = cond;
  cc->hash_value = hash_condition (cond);
  remainder = cc->hash_value;
  hv = 0;
  while (remainder) {
    hv ^= (remainder &
           masks_for_n_low_order_bits[LOG_2_CHUNK_COND_HASH_TABLE_SIZE]);
    remainder = remainder >> LOG_2_CHUNK_COND_HASH_TABLE_SIZE;
  }
  cc->compressed_hash_value = hv;
  return cc;
}

bool add_to_chunk_cond_set (chunk_cond_set *set, chunk_cond *new_cc) {
  chunk_cond *old;
  
  for (old=set->table[new_cc->compressed_hash_value]; old!=NIL;
       old=old->next_in_bucket)
    if (old->hash_value==new_cc->hash_value)
      if (conditions_are_equal (old->cond, new_cc->cond))
        break;
  if (old) {
    /* --- the new condition was already in the set; so don't add it --- */
    free_with_pool (&current_agent(chunk_cond_pool), new_cc);
    return FALSE;
  }
  /* --- add new_cc to the table --- */
  insert_at_head_of_dll (set->all, new_cc, next, prev);
  insert_at_head_of_dll (set->table[new_cc->compressed_hash_value], new_cc,
                         next_in_bucket, prev_in_bucket);
  return TRUE;
}

void remove_from_chunk_cond_set (chunk_cond_set *set, chunk_cond *cc) {
  remove_from_dll (set->all, cc, next, prev);
  remove_from_dll (set->table[cc->compressed_hash_value],
                   cc, next_in_bucket, prev_in_bucket);
}

/* ==================================================================== 

                 Other Miscellaneous Chunking Routines

==================================================================== */

/* --------------------------------------------------------------------
            Build Chunk Conds For Grounds And Add Negateds
       
   This routine is called once backtracing is finished.  It goes through
   the ground conditions and builds a chunk_cond (see above) for each
   one.  The chunk_cond includes two new copies of the condition:  one
   to be used for the initial instantiation of the chunk, and one to
   be (variablized and) used for the chunk itself.

   This routine also goes through the negated conditions and adds to
   the ground set (again building chunk_cond's) any negated conditions
   that are connected to the grounds.

   At exit, the "dest_top" and "dest_bottom" arguments are set to point
   to the first and last chunk_cond in the ground set.  The "tc_to_use"
   argument is the tc number that this routine will use to mark the
   TC of the ground set.  At exit, this TC indicates the set of identifiers
   in the grounds.  (This is used immediately afterwards to figure out
   which Nots must be added to the chunk.)
-------------------------------------------------------------------- */

void build_chunk_conds_for_grounds_and_add_negateds (chunk_cond **dest_top,
                                                     chunk_cond **dest_bottom,
                                                     tc_number tc_to_use) {
  cons *c;
  condition *ground;
  chunk_cond *cc, *first_cc, *prev_cc;

  /* --- build instantiated conds for grounds and setup their TC --- */
  prev_cc = NIL;
  while (current_agent(grounds)) {
    c = current_agent(grounds);
    current_agent(grounds) = current_agent(grounds)->rest;
    ground = c->first;
    free_cons (c);
    /* --- make the instantiated condition --- */
    allocate_with_pool (&current_agent(chunk_cond_pool), &cc);
    cc->cond = ground;
    cc->instantiated_cond = copy_condition (cc->cond);
    cc->variablized_cond = copy_condition (cc->cond);
    if (prev_cc) {
      prev_cc->next = cc;
      cc->prev = prev_cc;
      cc->variablized_cond->prev = prev_cc->variablized_cond;
      prev_cc->variablized_cond->next = cc->variablized_cond;
    } else {
      first_cc = cc;
      cc->prev = NIL;
      cc->variablized_cond->prev = NIL;
    }
    prev_cc = cc;
    /* --- add this in to the TC --- */
    add_cond_to_tc (ground, tc_to_use, NIL, NIL);
  }

  /* --- scan through negated conditions and check which ones are connected
     to the grounds --- */
  if (current_agent(sysparams)[TRACE_BACKTRACING_SYSPARAM])
    print_string ("\n\n*** Adding Grounded Negated Conditions ***\n");
  
  while (current_agent(negated_set).all) {
    cc = current_agent(negated_set).all;
    remove_from_chunk_cond_set (&current_agent(negated_set), cc);
    if (cond_is_in_tc (cc->cond, tc_to_use)) {
      /* --- negated cond is in the TC, so add it to the grounds --- */
      if (current_agent(sysparams)[TRACE_BACKTRACING_SYSPARAM]) {
        print_string ("\n-->Moving to grounds: ");
        print_condition (cc->cond);
      }
      cc->instantiated_cond = copy_condition (cc->cond);
      cc->variablized_cond = copy_condition (cc->cond);
      if (prev_cc) {
        prev_cc->next = cc;
        cc->prev = prev_cc;
        cc->variablized_cond->prev = prev_cc->variablized_cond;
        prev_cc->variablized_cond->next = cc->variablized_cond;
      } else {
        first_cc = cc;
        cc->prev = NIL;
        cc->variablized_cond->prev = NIL;
      }
      prev_cc = cc;
    } else {
      /* --- not in TC, so discard the condition --- */
      free_with_pool (&current_agent(chunk_cond_pool), cc);
    }
  }

  if (prev_cc) {
    prev_cc->next = NIL;
    prev_cc->variablized_cond->next = NIL;
  } else {
    first_cc = NIL;
  }
  
  *dest_top = first_cc;
  *dest_bottom = prev_cc;
}

/* --------------------------------------------------------------------
                  Get Nots For Instantiated Conditions

   This routine looks through all the Nots in the instantiations in
   instantiations_with_nots, and returns copies of the ones involving
   pairs of identifiers in the grounds.  Before this routine is called,
   the ids in the grounds must be marked with "tc_of_grounds."  
-------------------------------------------------------------------- */

not *get_nots_for_instantiated_conditions (list *instantiations_with_nots,
                                           tc_number tc_of_grounds) {
  cons *c;
  instantiation *inst;
  not *n1, *n2, *new_not, *collected_nots;

  /* --- collect nots for which both id's are marked --- */
  collected_nots = NIL;
  while (instantiations_with_nots) {
    c = instantiations_with_nots;
    instantiations_with_nots = c->rest;
    inst = c->first;
    free_cons (c);
    for (n1=inst->nots; n1 != NIL; n1=n1->next) {
      /* --- Are both id's marked? If no, goto next loop iteration --- */
      if (n1->s1->id.tc_num != tc_of_grounds) continue;
      if (n1->s2->id.tc_num != tc_of_grounds) continue;
      /* --- If the pair already in collected_nots, goto next iteration --- */
      for (n2=collected_nots; n2!=NIL; n2=n2->next) {
        if ((n2->s1 == n1->s1) && (n2->s2 == n1->s2)) break;
        if ((n2->s1 == n1->s2) && (n2->s2 == n1->s1)) break;
      }
      if (n2) continue;
      /* --- Add the pair to collected_nots --- */
      allocate_with_pool (&current_agent(not_pool), &new_not);
      new_not->next = collected_nots;
      collected_nots = new_not;
      new_not->s1 = n1->s1;
      symbol_add_ref (new_not->s1);
      new_not->s2 = n1->s2;
      symbol_add_ref (new_not->s2);
    } /* end of for n1 */
  } /* end of while instantiations_with_nots */

  return collected_nots;
}

/* --------------------------------------------------------------------
              Variablize Nots And Insert Into Conditions
             
   This routine goes through the given list of Nots and, for each one,
   inserts a variablized copy of it into the given condition list at
   the earliest possible location.  (The given condition list should
   be the previously-variablized condition list that will become the
   chunk's LHS.)  The given condition list is destructively modified;
   the given Not list is unchanged.
-------------------------------------------------------------------- */

void variablize_nots_and_insert_into_conditions (not *nots,
                                                 condition *conds) {
  not *n;
  Symbol *var1, *var2;
  test t;
  complex_test *ct;
  condition *c;
  bool added_it;

  /* --- don't bother Not-ifying justifications --- */
  if (! current_agent(variablize_this_chunk)) return;
  
  for (n=nots; n!=NIL; n=n->next) {
    var1 = n->s1->id.variablization;
    var2 = n->s2->id.variablization;
    /* --- find where var1 is bound, and add "<> var2" to that test --- */
    allocate_with_pool (&current_agent(complex_test_pool), &ct);
    t = make_test_from_complex_test (ct);
    ct->type = NOT_EQUAL_TEST;
    ct->data.referent = var2;
    symbol_add_ref (var2);
    added_it = FALSE;
    for (c=conds; c!=NIL; c=c->next) {
      if (c->type != POSITIVE_CONDITION) continue;
      if (test_includes_equality_test_for_symbol (c->data.tests.id_test,
                                                  var1)) {
        add_new_test_to_test (&(c->data.tests.id_test), t);
        added_it = TRUE;
        break;
      }
      if (test_includes_equality_test_for_symbol (c->data.tests.attr_test,
                                                  var1)) {
        add_new_test_to_test (&(c->data.tests.attr_test), t);
        added_it = TRUE;
        break;
      }
      if (test_includes_equality_test_for_symbol (c->data.tests.value_test,
                                                  var1)) {
        add_new_test_to_test (&(c->data.tests.value_test), t);
        added_it = TRUE;
        break;
      }
    }
    if (!added_it) {
      print ("Internal error: couldn't add Not test to chunk\n");
      abort_with_fatal_error();
    }
  } /* end of for n=nots */
}

/* --------------------------------------------------------------------
                     Add Goal or Impasse Tests

   This routine adds goal id or impasse id tests to the variablized
   conditions.  For each id in the grounds that happens to be the
   identifier of a goal or impasse, we add a goal/impasse id test
   to the variablized conditions, to make sure that in the resulting
   chunk, the variablization of that id is constrained to match against
   a goal/impasse.  (Note:  actually, in the current implementation of
   chunking, it's impossible for an impasse id to end up in the ground
   set.  So part of this code is unnecessary.)
-------------------------------------------------------------------- */

void add_goal_or_impasse_tests (chunk_cond *all_ccs) {
  chunk_cond *cc;
  tc_number tc;   /* mark each id as we add a test for it, so we don't add
                     a test for the same id in two different places */
  Symbol *id;
  test t;
  complex_test *ct;

  tc = get_new_tc_number();
  for (cc=all_ccs; cc!=NIL; cc=cc->next) {
    if (cc->instantiated_cond->type!=POSITIVE_CONDITION) continue;
    id = referent_of_equality_test (cc->instantiated_cond->data.tests.id_test);
    if ( (id->id.isa_goal || id->id.isa_impasse) &&
         (id->id.tc_num != tc) ) {
      allocate_with_pool (&current_agent(complex_test_pool), &ct);
      ct->type = (id->id.isa_goal) ? GOAL_ID_TEST : IMPASSE_ID_TEST;
      t = make_test_from_complex_test(ct);
      add_new_test_to_test (&(cc->variablized_cond->data.tests.id_test), t);
      id->id.tc_num = tc;
    }
  }
}

/* --------------------------------------------------------------------
                    Reorder Instantiated Conditions

   The Rete routines require the instantiated conditions (on the
   instantiation structure) to be in the same order as the original
   conditions from which the Rete was built.  This means that the
   initial instantiation of the chunk must have its conditions in
   the same order as the variablized conditions.  The trouble is,
   the variablized conditions get rearranged by the reorderer.  So,
   after reordering, we have to rearrange the instantiated conditions
   to put them in the same order as the now-scrambled variablized ones.
   This routine does this.

   Okay, so the obvious way is to have each variablized condition (VCond)
   point to the corresponding instantiated condition (ICond).  Then after
   reordering the VConds, we'd scan through the VConds and say
      VCond->Icond->next = VCond->next->Icond
      VCond->Icond->prev = VCond->prev->Icond
   (with some extra checks for the first and last VCond in the list).

   The problem with this is that it takes an extra 4 bytes per condition,
   for the "ICond" field.  Conditions were taking up a lot of memory in
   my test cases, so I wanted to shrink them.  This routine avoids needing
   the 4 extra bytes by using the following trick:  first "swap out" 4
   bytes from each VCond; then use that 4 bytes for the "ICond" field.
   Now run the above algorithm.  Finally, swap those original 4 bytes
   back in.
-------------------------------------------------------------------- */

void reorder_instantiated_conditions (chunk_cond *top_cc,
                                      condition **dest_inst_top,
                                      condition **dest_inst_bottom) {
  chunk_cond *cc;

  /* --- Step 1:  swap prev pointers out of variablized conds into chunk_conds,
     and swap pointer to the corresponding instantiated conds into the
     variablized conds' prev pointers --- */
  for (cc=top_cc; cc!=NIL; cc=cc->next) {
    cc->saved_prev_pointer_of_variablized_cond = cc->variablized_cond->prev;
    cc->variablized_cond->prev = cc->instantiated_cond;
  }

  /* --- Step 2:  do the reordering of the instantiated conds --- */
  for (cc=top_cc; cc!=NIL; cc=cc->next) {
    if (cc->variablized_cond->next) {
      cc->instantiated_cond->next = cc->variablized_cond->next->prev;
    } else {
      cc->instantiated_cond->next = NIL;
      *dest_inst_bottom = cc->instantiated_cond;
    }
    
    if (cc->saved_prev_pointer_of_variablized_cond) {
      cc->instantiated_cond->prev =
        cc->saved_prev_pointer_of_variablized_cond->prev;
    } else {
      cc->instantiated_cond->prev = NIL;
      *dest_inst_top = cc->instantiated_cond;
    }
  }

  /* --- Step 3:  restore the prev pointers on variablized conds --- */
  for (cc=top_cc; cc!=NIL; cc=cc->next) {
    cc->variablized_cond->prev = cc->saved_prev_pointer_of_variablized_cond;
  }
}

/* --------------------------------------------------------------------
                       Make Clones of Results

   When we build the initial instantiation of the new chunk, we have
   to fill in preferences_generated with *copies* of all the result
   preferences.  These copies are clones of the results.  This routine
   makes these clones and fills in chunk_inst->preferences_generated.
-------------------------------------------------------------------- */

void make_clones_of_results (preference *results, instantiation *chunk_inst) {
  preference *p, *result_p;

  chunk_inst->preferences_generated = NIL;
  for (result_p=results; result_p!=NIL; result_p=result_p->next_result) {
    /* --- copy the preference --- */
    p = make_preference (result_p->type, result_p->id, result_p->attr,
                         result_p->value, result_p->referent);
    symbol_add_ref (p->id);
    symbol_add_ref (p->attr);
    symbol_add_ref (p->value);
    if (preference_is_binary(p->type))
      symbol_add_ref (p->referent);
    /* --- put it onto the list for chunk_inst --- */
    p->inst = chunk_inst;
    insert_at_head_of_dll (chunk_inst->preferences_generated, p,
                           inst_next, inst_prev);
    /* --- insert it into the list of clones for this preference --- */
    p->next_clone = result_p;
    p->prev_clone = result_p->prev_clone;
    result_p->prev_clone = p;
    if (p->prev_clone) p->prev_clone->next_clone = p;
  }
}

/* ====================================================================

                        Chunk Instantiation

   This the main chunking routine.  It takes an instantiation, and a
   flag "allow_variablization"--if FALSE, the chunk will not be
   variablized.  (If TRUE, it may still not be variablized, due to
   chunk-free-problem-spaces, ^quiescence t, etc.)
==================================================================== */


void chunk_instantiation (instantiation *inst, bool allow_variablization) {
  goal_stack_level grounds_level;
  preference *results, *pref;
  action *rhs;
  production *prod;
  instantiation *chunk_inst;
  Symbol *prod_name;
  byte prod_type;
  bool print_name, print_prod;
  byte rete_addition_result;
  condition *lhs_top, *lhs_bottom;
  not *nots;
  chunk_cond *top_cc, *bottom_cc;

  explain_chunk_str temp_explain_chunk;

#ifdef DETAILED_TIMING_STATS
  struct timeval saved_start_tv;
#endif
  
  /* --- if it only matched an attribute impasse, don't chunk --- */
  if (! inst->match_goal) return; 

  /* --- if no preference is above the match goal level, exit --- */
  for (pref=inst->preferences_generated; pref!=NIL; pref=pref->inst_next) {
    if (pref->id->id.level < inst->match_goal_level)
      break;
  }
  if (! pref) return;
  
#ifdef DETAILED_TIMING_STATS
  start_timer (&saved_start_tv);
#endif

  results = get_results_for_instantiation (inst);
  if (!results) goto chunking_done;

  /* --- update flags on goal stack for bottom-up chunking --- */
  { Symbol *g;
    for (g=inst->match_goal->id.higher_goal;
         g && g->id.allow_bottom_up_chunks;
         g=g->id.higher_goal)
      g->id.allow_bottom_up_chunks = FALSE;
  }

  grounds_level = inst->match_goal_level - 1;

  current_agent(backtrace_number)++; 
  if (current_agent(backtrace_number)==0) 
    current_agent(backtrace_number)=1;
  current_agent(grounds_tc)++; 
  if (current_agent(grounds_tc)==0) 
    current_agent(grounds_tc)=1;
  current_agent(potentials_tc)++; 
  if (current_agent(potentials_tc)==0) 
    current_agent(potentials_tc)=1;
  current_agent(locals_tc)++; 
  if (current_agent(locals_tc)==0) 
    current_agent(locals_tc)=1;
  current_agent(grounds) = NIL;
  current_agent(positive_potentials) = NIL;
  current_agent(locals) = NIL;
  current_agent(instantiations_with_nots) = NIL;

  if (allow_variablization && (! current_agent(sysparams)[LEARNING_ALL_GOALS_SYSPARAM]))
    allow_variablization = inst->match_goal->id.allow_bottom_up_chunks;

#ifndef NNPSCM
  /* --- check whether ps name is in chunk_free_problem_spaces --- */
  if (allow_variablization) {
    if (inst->match_goal->id.problem_space_slot->wmes) {
      Symbol *ps_id, *ps_name;
      ps_id = inst->match_goal->id.problem_space_slot->wmes->value;
      ps_name = find_name_of_object (ps_id);
      if (ps_name) {
/* AGR MVL1 begin */
	if (! current_agent(sysparams)[LEARNING_SPECIFY_SYSPARAM]) {
	  if (member_of_list (ps_name, current_agent(chunk_free_problem_spaces))) {
	    allow_variablization = FALSE; 
	    current_agent(chunk_free_flag) = TRUE;
	  }
	}
	else 
	  if (member_of_list (ps_name, current_agent(chunky_problem_spaces))) {
	    allow_variablization = TRUE; 
	    current_agent(chunky_flag) = TRUE;
	  }
	  else {
	    allow_variablization = FALSE; 
	    current_agent(chunky_flag) = FALSE;
	  }
/* AGR MVL1 end */
      }
    }
  }
#else
  if (allow_variablization) {
    Symbol *ps_id, *ps_name;
    slot *ps_slot;
    ps_slot = find_slot(inst->match_goal, current_agent(problem_space_symbol));
    if (ps_slot) {
      ps_name = find_name_of_object (ps_slot->wmes->value);
      if (ps_name) {
/* AGR MVL1 begin */
	if (! current_agent(sysparams)[LEARNING_SPECIFY_SYSPARAM]) {
	  if (member_of_list (ps_name, current_agent(chunk_free_problem_spaces))) {
	    allow_variablization = FALSE; 
	    current_agent(chunk_free_flag) = TRUE;
	  }
	}
	else 
	  if (member_of_list (ps_name, current_agent(chunky_problem_spaces))) {
	    allow_variablization = TRUE; 
	    current_agent(chunky_flag) = TRUE;
	  }
	  else {
	    allow_variablization = FALSE; 
	    current_agent(chunky_flag) = FALSE;
	  }
/* AGR MVL1 end */
      }
    }
  }
#endif

  current_agent(variablize_this_chunk) = allow_variablization;

  /* Start a new structure for this potential chunk */

  if (current_agent(explain_flag)) {
    temp_explain_chunk.conds       = NULL;
    temp_explain_chunk.actions     = NULL;
    temp_explain_chunk.backtrace   = NULL;
    temp_explain_chunk.name[0]     = '\0';
    temp_explain_chunk.all_grounds = NIL;
    temp_explain_chunk.next_chunk  = NULL;
    reset_backtrace_list();
  }
  
  /* --- backtrace through the instantiation that produced each result --- */
  for (pref=results; pref!=NIL; pref=pref->next_result) {
    if (current_agent(sysparams)[TRACE_BACKTRACING_SYSPARAM]) {
      print_string ("\nFor result preference ");
      print_preference (pref);
      print_string (" ");
    }
    backtrace_through_instantiation (pref->inst, grounds_level, NULL, 0);
  }

  current_agent(quiescence_t_flag) = FALSE;

  while (TRUE) {
    trace_locals (grounds_level);
    trace_grounded_potentials ();
    if (! trace_ungrounded_potentials (grounds_level)) break;
  }
  free_list (current_agent(positive_potentials));

  /* --- backtracing done; collect the grounds into the chunk --- */
  { tc_number tc_for_grounds;
    tc_for_grounds = get_new_tc_number();
    build_chunk_conds_for_grounds_and_add_negateds (&top_cc, &bottom_cc,
                                                    tc_for_grounds);
    nots = get_nots_for_instantiated_conditions (current_agent(instantiations_with_nots),
                                                 tc_for_grounds);
  }

  /* --- get symbol for name of new chunk or justification --- */
  if (current_agent(variablize_this_chunk)) {
    prod_name = generate_new_sym_constant ("chunk-",&current_agent(chunk_count));

    current_agent(chunks_this_d_cycle)++;

    prod_type = CHUNK_PRODUCTION_TYPE;
    print_name = current_agent(sysparams)[TRACE_CHUNK_NAMES_SYSPARAM];
    print_prod = current_agent(sysparams)[TRACE_CHUNKS_SYSPARAM];
  } else {
    prod_name = generate_new_sym_constant ("justification-",
                                           &current_agent(justification_count));
    prod_type = JUSTIFICATION_PRODUCTION_TYPE;
    print_name = current_agent(sysparams)[TRACE_JUSTIFICATION_NAMES_SYSPARAM];
    print_prod = current_agent(sysparams)[TRACE_JUSTIFICATIONS_SYSPARAM];
  }
/* AGR 617/634 begin */
  if (print_name) {
    if (get_printer_output_column()!=1) print ("\n");
    print_with_symbols ("Building %y", prod_name);
  }
/* AGR 617/634 end */

  /* --- if there aren't any grounds, exit --- */
  if (! top_cc) {
    if (current_agent(sysparams)[PRINT_WARNINGS_SYSPARAM])
      print_string (" Warning: chunk has no grounds, ignoring it.");
    goto chunking_done;
  }

  /* MVP 6-8-94 */
  if (current_agent(chunks_this_d_cycle) >
      current_agent(sysparams)[MAX_CHUNKS_SYSPARAM]) {
    if (current_agent(sysparams)[PRINT_WARNINGS_SYSPARAM])
      print ("\nWarning: reached max-chunks! Halting system.");
    current_agent(max_chunks_reached) = TRUE;
    goto chunking_done;
  }

  /* --- variablize it --- */
  lhs_top = top_cc->variablized_cond;
  lhs_bottom = bottom_cc->variablized_cond;
  reset_variable_generator (lhs_top, NIL);
  current_agent(variablization_tc) = get_new_tc_number();
  variablize_condition_list (lhs_top);
  variablize_nots_and_insert_into_conditions (nots, lhs_top);
  rhs = copy_and_variablize_result_list (results);

  /* --- add goal/impasse tests to it --- */
  add_goal_or_impasse_tests (top_cc);

  /* --- reorder lhs and make the production --- */

  prod = make_production (prod_type, prod_name, &lhs_top, &lhs_bottom, &rhs,
                          FALSE);

  if (!prod) {
    print ("\nUnable to reorder this chunk:\n  ");
    print_condition_list (lhs_top, 2, FALSE);
    print ("\n  -->\n   ");
    print_action_list (rhs, 3, FALSE);
    print ("\n\n(Ignoring this chunk.  Weird things could happen from now on...)\n");
    goto chunking_done; /* this leaks memory but who cares */
  }

  { condition *inst_lhs_top, *inst_lhs_bottom;

    reorder_instantiated_conditions (top_cc, &inst_lhs_top, &inst_lhs_bottom);

    /* Record the list of grounds in the order they will appear in the chunk. */
    if (current_agent(explain_flag))
      temp_explain_chunk.all_grounds = inst_lhs_top;   /* Not a copy yet */

    allocate_with_pool (&current_agent(instantiation_pool), &chunk_inst);
    chunk_inst->prod = prod;
    chunk_inst->top_of_instantiated_conditions = inst_lhs_top;
    chunk_inst->bottom_of_instantiated_conditions = inst_lhs_bottom;
    chunk_inst->nots = nots;

    /* If:
         - you don't want to variablize this chunk, and
         - the reason is ONLY that it's chunk free, and
         - NOT that it's also quiescence, then
         it's okay to variablize through this instantiation later.
     */

/* AGR MVL1 begin */
    if (! current_agent(sysparams)[LEARNING_SPECIFY_SYSPARAM]) {
      if ((! current_agent(variablize_this_chunk)) 
	  && (current_agent(chunk_free_flag)) 
	  && (! current_agent(quiescence_t_flag)))
	chunk_inst->okay_to_variablize = TRUE;
      else
	chunk_inst->okay_to_variablize = current_agent(variablize_this_chunk);
    }
    else {
      if ((! current_agent(variablize_this_chunk)) 
	  && (! current_agent(chunky_flag)) 
	  && (! current_agent(quiescence_t_flag)))
	chunk_inst->okay_to_variablize = TRUE;
      else
	chunk_inst->okay_to_variablize = current_agent(variablize_this_chunk);
    }
/* AGR MVL1 end */

    chunk_inst->in_ms = TRUE;  /* set TRUE for now, we'll find out later... */
    make_clones_of_results (results, chunk_inst);
    fill_in_new_instantiation_stuff (chunk_inst, TRUE);
  }

  rete_addition_result = add_production_to_rete (prod, lhs_top, chunk_inst,
                                                 print_name);

  /* If didn't immediately excise the chunk from the rete net   
     then record the temporary structure in the list of explained chunks. */

  if (current_agent(explain_flag))
    if ((rete_addition_result != DUPLICATE_PRODUCTION) &&
	((prod_type != JUSTIFICATION_PRODUCTION_TYPE) ||
	 (rete_addition_result != REFRACTED_INST_DID_NOT_MATCH) )) {
      temp_explain_chunk.conds = lhs_top;  /* The variablized list */
      strcpy(temp_explain_chunk.name,prod_name->sc.name);
      explain_add_temp_to_chunk_list(&temp_explain_chunk,results);
    }

  /* --- deallocate chunks conds and variablized conditions --- */
  deallocate_condition_list (lhs_top);
  { chunk_cond *cc;
    while (top_cc) {
      cc = top_cc;
      top_cc = cc->next;
      free_with_pool (&current_agent(chunk_cond_pool), cc);
    }
  }

  if (print_prod && (rete_addition_result!=DUPLICATE_PRODUCTION)) {
    print_string ("\n");
    print_production (prod, FALSE);
  }
  
  if (rete_addition_result==DUPLICATE_PRODUCTION) {
    excise_production (prod, FALSE);
  } else if ((prod_type==JUSTIFICATION_PRODUCTION_TYPE) &&
             (rete_addition_result==REFRACTED_INST_DID_NOT_MATCH)) {
    excise_production (prod, FALSE);
  }

  if (rete_addition_result!=REFRACTED_INST_MATCHED) {
    /* --- it didn't match, or it was a duplicate production --- */
    /* --- tell the firer it didn't match, so it'll only assert the
       o-supported preferences --- */
    chunk_inst->in_ms = FALSE;
  }

  /* --- assert the preferences --- */
  chunk_inst->next = current_agent(newly_created_instantiations);
  current_agent(newly_created_instantiations) = chunk_inst;

#ifdef DETAILED_TIMING_STATS
  stop_timer (&saved_start_tv, &current_agent(chunking_cpu_time));
#endif

  /* MVP 6-8-94 */
  if (!current_agent(max_chunks_reached))
    chunk_instantiation (chunk_inst, current_agent(variablize_this_chunk));
  
  return;

  chunking_done: {}
#ifdef DETAILED_TIMING_STATS
  stop_timer (&saved_start_tv, &current_agent(chunking_cpu_time));
#endif
}

/* --------------------------------------------------------------------

                        Chunker Initialization

   Init_chunker() is called at startup time to do initialization here.
-------------------------------------------------------------------- */

void init_chunker (void) {
  init_memory_pool (&current_agent(chunk_cond_pool), sizeof(chunk_cond), "chunk condition");
  init_chunk_cond_set (&current_agent(negated_set));
}
