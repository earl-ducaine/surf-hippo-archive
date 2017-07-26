/* 
 * $Id: rete.c,v 1.5 1995/01/20 00:14:08 rempel Exp $
 * $Log: rete.c,v $
 * Revision 1.5  1995/01/20  00:14:08  rempel
 * moved defn of DEBUG_INSTANTIATIONS to recmem.c for release of 6.2.4c
 *
 * Revision 1.4  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.3  1994/08/23  10:37:38  portelli
 * For 6.2.4
 *
 * Revision 1.2  1993/11/21  17:10:15  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:51  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:23:59  jtraub
 * 6.1_checkin
 *
 * Revision 9.3  1993/06/14  20:07:02  jtraub
 * added MS printing enhancement.
 *
 * Revision 9.2  1993/05/10  18:45:25  jtraub
 * added RCS header information
 *
 */

/* ======================================================================

                      Rete Net Routines for Soar 6

====================================================================== */

#include "soar.h"

#ifdef _WINDOWS
#define HASH_SIZE       13
#else
#define HASH_SIZE       14
#endif

/* Uncomment the following line to get pnode printouts */
/* #define DEBUG_RETE_PNODES */

/* Comment out the following line to avoid right mem unlinking */
#define DO_RIGHT_UNLINKING

/* Comment out the following line to avoid the overhead of keeping statistics
   on token changes */
/* #define TOKEN_STATS */


/* ----------------------------------------------------------------------

                 Rete Net Structures and Declarations

---------------------------------------------------------------------- */

/* --- dll of all wmes currently in the rete:  this is needed to
       initialize newly created alpha memories --- */



/* --- hash tables in which all the alpha memories are stored --- */


/* --- types of tests found at beta nodes --- */
#define CONSTANT_RELATIONAL_RETE_TEST 0x00
#define VARIABLE_RELATIONAL_RETE_TEST 0x10
#define DISJUNCTION_RETE_TEST         0x20
#define ID_IS_GOAL_RETE_TEST          0x30
#define ID_IS_IMPASSE_RETE_TEST       0x31
#define test_is_constant_relational_test(x) (((x) & 0xF0)==0x00)
#define test_is_variable_relational_test(x) (((x) & 0xF0)==0x10)

/* --- for the last two (i.e., the relational tests), we add in one of
       the following, to specifiy the kind of relation --- */
#define RELATIONAL_EQUAL_RETE_TEST            0x00
#define RELATIONAL_NOT_EQUAL_RETE_TEST        0x01
#define RELATIONAL_LESS_RETE_TEST             0x02
#define RELATIONAL_GREATER_RETE_TEST          0x03
#define RELATIONAL_LESS_OR_EQUAL_RETE_TEST    0x04
#define RELATIONAL_GREATER_OR_EQUAL_RETE_TEST 0x05
#define RELATIONAL_SAME_TYPE_RETE_TEST        0x06
#define kind_of_relational_test(x) ((x) & 0x0F)
#define test_is_not_equal_test(x) (((x)==0x01) || ((x)==0x11))

/* define an equality predicate for var_location structures */
#define var_locations_equal(v1,v2) \
  ( ((v1).levels_up==(v2).levels_up) && ((v1).field_num==(v2).field_num) )

/* --- extract field (id/attr/value) from wme --- */
/* WARNING: this relies on the id/attr/value fields being consecutive in
   the wme structure (defined in soar.h) */
#define field_from_wme(wme,field_num) \
  ( (&((wme)->id))[(field_num)] )
                                    
/* --- types and structure of beta nodes --- */

#define DUMMY_TOP_BNODE 0
#define POSITIVE_BNODE 1
#define UNHASHED_POSITIVE_BNODE 2
#define NEGATIVE_BNODE 3
#define UNHASHED_NEGATIVE_BNODE 4
#define CN_BNODE 5
#define CN_PARTNER_BNODE 6
#define P_BNODE 7
#define DUMMY_MATCHES_BNODE 8

#define NUM_BNODE_TYPES 9
#define bnode_is_positive(x) (((x)>=1)&&((x)<=2))
#define bnode_is_negative(x) (((x)>=3)&&((x)<=4))
#define bnode_is_normal(x)   (((x)>=1)&&((x)<=4))
#define bnode_is_hashed(x)   (((x)==1)||((x)==3))


/* --- macros for unlinking beta nodes from their right memories --- */
#define DONT_RIGHT_UNLINK 1
#define RIGHT_LINKED 2
#define RIGHT_UNLINKED 3

#define relink_to_right_mem(node) { \
  insert_at_head_of_dll ((node)->d.norm.alpha_mem->beta_nodes, (node), \
                         d.norm.next_from_alpha_mem, \
                         d.norm.prev_from_alpha_mem); \
  (node)->right_mem_link_status = RIGHT_LINKED; }

#define unlink_from_right_mem(node) { \
  remove_from_dll ((node)->d.norm.alpha_mem->beta_nodes, (node), \
                   d.norm.next_from_alpha_mem, \
                   d.norm.prev_from_alpha_mem); \
  (node)->right_mem_link_status = RIGHT_UNLINKED; }

#define address_of_bucket_header_cell(t,hv) \
  ( (t)->buckets + ((hv)&masks_for_n_low_order_bits[(t)->log2size]) )

#define contents_of_bucket_header_cell(t,hv) \
  (* (address_of_bucket_header_cell((t),(hv))))

#define insert_into_hash_table(t,item,hv) { \
  void **header_zy37; \
  header_zy37 = address_of_bucket_header_cell((t),(hv)); \
  (item)->next_in_bucket = *header_zy37; \
  *header_zy37 = (item); }

void init_token_hash_table (token_hash_table *ht, short minimum_log2size) {
  ht->count = 0;
  ht->size = (((unsigned long)1) << minimum_log2size);
  ht->log2size = minimum_log2size;
  ht->minimum_log2size = minimum_log2size;
  ht->buckets = allocate_memory_and_zerofill (sizeof(char *) * ht->size,
                                              HASH_TABLE_MEM_USAGE);
}



/* --- beta node routines for left/right token/wme changes --- */
void (*(left_addition_routines[NUM_BNODE_TYPES])) (rete_node *node,
                                                   token *tok,
                                                   wme *w);
void (*(left_removal_routines[NUM_BNODE_TYPES])) (rete_node *node,
                                                  token *t,
                                                  wme *w);
void (*(right_addition_routines[NUM_BNODE_TYPES])) (rete_node *node, wme *w);
void (*(right_removal_routines[NUM_BNODE_TYPES])) (rete_node *node, wme *w);


#ifdef TOKEN_STATS
#define token_added() { current_agent(token_additions)++; }
#define token_deleted() { current_agent(token_deletions)++; }
#else
#define token_added() {}
#define token_deleted() {}
#endif

/* ----------------------------------------------------------------------

                           Match Set Changes

---------------------------------------------------------------------- */

bool any_assertions_or_retractions_ready (void) {
  return (current_agent(ms_assertions) || current_agent(ms_retractions));
}

bool get_next_assertion (production **prod,
                         struct token_struct **tok,
                         wme **w) {
  ms_change *msc;

  if (! current_agent(ms_assertions)) return FALSE;
  msc = current_agent(ms_assertions);
  remove_from_dll (current_agent(ms_assertions), msc, next, prev);
  remove_from_dll (msc->p_node->d.p.tentative_assertions, msc,
                   next_of_node, prev_of_node);
  *prod = msc->p_node->d.p.prod;
  *tok = msc->tok;
  *w = msc->w;
  free_with_pool (&current_agent(ms_change_pool), msc);
  return TRUE;
}

bool get_next_retraction (instantiation **inst) {
  ms_change *msc;

  if (! current_agent(ms_retractions)) return FALSE;
  msc = current_agent(ms_retractions);
  remove_from_dll (current_agent(ms_retractions), msc, next, prev);
  if (msc->p_node)
    remove_from_dll (msc->p_node->d.p.tentative_retractions, msc,
                     next_of_node, prev_of_node);
  *inst = msc->inst;
  free_with_pool (&current_agent(ms_change_pool), msc);
  return TRUE;
}

/* ----------------------------------------------------------------------
                    Alpha Portion of the Rete Net

The alpha (top) part of the rete net consists of the alpha memories.
Each of these memories is stored in one of 16 hash tables, depending
on which fields it tests:

      bit 0 (value 1) indicates it tests the id slot
      bit 1 (value 2) indicates it tests the attr slot
      bit 2 (value 4) indicates it tests the value slot
      bit 3 (value 8) indicates it tests for an acceptable preference

The hash tables are dynamically resized hash tables.
---------------------------------------------------------------------- */

#define wme_matches_alpha_mem(w,am) ( \
  (((am)->id==NIL) || ((am)->id==(w)->id)) && \
  (((am)->attr==NIL) || ((am)->attr==(w)->attr)) && \
  (((am)->value==NIL) || ((am)->value==(w)->value)) && \
  ((am)->acceptable==(w)->acceptable))

#define alpha_hash_value(i,a,v,num_bits) \
 ( ( ((i) ? ((Symbol *)(i))->common.hash_id : 0) ^ \
     ((a) ? ((Symbol *)(a))->common.hash_id : 0) ^ \
     ((v) ? ((Symbol *)(v))->common.hash_id : 0) ) & \
   masks_for_n_low_order_bits[(num_bits)] )

/* --- rehash funciton for resizable hash table routines --- */
unsigned long hash_alpha_mem (void *item, short num_bits) {
  alpha_mem *am;

  am = item;
  return alpha_hash_value (am->id, am->attr, am->value, num_bits);
}

#define table_for_tests(id,attr,value,acceptable) \
  current_agent(alpha_hash_tables) [ ((id) ? 1 : 0) + ((attr) ? 2 : 0) + ((value) ? 4 : 0) + \
                      ((acceptable) ? 8 : 0) ]

#define get_next_alpha_mem_id() (current_agent(alpha_mem_id_counter)++)

void init_new_alpha_mem (alpha_mem *am, Symbol *id, Symbol *attr,
                         Symbol *value, bool acceptable) {
  am->next_in_hash_table = NIL;
  am->right_mems = NIL;
  am->beta_nodes = NIL;
  am->reference_count = 1;
  am->id = id;
  am->attr = attr;
  am->value = value;
  am->acceptable = acceptable;
  am->am_id = get_next_alpha_mem_id();
}

void add_wme_to_alpha_mem (wme *w, alpha_mem *am) {
  right_mem *rm;
  unsigned long hv;

  allocate_with_pool (&current_agent(right_mem_pool), &rm);
  rm->rmi.am = am;
  rm->rmi.w = w;
  hv = am->am_id ^ w->id->common.hash_id;
  insert_into_hash_table (&current_agent(right_ht), &(rm->rmi), hv);
  insert_at_head_of_dll (am->right_mems, rm, next, prev);
}

void remove_wme_from_alpha_mem (wme *w, alpha_mem *am) {
  right_mem_item *rmi, **prev_rmi_pointer;
  right_mem *rm;
  int offset_of_rmi_in_rm;
  unsigned long hv;

  rm = NIL;     /* Added to placate the native HP cc */
  
  hv = am->am_id ^ w->id->common.hash_id;
  prev_rmi_pointer =
    (right_mem_item **) address_of_bucket_header_cell (&current_agent(right_ht), hv);
  while (TRUE) {
    rmi = *prev_rmi_pointer;
    if ((rmi->am == am) && (rmi->w == w)) break;
    prev_rmi_pointer = &(rmi->next_in_bucket);
  }

  *prev_rmi_pointer = rmi->next_in_bucket;

  offset_of_rmi_in_rm = (char *)(&(rm->rmi)) - (char *)(rm);
  rm = (right_mem *) ( ((char *)rmi) - offset_of_rmi_in_rm );
  remove_from_dll (am->right_mems, rm, next, prev);
  free_with_pool (&current_agent(right_mem_pool), rm);
}

alpha_mem *find_alpha_mem (Symbol *id, Symbol *attr,
                           Symbol *value, bool acceptable) {
  hash_table *ht;
  alpha_mem *am;
  unsigned long hash_value;

  ht = table_for_tests (id, attr, value, acceptable);
  hash_value = alpha_hash_value (id, attr, value, ht->log2size);

  for (am = (alpha_mem *) (*(ht->buckets+hash_value)); am!=NIL;
       am=am->next_in_hash_table)
    if ((am->id==id) && (am->attr==attr) &&
        (am->value==value) && (am->acceptable==acceptable))
      return am;
  return NIL;
}

alpha_mem *find_or_make_alpha_mem (Symbol *id, Symbol *attr,
                                   Symbol *value, bool acceptable) {
  hash_table *ht;
  alpha_mem *am, *more_general_am;
  wme *w;
  right_mem *rm;

  /* --- look for an existing alpha mem --- */
  am = find_alpha_mem (id, attr, value, acceptable);
  if (am) {
    am->reference_count++;
    return am;
  }
  
  /* --- no existing alpha_mem found, so create a new one --- */
  allocate_with_pool (&current_agent(alpha_mem_pool), &am);
  init_new_alpha_mem (am, id, attr, value, acceptable);
  if (id) symbol_add_ref (id);
  if (attr) symbol_add_ref (attr);
  if (value) symbol_add_ref (value);
  ht = table_for_tests (id, attr, value, acceptable);
  add_to_hash_table (ht, am);
  
  /* --- fill new mem with any existing matching WME's --- */
  more_general_am = NIL;
  if (id)
    more_general_am = find_alpha_mem (NIL, attr, value, acceptable);
  if (!more_general_am && value)
    more_general_am = find_alpha_mem (NIL, attr, NIL, acceptable);
  if (more_general_am) {
    /* --- fill new mem using the existing more general one --- */
    for (rm=more_general_am->right_mems; rm!=NIL; rm=rm->next)
      if (wme_matches_alpha_mem (rm->rmi.w, am))
        add_wme_to_alpha_mem (rm->rmi.w, am);
  } else {
    /* --- couldn't find such an existing mem, so do it the hard way --- */
    for (w=current_agent(all_wmes_in_rete); w!=NIL; w=w->rete_next)
      if (wme_matches_alpha_mem (w,am)) add_wme_to_alpha_mem (w, am);
  }
  
  return am;
}

void remove_ref_to_alpha_mem (alpha_mem *am) {
  hash_table *ht;
 
  am->reference_count--;
  if (am->reference_count!=0) return;
  /* --- remove from hash table, and deallocate the alpha_mem --- */
  ht = table_for_tests (am->id, am->attr, am->value, am->acceptable);
  remove_from_hash_table (ht, am);
  if (am->id) symbol_remove_ref (am->id);
  if (am->attr) symbol_remove_ref (am->attr);
  if (am->value) symbol_remove_ref (am->value);
  while (am->right_mems) remove_wme_from_alpha_mem (am->right_mems->rmi.w, am);
  free_with_pool (&current_agent(alpha_mem_pool), am);
}

void add_wme_to_aht (hash_table *ht, unsigned long hash_value, wme *w) {
  alpha_mem *am;
  rete_node *node;
 
  hash_value = hash_value & masks_for_n_low_order_bits[ht->log2size];
  am = (alpha_mem *) (*(ht->buckets+hash_value));
  while (am!=NIL) {
    if (wme_matches_alpha_mem (w,am)) {
      /* --- found the right alpha memory, first add the wme --- */
      add_wme_to_alpha_mem (w, am);

      /* --- now call the beta nodes --- */
      /* WARNING: for this to work, decendent nodes must be on the beta_nodes
         chain BEFORE their ancestor nodes */
      for (node=am->beta_nodes; node!=NIL;
           node=node->d.norm.next_from_alpha_mem)
        (*(right_addition_routines[node->node_type]))(node,w);
      return; /* only one possible alpha memory per table could match */
    }
    am = am->next_in_hash_table;
  }
}
     
void remove_wme_from_aht (hash_table *ht, unsigned long hash_value, wme *w) {
  alpha_mem *am;
  rete_node *node;
 
  hash_value = hash_value & masks_for_n_low_order_bits[ht->log2size];
  am = (alpha_mem *) (*(ht->buckets+hash_value));
  while (am!=NIL) {
    if (wme_matches_alpha_mem (w,am)) {
      /* --- found the right alpha memory, first remove the wme --- */
      remove_wme_from_alpha_mem (w, am);

      /* --- now call the beta nodes --- */
      /* WARNING: for this to work, decendent nodes must be on the beta_nodes
         chain BEFORE their ancestor nodes */
      for (node=am->beta_nodes; node!=NIL;
           node=node->d.norm.next_from_alpha_mem)
        (*(right_removal_routines[node->node_type]))(node,w);
      return; /* only one possible alpha memory per table could match */
    }
    am = am->next_in_hash_table;
  }
}
     
#define xor(i,a,v) ((i) ^ (a) ^ (v))

void add_wme_to_rete (wme *w) {
  unsigned long hi, ha, hv;
  
  /* --- add w to all_wmes_in_rete --- */
  insert_at_head_of_dll (current_agent(all_wmes_in_rete), w, rete_next, rete_prev);
  current_agent(num_wmes_in_rete)++;
  
  /* --- add w to the appropriate alpha_mem in each of 8 possible tables --- */
  hi = w->id->common.hash_id;
  ha = w->attr->common.hash_id;
  hv = w->value->common.hash_id;

  if (w->acceptable) {
    add_wme_to_aht (current_agent(alpha_hash_tables)[8],  xor( 0, 0, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[9],  xor(hi, 0, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[10], xor( 0,ha, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[11], xor(hi,ha, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[12], xor( 0, 0,hv), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[13], xor(hi, 0,hv), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[14], xor( 0,ha,hv), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[15], xor(hi,ha,hv), w);
  } else {
    add_wme_to_aht (current_agent(alpha_hash_tables)[0],  xor( 0, 0, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[1],  xor(hi, 0, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[2],  xor( 0,ha, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[3],  xor(hi,ha, 0), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[4],  xor( 0, 0,hv), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[5],  xor(hi, 0,hv), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[6],  xor( 0,ha,hv), w);
    add_wme_to_aht (current_agent(alpha_hash_tables)[7],  xor(hi,ha,hv), w);
  }
}

void remove_wme_from_rete (wme *w) {
  unsigned long hi, ha, hv;

  /* --- remove w from all_wmes_in_rete --- */
  remove_from_dll (current_agent(all_wmes_in_rete), w, rete_next, rete_prev);
  current_agent(num_wmes_in_rete)--;
  
  /* --- remove w from the appropriate alpha_mem in each of 8 tables --- */
  hi = w->id->common.hash_id;
  ha = w->attr->common.hash_id;
  hv = w->value->common.hash_id;

  if (w->acceptable) {
    remove_wme_from_aht (current_agent(alpha_hash_tables)[8],  xor( 0, 0, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[9],  xor(hi, 0, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[10], xor( 0,ha, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[11], xor(hi,ha, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[12], xor( 0, 0,hv), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[13], xor(hi, 0,hv), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[14], xor( 0,ha,hv), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[15], xor(hi,ha,hv), w);
  } else {
    remove_wme_from_aht (current_agent(alpha_hash_tables)[0],  xor( 0, 0, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[1],  xor(hi, 0, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[2],  xor( 0,ha, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[3],  xor(hi,ha, 0), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[4],  xor( 0, 0,hv), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[5],  xor(hi, 0,hv), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[6],  xor( 0,ha,hv), w);
    remove_wme_from_aht (current_agent(alpha_hash_tables)[7],  xor(hi,ha,hv), w);
  }
}


/* --------------------------------------------------------------------
                   Beta Portion of the Rete Net

The dummy top node always has one token in it (WME=NIL).  This is just
there so that (real) root nodes in the beta net can be handled the same
as non-root nodes.
-------------------------------------------------------------------- */

 
void init_dummy_top_node () {
  current_agent(dummy_top_node).node_type = DUMMY_TOP_BNODE;
  current_agent(dummy_top_node).parent = NIL;
  current_agent(dummy_top_node).first_child = NIL;
  current_agent(dummy_top_node).next_sibling = NIL;

  current_agent(dummy_matches_node).node_type = DUMMY_MATCHES_BNODE;
  current_agent(dummy_matches_node).parent = NIL;
  current_agent(dummy_matches_node).first_child = NIL;
  current_agent(dummy_matches_node).next_sibling = NIL;
}

void remove_node_from_parents_list_of_children (rete_node *node) {
  rete_node *prev_sibling;

  prev_sibling = node->parent->first_child;
  if (prev_sibling == node) {
    node->parent->first_child = node->next_sibling;
    return;
  }
  while (prev_sibling->next_sibling != node)
    prev_sibling = prev_sibling->next_sibling;
  prev_sibling->next_sibling = node->next_sibling;
}

/* -------------------------------------------------------------------
                               Varnames

   Varnames and Node_Varnames structures are used to record the names
   of variables bound (i.e., equality tested) at rete nodes.  For each
   production, a chain of node_varnames structures is built which
   parallels the structure of the rete net (i.e., the portion of the rete
   used for that production).  There is a node_varnames structure for
   each beta node in that part, giving the names of variable bound in
   the id, attribute, and value fields of the condition at that node.

   At each field, we could bind zero, one , or more variables.  To
   save space, we use some bit-twiddling here.  A "varnames" represents
   zero or more variables:   NIL means zero; a pointer (with the low-order
   bit being 0) to a variable means just that one variable; and any
   other pointer (with the low-order bit set to 1) points (minus 1, of
   course) to a consed list of variables.
------------------------------------------------------------------- */



#define one_var_to_varnames(x) ((varnames *) (x))
#define var_list_to_varnames(x) ((varnames *) (((char *)(x)) + 1))
#define varnames_is_one_var(x) (! (varnames_is_var_list(x)))
#define varnames_is_var_list(x) (((unsigned long)(x)) & 1)
#define varnames_to_one_var(x) ((Symbol *) (x))
#define varnames_to_var_list(x) ((list *) (((char *)(x)) - 1))

bool symbol_is_in_varnames (Symbol *var, varnames *names) {
  if (names == NIL) return FALSE;
  if (varnames_is_one_var(names)) {
    if (var == varnames_to_one_var(names)) return TRUE;
    return FALSE;
  }
  /* --- otherwise names is a list --- */
  return member_of_list (var, varnames_to_var_list(names));
}

varnames *add_var_to_varnames (Symbol *var, varnames *old_varnames) {
  cons *c1, *c2;

  symbol_add_ref (var);
  if (old_varnames == NIL)
    return one_var_to_varnames(var);
  if (varnames_is_one_var(old_varnames)) {
    allocate_cons (&c1);
    allocate_cons (&c2);
    c1->first = var;
    c1->rest = c2;
    c2->first = varnames_to_one_var(old_varnames);
    c2->rest = NIL;
    return var_list_to_varnames(c1);
  }
  /* --- otherwise old_varnames is a list --- */
  allocate_cons (&c1);
  c1->first = var;
  c1->rest = varnames_to_var_list(old_varnames);
  return var_list_to_varnames(c1);
}

void deallocate_varnames (varnames *vn) {
  Symbol *sym;
  list *symlist;

  if (vn == NIL) return;
  if (varnames_is_one_var(vn)) {
    sym = varnames_to_one_var(vn);
    symbol_remove_ref (sym);
  } else {
    symlist = varnames_to_var_list(vn);
    deallocate_symbol_list_removing_references (symlist);
  }
}

void deallocate_node_varnames (rete_node *node, rete_node *cutoff,
                               node_varnames *nvn) {
  node_varnames *temp;

  while (node!=cutoff) {
    if (node->node_type==CN_BNODE) {
      deallocate_node_varnames (node->d.cn.partner->parent, node->parent,
                                nvn->data.bottom_of_subconditions);
    } else {
      deallocate_varnames (nvn->data.fields.id_varnames);
      deallocate_varnames (nvn->data.fields.attr_varnames);
      deallocate_varnames (nvn->data.fields.value_varnames);
    }
    node = node->parent;
    temp = nvn;
    nvn = nvn->parent;
    free_with_pool (&current_agent(node_varnames_pool), temp);
  }
}

/* -------------------------------------------------------------------
                          Find Var Location

   This routine finds the most recent place a variable was bound.
   This routine scans up the net, starting with the current node,
   looking for a field in which the given variable is bound.  For
   bindings, it looks at the current node plus any positive ancestor
   nodes.

   If a binding is found, its location is stored in the parameter *result,
   and the function returns TRUE.  If no binding is found, the function
   returns FALSE.
------------------------------------------------------------------- */

bool find_var_location_in_node_varnames (Symbol *var,
                                         node_varnames *nv,
                                         rete_node_level levels_up,
                                         var_location *result) {
  if (symbol_is_in_varnames (var, nv->data.fields.id_varnames)) {
    result->levels_up = levels_up;
    result->field_num = 0;
    return TRUE;
  }
  if (symbol_is_in_varnames (var, nv->data.fields.attr_varnames)) {
    result->levels_up = levels_up;
    result->field_num = 1;
    return TRUE;
  }
  if (symbol_is_in_varnames (var, nv->data.fields.value_varnames)) {
    result->levels_up = levels_up;
    result->field_num = 2;
    return TRUE;
  }
  return FALSE; 
}

bool find_var_location (Symbol *var,
                        rete_node *parent_node,
                        node_varnames *parent_node_varnames,
                        node_varnames *current_node_varnames,
                        var_location *result) {
  rete_node *node;
  node_varnames *nvn;
  rete_node_level levels_up;

  if (find_var_location_in_node_varnames (var, current_node_varnames,
                                          0, result))
    return TRUE;
 
  node = parent_node;
  nvn = parent_node_varnames;
  levels_up = 1;
  while (node->node_type != DUMMY_TOP_BNODE) {
    if (bnode_is_positive(node->node_type))
      if (find_var_location_in_node_varnames (var, nvn, levels_up, result))
        return TRUE;
    node = node->parent;
    nvn = nvn->parent;
    levels_up++;
  }
  return FALSE; /* reached the top of the net without finding it */
}

/* -------------------------------------------------------------------

                 Updating varnames for a condition

------------------------------------------------------------------- */

varnames *add_varnames_for_test (test t,
                                 rete_node *parent,
                                 node_varnames *parent_nvn,
                                 node_varnames *current_nvn,
                                 bool dense,
                                 varnames *starting_vn) {
  var_location dummy;
  cons *c;
  Symbol *referent;
  complex_test *ct;

  if (test_is_blank_test(t)) return starting_vn;
  if (test_is_blank_or_equality_test(t)) {
    referent = referent_of_equality_test(t);
    if (referent->common.symbol_type==VARIABLE_SYMBOL_TYPE)
      if (dense || (! find_var_location (referent, parent, parent_nvn,
                                         current_nvn, &dummy)))
        starting_vn = add_var_to_varnames (referent, starting_vn);
    return starting_vn;
  }

  ct = complex_test_from_test(t);
  
  if (ct->type==CONJUNCTIVE_TEST) {
    for (c=ct->data.conjunct_list; c!=NIL; c=c->rest)
      starting_vn = add_varnames_for_test (c->first, parent, parent_nvn,
                                           current_nvn, dense, starting_vn);
  }
  return starting_vn;
}

node_varnames *make_nvn_for_condition (condition *cond,
                                       rete_node *parent,
                                       node_varnames *parent_nvn,
                                       bool dense) {
  node_varnames *new;

  if (cond->type==CONJUNCTIVE_NEGATION_CONDITION) {
    print ("Internal error: make_nvn_for_condition called with NCC\n");
    abort_with_fatal_error();
  }

  allocate_with_pool (&current_agent(node_varnames_pool), &new);
  new->parent = parent_nvn;
  new->data.fields.id_varnames = NIL;
  new->data.fields.attr_varnames = NIL;
  new->data.fields.value_varnames = NIL;
  new->data.fields.id_varnames = add_varnames_for_test
    (cond->data.tests.id_test, parent, parent_nvn, new, dense, NIL);
  new->data.fields.attr_varnames = add_varnames_for_test
    (cond->data.tests.attr_test, parent, parent_nvn, new, dense, NIL);
  new->data.fields.value_varnames = add_varnames_for_test
    (cond->data.tests.value_test, parent, parent_nvn, new, dense, NIL);
  return new;
}

/* ---------------------------------------------------------------------

          test_type <---> relational_rete_test_type conversions

--------------------------------------------------------------------- */

byte test_type_to_relational_test_type[256];
byte relational_test_type_to_test_type[256];

/* Warning: the two items below must not be the same as any xxx_TYPE's defined
   in soar.h for the types of complex_test's */
#define EQUAL_TEST_TYPE 254 
#define ERROR_TEST_TYPE 255

void init_test_type_conversion_tables (void) {
  int i;

  for (i=0; i<256; i++) test_type_to_relational_test_type[i] = ERROR_TEST_TYPE;
  for (i=0; i<256; i++) relational_test_type_to_test_type[i] = ERROR_TEST_TYPE;

  /* we don't need ...[equal test] */
  test_type_to_relational_test_type[NOT_EQUAL_TEST] =
    RELATIONAL_NOT_EQUAL_RETE_TEST;
  test_type_to_relational_test_type[LESS_TEST] =
    RELATIONAL_LESS_RETE_TEST;
 test_type_to_relational_test_type[GREATER_TEST] =
    RELATIONAL_GREATER_RETE_TEST;
  test_type_to_relational_test_type[LESS_OR_EQUAL_TEST] =
    RELATIONAL_LESS_OR_EQUAL_RETE_TEST;
  test_type_to_relational_test_type[GREATER_OR_EQUAL_TEST] =
    RELATIONAL_GREATER_OR_EQUAL_RETE_TEST;
  test_type_to_relational_test_type[SAME_TYPE_TEST] =
    RELATIONAL_SAME_TYPE_RETE_TEST;

  relational_test_type_to_test_type[RELATIONAL_EQUAL_RETE_TEST] =
    EQUAL_TEST_TYPE;
  relational_test_type_to_test_type[RELATIONAL_NOT_EQUAL_RETE_TEST] =
    NOT_EQUAL_TEST;
  relational_test_type_to_test_type[RELATIONAL_LESS_RETE_TEST] =
    LESS_TEST;
  relational_test_type_to_test_type[RELATIONAL_GREATER_RETE_TEST] =
    GREATER_TEST;
  relational_test_type_to_test_type[RELATIONAL_LESS_OR_EQUAL_RETE_TEST] =
    LESS_OR_EQUAL_TEST;
  relational_test_type_to_test_type[RELATIONAL_GREATER_OR_EQUAL_RETE_TEST] =
    GREATER_OR_EQUAL_TEST;
  relational_test_type_to_test_type[RELATIONAL_SAME_TYPE_RETE_TEST] =
    SAME_TYPE_TEST;
}


/* ------------------------------------------------------------------------
   Deallocates a list of rete test structures, removing references to
   symbols, etc.
------------------------------------------------------------------------ */

void deallocate_rete_test_list (rete_test *rt) {
  rete_test *next_rt;

  while (rt) {
    next_rt = rt->next;

    if (test_is_constant_relational_test(rt->type)) {
      symbol_remove_ref (rt->data.constant_referent);
    } else if (rt->type==DISJUNCTION_RETE_TEST) {
      deallocate_symbol_list_removing_references (rt->data.disjunction_list);
    }

    free_with_pool (&current_agent(rete_test_pool), rt);
    rt = next_rt;
  }
}
   
/* ------------------------------------------------------------------------
   Given a test and field_num, this routine destructively modifies the
   rete_test *rt.  If *alpha_constant is initially NIL, this routine
   may also set *alpha_constant to point to the constant symbol for the
   alpha net to test.
------------------------------------------------------------------------ */

void add_rete_tests_for_test (test t,
                              byte field_num,
                              rete_node *parent_node,
                              node_varnames *parent_dense_nvn,
                              node_varnames *current_sparse_nvn,
                              rete_test **rt,
                              Symbol **alpha_constant) {
  var_location where;
  cons *c;
  rete_test *new_rt;
  complex_test *ct;
  Symbol *referent;

  if (test_is_blank_test(t)) return;

  if (test_is_blank_or_equality_test(t)) {
    referent = referent_of_equality_test(t);
    
    /* --- if constant test and alpha=NIL, install alpha test --- */
    if ((referent->common.symbol_type!=VARIABLE_SYMBOL_TYPE) &&
        (*alpha_constant==NIL)) {
      *alpha_constant = referent;
      return;
    }
   
    /* --- if constant, make = constant test --- */
    if (referent->common.symbol_type!=VARIABLE_SYMBOL_TYPE) {
      allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
      new_rt->right_field_num = field_num;
      new_rt->type = CONSTANT_RELATIONAL_RETE_TEST+RELATIONAL_EQUAL_RETE_TEST;
      new_rt->data.constant_referent = referent;
      symbol_add_ref (referent);
      new_rt->next = *rt;
      *rt = new_rt;
      return;
    }

    /* --- variable: if binding is for current field, do nothing --- */
    if (! find_var_location (referent, parent_node, parent_dense_nvn,
                             current_sparse_nvn, &where)) {
      print_with_symbols ("Error: Rete build found test of unbound var: %y\n",
                          referent);
      abort_with_fatal_error();
    }
    if ((where.levels_up==0) && (where.field_num==field_num)) return;

    /* --- else make variable equality test --- */
    allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
    new_rt->right_field_num = field_num;
    new_rt->type = VARIABLE_RELATIONAL_RETE_TEST+RELATIONAL_EQUAL_RETE_TEST;
    new_rt->data.variable_referent = where;
    new_rt->next = *rt;
    *rt = new_rt;
    return;
  }

  ct = complex_test_from_test(t);

  switch (ct->type) {    
   
  case NOT_EQUAL_TEST:
  case LESS_TEST:
  case GREATER_TEST:
  case LESS_OR_EQUAL_TEST:
  case GREATER_OR_EQUAL_TEST:
  case SAME_TYPE_TEST:
    /* --- if constant, make constant test --- */
    if (ct->data.referent->common.symbol_type!=VARIABLE_SYMBOL_TYPE) {

      allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
      new_rt->right_field_num = field_num;
      new_rt->type = CONSTANT_RELATIONAL_RETE_TEST +
                     test_type_to_relational_test_type[ct->type];
      new_rt->data.constant_referent = ct->data.referent;
      symbol_add_ref (ct->data.referent);
      new_rt->next = *rt;
      *rt = new_rt;
      return;
    }
    /* --- else make variable test --- */   
    if (! find_var_location (ct->data.referent, parent_node, parent_dense_nvn,
                             current_sparse_nvn, &where)) {
      print_with_symbols ("Error: Rete build found test of unbound var: %y\n",
                          ct->data.referent);
      abort_with_fatal_error();
    }
    allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
    new_rt->right_field_num = field_num;
    new_rt->type = VARIABLE_RELATIONAL_RETE_TEST +
                   test_type_to_relational_test_type[ct->type];
    new_rt->data.variable_referent = where;
    new_rt->next = *rt;
    *rt = new_rt;
    return;

  case DISJUNCTION_TEST:
    allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
    new_rt->right_field_num = field_num;
    new_rt->type = DISJUNCTION_RETE_TEST;
    new_rt->data.disjunction_list =
      copy_symbol_list_adding_references (ct->data.disjunction_list);
    new_rt->next = *rt;
    *rt = new_rt;
    return;
   
  case CONJUNCTIVE_TEST:
    for (c=ct->data.conjunct_list; c!=NIL; c=c->rest) {
      add_rete_tests_for_test (c->first, field_num, parent_node,
                               parent_dense_nvn, current_sparse_nvn,
                               rt, alpha_constant);
    }
    return;

  case GOAL_ID_TEST:
    allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
    new_rt->type = ID_IS_GOAL_RETE_TEST;
    new_rt->right_field_num = 0;
    new_rt->next = *rt;
    *rt = new_rt;
    return;

  case IMPASSE_ID_TEST:
    allocate_with_pool (&current_agent(rete_test_pool), &new_rt);
    new_rt->type = ID_IS_IMPASSE_RETE_TEST;
    new_rt->right_field_num = 0;
    new_rt->next = *rt;
    *rt = new_rt;
    return;
    
  default:
    print ("Error: found bad test type %d while building rete\n",
           ct->type);
    abort_with_fatal_error();
  } /* end of switch statement */
} /* end of function add_rete_tests_for_test() */



/* ------------------------------------------------------------------------
   Check whether two rete tests are identical.
------------------------------------------------------------------------ */

bool rete_tests_are_identical (rete_test *rt1, rete_test *rt2) {
  cons *c1, *c2;
 
  if (rt1->type != rt2->type) return FALSE;

  if (rt1->right_field_num != rt2->right_field_num) return FALSE;

  if (test_is_variable_relational_test(rt1->type))
    return (var_locations_equal (rt1->data.variable_referent,
                                 rt2->data.variable_referent));

  if (test_is_constant_relational_test(rt1->type)) {
    return (rt1->data.constant_referent == rt2->data.constant_referent);
  }

  if (rt1->type==ID_IS_GOAL_RETE_TEST) return TRUE;
  if (rt1->type==ID_IS_IMPASSE_RETE_TEST) return TRUE;

  if (rt1->type == DISJUNCTION_RETE_TEST) {
    c1 = rt1->data.disjunction_list;
    c2 = rt2->data.disjunction_list;
    while ((c1!=NIL)&&(c2!=NIL)) {
      if (c1->first != c2->first) return FALSE;
      c1 = c1->rest;
      c2 = c2->rest;
    }
    if (c1==c2) return TRUE;
    return FALSE;
  }

  print ("Internal error: bad rete test type in rete_tests_are_identical\n");
  abort_with_fatal_error();
  return FALSE; /* unreachable, but without it, gcc -Wall warns here */
}

bool rete_test_lists_are_identical (rete_test *rt1, rete_test *rt2) {
  while (rt1 && rt2) {
    if (! rete_tests_are_identical(rt1,rt2)) return FALSE;
    rt1 = rt1->next;
    rt2 = rt2->next;
  }
  if (rt1==rt2) return TRUE;  /* make sure they both hit end-of-list */
  return FALSE;
}

/* ------------------------------------------------------------------------
   Extracts from a rete test list the variable equality test to use for
   hashing.  Returns TRUE if successful, or FALSE if there was no such
   test to use for hashing.
------------------------------------------------------------------------ */

bool extract_rete_test_to_hash_with (rete_test **rt,
                                     var_location *dest_hash_loc) {
  rete_test *prev, *current;
 
  /* --- look through rt list, find the first variable equality test --- */
  prev = NIL;
  for (current = *rt; current!=NIL; prev=current, current=current->next)
    if (current->type==VARIABLE_RELATIONAL_RETE_TEST +
                       RELATIONAL_EQUAL_RETE_TEST) break;
 
  if (!current) return FALSE;  /* no variable equality test was found */

  /* --- unlink it from rt --- */
  if (prev) prev->next = current->next; else *rt = current->next;

  /* --- extract info, and deallocate that single test --- */
  *dest_hash_loc = current->data.variable_referent;
  current->next = NIL;
  deallocate_rete_test_list (current);
  return TRUE;
}

/* ------------------------------------------------------------------------

    Calls a node's add_left/remove_left routine with each (real) token
    of its parent.

if parent is dummy_top_node:
  call node's add_left/remove_left routine with tok=NIL.

if parent is negative or cn:
  for each blmi on the parent's list, 
    if blmi.match_count == 0
      call node's add_left/remove_left routine

if parent is positive:
  do surgery on parent's child list:  replace list with just "node"
  for each wme in the parent's alpha mem, 
    execute parent's add_right/remove_right routine with that wme
  do surgery to restore previous child list of parent

------------------------------------------------------------------------ */

void call_child_with_each_real_token_of_parent (rete_node *child, bool add) {
  rete_node *parent;
  rete_node *saved_parents_first_child, *saved_childs_next_sibling;
  right_mem *rm;
  neg_token *tok;
  
  parent = child->parent;

  if (parent->node_type==DUMMY_TOP_BNODE) {
    if (add) (*(left_addition_routines[child->node_type]))(child,NIL,NIL);
      else (*(left_removal_routines[child->node_type]))(child,NIL,NIL);
    return;
  }

  if (bnode_is_positive(parent->node_type)) {
#ifdef DO_RIGHT_UNLINKING
    /* --- if parent's left mem is empty, don't bother doing anything --- */
    if (parent->c.num_left_tokens==0) return;
#endif
    saved_parents_first_child = parent->first_child;
    saved_childs_next_sibling = child->next_sibling;
    parent->first_child = child;
    child->next_sibling = NIL;
    if (add) {
      for (rm=parent->d.norm.alpha_mem->right_mems; rm!=NIL; rm=rm->next)
        (*(right_addition_routines[parent->node_type]))(parent,rm->rmi.w);
    } else {
      for (rm=parent->d.norm.alpha_mem->right_mems; rm!=NIL; rm=rm->next)
        (*(right_removal_routines[parent->node_type]))(parent,rm->rmi.w);
    }
    parent->first_child = saved_parents_first_child;
    child->next_sibling = saved_childs_next_sibling;
    return;
  }
    
  /* --- negative or cn nodes --- */
  for (tok=parent->c.neg_tokens; tok!=NIL; tok=tok->next) {
    if (tok->match_count != 0) continue;
    if (add) {
      (*(left_addition_routines[child->node_type])) (child,(token *)tok,NIL);
    } else {
      (*(left_removal_routines[child->node_type])) (child,(token *)tok,NIL);
    }
  }
  return;
}

/* ------------------------------------------------------------------------
   Scans up the net looking for nodes that use a given alpha_mem.  Marks
   them as DONT_RIGHT_UNLINK.
------------------------------------------------------------------------ */

#ifdef DO_RIGHT_UNLINKING

void dont_right_unlink_ancestors_using_am (rete_node *node, alpha_mem *am) {
  if (node->node_type==DUMMY_TOP_BNODE) return;
  if (node->node_type==CN_BNODE) {
    dont_right_unlink_ancestors_using_am (node->d.cn.partner->parent, am);
  } else {
    dont_right_unlink_ancestors_using_am (node->parent, am);
    if (bnode_is_normal(node->node_type) && (node->d.norm.alpha_mem==am)) {
      if (node->right_mem_link_status==RIGHT_UNLINKED)
        relink_to_right_mem (node);
      node->right_mem_link_status = DONT_RIGHT_UNLINK;
    }
  }
}


bool mark_dont_right_unlink (rete_node *node, rete_node *cutoff,
                             alpha_mem *am, bool require_marking) {
  while (node!=cutoff) {
    if (bnode_is_normal(node->node_type)) {
      if (require_marking && (node->d.norm.alpha_mem == am)) {
        dont_right_unlink_ancestors_using_am (node, am);
        return TRUE;
      }
    } else if (node->node_type==CN_BNODE) {
      if (mark_dont_right_unlink (node->d.cn.partner->parent, node->parent,
                                  am, TRUE)) return TRUE;
    }
    node=node->parent;
  }
  return FALSE;
}

#endif

/* ------------------------------------------------------------------------
   Finds or creates a node for the given single condition <cond>, which
   must be a simple (positive or negative, not ncc) condition.  The node
   is made a child of the given <parent> node, using the given bindings.
   Returns a pointer to the node.
------------------------------------------------------------------------ */
                                          
#define get_next_beta_node_id() (current_agent(beta_node_id_counter)++)

rete_node *make_node_for_simple_cond (condition *cond,
                                      rete_node *parent,
                                      node_varnames *parent_dense_nvn,
                                      node_varnames *parent_sparse_nvn,
                                      node_varnames **new_node_dense_nvn,
                                      node_varnames **new_node_sparse_nvn) {
  byte node_type;
  Symbol *alpha_id, *alpha_attr, *alpha_value;
  rete_node *node;
  alpha_mem *am;
  rete_test *rt;
  bool hash_this_node;
  var_location left_hash_loc;
 
  *new_node_dense_nvn =
    make_nvn_for_condition (cond, parent, parent_dense_nvn, TRUE);
  *new_node_sparse_nvn =
    make_nvn_for_condition (cond, parent, parent_sparse_nvn, FALSE);

  alpha_id = alpha_attr = alpha_value = NIL;
  rt = NIL;

  add_rete_tests_for_test (cond->data.tests.id_test, 0, parent,
                           parent_dense_nvn, *new_node_sparse_nvn,
                           &rt, &alpha_id);

  hash_this_node = extract_rete_test_to_hash_with (&rt, &left_hash_loc);

  add_rete_tests_for_test (cond->data.tests.attr_test, 1, parent,
                           parent_dense_nvn, *new_node_sparse_nvn,
                           &rt, &alpha_attr);
  add_rete_tests_for_test (cond->data.tests.value_test, 2, parent,
                           parent_dense_nvn, *new_node_sparse_nvn,
                           &rt, &alpha_value);

  am = find_or_make_alpha_mem (alpha_id, alpha_attr, alpha_value,
                     cond->test_for_acceptable_preference);

  if (cond->type==POSITIVE_CONDITION) {
    if (hash_this_node) node_type = POSITIVE_BNODE;
    else node_type = UNHASHED_POSITIVE_BNODE;
  } else {
    if (hash_this_node) node_type = NEGATIVE_BNODE;
    else node_type = UNHASHED_NEGATIVE_BNODE;
  }

  /* --- look for an existing node for the condition --- */
  for (node=parent->first_child; node!=NIL; node=node->next_sibling)
    if (node->node_type==node_type)
      if (am == node->d.norm.alpha_mem)
        if ((!hash_this_node) ||
            (var_locations_equal (node->d.norm.left_hash_loc,left_hash_loc)))
          if (rete_test_lists_are_identical (node->d.norm.other_tests, rt))
            break;
 
  if (node) {    /* --- A matching node was found --- */
    deallocate_rete_test_list (rt);
    remove_ref_to_alpha_mem (am);
    return node;
  } else {       /* --- No match was found, so create a new node --- */
    allocate_with_pool (&current_agent(rete_node_pool), &node);
    node->node_type = node_type;
    node->parent = parent;
    node->next_sibling = parent->first_child;
    parent->first_child = node;
    node->first_child = NIL;
    if (hash_this_node) node->d.norm.left_hash_loc = left_hash_loc;
    node->d.norm.other_tests = rt;

    node->d.norm.alpha_mem = am;

    if (cond->type==NEGATIVE_CONDITION) node->c.neg_tokens=NIL;
#ifdef DO_RIGHT_UNLINKING
    if (cond->type==POSITIVE_CONDITION) node->c.num_left_tokens = 0;
    mark_dont_right_unlink (parent, &current_agent(dummy_top_node), am, FALSE);
#endif
    relink_to_right_mem (node);

    node->node_id = get_next_beta_node_id();

    /* --- call new node's add_left routine with all the parent's tokens --- */
    call_child_with_each_real_token_of_parent (node, TRUE);
    
#ifdef DO_RIGHT_UNLINKING
    /* --- if no tokens arrived from parent, unlink the node --- */
    if (((cond->type==POSITIVE_CONDITION) && (node->c.num_left_tokens==0)) ||
        ((cond->type==NEGATIVE_CONDITION) && (node->c.neg_tokens==NIL)))
      unlink_from_right_mem (node);
#endif
    
    return node;
  }
}
   
/* ------------------------------------------------------------------------
   Finds or creates a node for the given single condition <cond>, which
   must be a conjunctive negation.  (If necessary, the subcondition part
   of the rete is also created.)  The node is made a child of the given
   <parent> node, using the given bindings.  Returns a pointer to the node.
------------------------------------------------------------------------ */

typedef void (install_conditions_callback_fn (rete_node *node,
                                              node_varnames *dense_nvn,
                                              node_varnames *sparse_nvn));

void install_conditions (condition *cond_list,
                         rete_node *parent,
                         node_varnames *parent_dense_nvn,
                         node_varnames *parent_sparse_nvn,
                         install_conditions_callback_fn *callback_fn);


void ncc_subconditions_callback_fn (rete_node *node,
                                    node_varnames *dense_nvn,
                                    node_varnames *sparse_nvn) {
  current_agent(ncc_subconditions_bottom_node) = node;
  current_agent(ncc_subconditions_bottom_dense_nvn) = dense_nvn;
  current_agent(ncc_subconditions_bottom_sparse_nvn) = sparse_nvn;
}

rete_node *make_node_for_ncc (condition *cond,
                              rete_node *parent,
                              node_varnames *parent_dense_nvn,
                              node_varnames *parent_sparse_nvn,
                              node_varnames **new_node_dense_nvn,
                              node_varnames **new_node_sparse_nvn) {
  rete_node *node, *partner;
  rete_node *ncc_subconditions_top_node;
  node_varnames *dense_nvn, *sparse_nvn;
 
  /* --- first, make the subconditions part of the rete --- */
  install_conditions (cond->data.ncc.top, parent, parent_dense_nvn,
                      parent_sparse_nvn, ncc_subconditions_callback_fn);

  allocate_with_pool (&current_agent(node_varnames_pool), &dense_nvn);
  allocate_with_pool (&current_agent(node_varnames_pool), &sparse_nvn);
  dense_nvn->parent = parent_dense_nvn;
  sparse_nvn->parent = parent_sparse_nvn;
  dense_nvn->data.bottom_of_subconditions =
    current_agent(ncc_subconditions_bottom_dense_nvn);
  sparse_nvn->data.bottom_of_subconditions =
    current_agent(ncc_subconditions_bottom_sparse_nvn);
  *new_node_dense_nvn = dense_nvn;
  *new_node_sparse_nvn = sparse_nvn;

  /* --- look for an existing node for the condition --- */
  for (node=parent->first_child; node!=NIL; node=node->next_sibling)
    if (node->node_type==CN_BNODE)
      if (node->d.cn.partner->parent==current_agent(ncc_subconditions_bottom_node))
        break;

  if (node) {
    /* --- A matching node was found --- */
    return node;
  }

  /* --- No match was found, so create a new node --- */
  /* --- Find top node in the subconditions branch --- */
  for (node=current_agent(ncc_subconditions_bottom_node); node!=parent; node=node->parent) {
    ncc_subconditions_top_node = node;
  }

  allocate_with_pool (&current_agent(rete_node_pool), &node);
  allocate_with_pool (&current_agent(rete_node_pool), &partner);
   
  node->node_type = CN_BNODE;

  /* WARNING: for correctness, <node> must be on the parent's children list
     *after* the ncc subcontitions top node */
  remove_node_from_parents_list_of_children (ncc_subconditions_top_node);
  node->parent = parent;
  node->next_sibling = parent->first_child;
  ncc_subconditions_top_node->next_sibling = node;
  parent->first_child = ncc_subconditions_top_node;
  node->first_child = NIL;

  node->c.neg_tokens = NIL;
  node->d.cn.partner = partner;
  node->node_id = get_next_beta_node_id();
  
  partner->node_type = CN_PARTNER_BNODE;
  partner->parent = current_agent(ncc_subconditions_bottom_node);
  partner->next_sibling = current_agent(ncc_subconditions_bottom_node)->first_child;
  current_agent(ncc_subconditions_bottom_node)->first_child = partner;
  partner->first_child = NIL;
  partner->c.neg_tokens = NIL;
  partner->d.cn.partner = node;
  
  /* --- call partner's add_left routine with all the parent's tokens --- */
  call_child_with_each_real_token_of_parent (partner, TRUE);
  /* --- call new node's add_left routine with all the parent's tokens --- */
  call_child_with_each_real_token_of_parent (node, TRUE);

  return node;
}
 
/* ------------------------------------------------------------------------
    Installs a list of conditions, given the parent node and the current
    variable bindings.  After the last condition is installed, the given
    callback function is called with the final node and node_varnames.
------------------------------------------------------------------------ */

void install_conditions (condition *cond_list,
                         rete_node *parent,
                         node_varnames *parent_dense_nvn,
                         node_varnames *parent_sparse_nvn,
                         install_conditions_callback_fn *callback_fn) {
  rete_node *node, *new_node;
  node_varnames *dense_nvn, *new_dense_nvn;
  node_varnames *sparse_nvn, *new_sparse_nvn;

  node = parent;
  dense_nvn = parent_dense_nvn;
  sparse_nvn = parent_sparse_nvn;

  while (cond_list != NIL) { 
    if (cond_list->type==CONJUNCTIVE_NEGATION_CONDITION) {
      new_node = make_node_for_ncc
        (cond_list, node, dense_nvn, sparse_nvn, &new_dense_nvn,
         &new_sparse_nvn);
    } else {
      new_node = make_node_for_simple_cond
        (cond_list, node, dense_nvn, sparse_nvn, &new_dense_nvn,
         &new_sparse_nvn);
    }
    node = new_node;
    dense_nvn = new_dense_nvn;
    sparse_nvn = new_sparse_nvn;
    cond_list = cond_list->next;
  }

  /* --- recursion has bottomed out; invoke the callback function --- */
  (*callback_fn)(node,dense_nvn,sparse_nvn);
}
 
/* ------------------------------------------------------------------------
                   Production Addition and Excising
------------------------------------------------------------------------ */

bool unify_symbols (Symbol *old, Symbol *new, rete_node *p_node,
                    node_varnames *new_prod_parents_sparse_nvn) {
  node_varnames current_nvn;
  var_location old_location, new_location;
  bool found_old, found_new;

  if (old->common.symbol_type != new->common.symbol_type) return FALSE;
  
  if (old->common.symbol_type != VARIABLE_SYMBOL_TYPE)
    return (old == new);
  
  /* --- initialize dummy node_varnames for this node (the p_node) --- */
  current_nvn.parent = new_prod_parents_sparse_nvn;
  current_nvn.data.fields.id_varnames = NIL;
  current_nvn.data.fields.attr_varnames = NIL;
  current_nvn.data.fields.value_varnames = NIL;

  /* --- find locations on LHS where they're bound --- */
  found_old = find_var_location (old, p_node->parent,
                                 p_node->d.p.parents_sparse_nvn,
                                 &current_nvn,  &old_location);
  found_new = find_var_location (new, p_node->parent,
                                 new_prod_parents_sparse_nvn,
                                 &current_nvn, &new_location);

  /* --- if one is bound on LHS but the other isn't, return FALSE --- */
  if (found_old && (!found_new)) return FALSE;
  if (found_new && (!found_old)) return FALSE;

  /* --- if both are bound, see if they're bound in the same place --- */
  if (found_old && found_new) {
    if ( (old_location.levels_up == new_location.levels_up) &&
         (old_location.field_num == new_location.field_num) ) return TRUE;
    return FALSE;
  }
  
  /* --- if neither is bound, try to unify them --- */    
  if (new->var.current_binding_value == old) return TRUE;
  if (new->var.current_binding_value == NIL) {
    new->var.current_binding_value = old;
    return TRUE;
  }
  return FALSE; /* new is already unified with some other variable */
}

bool p_node_matches_production (rete_node *p_node,
                                node_varnames *new_prod_parents_sparse_nvn,
                                action *new_prod_rhs_actions) {
  action *old_a, *new_a, *a;
  
  /* --- first, make sure they have the same number of RHS actions --- */
  /* --- while we're at it, also make sure there's no function calls, and
         make sure the preference types are the same --- */
  old_a = p_node->d.p.prod->action_list;
  new_a = new_prod_rhs_actions;
  while (old_a && new_a) {
    if (old_a->type == FUNCALL_ACTION) return FALSE;
    if (new_a->type == FUNCALL_ACTION) return FALSE;
    if (old_a->preference_type != new_a->preference_type) return FALSE;
    if (rhs_value_is_funcall(old_a->value)) return FALSE;
    if (rhs_value_is_funcall(new_a->value)) return FALSE;
    if (preference_is_binary(old_a->preference_type)) {
      if (rhs_value_is_funcall(old_a->referent)) return FALSE;
      if (rhs_value_is_funcall(new_a->referent)) return FALSE;
    }
    old_a = old_a->next;
    new_a = new_a->next;
  }
  if (old_a != new_a) return FALSE;  /* make sure they both hit NIL */

  /* --- From here on, we try to unify the old RHS with the new one.
         We start by binding the var's in the new one to NIL.  When
         we unify a new var with an old one, we set the new one's
         current_binding_value to point to the old one. --- */
  /* --- bind variables in new actions to NIL --- */
  for (a=new_prod_rhs_actions; a!=NIL; a=a->next) {
    if (a->id->common.symbol_type==VARIABLE_SYMBOL_TYPE)
      a->id->var.current_binding_value = NIL;
    if (a->attr->common.symbol_type==VARIABLE_SYMBOL_TYPE)
      a->attr->var.current_binding_value = NIL;
    if (rhs_value_to_symbol(a->value)->common.symbol_type ==
        VARIABLE_SYMBOL_TYPE)
      rhs_value_to_symbol(a->value)->var.current_binding_value = NIL;
    if (preference_is_binary(a->preference_type))
      if (rhs_value_to_symbol(a->referent)->common.symbol_type ==
          VARIABLE_SYMBOL_TYPE)
        rhs_value_to_symbol(a->referent)->var.current_binding_value = NIL;
  }

  /* --- scan through the action lists, and try to unify them --- */
  old_a = p_node->d.p.prod->action_list;
  new_a = new_prod_rhs_actions;
  while (old_a) {
    if (! unify_symbols (old_a->id,
                         new_a->id,
                         p_node,
                         new_prod_parents_sparse_nvn)) return FALSE;
    if (! unify_symbols (old_a->attr,
                         new_a->attr,
                         p_node,
                         new_prod_parents_sparse_nvn)) return FALSE;
    if (! unify_symbols (rhs_value_to_symbol(old_a->value),
                         rhs_value_to_symbol(new_a->value),
                         p_node,
                         new_prod_parents_sparse_nvn)) return FALSE;
    if (preference_is_binary(old_a->preference_type))
      if (! unify_symbols (rhs_value_to_symbol(old_a->referent),
                           rhs_value_to_symbol(new_a->referent),
                           p_node,
                           new_prod_parents_sparse_nvn)) return FALSE;
    old_a = old_a->next;
    new_a = new_a->next;
  }

  /* --- if we got this far, the unification must have succeeded --- */
  return TRUE;
}

void production_addition_callback_fn (rete_node *node,
                                      node_varnames *dense_nvn,
                                      node_varnames *sparse_nvn) {
  rete_node *p_node;
  ms_change *msc;

  for (p_node=node->first_child; p_node!=NIL; p_node=p_node->next_sibling) {
    if (p_node->node_type != P_BNODE) continue;
    if (! p_node_matches_production (p_node,
                                     sparse_nvn,
                                     current_agent(production_being_added_now)->action_list))
      continue;
    /* --- duplicate production found --- */
    if (current_agent(warn_on_duplicate_production))
      print_with_symbols ("\nIgnoring %y because it is a duplicate of %y ",
                          current_agent(production_being_added_now)->name,
                          p_node->d.p.prod->name);
    current_agent(production_addition_result) = DUPLICATE_PRODUCTION;
    deallocate_node_varnames (node, &current_agent(dummy_top_node), dense_nvn);
    deallocate_node_varnames (node, &current_agent(dummy_top_node), sparse_nvn);
    return;
  }

  allocate_with_pool (&current_agent(rete_node_pool), &p_node);
  current_agent(production_being_added_now)->p_node = p_node;
  p_node->node_type = P_BNODE;
  p_node->parent = node;
  p_node->next_sibling = node->first_child;
  node->first_child = p_node;
  p_node->first_child = NIL;
  p_node->d.p.prod = current_agent(production_being_added_now);
  p_node->d.p.parents_sparse_nvn = sparse_nvn;
  deallocate_node_varnames (node, &current_agent(dummy_top_node), dense_nvn);
  p_node->d.p.tentative_assertions = NIL;
  p_node->d.p.tentative_retractions = NIL;
  /* --- handle initial refraction by adding it to tentative_retractions --- */
  if (current_agent(refracted_inst_for_production_being_added_now)) {
    insert_at_head_of_dll (current_agent(production_being_added_now)->instantiations,
                           current_agent(refracted_inst_for_production_being_added_now),
                           next, prev);
    current_agent(refracted_inst_for_production_being_added_now)->rete_token = NIL;
    current_agent(refracted_inst_for_production_being_added_now)->rete_wme = NIL;
    allocate_with_pool (&current_agent(ms_change_pool), &msc);
    msc->inst = current_agent(refracted_inst_for_production_being_added_now);
    msc->p_node = p_node;
    insert_at_head_of_dll (current_agent(ms_retractions), msc, next, prev);
    insert_at_head_of_dll (p_node->d.p.tentative_retractions, msc,
                           next_of_node, prev_of_node);
  }

  /* --- call new node's add_left routine with all the parent's tokens --- */
  call_child_with_each_real_token_of_parent (p_node, TRUE);

  /* --- store result indicator --- */
  if (! current_agent(refracted_inst_for_production_being_added_now)) {
    current_agent(production_addition_result) = NO_REFRACTED_INST;
  } else {
    remove_from_dll (current_agent(production_being_added_now)->instantiations,
                     current_agent(refracted_inst_for_production_being_added_now),
                     next, prev);
    if (p_node->d.p.tentative_retractions) {
      current_agent(production_addition_result) = REFRACTED_INST_DID_NOT_MATCH;
      msc = p_node->d.p.tentative_retractions;
      p_node->d.p.tentative_retractions = NIL;
      remove_from_dll (current_agent(ms_retractions), msc, next, prev);
      free_with_pool (&current_agent(ms_change_pool), msc);
    } else {
      current_agent(production_addition_result) = REFRACTED_INST_MATCHED;
    }
  }
}


byte add_production_to_rete (production *p,
                             condition *lhs_top,
                             instantiation *refracted_inst,
                             bool warn_on_duplicates) {
  current_agent(production_being_added_now) = p;
  current_agent(refracted_inst_for_production_being_added_now) = refracted_inst;
  current_agent(warn_on_duplicate_production) = warn_on_duplicates;
  install_conditions (lhs_top, &current_agent(dummy_top_node), NIL, NIL,
                      production_addition_callback_fn);
  if (current_agent(production_addition_result)!=DUPLICATE_PRODUCTION)
    production_just_added_hook (p);
  return current_agent(production_addition_result);
}

void deallocate_rete_node (rete_node *node) {
  rete_node *parent;
 
  if (node==&current_agent(dummy_top_node)) return; /* never deallocate the dummy top node */
  
  parent = node->parent;
  
  /* --- clean up tokens by calling this node's left_removal routine on all
     token of the parent --- */
  call_child_with_each_real_token_of_parent (node, FALSE);

  /* --- deallocate any rete test structures, etc. --- */
  if (bnode_is_normal(node->node_type)) {
    deallocate_rete_test_list (node->d.norm.other_tests);
    if (node->right_mem_link_status != RIGHT_UNLINKED)
      unlink_from_right_mem (node);
    remove_ref_to_alpha_mem (node->d.norm.alpha_mem);
  }

  /* --- if a cn node, deallocate its partner too --- */
  if (node->node_type==CN_BNODE)
    deallocate_rete_node (node->d.cn.partner);

  /* --- finally, unlink the node and possibly deallocate its parent --- */
  remove_node_from_parents_list_of_children (node);
  free_with_pool (&current_agent(rete_node_pool), node);
  if (! parent->first_child) deallocate_rete_node (parent);
}

void excise_production_from_rete (production *p) {
  rete_node *p_node, *parent;
  ms_change *msc;

  production_about_to_be_excised_hook (p);
  
  p_node = p->p_node;
  p->p_node = NIL;      /* mark production as not being in the rete anymore */

  parent = p_node->parent;
  deallocate_node_varnames (parent, &current_agent(dummy_top_node),
                            p_node->d.p.parents_sparse_nvn);

  /* --- cause all existing instantiations to retract, by calling the p_node's
     left_removal routine with all tokens of its parent --- */
  call_child_with_each_real_token_of_parent (p_node, FALSE);

  /* --- At this point, there are no tentative_assertion's.  Now set
     the p_node field of all tentative_retractions to NIL, to indicate
     that the p_node is being excised  --- */
  for (msc=p_node->d.p.tentative_retractions; msc!=NIL; msc=msc->next_of_node)
    msc->p_node = NIL;

  /* --- finally, excise the p_node --- */
  remove_node_from_parents_list_of_children (p_node);
  free_with_pool (&current_agent(rete_node_pool), p_node);

  /* --- and propogate up the net --- */
  if (! parent->first_child) deallocate_rete_node (parent);
}

/* ========================================================================

        Building Conditions (instantiated or not) from the Rete Net

======================================================================== */

void bind_vars_in_varnames (varnames *vn, Symbol *binding) {
  cons *c;

  if (vn==NIL) return; 
  if (varnames_is_one_var(vn)) {
    (varnames_to_one_var(vn))->var.current_binding_value = binding;
  } else {
    for (c=varnames_to_var_list(vn); c!=NIL; c=c->rest)
      ((Symbol *)(c->first))->var.current_binding_value = binding;
  }
}

void bind_vars_in_node_varnames_list (rete_node *node,
                                      rete_node *cutoff,
                                      node_varnames *nvn,
                                      token *tok,
                                      wme *w) {
  while (node!=cutoff) {
    if (w && bnode_is_positive(node->node_type)) {
      if (nvn->data.fields.id_varnames)
        bind_vars_in_varnames (nvn->data.fields.id_varnames,
                               w->id);
      if (nvn->data.fields.attr_varnames)
        bind_vars_in_varnames (nvn->data.fields.attr_varnames,
                               w->attr);
      if (nvn->data.fields.value_varnames)
        bind_vars_in_varnames (nvn->data.fields.value_varnames,
                               w->value);
    } else if (bnode_is_normal(node->node_type)) {
      bind_vars_in_varnames (nvn->data.fields.id_varnames, NIL);
      bind_vars_in_varnames (nvn->data.fields.attr_varnames, NIL);
      bind_vars_in_varnames (nvn->data.fields.value_varnames, NIL);
    } else if (node->node_type == CN_BNODE) {
      bind_vars_in_node_varnames_list (node->d.cn.partner->parent,
                                       node->parent,
                                       nvn->data.bottom_of_subconditions,
                                       NIL, NIL);
    } else {
      print ("Error: bad node type in bind_vars_in_node_varnames_list\n");
      abort_with_fatal_error();
    }
    node = node->parent;
    nvn = nvn->parent;
    if (tok) { w=tok->w; tok=tok->parent; }
  }
}

Symbol *instantiated_var_from_location (rete_node *node,
                                        node_varnames *nvn,
                                        var_location *where);

Symbol *follow_equality_test_up_the_net (rete_node *node,
                                         node_varnames *nvn,
                                         byte field_num) {
  rete_test *rt;
 
  if (bnode_is_hashed(node->node_type) && (field_num==0))
    return instantiated_var_from_location (node,
                                           nvn,
                                           &(node->d.norm.left_hash_loc));
 
  for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next) {
    if ((rt->type==VARIABLE_RELATIONAL_RETE_TEST+RELATIONAL_EQUAL_RETE_TEST) &&
        (rt->right_field_num==field_num)) {
      return instantiated_var_from_location (node,
                                             nvn,
                                             &(rt->data.variable_referent));
    }
  }

  print ("Internal error: can't find variable equality rete test\n");
  print ("in follow_equality_test_up_the_net\n");
  abort_with_fatal_error();
  return NIL; /* unreachable, but without it, gcc -Wall warns here */
}

Symbol *instantiated_var_from_location (rete_node *node,
                                        node_varnames *nvn,
                                        var_location *where) {
  int i;
  varnames *names;
  Symbol *var;
 
  for (i=0; i < where->levels_up; i++) {
    node = node->parent;
    nvn=nvn->parent;
  }
 
  if (where->field_num==0) names=nvn->data.fields.id_varnames;
  else if (where->field_num==1) names=nvn->data.fields.attr_varnames;
  else names=nvn->data.fields.value_varnames;

  if (!names) {
    return follow_equality_test_up_the_net (node, nvn, where->field_num);
  }

  if (varnames_is_one_var(names))
    var = varnames_to_one_var(names);
  else
    var = (varnames_to_var_list(names))->first;

  if (var->var.current_binding_value) var=var->var.current_binding_value;
  return var;
}


void add_rete_test_list_to_tests (rete_node *node,
                                  node_varnames *nvn,
                                  rete_test *rt,
                                  test *id_t, test *attr_t, test *value_t) {
  Symbol *referent;
  test new;
  complex_test *new_ct;
  test *add_to_me;
  byte test_type;
  
  for ( ; rt!=NIL; rt=rt->next) {
   
    if (rt->right_field_num==0) add_to_me = id_t;
    else if (rt->right_field_num==2) add_to_me = value_t;
    else add_to_me = attr_t;

    if (rt->type==ID_IS_GOAL_RETE_TEST) {
      allocate_with_pool (&current_agent(complex_test_pool), &new_ct);
      new = make_test_from_complex_test(new_ct);
      new_ct->type = GOAL_ID_TEST;
    } else if (rt->type==ID_IS_IMPASSE_RETE_TEST) {
      allocate_with_pool (&current_agent(complex_test_pool), &new_ct);
      new = make_test_from_complex_test(new_ct);
      new_ct->type = IMPASSE_ID_TEST;
    } else if (rt->type==DISJUNCTION_RETE_TEST) {
      allocate_with_pool (&current_agent(complex_test_pool), &new_ct);
      new = make_test_from_complex_test(new_ct);
      new_ct->type = DISJUNCTION_TEST;
      new_ct->data.disjunction_list =
        copy_symbol_list_adding_references (rt->data.disjunction_list);
    } else if (test_is_constant_relational_test(rt->type)) {
      test_type =
        relational_test_type_to_test_type[kind_of_relational_test(rt->type)];
      referent = rt->data.constant_referent;
      symbol_add_ref (referent);
      if (test_type==EQUAL_TEST_TYPE) {
        new = make_equality_test_without_adding_reference (referent);
      } else {
        allocate_with_pool (&current_agent(complex_test_pool), &new_ct);
        new = make_test_from_complex_test(new_ct);
        new_ct->type = test_type;
        new_ct->data.referent = referent;
      }
    } else if (test_is_variable_relational_test(rt->type)) {
      test_type =
        relational_test_type_to_test_type[kind_of_relational_test(rt->type)];
      referent = instantiated_var_from_location
        (node, nvn, &(rt->data.variable_referent));
      symbol_add_ref (referent);
      if (test_type==EQUAL_TEST_TYPE) {
        new = make_equality_test_without_adding_reference (referent);
      } else {
        allocate_with_pool (&current_agent(complex_test_pool), &new_ct);
        new = make_test_from_complex_test(new_ct);
        new_ct->type = test_type;
        new_ct->data.referent = referent;
      }
    } else {
      print ("Error: bad test_type in add_rete_test_to_test\n");
      abort_with_fatal_error();
    }
    add_new_test_to_test (add_to_me, new);
  }
}


void collect_nots (rete_test *rt,
                   wme *right_wme,
                   rete_node *node,
                   node_varnames *nvn) {
  not *new_not;
  Symbol *right_sym;
  Symbol *referent;

  for ( ; rt!=NIL; rt=rt->next) {

    if (! test_is_not_equal_test(rt->type)) continue;

    right_sym = field_from_wme (right_wme, rt->right_field_num);

    if (right_sym->common.symbol_type != IDENTIFIER_SYMBOL_TYPE) continue;
   
    if (rt->type == CONSTANT_RELATIONAL_RETE_TEST +
                    RELATIONAL_NOT_EQUAL_RETE_TEST) {
      referent = rt->data.constant_referent;
      if (referent->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) continue;
      allocate_with_pool (&current_agent(not_pool), &new_not);
      new_not->next = current_agent(collected_nots);
      current_agent(collected_nots) = new_not;
      new_not->s1 = right_sym;
      symbol_add_ref (right_sym);
      new_not->s2 = referent;
      symbol_add_ref (referent);
      continue;
    }
   
    if (rt->type == VARIABLE_RELATIONAL_RETE_TEST +
                    RELATIONAL_NOT_EQUAL_RETE_TEST) {
      referent = instantiated_var_from_location
        (node, nvn, &(rt->data.variable_referent));
      if (referent->common.symbol_type!=IDENTIFIER_SYMBOL_TYPE) continue;
      allocate_with_pool (&current_agent(not_pool), &new_not);
      new_not->next = current_agent(collected_nots);
      current_agent(collected_nots) = new_not;
      new_not->s1 = right_sym;
      symbol_add_ref (right_sym);
      new_not->s2 = referent;
      symbol_add_ref (referent);
      continue;
    }
  }
}

void add_varnames_to_test (varnames *vn, test *t) {
  test new;
  cons *c;
 
  if (vn == NIL) return;
  if (varnames_is_one_var(vn)) {
    new = make_equality_test (varnames_to_one_var(vn));
    add_new_test_to_test (t, new);
  } else {
    for (c=varnames_to_var_list(vn); c!=NIL; c=c->rest) {
      new = make_equality_test ((Symbol *)(c->first));
      add_new_test_to_test (t, new);
    }
  }
}

void add_hash_info_to_tests (rete_node *node,
                             node_varnames *nvn,
                             var_location *left_hash_loc,
                             test *id_t) {
  test new;
 
  new = make_equality_test
    (instantiated_var_from_location (node, nvn, left_hash_loc));
  add_new_test_to_test (id_t, new);
}


void rete_node_to_conditions (rete_node *node,
                              node_varnames *nvn,
                              rete_node *cutoff,
                              token *tok,
                              wme *w,
                              condition **dest_top_cond,
                              condition **dest_bottom_cond) {
  condition *cond, *lower_cond, **place_to_store_pointer_to_next_cond;
  alpha_mem *am;

  lower_cond = NIL;
  place_to_store_pointer_to_next_cond = dest_bottom_cond;
 
  while (node!=cutoff) {
    allocate_with_pool (&current_agent(condition_pool), &cond);
    cond->next = lower_cond;
    lower_cond = cond;
    *place_to_store_pointer_to_next_cond = cond;
    place_to_store_pointer_to_next_cond = &(cond->prev);
    
    if (node->node_type==CN_BNODE) {
      cond->type = CONJUNCTIVE_NEGATION_CONDITION;
      rete_node_to_conditions (node->d.cn.partner->parent,
                               nvn->data.bottom_of_subconditions,
                               node->parent,
                               NIL,
                               NIL,
                               &(cond->data.ncc.top),
                               &(cond->data.ncc.bottom));
    } else {
      if (bnode_is_positive(node->node_type))
        cond->type = POSITIVE_CONDITION;
      else
        cond->type = NEGATIVE_CONDITION;
      
      if (w && (cond->type==POSITIVE_CONDITION)) {
        /* --- make simple tests and collect nots --- */
        cond->data.tests.id_test = make_equality_test (w->id);
        cond->data.tests.attr_test = make_equality_test (w->attr);
        cond->data.tests.value_test = make_equality_test (w->value);
        cond->test_for_acceptable_preference = w->acceptable;
        cond->bt.wme = w;
        if (node->d.norm.other_tests) /* don't bother if there are no tests */
          collect_nots (node->d.norm.other_tests, w, node, nvn);
      } else {
        am = node->d.norm.alpha_mem;
        cond->data.tests.id_test = make_blank_or_equality_test (am->id);
        cond->data.tests.attr_test = make_blank_or_equality_test (am->attr);
        cond->data.tests.value_test = make_blank_or_equality_test (am->value);
        
        if (bnode_is_hashed(node->node_type))
          add_hash_info_to_tests (node, nvn, &(node->d.norm.left_hash_loc),
                                  &(cond->data.tests.id_test));
        
        add_varnames_to_test (nvn->data.fields.id_varnames,
                              &(cond->data.tests.id_test));
        add_varnames_to_test (nvn->data.fields.attr_varnames,
                              &(cond->data.tests.attr_test));
        add_varnames_to_test (nvn->data.fields.value_varnames,
                              &(cond->data.tests.value_test));

        if (node->d.norm.other_tests) /* don't bother if there are no tests */
          add_rete_test_list_to_tests (node, nvn, node->d.norm.other_tests,
                                       &(cond->data.tests.id_test),
                                       &(cond->data.tests.attr_test),
                                       &(cond->data.tests.value_test));
        
        cond->test_for_acceptable_preference = am->acceptable;
      }
    }
    node = node->parent;
    nvn = nvn->parent;
    if (tok) { w=tok->w; tok=tok->parent; }
  } /* end of while (node!=cutoff) */

  *place_to_store_pointer_to_next_cond = NIL;
  *dest_top_cond = lower_cond;
}

/* ---------------------------------
    Note: if tok!=NIL, this routine has the (important) side effect of
    binding all the top-level positively tested variables on the LHS.
    If tok!=NIL, it also returns (in dest_nots) the top-level positive
    "<>" tests.  If tok==NIL, dest_nots is not used.
--------------------------------- */
void p_node_to_conditions_and_nots (rete_node *p_node,
                                    token *tok,
                                    wme *w,
                                    condition **dest_top_cond,
                                    condition **dest_bottom_cond,
                                    not **dest_nots) {
  current_agent(collected_nots) = NIL;
  if (tok==NIL) w=NIL;  /* just for safety */
  bind_vars_in_node_varnames_list (p_node->parent, &current_agent(dummy_top_node),
                                   p_node->d.p.parents_sparse_nvn, tok, w);
  rete_node_to_conditions (p_node->parent,
                           p_node->d.p.parents_sparse_nvn,
                           &current_agent(dummy_top_node),
                           tok,
                           w,
                           dest_top_cond,
                           dest_bottom_cond);
  if (tok) *dest_nots = current_agent(collected_nots);
}

/* ------------------------------------------------------------------------

                    Rete Test Evaluation Routines

------------------------------------------------------------------------ */

bool ( (*(rete_test_routines[256]))
       (rete_test *rt, token *left, wme *w));

#define match_left_and_right(rete_test,left,w) \
  ( (*(rete_test_routines[(rete_test)->type])) \
    ((rete_test),(left),(w)) )

#define numeric_comparison_between_symbols(s1,s2,comparator_op) ( \
  ( ((s1)->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) && \
    ((s2)->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) ) ? \
    (((s1)->ic.value) comparator_op ((s2)->ic.value)) : \
  ( ((s1)->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) && \
    ((s2)->common.symbol_type==FLOAT_CONSTANT_SYMBOL_TYPE) ) ? \
    (((s1)->ic.value) comparator_op ((s2)->fc.value)) : \
  ( ((s1)->common.symbol_type==FLOAT_CONSTANT_SYMBOL_TYPE) && \
    ((s2)->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) ) ? \
    (((s1)->fc.value) comparator_op ((s2)->ic.value)) : \
  ( ((s1)->common.symbol_type==FLOAT_CONSTANT_SYMBOL_TYPE) && \
    ((s2)->common.symbol_type==FLOAT_CONSTANT_SYMBOL_TYPE) ) ? \
    (((s1)->fc.value) comparator_op ((s2)->fc.value)) : \
  FALSE )

/* Note:  "=" and "<>" tests always return FALSE when one argument is
   an integer and the other is a floating point number */

bool error_rete_test_routine (rete_test *rt, token *left, wme *w) {
  print ("Internal error: bad rete test type, hit error_rete_test_routine\n");
  abort_with_fatal_error();
  return FALSE; /* unreachable, but without it, gcc -Wall warns here */
}

bool id_is_goal_rete_test_routine (rete_test *rt, token *left, wme *w) {
  return w->id->id.isa_goal;
}

bool id_is_impasse_rete_test_routine (rete_test *rt, token *left, wme *w) {
  return w->id->id.isa_impasse;
}

bool disjunction_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *sym;
  cons *c;

  sym = field_from_wme (w,rt->right_field_num);
  for (c=rt->data.disjunction_list; c!=NIL; c=c->rest)
    if (c->first==sym) return TRUE;
  return FALSE;
}

bool constant_equal_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return (s1 == s2);
}

bool constant_not_equal_rete_test_routine (rete_test *rt, token *left,
                                           wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return (s1 != s2);
}

bool constant_less_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return numeric_comparison_between_symbols (s1, s2, < );
}

bool constant_greater_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return numeric_comparison_between_symbols (s1, s2, > );
}

bool constant_less_or_equal_rete_test_routine (rete_test *rt, token *left,
                                               wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return numeric_comparison_between_symbols (s1, s2, <= );
}

bool constant_greater_or_equal_rete_test_routine (rete_test *rt, token *left,
                                                  wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return numeric_comparison_between_symbols (s1, s2, >= );
}

bool constant_same_type_rete_test_routine (rete_test *rt, token *left,
                                           wme *w) {
  Symbol *s1, *s2;
 
  s1 = field_from_wme (w,rt->right_field_num);
  s2 = rt->data.constant_referent;
  return (s1->common.symbol_type == s2->common.symbol_type);
}

bool variable_equal_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);

  return (s1 == s2);
}

bool variable_not_equal_rete_test_routine (rete_test *rt, token *left,
                                           wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);

  return (s1 != s2);
}

bool variable_less_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);

  return numeric_comparison_between_symbols (s1, s2, < );
}

bool variable_greater_rete_test_routine (rete_test *rt, token *left, wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);

  return numeric_comparison_between_symbols (s1, s2, > );
}

bool variable_less_or_equal_rete_test_routine (rete_test *rt, token *left,
                                               wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);

  return numeric_comparison_between_symbols (s1, s2, <= );
}

bool variable_greater_or_equal_rete_test_routine (rete_test *rt, token *left,
                                                  wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);

  return numeric_comparison_between_symbols (s1, s2, >= );
}

bool variable_same_type_rete_test_routine (rete_test *rt, token *left,
                                           wme *w) {
  Symbol *s1, *s2;
  int i;
 
  s1 = field_from_wme (w, rt->right_field_num);

  if (rt->data.variable_referent.levels_up!=0) {
    i = rt->data.variable_referent.levels_up - 1;
    while (i!=0) {
      left = left->parent;
      i--;
    }
    w = left->w;
  }
  s2 = field_from_wme (w, rt->data.variable_referent.field_num);
  return (s1->common.symbol_type == s2->common.symbol_type);
}

/* ------------------------------------------------------------------------
                   Bete Node Interpreter Routines
------------------------------------------------------------------------ */

void rete_error_left (rete_node *node, token *t, wme *w) {
  print ("Rete net error:  tried to add/remove left token from\n");
  print ("node of type %d\n", node->node_type);
  abort_with_fatal_error();
}

void rete_error_right (rete_node *node, wme *w) {
  print ("Rete net error:  tried to add/remove right token from\n");
  print ("node of type %d\n", node->node_type);
  abort_with_fatal_error();
}

void positive_node_left_addition (rete_node *node, token *tok, wme *w) {
  unsigned long hv, right_hv;
  Symbol *referent;
  right_mem_item *rmi;
  alpha_mem *am;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  token *new;

#ifdef DO_RIGHT_UNLINKING
  if (node->right_mem_link_status==RIGHT_UNLINKED)
    relink_to_right_mem (node);
  node->c.num_left_tokens++;
#endif

  {
    int levels_up;
    token *t;
    
    levels_up = node->d.norm.left_hash_loc.levels_up;
    if (levels_up==1) {
      referent = field_from_wme (w, node->d.norm.left_hash_loc.field_num);
    } else { /* --- levels_up > 1 --- */
      for (t=tok, levels_up -= 2; levels_up!=0; levels_up--) t=t->parent;
      referent = field_from_wme (t->w, node->d.norm.left_hash_loc.field_num);
    }
  }
  
  hv = node->node_id ^ referent->common.hash_id;

  /* --- build new left_token, add it to the hash table --- */
  token_added();
  allocate_with_pool (&current_agent(token_pool), &new);
  new->node = node;
  new->parent = tok;
  new->w = w;
  insert_into_hash_table (&current_agent(left_ht), new, hv);

  /* --- look through right memory for matches --- */
  am = node->d.norm.alpha_mem;
  right_hv = am->am_id ^ referent->common.hash_id;
  for (rmi = contents_of_bucket_header_cell (&current_agent(right_ht), right_hv);
       rmi!=NIL;
       rmi=rmi->next_in_bucket) {
    if (rmi->am != am) continue;
    /* --- does rmi->w match new? --- */
    if (referent != rmi->w->id) continue;
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, new, rmi->w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,new,rmi->w);
  }
}   

void unhashed_positive_node_left_addition (rete_node *node, token *tok,
                                           wme *w) {
  unsigned long hv;
  right_mem *rm;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  token *new;
  
#ifdef DO_RIGHT_UNLINKING
  if (node->right_mem_link_status==RIGHT_UNLINKED)
    relink_to_right_mem (node);
  node->c.num_left_tokens++;
#endif

  hv = node->node_id;

  /* --- build new left_token, add it to the hash table --- */
  token_added();
  allocate_with_pool (&current_agent(token_pool), &new);
  new->node = node;
  new->parent = tok;
  new->w = w;
  insert_into_hash_table (&current_agent(left_ht), new, hv);

  /* --- look through right memory for matches --- */
  for (rm=node->d.norm.alpha_mem->right_mems; rm!=NIL; rm=rm->next) {
    /* --- does rm->rmi.w match new? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, new, rm->rmi.w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,new,rm->rmi.w);
  }
}   



void positive_node_left_removal (rete_node *node, token *tok, wme *w) {
  unsigned long hv, right_hv;
  Symbol *referent;
  right_mem_item *rmi;
  alpha_mem *am;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  token *t, **prev_t_pointer;
  
#ifdef DO_RIGHT_UNLINKING
  node->c.num_left_tokens--;
  if (node->c.num_left_tokens==0)
    if (node->right_mem_link_status!=DONT_RIGHT_UNLINK)
      unlink_from_right_mem (node);
#endif

  {
    int levels_up;
    token *t;

    levels_up = node->d.norm.left_hash_loc.levels_up;
    if (levels_up==1) {
      referent = field_from_wme (w, node->d.norm.left_hash_loc.field_num);
    } else { /* --- levels_up > 1 --- */
      for (t=tok, levels_up -= 2; levels_up!=0; levels_up--) t=t->parent;
      referent = field_from_wme (t->w, node->d.norm.left_hash_loc.field_num);
    }
  }
  
  hv = node->node_id ^ referent->common.hash_id;

  /* --- find and remove the old left_token from the hash table --- */
  prev_t_pointer =
    (token **) address_of_bucket_header_cell (&current_agent(left_ht),hv);
  while (TRUE) {
    t = *prev_t_pointer;
    if ((t->node==node) && (t->parent==tok) && (t->w==w)) break;
    prev_t_pointer = &(t->next_in_bucket);
  }
  *prev_t_pointer = t->next_in_bucket;

  /* --- look through right memory for matches --- */
  am = node->d.norm.alpha_mem;
  right_hv = am->am_id ^ referent->common.hash_id;
  for (rmi = * ((right_mem_item **)
                (address_of_bucket_header_cell (&current_agent(right_ht), right_hv)));
       rmi!=NIL;
       rmi=rmi->next_in_bucket) {
    if (rmi->am != am) continue;
    /* --- does rmi->w match t? --- */
    if (referent != rmi->w->id) continue;
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, t, rmi->w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,t,rmi->w);
  }

  token_deleted();
  free_with_pool (&current_agent(token_pool), t);
}

void unhashed_positive_node_left_removal (rete_node *node, token *tok,
                                          wme *w) {
  unsigned long hv;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  token *t, **prev_t_pointer;
  right_mem *rm;
  
#ifdef DO_RIGHT_UNLINKING
  node->c.num_left_tokens--;
  if (node->c.num_left_tokens==0)
    if (node->right_mem_link_status!=DONT_RIGHT_UNLINK)
      unlink_from_right_mem (node);
#endif

  hv = node->node_id;

  /* --- find and remove the old left_token from the hash table --- */
  prev_t_pointer =
    (token **) address_of_bucket_header_cell (&current_agent(left_ht),hv);
  while (TRUE) {
    t = *prev_t_pointer;
    if ((t->node==node) && (t->parent==tok) && (t->w==w)) break;
    prev_t_pointer = &(t->next_in_bucket);
  }
  *prev_t_pointer = t->next_in_bucket;

  /* --- look through right memory for matches --- */
  for (rm=node->d.norm.alpha_mem->right_mems; rm!=NIL; rm=rm->next) {
    /* --- does rm->rmi.w match t? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, t, rm->rmi.w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,t,rm->rmi.w);
  }

  token_deleted();
  free_with_pool (&current_agent(token_pool), t);
}

void positive_node_right_addition (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  Symbol *referent;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
 
  referent = w->id;
  hv = node->node_id ^ referent->common.hash_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    { token *t;
      int i;
      t = tok;
      i = node->d.norm.left_hash_loc.levels_up - 1;
      while (i!=0) { t = t->parent; i--; }
      if (referent != field_from_wme (t->w,
                               node->d.norm.left_hash_loc.field_num))
        continue;
    }
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,tok,w);
  }
}

void unhashed_positive_node_right_addition (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
 
  hv = node->node_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,tok,w);
  }
}



void positive_node_right_removal (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  Symbol *referent;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
 
  referent = w->id;
  hv = node->node_id ^ referent->common.hash_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    { token *t;
      int i;
      t = tok;
      i = node->d.norm.left_hash_loc.levels_up - 1;
      while (i!=0) { t = t->parent; i--; }
      if (referent != field_from_wme (t->w,
                               node->d.norm.left_hash_loc.field_num))
        continue;
    }
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,tok,w);
  }
}

void unhashed_positive_node_right_removal (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
 
  hv = node->node_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so call each child node --- */
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,tok,w);
  }
}

void negative_node_left_addition (rete_node *node, token *tok, wme *w) {
  unsigned long hv, right_hv;
  Symbol *referent;
  right_mem_item *rmi;
  alpha_mem *am;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  neg_token *new;

#ifdef DO_RIGHT_UNLINKING
  if (node->right_mem_link_status==RIGHT_UNLINKED)
    relink_to_right_mem (node);
#endif

  {
    int levels_up;
    token *t;
    
    levels_up = node->d.norm.left_hash_loc.levels_up;
    if (levels_up==1) {
      referent = field_from_wme (w, node->d.norm.left_hash_loc.field_num);
    } else { /* --- levels_up > 1 --- */
      for (t=tok, levels_up -= 2; levels_up!=0; levels_up--) t=t->parent;
      referent = field_from_wme (t->w, node->d.norm.left_hash_loc.field_num);
    }
  }
  
  hv = node->node_id ^ referent->common.hash_id;

  /* --- build new neg_token, add it to the hash table --- */
  token_added();
  allocate_with_pool (&current_agent(neg_token_pool), &new);
  new->tok.node = node;
  new->tok.parent = tok;
  new->tok.w = w;
  insert_into_hash_table (&current_agent(left_ht), (token *)new, hv);
  insert_at_head_of_dll (node->c.neg_tokens, new, next, prev);
  new->match_count = 0;

  /* --- look through right memory for matches --- */
  am = node->d.norm.alpha_mem;
  right_hv = am->am_id ^ referent->common.hash_id;
  for (rmi = contents_of_bucket_header_cell (&current_agent(right_ht), right_hv);
       rmi!=NIL;
       rmi=rmi->next_in_bucket) {
    if (rmi->am != am) continue;
    /* --- does rmi->w match new? --- */
    if (referent != rmi->w->id) continue;
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, (token *)new, rmi->w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    new->match_count++;
  }

  /* --- if no matches were found, call each child node --- */
  if (new->match_count==0) {
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,(token *)new,NIL);
  }
}

void unhashed_negative_node_left_addition (rete_node *node, token *tok,
                                           wme *w) {
  unsigned long hv;
  rete_test *rt;
  bool failed_a_test;
  right_mem *rm;
  rete_node *child;
  neg_token *new;
  
#ifdef DO_RIGHT_UNLINKING
  if (node->right_mem_link_status==RIGHT_UNLINKED)
    relink_to_right_mem (node);
#endif

  hv = node->node_id;

  /* --- build new neg_token, add it to the hash table --- */
  token_added();
  allocate_with_pool (&current_agent(neg_token_pool), &new);
  new->tok.node = node;
  new->tok.parent = tok;
  new->tok.w = w;
  insert_into_hash_table (&current_agent(left_ht), (token *)new, hv);
  insert_at_head_of_dll (node->c.neg_tokens, new, next, prev);
  new->match_count = 0;

  /* --- look through right memory for matches --- */
  for (rm=node->d.norm.alpha_mem->right_mems; rm!=NIL; rm=rm->next) {
    /* --- does rm->rmi.w match new? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, (token *)new, rm->rmi.w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    new->match_count++;
  }

  /* --- if no matches were found, call each child node --- */
  if (new->match_count==0) {
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,(token *)new,NIL);
  }
}


void negative_node_left_removal (rete_node *node, token *tok, wme *w) {
  unsigned long hv;
  Symbol *referent;
  rete_node *child;
  neg_token *nt;
  token *t, **prev_t_pointer;
  
  {
    int levels_up;
    token *t;

    levels_up = node->d.norm.left_hash_loc.levels_up;
    if (levels_up==1) {
      referent = field_from_wme (w, node->d.norm.left_hash_loc.field_num);
    } else { /* --- levels_up > 1 --- */
      for (t=tok, levels_up -= 2; levels_up!=0; levels_up--) t=t->parent;
      referent = field_from_wme (t->w, node->d.norm.left_hash_loc.field_num);
    }
  }
  
  hv = node->node_id ^ referent->common.hash_id;

  /* --- find and remove the old neg_token from the hash table --- */
  prev_t_pointer =
    (token **) address_of_bucket_header_cell (&current_agent(left_ht),hv);
  while (TRUE) {
    t = *prev_t_pointer;
    if ((t->node==node) && (t->parent==tok) && (t->w==w)) break;
    prev_t_pointer = &(t->next_in_bucket);
  }
  *prev_t_pointer = t->next_in_bucket;

  /* --- if it previously had no matches, call each child node --- */
  nt = (neg_token *) t;
  if (nt->match_count==0) {
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,t,NIL);
  }

  remove_from_dll (node->c.neg_tokens, nt, next, prev);
  free_with_pool (&current_agent(neg_token_pool), nt);
  token_deleted();

#ifdef DO_RIGHT_UNLINKING
  if (node->c.neg_tokens==NIL)
    if (node->right_mem_link_status!=DONT_RIGHT_UNLINK)
      unlink_from_right_mem (node);
#endif

}


void unhashed_negative_node_left_removal (rete_node *node, token *tok,
                                          wme *w) {
  unsigned long hv;
  rete_node *child;
  neg_token *nt;
  token *t, **prev_t_pointer;
  
  hv = node->node_id;

  /* --- find and remove the old neg_token from the hash table --- */
  prev_t_pointer =
    (token **) address_of_bucket_header_cell (&current_agent(left_ht),hv);
  while (TRUE) {
    t = *prev_t_pointer;
    if ((t->node==node) && (t->parent==tok) && (t->w==w)) break;
    prev_t_pointer = &(t->next_in_bucket);
  }
  *prev_t_pointer = t->next_in_bucket;

  /* --- if it previously had no matches, call each child node --- */
  nt = (neg_token *) t;
  if (nt->match_count==0) {
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,t,NIL);
  }

  remove_from_dll (node->c.neg_tokens, nt, next, prev);
  free_with_pool (&current_agent(neg_token_pool), nt);
  token_deleted();

#ifdef DO_RIGHT_UNLINKING
  if (node->c.neg_tokens==NIL)
    if (node->right_mem_link_status!=DONT_RIGHT_UNLINK)
      unlink_from_right_mem (node);
#endif

}


void negative_node_right_addition (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  Symbol *referent;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  neg_token *nt;

  referent = w->id;
  hv = node->node_id ^ referent->common.hash_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    { token *t;
      int i;
      t = tok;
      i = node->d.norm.left_hash_loc.levels_up - 1;
      while (i!=0) { t = t->parent; i--; }
      if (referent != field_from_wme (t->w,
                               node->d.norm.left_hash_loc.field_num))
        continue;
    }
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so update match count and possibly call children --- */
    nt = (neg_token *) tok;
    nt->match_count++;
    if (nt->match_count > 1) continue;
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,tok,NIL);
  }
}

void unhashed_negative_node_right_addition (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  neg_token *nt;
  
  hv = node->node_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so update match count and possibly call children --- */
    nt = (neg_token *) tok;
    nt->match_count++;
    if (nt->match_count > 1) continue;
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,tok,NIL);
  }
}



void negative_node_right_removal (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  Symbol *referent;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  neg_token *nt;
  
  referent = w->id;
  hv = node->node_id ^ referent->common.hash_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    { token *t;
      int i;
      t = tok;
      i = node->d.norm.left_hash_loc.levels_up - 1;
      while (i!=0) { t = t->parent; i--; }
      if (referent != field_from_wme (t->w,
                               node->d.norm.left_hash_loc.field_num))
        continue;
    }
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so update match count and possibly call children --- */
    nt = (neg_token *) tok;
    nt->match_count--;
    if (nt->match_count > 0) continue;
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,tok,NIL);
  }
}

void unhashed_negative_node_right_removal (rete_node *node, wme *w) {
  unsigned long hv;
  token *tok;
  rete_test *rt;
  bool failed_a_test;
  rete_node *child;
  neg_token *nt;
  
  hv = node->node_id;

  for (tok = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       tok!=NIL;
       tok=tok->next_in_bucket) {
    if (tok->node != node) continue;
    /* --- does tok match w? --- */
    failed_a_test = FALSE;
    for (rt=node->d.norm.other_tests; rt!=NIL; rt=rt->next)
      if (! match_left_and_right (rt, tok, w)) {
        failed_a_test = TRUE;
        break;
      }
    if (failed_a_test) continue;
    /* --- match found, so update match count and possibly call children --- */
    nt = (neg_token *) tok;
    nt->match_count--;
    if (nt->match_count > 0) continue;
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,tok,NIL);
  }
}

/* -----------------------------------------------------------------------
   cn left addition:
     look for (tok,w) in left_ht
     if found, do nothing
     if not found, create new neg_token, add it to left_ht, and call children

   cn left removal:
     find (tok,w) in left_ht
     remove it from left_ht
     if match_count==0, call children

   cn_partner left addition:
     scan up to find cn's (tok,w) pair
     look for (tok,w) in left_ht
     if found:
       increment match_count;
       if match_count just went positive, call children
     if not found:
       create entry in left_ht with match_count=1

   cn_partner left removal:
     scan up to find cn's (tok,w) pair
     look for (tok,w) in left_ht
     if found:
       decrement match_count
       if match_count just went to 0, call children
     if not found:
       do nothing
----------------------------------------------------------------------- */

void cn_node_left_addition (rete_node *node, token *tok, wme *w) {
  unsigned long hv;
  token *t;
  neg_token *new;
  rete_node *child;
 
  hv = node->node_id ^ (unsigned long)tok ^ (unsigned long)w;

  /* --- look for the matching token in left_ht; if found, exit --- */
  for (t = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       t!=NIL;
       t=t->next_in_bucket)
    if ((t->node==node)&&(t->parent==tok)&&(t->w==w)) return;

  /* --- build new neg_token, add it to the hash table --- */
  token_added();
  allocate_with_pool (&current_agent(neg_token_pool), &new);
  new->tok.node = node;
  new->tok.parent = tok;
  new->tok.w = w;
  insert_into_hash_table (&current_agent(left_ht), (token *)new, hv);
  insert_at_head_of_dll (node->c.neg_tokens, new, next, prev);
  new->match_count = 0;
 
  /* --- ... and pass new token on to each child node --- */
  for (child=node->first_child; child!=NIL; child=child->next_sibling)
    (*(left_addition_routines[child->node_type]))(child,(token *)new,NIL);
}

void cn_node_left_removal (rete_node *node, token *tok, wme *w) {
  unsigned long hv;
  rete_node *child;
  neg_token *nt;
  token *t, **prev_t_pointer;
  
  hv = node->node_id ^ (unsigned long)tok ^ (unsigned long)w;

  /* --- find and remove the old neg_token from the hash table --- */
  prev_t_pointer =
    (token **) address_of_bucket_header_cell (&current_agent(left_ht),hv);
  while (TRUE) {
    t = *prev_t_pointer;
    if ((t->node==node) && (t->parent==tok) && (t->w==w)) break;
    prev_t_pointer = &(t->next_in_bucket);
  }
  *prev_t_pointer = t->next_in_bucket;

  /* --- if it previously had no matches, call each child node --- */
  nt = (neg_token *) t;
  if (nt->match_count==0) {
    for (child=node->first_child; child!=NIL; child=child->next_sibling)
      (*(left_removal_routines[child->node_type]))(child,t,NIL);
  }

  remove_from_dll (node->c.neg_tokens, nt, next, prev);
  free_with_pool (&current_agent(neg_token_pool), nt);
  token_deleted();
}
 

void cn_partner_node_left_addition (rete_node *node, token *tok, wme *w) {
  token *t;
  rete_node *child;
  rete_node *partner, *temp;
  neg_token *nt, *new;
  unsigned long hv;
   
  /* --- advance tok,w up to the token from the top of the branch --- */
  partner = node->d.cn.partner;
  temp = node->parent;
  while (temp != partner->parent) {
    temp = temp->parent;
    w = tok->w;
    tok = tok->parent;
  }

  /* --- look for the matching token in left_ht --- */
  hv = partner->node_id ^ (unsigned long)tok ^ (unsigned long)w;
  for (t = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       t!=NIL;
       t=t->next_in_bucket)
    if ((t->node==partner)&&(t->parent==tok)&&(t->w==w)) break;

  /* --- if found, increment match count and possibly call children --- */
  if (t) {
    nt = (neg_token *) t;
    nt->match_count++;
    if (nt->match_count == 1)
      for (child=partner->first_child; child!=NIL; child=child->next_sibling)
        (*(left_removal_routines[child->node_type]))(child,t,NIL);
    return;
  }

  /* --- if not found, create new entry in left_ht with match_count=1 --- */
  token_added();
  allocate_with_pool (&current_agent(neg_token_pool), &new);
  new->tok.node = partner;
  new->tok.parent = tok;
  new->tok.w = w;
  insert_into_hash_table (&current_agent(left_ht), (token *)new, hv);
  insert_at_head_of_dll (partner->c.neg_tokens, new, next, prev);
  new->match_count = 1;
}


void cn_partner_node_left_removal (rete_node *node, token *tok, wme *w) {
  token *t;
  rete_node *child;
  rete_node *partner, *temp;
  neg_token *nt;
  unsigned long hv;

  /* --- advance tok,w up to the token from the top of the branch --- */
  partner = node->d.cn.partner;
  temp = node->parent;
  while (temp != partner->parent) {
    temp = temp->parent;
    w = tok->w;
    tok = tok->parent;
  }

  /* --- look for matching token in left_ht --- */
  hv = partner->node_id ^ (unsigned long)tok ^ (unsigned long)w;
  for (t = contents_of_bucket_header_cell (&current_agent(left_ht),hv);
       t!=NIL;
       t=t->next_in_bucket)
    if ((t->node==partner)&&(t->parent==tok)&&(t->w==w)) break;

  /* --- if not found, exit --- */
  if (!t) return;

  /* --- if found, decrement match count and possibly call children --- */
  nt = (neg_token *) t;
  nt->match_count--;
  if (nt->match_count == 0)
    for (child=partner->first_child; child!=NIL; child=child->next_sibling)
      (*(left_addition_routines[child->node_type]))(child,t,NIL);
}

/* 
   Algorithm:
   
   Does this token match (wme's equal) one of tentative_retractions?
     (We have to check instantiation structure for this--when an
     instantiation retracts then re-asserts in one e-cycle, the
     token itself will be different, but all the wme's tested positively
     will be the same.)
   If so, remove that tentative_retraction.
   If not, store this new token in tentative_assertions.
*/

void p_node_left_addition (rete_node *node, token *tok, wme *w) {
  ms_change *msc;
  condition *cond;
  token *current_token;
  wme *current_wme;
  rete_node *current_node;
  bool match_found;

  match_found = FALSE;
  for (msc=node->d.p.tentative_retractions; msc!=NIL; msc=msc->next_of_node) {
    match_found = TRUE;
    cond = msc->inst->bottom_of_instantiated_conditions;
    current_token = tok;
    current_wme = w;
    current_node = node->parent;
    while (current_node->node_type!=DUMMY_TOP_BNODE) {
      if (bnode_is_positive(current_node->node_type))
        if (current_wme != cond->bt.wme) { match_found=FALSE; break; }
      current_node = current_node->parent;
      current_wme = current_token->w;
      current_token = current_token->parent;
      cond = cond->prev;
    }
    if (match_found) break;
  }

  if (match_found) {
    msc->inst->rete_token = tok;
    msc->inst->rete_wme = w;
    remove_from_dll (node->d.p.tentative_retractions, msc,
                     next_of_node, prev_of_node);
    remove_from_dll (current_agent(ms_retractions), msc, next, prev);
    free_with_pool (&current_agent(ms_change_pool), msc);

#ifdef DEBUG_RETE_PNODES
print_with_symbols ("\nRemoving tentative retraction: %y",
                    node->d.p.prod->name);
#endif

    return;
  }

#ifdef DEBUG_RETE_PNODES
print_with_symbols ("\nAdding tentative assertion: %y",
                    node->d.p.prod->name);
#endif

  allocate_with_pool (&current_agent(ms_change_pool), &msc);
  msc->tok = tok;
  msc->w = w;
  msc->p_node = node;
  msc->inst = NIL;  /* just for safety */
  insert_at_head_of_dll (node->d.p.tentative_assertions, msc,
                         next_of_node, prev_of_node);
  insert_at_head_of_dll (current_agent(ms_assertions), msc, next, prev);
}


/*
   Algorithm:

   Does this token match (eq) one of the tentative_assertions?
   If so, just remove that tentative_assertion.
   If not, find the instantiation corresponding to this token
     and add it to tentative_retractions.
*/

void p_node_left_removal (rete_node *node, token *tok, wme *w) {
  ms_change *msc;
  instantiation *inst;
  
  for (msc=node->d.p.tentative_assertions; msc!=NIL; msc=msc->next_of_node) {
    if ((msc->tok==tok) && (msc->w==w)) {
      remove_from_dll (node->d.p.tentative_assertions, msc,
                       next_of_node, prev_of_node);
      remove_from_dll (current_agent(ms_assertions), msc, next, prev);
      free_with_pool (&current_agent(ms_change_pool), msc);

#ifdef DEBUG_RETE_PNODES
print_with_symbols ("\nRemoving tentative assertion: %y",
                    node->d.p.prod->name);
#endif

      return;
    }
  } /* end of for loop */

  for (inst=node->d.p.prod->instantiations; inst!=NIL; inst=inst->next)
    if ((inst->rete_token==tok)&&(inst->rete_wme==w)) break;

  if (inst) {

#ifdef DEBUG_RETE_PNODES
print_with_symbols ("\nAdding tentative retraction: %y",
                    node->d.p.prod->name);
#endif

    inst->rete_token = NIL;
    inst->rete_wme = NIL;
    allocate_with_pool (&current_agent(ms_change_pool), &msc);
    msc->inst = inst;
    msc->p_node = node;
    msc->tok = NIL;     /* just for safety */
    msc->w = NIL;       /* just for safety */
    insert_at_head_of_dll (node->d.p.tentative_retractions, msc,
                           next_of_node, prev_of_node);
    insert_at_head_of_dll (current_agent(ms_retractions), msc, next, prev);
    return;
  }

  print ("Internal error: can't find existing instantiation to retract\n");
  abort_with_fatal_error();
}


/* --------------------------------------------------------------------
                          Rete Statistics   
-------------------------------------------------------------------- */

typedef void ((*rete_node_callback_fn)(rete_node *node));

void do_for_this_and_all_child_rete_nodes (rete_node *node,
                                           rete_node_callback_fn f) {
  rete_node *child;

  /* --- do it for this node --- */
  (*f)(node);

  /* --- do it for each child --- */
  for (child=node->first_child; child!=NIL; child=child->next_sibling)
    do_for_this_and_all_child_rete_nodes (child, f);
}

void stats_callback_fn (rete_node *node) {
  current_agent(rete_node_counts)[node->node_type]++;
}

void print_rete_statistics (void) {
  int i;
  unsigned long total_rete_nodes;
  
  for (i=0; i<256; i++) current_agent(rete_node_counts)[i]=0;
  do_for_this_and_all_child_rete_nodes (&current_agent(dummy_top_node), stats_callback_fn);
  total_rete_nodes = 0;
  for (i=0; i<256; i++) total_rete_nodes += current_agent(rete_node_counts)[i];
  print ("%lu total rete nodes\n", total_rete_nodes);
  print ("%10lu dummy top node\n", current_agent(rete_node_counts)[DUMMY_TOP_BNODE]);
  print ("%10lu pos. nodes\n", current_agent(rete_node_counts)[POSITIVE_BNODE]);
  print ("%10lu unhashed pos. nodes\n", current_agent(rete_node_counts)[UNHASHED_POSITIVE_BNODE]);
  print ("%10lu neg. nodes\n", current_agent(rete_node_counts)[NEGATIVE_BNODE]);
  print ("%10lu unhashed neg. nodes\n", current_agent(rete_node_counts)[UNHASHED_NEGATIVE_BNODE]);
  print ("%10lu c.n. nodes\n", current_agent(rete_node_counts)[CN_BNODE]);
  print ("%10lu c.n. partner nodes\n", current_agent(rete_node_counts)[CN_PARTNER_BNODE]);
  print ("%10lu production nodes\n", current_agent(rete_node_counts)[P_BNODE]);
}

/* ----------------------------------------------------------------------
                        Partial Match Information
---------------------------------------------------------------------- */

void dummy_matches_node_left_addition (rete_node *node, token *tok, wme *w) {
  token *new;
  
  /* --- just add a token record to dummy_matches_node_tokens --- */
  allocate_with_pool (&current_agent(token_pool), &new);
  new->node = NIL;
  new->parent = tok;
  new->w = w;
  new->next_in_bucket = current_agent(dummy_matches_node_tokens);
  current_agent(dummy_matches_node_tokens) = new;
}

token *get_all_left_tokens_emerging_from_node (rete_node *node) {
  token *result;
  
  current_agent(dummy_matches_node_tokens) = NIL;
  current_agent(dummy_matches_node).parent = node;
  call_child_with_each_real_token_of_parent (&current_agent(dummy_matches_node), TRUE);
  current_agent(dummy_matches_node).parent = NIL;
  result = current_agent(dummy_matches_node_tokens);
  current_agent(dummy_matches_node_tokens) = NIL;
  return result;
}
/* 
  matches:
    (matches production-name [level: 0/1/2])
  call p_node_to_conditions_and_nots()
  start at prod's p_node, walk up the net:
    - at each node, call call_child_with_each_real_token_of_parent()
      using some dummy node that just records the token changes.
    - back up:  
        - print # of matches; print condition in p-format
        - if this is the first top-level cond that didn't match,
          print its matches-for-left and matches-for-right
        - if earlier cond didn't match, just print the p-format, nothing else.
*/

void deallocate_token_list (token *t) {
  token *next;

  while (t) {
    next = t->next_in_bucket;
    free_with_pool (&current_agent(token_pool), t);
    t = next;
  }
}

void print_whole_token (token *t,
                        rete_node *node_where_t_was_created,
                        wme_trace_type wtt) {
  if (node_where_t_was_created==&current_agent(dummy_top_node)) return;
  print_whole_token (t->parent, node_where_t_was_created->parent, wtt);
  if (bnode_is_positive(node_where_t_was_created->parent->node_type)) {
    if (wtt==TIMETAG_WME_TRACE) print ("%lu", t->w->timetag);
    else if (wtt==FULL_WME_TRACE) print_wme (t->w);
    if (wtt!=NONE_WME_TRACE) print (" ");
  }
}

int ppmi_aux (rete_node *node,
              rete_node *cutoff,
              condition *cond,
              wme_trace_type wtt,
              bool print_any_counts,
              int indent) {
  token *tokens, *t, *parent_tokens;
  int matches_one_level_up;
  int matches_at_this_level;
  right_mem *rm;
  char match_count_string[20];

  tokens = get_all_left_tokens_emerging_from_node (node);
  matches_at_this_level = 0;
  for (t=tokens; t!=NIL; t=t->next_in_bucket) matches_at_this_level++;

  if (node==cutoff) {
    deallocate_token_list (tokens);
    return matches_at_this_level;
  }

  matches_one_level_up = ppmi_aux (node->parent,
                                   cutoff,
                                   cond->prev,
                                   wtt,
                                   print_any_counts,
                                   indent);

  if (!matches_one_level_up) print_any_counts = FALSE;

  if (! print_any_counts)
    strcpy (match_count_string, "    ");
  else if (matches_at_this_level)
    sprintf (match_count_string, "%4d", matches_at_this_level);
  else
    strcpy (match_count_string, ">>>>");

  print_spaces (indent);

  if (cond->type==CONJUNCTIVE_NEGATION_CONDITION) {
    print ("    -{\n");
    ppmi_aux (node->d.cn.partner->parent,
              node->parent,
              cond->data.ncc.bottom,
              wtt,
              print_any_counts,
              indent+5);
    print_spaces (indent);
    print ("%s }\n", match_count_string);
  } else {
    print ("%s", match_count_string);
    print_condition (cond);
    print ("\n");
    if (print_any_counts && (!matches_at_this_level)) {
      if (wtt!=NONE_WME_TRACE) {
        print_spaces (indent);
        print ("*** Matches For Left ***\n");
        parent_tokens = get_all_left_tokens_emerging_from_node (node->parent);
        for (t=parent_tokens; t!=NIL; t=t->next_in_bucket) {
          print_spaces (indent);
          print_whole_token (t, node, wtt);
          print ("\n");
        }
        deallocate_token_list (parent_tokens);
        print_spaces (indent);
        print ("*** Matches for Right ***\n");
        print_spaces (indent);
        for (rm=node->d.norm.alpha_mem->right_mems; rm!=NIL; rm=rm->next) {
          if (wtt==TIMETAG_WME_TRACE) print ("%lu", rm->rmi.w->timetag);
          else if (wtt==FULL_WME_TRACE) print_wme (rm->rmi.w);
          print (" ");
        }
        print ("\n");
      }
    } /* end of if (print_any_counts...) */
  }
  
  deallocate_token_list (tokens);
  return matches_at_this_level;
}

void print_partial_match_information (rete_node *p_node, wme_trace_type wtt) {
  condition *top_cond, *bottom_cond;
  int n;
  token *tokens, *t;

  p_node_to_conditions_and_nots (p_node,NIL,NIL, &top_cond, &bottom_cond, NIL);
  n = ppmi_aux (p_node->parent, &current_agent(dummy_top_node), bottom_cond, wtt, TRUE, 0);
  print ("\n%d complete matches.\n", n);
  if (n && (wtt!=NONE_WME_TRACE)) {
    print ("*** Complete Matches ***\n");
    tokens = get_all_left_tokens_emerging_from_node (p_node->parent);
    for (t=tokens; t!=NIL; t=t->next_in_bucket) {
      print_whole_token (t, p_node, wtt);
      print ("\n");
    }
    deallocate_token_list (tokens);
  }
  deallocate_condition_list (top_cond);
}

MS_trace *in_ms_trace(Symbol *sym, MS_trace *trace) {
  MS_trace *tmp;
  for(tmp = trace; tmp; tmp=tmp->next) {
    if(tmp->sym == sym) return tmp;
  }
  return (MS_trace *)0;
}

void print_match_set (wme_trace_type wtt, ms_trace_type mst) {
  ms_change *msc;
  token temp_token;
  MS_trace *ms_trace = NIL, *tmp;
  
  if (mst == MS_ASSERT_RETRACT || mst == MS_ASSERT) {
    print ("Assertions:\n");
    for (msc=current_agent(ms_assertions); msc!=NIL; msc=msc->next) {
      if(wtt != NONE_WME_TRACE) {
        print_with_symbols ("  %y ", msc->p_node->d.p.prod->name);
        temp_token.parent = msc->tok;
        temp_token.w = msc->w;
        print_whole_token (&temp_token, msc->p_node, wtt);
        print ("\n");
      } else {
        if(tmp = in_ms_trace(msc->p_node->d.p.prod->name, ms_trace)) {
          tmp->count = tmp->count+1;
        } else {
          tmp = allocate_memory(sizeof(MS_trace), MISCELLANEOUS_MEM_USAGE);
          symbol_add_ref(msc->p_node->d.p.prod->name);
          tmp->sym = msc->p_node->d.p.prod->name;
          tmp->count = 1;
          tmp->next = ms_trace;
          ms_trace = tmp;
        }
      }
    }
    if(wtt == NONE_WME_TRACE) {
      for(tmp = ms_trace; tmp; tmp = ms_trace) {
         print_with_symbols ("  %y ", tmp->sym);
         if(tmp->count > 1)
           print("(%d)\n", tmp->count);
         else
           print("\n");
         symbol_remove_ref(tmp->sym);
         ms_trace = tmp->next;
         free_memory((void *)tmp, MISCELLANEOUS_MEM_USAGE);
      }
      ms_trace = NIL;
    }
  }
  if (mst == MS_ASSERT_RETRACT || mst == MS_RETRACT) {
    print ("Retractions:\n");
    for (msc=current_agent(ms_retractions); msc!=NIL; msc=msc->next) {
      if(wtt != NONE_WME_TRACE) {
        print ("  ");
        print_instantiation_with_wmes (msc->inst, wtt);
        print ("\n");
      } else {
        if(msc->inst->prod) {
          if(tmp = in_ms_trace(msc->inst->prod->name, ms_trace)) {
            tmp->count = tmp->count+1;
          } else {
	    tmp = allocate_memory(sizeof(MS_trace), MISCELLANEOUS_MEM_USAGE);
	    symbol_add_ref(msc->inst->prod->name);
	    tmp->sym = msc->inst->prod->name;
	    tmp->count = 1;
            tmp->next = ms_trace;
            ms_trace = tmp;
          }
        }
      }
    }
  }
  if(wtt == NONE_WME_TRACE) {
    for(tmp = ms_trace; tmp; tmp = ms_trace) {
      print_with_symbols ("  %y ", tmp->sym);
      if(tmp->count > 1)
        print("(%d)\n", tmp->count);
      else
	print("\n");
      symbol_remove_ref(tmp->sym);
      ms_trace = tmp->next;
      free_memory((void *)tmp, MISCELLANEOUS_MEM_USAGE);
    }
  }
}

/* ----------------------------------------------------------------------
                          Rete Initialization
---------------------------------------------------------------------- */

void init_rete (void) {
  int i;

  init_test_type_conversion_tables ();

  left_addition_routines[DUMMY_TOP_BNODE] = rete_error_left;
  left_removal_routines[DUMMY_TOP_BNODE] = rete_error_left;
  right_addition_routines[DUMMY_TOP_BNODE] = rete_error_right;
  right_removal_routines[DUMMY_TOP_BNODE] = rete_error_right;

  left_addition_routines[DUMMY_MATCHES_BNODE] =
    dummy_matches_node_left_addition;
  left_removal_routines[DUMMY_MATCHES_BNODE] = rete_error_left;
  right_addition_routines[DUMMY_MATCHES_BNODE] = rete_error_right;
  right_removal_routines[DUMMY_MATCHES_BNODE] = rete_error_right;

  left_addition_routines[POSITIVE_BNODE] = positive_node_left_addition;
  left_removal_routines[POSITIVE_BNODE] = positive_node_left_removal;
  right_addition_routines[POSITIVE_BNODE] = positive_node_right_addition;
  right_removal_routines[POSITIVE_BNODE] = positive_node_right_removal;

  left_addition_routines[UNHASHED_POSITIVE_BNODE] =
    unhashed_positive_node_left_addition;
  left_removal_routines[UNHASHED_POSITIVE_BNODE] =
    unhashed_positive_node_left_removal;
  right_addition_routines[UNHASHED_POSITIVE_BNODE] =
    unhashed_positive_node_right_addition;
  right_removal_routines[UNHASHED_POSITIVE_BNODE] =
    unhashed_positive_node_right_removal;

  left_addition_routines[NEGATIVE_BNODE] = negative_node_left_addition;
  left_removal_routines[NEGATIVE_BNODE] = negative_node_left_removal;
  right_addition_routines[NEGATIVE_BNODE] = negative_node_right_addition;
  right_removal_routines[NEGATIVE_BNODE] = negative_node_right_removal;

  left_addition_routines[UNHASHED_NEGATIVE_BNODE] =
    unhashed_negative_node_left_addition;
  left_removal_routines[UNHASHED_NEGATIVE_BNODE] =
    unhashed_negative_node_left_removal;
  right_addition_routines[UNHASHED_NEGATIVE_BNODE] =
    unhashed_negative_node_right_addition;
  right_removal_routines[UNHASHED_NEGATIVE_BNODE] =
    unhashed_negative_node_right_removal;

  left_addition_routines[CN_BNODE] = cn_node_left_addition;
  left_removal_routines[CN_BNODE] = cn_node_left_removal;
  right_addition_routines[CN_BNODE] = rete_error_right;
  right_removal_routines[CN_BNODE] = rete_error_right;

  left_addition_routines[CN_PARTNER_BNODE] = cn_partner_node_left_addition;
  left_removal_routines[CN_PARTNER_BNODE] = cn_partner_node_left_removal;
  right_addition_routines[CN_PARTNER_BNODE] = rete_error_right;
  right_removal_routines[CN_PARTNER_BNODE] = rete_error_right;

  left_addition_routines[P_BNODE] = p_node_left_addition;
  left_removal_routines[P_BNODE] = p_node_left_removal;
  right_addition_routines[P_BNODE] = rete_error_right;
  right_removal_routines[P_BNODE] = rete_error_right;



  for (i=0; i<256; i++) rete_test_routines[i] = error_rete_test_routine;
  rete_test_routines[DISJUNCTION_RETE_TEST] = disjunction_rete_test_routine;
  rete_test_routines[ID_IS_GOAL_RETE_TEST] = id_is_goal_rete_test_routine;
  rete_test_routines[ID_IS_IMPASSE_RETE_TEST]= id_is_impasse_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_EQUAL_RETE_TEST] =
                       constant_equal_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_NOT_EQUAL_RETE_TEST] =
                       constant_not_equal_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_LESS_RETE_TEST] =
                       constant_less_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_GREATER_RETE_TEST] =
                       constant_greater_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_LESS_OR_EQUAL_RETE_TEST] =
                       constant_less_or_equal_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_GREATER_OR_EQUAL_RETE_TEST] =
                       constant_greater_or_equal_rete_test_routine;
  rete_test_routines[CONSTANT_RELATIONAL_RETE_TEST +
                     RELATIONAL_SAME_TYPE_RETE_TEST] =
                       constant_same_type_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_EQUAL_RETE_TEST] =
                       variable_equal_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_NOT_EQUAL_RETE_TEST] =
                       variable_not_equal_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_LESS_RETE_TEST] =
                       variable_less_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_GREATER_RETE_TEST] =
                       variable_greater_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_LESS_OR_EQUAL_RETE_TEST] =
                       variable_less_or_equal_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_GREATER_OR_EQUAL_RETE_TEST] =
                       variable_greater_or_equal_rete_test_routine;
  rete_test_routines[VARIABLE_RELATIONAL_RETE_TEST +
                     RELATIONAL_SAME_TYPE_RETE_TEST] =
                       variable_same_type_rete_test_routine;


  init_memory_pool (&current_agent(alpha_mem_pool), sizeof(alpha_mem), "alpha mem");
  init_memory_pool (&current_agent(rete_test_pool), sizeof(rete_test), "rete test");
  init_memory_pool (&current_agent(rete_node_pool), sizeof(rete_node), "rete node");
  init_memory_pool (&current_agent(node_varnames_pool),sizeof(node_varnames),"node varnames");
  init_memory_pool (&current_agent(token_pool), sizeof(token), "token");
  init_memory_pool (&current_agent(neg_token_pool), sizeof(neg_token), "negative token");
  init_memory_pool (&current_agent(right_mem_pool), sizeof(right_mem), "right mem");
  init_memory_pool (&current_agent(ms_change_pool), sizeof(ms_change), "ms change");

  for (i=0; i<16; i++)
    current_agent(alpha_hash_tables)[i] = make_hash_table (0, hash_alpha_mem);

  init_token_hash_table (&current_agent(left_ht), HASH_SIZE);
  init_token_hash_table (&current_agent(right_ht), HASH_SIZE);
  
  init_dummy_top_node();
}
