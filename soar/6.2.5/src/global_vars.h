/*
 * $Id: global_vars.h,v 1.11 1995/02/06 14:35:38 rempel Exp $
 * $Log: global_vars.h,v $
 * Revision 1.11  1995/02/06  14:35:38  rempel
 * check for MAXPATHLEN defined before defining it
 *
 * Revision 1.10  1994/12/06  22:03:00  rempel
 * For 6.2.4b
 *
 * Revision 1.9  1994/11/23  16:42:28  rempel
 * for 6.2.4
 *
 * Revision 1.8  1994/08/23  10:34:00  portelli
 * For 6.2.4
 *
 * Revision 1.7  1994/06/09  21:14:03  portelli
 * For 6.2.1
 *
 * Revision 1.6  94/06/01  18:02:55  rempel
 * *** empty log message ***
 * 
 * Revision 1.5  1994/05/18  13:31:49  portelli
 * Soar 6.2.0 b
 *
 * Revision 1.4  94/05/13  18:06:17  rempel
 * added alias & directory stack
 * 
 * Revision 1.3  1994/05/10  15:05:06  rempel
 * *** empty log message ***
 *
 * Revision 1.2  1993/11/21  16:53:58  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:16  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:20:15  jtraub
 * 6.1_checkin
 *
 * Revision 9.4  1993/06/01  21:13:52  jtraub
 * added path variable.
 *
 * Revision 9.3  1993/05/10  18:14:27  jtraub
 * added RCS headers
 *
 */
 
/* Globals */

/* WARNING!! If you add a new global into the Soar C code, be
   sure to use the current_agent macro to ensure compatibility
   with the multi-agent code!  E.g. if your new global is "foo"
   then do NOT refer to it in the code as "foo" but instead as
   "current_agent(foo)". */

/* If you define a new global, initialize it in the create_soar_agent
   routine.  AGR 527c 3-May-94 */

#ifndef MAXPATHLEN
#define MAXPATHLEN 1024   /* AGR 536  - from sys/param.h */
#endif

#ifndef GLOBAL
#define GLOBAL extern
#endif
   
#ifdef _WINDOWS
GLOBAL	char		    current_line[1024];
GLOBAL	int	            current_line_index;
#endif
/* String redirection */
GLOBAL	bool		    using_input_string;
GLOBAL	char		  * input_string;
GLOBAL	bool		    using_output_string;
GLOBAL	char		  * output_string;

GLOBAL  memory_pool         action_pool;
GLOBAL  alias_struct      * alias_list;   /* AGR 568 */
GLOBAL  production        * all_productions_of_type[NUM_PRODUCTION_TYPES];
GLOBAL  wme               * all_wmes_in_rete;
GLOBAL  hash_table        *(alpha_hash_tables[16]);
GLOBAL  unsigned long       alpha_mem_id_counter;
GLOBAL  memory_pool         alpha_mem_pool;
GLOBAL  Symbol            * atom_symbol;
GLOBAL  Symbol            * attribute_symbol;
GLOBAL  tc_number           backtrace_number;
GLOBAL  unsigned long       beta_node_id_counter;
GLOBAL  Symbol            * bottom_goal;
GLOBAL  Symbol            * cc_symbol;
GLOBAL  dl_list           * changed_slots;
GLOBAL  Symbol            * choices_symbol;
GLOBAL  memory_pool         chunk_cond_pool;
GLOBAL  unsigned long       chunk_count;
GLOBAL  bool                chunk_free_flag;
GLOBAL  bool                chunky_flag;     /* AGR MVL1 */
GLOBAL  list              * chunk_free_problem_spaces;
GLOBAL  list              * chunky_problem_spaces;   /* AGR MVL1 */
GLOBAL  unsigned long       chunks_this_d_cycle;    /* # of chunks run this DC */
GLOBAL  io_wme            * collected_io_wmes;
GLOBAL  not               * collected_nots;
GLOBAL  memory_pool         complex_test_pool;
GLOBAL  memory_pool         condition_pool;
GLOBAL  Symbol            * conflict_symbol;
GLOBAL  memory_pool         cons_cell_pool;
GLOBAL  Symbol            * constraint_failure_symbol;
GLOBAL  dl_list           * context_slots_with_changed_acceptable_preferences;
GLOBAL  Symbol            * crlf_symbol;
GLOBAL  double              cumulative_wm_size;
GLOBAL  char                current_char;
GLOBAL  lexer_source_file * current_file;
GLOBAL  enum top_level_phase current_phase;
GLOBAL  unsigned long       current_symbol_hash_id;
GLOBAL  struct text_environment current_text_environment;
GLOBAL  unsigned long       current_variable_gensym_number;
GLOBAL  unsigned long       current_wme_timetag;
GLOBAL  int                 default_print_depth;      /* AGR 646 */
GLOBAL  unsigned long       d_cycle_count;            /* # of DC's run so far */
GLOBAL  dl_list           * disconnected_ids;
GLOBAL  memory_pool         dl_cons_pool;
GLOBAL  rete_node           dummy_matches_node;
GLOBAL  token             * dummy_matches_node_tokens;
GLOBAL  rete_node           dummy_top_node;
GLOBAL  backtrace_str     * explain_backtrace_list;     /* AGR 564 */
GLOBAL  explain_chunk_str * explain_chunk_list;         /* AGR 564 */
GLOBAL  char                explain_chunk_name[256];    /* AGR 564 */
GLOBAL  bool                explain_flag;
GLOBAL  unsigned long       e_cycle_count;            /* # of EC's run so far */
GLOBAL  unsigned long       e_cycles_this_d_cycle;    /* # of EC's run this DC */
GLOBAL  output_link       * existing_output_links;
GLOBAL  preference        * extra_result_prefs_from_instantiation;
GLOBAL  struct hash_table_struct  * float_constant_hash_table;
GLOBAL  memory_pool         float_constant_pool;
GLOBAL  char              * format;
GLOBAL  char              * format_string_error_message;
GLOBAL  bool                found_undefined;   
GLOBAL  Symbol            * g_context_variable;
GLOBAL  unsigned long       gensymed_variable_count[26];
GLOBAL  long                go_number;
GLOBAL  Symbol            * go_slot_attr;
GLOBAL  goal_stack_level    go_slot_level;
GLOBAL  enum go_type_enum   go_type;
GLOBAL  Symbol            * goal_symbol;
GLOBAL  list              * grounds;
GLOBAL  tc_number           grounds_tc;
GLOBAL  Symbol            * highest_goal_whose_context_changed;
GLOBAL  goal_stack_level    highest_level_anything_could_fall_from;
GLOBAL  unsigned long       id_counter[26]; 
GLOBAL  struct hash_table_struct * identifier_hash_table;
GLOBAL  memory_pool         identifier_pool;
GLOBAL  dl_list           * ids_with_unknown_level;
GLOBAL  Symbol            * impasse_symbol;
GLOBAL  int                 index_num;
GLOBAL  input_function_info * input_functions;
GLOBAL  int                 input_period;      /* AGR REW1 */
GLOBAL  bool                input_cycle_flag;  /* AGR REW1 */
GLOBAL  memory_pool         instantiation_pool;
GLOBAL  list              * instantiations_with_nots;
GLOBAL  struct hash_table_struct  * int_constant_hash_table;
GLOBAL  memory_pool         int_constant_pool;
GLOBAL  char                interrupt_source[2*MAX_LEXEME_LENGTH+100];
#ifdef NNPSCM
GLOBAL  Symbol            * io_symbol;
GLOBAL  Symbol            * io_header;
GLOBAL  wme               * io_header_link;
#endif
GLOBAL  memory_pool         io_wme_pool;
GLOBAL  Symbol            * item_symbol;
GLOBAL  unsigned long       justification_count;
GLOBAL  token_hash_table    left_ht;
GLOBAL  goal_stack_level    level_at_which_marking_started;
GLOBAL  expansion_node    * lex_alias;         /* AGR 568 */
GLOBAL  struct lexeme_info  lexeme;
GLOBAL  int                 link_update_mode;
GLOBAL  bool                load_errors_quit;  /* AGR 527c */
GLOBAL  list              * locals;
GLOBAL  tc_number           locals_tc;
GLOBAL  FILE              * log_file;
GLOBAL  char              * log_file_name;
GLOBAL  bool                logging_to_file;
GLOBAL  goal_stack_level    lowest_level_anything_could_fall_to;
GLOBAL  tc_number           mark_tc_number;
#ifndef NO_TIMING_STUFF
GLOBAL  struct timeval      match_cpu_time;
#endif
GLOBAL  bool		    max_chunks_reached;
GLOBAL  unsigned long       max_wm_size;    /* maximum size of WM so far */
GLOBAL  unsigned long       mcs_counter;
GLOBAL  unsigned long       memory_for_usage[NUM_MEM_USAGE_CODES];
GLOBAL  memory_pool       * memory_pools_in_use;
GLOBAL  ms_change         * ms_assertions;
GLOBAL  memory_pool         ms_change_pool;
GLOBAL  ms_change         * ms_retractions;
GLOBAL  Symbol            * multiple_symbol;
GLOBAL  char              * name;
GLOBAL  Symbol            * name_symbol;
GLOBAL  node_varnames     * ncc_subconditions_bottom_dense_nvn;
GLOBAL  rete_node         * ncc_subconditions_bottom_node;
GLOBAL  node_varnames     * ncc_subconditions_bottom_sparse_nvn;
GLOBAL  memory_pool         neg_token_pool;
GLOBAL  chunk_cond_set      negated_set; 
GLOBAL  instantiation     * newly_created_instantiations;
GLOBAL  Symbol            * next_symbol;
GLOBAL  Symbol            * nil_symbol;
GLOBAL  Symbol            * no_change_symbol;
GLOBAL  memory_pool         node_varnames_pool;
GLOBAL  Symbol            * none_symbol;
GLOBAL  memory_pool         not_pool;
GLOBAL  unsigned long       num_existing_wmes;
GLOBAL  unsigned long       num_productions_of_type[NUM_PRODUCTION_TYPES];
GLOBAL  unsigned long       num_wm_sizes_accumulated; 
GLOBAL  unsigned long       num_wmes_in_rete;
GLOBAL  Symbol            * o_context_variable;
GLOBAL  tc_number           o_support_tc;   
GLOBAL  Symbol            * object_symbol;
GLOBAL  trace_format      *(object_tf_for_anything[5]);
GLOBAL  struct hash_table_struct *(object_tr_ht[5]);
GLOBAL  Symbol            * operator_symbol;
GLOBAL  output_function_info * output_functions;
GLOBAL  output_link       * output_link_for_tc;
GLOBAL  memory_pool         output_link_pool;
GLOBAL  tc_number           output_link_tc_num;
GLOBAL  Symbol            * p_context_variable;
GLOBAL  unsigned long       placeholder_counter[26];
GLOBAL  list              * positive_potentials;
GLOBAL  tc_number           potentials_tc;
GLOBAL  memory_pool         preference_pool;
GLOBAL  struct text_environment prev_text_environment;
GLOBAL  Symbol            * prev_top_state;
GLOBAL  char                printed_output_string[MAX_LEXEME_LENGTH*2+10];
GLOBAL  int                 printer_output_column;
GLOBAL  bool                printing_stack_traces;
GLOBAL  bool                print_prompt_flag;
GLOBAL  Symbol            * problem_space_symbol;
GLOBAL  byte                production_addition_result;
GLOBAL  production        * production_being_added_now;
GLOBAL  production        * production_being_fired;
GLOBAL  unsigned long       production_firing_count;  /* # of production firings */
GLOBAL  memory_pool         production_pool;
GLOBAL  list              * productions_being_traced; 
GLOBAL  list              * promoted_ids;
GLOBAL  Symbol            * quiescence_symbol;
GLOBAL  bool                quiescence_t_flag;
GLOBAL  char              * reason_for_stopping;
GLOBAL  bool                redirecting_to_file;
GLOBAL  FILE              * redirection_file;
GLOBAL  instantiation     * refracted_inst_for_production_being_added_now;
GLOBAL  preference        * results;
GLOBAL  goal_stack_level    results_match_goal_level;
GLOBAL  tc_number           results_tc_number;
GLOBAL  unsigned long       rete_node_counts[256];
GLOBAL  memory_pool         rete_node_pool;
GLOBAL  memory_pool         rete_test_pool;
GLOBAL  preference        * rhs_prefs_from_instantiation;
GLOBAL  token_hash_table    right_ht;
GLOBAL  memory_pool         right_mem_pool;
GLOBAL  Symbol            * s_context_variable;
GLOBAL  int                 saved_printer_output_column;
GLOBAL  memory_pool         saved_test_pool;
GLOBAL  Symbol            * sg_context_variable;
GLOBAL  memory_pool         slot_pool;
GLOBAL  list              * slots_for_possible_removal;
GLOBAL  Symbol            * so_context_variable;
GLOBAL  Symbol            * sp_context_variable;
GLOBAL  Symbol            * space_symbol;
GLOBAL  Symbol            * ss_context_variable;
GLOBAL  Symbol            * ssg_context_variable;
GLOBAL  Symbol            * sso_context_variable;
GLOBAL  Symbol            * ssp_context_variable;
GLOBAL  Symbol            * sss_context_variable;
GLOBAL  trace_format      *(stack_tf_for_anything[5]);
GLOBAL  struct hash_table_struct *(stack_tr_ht[5]);
#ifndef NO_TIMING_STUFF
GLOBAL  struct timeval      start_total_tv;
#endif
GLOBAL  Symbol            * state_symbol;
GLOBAL  Symbol            * stdin_symbol;
GLOBAL  Symbol            * stdout_symbol;
GLOBAL  bool                stop_soar;
#ifdef NNPSCM
GLOBAL  Symbol            * superstate_symbol;
#endif
GLOBAL  struct hash_table_struct  * sym_constant_hash_table;
GLOBAL  memory_pool         sym_constant_pool;
GLOBAL  long                sysparams[HIGHEST_SYSPARAM_NUMBER+1];
GLOBAL  bool                system_halted;
GLOBAL  Symbol            * t_symbol;
GLOBAL  Symbol            * te_identifier;
GLOBAL  wme               * te_input_stream_wme;
GLOBAL  wme               * te_output_stream_wme;
GLOBAL  Symbol            * text_command_symbol;
GLOBAL  wme               * text_environment_link;
GLOBAL  Symbol            * text_environment_symbol;
GLOBAL  wme               * text_input_link;
GLOBAL  queue             * text_input_queue;
GLOBAL  Symbol            * text_input_stream_symbol;
GLOBAL  Symbol            * text_input_symbol;
GLOBAL  text_io_symbol_to_file_mapping * text_io_symbol_to_file_mappings;
GLOBAL  Symbol            * text_output_stream_symbol;
GLOBAL  Symbol            * text_output_symbol;
GLOBAL  tc_number           tf_printing_tc;   
GLOBAL  Symbol            * tg_context_variable;
GLOBAL  Symbol            * tie_symbol;
GLOBAL  Symbol            * to_context_variable;
GLOBAL  unsigned long       token_additions;
GLOBAL  unsigned long       token_deletions;
GLOBAL  memory_pool         token_pool;
GLOBAL  dir_stack_struct  * top_dir_stack;   /* AGR 568 */
GLOBAL  Symbol            * top_goal;
GLOBAL  Symbol            * top_state;
#ifndef NO_TIMING_STUFF
GLOBAL  struct timeval      total_cpu_time;
#endif
GLOBAL  Symbol            * tp_context_variable;
GLOBAL  Symbol            * ts_context_variable;
GLOBAL  Symbol            * type_symbol;
GLOBAL  struct hash_table_struct * variable_hash_table;
GLOBAL  memory_pool         variable_pool;

/*mvp 5-17-94 */
GLOBAL  list              * variables_set;

GLOBAL  tc_number           variablization_tc;
GLOBAL  bool                variablize_this_chunk;
GLOBAL  goal_stack_level    walk_level;
GLOBAL  tc_number           walk_tc_number;
GLOBAL  bool                warn_on_duplicate_production;
GLOBAL  unsigned long       wme_addition_count;       /* # of wmes added to WM */
GLOBAL  memory_pool         wme_pool;
GLOBAL  unsigned long       wme_removal_count;      /* # of wmes removed from WM */
GLOBAL  list              * wmes_to_add;
GLOBAL  list              * wmes_to_remove;
GLOBAL  multi_attribute   * multi_attributes;
/* GLOBAL  char                path[MAXPATHLEN];    AGR 568 */
#ifdef DETAILED_TIMING_STATS
GLOBAL  struct timeval      ownership_cpu_time;
GLOBAL  struct timeval      chunking_cpu_time;
GLOBAL  struct timeval      preference_phase_cpu_time;
GLOBAL  struct timeval      create_instantiations_cpu_time;
GLOBAL  struct timeval      o_support_cpu_time;
#endif

#undef GLOBAL

