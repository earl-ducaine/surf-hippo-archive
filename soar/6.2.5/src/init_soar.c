/*
 * $Id: init_soar.c,v 1.14 1994/12/15 20:55:21 rempel Exp $
 * $Log: init_soar.c,v $
 * Revision 1.14  1994/12/15  20:55:21  rempel
 * For 6.2.4 final release
 *
 * Revision 1.13  1994/12/06  22:03:13  rempel
 * For 6.2.4b
 *
 * Revision 1.12  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.11  1994/08/23  10:34:24  portelli
 * For 6.2.4
 *
 * Revision 1.10  1994/07/01  15:57:45  portelli
 * For 6.2.2
 *
 * Revision 1.9  1994/06/08  22:18:53  portelli
 * For 6.2.1
 *
 * Revision 1.8  94/05/18  13:32:02  portelli
 * Soar 6.2.0 b
 * 
 * Revision 1.7  94/05/13  18:18:00  rempel
 * added alias & directory stack
 * 
 * Revision 1.6  1994/05/06  20:27:57  rempel
 * *** empty log message ***
 *
 * Revision 1.5  1994/03/15  17:36:01  rempel
 * added destroy agent message
 *
 * Revision 1.4  93/11/24  14:13:27  portelli
 * fixed 6.1.1 bug
 * 
 * Revision 1.3  93/11/21  16:55:43  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:47:20  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:20:40  jtraub
 * 6.1_checkin
 *
 * Revision 9.7  1993/06/17  18:33:32  jtraub
 * fixed one last bug in the new cd code.
 *
 * Revision 9.6  1993/06/01  21:14:11  jtraub
 * various changes to support seperate working directories.
 *
 * Revision 9.5  1993/05/10  19:37:52  jtraub
 * added RCS header information
 *
 */

/* ===================================================================
                     Initialization routines for Soar 6
=================================================================== */

#include <signal.h>         /* used for control-c handler */
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
#include <sys/time.h>       /* used for timing stuff */
#include <sys/resource.h>   /* used for timing stuff */
#endif
#endif
#ifdef __hpux
#undef _INCLUDE_HPUX_SOURCE
#endif /* __hpux */
#include "soar.h"
#include "scheduler.h"

#ifdef __hpux
#include <sys/syscall.h>
#include <unistd.h>
#define getrusage(a, b) syscall(SYS_GETRUSAGE, a, b)
#define getwd(arg) getcwd(arg, (size_t) 9999)
#endif /* __hpux */

#ifdef __SC__
#include "Menus.h"
#include "Windows.h"
#include "TextEdit.h"
#include "Dialogs.h"
#include "Memory.h"
#include "Fonts.h"
#include "Events.h"

	static char console_environment;
	static MenuHandle appleMenu;
	
#endif
#define GLOBAL_AGENT_NAME "control"

#ifdef _WINDOWS
#define INIT_FILE	"init.soa"
#define MULTI_INIT_FILE	"init.mso"
#else
#define INIT_FILE	".init.soar"
#define MULTI_INIT_FILE	".init.soar.multi"
#endif

/* ===================================================================

                            Exiting Soar

   Exit_soar() and abort_with_fatal_error() both terminate Soar, closing
   the log file before exiting.  Abort_with_fatal_error() also prints
   an error message before exiting.
=================================================================== */

void exit_soar (void) {
#ifdef _WINDOWS
  print("Cannot exit from Soar via the command line.\n");
#else
  system_termination_hook (TRUE);
  if (current_agent(logging_to_file)) stop_log_file ();
  exit (0);
#endif
}

void abort_with_fatal_error (void) {
  print ("Soar cannot recover from this error.  Aborting...\n");
  system_termination_hook (FALSE);
  if (current_agent(logging_to_file)) stop_log_file ();
#ifdef _WINDOWS
  Terminate(1);
#else
  exit (1);
#endif
}

/* ===================================================================
   
                        Signal Handling

   Setup things so control_c_handler() gets control whenever the program
   receives a SIGINT (e.g., from a ctrl-c at the keyboard).  The handler
   just sets the stop_soar flag.
=================================================================== */

char * c_interrupt_msg = "*** Ctrl-C Interrupt ***";

/* AGR 581  The variable the_signal is not used at all, so I thought I
   would remove it.  Unfortunately, the signal command at the end of this
   function requires a function name that has a single integer parameter.
   It's probably some unix thing.  So I left the variable in the parameter
   list and instead changed the calling functions to use a parameter.
   94.11.15 (although this was done a month or two earlier--this comment
   was placed here in retrospect.)  */

void control_c_handler (int the_signal) {
/* Windows 3.1 can't do ^C handling */
#ifndef _WINDOWS

#ifdef MULTI_AGENT_ENABLED
  cons * c;
  agent * the_agent;

  global_agent->stop_soar = TRUE;	
  global_agent->reason_for_stopping =  c_interrupt_msg;

  for(c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = ((agent *) c->first);
    the_agent->stop_soar = TRUE;
    the_agent->reason_for_stopping =  c_interrupt_msg;
  }
#else
  current_agent(stop_soar) = TRUE;
  current_agent(reason_for_stopping) = c_interrupt_msg;
#endif

  /* --- reinstall this signal handler -- some brain-damaged OS's uninstall
     it after delivering the signal --- */
  signal (SIGINT, control_c_handler);
#endif
}

void setup_signal_handling (void) {
#ifndef _WINDOWS
  signal (SIGINT, control_c_handler);
/* BUGBUG according to the ANSI standard, we're supposed to check whether
   the result of the signal() call is SIG_ERR--if so, it means that the
   signal handler was not properly set up, and thus ctrl-c won't work.
   I couldn't seem to get this to compile right at CMU or U-M, though.
*/
#endif /* _WINDOWS */
}

/* ===================================================================

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
=================================================================== */
#ifndef NO_TIMING_STUFF
#define ONE_MILLION (1000000)

void reset_timer (struct timeval *tv_to_reset) {
  tv_to_reset->tv_sec = 0;
  tv_to_reset->tv_usec = 0;
}

void get_cputime_from_rusage (struct rusage *r, struct timeval *dest_tv) {
  dest_tv->tv_sec = r->ru_utime.tv_sec + r->ru_stime.tv_sec;
  dest_tv->tv_usec = r->ru_utime.tv_usec + r->ru_stime.tv_usec;
  if (dest_tv->tv_usec >= ONE_MILLION) {
    dest_tv->tv_usec -= ONE_MILLION;
    dest_tv->tv_sec++;
  }
}

void start_timer (struct timeval *tv_for_recording_start_time) {
  struct rusage temp_rusage;
  
  getrusage (RUSAGE_SELF, &temp_rusage);
  get_cputime_from_rusage (&temp_rusage, tv_for_recording_start_time);
}

void stop_timer (struct timeval *tv_with_recorded_start_time,
                 struct timeval *tv_with_accumulated_time) {
  struct rusage end_rusage;
  struct timeval end_tv;
  long delta_sec, delta_usec;
  
  getrusage (RUSAGE_SELF, &end_rusage);
  get_cputime_from_rusage (&end_rusage, &end_tv);

  delta_sec = end_tv.tv_sec - tv_with_recorded_start_time->tv_sec;
  delta_usec = end_tv.tv_usec - tv_with_recorded_start_time->tv_usec;
  if (delta_usec < 0) {
    delta_usec += ONE_MILLION;
    delta_sec--;
  }

  tv_with_accumulated_time->tv_sec += delta_sec;
  tv_with_accumulated_time->tv_usec += delta_usec;
  if (tv_with_accumulated_time->tv_usec >= ONE_MILLION) {
    tv_with_accumulated_time->tv_usec -= ONE_MILLION;
    tv_with_accumulated_time->tv_sec++;
  }
}

double timer_value (struct timeval *tv) {
  return (double)(tv->tv_sec) + (double)(tv->tv_usec)/(double)ONE_MILLION;
}
#endif
/* ===================================================================
   
                            Sysparams

=================================================================== */


void set_sysparam (int param_number, long new_value) {
  if ((param_number < 0) || (param_number > HIGHEST_SYSPARAM_NUMBER)) {
    print ("Internal error: tried to set bad sysparam #: %d\n", param_number);
    return;
  }
  current_agent(sysparams)[param_number] = new_value;
  system_parameter_changed_hook (param_number);
}

void init_sysparams (void) {
  int i;

  for (i=0; i<HIGHEST_SYSPARAM_NUMBER+1; i++) current_agent(sysparams)[i] = 0;
  
  /* --- set all params to zero, except the following: --- */
  current_agent(sysparams)[TRACE_CONTEXT_DECISIONS_SYSPARAM] = TRUE;
  current_agent(sysparams)[TRACE_FIRINGS_OF_CHUNKS_SYSPARAM] = TRUE;
  current_agent(sysparams)[TRACE_FIRINGS_WME_TRACE_TYPE_SYSPARAM] = NONE_WME_TRACE;
  current_agent(sysparams)[TRACE_CHUNK_NAMES_SYSPARAM] = TRUE;
  current_agent(sysparams)[TRACE_JUSTIFICATION_NAMES_SYSPARAM] = TRUE;
  current_agent(sysparams)[MAX_ELABORATIONS_SYSPARAM] = 100;
  current_agent(sysparams)[MAX_CHUNKS_SYSPARAM] = 500;
#ifdef USE_X_DISPLAY
  current_agent(sysparams)[RESPOND_TO_LOAD_ERRORS_SYSPARAM] = FALSE;
#else
  current_agent(sysparams)[RESPOND_TO_LOAD_ERRORS_SYSPARAM] = TRUE;
#endif
  current_agent(sysparams)[LEARNING_ON_SYSPARAM] = TRUE;
  current_agent(sysparams)[LEARNING_SPECIFY_SYSPARAM] = FALSE;  /* AGR MVL1 */
  current_agent(sysparams)[LEARNING_ALL_GOALS_SYSPARAM] = TRUE;
  current_agent(sysparams)[USER_SELECT_MODE_SYSPARAM] = USER_SELECT_FIRST;
  current_agent(sysparams)[PRINT_WARNINGS_SYSPARAM] = TRUE;
  current_agent(sysparams)[PRINT_ALIAS_SYSPARAM] = TRUE;  /* AGR 627 */
}

/* ===================================================================
   
                     Adding and Removing Ptraces

   Productions_being_traced is a (consed) list of all productions
   on which a ptrace has been set.  Ptraces are added/removed via
   calls to add_ptrace() and remove_ptrace().
=================================================================== */
/* list of production structures */


void add_ptrace (production *prod) {
  if (prod->trace_firings) return;
  prod->trace_firings = TRUE;
  push (prod, current_agent(productions_being_traced));
}

production *prod_to_remove_ptrace_of;

bool remove_ptrace_test_fn (cons *c) {
  return (c->first == prod_to_remove_ptrace_of);
}

void remove_ptrace (production *prod) {
  if (! prod->trace_firings) return;
  prod->trace_firings = FALSE;
  prod_to_remove_ptrace_of = prod;
  free_list (extract_list_elements (&current_agent(productions_being_traced),
                                    remove_ptrace_test_fn));
}

/* ===================================================================
   
                        Global Variables, Etc.

=================================================================== */

/* --- list of symbols (problem space names) declared chunk-free --- */


/* --- during firing, this points to the production being fired --- */


/* --- to interrupt at the end of the current phase, set stop_soar to TRUE
   and reason_for_stopping to some appropriate string --- */


/* --- current top level phase --- */


/* --- the RHS action (halt) sets this TRUE --- */


/* ===================================================================
   
                         Reinitializing Soar

   Reset_statistics() resets all the statistics (except the firing counts
   on each individual production).  Reinitialize_soar() does all the 
   work for an init-soar.
=================================================================== */

void reset_statistics (void) {
  current_agent(d_cycle_count) = 0;
  current_agent(e_cycle_count) = 0;
  current_agent(e_cycles_this_d_cycle) = 0;
  current_agent(chunks_this_d_cycle) = 0;
  current_agent(production_firing_count) = 0;
  current_agent(wme_addition_count) = 0;
  current_agent(wme_removal_count) = 0;
  current_agent(max_wm_size) = 0;
  current_agent(cumulative_wm_size) = 0.0;
  current_agent(num_wm_sizes_accumulated) = 0;
#ifndef NO_TIMING_STUFF
  reset_timer (&current_agent(total_cpu_time));
  reset_timer (&current_agent(match_cpu_time));
#endif
#ifdef DETAILED_TIMING_STATS
  reset_timer (&current_agent(ownership_cpu_time));
  reset_timer (&current_agent(chunking_cpu_time));
  reset_timer (&current_agent(preference_phase_cpu_time));
  reset_timer (&current_agent(create_instantiations_cpu_time));
  reset_timer (&current_agent(o_support_cpu_time));
#endif
}

void reinitialize_soar (void) {
  before_init_soar_hook();
  /* BUGBUG ought to temporarily set tracing flags to FALSE so we don't
     get tons of extra info being printed out during the call to
     do_preference_phase(). */
  clear_goal_stack ();
  do_preference_phase ();   /* allow all instantiations to retract */

  reset_explain();
  reset_id_counters ();
  reset_wme_timetags ();
  reset_statistics ();
  current_agent(system_halted) = FALSE;
  current_agent(go_number) = 1;
  current_agent(go_type) = GO_DECISION;
  after_init_soar_hook();
  current_agent(input_cycle_flag) = TRUE;  /* reinitialize flag  AGR REW1 */
}

/* ===================================================================
   
                            Running Soar

   Do_one_top_level_phase() runs Soar one top-level phase.  Note that
   this does not start/stop the total_cpu_time timer--the caller must
   do this.

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
=================================================================== */

void do_one_top_level_phase (void) {
  if (current_agent(system_halted)) {
    print ("\nSystem halted.  Use (init-soar) before running Soar again.");
    current_agent(stop_soar) = TRUE;
    current_agent(reason_for_stopping) = "System halted.";
    return;
  }
  
  if (! current_agent(top_goal)) {
    create_top_goal();
    if (current_agent(sysparams)[TRACE_CONTEXT_DECISIONS_SYSPARAM]) {
      print_string ("\n");
      print_lowest_slot_in_context_stack ();
    }
    current_agent(current_phase) = INPUT_PHASE;
  }

  switch (current_agent(current_phase)) {
  case INPUT_PHASE:
    if (current_agent(e_cycles_this_d_cycle)==0) before_decision_cycle_hook ();
    if (current_agent(input_cycle_flag) == TRUE) {  /* AGR REW1 */
      before_input_phase_hook ();
      do_input_cycle();
      after_input_phase_hook ();
      if (current_agent(input_period)) current_agent(input_cycle_flag) = FALSE;
    }  /* AGR REW1 this line and 1 previous line */
    if (any_assertions_or_retractions_ready())
      current_agent(current_phase) = PREFERENCE_PHASE;
    else
      current_agent(current_phase) = QUIESCENCE_PHASE;
    break;
    
  case PREFERENCE_PHASE:
    before_preference_phase_hook ();
    do_preference_phase();
    after_preference_phase_hook ();
    current_agent(current_phase) = WM_PHASE;
    break;
    
  case WM_PHASE:
    before_wm_phase_hook ();
    do_working_memory_phase();
    after_wm_phase_hook ();
    current_agent(current_phase) = OUTPUT_PHASE;
    break;
    
  case OUTPUT_PHASE:
    before_output_phase_hook ();
    do_output_cycle();
    after_output_phase_hook ();
    current_agent(e_cycle_count)++;
    current_agent(e_cycles_this_d_cycle)++;

    /* MVP 6-8-94 */
    if (current_agent(e_cycles_this_d_cycle) >=
        current_agent(sysparams)[MAX_ELABORATIONS_SYSPARAM]) {
      if (current_agent(sysparams)[PRINT_WARNINGS_SYSPARAM])
        print ("\nWarning: reached max-elaborations; proceeding to quiescence phase.");
      current_agent(current_phase) = QUIESCENCE_PHASE;
    } else
      current_agent(current_phase) = INPUT_PHASE;

    break;
    
  case QUIESCENCE_PHASE:
    current_agent(d_cycle_count)++;
/* AGR REW1 begin */
    if (!current_agent(input_period)) current_agent(input_cycle_flag) = TRUE;
    else
      if ((current_agent(d_cycle_count) % current_agent(input_period)) == 0)
	current_agent(input_cycle_flag) = TRUE;
/* AGR REW1 end */
    before_quiescence_phase_hook ();
    do_quiescence_phase();
    after_quiescence_phase_hook ();
    after_decision_cycle_hook ();
    if (current_agent(sysparams)[TRACE_CONTEXT_DECISIONS_SYSPARAM]) {
      if(current_agent(printer_output_column) != 1)
        print_string ("\n");
      print_lowest_slot_in_context_stack ();
    }
    current_agent(chunks_this_d_cycle) = 0;

    current_agent(e_cycles_this_d_cycle) = 0;
    current_agent(current_phase) = INPUT_PHASE;
    break;
  }

  /* --- update WM size statistics --- */
  if (current_agent(num_wmes_in_rete) > current_agent(max_wm_size)) 
     current_agent(max_wm_size) = current_agent(num_wmes_in_rete);
  current_agent(cumulative_wm_size) += current_agent(num_wmes_in_rete);
  current_agent(num_wm_sizes_accumulated)++;
  
#ifdef __SC__
// 	(void) _EventLoop (0,0);
	{EventRecord e;
	unsigned char key;
	
	if (WaitNextEvent(everyEvent, &e, 0, (RgnHandle)0)) {
		if (e.what == keyDown) {
			key = (unsigned char) e.message;
			if (e.modifiers & cmdKey) {
				if (key == '.') {
					/* process cmd-. (interrupt) event */
					current_agent(system_halted) = TRUE;
				} else if ((key == 'Q') || (key == 'q')) {
					/* process cmd-Q (quit) event */
				}
			}
		} else {
			_DoDebugEvent(0,e);
		}
	}}
#endif

  if (current_agent(system_halted)) {
    current_agent(stop_soar) = TRUE;
    current_agent(reason_for_stopping) = "System halted.";
    after_halt_soar_hook ();
  }
  
  if (current_agent(stop_soar)) print ("\n%s", current_agent(reason_for_stopping));
}

void run_forever (void) {
#ifndef NO_TIMING_STUFF
  start_timer (&current_agent(start_total_tv));
#endif
  current_agent(stop_soar) = FALSE;
  current_agent(reason_for_stopping) = "";
  while (! current_agent(stop_soar)) {
    do_one_top_level_phase();
  }
#ifndef NO_TIMING_STUFF
  stop_timer (&current_agent(start_total_tv), &current_agent(total_cpu_time));
#endif
}

void run_for_n_phases (long n) {
  if (n == -1) { run_forever(); return; }
  if (n < -1) return;
#ifndef NO_TIMING_STUFF
  start_timer (&current_agent(start_total_tv));
#endif
  current_agent(stop_soar) = FALSE;
  current_agent(reason_for_stopping) = "";
  while (!current_agent(stop_soar) && n) {
    do_one_top_level_phase();
    n--;
  }
#ifndef NO_TIMING_STUFF
  stop_timer (&current_agent(start_total_tv), &current_agent(total_cpu_time));
#endif
}

void run_for_n_elaboration_cycles (long n) {
  long e_cycles_at_start, d_cycles_at_start, elapsed_cycles;
  
  if (n == -1) { run_forever(); return; }
  if (n < -1) return;
#ifndef NO_TIMING_STUFF
  start_timer (&current_agent(start_total_tv));
#endif
  current_agent(stop_soar) = FALSE;
  current_agent(reason_for_stopping) = "";
  e_cycles_at_start = current_agent(e_cycle_count);
  d_cycles_at_start = current_agent(d_cycle_count);
  while (!current_agent(stop_soar)) {
    elapsed_cycles = (current_agent(d_cycle_count)-d_cycles_at_start) +
                     (current_agent(e_cycle_count)-e_cycles_at_start);
    if (n==elapsed_cycles) break;
    do_one_top_level_phase();
  }
#ifndef NO_TIMING_STUFF
  stop_timer (&current_agent(start_total_tv), &current_agent(total_cpu_time));
#endif
}

void run_for_n_decision_cycles (long n) {
  long d_cycles_at_start;
  
  if (n == -1) { run_forever(); return; }
  if (n < -1) return;
#ifndef NO_TIMING_STUFF
  start_timer (&current_agent(start_total_tv));
#endif
  current_agent(stop_soar) = FALSE;
  current_agent(reason_for_stopping) = "";
  d_cycles_at_start = current_agent(d_cycle_count);
  while (!current_agent(stop_soar)) {
    if (n==current_agent(d_cycle_count)-d_cycles_at_start) break;
    do_one_top_level_phase();
  }
#ifndef NO_TIMING_STUFF
  stop_timer (&current_agent(start_total_tv), &current_agent(total_cpu_time));
#endif
}

Symbol *attr_of_slot_just_decided (void) {
  if (current_agent(bottom_goal)->id.operator_slot->wmes) return current_agent(operator_symbol);
#ifndef NNPSCM
  if (current_agent(bottom_goal)->id.state_slot->wmes) return current_agent(state_symbol);
  if (current_agent(bottom_goal)->id.problem_space_slot->wmes) return current_agent(problem_space_symbol);
  return current_agent(goal_symbol);
#else
  return current_agent(state_symbol);
#endif
}

void run_for_n_selections_of_slot (long n, Symbol *attr_of_slot) {
  long count;
  bool was_quiescence_phase;
  
  if (n == -1) { run_forever(); return; }
  if (n < -1) return;
#ifndef NO_TIMING_STUFF
  start_timer (&current_agent(start_total_tv));
#endif
  current_agent(stop_soar) = FALSE;
  current_agent(reason_for_stopping) = "";
  count = 0;
  while (!current_agent(stop_soar) && (count < n)) {
    was_quiescence_phase = (current_agent(current_phase)==QUIESCENCE_PHASE);
    do_one_top_level_phase();
    if (was_quiescence_phase)
      if (attr_of_slot_just_decided()==attr_of_slot) count++;
  }
#ifndef NO_TIMING_STUFF
  stop_timer (&current_agent(start_total_tv), &current_agent(total_cpu_time));
#endif
}

void run_for_n_selections_of_slot_at_level (long n,
                                            Symbol *attr_of_slot,
                                            goal_stack_level level) {
  long count;
  bool was_quiescence_phase;
  
  if (n == -1) { run_forever(); return; }
  if (n < -1) return;
#ifndef NO_TIMING_STUFF
  start_timer (&current_agent(start_total_tv));
#endif
  current_agent(stop_soar) = FALSE;
  current_agent(reason_for_stopping) = "";
  count = 0;
  while (!current_agent(stop_soar) && (count < n)) {
    was_quiescence_phase = (current_agent(current_phase)==QUIESCENCE_PHASE);
    do_one_top_level_phase();
    if (was_quiescence_phase) {
      if (current_agent(bottom_goal)->id.level < level) break;
      if (current_agent(bottom_goal)->id.level==level) {
        if (attr_of_slot_just_decided()==attr_of_slot) count++;
      }
    }
  }
#ifndef NO_TIMING_STUFF
  stop_timer (&current_agent(start_total_tv), &current_agent(total_cpu_time));
#endif
}

/* ===================================================================

                     Print the Startup Banner

=================================================================== */

void print_startup_banner (void) {
  /*
   * only show major and minor versions here.. this keeps us from 
   * having to update the tests every time we make a fix.
   */
#ifdef NNPSCM
  print ("Soar %d.%d.%d  NNPSCM\n", MAJOR_VERSION_NUMBER, MINOR_VERSION_NUMBER, MICRO_VERSION_NUMBER);
#else
  print ("Soar %d.%d.%d\n", MAJOR_VERSION_NUMBER, MINOR_VERSION_NUMBER, MICRO_VERSION_NUMBER);
#endif
  print ("\n");
  print ("Bugs and questions should be sent to soar-bugs@cs.cmu.edu\n");
  print ("The current bug-list may be obtained by sending mail to\n");
  print ("soarhack@cs.cmu.edu with the Subject: line \"bug list\".\n");
  print ("\n");
  print ("This software is in the public domain, and is made available AS IS.\n");
  print ("Carnegie Mellon University, The University of Michigan, and\n");
  print ("The University of Southern California/Information Sciences Institute\n");
  print ("make no warranties about the software or its performance, implied\n");
  print ("or otherwise.\n");
  print ("\n");
  print ("Type \"help\" for information on various topics.\n");
  print ("Type \"quit\" to exit.  Use ctrl-c to stop a Soar run.\n");
  print ("Type \"soarnews\" for news.\n");
  print ("Type \"version\" for complete version information.\n");
}

/* ===================================================================
   
             Loading the Initialization File ".init.soar"

   This routine looks for a file ".init.soar" in either the current
   directory or $HOME, and if found, loads it.
=================================================================== */

#ifdef MULTI_AGENT_ENABLED
bool multi_agent_mode = FALSE;
#endif

extern char *getenv();

void load_init_file (void) {
  char filename[MAXPATHLEN];   /* AGR 536 */
  char *home_directory;
  FILE *initfile;

  strcpy (filename, INIT_FILE);
  initfile = fopen (filename, "r");
  if (!initfile) {
#ifdef MULTI_AGENT_ENABLED
      strcpy (filename, MULTI_INIT_FILE);
      initfile = fopen (filename, "r");
      if (initfile) {
         multi_agent_mode = TRUE;
       } else {
#endif
	 home_directory = getenv ("HOME");
	 if (home_directory) {
	   strcpy (filename, home_directory);
	   strcat (filename, "/");
	   strcat (filename, INIT_FILE);
	   initfile = fopen (filename, "r");
#ifdef MULTI_AGENT_ENABLED
	   if (!initfile) {
	     strcpy (filename, home_directory);
	     strcat (filename, "/");
	     strcat (filename, MULTI_INIT_FILE);
	     initfile = fopen (filename, "r");
	     if (initfile) {
	       multi_agent_mode = TRUE;
	     }
	   }
	 }
#endif
       }
    }

  print_startup_banner();

#ifdef MULTI_AGENT_ENABLED
  if (multi_agent_mode) {
    print ("\nMultiple agent mode selected.\n");
  } 
#endif

  if (initfile) {
    print ("\nLoading %s\n",filename);
    load_file (filename, initfile);
    fclose (initfile);
  }
}

/* AGR 536  Soar core dumped when it used filenames longer than 1000 chars
   but shorter than MAXPATHLEN (from sys/param.h).  4-May-94  */

void load_init_multi_file (char * sub_dir) {
  char filename[MAXPATHLEN];        /* AGR 536 */
  char current_dir[MAXPATHLEN];     /* AGR 536 */
  char init_file_dir[MAXPATHLEN];   /* AGR 536 */
  char pathname[MAXPATHLEN];        /* AGR 536 */
  char *home_directory;
  FILE *initfile;

  if (!getwd(current_dir))
    print("Unable to determine current directory while loading init file.\n");

  strcpy (init_file_dir, sub_dir);
  strcpy (filename, sub_dir);
  strcat (filename, INIT_FILE);
  initfile = fopen (filename, "r");
  if (!initfile) {
    home_directory = getenv ("HOME");
    if (home_directory) {
      strcpy (init_file_dir, home_directory);
      strcpy (filename, home_directory);
      strcat (filename, "/");
      strcat (filename, INIT_FILE);
      initfile = fopen (filename, "r");
    }
  }

  if (initfile) {
    print ("\nChanging to directory %s.", init_file_dir);
    chdir (init_file_dir);
    if(getwd(pathname)) strcpy(current_agent(top_dir_stack)->directory, pathname);
    print ("\nLoading %s\n",INIT_FILE);
    load_file (filename, initfile);
    fclose (initfile);
    print ("Returning from directory %s.\n", init_file_dir);
    chdir (current_dir);
    if(getwd(pathname)) strcpy(current_agent(top_dir_stack)->directory, pathname);
  }
}


/* ===================================================================
   
                           Initialization Function

=================================================================== */

#ifdef MULTI_AGENT_ENABLED
agent *soar_agent;

list *all_soar_agents = NIL;

agent *global_agent;
#endif

void init_soar_agent(void) {

  /* --- initialize everything --- */
  init_memory_utilities();
  init_symbol_tables();
  create_predefined_symbols();
  init_production_utilities();
  init_built_in_rhs_functions ();
  init_rete ();
  init_lexer ();
  init_firer ();
  init_decider ();
  init_soar_io ();
  init_text_io ();
  init_chunker ();
  init_sysparams ();
  init_tracing ();
  init_explain();  /* AGR 564 */


  /* --- add default object trace formats --- */
  add_trace_format (FALSE, FOR_ANYTHING_TF, NIL,
                    "%id %ifdef[(%v[name])]");
  add_trace_format (FALSE, FOR_GOALS_TF, NIL,
                    "%id %ifdef[(%v[attribute] %v[impasse])]");
  { Symbol *evaluate_object_sym;
    evaluate_object_sym = make_sym_constant ("evaluate-object");
    add_trace_format (FALSE, FOR_OPERATORS_TF, evaluate_object_sym,
                      "%id (evaluate-object %o[object])");
    symbol_remove_ref (evaluate_object_sym);
  }
  /* --- add default stack trace formats --- */
#ifndef NNPSCM
  add_trace_format (TRUE, FOR_GOALS_TF, NIL,
                    "%right[6,%dc]: %rsd[   ]==>G: %cg");
  add_trace_format (TRUE, FOR_PROBLEM_SPACES_TF, NIL,
                    "%right[6,%dc]: %rsd[   ]   P: %cp");
  add_trace_format (TRUE, FOR_STATES_TF, NIL,
                    "%right[6,%dc]: %rsd[   ]   S: %cs");
#else
  add_trace_format (TRUE, FOR_GOALS_TF, NIL,
                    "%right[6,%dc]: %rsd[   ]==>S: %cg");
#endif
  add_trace_format (TRUE, FOR_OPERATORS_TF, NIL,
                    "%right[6,%dc]: %rsd[   ]   O: %co");
  reset_statistics ();
  after_init_agent_hook ();
}

int agent_counter = -1;
int agent_count = -1;

#ifdef MULTI_AGENT_ENABLED
agent *
#else
void
#endif
create_soar_agent (char * agent_name) {

  int i;                                          /* loop index */
  char cur_path[MAXPATHLEN];   /* AGR 536 */

#ifdef MULTI_AGENT_ENABLED
  agent * curr_agent;
  agent * this_agent;

  this_agent = (agent *) malloc(sizeof(agent));  

  curr_agent = soar_agent;
  soar_agent = this_agent;
#endif

  agent_counter++;
  agent_count++;

  current_agent(name)                               = savestring(agent_name);
  current_agent(index_num)                          = agent_counter;

  /* mvp 5-17-94 */
  current_agent(variables_set)                      = NIL;

#ifdef _WINDOWS
  current_agent(current_line)[0]		    = 0;
  current_agent(current_line_index)		    = 0;
#endif /* _WINDOWS */
  /* String redirection */
  current_agent(using_output_string)		    = FALSE;
  current_agent(using_input_string)		    = FALSE;
  current_agent(output_string)			    = NIL;
  current_agent(input_string)			    = NIL;

  current_agent(alias_list)                         = NIL;  /* AGR 568 */
  current_agent(all_wmes_in_rete)                   = NIL;
  current_agent(alpha_mem_id_counter)               = 0;
  current_agent(backtrace_number)                   = 0;
  current_agent(beta_node_id_counter)               = 0;
  current_agent(bottom_goal)                        = NIL;
  current_agent(changed_slots)                      = NIL;
  current_agent(chunk_count)                        = 1;
  current_agent(chunk_free_problem_spaces)          = NIL;
  current_agent(chunky_problem_spaces)              = NIL;  /* AGR MVL1 */
  current_agent(context_slots_with_changed_acceptable_preferences) = NIL;
  current_agent(current_file)                       = NIL;
  current_agent(current_phase)                      = INPUT_PHASE;
  current_agent(current_symbol_hash_id)             = 0;
  current_agent(current_variable_gensym_number)     = 0;
  current_agent(current_wme_timetag)                = 1;
  current_agent(default_print_depth)                = 1;  /* AGR 646 */
  current_agent(disconnected_ids)                   = NIL;
  current_agent(dummy_matches_node_tokens)          = NIL;
  current_agent(existing_output_links)              = NIL;
  current_agent(go_number)                          = 1;
  current_agent(go_type)                            = GO_DECISION;
  current_agent(grounds_tc)                         = 0;
  current_agent(highest_goal_whose_context_changed) = NIL;
  current_agent(ids_with_unknown_level)             = NIL;
  current_agent(input_functions)                    = NIL;
  current_agent(input_period)                       = 0;     /* AGR REW1 */
  current_agent(input_cycle_flag)                   = TRUE;  /* AGR REW1 */
  current_agent(justification_count)                = 1;
  current_agent(lex_alias)                          = NIL;  /* AGR 568 */
  current_agent(link_update_mode)                   = UPDATE_LINKS_NORMALLY;
  current_agent(locals_tc)                          = 0;
  current_agent(logging_to_file)                    = FALSE;
  current_agent(max_chunks_reached)                 = FALSE; /* MVP 6-24-94 */
  current_agent(mcs_counter)                        = 1;
  current_agent(memory_pools_in_use)                = NIL;
  current_agent(ms_assertions)                      = NIL;
  current_agent(ms_retractions)                     = NIL;
  current_agent(num_existing_wmes)                  = 0;
  current_agent(num_wmes_in_rete)                   = 0;
  current_agent(output_functions)                   = NIL;
  current_agent(potentials_tc)                      = 0;
  current_agent(prev_top_state)                     = NIL;
  current_agent(print_prompt_flag)                  = TRUE;
  current_agent(printer_output_column)              = 1;
  current_agent(production_being_fired)             = NIL;
  current_agent(productions_being_traced)           = NIL; 
  current_agent(promoted_ids)                       = NIL;
  current_agent(reason_for_stopping)                = "Startup";
  current_agent(redirecting_to_file)                = FALSE;
  current_agent(slots_for_possible_removal)         = NIL;
  current_agent(stop_soar)                          = TRUE;           
  current_agent(system_halted)                      = FALSE;
  current_agent(text_input_link)                    = NIL;
  current_agent(text_io_symbol_to_file_mappings)    = NIL;
  current_agent(token_additions)                    = 0;
  current_agent(token_deletions)                    = 0;
  current_agent(top_dir_stack)                      = NIL;   /* AGR 568 */
  current_agent(top_goal)                           = NIL;
  current_agent(top_state)                          = NIL;
  current_agent(wmes_to_add)                        = NIL;
  current_agent(wmes_to_remove)                     = NIL;
  current_agent(multi_attributes)                   = NIL;
  if(!getwd(cur_path))
    print("Unable to set current directory while initializing agent.\n");
  current_agent(top_dir_stack) = (dir_stack_struct *) malloc(sizeof(dir_stack_struct));   /* AGR 568 */
  current_agent(top_dir_stack)->directory = (char *) malloc(MAXPATHLEN*sizeof(char));   /* AGR 568 */
  current_agent(top_dir_stack)->next = NIL;   /* AGR 568 */
  strcpy(current_agent(top_dir_stack)->directory, cur_path);   /* AGR 568 */

  for (i=0; i<NUM_PRODUCTION_TYPES; i++) {  
    current_agent(all_productions_of_type)[i] = NIL;
    current_agent(num_productions_of_type)[i] = 0;
  }

  queue_create( &(current_agent(text_input_queue)));

  init_soar_agent();

#ifdef MULTI_AGENT_ENABLED
  soar_agent = curr_agent;

  scheduler_cycle_count = 0;

  return this_agent;
#endif

}

#ifdef MULTI_AGENT_ENABLED
void
destroy_soar_agent (agent * delete_agent)
{
  cons  * c;
  cons  * prev;
  agent * the_agent;
  agent * prev_agent;
  void  * queued_string;
  bool    strings_to_delete;

/* AGR 601 begin */
  /* If about to destroy current agent, restore control agent at end */
  if (soar_agent == delete_agent)
      prev_agent = global_agent;
  else
      prev_agent = soar_agent;
/* AGR 601 end */
  soar_agent = delete_agent;

  print("\nDestroying agent %s.\n", delete_agent->name);  /* AGR 532 */

/* AGR 610 begin */
#ifdef USE_X_DISPLAY
  /* Free text I/O queue */
  while (!queue_is_empty(delete_agent->text_input_queue))
    {
      strings_to_delete = queue_delete(delete_agent->text_input_queue, 
				       &queued_string);
      free_memory_block_for_string(queued_string);
    }

  free(delete_agent->text_input_queue);
/* AGR 610 end */

  /* Destroy X window associated with agent */
  destroy_agent_window (delete_agent);
#endif /* USE_X_DISPLAY */

  /* Splice agent structure out of global list of agents. */
  for (c = all_soar_agents; c != NIL; c = c->rest) {  
    the_agent = (agent *) c->first;
    if (the_agent == delete_agent) {
      if (c == all_soar_agents) {
	all_soar_agents = c->rest;
      } else {
	prev->rest = c->rest;
      }
      break;
    }
    prev = c;
  }

  /* Free structures stored in agent structure */
  free(delete_agent->name);

  /* KNOWN MEMORY LEAK! Need to track down and free ALL structures */
  /* pointed to be fields in the agent structure.                  */

  /* Free soar agent structure */
  free(delete_agent);
 
  agent_count--;

  soar_agent = prev_agent;

}
#endif


void init_soar (void)
{

#ifdef MULTI_AGENT_ENABLED
  global_agent = soar_agent = 
#endif
        create_soar_agent(GLOBAL_AGENT_NAME);

#ifdef USE_X_DISPLAY
  create_global_display();
#endif

  /* --- set the random number generator seed to a "random" value --- */
  {
#ifndef NO_TIMING_STUFF
	struct timeval tv;
	gettimeofday (&tv, NIL);
#if defined(__hpux) || defined(_WINDOWS)
	srand(tv.tv_usec);
#else
	srandom(tv.tv_usec);
#endif
#else
#ifdef THINK_C || defined(_WINDOWS)
	srand(time(0));
#else
	srandom(time(0));
#endif
#endif
   }

  setup_signal_handling();
  init_built_in_commands ();
  init_explain ();
  init_parser ();
  system_startup_hook();
  load_init_file ();

}


int terminate_soar (void)
{
#ifdef MULTI_AGENT_ENABLED
  free(soar_agent);
#endif
  exit_soar();  
  return 0; /* unreachable, but without it, gcc -Wall warns here */
}
