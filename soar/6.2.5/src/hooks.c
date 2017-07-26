/* =====================================================================
                      Hook Routines for Soar 6

   These routines are called from Soar 6 when certain events happen.
   They are provided so users can add their own code to Soar 6 without
   having to modify the main Soar 6 source code.
===================================================================== */

#include "soar.h"
#include "explain.h"         /* AGR 564 */

/* --------------------------------------------------------------------
                         System Startup Hook

   This function is called only once, at system startup time.  Typically,
   it should be used to set up any Soar I/O routines and install any
   interface commands.  This function is called after most of the system 
   has been initialized, but before ".init.soar" is loaded. (In 
   multi-agent mode, this applies to the global agent "control".)
-------------------------------------------------------------------- */

void system_startup_hook (void) {
}

/* --------------------------------------------------------------------
                       System Termination Hook

   This function is called only once, just before the system exits back
   to the shell.  The parameter normal_exit is TRUE if the system is
   exiting normally, FALSE if the exit is happening because some fatal
   error situation was detected.  Typically, this hook routine should
   do any final cleanup (closing files, etc.) necessary.
-------------------------------------------------------------------- */

void system_termination_hook (bool normal_exit) {
}


/* --------------------------------------------------------------------
                         Agent Initialization Hook

   This function is called only once per each agent after the agent
   has been initialized.  Typically, it should be used to set up any 
   Soar I/O routines and install any user-RHS functions.  This 
   function is called after most of the agent data structures
   have been initialized, but before the ".init.soar" is loaded.

-------------------------------------------------------------------- */

void after_init_agent_hook (void) {
}

/* --------------------------------------------------------------------
                    Before and After Init Soar Hooks

   These functions are called just before and after, respectively, any
   init-soar is done.  (This includes not only the (init-soar) command,
   but also (excise-task) and (excise-all), which do an init-soar.)
-------------------------------------------------------------------- */

void before_init_soar_hook (void) {
}

void after_init_soar_hook (void) {
}

/* --------------------------------------------------------------------
                      After Halt Soar Hook

   This function is called after Soar halts; i.e., after the preference
   phase in which the RHS function (halt) is executed.
-------------------------------------------------------------------- */

void after_halt_soar_hook (void) {
}

/* --------------------------------------------------------------------
                  Before and After Decision Cycle Hooks

   These functions are called at the start and end, respectively, of
   each decision cycle.
-------------------------------------------------------------------- */

void before_decision_cycle_hook (void) {
}

void after_decision_cycle_hook (void) {
}

/* --------------------------------------------------------------------
                  Before and After Input Phase Hooks

   These functions are called just before and after, respectively, the
   Soar I/O input cycle is executed.  (They are called even if the input
   cycle is effectively null because there is no top state.)
-------------------------------------------------------------------- */

void before_input_phase_hook (void) {
}

void after_input_phase_hook (void) {
}

/* --------------------------------------------------------------------
                Before and After Preference Phase Hooks

   These functions are called just before and after, respectively, each
   preference phase.
-------------------------------------------------------------------- */

void before_preference_phase_hook (void) {
}

void after_preference_phase_hook (void) {
}

/* --------------------------------------------------------------------
                Before and After Working Memory Phase Hooks

   These functions are called just before and after, respectively, each
   working memory phase.
-------------------------------------------------------------------- */

void before_wm_phase_hook (void) {
}

void after_wm_phase_hook (void) {
}

/* --------------------------------------------------------------------
                  Before and After Output Phase Hooks

   These functions are called just before and after, respectively, each
   Soar I/O output cycle.  (They are called even if there is no top state
   or there are no output links on the top state.)
-------------------------------------------------------------------- */

void before_output_phase_hook (void) {
}

void after_output_phase_hook (void) {
}

/* --------------------------------------------------------------------
                  Before and After Quiescence Phase Hooks

   These functions are called just before and after, respectively, each
   quiescence phase.
-------------------------------------------------------------------- */

void before_quiescence_phase_hook (void) {
}

void after_quiescence_phase_hook (void) {
#ifdef _WINDOWS
	char *str;
	if (soar_is_halted(&str)) {
	  current_agent(stop_soar)=TRUE;
	  current_agent(reason_for_stopping)=str;
	}
#endif
}

/* --------------------------------------------------------------------
                          WM Changes Hook

   This function is called just before changes are made to working memory.
   It is called with two arguments:  a list (consed) of wmes that are
   about to be added to WM, and a list (consed) of wmes that are about
   to be removed from WM.
-------------------------------------------------------------------- */

void wm_changes_hook (list *wmes_being_added, list *wmes_being_removed) {
}

/* --------------------------------------------------------------------
                      Create New Context Hook
                               and
                       Pop Context Stack Hook

   Create_new_context_hook() is called after a new goal context is created.
   Its argument is a pointer to the new goal identifier (which is equal
   to the global variable bottom_goal).

   Pop_context_stack_hook() is called just before the context stack is 
   popped.  Its argument is a pointer to the identifier of the goal about
   to be removed (which is equal to the global variable bottom_goal).  If
   the stack is popped k levels at once, this routine is called k times in
   bottom-up order.
-------------------------------------------------------------------- */

void create_new_context_hook (Symbol *new_goal) {
}

void pop_context_stack_hook (Symbol *goal_about_to_be_removed) {
}

/* --------------------------------------------------------------------
                   Create New Attribute Impasse Hook
                               and
                     Remove Attribute Impasse Hook

   These functions are called just after an attribute impasse is created
   and just before an attribute impasse is removed, respectively.  The
   argument is a pointer to the impassed slot.
-------------------------------------------------------------------- */

void create_new_attribute_impasse_hook (slot *slot_with_new_impasse) {
}

void remove_attribute_impasse_hook (slot *slot_with_impasse_to_be_removed) {
}

/* --------------------------------------------------------------------
                   Production Just Added Hook
                               and
              Production About to be Excised Hook

   These functions are called just after a production (including chunks
   and justifications) is added to the system, and just before a production
   is excised from the system.  The argument is a pointer to the production
   structure.
-------------------------------------------------------------------- */

void production_just_added_hook (production *new_prod) {
#ifdef _WINDOWS
	add_production_to_stat_lists(new_prod);
#endif
}

void production_about_to_be_excised_hook (production *prod_to_be_excised) {
#ifdef _WINDOWS
	remove_production_from_stat_lists(prod_to_be_excised);
#endif
}

/* --------------------------------------------------------------------
                     Firing and Retraction Hooks

   These functions are called after every production firing and 
   before every production retraction, respectively.  The argument is
   a pointer to the newly created instantiation or the existing
   instantiation about to be retracted.
-------------------------------------------------------------------- */

void firing_hook (instantiation *inst) {
}

void retraction_hook (instantiation *inst) {
}

/* --------------------------------------------------------------------
                   System Parameter Changed Hook

   This functions is called after any change to one of the global system
   parameters (e.g., learn on/off).  See soar.h for a list of these
   system parameters.  The argument to the functions indicates which
   parameter is being changed.  This functions should examine the new
   value of the parameter by looking at the appropriate global variable.
   (For most parameters, this means looking at the sysparams[] array.)
-------------------------------------------------------------------- */

void system_parameter_changed_hook (int param_num) {
}



