/*
 * $Id: ma_interface.c,v 1.9 1994/11/23 16:40:34 rempel Exp $
 * $Log: ma_interface.c,v $
 * Revision 1.9  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.8  1994/08/23  10:35:22  portelli
 * For 6.2.4
 *
 * Revision 1.7  1994/06/01  18:06:37  rempel
 * added a newline to a print statement
 *
 * Revision 1.6  1994/05/06  20:28:27  rempel
 * *** empty log message ***
 *
 * Revision 1.5  1994/03/15  17:28:50  rempel
 * fixes bugs #529, 532, 533, and the '=' -> '==' problem
 *
 * Revision 1.4  93/12/01  14:38:13  portelli
 * Bug fix for 6.1.1
 * 
 * Revision 1.3  93/11/30  13:29:14  portelli
 * Bug fix for 6.1.1
 * 
 * Revision 1.2  93/11/21  17:00:13  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:48:16  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:21:37  jtraub
 * 6.1_checkin
 *
 * Revision 9.1  1993/06/15  18:04:46  jtraub
 * Split these routines out of interface.c so that mac could continue
 * to compile.
 *
 */

/* =================================================================
                          ma_interface.c
      Interface routines for multi-agent soar mode
================================================================= */

#ifdef __hpux
#define _INCLUDE_POSIX_SOURCE
#define _INCLUDE_XOPEN_SOURCE
#define _INCLUDE_HPUX_SOURCE
#include <sys/types.h>
#undef  _INCLUDE_POSIX_SOURCE
#undef  _INCLUDE_XOPEN_SOURCE
#undef _STRUCT_TIMEVAL
#endif /* __hpux */
#ifndef THINK_C
#include <sys/time.h>       /* used for "time" command */
#include <sys/resource.h>
#endif
#ifdef __hpux
#undef _INCLUDE_HPUX_SOURCE
#define _STRUCT_TIMEVAL
#endif /* __hpux */
#include <time.h>
#include "soar.h"
#include "scheduler.h"

#ifdef __hpux
#include <sys/syscall.h>
#include <unistd.h>
#define getrusage(a, b) syscall(SYS_GETRUSAGE, a, b)
#define getwd(arg) getcwd(arg, (size_t) 9999)
#endif /* __hpux */

#ifdef MULTI_AGENT_ENABLED
/* -------------------------------------------------------------------
   
                          "Create Agent" Command

   Syntax:  (create-agent agent-name [agent-display-type])
------------------------------------------------------------------- */

char *help_on_create_agent[] = {
"Command: create-agent",
"",
"Syntax: (create-agent agent-name [agent-display-type])",
"",
"This command creates a single soar agent identified by the given",
"name.  The optional agent-display-type is used to select a set",
"of X resources that define the display characteristics of the",
"agent's window.  The agent-display-type is used to form the",
"prefix \"Soar.<agent-display-type>\" used in retrieving X",
"resources.  For example, if you had an agent-display-type of",
"Friendly, then the border color of the window would be set by",
"finding the resource \"Soar.Friendly.borderColor\".  You can",
"create new resources by editing the XSoar file of X resources.",
"The agent-display-type is valid only when using the X version",
"of Soar.",
0 };

bool create_agent_interface_routine (void) {
  char sub_dir[MAXPATHLEN];    /* AGR 536 */
  char agent_name[100];
  char display_type[100];
  agent * first_agent;
  agent * new_agent;
  cons * c;
  agent * the_agent;
  bool agent_already_defined;

  first_agent = soar_agent;

  get_lexeme();  /* Consume "create-agent", advance to agent name */

  if (current_agent(lexeme).type != SYM_CONSTANT_LEXEME) {
    print("Expected agent name.\n");
    print_location_of_most_recent_lexeme();    
    return FALSE;
  }

  agent_already_defined = FALSE;
  for (c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = (agent *) c->first;
    if (!strcmp(the_agent->name, current_agent(lexeme).string)) {
      agent_already_defined = TRUE;
    }
  }

  if (!strcmp(global_agent->name, current_agent(lexeme).string)) {
    agent_already_defined = TRUE;
  }

  if (agent_already_defined) {
    print ("\nError: Agent %s ", current_agent(lexeme).string);
    print ("already defined, duplicate declaration ignored.\n");
    print_location_of_most_recent_lexeme();
    return FALSE;
  } else {
    print("\nCreating agent %s.\n", current_agent(lexeme).string);
    strcpy (sub_dir, current_agent(lexeme).string);
    strcat (sub_dir, "/");

    strcpy(agent_name, current_agent(lexeme).string);
    
    get_lexeme();
   
    if (current_agent(lexeme).type == R_PAREN_LEXEME)
	display_type[0] = '\0';
    else
      {
	if (current_agent(lexeme).type != SYM_CONSTANT_LEXEME) 
	  {
	    print("agent-display-type not a symbolic constant, ");
	    print("using default instead.\n");
	    display_type[0] = '\0';
	  } 
	else
	  {
	    strcpy(display_type, current_agent(lexeme).string);
	  }
      }

    /* Read remaining tokens to avoid a hang caused by tokens
       left in x_input_buffer intended for the creating agent
       but read by the created agent during the load_file 
       below. */

    if (current_agent(lexeme).type != R_PAREN_LEXEME)
      get_lexeme();

    while (current_agent(lexeme).type != R_PAREN_LEXEME)
      {
	print("Ignoring extra token: %s\n", current_agent(lexeme).string);
	get_lexeme();
      }

    soar_agent = create_soar_agent(agent_name);

#ifdef USE_X_DISPLAY
    create_agent_window(soar_agent, display_type);
#else
    if (display_type[0] != '\0')
      print("agent-display-type argument only useable with X window version.\n");
#endif

    load_init_multi_file(sub_dir);

#ifdef USE_X_DISPLAY
    print_agent_prompt(soar_agent);
#endif

    new_agent = soar_agent;
    soar_agent = first_agent;
    push(new_agent, all_soar_agents);

    if (current_agent(lexeme).type != R_PAREN_LEXEME)
      get_lexeme(); /* consume this one, advance to next agent name */

    all_soar_agents = destructively_reverse_list (all_soar_agents);

    return TRUE;
  }
}


#ifdef TRUE /* This command is due to be eliminated.  Its only
             * commented out here in case someone has some last
             * minute reservations.  This was done in the 6.1.0
             * version.
             */
/* -------------------------------------------------------------------
   
                          "Create Agents" Command

   Syntax:  (create-agents agent-name*)
------------------------------------------------------------------- */

char *help_on_create_agents[] = {
"Command: create-agents",
"",
"Syntax: (create-agents agent-name*)",
"",
"This command creates soar agents identified by the given names",
0 };

bool create_agents_interface_routine (void) {
  char sub_dir[MAXPATHLEN];     /* AGR 536 */
  int agent_count_in_command = 0;
  agent * first_agent;
  agent * new_agent;
  cons * c;
  agent * the_agent;
  bool agent_already_defined;

  first_agent = soar_agent;

  get_lexeme();  /* consume "create-agents", advance to agent name(s) */
  while (current_agent(lexeme).type!=R_PAREN_LEXEME) {

    agent_already_defined = FALSE;
    for (c = all_soar_agents; c != NIL; c = c->rest) {
      the_agent = (agent *) c->first;
      if (!strcmp(the_agent->name, current_agent(lexeme).string)) {
         agent_already_defined = TRUE;
       }
    }

    if (!strcmp(global_agent->name, current_agent(lexeme).string)) {
      agent_already_defined = TRUE;
    }

    if (agent_already_defined) {
      print ("\nError: Agent %s ", current_agent(lexeme).string);
      print ("already defined, duplicate declaration ignored.\n");
      print_location_of_most_recent_lexeme();
    } else {
      print("\nCreating agent %s.\n", current_agent(lexeme).string);
      strcpy (sub_dir, current_agent(lexeme).string);
      strcat (sub_dir, "/");

      soar_agent = create_soar_agent(current_agent(lexeme).string);

#ifdef USE_X_DISPLAY
      create_agent_window(soar_agent, "");
#endif

      load_init_multi_file(sub_dir);

#ifdef USE_X_DISPLAY
      print_agent_prompt(soar_agent);
#endif

      agent_count_in_command++;

      new_agent = soar_agent;
      soar_agent = first_agent;
      push(new_agent, all_soar_agents);
    }
    get_lexeme(); /* consume this one, advance to next agent name */
  }

  if (agent_count_in_command == 0) {
    print("Warning! No agents defined in define-agents command.\n");
    if (agent_count == 0) {
      print("Switching to single agent mode.\n");
      multi_agent_mode = FALSE;
    }
  } else {
    all_soar_agents = destructively_reverse_list (all_soar_agents);
  }

  return TRUE;
}

#endif /* Commenting out create-agents command */

/* -------------------------------------------------------------------
                           "Destroy Agent" Command

   Syntax:  (destroy-agent agent-name)
------------------------------------------------------------------- */

char *help_on_destroy_agent[] = {
"Command: destroy-agent",
"",
"Syntax: (destroy-agent agent-name)",
"",
"Remove agent from Soar.  If this command is issued in the window",
"of the agent being destroyed, then control switches to the",
"global control agent.",
0 };

bool destroy_agent_interface_routine (void) {
  cons * c;
  
  get_lexeme();  /* consume "destroy-agent" */

  if (current_agent(lexeme).type == SYM_CONSTANT_LEXEME) {
    for(c = all_soar_agents; c != NIL; c = c->rest) {
      if (!strcmp(current_agent(lexeme).string,
		  ((agent *)c->first)->name)) {
        get_lexeme(); 

	if (soar_agent == (agent *) c->first)
	  { 
	    global_agent->lexeme = current_agent(lexeme);
	    soar_agent = global_agent;
	  }	

	destroy_soar_agent((agent *) c->first);
        return TRUE;
      }
    }

    if(!strcmp(current_agent(lexeme).string, "control")) {
      print ("The global control agent cannot be destroyed.\n");
    } else {
      if (strcmp(current_agent(lexeme).string, ")")) {
	print ("Unknown agent name.\n");
      } else {
	print ("Expected agent name.\n");
      }
    }
  } else {
    print ("Expected agent name.\n");
  }
  print_location_of_most_recent_lexeme();
  if (strcmp(current_agent(lexeme).string, ")")) {
    get_lexeme(); 
  }
  return FALSE;      
}

/* -------------------------------------------------------------------
   
                          "Agent Go" Command

   Syntax:  (agent-go agent-name [integer | 'forever'] [type])
            type ::= p | e | d | g | ps | s | o | context-variable
------------------------------------------------------------------- */

char *help_on_agent_go[] = {
"Command: agent-go",
"",
"Syntax: (agent-go agent_name [integer | 'forever'] [type])",
"        type ::= 'p' | 'e' | 'd' | 'g' | 'ps' | 's' | 'o' | context-variable",
"",
"This command operates exactly the same as the go command, EXCEPT that",
"this only defines the go settings for a agent when it is selected to",
"run by the multi-agent scheduler.  Hence, the go settings are defined",
"for subsequent runs, but no runs are made when the command is read."
"",
"See also:  go, d, run",
0 };

bool agent_go_interface_routine (void) {
  cons  * c;
  agent * the_agent;
  agent * curr_agent;
  long               prev_go_number;
  Symbol           * prev_go_slot_attr;
  goal_stack_level   prev_go_slot_level;
  enum go_type_enum  prev_go_type;
  bool the_result;

  get_lexeme();

  for (c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = (agent *) c->first;
    if (!strcmp(the_agent->name, current_agent(lexeme).string)) {
      if(the_agent != soar_agent) {
        prev_go_number     = current_agent(go_number);
        prev_go_slot_attr  = current_agent(go_slot_attr);
        prev_go_slot_level = current_agent(go_slot_level);
        prev_go_type       = current_agent(go_type);

        current_agent(go_number)     = the_agent->go_number;
        current_agent(go_slot_attr)  = the_agent->go_slot_attr;
        current_agent(go_slot_level) = the_agent->go_slot_level;
        current_agent(go_type)       = the_agent->go_type;
      }

      the_result = parse_go_command();

      if (the_result && the_agent != soar_agent) {
	the_agent->go_number     = current_agent(go_number);
	the_agent->go_slot_attr  = current_agent(go_slot_attr);
	the_agent->go_slot_level = current_agent(go_slot_level);
	the_agent->go_type       = current_agent(go_type);
      }

      if(the_agent != soar_agent) {
        current_agent(go_number)     = prev_go_number;
        current_agent(go_slot_attr)  = prev_go_slot_attr;
        current_agent(go_slot_level) = prev_go_slot_level;
        current_agent(go_type)       = prev_go_type;
      }
      return the_result;
    }
  }  

  print ("Agent %s is not known.  Ignoring command.\n", 
	 current_agent(lexeme).string);

  print_location_of_most_recent_lexeme();
  if (strcmp(current_agent(lexeme).string, ")")) {
    get_lexeme(); 
  }
  return FALSE;      
}


/* -------------------------------------------------------------------
   
                           "schedule" Command

   Syntax:  (schedule [cycles])
------------------------------------------------------------------- */

char *help_on_schedule[] = {
"Command: schedule",
"",
"Syntax: (schedule [cycles])",
"",
"Schedule agents in a multi-agent soar setting.  If given, cycles",
"indicates how many cycles to run the scheduler.  If not given, the",
"scheduler runs until interrupted with a control-c or all agents",
"have completed processing.",
0 };

bool schedule_interface_routine (void) {
  struct lexeme_info last_lexeme;
  cons * c;
  agent * the_agent;
  int cycles = -1;
  
  get_lexeme();  /* consume "schedule" */

  if (!multi_agent_mode) {
    print("Warning! Only one agent is known. Scheduling single agent only.\n");
  }

  if (current_agent(lexeme).type==INT_CONSTANT_LEXEME) {
    cycles = current_agent(lexeme).int_val;
    scheduler_cycle_count = cycles;
    get_lexeme();  
  } else {
    scheduler_cycle_count = -1;
  }

  last_lexeme = current_agent(lexeme); 

  schedule_agents(cycles);

#ifdef USE_X_DISPLAY  
  for (c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = (agent *) c->first;
    if (the_agent != soar_agent) {
    print_agent_prompt (the_agent);
    }
  }
#endif

  current_agent(lexeme) = last_lexeme;
  return TRUE;
}

/* -------------------------------------------------------------------
                           "Select Agent" Command

   Syntax:  (select-agent agent-name)
------------------------------------------------------------------- */

#ifndef USE_X_DISPLAY

char *help_on_select_agent[] = {
"Command: select-agent",
"",
"Syntax: (select-agent agent-name)",
"",
"Select agent to receive commands",
0 };

bool select_agent_interface_routine (void) {
  cons * c;
  
  get_lexeme();  /* consume "select-agent" */

  if (current_agent(lexeme).type == SYM_CONSTANT_LEXEME) {
    for(c = all_soar_agents; c != NIL; c = c->rest) {
      if (!strcmp(current_agent(lexeme).string,
		  ((agent *)c->first)->name)) {
        get_lexeme(); 

/*  AGR 533
        if (((agent *)c->first)->lexeme.type == EOF_LEXEME)
           print ("\nSoar agent %s> ", ((agent *)c->first)->name);
*/

	((agent *)c->first)->lexeme = current_agent(lexeme); 
	soar_agent = (agent *)c->first;

/* Here is where I make my first bug fix.  This is to fix bug 533, the
   missing prompt bug reported by Mike Hucka.  I will endeavour to comment
   my changes to the source code, in the hopes that they will be
   understandable to future maintainers.  I have found the existing source
   code to be only marginally understandable.
   My fix consists of one statement, which follows this comment.  I put it in
   because for some reason, the "soar_agent = (agent *)c->first;" statement
   immediately above changes current_agent(current_char) from '\n' to ' ',
   which causes the next prompt (in lexer.c) to not print out.  So it's a
   bit of a hack, but in that respect it seems to fit in with the rest of
   this code.  AGR 94.03.15  */
/* With this change, there were instances where 2 prompts were printed out,
   so I have commented out above where the other prompt gets printed out.
   AGR 94.03.15  */
/* Note that I am trying to implement some kind of standard notation for
   fixing bugs.  For now, I will attach a comment to every fix I make.
   This comment will be in the form "AGR x" where AGR are my initials and
   x is a number which corresponds to the bug number.  Allan G Rempel. */

	current_agent(current_char) = '\n';     /* AGR 533 */

        return TRUE;
      }
    }
    if(!strcmp(current_agent(lexeme).string, "control")) {
      get_lexeme();
      global_agent->lexeme = current_agent(lexeme);
      soar_agent = global_agent;
      return TRUE;
    }
    if (strcmp(current_agent(lexeme).string, ")")) {
      print ("Unknown agent name.\n");
    } else {
      print ("Expected agent name.\n");
    }
  } else {
    print ("Expected agent name.\n");
  }
  print_location_of_most_recent_lexeme();
  if (strcmp(current_agent(lexeme).string, ")")) {
    get_lexeme(); 
  }
  return FALSE;      
}

#else /* USE_X_DISPLAY is defined */

/* -------------------------------------------------------------------
   
                          "Monitor" Command

   Syntax:  (monitor "command")
------------------------------------------------------------------- */

char *help_on_monitor[] = {
"Command: monitor",
"",
"Syntax: (monitor \"command\")",
"",
"Monitor displays the results of executing the given command after",
"each agent-go cycle.",
0 };

bool monitor_interface_routine (void) {
  
  get_lexeme();  /* consume "monitor", advance to quoted file name */
  if (current_agent(lexeme).type!=QUOTED_STRING_LEXEME) {
    print ("Expected string in quotes for command to monitor\n");
    print_location_of_most_recent_lexeme();
    return FALSE;
  }

  create_monitor_window(soar_agent, current_agent(lexeme).string);

  strcpy(soar_agent->monitor->input_buffer, current_agent(lexeme).string);
  strcat(soar_agent->monitor->input_buffer, "\n");

  get_lexeme();  /* consume command, advance to rparen */
  return TRUE;
}

#endif /* USE_X_DISPLAY */

void init_multi_agent_built_in_commands(void) {

  add_command ("create-agent", create_agent_interface_routine);
  add_help ("create-agent", help_on_create_agent);

/* See command declaration above...
  add_command ("create-agents", create_agents_interface_routine);
  add_help ("create-agents", help_on_create_agents);
*/
  add_command ("destroy-agent", destroy_agent_interface_routine);
  add_help ("destroy-agent", help_on_destroy_agent);

  add_command ("agent-go", agent_go_interface_routine);
  add_help ("agent-go", help_on_agent_go);

  add_command ("schedule", schedule_interface_routine);
  add_help ("schedule", help_on_schedule);

#ifdef USE_X_DISPLAY
  add_command ("monitor", monitor_interface_routine);
  add_help ("monitor", help_on_monitor);
#else
  add_command ("select-agent", select_agent_interface_routine);
  add_help ("select-agent", help_on_select_agent);
#endif /* USE_X_DISPLAY */
}

#endif /* MULTI_AGENT_ENABLED */


