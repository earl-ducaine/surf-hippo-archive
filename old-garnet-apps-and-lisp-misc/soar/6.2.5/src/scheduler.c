/*
 * $Id: scheduler.c,v 1.3 1994/08/23 10:38:26 portelli Exp $
 * $Log: scheduler.c,v $
 * Revision 1.3  1994/08/23  10:38:26  portelli
 * For 6.2.4
 *
 * Revision 1.2  1993/11/21  17:22:24  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:58  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:24:30  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  18:37:38  jtraub
 * added RCS header information
 *
 */

#include "soar.h"

#ifdef MULTI_AGENT_ENABLED

long scheduler_cycle_count = 0;

void
schedule_agents (int cycles) 
{
  bool agent_to_run = TRUE;
  cons  * c; 
  agent * the_agent; 
  agent * prev_agent;
  int cycle_count = 0;

  prev_agent = soar_agent;

  for(c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = (agent *) c->first;
    the_agent->stop_soar = FALSE;
  }

  while (agent_to_run) {

    if (cycle_count == cycles) break;
    cycle_count++;

    agent_to_run = FALSE;

    for(c = all_soar_agents; c != NIL; c = c->rest) {
      soar_agent = (agent *) c->first;

     if (!current_agent(stop_soar)) {
       agent_to_run = TRUE;

#ifndef USE_X_DISPLAY
        print("\nSelecting agent %s", soar_agent->name);
#endif

        execute_go_selection();

#ifdef USE_X_DISPLAY
        if (soar_agent->monitor) 
          {
	    refresh_monitor_window(soar_agent);
	  }
#endif
      }

    }

#ifdef USE_X_DISPLAY
      handle_soar_x_events();
#endif

  }

  soar_agent = prev_agent;
}

#endif
