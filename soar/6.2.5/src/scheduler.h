/*
 * $Id: scheduler.h,v 1.4 1994/08/23 10:38:38 portelli Exp $
 * $Log: scheduler.h,v $
 * Revision 1.4  1994/08/23  10:38:38  portelli
 * For 6.2.4
 *
 * Revision 1.3  1993/11/21  18:50:48  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:59  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:24:42  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  18:37:05  jtraub
 * Added RCS header information
 *
 */

/*  
    The schedule_agents function is the entry point to the multi-agent
    soar scheduler.  A round robin scheduling protocol is used for now.

    In a cycle all agents are run for their specified "agent-go" duration
    and the X-window event queue is processed (if applicable).  The allowed
    values for input are some nuber of cycles.  With an input of -1, the
    scheduler continues until all agents are stopped.  This may happen
    in normal agent processing termination or through a user interrupt.
*/

extern long scheduler_cycle_count;

extern void schedule_agents (int cycles);
