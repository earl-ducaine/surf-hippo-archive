/*
 * $Id: main.c,v 1.4 1994/11/23 16:40:34 rempel Exp $
 * $Log: main.c,v $
 * Revision 1.4  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.3  1994/08/23  10:35:32  portelli
 * For 6.2.4
 *
 * Revision 1.2  1993/11/21  17:00:49  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:33  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:21:49  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  19:31:26  jtraub
 * added RCS header information
 *
 */

/* ===================================================================
                       Main file for Soar 6
=================================================================== */

#include "soar.h"

#ifdef __SC__
#include <Memory.h>
#endif

#ifndef MULTI_AGENT_ENABLED
#include "global_vars.c"
#endif

/* ===================================================================
   
                           Main Function

=================================================================== */


int main ()
{
#ifdef THINK_C
	/* Increase the application stack by 16K.
	 * This is done by decreasing the heap.
	 */
	SetApplLimit(GetApplLimit() - 16384);
	MaxApplZone();
#endif

  init_soar();
  repeatedly_read_and_dispatch_commands ();
  return terminate_soar();
}

