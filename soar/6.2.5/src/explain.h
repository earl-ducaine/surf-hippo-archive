/*
 * $Id: explain.h,v 1.5 1994/11/23 16:42:28 rempel Exp $
 * $Log: explain.h,v $
 * Revision 1.5  1994/11/23  16:42:28  rempel
 * for 6.2.4
 *
 * Revision 1.4  1994/08/23  10:33:28  portelli
 * For 6.2.4
 *
 * Revision 1.3  1994/05/10  15:04:55  rempel
 * *** empty log message ***
 *
 * Revision 1.2  1993/11/21  16:45:31  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  93/11/21  16:21:25  soarhack
 * initial checkin
 */

/* About 80 lines of stuff deleted.  AGR 564  2-May-94 */

/* KBS commented this out -- redundant with global_vars.h */
/* extern bool explain_flag;   Flag for whether we're explaining or not */

extern bool explain_interface_routine (void);
extern char *help_on_explain[];

