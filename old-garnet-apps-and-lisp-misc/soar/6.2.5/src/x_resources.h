/*
 * $Id: x_resources.h,v 1.4 1994/08/23 10:40:21 portelli Exp $
 * $Log: x_resources.h,v $
 * Revision 1.4  1994/08/23  10:40:21  portelli
 * For 6.2.4
 *
 * Revision 1.3  1994/06/02  20:20:23  rempel
 * fixed some ModSAF stuff for Karl
 *
 * Revision 1.2  1993/11/21  22:33:49  portelli
 * 6.1.1 checkin
 *
 * Revision 1.1  93/11/21  16:21:25  soarhack
 * initial checkin
 */

extern Display * soar_display;

/*
 * Startup options.
 */

typedef struct soar_x_default_struct {
  char display[100];
  char geometry[20];
  char font_name[100];
  char background[100];
  char foreground[100];
  char borderwidth[4];
  char bordercolor[100];
  char initialstate[100];
} soar_x_default_record;

extern soar_x_default_record soar_x_defaults;

/* resource reading function: */
extern void soar_init_x_resources(int argc, char * control_name,  char * argv[]);
extern void soar_get_all_X_resources(char * agent_class, char * name, bool monitor);
