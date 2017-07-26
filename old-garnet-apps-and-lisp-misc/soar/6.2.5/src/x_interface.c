/*
 * $Id: x_interface.c,v 1.8 1994/11/23 16:40:34 rempel Exp $
 * $Log: x_interface.c,v $
 * Revision 1.8  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.7  1994/08/23  10:39:59  portelli
 * For 6.2.4
 *
 * Revision 1.6  1994/06/02  20:18:40  rempel
 * fixed some ModSAF stuff for Karl
 *
 * Revision 1.5  1994/06/01  19:51:45  rempel
 * fixed small problem with shell escape
 *
 * Revision 1.4  1994/03/15  21:28:37  rempel
 * no changes
 *
 * Revision 1.3  93/11/24  14:13:08  portelli
 * fixed 6.1.1 bug
 * 
 * Revision 1.2  93/11/21  17:26:55  soarhack
 * 6.1.1 checkin
 * 
 * Revision 1.1  1993/06/17  20:48:12  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:26:15  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  18:23:07  jtraub
 * added RCS header information
 *
 */

/*- I N C L U D E S -------------------------------------------------------*/

#include "soar.h"                              /* Imports stop_soar and    */
                                               /* reason_for_stopping.     */

#ifdef USE_X_DISPLAY

#include <X11/Xlib.h>                          /* Standard X headers.      */
#include <X11/Xutil.h>
#include <X11/keysym.h>                        /* Imports cursor key codes */

#include <stdio.h>

#ifdef USE_STDARGS
#include <stdarg.h>
#else
#include <varargs.h>
#endif

#include <math.h>                              /* Imports atan2, sin, cos  */

#include "x_resources.h"

/*- G L O B A L S ---------------------------------------------------------*/
/*--------------- X   I N F O ---------------------------------------------*/ 

Display * soar_display;

/*--------------- W I N D O W   P A R A M E T E R S -----------------------*/

/*--------------- O T H E R -----------------------------------------------*/

#define BUFFER_SIZE 200           /* Handy char buffer decl.               */
typedef char buffer[BUFFER_SIZE];

typedef char * string;            /* Constant strings used in display.     */
static string hi = "Click!";

char * x_input_buffer       = NIL;
int    x_input_buffer_index = 0;

/*- F U N C T I O N S -----------------------------------------------------*/

/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  redraw_x_display: Redraws the display showing the current state of the */
/*                    simulation.                                          */

void redraw_soar_x_display (Window window) {
  cons * c;
  agent * the_agent;

  for (c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = (agent *) c->first;
    if (the_agent->X_data->parent_window == window) {
      return;
    }

    if (the_agent->monitor)
      if (the_agent->monitor->parent_window == window) {
	if (the_agent->monitor)
	  the_agent->monitor->last_op_id = NULL;
	return;
      }
  }
  XFlush(soar_display);       /* Flush events so updates seen at X server  */
}

void note_window_resized (Window window, int width, int height) {
  cons * c;
  agent * the_agent;

  if (global_agent->X_data->parent_window == window) {
    global_agent->X_data->width = width;
    global_agent->X_data->height = height;
    XResizeWindow(soar_display, global_agent->X_data->window,
		  width - 2*global_agent->X_data->borderwidth,
		  height - 2*global_agent->X_data->borderwidth);
    return;
  }
  for (c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = (agent *) c->first;
    if (the_agent->X_data->parent_window == window) {
      the_agent->X_data->width = width;
      the_agent->X_data->height = height;
      XResizeWindow(soar_display, the_agent->X_data->window,
		    width - 2*the_agent->X_data->borderwidth,
		    height - 2*the_agent->X_data->borderwidth);
      return;
    }
    if (the_agent->monitor)
      if (the_agent->monitor->parent_window == window) {
	the_agent->monitor->width = width;
	the_agent->monitor->height = height;
	XResizeWindow(soar_display, the_agent->monitor->window,
		      width - 2*the_agent->monitor->borderwidth,
		      height - 2*the_agent->monitor->borderwidth);
	if (the_agent->monitor)
	  the_agent->monitor->last_op_id = NULL;
	return;
      }
  }
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  handle_cursor_event: This function is given a cursor key event to      */
/*                       process.  Currently, the cursors handle management*/
/*                       of the panning paramters.                         */

static void handle_cursor_event(KeySym key) {
  print("\nThat cursor key doesn't mean anything to Soar");
  print("-- ignoring keystroke.\n");
}


bool command_is_complete (x_info * xi) {
  int i;
  int paren_count = 0;
  int double_quotes_count = 0;

  for (i=0; i < xi->input_buffer_index; i++) {
    switch (xi->input_buffer[i]) {
    case '(':       
      paren_count++;
      break;
    case ')':       
      if (paren_count > 0)
	paren_count--;
      break;
    case '"':
      double_quotes_count++;
      break;
    }
  }
  return (paren_count == 0) && ((double_quotes_count % 2) == 0);
}

bool scroll_if_necessary (x_info * window) {
  if (window->window_y >= window->height - window->char_height) {
    window->window_x = 0;
    window->window_y -= window->char_height;
    XCopyArea(soar_display, window->window, window->window,
	      window->gc, 0, window->char_height,
	      window->width, window->height, 0, 0);
    XClearArea(soar_display, window->window,
	       0, window->height - window->char_height,
	       0, 0, FALSE);
    return TRUE;
  } else {
    return FALSE;
  }
}


void print_x_string(x_info * current_window, char * s) {
  char * c;
  static char buf[2000];
  int buf_start = 0;
  int buf_index = 0;
  int x_so_far;

  if (current_window) {
    x_so_far = current_window->window_x;
    for (c=s; *c!='\0'; c++) {
      if ((x_so_far + XTextWidth(current_window->font_struct, c, 1)) 
	  >= current_window->width) {
	buf[buf_index] = '\0';
	XDrawImageString(soar_display, current_window->window, current_window->gc,
			   current_window->window_x, current_window->window_y,
			   &buf[buf_start], buf_index - buf_start);
	buf_index++;
	buf_start = buf_index;
	current_window->window_x = 0;
	current_window->window_y += current_window->char_height;
	x_so_far = 0;
      }
      if (scroll_if_necessary(current_window)) {
	x_so_far = 0;
      }
      switch (*c) {
      case '\12':                         /* line feed       */
      case '\15':                         /* carriage return */
	if (buf_index) {
	  buf[buf_index] = '\0';
	  XDrawImageString(soar_display, current_window->window, current_window->gc,
			   current_window->window_x, current_window->window_y,
			   &buf[buf_start], buf_index - buf_start);
	buf_index++;
	buf_start = buf_index;
	}
	current_window->window_x  =  0;
	current_window->window_y += current_window->char_height;
	x_so_far = 0;
	break;
      default:
	buf[buf_index++] = *c;
	x_so_far += XTextWidth(current_window->font_struct, c, 1);
      }
    }
    if (buf_index - buf_start) {
      buf[buf_index] = '\0';
      XDrawImageString(soar_display, current_window->window, current_window->gc,
		       current_window->window_x, current_window->window_y,
		       &buf[buf_start], buf_index - buf_start);
      current_window->window_x += 
	XTextWidth(current_window->font_struct, 
		   &buf[buf_start], (buf_index - buf_start));
    }
    XFlush(soar_display);
  } else {
    fputs (s, stdout);
    fflush(stdout);
  }
}


#ifdef USE_STDARGS

void print_x_format_string (x_info * window, char * format, ...) {
  va_list args;
  buffer buf;

  va_start (args, format);
#else
void print_x_format_string (va_alist) va_dcl { 
  va_list args;
  x_info * window;
  char *format;
  buffer buf;

  va_start (args);
  window = va_arg(args, x_info *);
  format = va_arg(args, char *);
#endif
  vsprintf (buf, format, args);
  va_end (args);
  print_x_string (window, buf);
}

void print_agent_prompt (agent * agent_for_prompt) {
  print_x_format_string (agent_for_prompt->X_data, "\nSoar agent %s> ",
			 agent_for_prompt->name);  
}

void add_buffer_to_text_io_queue (agent * agent_receiving_text_io, x_info * x_data) {
  queue_add(agent_receiving_text_io->text_input_queue,
	    make_memory_block_for_string (x_data->text_input_buffer));
  x_data->text_input_buffer_index = 0;
}

bool text_io_mode = FALSE;

void send_buffer_to_soar_agent (agent * agent_to_get_command) {
  x_info * xi;
  agent * prev_agent;

  prev_agent = soar_agent;
  soar_agent = agent_to_get_command; 

  xi = agent_to_get_command->X_data;

  if (xi->input_buffer_index != 0) {
    xi->input_buffer[xi->input_buffer_index] = '\n';
    x_input_buffer = xi->input_buffer;
    x_input_buffer_index = 0;
  
    /* --- consume rparen from previous command, get start of next cmd. --- */
    get_lexeme();
    if (current_agent(lexeme).type==EOF_LEXEME) return;

    /* --- if not lparen, fake one at end of the current line --- */
    if (current_agent(lexeme).type==L_PAREN_LEXEME) {
      get_lexeme(); /* consume lparen */
    } else {
      fake_rparen_at_next_end_of_line ();
    }
    if ((current_agent(lexeme).type == SYM_CONSTANT_LEXEME) ||
	(current_agent(lexeme).type == DOLLAR_STRING_LEXEME)) { /* AGR 562 */
      text_io_mode = TRUE;
      dispatch_command();
      text_io_mode = FALSE;
    } else {
      print ("Error:  unknown command %s\n", current_agent(lexeme).string);
      print_location_of_most_recent_lexeme();
      skip_ahead_to_balanced_parentheses(0);
    }
  }

  x_input_buffer = NIL;    
  xi->input_buffer_index = 0;
  print_agent_prompt(soar_agent);
  soar_agent = prev_agent;
}


bool need_to_update_monitor (x_info * monitor) {
  Symbol * g;
  Symbol * last_goal;

  for (g=current_agent(top_goal); g!=NIL; g=g->id.lower_goal)
    last_goal = g;

  if (last_goal->id.operator_slot->id == monitor->last_op_id) {
    return FALSE;
  } else {
    monitor->last_op_id = last_goal->id.operator_slot->id;
    return TRUE;
  }
}

void refresh_monitor_window (agent * agent_to_get_command) {
  x_info * normal_xi;
  x_info * monitor_xi;
  agent * prev_agent;
  bool currently_logging;
  bool currently_redirecting;

  prev_agent = soar_agent;
  soar_agent = agent_to_get_command; 

  normal_xi = agent_to_get_command->X_data;
  monitor_xi = agent_to_get_command->monitor;

  if (need_to_update_monitor(monitor_xi))
    {
      agent_to_get_command->X_data = monitor_xi;

      x_input_buffer = monitor_xi->input_buffer;
      x_input_buffer_index = 0;

      currently_logging     = current_agent(logging_to_file);
      currently_redirecting = current_agent(redirecting_to_file);
      current_agent(logging_to_file) = FALSE;
      current_agent(redirecting_to_file) = FALSE;
  
      /*-- consume rparen from previous command, get start of next cmd. --*/
      get_lexeme();
      if (current_agent(lexeme).type==EOF_LEXEME) return;

      /* --- if not lparen, fake one at end of the current line --- */
      if (current_agent(lexeme).type==L_PAREN_LEXEME) {
	get_lexeme(); /* consume lparen */
      } else {
	fake_rparen_at_next_end_of_line ();
      }
      if ((current_agent(lexeme).type == SYM_CONSTANT_LEXEME) ||
	  (current_agent(lexeme).type == DOLLAR_STRING_LEXEME)) { /* AGR 562 */
	monitor_xi->window_x = 0;
	monitor_xi->window_y = monitor_xi->char_height;
	XClearWindow(soar_display, monitor_xi->window);
	dispatch_command();
      } else {
	print ("Error:  unknown command %s\n", current_agent(lexeme).string);
	print_location_of_most_recent_lexeme();
	skip_ahead_to_balanced_parentheses(0);
      }
      x_input_buffer = NIL;    

      agent_to_get_command->X_data = normal_xi;

      current_agent(logging_to_file)     = currently_logging;
      current_agent(redirecting_to_file) = currently_redirecting;
    }
  soar_agent = prev_agent;
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  handle_keystroke_event: This procedure handles a single character key- */
/*                          stroke event.                                  */

static void handle_keystroke_event(Window soar_window, char keystroke) {
  x_info * x_data;
  char buf[2];
  cons * c;
  agent * agent_for_window;
  bool window_found = FALSE;

  if (global_agent->X_data->window == soar_window) {
    window_found = TRUE;
    x_data = global_agent->X_data;
    agent_for_window = global_agent;
  } else {
    for(c = all_soar_agents; c != NIL; c = c->rest) {
      agent_for_window = (agent *) c->first;
      x_data = agent_for_window->X_data;
      if (x_data->window == soar_window) {
	window_found = TRUE;
	break;
      }
    }
  }
  if (window_found) {
    switch (keystroke) {
    case '\3' :                        /* \3 is a cntl-C which stops Soar   */
      control_c_handler (0);
      break;
    case '\15':                        /* \15 is a carriage return          */
      x_data->window_x = 0;
      x_data->window_y += x_data->char_height;
      scroll_if_necessary (x_data);

      if (text_io_mode) {
	x_data->text_input_buffer[x_data->text_input_buffer_index++] 
	  = 0;
	add_buffer_to_text_io_queue(agent_for_window, x_data);
      } else {
	if (command_is_complete(x_data)) {
	  send_buffer_to_soar_agent(agent_for_window);
	} else {
	  x_data->input_buffer[x_data->input_buffer_index++] = ' ';
	}
      }
      break;
    case 0:                           /* Ignore null char events.           */
      break;
    case '\10':                       /* Backspace */
    case '\177':                      /* DEL       */
      if (x_data->window_x > 0 
	  && (   (text_io_mode && x_data->text_input_buffer_index > 0)
	      || (!text_io_mode && x_data->input_buffer_index > 0)))
	{
	  char c;

	  buf[0] = ' ';
	  buf[1] = 0;

	  
	  if (text_io_mode) {
	    c = x_data->text_input_buffer[x_data->text_input_buffer_index - 1];
	    x_data->text_input_buffer[--x_data->text_input_buffer_index] = buf[0];
	  } else {
	    c = x_data->input_buffer[x_data->input_buffer_index - 1];
	    x_data->input_buffer[--x_data->input_buffer_index] = buf[0];
	  }

	  x_data->window_x -= XTextWidth(x_data->font_struct, &c, 1);

	  XSetForeground(soar_display, x_data->gc, x_data->background);
	  XDrawImageString(soar_display, soar_window, x_data->gc, 
			   x_data->window_x, x_data->window_y, 
			   &c, 1);
	  XSetForeground(soar_display, x_data->gc, x_data->foreground);

	  XFlush(soar_display);
	}
      break;
    default:
      buf[0] = keystroke;
      buf[1] = 0;
      XDrawImageString(soar_display, soar_window, x_data->gc, 
		       x_data->window_x, x_data->window_y, 
		       buf, 1);
      XFlush(soar_display);
      if (text_io_mode) {
	if (x_data->text_input_buffer_index >= MAX_TEXT_INPUT_LINE_LENGTH) {
	  print ("\nText Input Error: input line too long, truncated it.\n");	
	} else {
	  x_data->text_input_buffer[x_data->text_input_buffer_index++] 
	    = keystroke;
	}
      } else {
	x_data->input_buffer[x_data->input_buffer_index++] = keystroke;
      }
      x_data->window_x += XTextWidth(x_data->font_struct, &keystroke, 1);  
                                                      /* font char width */
    }
  }
}
  

/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  handle_x_events: Reads and processes any pending X events for the      */
/*                   graphics window.  This routine uses non-wait I/O.     */

void handle_soar_x_events(void) {
  XEvent event;               /* X event sent to graphics window           */
  KeySym  key;                /* Code assoc. with user's keystroke         */
  int event_count;            /* Number of events pending in event queue   */
  int char_count;             /* Number of characters in keystroke         */
  buffer text;                /* Actual characters in keystroke            */

                              /* Process all events pending in the X-event */
                              /* queue.                                    */

  for (event_count = XEventsQueued(soar_display, QueuedAfterFlush) 
       ; event_count != 0 
       ; event_count = XEventsQueued(soar_display, QueuedAlready) ) {
 
    XNextEvent(soar_display, &event);      /* Get the next X event in the queue */

    switch (event.type) {
    case Expose:                      /* Window was exposed... redraw:     */
    case VisibilityNotify:
      if (!event.xexpose.count) {     
	redraw_soar_x_display(event.xexpose.window);           
      } 
      break;
    case MappingNotify:               /* Don't ask.                        */
      XRefreshKeyboardMapping(&(event.xmapping));
      break;
    case ButtonPress:                 /* Stub for mouse clicks.            */
      XDrawImageString(event.xbutton.display, event.xbutton.window, 
		       global_agent->X_data->gc, 
		       event.xbutton.x, event.xbutton.y, hi, strlen(hi));
      break;
    case KeyPress:                    /* Regular keystroke...              */
      char_count = XLookupString(&(event.xkey), text, BUFFER_SIZE, &key, 0);

      if (IsCursorKey(key))           /* Cursor movement keys alter the    */
	handle_cursor_event(key);     /* pan settings.                     */
      else if (IsModifierKey(key))
        ;                             /* Ignore modifier keys */ 
      else if (char_count == 1)       /* Here we've got other actions      */
	handle_keystroke_event(event.xkey.window, text[0]);
      break;
    case GraphicsExpose:              /* XCopyArea side effect - ignore.   */
    case NoExpose:
      break;
    case CirculateNotify:
    case DestroyNotify:
    case GravityNotify:
    case MapNotify:
    case ReparentNotify:
    case UnmapNotify:
      break;
    case ConfigureNotify:
      note_window_resized (event.xconfigure.window,
			   event.xconfigure.width,
			   event.xconfigure.height);
      break;
    default:                          /* Unknown event seen, so stop.      */
      fprintf(stderr, "\nUnknown event seen!  Type = %d\n", event.type); 
      return;
    } /* switch */
  } /* for */
}


void wait_for_exposure (Display * display, Window window) {
  XEvent event;               /* X event sent to graphics window           */

  while(TRUE)
    {
      XNextEvent(display, &event);      /* Get the next X event in the queue */

      if (event.type == Expose) {       /* Window was exposed... redraw:     */
	if (event.xexpose.window == window && !event.xexpose.count) {     
	  return;
	} 
      }
    }
}


void soar_open_display(char * pgm_name, Display * * soar_display) {

  if (!(*soar_display = XOpenDisplay(soar_x_defaults.display))) {
    fprintf(stderr, "%s: Can't open display '%s'\n",
	    pgm_name, XDisplayName(soar_x_defaults.display));
    fprintf (stderr, "Check your DISPLAY environment variable and\n");
    fprintf (stderr, "your X resources setup.\n");
    if (!(*soar_display = XOpenDisplay(""))) {
      fprintf(stderr, "%s: Can't open default display\n",
	      pgm_name, XDisplayName(soar_x_defaults.display));
      fprintf (stderr, "Terminating program...\n");
      exit(1);
    }
  }
}


unsigned long find_color (Display * soar_display, char * color_name, 
	                  unsigned long error_color) {
  int      screen;            /* Code for current screen.                  */
  Visual * visual;
  Colormap colormap;          /* Color map.                                */
  XColor   screen_def;

  unsigned long color;

  screen = DefaultScreen(soar_display);      /* Get the current screen.    */
  visual = DefaultVisual(soar_display, screen);
  colormap = DefaultColormap(soar_display, screen);

  if (*color_name) {
      if (XParseColor(soar_display, colormap, color_name, &screen_def) == 0) {
          (void) fprintf(stderr,
                         "Soar: color specification %s invalid", color_name);
          color = error_color;
      } else {
          if ((visual->class == StaticGray)
              || (visual->class == GrayScale))
            color = error_color;
          else if (XAllocColor(soar_display, colormap, &screen_def) == 0) {
              (void) fprintf(stderr, "Soar: couldn't allocate color: %s.\n",
                             color_name);
              color = error_color;
          } else
            color = screen_def.pixel;
        }
    }
  else {
      color = error_color;
  }
  return color;
}


void determine_foreground_background_colors (unsigned long * foreground,
					     unsigned long * background) {
  int      screen;            /* Code for current screen.                  */

  screen = DefaultScreen(soar_display);      /* Get the current screen.    */

  *foreground = find_color (soar_display, soar_x_defaults.foreground, 
			   BlackPixel(soar_display, screen));
  *background = find_color (soar_display, soar_x_defaults.background, 
			   WhitePixel(soar_display, screen));

                  /* one last check to make sure the colors are different! */
  if (*background == *foreground) {
    *background = WhitePixel(soar_display, screen);
    *foreground = BlackPixel(soar_display, screen);
  }
}


void find_selected_font (Display * soar_display, x_info * xi) {
  XFontStruct * font_struct;

  font_struct = XLoadQueryFont (soar_display, soar_x_defaults.font_name);
  if (font_struct)
    XSetFont(soar_display, xi->gc, font_struct->fid);
  else {
    font_struct = XLoadQueryFont (soar_display, "6x13");
    XSetFont(soar_display, xi->gc, font_struct->fid);    
  }
  xi->char_height = font_struct->ascent + font_struct->descent;
  xi->font_struct = font_struct;
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  create_agent_window: This procedure creates the window used to display */
/*                      the command line interaction with each agent.      */

void create_agent_window(agent * soar_agent, char * agent_class) {
  buffer title;
                              /* Various X window vars decl'd for calls to */
                              /* X init routines:                          */
  int screen;                 /* Code for current screen.                  */
  XWMHints wm_h;              /* Window manager hints */
  XSizeHints  h;              /* Window sizeing information.               */
  int argc;                   /* Used to simulate command line input below */
  char *argv[1];
  XGCValues values;           /* Graphics context values.                  */
  unsigned long valuemask;    /* var used to set window values.            */
  XSetWindowAttributes xswa;  /* Window attribute helper var.              */
  int flags;
  int borderwidth;
  unsigned long foreground;
  unsigned long background;
  unsigned long bordercolor;

  soar_agent->display_class = savestring(agent_class);

  soar_get_all_X_resources(agent_class, soar_agent->name, FALSE);

  screen = DefaultScreen(soar_display);      /* Get the current screen.    */

  determine_foreground_background_colors (&foreground, &background);

  flags = XGeometry(soar_display, screen, soar_x_defaults.geometry,
		    "480x195+0+0", 5, 1, 1, 0, 0, 
		    &h.x, &h.y, &h.width, &h.height);                 

  h.flags = USPosition | PSize;

  sscanf(soar_x_defaults.borderwidth, "%d", &borderwidth);
  bordercolor = find_color(soar_display, soar_x_defaults.bordercolor,
			   background);

  soar_agent->X_data = (x_info *) malloc (sizeof(x_info));

  soar_agent->X_data->borderwidth = borderwidth;

  soar_agent->X_data->parent_window = 
    XCreateSimpleWindow(soar_display, 
			DefaultRootWindow(soar_display),
			h.x, h.y, h.width, h.height, 0, 
			background, background);

  soar_agent->X_data->window  = 
    XCreateSimpleWindow(soar_display, 
			soar_agent->X_data->parent_window,
			0, 0,
			h.width - 2*borderwidth, 
			h.height - 2*borderwidth, 
			borderwidth, 
			bordercolor,
			background);

  if (!strcmp(soar_x_defaults.initialstate, "IconicState"))
    {
      wm_h.initial_state = IconicState;
    }
  else
    {
      wm_h.initial_state = NormalState;
    }    
  wm_h.flags = StateHint;
  XSetWMHints(soar_display, soar_agent->X_data->parent_window, &wm_h);

  argc = 1;                  /* Setup fake command line to set up window   */
  argv[0] = "soar-agent";    /* title bar.                                 */
  sprintf(title, "Agent %s", soar_agent->name);
  XSetStandardProperties(soar_display, soar_agent->X_data->parent_window, 
			 title, title, None, argv, argc, &h);

                             /* Set up mode for drawing.                   */
  valuemask = GCFunction | GCForeground; 
  values.function = GXcopy;
  values.foreground = foreground;
  soar_agent->X_data->gc = XCreateGC(soar_display, soar_agent->X_data->window, 
				valuemask, &values);

  find_selected_font (soar_display, soar_agent->X_data);

                             /* Setup fore/background colors.              */
  XSetBackground(soar_display, soar_agent->X_data->gc, background);
  XSetForeground(soar_display, soar_agent->X_data->gc, foreground);

                             /* Set events we'll be accepting.             */
  XSelectInput(soar_display, soar_agent->X_data->window, 
        KeyPressMask | ExposureMask | VisibilityChangeMask | StructureNotifyMask);

  XSelectInput(soar_display, soar_agent->X_data->parent_window, 
        ExposureMask | VisibilityChangeMask | StructureNotifyMask);

                             /* Set backing store to cut down on expose    */
                             /* events and produce faster restoration of   */
                             /* window.                                    */
  xswa.backing_store = WhenMapped;
  valuemask = CWBackingStore;
  XChangeWindowAttributes(soar_display, 
			  soar_agent->X_data->window, valuemask, &xswa);

  soar_agent->X_data->input_buffer_index      = 0;
  soar_agent->X_data->text_input_buffer_index = 0;
  soar_agent->X_data->window_x                = 0;
  soar_agent->X_data->window_y                = 0;
  soar_agent->X_data->width                   = h.width  - 2*borderwidth;
  soar_agent->X_data->height                  = h.height - 2*borderwidth;
  soar_agent->X_data->foreground              = foreground;
  soar_agent->X_data->background              = background;

  soar_agent->monitor = NULL;

  XMapRaised(soar_display, soar_agent->X_data->parent_window);
  wait_for_exposure(soar_display, soar_agent->X_data->parent_window);

  XMapRaised(soar_display, soar_agent->X_data->window);
  wait_for_exposure(soar_display, soar_agent->X_data->window);
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  create_monitor_window: This procedure creates the window used to       */
/*                         display monitored commands for an agent.        */

void create_monitor_window(agent * soar_agent, char * command_string) {
  buffer title;
                              /* Various X window vars decl'd for calls to */
                              /* X init routines:                          */
  int screen;                 /* Code for current screen.                  */
  XWMHints wm_h;              /* Window manager hints */
  XSizeHints  h;              /* Window sizeing information.               */
  int argc;                   /* Used to simulate command line input below */
  char *argv[1];
  XGCValues values;           /* Graphics context values.                  */
  unsigned long valuemask;    /* var used to set window values.            */
  XSetWindowAttributes xswa;  /* Window attribute helper var.              */
  int flags;
  unsigned long foreground;
  unsigned long background;
  int borderwidth;
  int bordercolor;

  soar_get_all_X_resources (soar_agent->display_class, soar_agent->name, TRUE);

  screen = DefaultScreen(soar_display);      /* Get the current screen.    */

  determine_foreground_background_colors (&foreground, &background);

  flags = XGeometry(soar_display, screen, 
		    soar_x_defaults.geometry,
		    "480x195+100+100", 5, 1, 1, 0, 0, 
		    &h.x, &h.y, &h.width, &h.height);                 

  h.flags = USPosition | PSize;

  sscanf(soar_x_defaults.borderwidth, "%d", &borderwidth);

  bordercolor = find_color(soar_display, soar_x_defaults.bordercolor,
			   background);

  soar_agent->monitor = (x_info *) malloc (sizeof(x_info));

  soar_agent->monitor->borderwidth = borderwidth;

  soar_agent->monitor->parent_window = 
    XCreateSimpleWindow(soar_display, 
			DefaultRootWindow(soar_display),
			h.x, h.y, h.width, h.height, 0,
			background, background);
  soar_agent->monitor->window  = 
    XCreateSimpleWindow(soar_display, 
			soar_agent->monitor->parent_window,
			0, 0, 
			h.width - 2*borderwidth, 
			h.height - 2*borderwidth, 
			borderwidth, 
			bordercolor,
			background);

  if (!strcmp(soar_x_defaults.initialstate, "IconicState"))
    {
      wm_h.initial_state = IconicState;
    }
  else
    {
      wm_h.initial_state = NormalState;
    }    
  wm_h.flags = StateHint;
  XSetWMHints(soar_display, soar_agent->monitor->parent_window, &wm_h);

  argc = 1;                  /* Setup fake command line to set up window   */
  argv[0] = "soar-agent-monitor"; /* title bar.                            */
  sprintf(title, "Agent %s monitor %s", soar_agent->name, command_string);
  XSetStandardProperties(soar_display, soar_agent->monitor->parent_window, 
			 title, title, None, argv, argc, &h);

                             /* Set up mode for drawing.                   */
  valuemask = GCFunction | GCForeground; 
  values.function = GXor;
  values.foreground = foreground;

  soar_agent->monitor->gc = XCreateGC(soar_display, 
				      soar_agent->monitor->window, 
				      valuemask, &values);

  find_selected_font(soar_display, soar_agent->monitor);

                             /* Setup fore/background colors.              */
  XSetBackground(soar_display, soar_agent->monitor->gc, background);
  XSetForeground(soar_display, soar_agent->monitor->gc, foreground);

                             /* Set events we'll be accepting.             */
  XSelectInput(soar_display, soar_agent->monitor->window, 
        ExposureMask | VisibilityChangeMask | StructureNotifyMask);

  XSelectInput(soar_display, soar_agent->monitor->parent_window, 
        ExposureMask | VisibilityChangeMask | StructureNotifyMask);

                             /* Set backing store to cut down on expose    */
                             /* events and produce faster restoration of   */
                             /* window.                                    */
  xswa.backing_store = WhenMapped;
  valuemask = CWBackingStore;
  XChangeWindowAttributes(soar_display, soar_agent->monitor->window, valuemask, &xswa);

  soar_agent->monitor->input_buffer_index      = 0;
  soar_agent->monitor->text_input_buffer_index = 0;
  soar_agent->monitor->window_x                = 0;
  soar_agent->monitor->window_y                = 0;
  soar_agent->monitor->width                   = h.width  - 2*borderwidth;
  soar_agent->monitor->height                  = h.height - 2*borderwidth;
  soar_agent->monitor->foreground              = foreground;
  soar_agent->monitor->background              = background;
  soar_agent->monitor->last_op_id              = NULL;

  XMapRaised(soar_display, soar_agent->monitor->parent_window);
  wait_for_exposure(soar_display, soar_agent->monitor->parent_window);

  XMapRaised(soar_display, soar_agent->monitor->window);
  wait_for_exposure(soar_display, soar_agent->monitor->window);
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  create_display: Creates and shows to the user the X window used for    */
/*                  the graphic display.                                   */

void create_global_display (void) {
  int i, j;                   /* Loop indices.                             */

  string title = "Global Soar Control";
                              /* Various X window vars decl'd for calls to */
                              /* X init routines:                          */
  int screen;                 /* Code for current screen.                  */
  XWMHints wm_h;              /* Window manager hints */
  XSizeHints  h;              /* Window sizeing information.               */
  int argc;                   /* Used to simulate command line input below */
  char *argv[1];
  XGCValues values;           /* Graphics context values.                  */
  unsigned long valuemask;    /* var used to set GC values.                */
  XSetWindowAttributes xswa;  /* Window attribute helper var.              */
  XColor exact;               /* Color struct.                             */
  XColor screen_def;
  static char dot[] = {2,2};  /* Encoding for (pseudo)dotted lines.        */
  int flags;
  int borderwidth;
  unsigned long bordercolor;
  unsigned long foreground;
  unsigned long background;

  argc = 1;                  /* Setup fake command line to set up window   */
  argv[0] = "soar";          /* title bar.                                 */

  global_agent->display_class = "";
  soar_init_x_resources(argc, global_agent->name, argv);

  soar_open_display(argv[0], &soar_display);

  screen = DefaultScreen(soar_display);      /* Get the current screen.    */

  determine_foreground_background_colors (&foreground, &background);

  flags = XGeometry(soar_display, screen, soar_x_defaults.geometry,
		    "480x195+0+0", 5, 1, 1, 0, 0, 
		    &h.x, &h.y, &h.width, &h.height);
		    
  h.flags = USPosition | PSize;

  global_agent = soar_agent;

                             /* Create a window using the above data.      */
  global_agent->X_data = (x_info *) malloc (sizeof(x_info));

  sscanf(soar_x_defaults.borderwidth, "%d", &borderwidth);
  global_agent->X_data->borderwidth = borderwidth;

  bordercolor = find_color(soar_display, soar_x_defaults.bordercolor,
			   background);

  global_agent->X_data->parent_window = 
    XCreateSimpleWindow(soar_display,
			DefaultRootWindow(soar_display),
			h.x, h.y,
			h.width, h.height, 0,
			background,
			background);

  global_agent->X_data->window = 
    XCreateSimpleWindow(soar_display, 
			global_agent->X_data->parent_window,
			0, 0, h.width - 2*borderwidth, 
			h.height - 2*borderwidth, 
			borderwidth,
			bordercolor, 
			background);

  if (!strcmp(soar_x_defaults.initialstate, "IconicState"))
    {
      wm_h.initial_state = IconicState;
    }
  else
    {
      wm_h.initial_state = NormalState;
    }    
  wm_h.flags = StateHint;
  XSetWMHints(soar_display, global_agent->X_data->parent_window, &wm_h);

  global_agent->X_data->input_buffer_index = 0;
  global_agent->X_data->window_x           = 0;
  global_agent->X_data->window_y           = 0;
  global_agent->X_data->width              = h.width  - 2*borderwidth;
  global_agent->X_data->height             = h.height - 2*borderwidth;
  global_agent->X_data->foreground         = foreground;
  global_agent->X_data->background         = background;

  XSetStandardProperties(soar_display, global_agent->X_data->parent_window,
			 title, title, None, argv, argc, &h);

                             /* Set up Xor mode for drawing.               */
  valuemask = GCFunction | GCForeground | GCBackground;
  values.function = GXcopy;
  values.foreground = foreground;
  values.background = background;
  global_agent->X_data->gc = XCreateGC(soar_display, 
				       global_agent->X_data->window, 
				       valuemask, &values);

  find_selected_font (soar_display, global_agent->X_data);

                             /* Set up video.                              */
  XSetBackground(soar_display, global_agent->X_data->gc, background);
  XSetForeground(soar_display, global_agent->X_data->gc, foreground);

                             /* Set events we'll be accepting.             */
  XSelectInput(soar_display, global_agent->X_data->window, 
       KeyPressMask | ExposureMask | VisibilityChangeMask | StructureNotifyMask);

  XSelectInput(soar_display, global_agent->X_data->parent_window,
       ExposureMask | VisibilityChangeMask | StructureNotifyMask);
                             /* Set backing store to cut down on expose    */
                             /* events and produce faster restoration of   */
                             /* window.                                    */
  xswa.backing_store = WhenMapped;
  valuemask = CWBackingStore;
  XChangeWindowAttributes(soar_display, global_agent->X_data->window, valuemask, &xswa);

  XMapRaised(soar_display, global_agent->X_data->parent_window);
  wait_for_exposure(soar_display, global_agent->X_data->parent_window);

  XMapRaised(soar_display, global_agent->X_data->window);
  wait_for_exposure(soar_display, global_agent->X_data->window);
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  destroy_agent_window: Destroys the X window associated with a given    */
/*                        soar agent.                                      */

void destroy_agent_window (agent * soar_agent) {
  if (soar_agent->monitor) {
      XFreeGC(soar_display, soar_agent->monitor->gc);
      XDestroyWindow(soar_display, soar_agent->monitor->window);
      XDestroyWindow(soar_display, soar_agent->monitor->parent_window);
      free (soar_agent->monitor);
    }
  XFreeGC(soar_display, soar_agent->X_data->gc);
  XDestroyWindow(soar_display, soar_agent->X_data->window);
  XDestroyWindow(soar_display, soar_agent->X_data->parent_window);
  free (soar_agent->X_data);
}


/*-------------------------------------------------------------------------*/
/*                                                                         */
/*  destroy_display: Destroys the X window used for the graphics display   */
/*                   and performs other standard X cleanup actions.        */

void destroy_soar_display (void) {
  cons * c;
  agent * ag;

  XFreeGC(soar_display, global_agent->X_data->gc);
  XDestroyWindow(soar_display, global_agent->X_data->window);
  XDestroyWindow(soar_display, global_agent->X_data->parent_window);

  for (c = all_soar_agents; c != NIL; c = c->rest) {
    ag = (agent *) c->first;
    XFreeGC(soar_display, ag->X_data->gc);
    XDestroyWindow(soar_display, ag->X_data->window);
    XDestroyWindow(soar_display, ag->X_data->parent_window);
  }

  XCloseDisplay(soar_display);
}

#endif

