/*
 * $Id: rhsfun.c,v 1.7 1994/11/23 16:40:34 rempel Exp $
 * $Log: rhsfun.c,v $
 * Revision 1.7  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.6  1994/08/23  10:37:54  portelli
 * For 6.2.4
 *
 * Revision 1.5  1994/07/01  15:58:11  portelli
 * For 6.2.2
 *
 * Revision 1.4  1994/06/08  22:20:12  portelli
 * For 6.2.1
 *
 * Revision 1.3  94/05/06  20:28:41  rempel
 * *** empty log message ***
 * 
 * Revision 1.2  1993/11/21  17:10:53  soarhack
 * 6.1.1 checkin
 *
 * Revision 1.1  1993/06/17  20:47:56  jtraub
 * Released
 *
 * Revision 0.1  1993/06/17  20:24:15  jtraub
 * 6.1_checkin
 *
 * Revision 9.2  1993/05/10  18:39:25  jtraub
 * added RCS header information
 *
 */

/* ====================================================================
                   RHS Function Management for Soar 6

   The system maintains a list of available RHS functions.  Functions
   can appear on the RHS of productions either as values (in make actions
   or as arguments to other function calls) or as stand-alone actions
   (e.g., "write" and "halt").  When a function is executed, its C code
   is called with one parameter--a (consed) list of the arguments (symbols).
   The C function should return either a symbol (if all goes well) or NIL
   (if an error occurred, or if the function is a stand-alone action).

   All available RHS functions should be setup at system startup time via
   calls to add_rhs_function().  It takes as arguments the name of the
   function (a symbol), a pointer to the corresponding C function, the
   number of arguments the function expects (-1 if the function can take
   any number of arguments), and flags indicating whether the function can
   be a RHS value or a stand-alone action.

   Lookup_rhs_function() takes a symbol and returns the corresponding
   rhs_function structure (or NIL if there is no such function).

   Init_built_in_rhs_functions() should be called at system startup time
   to setup all the built-in functions.
==================================================================== */

#include "soar.h"
#include <time.h>
#include "rhsfun_math.h"

rhs_function *available_rhs_functions = NIL;

void add_rhs_function (Symbol *name,
                       rhs_function_routine f,
                       int num_args_expected,
                       bool can_be_rhs_value,
                       bool can_be_stand_alone_action) {
  rhs_function *rf;

  if ((!can_be_rhs_value) && (!can_be_stand_alone_action)) {
    print ("Internal error: attempt to add_rhs_function that can't appear anywhere\n");
    return;
  }
  for (rf=available_rhs_functions; rf!=NIL; rf=rf->next)
    if (rf->name==name) break;
  if (rf) {
    print_with_symbols ("Internal error: attempt to add_rhs_function that already exists: %y\n", name);
    return;
  }
  rf = allocate_memory (sizeof(rhs_function), MISCELLANEOUS_MEM_USAGE);
  rf->next = available_rhs_functions;
  available_rhs_functions = rf;
  rf->name = name;
  rf->f = f;
  rf->num_args_expected = num_args_expected;
  rf->can_be_rhs_value = can_be_rhs_value;
  rf->can_be_stand_alone_action = can_be_stand_alone_action;
}

rhs_function *lookup_rhs_function (Symbol *name) {
  rhs_function *rf;

  for (rf=available_rhs_functions; rf!=NIL; rf=rf->next)
    if (rf->name==name) return rf;
  return NIL;
}

/* ====================================================================

               Code for Executing Built-In RHS Functions

====================================================================  */

/* MVP 6-8-94 */
/* --------------------------------------------------------------------
			       User-Select
-------------------------------------------------------------------- */
Symbol *user_select_rhsfun (list *args) {
  Symbol *uselect;

  uselect = args->first;

  if (!strcmp(uselect->sc.name,"first")) {
    set_sysparam (USER_SELECT_MODE_SYSPARAM, USER_SELECT_FIRST);
    return NIL;
  }
/* AGR 615 begin */
  if (!strcmp(uselect->sc.name,"last")) {
    set_sysparam (USER_SELECT_MODE_SYSPARAM, USER_SELECT_LAST);
    return NIL;
  }
/* AGR 615 end */
  if ((!strcmp(uselect->sc.name,"ask")) ||
      (!strcmp(uselect->sc.name,"t"))) {
    set_sysparam (USER_SELECT_MODE_SYSPARAM, USER_SELECT_ASK);
    return NIL;
  }
  if ((!strcmp(uselect->sc.name,"random")) ||
      (!strcmp(uselect->sc.name,"nil"))) {
    set_sysparam (USER_SELECT_MODE_SYSPARAM, USER_SELECT_RANDOM);
    return NIL;
  }
  print ("Expected first, ask, or random for new value of user-select.\n");

  return NIL;
}

/* --------------------------------------------------------------------
                                Write

   Takes any number of arguments, and prints each one.
-------------------------------------------------------------------- */

Symbol *write_rhs_function_code (list *args) {
  Symbol *arg;
  char *string;
  
  for ( ; args!=NIL; args=args->rest) {
    arg = args->first;
    /* --- Note use of FALSE here--print the symbol itself, not a rereadable
       version of it --- */
    string = symbol_to_string (arg, FALSE, NIL);
    print_string (string);
  }
  return NIL;
}

/* --------------------------------------------------------------------
                                Crlf

   Just returns a sym_constant whose print name is a line feed.
-------------------------------------------------------------------- */

Symbol *crlf_rhs_function_code (list *args) {
  return make_sym_constant ("\n");
}

/* --------------------------------------------------------------------
                                Halt

   Just sets a flag indicating that the system has halted.
-------------------------------------------------------------------- */

Symbol *halt_rhs_function_code (list *args) {
  current_agent(system_halted) = TRUE;
  return NIL;
}

/* --------------------------------------------------------------------
                              Interrupt

   This causes an interrupt at the end of the current preference phase.
   It sets stop_soar to TRUE, and reason_for_stopping to an appropriate
   string.
-------------------------------------------------------------------- */

/* MVP 6-27-94 */

char * RHS_interrupt_msg = "*** RHS Function Interrupt ***";

Symbol *interrupt_rhs_function_code (list *args) {
  char *ch;
  
#ifdef MULTI_AGENT_ENABLED
  cons * c;
  agent * the_agent;

  global_agent->stop_soar = TRUE;	
  global_agent->reason_for_stopping =  RHS_interrupt_msg;

  for(c = all_soar_agents; c != NIL; c = c->rest) {
    the_agent = ((agent *) c->first);
    the_agent->stop_soar = TRUE;
    the_agent->reason_for_stopping =  RHS_interrupt_msg;
  }
#else
  current_agent(stop_soar) = TRUE;
#endif
  strcpy (current_agent(interrupt_source), "*** Interrupt from production ");
  ch = current_agent(interrupt_source);
  while (*ch) ch++;
  symbol_to_string (current_agent(production_being_fired)->name, TRUE, ch); 
  while (*ch) ch++;
  strcpy (ch, " ***");
  current_agent(reason_for_stopping) = current_agent(interrupt_source);
  return NIL;
}

/* --------------------------------------------------------------------
                         Make-constant-symbol

   Returns a newly generated sym_constant.  If no arguments are given,
   the constant will start with "constant".  If one or more arguments
   are given, the constant will start with a string equal to the
   concatenation of those arguments.
-------------------------------------------------------------------- */

Symbol *make_constant_symbol_rhs_function_code (list *args) {
  char buf[1000]; /* that ought to be long enough */
  char *string;
  cons *c;

  if (!args) {
    strcpy (buf, "constant");
  } else {
    buf[0] = 0;
    for (c=args; c!=NIL; c=c->rest) {
      string = symbol_to_string (c->first, FALSE, NIL);
      strcat (buf, string);
    }
  }
  if ((!args) && (!find_sym_constant (buf))) return make_sym_constant (buf);
  return generate_new_sym_constant (buf, &current_agent(mcs_counter));
}

/* --------------------------------------------------------------------
                               Timestamp

   Returns a newly generated sym_constant whose name is a representation
   of the current local time.
-------------------------------------------------------------------- */

Symbol *timestamp_rhs_function_code (list *args) {
  long now;
  struct tm *temp;
  char buf[100];

  now = time(NULL);
#ifdef THINK_C
  temp = localtime ((const time_t *)&now);
#else
#ifdef __SC__
  temp = localtime ((const time_t *)&now);
#else
  temp = localtime (&now);
#endif
#endif
  sprintf (buf, "%d/%d/%d-%02d:%02d:%02d",
           temp->tm_mon + 1, temp->tm_mday, temp->tm_year,
           temp->tm_hour, temp->tm_min, temp->tm_sec);
  return make_sym_constant (buf);
}

/* --------------------------------------------------------------------
                              Accept

   Waits for the user to type a line of input; then returns the first
   symbol from that line.
-------------------------------------------------------------------- */

Symbol *accept_rhs_function_code (list *args) {
  char buf[2000], *s;
  Symbol *sym;

  while (TRUE) {
    s = fgets (buf, 2000, stdin);
    if (!s) {
      /* s==NIL means immediate eof encountered or read error occurred */
      return NIL;
    }
    s = buf;
    sym = get_next_io_symbol_from_text_input_line (&s);
    if (sym) break;
  }
  symbol_add_ref (sym);
  release_io_symbol (sym); /* because it was obtained using get_io_... */
  return sym;
}


/* ---------------------------------------------------------------------
  Capitalize a Symbol
------------------------------------------------------------------------ */

#include <ctype.h>

Symbol * 
capitalize_symbol_rhs_function_code (list *args) 
{
  char * symbol_to_capitalize;
  Symbol * sym;

  if (!args) {
    print ("Error: 'capitalize-symbol' function called with no arguments.\n");
    return NIL;
  }

  sym = (Symbol *) args->first;
  if (sym->common.symbol_type != SYM_CONSTANT_SYMBOL_TYPE) {
    print_with_symbols ("Error: non-symbol (%y) passed to capitalize-symbol function.\n", sym);
    return NIL;
  }

  if (args->rest) {
    print ("Error: 'capitalize-symbol' takes exactly 1 argument.\n");
    return NIL;
  }

  symbol_to_capitalize = symbol_to_string(sym, FALSE, NIL);
  symbol_to_capitalize = savestring(symbol_to_capitalize);
  *symbol_to_capitalize = toupper(*symbol_to_capitalize);
  return make_sym_constant(symbol_to_capitalize);
}

/* AGR 520 begin     6-May-94 */
/* ------------------------------------------------------------
   2 general purpose rhs functions by Gary.
------------------------------------------------------------

They are invoked in the following manner, and I use them
to produce nice traces.

   (ifeq <a> <b> abc def)
and 
   (strlen <a>)

ifeq -- checks if the first argument is "eq" to the second argument
        if it is then it returns the third argument else the fourth.
        It is useful in similar situations to the "?" notation in C.
        Contrary to earlier belief, all 4 arguments are required.

        examples:
           (sp trace-juggling
            (goal <g> ^state.ball.position <pos>)
           -->
            (write (ifeq <pos> at-top        |    o    | ||) (crlf)
                   (ifeq <pos> left-middle   | o       | ||)
                   (ifeq <pos> right-middle  |       o | ||) (crlf)
                   (ifeq <pos> left-hand     |o        | ||)
                   (ifeq <pos> right-hand    |        o| ||) (crlf)
                                             |V_______V|    (crlf))
            )

            This outputs with a single production one of the following
            pictures depending on the ball's position (providing the ball
            is not dropped of course. Then it outputs empty hands. :-)

                                         o
                         o                              o
           o                                                           o
           V-------V    V-------V    V-------V    V-------V    V-------V
                     or           or           or           or


           for a ball that takes this path.

               o
            o     o
           o       o
           V-------V

           Basically this is useful when you don't want the trace to
           match the internal working memory structure.

strlen <val> - returns the string length of the output string so that
               one can get the output to line up nicely. This is useful
               along with ifeq when the output string varies in length.

           example:

              (strlen |abc|) returns 3

              (write (ifeq (strlen <foo>) 3 | | ||)
                     (ifeq (strlen <foo>) 2 |  | ||)
                     (ifeq (strlen <foo>) 1 |   | ||) <foo>)

                  writes foo padding on the left with enough blanks so that
                  the length of the output is always at least 4 characters.

------------------------------------------------------------ */

Symbol *ifeq_rhs_function_code (list *args) {
  Symbol *arg1, *arg2;
  cons *c;

  if (!args) {
    print ("Error: 'ifeq' function called with no arguments\n");
    return NIL;
  }

  /* --- two or more arguments --- */
  arg1 = args->first;
  c=args->rest;
  arg2 = c->first;
  c=c->rest;

  if (arg1 == arg2)
    {
      symbol_add_ref((Symbol *)(c->first));
      return c->first;
    }
  else if (c->rest)
    {
      symbol_add_ref((Symbol *)(c->rest->first));
      return c->rest->first;
    }
  else return NIL;
}


Symbol *strlen_rhs_function_code (list *args) {
  Symbol *arg;
  char *string;

  arg = args->first;

  /* --- Note use of FALSE here--print the symbol itself, not a rereadable
     version of it --- */
  string = symbol_to_string (arg, FALSE, NIL);

  return make_int_constant (strlen(string));
}
/* AGR 520     end */


/* ====================================================================

                  Initialize the Built-In RHS Functions

====================================================================  */

void init_built_in_rhs_functions (void) {
  add_rhs_function (make_sym_constant ("write"), write_rhs_function_code,
                    -1, FALSE, TRUE);
  add_rhs_function (make_sym_constant ("crlf"), crlf_rhs_function_code,
                    0, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("halt"), halt_rhs_function_code,
                    0, FALSE, TRUE);
  add_rhs_function (make_sym_constant ("interrupt"),
                    interrupt_rhs_function_code,
                    0, FALSE, TRUE);
  add_rhs_function (make_sym_constant ("make-constant-symbol"),
                    make_constant_symbol_rhs_function_code,
                    -1, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("timestamp"),
                    timestamp_rhs_function_code,
                    0, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("accept"), accept_rhs_function_code,
                    0, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("capitalize-symbol"),
		    capitalize_symbol_rhs_function_code,
		    1,
		    TRUE,
		    FALSE);
/* AGR 520  begin */
  add_rhs_function (make_sym_constant ("ifeq"), ifeq_rhs_function_code,
		    4, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("strlen"), strlen_rhs_function_code,
		    1, TRUE, FALSE);
/* AGR 520  end   */

  init_built_in_rhs_math_functions();
}
