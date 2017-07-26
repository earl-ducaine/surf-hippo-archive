/*
 * $Id: rhsfun_math.c,v 1.4 1994/11/23 16:40:34 rempel Exp $
 * $Log: rhsfun_math.c,v $
 * Revision 1.4  1994/11/23  16:40:34  rempel
 * for 6.2.4
 *
 * Revision 1.3  1994/08/23  10:38:05  portelli
 * For 6.2.4
 *
 * Revision 1.2  1993/11/21  22:14:11  portelli
 * 6.1.1 checkin
 *
 *
 * Revision 1.1  93/11/21  17:00:25  soarhack
 * initial checkin
 */


#include <math.h>
#include "soar.h"

/* --------------------------------------------------------------------
                                Plus

   Takes any number of int_constant or float_constant arguments, and
   returns their sum.
-------------------------------------------------------------------- */

Symbol *plus_rhs_function_code (list *args) {
  bool float_found;
  long i;
  float f = 0;
  Symbol *arg;
  cons *c;

  for (c=args; c!=NIL; c=c->rest) {
    arg = c->first;
    if ((arg->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) &&
        (arg->common.symbol_type != FLOAT_CONSTANT_SYMBOL_TYPE)) {
      print_with_symbols ("Error: non-number (%y) passed to + function\n",
                          arg);
      return NIL;
    }
  }

  i = 0;
  float_found = FALSE;
  while (args) {
    arg = args->first;
    if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) {
      if (float_found) f += arg->ic.value;
      else i += arg->ic.value;
    } else {
      if (float_found) f += arg->fc.value;
      else { float_found = TRUE; f = arg->fc.value + i; }
    }
    args = args->rest;
  }
  if (float_found) return make_float_constant (f);
  return make_int_constant (i);
}

/* --------------------------------------------------------------------
                                Times

   Takes any number of int_constant or float_constant arguments, and
   returns their product.
-------------------------------------------------------------------- */

Symbol *times_rhs_function_code (list *args) {
  bool float_found;
  long i;
  float f = 0;
  Symbol *arg;
  cons *c;
  
  for (c=args; c!=NIL; c=c->rest) {
    arg = c->first;
    if ((arg->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) &&
        (arg->common.symbol_type != FLOAT_CONSTANT_SYMBOL_TYPE)) {
      print_with_symbols ("Error: non-number (%y) passed to * function\n",
                          arg);
      return NIL;
    }
  }

  i = 1;
  float_found = FALSE;
  while (args) {
    arg = args->first;
    if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) {
      if (float_found) f *= arg->ic.value;
      else i *= arg->ic.value;
    } else {
      if (float_found) f *= arg->fc.value;
      else { float_found = TRUE; f = arg->fc.value * i; }
    }
    args = args->rest;
  }
  if (float_found) return make_float_constant (f);
  return make_int_constant (i);
}

/* --------------------------------------------------------------------
                                Minus

   Takes one or more int_constant or float_constant arguments.
   If 0 arguments, returns NIL (error).
   If 1 argument (x), returns -x.
   If >=2 arguments (x, y1, ..., yk), returns x - y1 - ... - yk.
-------------------------------------------------------------------- */

Symbol *minus_rhs_function_code (list *args) {
  Symbol *arg;
  float f;
  long i;
  cons *c;
  bool float_found;

  if (!args) {
    print ("Error: '-' function called with no arguments\n");
    return NIL;
  }
  
  for (c=args; c!=NIL; c=c->rest) {
    arg = c->first;
    if ((arg->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) &&
        (arg->common.symbol_type != FLOAT_CONSTANT_SYMBOL_TYPE)) {
      print_with_symbols ("Error: non-number (%y) passed to - function\n",
                          arg);
      return NIL;
    }
  }

  if (! args->rest) {
    /* --- only one argument --- */
    arg = args->first;
    if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE)
      return make_int_constant (- arg->ic.value);
    return make_float_constant (- arg->fc.value);
  }

  /* --- two or more arguments --- */
  arg = args->first;
  float_found = FALSE;
  if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) i = arg->ic.value;
  else { float_found = TRUE; f = arg->fc.value; }
  for (c=args->rest; c!=NIL; c=c->rest) {
    arg = c->first;
    if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) {
      if (float_found) f -= arg->ic.value;
      else i -= arg->ic.value;
    } else {
      if (float_found) f -= arg->fc.value;
      else { float_found = TRUE; f = i - arg->fc.value; }
    }
  }
 
  if (float_found) return make_float_constant (f);
  return make_int_constant (i);
}

/* --------------------------------------------------------------------
                     Floating-Point Division

   Takes one or more int_constant or float_constant arguments.
   If 0 arguments, returns NIL (error).
   If 1 argument (x), returns 1/x.
   If >=2 arguments (x, y1, ..., yk), returns x / y1 / ... / yk.
-------------------------------------------------------------------- */

Symbol *fp_divide_rhs_function_code (list *args) {
  Symbol *arg;
  float f;
  cons *c;

  if (!args) {
    print ("Error: '/' function called with no arguments\n");
    return NIL;
  }
  
  for (c=args; c!=NIL; c=c->rest) {
    arg = c->first;
    if ((arg->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) &&
        (arg->common.symbol_type != FLOAT_CONSTANT_SYMBOL_TYPE)) {
      print_with_symbols ("Error: non-number (%y) passed to / function\n",
                          arg);
      return NIL;
    }
  }

  if (! args->rest) {
    /* --- only one argument --- */
    arg = args->first;
    if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) f = arg->ic.value;
    else f = arg->fc.value;
    if (f != 0.0) return make_float_constant (1.0 / f);
    print ("Error: attempt to divide ('/') by zero.\n");
    return NIL;
  }

  /* --- two or more arguments --- */
  arg = args->first;
  if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) f = arg->ic.value;
  else f = arg->fc.value;
  for (c=args->rest; c!=NIL; c=c->rest) {
    arg = c->first;
    if (arg->common.symbol_type==INT_CONSTANT_SYMBOL_TYPE) {
      if (arg->ic.value) f /= arg->ic.value;
      else { print ("Error: attempt to divide ('/') by zero.\n"); return NIL; }
    } else {
      if (arg->fc.value != 0.0) f /= arg->fc.value;
      else { print ("Error: attempt to divide ('/') by zero.\n"); return NIL; }
    }
  }
  return make_float_constant (f);
}

/* --------------------------------------------------------------------
                     Integer Division (Quotient)

   Takes two int_constant arguments, and returns their quotient.
-------------------------------------------------------------------- */

Symbol *div_rhs_function_code (list *args) {
  Symbol *arg1, *arg2;

  arg1 = args->first;
  arg2 = args->rest->first;
  
  if (arg1->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) {
    print_with_symbols ("Error: non-integer (%y) passed to div function\n",
                        arg1);
    return NIL;
  }
  if (arg2->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) {
    print_with_symbols ("Error: non-integer (%y) passed to div function\n",
                        arg2);
    return NIL;
  }

  if (arg2->ic.value == 0) {
    print ("Error: attempt to divide ('div') by zero.\n");
    return NIL;
  }
  
  return make_int_constant (arg1->ic.value / arg2->ic.value);
 /* Warning: ANSI doesn't say precisely what happens if one or both of the
    two args is negative. */
}

/* --------------------------------------------------------------------
                          Integer Modulus

   Takes two int_constant arguments (x,y) and returns (x mod y), i.e.,
   the remainder after dividing x by y.
-------------------------------------------------------------------- */

Symbol *mod_rhs_function_code (list *args) {
  Symbol *arg1, *arg2;

  arg1 = args->first;
  arg2 = args->rest->first;
  
  if (arg1->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) {
    print_with_symbols ("Error: non-integer (%y) passed to mod function\n",
                        arg1);
    return NIL;
  }
  if (arg2->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE) {
    print_with_symbols ("Error: non-integer (%y) passed to mod function\n",
                        arg2);
    return NIL;
  }

  if (arg2->ic.value == 0) {
    print ("Error: attempt to divide ('mod') by zero.\n");
    return NIL;
  }
  
  return make_int_constant (arg1->ic.value % arg2->ic.value);
 /* Warning:  ANSI guarantees this does the right thing if both args are
    positive.  If one or both is negative, it only guarantees that
    (a/b)*b + a%b == a. */
}

/*
 * SIN_RHS_FUNCTION_CODE
 *
 * Returns as a float the sine of an angle measured in radians.
 */
Symbol *sin_rhs_function_code(list *args)
{
    Symbol *arg;
    float  arg_value;

    if (!args) {
	print("Error: 'sin' function called with no arguments\n");
	return NIL;
    }

    arg = args->first;
    if (arg->common.symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
	arg_value = arg->fc.value;
    else if (arg->common.symbol_type == INT_CONSTANT_SYMBOL_TYPE)
	arg_value = (float) arg->ic.value;
    else {
	print_with_symbols("Error: 'sin' function called with non-numeric argument %y\n", arg);
	return NIL;
    }

    return make_float_constant(sin(arg_value));
}


/*
 * COS_RHS_FUNCTION_CODE
 *
 * Returns as a float the cosine of an angle measured in radians.
 */
Symbol *cos_rhs_function_code(list *args)
{
    Symbol *arg;
    float  arg_value;

    if (!args) {
	print("Error: 'cos' function called with no arguments\n");
	return NIL;
    }

    arg = args->first;
    if (arg->common.symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
	arg_value = arg->fc.value;
    else if (arg->common.symbol_type == INT_CONSTANT_SYMBOL_TYPE)
	arg_value = (float) arg->ic.value;
    else {
	print_with_symbols("Error: 'cos' function called with non-numeric argument %y\n", arg);
	return NIL;
    }
    return make_float_constant(cos(arg_value));
}


/*
 * SQRT_RHS_FUNCTION_CODE
 *
 * Returns as a float the square root of its argument (integer or float).
 */
Symbol *sqrt_rhs_function_code(list *args)
{
    Symbol *arg;
    float  arg_value;

    if (!args) {
	print("Error: 'sqrt' function called with no arguments\n");
	return NIL;
    }

    arg = args->first;
    if (arg->common.symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
	arg_value = arg->fc.value;
    else if (arg->common.symbol_type == INT_CONSTANT_SYMBOL_TYPE)
	arg_value = (float)arg->ic.value;
    else {
	print_with_symbols("Error: 'sqrt' function called with non-numeric argument %y\n", arg);
	return NIL;
    }
    return make_float_constant(sqrt(arg_value));
}


/*
 * ATAN2_RHS_FUNCTION_CODE
 *
 * Returns as a float in radians the arctangent of (first_arg/second_arg)
 * which are floats or integers.
 */
Symbol *atan2_rhs_function_code(list *args)
{
    Symbol *arg;
    cons *c;
    float  numer_value,
           denom_value;

    if (!args) {
	print("Error: 'atan2' function called with no arguments\n");
	return NIL;
    }

    for (c=args; c!=NIL; c=c->rest) {
	arg = c->first;
	if (   (arg->common.symbol_type != INT_CONSTANT_SYMBOL_TYPE)
	    && (arg->common.symbol_type != FLOAT_CONSTANT_SYMBOL_TYPE)) {
	    print_with_symbols ("Error: non-number (%y) passed to atan2\n",
				arg);
	    return NIL;
	}
    }

    if (!args->rest) {
	print("Error: 'atan2' function called with only one argument\n");
	return NIL;
    }

    arg = args->first;
    if (arg->common.symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
	numer_value = arg->fc.value;
    else
	numer_value = (float) arg->ic.value;

    c = args->rest;
    if (c->rest) {
	print("Error: 'atan2' function called with more than two arguments.\n");
	return NIL;
    }
    arg = c->first;
    if (arg->common.symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
	denom_value = arg->fc.value;
    else
	denom_value = (float) arg->ic.value;

    return make_float_constant(atan2(numer_value, denom_value));
}


/*
 * ABS_RHS_FUNCTION_CODE
 *
 * Returns the absolute value of a float as a float, of an int as an int.
 */
Symbol *abs_rhs_function_code(list *args)
{
    Symbol *arg,
           *return_value;
    float  arg_value;

    if (!args) {
	print("Error: 'abs' function called with no arguments\n");
	return NIL;
    }

    arg = args->first;
    if (arg->common.symbol_type == FLOAT_CONSTANT_SYMBOL_TYPE)
	return_value = make_float_constant(fabs(arg->fc.value));
    else if (arg->common.symbol_type == INT_CONSTANT_SYMBOL_TYPE)
	return_value = make_int_constant((arg->ic.value<0) ? -arg->ic.value : arg->ic.value);
    else {
	print_with_symbols("Error: 'abs' function called with non-numeric argument %y\n", arg);
	return NIL;
    }
    return return_value;
}


/* ====================================================================

                  Initialize the Built-In RHS Math Functions

====================================================================
*/

void init_built_in_rhs_math_functions (void)
{
  add_rhs_function (make_sym_constant ("+"), plus_rhs_function_code,
                    -1, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("*"), times_rhs_function_code,
                    -1, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("-"), minus_rhs_function_code,
                    -1, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("/"), fp_divide_rhs_function_code,
                    -1, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("div"), div_rhs_function_code,
                    2, TRUE, FALSE);
  add_rhs_function (make_sym_constant ("mod"), mod_rhs_function_code,
                    2, TRUE, FALSE);

  add_rhs_function (make_sym_constant("sin"),
		    sin_rhs_function_code,
		    1,
		    TRUE,
		    FALSE);
  add_rhs_function (make_sym_constant("cos"),
		    cos_rhs_function_code,
		    1,
		    TRUE,
		    FALSE);
  add_rhs_function (make_sym_constant("atan2"),
		    atan2_rhs_function_code,
		    2,
		    TRUE,
		    FALSE);
  add_rhs_function (make_sym_constant("sqrt"),
		    sqrt_rhs_function_code,
		    1,
		    TRUE,
		    FALSE);
  add_rhs_function (make_sym_constant("abs"),
		    abs_rhs_function_code,
		    1,
		    TRUE,
		    FALSE);
}

