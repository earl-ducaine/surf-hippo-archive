/*  -*- Mode: C -*-  */
/************************************************************************
/*
/* File            : soar-socket.c
/* Author          : Joe Mertz
/* Created On      : December 1992
/* Last Modified By: Roberto L. Ong <rlo@psyc.nott.ac.uk>
/* Last Modified On: Wed Feb 01 15:23:20 1995
/* Update Count    : 30
/* Associated modules : std-soar-socket.c
/*                    : ../src/*  (i.e., the rest of Soar6)
/*
/* PURPOSE
/*    This file implements a simple socket I/O facility for Soar6.  It lets 
/*    the user type the following new Soar interface commands for initiating
/*    socket communication.
/*
/*              init-socket-io
/*              init-socket-server
/*              close-socket-io
/*              shutdown-socket-io
/*              socket-output-link
/*
/*    See "help" for each command in Soar6 for more information, or the code
/*    below.
/*
/* Changes necessary in HOOKS.C:
/*    stdsocket_init() must be called from system_startup_hook() 
/*    stdsocket_flush() must be called from before_init_soar_hook()
/*
/* Also, remember - to include this in Soar, you must add soar-socket.c and 
/* std-soar-socket.c to the makefile and recompile Soar by typing "make" in 
/* Soar directory.
/* 
/* LOCATION
/*     This code together with other codes is packaged together as a socket 
/*     support utility called MONGSU. They can be downloaded via anonymous
/*     FTP from host 128.243.40.7 (unicorn.ccc.nott.ac.uk, but many machines
/*     don't know it, so you may wish to use the numbers) in the directory 
/*     "/pub/lpzfr" (From within ftp only the part of the tree rooted at 
/*     /usr/ftp is visible).
/*
/* TO COMPILE: 
/*    Use the Soar "make.body", which is contained in the Makefile.
/*
/*  TABLE OF CONTENTS
/* 
/*    i.    Caveats
/*    I.    Initializations
/*    II.   Output Routine
/*    III.  Soar Interface Commands
/*    IV.   Input Routine 
/*    V.    Utilities
/*    VI.   Help
/*    VII.  Declarations and Definitions
/*
/* i. Caveats
/*
/* Some sizes to keep in mind:
/*  1024 chars   Max path length -RLO (13Dec94)
/*    15 chars   Max length server name -RLO (19Aug94)
/*    15 chars   Max length server port number -RLO (12Dec94)
/*  1024 chars   Max length of single output wme list
/*  2048 chars   Max length of single input wme list
/*   128 chars   Max length of any input attribute or value constant
/*   512 tokens  Max number per single input list.
/*                   Token = '(' | ')' | attribute constant | value constant
/*
/* That should be all of them.  I think that these are reasonably 
/* large, and making them dynamic was more work than I wanted to do 
/* at this point.  But if you start blowing up, you might check 
/* these hardcoded barriers first (there is no checking in the code).
/*
/* Error recovery from incoherent input lists is not very strong.
/*
/* "When working with cT, and other systems where Soar is directing the action,
/* Soar as a server works well. Some of these systems can not really work well 
/* as a server, but can interface to a server easily.
/*
/* When working with multiple Soars or Soar and other agent-like systems and 
/* and all these agents talking to some common interface, Soar as a client 
/* works better."
/* -GAP (September, 1994)
/*
/* SECTIONS:
/*  (C) Copyright 1991, Carnegie Mellon University, all rights reserved.
/*  (C) Other sections Copyright 1995, Roberto L. Ong and Frank E. Ritter.
/***************************************************************************/
/* HISTORY
/* ??Dec92 - started hacking
/* 01Feb93 - added this header
/* 16Jun93 - added ability to have (foo ()) which will translate into
/*           an attribute foo with an id as its value.
/* 03Feb93 - seriously bumped up the limits listed above to accomodate the
/*           posttest test.  Used respectively: 1024 30000 128 5000
/* 19Aug94 - added header files for Soar global variables
/* (RLO)     modified init_socket_io to act as a server and allow parameters
/*                    to be typed in the Soar command line
/*           modified variables to make it compatible to Soar6.2.3
/*           modified init-socket-io for Soar to act as client
/*           added init-socket-server for Soar to act as server
/*           modified functions to work in Soar 6.2.3
/*           added shutdown-socket-io 
/* 07Dec94 - changed variable names for it to run in NNPSCM
/* (RLO)     modified to allow it to compile in both NNPSCM and NON-NNPSCM 
/* 04Jan95 - provided auto-socket hookup utility through a SOCKET-PROCESS-FILE
/* (RLO)    
/* 11Jan95 - added comments on the SOCKET-PROCESS-FILE
/* (RLO)    
/* 01Feb95 - final clean-up of the code
/* (RLO)    
/* 22May95 - sorted out bug of overflowing wme_list_buf array
/* (GDB)
/* 05Jun95 - error messages now begin on a new line and start with >>>MONGSU<<<
/* (GDB)
/************************************************************************/


/*                          */
/*   I.   Initializations   */
/*                          */

#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
#include <strings.h>
#include <fcntl.h>
#include <ctype.h>
#include <errno.h>
#include <unistd.h>
#include "soar.h"

/* The following external functions are defined in socket.c. */
extern int create_socket (void);
extern int connect_socket (int, char*, int);
extern int bind_socket (int);
extern int listen_socket (int);
extern int accept_socket (int);
extern int close_socket (int);
extern int shutdown_socket (int, int);
extern char *itoa (int);

/* Definition of constants */
#define MAX_PATH_LENGTH 1024         /* from sys/param.h */
                                     /* -RLO (13Dec94) */
#define MAX_SERVER_NAME_LENGTH 15    /* max length of server name */
                                     /* -RLO (19Aug94) */
#define MAX_SERVER_PORT_NO_LENGTH 15 /* max length of server port number */
                                     /* -RLO (13Dec94) */
#define SOCKET_PROCESS_FILE ".socket.process" /* socket process file name */
                                              /* -RLO (13Dec94) */
#define WME_LIST_BUF_LENGTH 256      /* made constant, increased from 102 */
                                     /* -GDB (22May95)*/

int client_id = -1;  /* file descriptor for client process             */
                     /* if client_id == -1, then the socket is not up  */
                     /* initialize for use with stdsocket_flush during */
                     /* init-soar       */
                     /* -RLO (31Aug94) */

int socket_id;       /* file descriptor for server process    */
                     /* This is used if Soar acts as a server */
                     /* -RLO (31Aug94) */

char socket_process_filename [MAX_PATH_LENGTH]; /* complete pathname of the */
                                                /* socket-process-file in   */
                                                /* the home directory.      */
                                                /* -RLO (04Jan95)           */

/* OUTPUT STUFF */
char wme_list_buf[WME_LIST_BUF_LENGTH]; /* where wme lists are built for o/p */

/* INPUT STUFF */
char in_buf[30000];   /* where whole input lists are read and staged */
int in_nest;          /* the level of nesting currently being input  */
int in_count;         /* the character count in the current list being input */
char in_string[128];  /* where each input token is built */
int in_string_idx;    /* end of input token being built  */
int in_integer, in_float, in_sym_constant;  /* use to decide token type */

typedef enum paren_type_enum {LEFTPAREN, RIGHTPAREN} paren_type;

typedef union in_token_union 
{
  Symbol *in_symbol;
  paren_type paren;
} in_token;

in_token in_tokens[5000];

typedef struct in_list_struct 
{
  Symbol *link_symbol;
  wme *link_wme;
  struct in_list_struct *next;
} in_list;

in_list *in_links;

int in_token_last;
int in_token_next;

/* Routines defined in this file */
void stdsocket_init (void);
void stdsocket_flush (void);
void socketout (int, io_wme*);
void parse_output (Symbol*, io_wme*);
bool init_socket_io (void);
bool init_socket_server (void);
bool close_socket_io (void);
bool shutdown_socket_io (void);
void socketin (int);
void parse_input (void);
void build_wmes (Symbol*);
void buildtoken (char);
void endtoken (void);
void paren (paren_type);
in_token get_input_token (void);
in_token get_input_token_paren (void);
in_token get_input_token_symbol (void);
bool is_in_token_paren (in_token);
bool is_in_token_symbol (in_token);
bool socket_output_link (void);


/***** stdsocket_flush: read any input on socket that may be pending *******/
void stdsocket_flush (void) 
{
  if (client_id == -1) return;  /* flush only if the socket is open */

  while(TRUE) { /* read until error or when input blocked */
    char next_char;
    if (read(client_id, &next_char, 1) == -1)
      if (errno == EWOULDBLOCK)  /* poll shows nothing to read on socket */
	return;
      else {
	perror ("ERROR message ");
	return;
      }
  }
}


/*                          */
/*   II.   Output Routine   */
/*                          */

/*********** socketout:  output routine called by Soar **********/
void socketout (int mode, io_wme *outputs) 
{
  /* currently unused in this function, unless */
  /* modifications were done on the way lists  */
  /* are put to working memory                 */
  /* -RLO (24Jan95)                            */
  /* Symbol *socketout_link_id, *output_link_attr; */
  
  /* currently only care about ADDED_OUTPUT_COMMAND */
  switch (mode) 
    {
    case REMOVED_OUTPUT_COMMAND:
      printf ("\n>>>MONGSU<<< This is remove_output_command\n");
      return;
    case MODIFIED_OUTPUT_COMMAND: 
      printf ("\n>>>MONGSU<<< This is modified_output_command\n");
      return;
    case ADDED_OUTPUT_COMMAND: break;
    }

/* The statement below converts something like: */
/* (X1 ^foo Z1)                                 */
/* (Z1 ^bar noo)                                */
/* (S1 ^socketout-link X1)                      */
/* and turns it into                            */
/* (foo (bar noo))                              */
/* If the above output is desired               */
/* (a) uncomment the statement below            */
/* (b) change arguments of parse_output to (socketout_link_id, outputs) */
/* -RLO (19Aug94)                             */
/* #ifdef NNPSCM
/* socketout_link_id = get_output_value (outputs, current_agent(io_header), NIL);  */
/* #endif
/* #ifndef NNPSCM
/* socketout_link_id = get_output_value (outputs, current_agent(top_state), NIL);  */
/* #endif

  /* take something like:	      */
  /* (X1 ^foo Z1)                     */
  /* (Z1 ^bar noo)                    */
  /* (S1 ^socketout-link X1)          */
  /* and turn it into                 */
  /* (socketout-link (foo (bar noo))) */
  
  /* reset the buffer "wme_list_buf" for building the nested wme list */
  wme_list_buf[0] = '\0';

#ifdef NNPSCM
  parse_output (current_agent(io_header), outputs);
#endif

#ifndef NNPSCM
  parse_output (current_agent(top_state), outputs);
#endif

  strcat (wme_list_buf, " "); 
  /* put extra space on end for dumb lisp reader*/
  if (write (client_id, wme_list_buf, strlen(wme_list_buf) ) == -1 ) {
    printf("\n>>>MONGSU<<<soar-socket.c: Unable to write to fd %d.\n",
	   client_id); 
  }
  printf("\n>>>MONGSU<<<write successful socket %d\n", client_id);
}


/****** parse_output:  take output wme's and turn in to nested list *****/

/* GDB 22May95 - code added to check for overfilling wme_list_buf */

void parse_output (Symbol *id, io_wme *outputs)
{
  io_wme *next_wme;

  for (next_wme = outputs ; next_wme != NIL ; next_wme = next_wme->next) 
    {
      if (next_wme->id == id) {
	
	/* start with attribute */
	strcat (wme_list_buf, "(");
	strcat (wme_list_buf, next_wme->attr->sc.name);
	
	/* add value and finally right paren */
	switch (next_wme->value->common.symbol_type) 
	  {
	  case VARIABLE_SYMBOL_TYPE:
	    print ("\n>>>MONGSU<<<whoops! a variable as a value.\n");
	    break;
	  case IDENTIFIER_SYMBOL_TYPE:
	    strcat (wme_list_buf, " ");
	    parse_output (next_wme->value, outputs);
	    if (strlen (wme_list_buf) > WME_LIST_BUF_LENGTH) {
	      printf ("\n>>>MONGSU<<< ERROR: wme_list_buf: array size too small.\n");
	      printf ("\n>>>MONGSU<<< Current size: %d Array size: %d\n",
		      strlen(wme_list_buf), WME_LIST_BUF_LENGTH);
	    }
	    break;
	  case SYM_CONSTANT_SYMBOL_TYPE:
	    strcat (wme_list_buf, " ");
	    strcat (wme_list_buf, next_wme->value->sc.name);
	    if (strlen (wme_list_buf) > WME_LIST_BUF_LENGTH) {
	      printf ("\n>>>MONGSU<<< ERROR: wme_list_buf: array size too small.\n");
	      printf ("\n>>>MONGSU<<< Current size: %d Array size: %d\n",
		      strlen(wme_list_buf), WME_LIST_BUF_LENGTH);
	    }
	    break;
	  case INT_CONSTANT_SYMBOL_TYPE:
	    sprintf (wme_list_buf+strlen(wme_list_buf), " %u",
		     next_wme->value->ic.value);
	    if (strlen (wme_list_buf) > WME_LIST_BUF_LENGTH) {
	      printf ("\n>>>MONGSU<<< ERROR: wme_list_buf: array size too small.\n");
	      printf ("\n>>>MONGSU<<< Current size: %d Array size: %d\n",
		      strlen(wme_list_buf), WME_LIST_BUF_LENGTH);
	    }
	    break;
	  case FLOAT_CONSTANT_SYMBOL_TYPE:
	    sprintf (wme_list_buf+strlen(wme_list_buf), " %f",
		     next_wme->value->fc.value);
	    if (strlen (wme_list_buf) > WME_LIST_BUF_LENGTH) {
	      printf ("\n>>>MONGSU<<< ERROR: wme_list_buf: array size too small.\n");
	      printf ("\n>>>MONGSU<<< Current size: %d Array size: %d\n",
		      strlen(wme_list_buf), WME_LIST_BUF_LENGTH);
	    }
	    break;
	  }
	strcat(wme_list_buf, ")");
	if (strlen (wme_list_buf) > WME_LIST_BUF_LENGTH) {
	  printf ("\n>>>MONGSU<<< ERROR: wme_list_buf: array size too small.\n");
	  printf ("\n>>>MONGSU<<< Current size: %d Array size: %d\n",
		  strlen(wme_list_buf), WME_LIST_BUF_LENGTH);
	}

      }
    }
}


/*                                    */
/*   III.   Soar Interface Commands   */
/*                                    */

/***** init_socket_io:  interface command to initialize socket *****/
/* Syntax: init-socket-io -RLO (02Jan95) */
/* This allows Soar to act as a client */
bool init_socket_io (void)
{
  FILE *fptr;
  int server_port_no;
  char server_name [MAX_SERVER_NAME_LENGTH];
  char temp_server_name [MAX_SERVER_NAME_LENGTH];
  char temp_server_port_no [MAX_SERVER_PORT_NO_LENGTH];
  char filename [MAX_PATH_LENGTH];
  char *home_directory;


  get_lexeme();  /* consume "init_socket_io", advance to arguments */
  
  /* create a raw socket */ 
  if ((client_id = create_socket())  == -1) return FALSE;

  /* read socket-process file for automatic hookup to server process */
  /* -RLO (13Dec94) */
  home_directory = getenv ("HOME");
  if (home_directory)
    {
      strcpy (filename, home_directory);
      strcat (filename, "/");
      strcat (filename, SOCKET_PROCESS_FILE);
    }

  if ((fptr = fopen(filename, "r")) == NULL) 
    {
      printf ("\n>>>MONGSU<<< Unable to open %s file.
ERROR: There is no existing server socket.\n", filename);
      return FALSE;
    }

  /* read server_name and server_port_no from SOCKET_PROCESS_FILE */
  /* -RLO (13Dec94) */
  fscanf(fptr, "%[^\n]\n %s", &temp_server_name, &temp_server_port_no);
  server_port_no = atoi (temp_server_port_no);
  fclose(fptr);

  strcpy(server_name, temp_server_name);
  printf("\n>>>MONGSU<<< Connecting to server with name %s\n", server_name);

  printf("\n>>>MONGSU<<< Connecting to server with port #%d\n", server_port_no);
  fflush(fptr);

  /* connect to named server */
  if ((connect_socket(client_id, server_name, server_port_no)) == -1)
    return FALSE;

  printf("\n>>>MONGSU<<< Connected to socket %d", client_id);
  /* use non-blocking i/o to stream */
  if ((fcntl (client_id, F_SETFL, FNDELAY)) == -1) return FALSE;

  /* do the rest of initialization: */
  in_nest = 0;
  in_buf[0] = '\0';
  in_count = 0;
  in_links = NIL;
  return TRUE;
}


/********** init_socket_server:  interface command to initialize socket ***/
/* Syntax: init-socket-server  -RLO (31Aug94) */
/* Use this function to make Soar act as a server. */
bool init_socket_server (void) 
{
  FILE *fptr;
  int server_port_no;
  char *server_name;
  char filename [MAX_PATH_LENGTH];
  char *temp_server_port_no, *home_directory;
  char *comment = 
    "\n;; This file was created by the server process that contains
;; the <hostname> and <port-number>. This will be read by
;; the client process for use in connecting to the server
;; socket.
;; This file will always be deleted after the server socket
;; is shutdown.
;; -RLO (11Jan95)";

  
  get_lexeme();  /* consume command */
  
  if (client_id != -1) {  /* if the socket is already open, close first */
    print ("\n>>>MONGSU<<< First close previous socket connection.\n");
    return FALSE;
  }
  
  if ((socket_id = create_socket())  == -1 ) return FALSE;

  if ((server_port_no = bind_socket(socket_id)) == -1) return FALSE;

  /* write to socket-process file for automatic hookup by client process */
  /* -RLO (13Dec94) */
  home_directory = getenv ("HOME");
  if (home_directory) 
    {
      strcpy (filename, home_directory);
      strcat (filename, "/");
      strcat (filename, SOCKET_PROCESS_FILE);
    }

  strcpy (socket_process_filename, filename);

  if ((fptr = fopen(filename, "w")) == NULL) 
    {
      printf ("\n>>>MONGSU<<< ERROR: Unable to open %s\n.", filename);
      return FALSE;
    }
  
  /* write server_name and server_port_no to SOCKET_PROCESS_FILE */
  /* -RLO (13Dec94) */
  server_name = getenv ("HOSTNAME");
  fprintf (fptr, server_name);
  fprintf (fptr, "\n");
  temp_server_port_no = itoa (server_port_no);
  fprintf (fptr, temp_server_port_no);
  fprintf (fptr, "\n");
  fprintf (fptr, comment);
  fclose(fptr);

  /* Put the socket in "listen" state (allow only one connection) */
  if (listen_socket(socket_id) == -1) return FALSE;
  print ("\n>>>MONGSU<<< Waiting for connection...\n");
  fflush(fptr);
  
  if ((client_id = accept_socket (socket_id)) == -1 ) return FALSE;
  
  /* use non-blocking i/o to stream */
  if ((fcntl (client_id, F_SETFL, FNDELAY)) == -1) return FALSE;
  
  print ("\n>>>MONGSU<<< Connection complete.\n");
  
  /* do the rest of initialization: */

  in_nest = 0;
  in_buf[0] = '\0';
  in_count = 0;
  in_links = NIL;
  return TRUE;
}

/********** close_socket_io:  interface command to close socket *****/
/* Syntax: close-socket-io */
bool close_socket_io (void)
{
  get_lexeme();  /* consume command */

  /* clean up */
  while (in_links) {
    in_list *x;
    remove_input_wme (in_links->link_wme);
    release_io_symbol (in_links->link_symbol);
    x = in_links;
    in_links = in_links->next;
    free_memory (x, MISCELLANEOUS_MEM_USAGE);
  }
  
  if (close_socket(client_id) == -1) return FALSE;
  client_id = -1;
  return TRUE;
}


/********** shutdown_socket_io:  interface command to close socket *****/
/* Syntax: shutdown-socket-io */
/* - RLO (15Sep94) */
bool shutdown_socket_io (void)
{
  get_lexeme();  /* consume command */

  /* clean up */
  while (in_links) {
    in_list *x;
    remove_input_wme (in_links->link_wme);
    release_io_symbol (in_links->link_symbol);
    x = in_links;
    in_links = in_links->next;
    free_memory (x, MISCELLANEOUS_MEM_USAGE);
  }
  
  if (shutdown_socket(socket_id, 2) == -1) return FALSE;
  if (close_socket(socket_id) == -1) return FALSE;
  client_id = -1; 

  /* removes the socket_process_filename from your home directory */
  /* - RLO (04Jan95) */
  remove(socket_process_filename);

  return TRUE;
}

/****** socket_output_link:  interface command to set output link *****/
/* Syntax: socket-output-link <output-link-name> */
bool socket_output_link (void) 
{
  get_lexeme();  /* consume command */

  while (current_agent(lexeme).type != R_PAREN_LEXEME) {
    if (current_agent(lexeme).type == SYM_CONSTANT_LEXEME) {
      add_output_function (current_agent(lexeme).string, socketout);
      get_lexeme();
    } else {
      printf("\n>>>MONGSU<<< Expected an output link name.\n");
      print_location_of_most_recent_lexeme();
      return FALSE;
    }
  }
  return TRUE;
}


/*                       */
/*   IV. Input Routine   */
/*                       */

/********** socketin:  input routine called by Soar ******/
void socketin (int mode) 
{
  int rd_result;
  char next_char;

  if (client_id == -1) return;   /* if the socket is not initialized */
  
  switch (mode) 
    {
    case TOP_STATE_JUST_CREATED:     /* Initialize and internalize objects */
      return;
    case TOP_STATE_JUST_REMOVED:     /* Clean up */
      while (in_links) { 
	in_list *x; 
	/* remove_input_wme is not done because it has already been removed */
	release_io_symbol (in_links->link_symbol); 
	x = in_links; 
	in_links = in_links->next; 
	free_memory (x, MISCELLANEOUS_MEM_USAGE);
      } 
      return;
    case NORMAL_INPUT_CYCLE:         /* Standard stuff */
      break;
    }

  while((rd_result = read(client_id, &next_char, 1)) > 0)
    {
      in_buf[in_count++] = next_char;
      switch (next_char) 
	{
	case '(':
	  in_nest++;
	  break;
	case ')':
	  if (in_nest-- == 1) { /* transitioning 1 to 0 means completed list */
	    parse_input();      /* so parse it into wme's */
	    in_buf[0] = '\0';
	    in_count = 0;
	  }
	  break;
	}
    }
  
  /* return on error or when input blocked */
  if (rd_result == 0) {
    return;
  }
  else {
    if (errno == EWOULDBLOCK) { /* poll shows nothing to read on socket */
      return;
    } else {
      perror ("ERROR message ");
      return;
    }
  }
}


/*                    */
/*   V.   Utilities   */
/*                    */

/********** parse_input:  take input and turn into wme's ***********/
void parse_input () 
{

  in_token link_token;
  in_list **link_p;
  int idx;
  
  /* first syntactically turn the string in_buf into a list in_token
   * of either symbol or parenthesis tokens
   */
  in_string_idx = in_integer = in_float = in_sym_constant = in_token_last = 0;
  for (idx = 0 ; in_buf[idx] != '\0' ; idx++) 
    {
      switch (in_buf[idx]) 
	{
	case '(':   endtoken(); paren(LEFTPAREN);  break;
	case ')':   endtoken(); paren(RIGHTPAREN); break;
	case ' ':   endtoken();                    break;
	default:   buildtoken(in_buf[idx]);        break;
	}
    }

  /* the link name should be the second token: "( eyes ..." */
  link_token = in_tokens[1];  
  if (is_in_token_paren(link_token)) 
    {
      printf("\n>>>MONGSU<<< Socket input error, first item on list is not an atom\n");
      printf("\n>>>MONGSU<<< Input string: %s\n", in_buf);
      return;
    }
  
  /* for now, throw out all old input link wmes matching the link symbol */
  /* this does not seem to work at the moment, */
  /* take a closer look and find out why */
  /* -RLO (07Oct94) */

  link_p = &in_links;
  
  while (*link_p) 
    {
      if ((*link_p)->link_symbol == link_token.in_symbol) 
	{
	  in_list *x;
	  remove_input_wme ((*link_p)->link_wme);
	  release_io_symbol ((*link_p)->link_symbol); 
	  x = *link_p;
	  *link_p = (*link_p)->next;
	  free_memory (x, MISCELLANEOUS_MEM_USAGE);
      } else
	link_p = &((*link_p)->next);
    }

  in_token_next = 0;  /* reset the index to the first token */
  get_input_token_paren();  /* always call build_wmes past opening paren */

#ifdef NNPSCM
  build_wmes(current_agent(io_header));
#endif

#ifndef NNPSCM
  build_wmes(current_agent(top_state));
#endif

}

/********** build_wmes: recursively transverse the in_token's and make wmes **/
void build_wmes (Symbol *id) 
{
  in_token attribute_token, value_token;
  wme *new_wme;
  Symbol *sub_value;
  
  sub_value = NIL;
  attribute_token = get_input_token_symbol();

  /* may be the case of (), which means just an unattached id */
  if (attribute_token.paren == RIGHTPAREN) return;

  for (value_token = get_input_token();
       value_token.paren != RIGHTPAREN;
       value_token = get_input_token()) 
    {
      new_wme = NIL;
      if (value_token.paren == LEFTPAREN) 
	{  /* we have a substructure */
	  if (!sub_value) 
	    {
	      sub_value =
		get_new_io_identifier(attribute_token.in_symbol->sc.name[0]);
	      new_wme = add_input_wme (id,
				       attribute_token.in_symbol,
				       sub_value);
	    }
	  build_wmes (sub_value);
	}
      else {  /* no substructure */
	new_wme = add_input_wme (id,
				 attribute_token.in_symbol,
				 value_token.in_symbol);
	release_io_symbol (value_token.in_symbol);
      }
      if (new_wme) 
	{
#ifdef NNPSCM
	  if (id == current_agent(io_header)) 
	    { /* save a record of these wme's */
#endif
	      
#ifndef NNPSCM
	  if (id == current_agent(top_state)) 
	    { /* save a record of these wme's */
#endif
	      in_list *new;
	      new = (in_list *) allocate_memory
		(sizeof (in_list), MISCELLANEOUS_MEM_USAGE);
	      /* need to get_io_sym_constant to keep the reference */
	      /* counts correct */
	      new->link_symbol
		= get_io_sym_constant(attribute_token.in_symbol->sc.name);
	      new->link_wme = new_wme;
	      new->next = in_links;
	      in_links = new;
	    }
	}
    }
	  
      release_io_symbol (attribute_token.in_symbol);
      if (sub_value) release_io_symbol (sub_value);
}


/**********buildtoken(): add letters to a token *****************/
void buildtoken (char nextchar) 
{
  in_string[in_string_idx++] = nextchar;
  in_string[in_string_idx] = '\0' ;
  if (isdigit(nextchar)) in_integer++;
  else if ((nextchar == '.') && (in_float == 0)) in_float++;
  else in_sym_constant++;  
}

/********** endtoken:  turn string into token and add to list ******/
void endtoken (void) 
{
  /* the io constants gotten here are released as they are used after */
  /* becoming part of a wme.                                          */

  if ((in_sym_constant > 0) || ((in_float == 1) && (in_integer == 0))) 
    {
      if (in_string[0] == '<') 
	{ /* simple test that it is an identifier  */
	  /*  ... it would be nice if I kept track */
	  /* of these, then i could have <x>       */
	  /* multiple places in the tree and make  */
	  /* them the same Soar id... but I don't  */
	  /* need this immediately so i will take  */
	  /* the short route                       */

	  in_tokens[in_token_last++].in_symbol
	    = get_new_io_identifier(in_string[1]);
      } else
	in_tokens[in_token_last++].in_symbol = get_io_sym_constant(in_string);
    }
  else if (in_float == 1) 
    { /* and in_integer must be > 0 */
      in_tokens[in_token_last++].in_symbol
	= get_io_float_constant(atof(in_string));
    }
  else if (in_integer > 0) 
    {
      in_tokens[in_token_last++].in_symbol =
	get_io_int_constant(atoi(in_string));
    }    
  
  /* reset everything */
  in_string_idx = in_integer = in_float = in_sym_constant = 0;
}
  

/********** leftparen: add to input list that found left paren ******/
void paren(paren_type paren) 
{
  in_tokens[in_token_last++].paren = paren;
}


/********** get_input_token: return the next token in the input list ****/
in_token get_input_token (void)
{
  return in_tokens[in_token_next++];
}


/********* get_input_token_paren: return next token & check if paren ***/
in_token get_input_token_paren (void) 
{
  in_token tok;
  tok = get_input_token();
  if (is_in_token_paren(tok))
    return tok;
  else {
    printf("\n>>>MONGSU<<< Socket input error, expected a parenthesis\n");
    printf("\n>>>MONGSU<<< Input string: %s\n", in_buf);
    return;
  }
}

/********* get_input_token_symbol: return next token & check if symbol ***/
in_token get_input_token_symbol (void) 
{
  in_token tok;
  tok = get_input_token();
  if (is_in_token_paren(tok)) 
    {
      printf("\n>>>MONGSU<<< Socket input error, first item on list is not an atom\n");
      printf("\n>>>MONGSU<<< Input string: %s\n", in_buf);
      return;
    }
  else return tok;
}

/********** is_in_token_paren: is the token a paren? ***/
bool is_in_token_paren(in_token tok) 
{
  if ((tok.paren == LEFTPAREN) || (tok.paren == RIGHTPAREN))
    return TRUE;
  else
    return FALSE;
}

/********** is_in_token_paren: is the token a symbol (not paren)? ***/      
bool is_in_token_symbol (in_token tok) 
{
  return (!is_in_token_paren(tok));
}


/*                */
/*   VI.   Help   */
/*                */

/******************** HELP STRINGS ********************************/
/* modified to incorporate changes on the commands -RLO (31Aug94) */
char *help_on_init_socket_io [] = 
{
  "Command: init-socket-io",
  "",
  "Syntax: init-socket-io server_name server_port_no",
  "",
  "This command creates a socket port and connects to a named server and",
  "port number.",
  "",
  "Once a socket is initialized, then when any modification is made",
  "to a socket-output-link augmentation on the top-state, the",
  "augmentation and its substructure is sent as a nested list",
  "out the socket port.",
  "",
  "A default socket-output-link is created called socketout-link.",
  "Use socket-output-link to add other links.",
  0 
};

/* added this help for init-socket-servger */
/* -RLO (15Sep94) */
char *help_on_init_socket_server [] = 
{
  "Command: init-socket-server",
  "",
  "Syntax: init-socket-server",
  "",
  "This command creates a socket port and displays its port number.",
  "The command then waits for another process to connect to the socket",
  "port, using this number.",
  "",
  "Once a socket is initialized, then when any modification is made",
  "to a socket-output-link augmentation on the top-state, the",
  "augmentation and its substructure is sent as a nested list",
  "out the socket port.",
  "",
  "A default socket-output-link is created called socketout-link.",
  "Use socket-output-link to add other links.",
  0
};

char *help_on_close_socket_io [] = 
{
  "Command:  close-socket-io",
  "",
  "Syntax: close-socket-io",
  "",
  "This command closes a previously initialized and opened client socket.",
  0 
};

/* added this help for shutdown-socket-io */
/* -RLO (15Sep94) */
char *help_on_shutdown_socket_io [] = 
{
  "Command:  shutdown-socket-io",
  "",
  "Syntax: shutdown-socket-io",
  "",
  "This command closes a previously initialized and opened server socket.",
  0 
};

char *help_on_socket_output_link [] = 
{
  "Command: socket-output-link", 
  "",
  "Syntax: socket-output-link output-link-name", 
  "",
  "The socket-output-link command adds socket output-links.  Once a",
  "socket-output-link is named, when any modification to an augmentation",
  "by that name on the top-state is made, the augmentation and its",
  "substructure is sent as a nested list out the socket port.",
  0 
};


/*                                         */
/*   VII.   Declarations and Definitions   */
/*                                         */

/********** stdsocket_init:  register socket routines with Soar ********/
void stdsocket_init (void) 
{
  add_command ("init-socket-io", init_socket_io);
  add_help ("init-socket-io", help_on_init_socket_io);

  /* added this function for creating a server socket */
  /* -RLO (15Sep94) */
  add_command ("init-socket-server", init_socket_server);
  add_help ("init-socket-server", help_on_init_socket_server);

  add_command ("close-socket-io", close_socket_io);
  add_help ("close-socket-io", help_on_close_socket_io);

  /* added this function for shutting down a server socket */
  /* -RLO (15Sep94) */
  add_command ("shutdown-socket-io", shutdown_socket_io);
  add_help ("shutdown-socket-io", help_on_shutdown_socket_io);

  add_command ("socket-output-link", socket_output_link);
  add_help ("socket-output-link", help_on_socket_output_link);

  add_input_function (socketin); 
  add_output_function ("socketout-link", socketout);
}

