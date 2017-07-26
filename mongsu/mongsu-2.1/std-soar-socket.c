/*  -*- Mode: C -*-  */
/***************************************************************************/
/*  
/*  File            : std-soar-socket.c
/*  Author          : Ralph Morelli
/*  Created On      : Sat Mar 14 14:32:14 1992
/*  Last Modified By: Gordon D. Baxter
/*  Last Modified On: July 18, 1995
/*  Update Count    : 29
/*  
/*  PURPOSE
/*     This file provides socket support for sockets from Soar to Lists. This
/*     code will interface with associated code in "socket.lisp" and 
/*     "stdio.lisp" for Allegro, Lucid or CMU Common Lisp. It was originally 
/*     written for use with cT.lisp, which defines the functions given here as
/*     foreign functions. It is compiled to std-soar-socket.o and loaded into
/*     LISP. See these programs and the Makefile for details.
/*
/*      The following foreign (integer) functions are defined.
/*        sock = create_socket ()
/*        port = bind_socket (sock)
/*        client_file_descriptor = listen_socket (sock)
/*        accept_socket (sock)
/*        close_socket (sock)
/*        shutdown_socket (sock, how)
/*        connect_socket (sock, server_system_name, server_port_number)
/*
/*      The following foreign utility functions are defined.
/*        itoa (n, s)
/*        reverse (s)
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
/* TABLE OF CONTENTS
/* 
/*    I.    Initialization and Structures
/*    II.   Socket Creation
/*    III.  Socket Binding
/*    IV.   Socket Listening
/*    V.    Socket Acceptance
/*    VI.   Socket Connection
/*    VII.  Socket Closing
/*    VIII. Socket Shutdown
/*    IX.   Utilities
/*  
/* SECTIONS:
/*  (C) Copyright 1991, Carnegie Mellon University, all rights reserved.
/*  (C) Other sections Copyright 1995, Roberto L. Ong and Frank E. Ritter.
/***************************************************************************/
/* HISTORY
/*    Blake Ward developed the first version of this program that defined
/*    three functions: create-socket, connect-socket, destroy-socket
/* ??????? - Garret Pelton modified the functions to work with cT
/* 09Mar92 - RM modified documentation 
/* 11Mar92 - RM added DEBUG and PERROR code
/* 11Mar92 - RM modified destroy_socket 
/* 14Mar92 - RM redefined the foreign functions to bring them into a 1-1
/*           relationship with C-language system calls, thereby giving 
/*           more fine-grained control to LISP. 
/* 07Dec92 - JM added connect_socket
/* 19Aug94 - RLO changed variable names and added global variables to make 
/*           the code more general, understandable and clear.
/*           Made error messages more constructive and clear.
/*           NNPSCM and NON-NNPSCM in one file
/* 04Jan95 - Added itoa and reverse functions for use as utilities.
/* 01Feb95 - Final clean-up of the code.
/* 18Jul95 - >>>MONGSU<<< added to message strings (GDB)
/*                           
/***************************************************************************/


/*                                       */
/*   I.  Initialization and Structures   */
/*                                       */

#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <stdio.h>
#include <errno.h>
#include <unistd.h>

/* Definition of constants */
#define DEBUG 1                 /* Set to 1 for ON, 0 for OFF */
#define DEFAULT_PROTOCOL 0      /* Set to standard protocol -RLO (19Aug94) */
#define DEFAULT_QUEUE_LENGTH 1  /* Set to number of connectrions allowed */
                                /* -RLO (19Aug94) */

/* Full internet name of lisp side of socket */
/* -RLO (19Aug94) */
static struct sockaddr_in serverINETaddress;

/* Routines defined in this file */
int create_socket (void);
int bind_socket (int);
int listen_socket (int);
int accept_socket (int);
int connect_socket (int, char*, int);
int close_socket (int);
int shutdown_socket (int, int);
char *itoa (int);
char *reverse (char []);



/*                         */
/*   II. Socket Creation   */ 
/*                         */

/* Create a socket */
int create_socket (void)      
{
  int sock;

  /* Create a socket on which to send. */
  sock = socket(AF_INET, SOCK_STREAM, DEFAULT_PROTOCOL);
  if (sock == -1) {
    /* modified error message -RLO (31Aug94) */
    fprintf(stderr, 
	    ">>>MONGSU<<< Establish Socket: Unable to create socket in create_socket.\n");
    perror (">>>MONGSU<<< ERROR: CREATING STREAM SOCKET");
    return(-1);
  }
  if (DEBUG)
    fprintf (stderr, ">>>MONGSU<<< Created Socket with id %d\n", sock);
  return (sock);
} /* end create_socket */


/*                         */
/*   III. Socket Binding   */ 
/*                         */

/* Bind the socket to a port */
int bind_socket (int sock)
{   
  int namelen;

  serverINETaddress.sin_family = AF_INET;
  serverINETaddress.sin_addr.s_addr = INADDR_ANY;
  serverINETaddress.sin_port = htons(0);     /* Uses a wild card port number */
                                             /* assigned by the system */

  if (bind(sock, &serverINETaddress, sizeof(serverINETaddress)) == -1) {
    /* modified error message -RLO (31Aug94) */
    fprintf(stderr, 
	    ">>>MONGSU<<< Establish Socket: Unable to bind to socket %d (in bind_socket).\n",sock);
    perror (">>>MONGSU<<< BIND ERROR: Closing Socket");
    return(-1);
  }
  namelen = sizeof(serverINETaddress);
  if (getsockname (sock, &serverINETaddress, &namelen)) {
    /* modified error message -RLO (31Aug94) */
    perror(">>>MONGSU<<< ERROR: Unable to get socket name.");
    return(-1);
  }

  return (ntohs(serverINETaddress.sin_port));
} /* end bind_socket */


/*                          */
/*   IV. Socket Listening   */ 
/*                          */

/* Put socket in "listen" state (one connection) */
int listen_socket (int sock)     
{
  int rtncode;

  rtncode = listen(sock,DEFAULT_QUEUE_LENGTH);
  if (rtncode == -1) {
    fprintf(stderr, ">>>MONGSU<<< Establish Socket: Unable to listen to socket %d.\n",sock);
    perror (">>>MONGSU<<< LISTEN ERROR: Closing Socket");
    return(-1);
  }
  if (DEBUG)
    fprintf (stderr, ">>>MONGSU<<< Listening at socket with id %d\n", sock);

  return(rtncode);
}  /* end listen_socket */


/*                          */
/*   V. Socket Acceptance   */ 
/*                          */

/* accept_socket takes a socket, waits for someone to connect to it and */
/* then returns a Unix file descriptor corresponding to the socket.     */

int accept_socket (int sock)
{
  int client_fd;		/* fd after client has connected */
  struct sockaddr_in clientINETaddr;
  int client_addr_len;
  struct hostent *host;
  
  /* Wait for a connection */
  client_addr_len = sizeof(clientINETaddr);

  client_fd= accept(sock, &clientINETaddr, &client_addr_len);
  if (client_fd == -1) {
    /* commented out for use with launching a process in Lisp,       */
    /* because it keeps on printing error messages during initial    */
    /* launching of a process (as there is no connected process yet) */
    /* -RLO (19Aug94) */
    /* fprintf(stderr, ">>>MONGSU<<< Accept: Unable to accept connection at socket %d\n",sock); */
    /* perror (">>>MONGSU<<< ACCEPT ERROR: Closing Socket"); */
    return(-1); 
  }
  
  host = gethostbyaddr((char*) &clientINETaddr.sin_addr.s_addr,4,AF_INET);
  
  if (DEBUG)
    fprintf(stderr, ">>>MONGSU<<< Accepted connection from %s\n",host->h_name);
  
  return(client_fd);
} /* end accept_socket */


/*                           */
/*   VI. Socket Connection   */ 
/*                           */

/* connect_socket: This function takes the server name and port number
 * of a socket from a server process, and a socket server_fd and connects
 * to it.  The process returns the result of the connect system call.
 */

int connect_socket(int server_fd, char *server_name, int port_no)
{
  struct sockaddr_in serverINETaddress;
  struct hostent *host;
  int rtncode;

  host = gethostbyname(server_name);
  if (host == 0) {
    fprintf(stderr, ">>>MONGSU<<< Connect: Unknown host %s.\n", host->h_name);
    perror(">>>MONGSU<<< CONNECT ERROR");
    return(-1);  
  }
  bzero((char *)&serverINETaddress, sizeof(serverINETaddress));
  serverINETaddress.sin_family = host->h_addrtype;
  bcopy(host->h_addr, (char *)&serverINETaddress.sin_addr, host->h_length);
  serverINETaddress.sin_port = htons(port_no);

  rtncode = connect(server_fd, &serverINETaddress, sizeof(serverINETaddress));
  if (rtncode == -1) {
    fprintf(stderr, ">>>MONGSU<<< Test:  Unable to connect (errno=%d).\n",errno);
    perror (">>>MONGSU<<< ERROR MESSAGE");
    return(-1);
  }
  
  if (DEBUG)
    fprintf(">>>MONGSU<<< Connected to server with fd %d\n",server_fd);
  return(rtncode);
}


/*                         */
/*   VII. Socket Closing   */ 
/*                         */

/* Close the socket */
int close_socket (int sock)         
{
  int rtncode;
  
  rtncode = close(sock);
  if (rtncode == -1) {
    fprintf (stderr, ">>>MONGSU<<< close: Unable to close socket %d.\n", sock);
    perror (">>>MONGSU<<< CLOSE ERROR");
    return(-1);
  }

  if (DEBUG)
    fprintf (stderr,">>>MONGSU<<< Closed socket with id %d\n", sock);

  return(rtncode);
}  /* end close */


/*                           */
/*   VIII. Socket Shutdown   */ 
/*                           */

/*   Shutdown causes a socket's connection to be shutdown. If
     how is 0, then further receives will be disallowed.  If how
     is 1, then further sends will be disallowed.  If how is 2,
     then further sends and receives will be disallowed.          */

int shutdown_socket (int sock, int how)
{
  int rtncode;

  rtncode = shutdown(sock, how);
  if (rtncode == -1) {
    fprintf (stderr, ">>>MONGSU<<< close: Unable to shutdown socket %d.\n", sock);
    perror (">>>MONGSU<<< SHUTDOWN ERROR");
    return(-1);
  }

  if (DEBUG)
    fprintf (stderr,">>>MONGSU<<< Shutdown socket with id %d\n", sock);

  return (rtncode);
}


/*                     */
/*   IX.   Utilities   */ 
/*                     */

/* These utilities supports the conversion of INT to ASCII as there */
/* is no existing function that does this in C.                     */
/* - RLO (02Jan95)                                                  */

/* Converts INT to STRING */
char *itoa (int n)
{
  int i, sign;
  int MAX_LENGTH = 10;
  char s[MAX_LENGTH];

  if ((sign = n) < 0)
    n = -n;
  i = 0;
  do {
    s[i++] = n % 10 + '0';
  } while ((n /= 10) > 0);
  if (sign < 0)
    s[i++] = '-';
  s[i] = '\0';
  return reverse(s);
}

/* Reverses the characters of a string. */
char *reverse (char s[])
{
  int c, i, j;

  for(i=0, j=strlen(s) - 1; i<j; i++, j--)
    {
      c= s[i];
      s[i] = s[j];
      s[j] = c;
    }
  return s;
}
