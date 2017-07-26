/*  -*- Mode: C -*-  */
/************************************************************************
/*
/* File            : clienttest.c
/* Author          : Roberto L. Ong <rlo@psyc.nott.ac.uk>
/* Created On      : Fri Aug 19 10:57:01 1994
/* Last Modified By: Roberto L. Ong <rlo@psyc.nott.ac.uk>
/* Last Modified On: Wed Jan 11 11:57:01 1995
/* Update Count    : 7
/*
/* Associated modules : .../socket.lisp
/*                      .../stdio.lisp
/*
/* PURPOSE
/*     This file implements a simple socket I/O facility for testing
/*     the Lisp socket code - "socket.lisp".  It is for use when testing
/*     Lisp as a server process.
/*
/* LOCATION
/*     This code together with other codes is packaged together as a socket 
/*     support utility called MONGSU.  They can be downloaded via anonymous
/*     FTP from host 128.243.40.7 (unicorn.ccc.nott.ac.uk, but many machines
/*     don't know it, so you may wish to use the numbers) in the directory 
/*     "/pub/lpzfr" (From within ftp only the part of the tree rooted at 
/*     /usr/ftp is visible).
/*
/* COMPILE: 
/*     gcc clienttest.c -o clienttest.out
/*
/* RUN:
/*     Type the following on the command prompt:
/*
/*           clienttest.out
/*
/* N.B. Please don't forget to load and run the Lisp socket code before
/*      running this code.
/*        
/* That should be all of them.  I think that this is reasonably 
/* simple, and that anyone would be able to handle them easily.
/* But if you need help setting this up, drop an email and I
/* would gladly help you out.
/*
/* (C) Copyright 1995, Roberto L. Ong.
/*     University of Nottingham, all rights reserved.
/************************************************************************/
/* HISTORY
/* 19Aug94 - started creating this file
/* (RLO)
/* 11Jan95 - modified error messages on reading a non-existing 
/* (RLO)     SOCKET_PROCESS_FILE
/*
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>

/* Definition of constants */
#define DEFAULT_PROTOCOL 0
#define MAX_PATH_LENGTH 1024         /* from sys/param.h */
                                     /* -RLO (13Dec94) */
#define MAX_SERVER_NAME_LENGTH 15    /* max length of server name */
                                     /* -RLO (19Aug94) */
#define MAX_SERVER_PORT_NO_LENGTH 15 /* max length of server port number */
                                     /* -RLO (13Dec94) */
#define SOCKET_PROCESS_FILE ".socket.process" /* socket process file name */
                                              /* -RLO (13Dec94) */

/* the definition of DATA could be change to other list */
/* list S-expressions, as long as they can be evaluated -RLO (31-08-94) */
#define DATA "(+ 1 2)"

main() 
{
  FILE *fptr;
  int sock;
  struct sockaddr_in server;
  struct hostent *hp, *gethostbyname();
  char buf[1024];
  char server_name [MAX_SERVER_NAME_LENGTH];
  char server_port_no [MAX_SERVER_PORT_NO_LENGTH];
  char filename [MAX_PATH_LENGTH];
  char *home_directory;

  /* create an Internet socket */
  sock = socket (AF_INET, SOCK_STREAM, DEFAULT_PROTOCOL);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }

  /* read socket-process file for automatic hookup to server process */
  /* -RLO (13Dec94) */
  home_directory = getenv ("HOME");
  if (home_directory) {
    strcpy (filename, home_directory);
    strcat (filename, "/");
    strcat (filename, SOCKET_PROCESS_FILE);
  }

  if ((fptr = fopen(filename, "r")) == NULL) {
    fprintf (stderr, "Unable to open %s file.
ERROR: There is no existing server socket.\n", filename);
    exit (2);
  }

  /* read server_name and server_port_no from SOCKET_PROCESS_FILE */
  /* -RLO (13Dec94) */
  fscanf(fptr, "%[^\n]\n %s", &server_name, &server_port_no);
  fclose(fptr);

  printf("Connecting to server with name %s\n", server_name);
  printf("Connecting to server with port #%d\n", atoi(server_port_no));

  /* connect socket using name specified by command line */
  server.sin_family = AF_INET;
  hp = gethostbyname(server_name);
  if (hp == 0) {
     fprintf(stderr, "%s: unknown host", server_name);
     exit(2);
   }

  bcopy(hp->h_addr, &server.sin_addr, hp->h_length);
  server.sin_port = htons(atoi(server_port_no));

  if (connect(sock, &server, sizeof(server)) < 0) {
     perror("Connecting stream socket");
     exit(1);
  }

  if (write(sock, DATA, sizeof(DATA)) < 0) {
     perror("Writing on stream socket");
     close(sock);
  }
}










