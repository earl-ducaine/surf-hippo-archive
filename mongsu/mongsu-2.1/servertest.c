/*  -*- Mode: C -*-  */
/************************************************************************
/*
/* File            : servertest.c
/* Author          : Roberto L. Ong <rlo@psyc.nott.ac.uk>
/* Created On      : Fri Aug 19 10:57:01 1994
/* Last Modified By: Roberto L. Ong <rlo@psyc.nott.ac.uk>
/* Last Modified On: Wed Jan 11 11:44:01 1995
/* Update Count    : 10
/*
/* Associated modules : .../socket.lisp
/*                      .../stdio.lisp
/*
/* PURPOSE
/*     This file implements a simple socket I/O facility for testing
/*     the Lisp socket code - "socket.lisp".  It is for use when testing
/*     Lisp as a client process.
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
/*     gcc servertest.c -o servertest.out
/*
/* RUN:
/*     Type the following on the command prompt:
/*
/*           servertest.out
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
/* 11Jan95 - added comments on the SOCKET-PROCESS-FILE
/* (RLO)    
/*
/************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netinet/in.h>

#define DEFAULT_PROTOCOL 0
#define MAX_PATH_LENGTH 1024         /* from sys/param.h */
                                     /* -RLO (13Dec94) */
#define MAX_SERVER_PORT_NO_LENGTH 15 /* max length of server port number */
                                     /* -RLO (13Dec94) */
#define SOCKET_PROCESS_FILE ".socket.process" /* socket process file name */
                                              /* -RLO (13Dec94) */

/* Declaration of Functions */
void itoa (int, char []);
void reverse (char []);

main() {
  FILE *fptr;
  int sock, length, msgsock, rval;
  struct sockaddr_in server;
  char buf[1024];
  char filename [MAX_PATH_LENGTH];
  char server_port_no [MAX_SERVER_PORT_NO_LENGTH];
  char *server_name, *home_directory;
  char *comment = 
    "\n;; This file was created by the server process that contains
;; the <hostname> and <port-number>. This will be read by
;; the client process for use in connecting to the server
;; socket.
;; This file will always be deleted after the server socket
;; is shutdown.
;; -RLO (11Jan95)";

  /* create a Internet socket */
  sock = socket (AF_INET, SOCK_STREAM, DEFAULT_PROTOCOL);
  if (sock < 0) {
    perror("opening stream socket");
    exit(1);
  }

  /* connect socket using name specified by command line */
  server.sin_family = AF_INET;
  server.sin_addr.s_addr = INADDR_ANY;
  server.sin_port = 0;

  if (bind(sock, &server, sizeof(server))) {
    perror("binding stream socket");
    exit(1);
  }

  length = sizeof(server);
  if (getsockname(sock, &server, &length)) {
    perror("getting socket name");
    exit(1);
  }

  /* write to socket-process file for automatic hookup by client process */
  /* -RLO (13Dec94) */
  home_directory = getenv ("HOME");
  if (home_directory) {
    strcpy (filename, home_directory);
    strcat (filename, "/");
    strcat (filename, SOCKET_PROCESS_FILE);
  }

  if ((fptr = fopen(filename, "w")) == NULL) {
    fprintf (stderr, "Unable to open %s\n.", filename);
  }

  /* write server_name and server_port_no to SOCKET_PROCESS_FILE */
  /* -RLO (13Dec94) */
  server_name = getenv ("HOSTNAME");
  fprintf(fptr, server_name);
  fprintf(fptr, "\n");
  itoa (ntohs(server.sin_port), server_port_no);
  fprintf(fptr, server_port_no);
  fprintf(fptr, "\n");
  fprintf(fptr, comment);
  fclose(fptr);

  printf("Socket has port #%d\n", ntohs(server.sin_port));

  /* Listens for an incoming request for a connection. */
  /* Only 1 connection at a time. */
  /* -RLO (04Jan95) */
  listen(sock, 1);

  msgsock = accept(sock, 0, 0);
  if (msgsock == -1)
    perror("accept");
  else do {
    bzero(buf, sizeof(buf));
    if((rval = read(msgsock, buf, 1024)) < 0)
      perror("reading stream message");
    if (rval == 0)
      printf("Ending connection\n");
    else
      printf("-->%s\n", buf);
  } while(rval != 0);
  close(msgsock);
  remove(filename);
}

/* Converts an INT to a STRING */
void itoa (int n, char s[])
{
  int i, sign;

  if ((sign = n) < 0)
    n = -n;
  i = 0;
  do {
    s[i++] = n % 10 + '0';
  } while ((n /= 10) > 0);
  if (sign < 0)
    s[i++] = '-';
  s[i] = '\0';
  reverse(s);
}

/* Reverses the characters in a string */
void reverse (char s[])
{
  int c, i, j;

  for (i=0, j=strlen(s) - 1; i<j; i++, j--)
    {
      c= s[i];
      s[i] = s[j];
      s[j] = c;
    }
}



